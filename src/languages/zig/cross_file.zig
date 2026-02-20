const std = @import("std");
const graph_mod = @import("../../core/graph.zig");
const logging = @import("../../logging.zig");
const node_mod = @import("../../core/node.zig");
const edge_mod = @import("../../core/edge.zig");
const types = @import("../../core/types.zig");
const ts = @import("tree-sitter");
const ts_api = @import("../../parser/tree_sitter_api.zig");
const ast = @import("ast_analysis.zig");
const pc = @import("parse_context.zig");

const Field = logging.Field;
const Logger = logging.Logger;

const Graph = graph_mod.Graph;
const Node = node_mod.Node;
const Edge = edge_mod.Edge;
const NodeId = types.NodeId;
const NodeKind = types.NodeKind;
const EdgeType = types.EdgeType;
const EdgeSource = types.EdgeSource;
const Visibility = types.Visibility;
const KindIds = pc.KindIds;
const ScopeIndex = pc.ScopeIndex;
const FileIndex = pc.FileIndex;

// =========================================================================
// Cross-file resolution context
// =========================================================================

/// Groups all context needed during edge creation for one file parse.
/// Contains the file's node scope range and a map of project-file imports.
pub const EdgeContext = struct {
    scope_start: usize,
    scope_end: usize,
    import_names: [max_imports][]const u8 = undefined,
    import_targets: [max_imports]NodeId = undefined,
    import_count: usize = 0,

    // Max number of tracked @import bindings per file for cross-file edge resolution.
    pub const max_imports = 32;

    pub fn findImportTarget(self: *const EdgeContext, name: []const u8) ?NodeId {
        for (self.import_names[0..self.import_count], self.import_targets[0..self.import_count]) |iname, itarget| {
            if (std.mem.eql(u8, iname, name)) return itarget;
        }
        return null;
    }
};

/// Tracks per-function variable assignments from import-qualified expressions.
/// Used to resolve method calls like `a.deinit()` where `a = alpha.Self.init(42)`.
pub const VarTracker = struct {
    var_names: [max_vars][]const u8 = undefined,
    var_targets: [max_vars]NodeId = undefined,
    var_count: usize = 0,

    // Max number of tracked import-qualified variable bindings per function.
    pub const max_vars = 32;

    pub fn addBinding(self: *VarTracker, name: []const u8, target_file: NodeId, log: Logger) void {
        // Graceful cap: silently skip excess bindings.
        if (self.var_count >= max_vars) {
            log.trace("var tracker at capacity", &.{Field.string("name", name)});
            return;
        }
        self.var_names[self.var_count] = name;
        self.var_targets[self.var_count] = target_file;
        self.var_count += 1;
    }

    pub fn findTarget(self: *const VarTracker, name: []const u8) ?NodeId {
        for (self.var_names[0..self.var_count], self.var_targets[0..self.var_count]) |vname, vtarget| {
            if (std.mem.eql(u8, vname, name)) return vtarget;
        }
        return null;
    }
};

// =========================================================================
// Constants
// =========================================================================

/// Maximum depth for field_expression chains like `a.b.c.d`.
/// 8 covers realistic Zig qualified paths (e.g., `std.mem.Allocator.Error`)
/// while bounding stack usage in the recursive descent.
pub const max_chain_depth = 8;

/// Maximum AST depth for scanForCalls and scanForTypeIdentifiersScoped.
/// 256 covers any realistic Zig code while bounding stack usage.
pub const max_ast_scan_depth: u32 = 256;

/// Maximum AST depth when searching for `@import` calls inside a declaration.
/// A standard `const x = @import("path")` has the builtin_function at depth 1-2.
/// Depth 5 covers realistic patterns (e.g., `@import` inside `if` or `orelse`)
/// while bounding stack usage in the recursive descent.
pub const max_import_search_depth = 5;

// =========================================================================
// Edge utility
// =========================================================================

pub fn addEdgeIfNew(g: *Graph, source_id: NodeId, target_id: NodeId, edge_type: EdgeType, log: Logger) !void {
    if (source_id == target_id) return;
    const added = try g.addEdgeIfNew(.{
        .source_id = source_id,
        .target_id = target_id,
        .edge_type = edge_type,
        .source = .tree_sitter,
    });
    if (!added) {
        log.trace("duplicate edge skipped", &.{
            Field.uint("source", @intFromEnum(source_id)),
            Field.uint("target", @intFromEnum(target_id)),
        });
    }
}

// =========================================================================
// Cross-file resolution helpers
// =========================================================================

/// Collect the chain of identifiers from a (possibly nested) field_expression.
/// For `alpha.Self.init`, populates out with ["alpha", "Self", "init"] and returns 3.
pub fn collectFieldExprChain(source: []const u8, node: ts.Node, out: *[max_chain_depth][]const u8, k: *const KindIds) usize {
    return collectChainRecursive(source, node, out, k, 0);
}

/// Recursive helper for collectFieldExprChain. Descends into nested
/// field_expression nodes, appending each identifier segment to `out`.
fn collectChainRecursive(source: []const u8, node: ts.Node, out: *[max_chain_depth][]const u8, k: *const KindIds, depth: usize) usize {
    // Graceful cap: return truncated chain, remaining segments are ignored.
    if (depth >= max_chain_depth) return depth;
    const kid = node.kindId();
    if (kid == k.identifier) {
        out[depth] = ts_api.nodeText(source, node);
        return depth + 1;
    }
    if (kid == k.field_expression) {
        var result = depth;
        if (node.namedChild(0)) |obj| {
            result = collectChainRecursive(source, obj, out, k, result);
        }
        const count = node.namedChildCount();
        if (count >= 2) {
            if (node.namedChild(count - 1)) |field| {
                result = collectChainRecursive(source, field, out, k, result);
            }
        }
        return result;
    }
    // Unwrap call_expression to reach the function reference (first named child).
    if (kid == k.call_expression) {
        if (node.namedChild(0)) |fn_ref| {
            return collectChainRecursive(source, fn_ref, out, k, depth);
        }
    }
    return depth;
}

/// Extract the import path from a variable_declaration containing @import("...").
/// For `const alpha = @import("alpha.zig")`, returns "alpha.zig".
pub fn extractImportPath(source: []const u8, var_decl: ts.Node, k: *const KindIds) ?[]const u8 {
    return extractImportPathRecursive(source, var_decl, k, 0);
}

/// Recursive helper for extractImportPath. Walks AST children looking for
/// a builtin_function node containing `@import`, then extracts the string argument.
fn extractImportPathRecursive(source: []const u8, node: ts.Node, k: *const KindIds, depth: u32) ?[]const u8 {
    // Graceful cap: deeply nested @import calls are not recorded.
    if (depth > max_import_search_depth) return null;
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        const kid = child.kindId();
        if (kid == k.builtin_function) {
            // Check if this is @import by looking for builtin_identifier.
            var j: u32 = 0;
            while (j < child.childCount()) : (j += 1) {
                const bi = child.child(j) orelse continue;
                if (bi.kindId() == k.builtin_identifier) {
                    if (std.mem.eql(u8, ts_api.nodeText(source, bi), "@import")) {
                        return findStringContent(source, child, k);
                    }
                }
            }
        }
        if (extractImportPathRecursive(source, child, k, depth + 1)) |path| return path;
    }
    return null;
}

/// Walk an AST subtree looking for the first `string_content` node.
/// Used to extract the path argument from `@import("path")`.
fn findStringContent(source: []const u8, node: ts.Node, k: *const KindIds) ?[]const u8 {
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        if (child.kindId() == k.string_content) {
            return ts_api.nodeText(source, child);
        }
        if (findStringContent(source, child, k)) |path| return path;
    }
    return null;
}

/// Build the import map in an EdgeContext by scanning root-level variable declarations.
pub fn buildImportMap(g: *const Graph, source: []const u8, root: ts.Node, ctx: *EdgeContext, file_index: *const FileIndex, k: *const KindIds, log: Logger) void {
    var i: u32 = 0;
    while (i < root.childCount()) : (i += 1) {
        const child = root.child(i) orelse continue;
        if (!child.isNamed()) continue;
        if (child.kindId() != k.variable_declaration) continue;
        // Graceful cap: silently skip excess imports.
        if (ctx.import_count >= EdgeContext.max_imports) {
            log.debug("import map at capacity", &.{});
            break;
        }

        const name = ast.getIdentifierName(source, child, k) orelse continue;

        // Check if this variable was classified as import_decl during node creation.
        const scoped_nodes = g.nodes.items[ctx.scope_start..ctx.scope_end];
        var is_import = false;
        for (scoped_nodes) |n| {
            if (n.kind == .import_decl and std.mem.eql(u8, n.name, name)) {
                is_import = true;
                break;
            }
        }
        if (!is_import) continue;

        // Extract import path from AST.
        const import_path = extractImportPath(source, child, k) orelse {
            log.trace("import path extraction failed", &.{});
            continue;
        };

        // Find the target file node in the graph.
        if (file_index.findByName(import_path)) |target_id| {
            ctx.import_names[ctx.import_count] = name;
            ctx.import_targets[ctx.import_count] = target_id;
            ctx.import_count += 1;
        } else {
            log.trace("import target file not found", &.{Field.string("path", import_path)});
        }
    }
}

/// Resolve an import-qualified chain against a target file.
/// For chain ["MyType", "init"] with a target file:
///   - "MyType" as type_def (direct child of file) creates uses_type edge
///   - "init" as function (direct child of MyType) creates calls edge (if is_call)
/// Each segment narrows the scope to direct children of the resolved node.
pub fn resolveQualifiedCall(
    g: *Graph,
    caller_id: NodeId,
    target_file_id: NodeId,
    chain: []const []const u8,
    is_call: bool,
    scope_index: *const ScopeIndex,
    file_index: *const FileIndex,
    log: Logger,
) !void {
    var current_scope_id = target_file_id;

    for (chain, 0..) |segment, seg_idx| {
        const is_last = (seg_idx == chain.len - 1);

        // Search direct children of the current scope using the scope index.
        var matched_id: ?NodeId = null;
        for (scope_index.childrenOf(current_scope_id)) |child_idx| {
            const n = g.nodes.items[child_idx];
            if (!std.mem.eql(u8, n.name, segment)) continue;
            matched_id = @enumFromInt(child_idx);
            break;
        }

        // If no direct child matched (e.g. var-tracked chains that skip the
        // intermediate type), search all descendants but only resolve when
        // exactly one matches.
        if (matched_id == null) {
            var match_count: usize = 0;
            for (g.nodes.items, 0..) |n, idx| {
                if (!std.mem.eql(u8, n.name, segment)) continue;
                // Walk parent chain to verify this node is under current_scope_id.
                var ancestor = n.parent_id;
                var hops: usize = 0;
                const is_under_scope = while (ancestor != null and hops < 100) : (hops += 1) {
                    if (ancestor.? == current_scope_id) break true;
                    const anc_node = g.getNode(ancestor.?) orelse break false;
                    ancestor = anc_node.parent_id;
                } else false;
                if (!is_under_scope) continue;
                matched_id = @enumFromInt(idx);
                match_count += 1;
                if (match_count > 1) {
                    matched_id = null;
                    break;
                }
            }
        }

        // Self fallback: when segment is "Self" and no match is found
        // (because @This() aliases are now filtered at parse time),
        // keep the scope at the current level. Self is a self-reference
        // and should NOT produce a uses_type edge.
        if (matched_id == null and std.mem.eql(u8, segment, "Self")) {
            const scope_node = g.getNode(current_scope_id) orelse return;
            if (scope_node.kind == .type_def or scope_node.kind == .enum_def or scope_node.kind == .file) {
                // Self refers to the current type or file-struct; stay at this scope.
                continue;
            }
            return; // Unknown scope kind, resolution fails.
        }

        const resolved_id = matched_id orelse {
            log.trace("qualified call: segment not found", &.{Field.string("segment", segment)});
            return;
        };

        const resolved_node = g.getNode(resolved_id) orelse return;

        if (is_last and is_call and resolved_node.kind == .function) {
            try addEdgeIfNew(g, caller_id, resolved_id, .calls, log);
        } else if (!is_last and resolved_node.kind == .function) {
            // Mid-chain function call (e.g., getClient() in svc.getClient().send()).
            // Create calls edge and follow the function's return type.
            if (is_call) {
                try addEdgeIfNew(g, caller_id, resolved_id, .calls, log);
            }
            if (resolveReturnTypeScope(g, resolved_id, scope_index, file_index)) |return_type_id| {
                current_scope_id = return_type_id;
                continue;
            }
            log.trace("qualified call: return type unresolvable", &.{});
            return; // Cannot resolve return type; stop chain.
        } else {
            const is_type = resolved_node.kind == .type_def or resolved_node.kind == .enum_def;
            const is_type_alias = resolved_node.kind == .constant and
                resolved_node.name.len > 0 and resolved_node.name[0] >= 'A' and resolved_node.name[0] <= 'Z';
            if (is_type or is_type_alias) {
                try addEdgeIfNew(g, caller_id, resolved_id, .uses_type, log);
            }
        }

        // Narrow scope for next segment.
        if (resolved_node.kind == .type_def or resolved_node.kind == .enum_def) {
            // Type container: next segment is a child of this type.
            current_scope_id = resolved_id;
        } else {
            // Constant/alias (e.g. renamed alias): next segment is a sibling (same parent).
            current_scope_id = resolved_node.parent_id orelse return;
        }
    }
}

/// Check if a variable_declaration's value is rooted in an import-qualified expression.
/// Returns the target file NodeId if the value starts with an import name.
pub fn findImportQualifiedRoot(
    source: []const u8,
    var_decl: ts.Node,
    ctx: *const EdgeContext,
    k: *const KindIds,
) ?NodeId {
    var i: u32 = 0;
    while (i < var_decl.childCount()) : (i += 1) {
        const child = var_decl.child(i) orelse continue;
        if (extractExpressionImportRoot(source, child, ctx, k)) |target| return target;
    }
    return null;
}

/// Recursively extract the root import target from an expression.
/// Handles call_expression, field_expression, and try_expression wrappers.
pub fn extractExpressionImportRoot(source: []const u8, node: ts.Node, ctx: *const EdgeContext, k: *const KindIds) ?NodeId {
    const kid = node.kindId();
    if (kid == k.call_expression or kid == k.field_expression) {
        var chain: [max_chain_depth][]const u8 = undefined;
        const chain_len = collectFieldExprChain(source, node, &chain, k);
        if (chain_len > 0) {
            return ctx.findImportTarget(chain[0]);
        }
    }
    // Unwrap try_expression: `try alpha.init()`
    if (kid == k.try_expression) {
        var i: u32 = 0;
        while (i < node.namedChildCount()) : (i += 1) {
            const child = node.namedChild(i) orelse continue;
            if (extractExpressionImportRoot(source, child, ctx, k)) |target| return target;
        }
    }
    return null;
}

// =========================================================================
// Return-type resolution (mid-chain function calls)
// =========================================================================

/// Given a function node, try to resolve its return type to a type node
/// in the graph. Returns the type's NodeId (to use as scope for further
/// chain resolution) or null if the return type can't be resolved.
///
/// Parses the return type from the function's stored signature text.
/// Handles import-qualified types (e.g., `svc_mod.Service`) by resolving
/// the import in the function's containing file context.
pub fn resolveReturnTypeScope(g: *const Graph, fn_id: NodeId, scope_index: *const ScopeIndex, file_index: *const FileIndex) ?NodeId {
    const fn_node = g.getNode(fn_id) orelse return null;
    const sig = fn_node.signature orelse return null;

    // Extract return type: text after the last ')' in the signature.
    const paren_pos = std.mem.lastIndexOfScalar(u8, sig, ')') orelse return null;
    if (paren_pos + 1 >= sig.len) return null;
    var return_text = std.mem.trim(u8, sig[paren_pos + 1 ..], " \t\n\r");
    if (return_text.len == 0) return null;

    // Strip error union prefix (e.g., "!void" -> "void", "MyError!Type" -> "Type").
    if (std.mem.indexOfScalar(u8, return_text, '!')) |bang_pos| {
        return_text = return_text[bang_pos + 1 ..];
    }

    // Strip pointer/optional markers.
    while (return_text.len > 0 and (return_text[0] == '*' or return_text[0] == '?')) {
        return_text = return_text[1..];
    }
    // Strip "const " prefix (from *const T).
    if (std.mem.startsWith(u8, return_text, "const ")) {
        return_text = return_text[6..];
    }
    return_text = std.mem.trim(u8, return_text, " \t\n\r");
    if (return_text.len == 0) return null;

    // Split on '.' to get segments (e.g., "svc_mod.Service" -> ["svc_mod", "Service"]).
    var segments: [max_chain_depth][]const u8 = undefined;
    var seg_count: usize = 0;
    var iter = std.mem.splitScalar(u8, return_text, '.');
    while (iter.next()) |seg| {
        if (seg_count >= max_chain_depth) break;
        // Trim and extract only identifier characters.
        var s = std.mem.trim(u8, seg, " \t\n\r");
        var end: usize = 0;
        while (end < s.len and pc.isIdentChar(s[end])) : (end += 1) {}
        if (end == 0) continue;
        segments[seg_count] = s[0..end];
        seg_count += 1;
    }
    if (seg_count == 0) return null;

    // Import-qualified return type (e.g., "svc_mod.Service").
    if (seg_count >= 2) {
        const fn_file_id = findContainingFile(g, fn_id) orelse return null;
        const target_file_id = findImportInFile(g, fn_file_id, segments[0], scope_index, file_index) orelse return null;
        return findTypeInFile(g, target_file_id, segments[1], scope_index);
    }

    // Bare type name (e.g., "Service"): look among siblings in the same scope.
    const fn_parent = fn_node.parent_id orelse return null;
    return findTypeInFile(g, fn_parent, segments[0], scope_index);
}

/// Walk the parent chain to find the containing file node.
fn findContainingFile(g: *const Graph, node_id: NodeId) ?NodeId {
    var current = node_id;
    var hops: usize = 0;
    while (hops < 100) : (hops += 1) {
        const node = g.getNode(current) orelse return null;
        if (node.kind == .file) return current;
        current = node.parent_id orelse return null;
    }
    return null;
}

/// Find an import_decl child of a file node that matches the given name,
/// and return the target file's NodeId by resolving the import path.
fn findImportInFile(g: *const Graph, file_id: NodeId, import_name: []const u8, scope_index: *const ScopeIndex, file_index: *const FileIndex) ?NodeId {
    for (scope_index.childrenOf(file_id)) |child_idx| {
        const n = g.nodes.items[child_idx];
        if (n.kind != .import_decl) continue;
        if (!std.mem.eql(u8, n.name, import_name)) continue;
        const import_path = n.signature orelse continue;
        return file_index.findByName(import_path);
    }
    return null;
}

/// Find a type_def or enum_def child of a scope node with the given name.
fn findTypeInFile(g: *const Graph, scope_id: NodeId, type_name: []const u8, scope_index: *const ScopeIndex) ?NodeId {
    for (scope_index.childrenOf(scope_id)) |child_idx| {
        const n = g.nodes.items[child_idx];
        if (n.kind != .type_def and n.kind != .enum_def) continue;
        if (std.mem.eql(u8, n.name, type_name)) return @enumFromInt(child_idx);
    }
    return null;
}

/// For a variable assigned from an import-qualified function call (e.g.,
/// `var svc = factory_mod.create()`), try to resolve through the called
/// function's return type to find the file where the result type lives.
/// Returns the resolved file NodeId, or null to use the original target.
pub fn resolveVarTargetThroughReturnType(
    g: *const Graph,
    source: []const u8,
    var_decl: ts.Node,
    ctx: *const EdgeContext,
    k: *const KindIds,
    import_file_id: NodeId,
    scope_index: *const ScopeIndex,
    file_index: *const FileIndex,
    log: Logger,
) ?NodeId {
    // Extract the full chain from the assignment expression.
    var chain: [max_chain_depth][]const u8 = undefined;
    var chain_len: usize = 0;

    var i: u32 = 0;
    while (i < var_decl.childCount()) : (i += 1) {
        const c = var_decl.child(i) orelse continue;
        const ck = c.kindId();
        if (ck == k.call_expression or ck == k.field_expression) {
            chain_len = collectFieldExprChain(source, c, &chain, k);
            break;
        }
        if (ck == k.try_expression) {
            var j: u32 = 0;
            while (j < c.namedChildCount()) : (j += 1) {
                const inner = c.namedChild(j) orelse continue;
                const ik = inner.kindId();
                if (ik == k.call_expression or ik == k.field_expression) {
                    chain_len = collectFieldExprChain(source, inner, &chain, k);
                    break;
                }
            }
            if (chain_len > 0) break;
        }
    }

    if (chain_len < 2) {
        log.trace("var target: chain extraction failed", &.{});
        return null;
    }
    // Verify the root is the import that resolved to import_file_id.
    if (ctx.findImportTarget(chain[0]) == null) return null;

    // Walk chain[1..] to find the function node in the target file.
    var scope_id = import_file_id;
    var last_fn_id: ?NodeId = null;

    for (chain[1..chain_len]) |segment| {
        var matched: ?NodeId = null;
        for (scope_index.childrenOf(scope_id)) |child_idx| {
            const n = g.nodes.items[child_idx];
            if (!std.mem.eql(u8, n.name, segment)) continue;
            matched = @enumFromInt(child_idx);
            break;
        }
        if (matched == null) {
            if (std.mem.eql(u8, segment, "Self")) continue;
            return null;
        }
        const node = g.getNode(matched.?) orelse return null;
        if (node.kind == .function) {
            last_fn_id = matched;
        }
        if (node.kind == .type_def or node.kind == .enum_def) {
            scope_id = matched.?;
        } else {
            scope_id = node.parent_id orelse return null;
        }
    }

    const fn_id = last_fn_id orelse return null;

    // Resolve the function's return type to a type node.
    const return_type_id = resolveReturnTypeScope(g, fn_id, scope_index, file_index) orelse return null;

    // Return the file containing the return type.
    return findContainingFile(g, return_type_id);
}
