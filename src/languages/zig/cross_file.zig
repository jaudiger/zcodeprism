const std = @import("std");
const graph_mod = @import("../../core/graph.zig");
const node_mod = @import("../../core/node.zig");
const edge_mod = @import("../../core/edge.zig");
const types = @import("../../core/types.zig");
const ts = @import("tree-sitter");
const ts_api = @import("../../parser/tree_sitter_api.zig");
const ast = @import("ast_analysis.zig");

const Graph = graph_mod.Graph;
const Node = node_mod.Node;
const Edge = edge_mod.Edge;
const NodeId = types.NodeId;
const NodeKind = types.NodeKind;
const EdgeType = types.EdgeType;
const EdgeSource = types.EdgeSource;
const Visibility = types.Visibility;

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

    pub fn addBinding(self: *VarTracker, name: []const u8, target_file: NodeId) void {
        // Graceful cap: silently skip excess bindings.
        if (self.var_count >= max_vars) return;
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
/// A standard `const x = @import("path")` has the builtin_function at depth 1–2.
/// Depth 5 covers realistic patterns (e.g., `@import` inside `if` or `orelse`)
/// while bounding stack usage in the recursive descent.
pub const max_import_search_depth = 5;

// =========================================================================
// Edge utility
// =========================================================================

pub fn addEdgeIfNew(g: *Graph, source_id: NodeId, target_id: NodeId, edge_type: EdgeType) !void {
    if (source_id == target_id) return;
    for (g.edges.items) |e| {
        if (e.source_id == source_id and e.target_id == target_id and e.edge_type == edge_type) {
            return;
        }
    }
    _ = try g.addEdge(.{
        .source_id = source_id,
        .target_id = target_id,
        .edge_type = edge_type,
        .source = .tree_sitter,
    });
}

// =========================================================================
// Cross-file resolution helpers
// =========================================================================

/// Collect the chain of identifiers from a (possibly nested) field_expression.
/// For `alpha.Self.init`, populates out with ["alpha", "Self", "init"] and returns 3.
pub fn collectFieldExprChain(source: []const u8, node: ts.Node, out: *[max_chain_depth][]const u8) usize {
    return collectChainRecursive(source, node, out, 0);
}

/// Recursive helper for collectFieldExprChain. Descends into nested
/// field_expression nodes, appending each identifier segment to `out`.
fn collectChainRecursive(source: []const u8, node: ts.Node, out: *[max_chain_depth][]const u8, depth: usize) usize {
    // Graceful cap: return truncated chain, remaining segments are ignored.
    if (depth >= max_chain_depth) return depth;
    const kind = node.kind();
    if (std.mem.eql(u8, kind, "identifier")) {
        out[depth] = ts_api.nodeText(source, node);
        return depth + 1;
    }
    if (std.mem.eql(u8, kind, "field_expression")) {
        var result = depth;
        if (node.namedChild(0)) |obj| {
            result = collectChainRecursive(source, obj, out, result);
        }
        const count = node.namedChildCount();
        if (count >= 2) {
            if (node.namedChild(count - 1)) |field| {
                result = collectChainRecursive(source, field, out, result);
            }
        }
        return result;
    }
    // Unwrap call_expression to reach the function reference (first named child).
    if (std.mem.eql(u8, kind, "call_expression")) {
        if (node.namedChild(0)) |fn_ref| {
            return collectChainRecursive(source, fn_ref, out, depth);
        }
    }
    return depth;
}

/// Extract the import path from a variable_declaration containing @import("...").
/// For `const alpha = @import("alpha.zig")`, returns "alpha.zig".
pub fn extractImportPath(source: []const u8, var_decl: ts.Node) ?[]const u8 {
    return extractImportPathRecursive(source, var_decl, 0);
}

/// Recursive helper for extractImportPath. Walks AST children looking for
/// a builtin_function node containing `@import`, then extracts the string argument.
fn extractImportPathRecursive(source: []const u8, node: ts.Node, depth: u32) ?[]const u8 {
    // Graceful cap: deeply nested @import calls are not recorded.
    if (depth > max_import_search_depth) return null;
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        const kind = child.kind();
        if (std.mem.eql(u8, kind, "builtin_function")) {
            // Check if this is @import by looking for builtin_identifier.
            var j: u32 = 0;
            while (j < child.childCount()) : (j += 1) {
                const bi = child.child(j) orelse continue;
                if (std.mem.eql(u8, bi.kind(), "builtin_identifier")) {
                    if (std.mem.eql(u8, ts_api.nodeText(source, bi), "@import")) {
                        return findStringContent(source, child);
                    }
                }
            }
        }
        if (extractImportPathRecursive(source, child, depth + 1)) |path| return path;
    }
    return null;
}

/// Walk an AST subtree looking for the first `string_content` node.
/// Used to extract the path argument from `@import("path")`.
fn findStringContent(source: []const u8, node: ts.Node) ?[]const u8 {
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        if (std.mem.eql(u8, child.kind(), "string_content")) {
            return ts_api.nodeText(source, child);
        }
        if (findStringContent(source, child)) |path| return path;
    }
    return null;
}

/// Find a file node in the graph by basename (e.g., "alpha.zig").
pub fn findImportTargetFile(g: *const Graph, import_path: []const u8) ?NodeId {
    for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .file and std.mem.eql(u8, n.name, import_path)) {
            return @enumFromInt(i);
        }
    }
    return null;
}

/// Build the import map in an EdgeContext by scanning root-level variable declarations.
pub fn buildImportMap(g: *const Graph, source: []const u8, root: ts.Node, ctx: *EdgeContext) void {
    var i: u32 = 0;
    while (i < root.childCount()) : (i += 1) {
        const child = root.child(i) orelse continue;
        if (!child.isNamed()) continue;
        if (!std.mem.eql(u8, child.kind(), "variable_declaration")) continue;
        // Graceful cap: silently skip excess imports.
        if (ctx.import_count >= EdgeContext.max_imports) break;

        const name = ast.getIdentifierName(source, child) orelse continue;

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
        const import_path = extractImportPath(source, child) orelse continue;

        // Find the target file node in the graph.
        if (findImportTargetFile(g, import_path)) |target_id| {
            ctx.import_names[ctx.import_count] = name;
            ctx.import_targets[ctx.import_count] = target_id;
            ctx.import_count += 1;
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
) !void {
    var current_scope_id = target_file_id;

    for (chain, 0..) |segment, seg_idx| {
        const is_last = (seg_idx == chain.len - 1);

        // Search direct children of the current scope first.
        var matched_id: ?NodeId = null;
        for (g.nodes.items, 0..) |n, idx| {
            if (n.parent_id == null or n.parent_id.? != current_scope_id) continue;
            if (!std.mem.eql(u8, n.name, segment)) continue;
            matched_id = @enumFromInt(idx);
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

        const resolved_id = matched_id orelse return;

        const resolved_node = g.getNode(resolved_id) orelse return;

        if (is_last and is_call and resolved_node.kind == .function) {
            try addEdgeIfNew(g, caller_id, resolved_id, .calls);
        } else if (!is_last and resolved_node.kind == .function) {
            // Mid-chain function call (e.g., getClient() in svc.getClient().send()).
            // Create calls edge and follow the function's return type.
            if (is_call) {
                try addEdgeIfNew(g, caller_id, resolved_id, .calls);
            }
            if (resolveReturnTypeScope(g, resolved_id)) |return_type_id| {
                current_scope_id = return_type_id;
                continue;
            }
            return; // Cannot resolve return type; stop chain.
        } else {
            const is_type = resolved_node.kind == .type_def or resolved_node.kind == .enum_def;
            const is_type_alias = resolved_node.kind == .constant and
                resolved_node.name.len > 0 and resolved_node.name[0] >= 'A' and resolved_node.name[0] <= 'Z';
            if (is_type or is_type_alias) {
                try addEdgeIfNew(g, caller_id, resolved_id, .uses_type);
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

/// Pre-scan a function body for variable assignments from import-qualified expressions.
pub fn prescanForVarBindings(
    g: *const Graph,
    source: []const u8,
    fn_node: ts.Node,
    ctx: *const EdgeContext,
    tracker: *VarTracker,
) void {
    var i: u32 = 0;
    while (i < fn_node.childCount()) : (i += 1) {
        const child = fn_node.child(i) orelse continue;
        if (std.mem.eql(u8, child.kind(), "block")) {
            scanBlockForVarBindings(g, source, child, ctx, tracker);
            return;
        }
    }
}

/// Scan a block's children for variable_declaration nodes whose initializer
/// is rooted in an import-qualified expression. Recurses into nested blocks
/// and control-flow statements (defer, if) to catch bindings at any depth.
fn scanBlockForVarBindings(
    g: *const Graph,
    source: []const u8,
    block: ts.Node,
    ctx: *const EdgeContext,
    tracker: *VarTracker,
) void {
    var i: u32 = 0;
    while (i < block.childCount()) : (i += 1) {
        const child = block.child(i) orelse continue;
        const kind = child.kind();

        // Direct variable_declaration: `var a = alpha.Self.init(42);`
        if (std.mem.eql(u8, kind, "variable_declaration")) {
            const var_name = ast.getIdentifierName(source, child) orelse continue;
            if (findImportQualifiedRoot(source, child, ctx)) |target_file_id| {
                // Try to resolve through the called function's return type so that
                // the tracker points to the file containing the result type, not
                // the file containing the called function.
                const resolved = resolveVarTargetThroughReturnType(g, source, child, ctx, target_file_id) orelse target_file_id;
                tracker.addBinding(var_name, resolved);
            }
            continue;
        }

        // Recurse into blocks and statements that may contain variable declarations
        // (e.g., defer_statement wrapping a variable_declaration).
        if (std.mem.eql(u8, kind, "block") or
            std.mem.eql(u8, kind, "defer_statement") or
            std.mem.eql(u8, kind, "if_statement") or
            std.mem.eql(u8, kind, "expression_statement"))
        {
            scanBlockForVarBindings(g, source, child, ctx, tracker);
        }
    }
}

/// Check if a variable_declaration's value is rooted in an import-qualified expression.
/// Returns the target file NodeId if the value starts with an import name.
pub fn findImportQualifiedRoot(
    source: []const u8,
    var_decl: ts.Node,
    ctx: *const EdgeContext,
) ?NodeId {
    var i: u32 = 0;
    while (i < var_decl.childCount()) : (i += 1) {
        const child = var_decl.child(i) orelse continue;
        if (extractExpressionImportRoot(source, child, ctx)) |target| return target;
    }
    return null;
}

/// Recursively extract the root import target from an expression.
/// Handles call_expression, field_expression, and try_expression wrappers.
pub fn extractExpressionImportRoot(source: []const u8, node: ts.Node, ctx: *const EdgeContext) ?NodeId {
    const kind = node.kind();
    if (std.mem.eql(u8, kind, "call_expression") or std.mem.eql(u8, kind, "field_expression")) {
        var chain: [max_chain_depth][]const u8 = undefined;
        const chain_len = collectFieldExprChain(source, node, &chain);
        if (chain_len > 0) {
            return ctx.findImportTarget(chain[0]);
        }
    }
    // Unwrap try_expression: `try alpha.init()`
    if (std.mem.eql(u8, kind, "try_expression")) {
        var i: u32 = 0;
        while (i < node.namedChildCount()) : (i += 1) {
            const child = node.namedChild(i) orelse continue;
            if (extractExpressionImportRoot(source, child, ctx)) |target| return target;
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
pub fn resolveReturnTypeScope(g: *const Graph, fn_id: NodeId) ?NodeId {
    const fn_node = g.getNode(fn_id) orelse return null;
    const sig = fn_node.signature orelse return null;

    // Extract return type: text after the last ')' in the signature.
    const paren_pos = std.mem.lastIndexOfScalar(u8, sig, ')') orelse return null;
    if (paren_pos + 1 >= sig.len) return null;
    var return_text = std.mem.trim(u8, sig[paren_pos + 1 ..], " \t\n\r");
    if (return_text.len == 0) return null;

    // Strip error union prefix (e.g., "!void" → "void", "MyError!Type" → "Type").
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

    // Split on '.' to get segments (e.g., "svc_mod.Service" → ["svc_mod", "Service"]).
    var segments: [max_chain_depth][]const u8 = undefined;
    var seg_count: usize = 0;
    var iter = std.mem.splitScalar(u8, return_text, '.');
    while (iter.next()) |seg| {
        if (seg_count >= max_chain_depth) break;
        // Trim and extract only identifier characters.
        var s = std.mem.trim(u8, seg, " \t\n\r");
        var end: usize = 0;
        while (end < s.len and isIdentChar(s[end])) : (end += 1) {}
        if (end == 0) continue;
        segments[seg_count] = s[0..end];
        seg_count += 1;
    }
    if (seg_count == 0) return null;

    // Import-qualified return type (e.g., "svc_mod.Service").
    if (seg_count >= 2) {
        const fn_file_id = findContainingFile(g, fn_id) orelse return null;
        const target_file_id = findImportInFile(g, fn_file_id, segments[0]) orelse return null;
        return findTypeInFile(g, target_file_id, segments[1]);
    }

    // Bare type name (e.g., "Service"): look among siblings in the same scope.
    const fn_parent = fn_node.parent_id orelse return null;
    return findTypeInFile(g, fn_parent, segments[0]);
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
fn findImportInFile(g: *const Graph, file_id: NodeId, import_name: []const u8) ?NodeId {
    for (g.nodes.items) |n| {
        if (n.kind != .import_decl) continue;
        if (n.parent_id == null or n.parent_id.? != file_id) continue;
        if (!std.mem.eql(u8, n.name, import_name)) continue;
        const import_path = n.signature orelse continue;
        return findImportTargetFile(g, import_path);
    }
    return null;
}

/// Find a type_def or enum_def child of a scope node with the given name.
fn findTypeInFile(g: *const Graph, scope_id: NodeId, type_name: []const u8) ?NodeId {
    for (g.nodes.items, 0..) |n, idx| {
        if (n.parent_id == null or n.parent_id.? != scope_id) continue;
        if (n.kind != .type_def and n.kind != .enum_def) continue;
        if (std.mem.eql(u8, n.name, type_name)) return @enumFromInt(idx);
    }
    return null;
}

fn isIdentChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_';
}

/// For a variable assigned from an import-qualified function call (e.g.,
/// `var svc = factory_mod.create()`), try to resolve through the called
/// function's return type to find the file where the result type lives.
/// Returns the resolved file NodeId, or null to use the original target.
fn resolveVarTargetThroughReturnType(
    g: *const Graph,
    source: []const u8,
    var_decl: ts.Node,
    ctx: *const EdgeContext,
    import_file_id: NodeId,
) ?NodeId {
    // Extract the full chain from the assignment expression.
    var chain: [max_chain_depth][]const u8 = undefined;
    var chain_len: usize = 0;

    var i: u32 = 0;
    while (i < var_decl.childCount()) : (i += 1) {
        const c = var_decl.child(i) orelse continue;
        const ck = c.kind();
        if (std.mem.eql(u8, ck, "call_expression") or std.mem.eql(u8, ck, "field_expression")) {
            chain_len = collectFieldExprChain(source, c, &chain);
            break;
        }
        if (std.mem.eql(u8, ck, "try_expression")) {
            var j: u32 = 0;
            while (j < c.namedChildCount()) : (j += 1) {
                const inner = c.namedChild(j) orelse continue;
                const ik = inner.kind();
                if (std.mem.eql(u8, ik, "call_expression") or std.mem.eql(u8, ik, "field_expression")) {
                    chain_len = collectFieldExprChain(source, inner, &chain);
                    break;
                }
            }
            if (chain_len > 0) break;
        }
    }

    if (chain_len < 2) return null;
    // Verify the root is the import that resolved to import_file_id.
    if (ctx.findImportTarget(chain[0]) == null) return null;

    // Walk chain[1..] to find the function node in the target file.
    var scope_id = import_file_id;
    var last_fn_id: ?NodeId = null;

    for (chain[1..chain_len]) |segment| {
        var matched: ?NodeId = null;
        for (g.nodes.items, 0..) |n, idx| {
            if (n.parent_id == null or n.parent_id.? != scope_id) continue;
            if (!std.mem.eql(u8, n.name, segment)) continue;
            matched = @enumFromInt(idx);
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
    const return_type_id = resolveReturnTypeScope(g, fn_id) orelse return null;

    // Return the file containing the return type.
    return findContainingFile(g, return_type_id);
}

// =========================================================================
// Scope-aware name resolution tests (intra-file)
// =========================================================================

const parse = @import("visitor.zig").parse;
const fixtures = @import("test-fixtures");

test "duplicate structs have zero Self constants after filtering" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.duplicate_method_names, &g);

    // Assert: @This() aliases are filtered — no Self constants exist
    var self_count: usize = 0;
    for (g.nodes.items) |n| {
        if (n.kind == .constant and std.mem.eql(u8, n.name, "Self")) {
            self_count += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 0), self_count);
}

test "Alpha type_def exists without Self constant child" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.duplicate_method_names, &g);

    // Assert: Alpha type_def exists but has no Self constant child
    var alpha_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Alpha")) {
            alpha_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(alpha_id != null);

    var found_self = false;
    for (g.nodes.items) |n| {
        if (std.mem.eql(u8, n.name, "Self") and
            n.parent_id != null and n.parent_id.? == alpha_id.?)
        {
            found_self = true;
            break;
        }
    }
    try std.testing.expect(!found_self);
}

test "Beta type_def exists without Self constant child" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.duplicate_method_names, &g);

    // Assert: Beta type_def exists but has no Self constant child
    var beta_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Beta")) {
            beta_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(beta_id != null);

    var found_self = false;
    for (g.nodes.items) |n| {
        if (std.mem.eql(u8, n.name, "Self") and
            n.parent_id != null and n.parent_id.? == beta_id.?)
        {
            found_self = true;
            break;
        }
    }
    try std.testing.expect(!found_self);
}

test "Alpha.reset calls Alpha.deinit not Beta.deinit" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.duplicate_method_names, &g);

    // Assert: find Alpha, Beta, their respective deinit methods, and reset
    var alpha_id: ?NodeId = null;
    var beta_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Alpha")) {
            alpha_id = @enumFromInt(idx);
        }
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Beta")) {
            beta_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(alpha_id != null);
    try std.testing.expect(beta_id != null);

    var alpha_deinit_id: ?NodeId = null;
    var beta_deinit_id: ?NodeId = null;
    var reset_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "deinit")) {
            if (n.parent_id != null and n.parent_id.? == alpha_id.?) {
                alpha_deinit_id = @enumFromInt(idx);
            }
            if (n.parent_id != null and n.parent_id.? == beta_id.?) {
                beta_deinit_id = @enumFromInt(idx);
            }
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "reset")) {
            reset_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(alpha_deinit_id != null);
    try std.testing.expect(beta_deinit_id != null);
    try std.testing.expect(reset_id != null);

    // reset (in Alpha) should call Alpha.deinit, not Beta.deinit
    var calls_alpha_deinit = false;
    var calls_beta_deinit = false;
    for (g.edges.items) |e| {
        if (e.source_id == reset_id.? and e.edge_type == .calls) {
            if (e.target_id == alpha_deinit_id.?) calls_alpha_deinit = true;
            if (e.target_id == beta_deinit_id.?) calls_beta_deinit = true;
        }
    }
    try std.testing.expect(calls_alpha_deinit);
    try std.testing.expect(!calls_beta_deinit);
}

test "Beta.clear calls Beta.deinit not Alpha.deinit" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.duplicate_method_names, &g);

    // Assert: find Alpha, Beta, their respective deinit methods, and clear
    var alpha_id: ?NodeId = null;
    var beta_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Alpha")) {
            alpha_id = @enumFromInt(idx);
        }
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Beta")) {
            beta_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(alpha_id != null);
    try std.testing.expect(beta_id != null);

    var alpha_deinit_id: ?NodeId = null;
    var beta_deinit_id: ?NodeId = null;
    var clear_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "deinit")) {
            if (n.parent_id != null and n.parent_id.? == alpha_id.?) {
                alpha_deinit_id = @enumFromInt(idx);
            }
            if (n.parent_id != null and n.parent_id.? == beta_id.?) {
                beta_deinit_id = @enumFromInt(idx);
            }
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "clear")) {
            clear_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(alpha_deinit_id != null);
    try std.testing.expect(beta_deinit_id != null);
    try std.testing.expect(clear_id != null);

    // clear (in Beta) should call Beta.deinit, not Alpha.deinit
    var calls_alpha_deinit = false;
    var calls_beta_deinit = false;
    for (g.edges.items) |e| {
        if (e.source_id == clear_id.? and e.edge_type == .calls) {
            if (e.target_id == alpha_deinit_id.?) calls_alpha_deinit = true;
            if (e.target_id == beta_deinit_id.?) calls_beta_deinit = true;
        }
    }
    try std.testing.expect(calls_beta_deinit);
    try std.testing.expect(!calls_alpha_deinit);
}

test "Alpha.init has no uses_type edge to Beta-scoped node" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.duplicate_method_names, &g);

    // Assert: Alpha.init's uses_type edges should not target any node whose
    // parent_id is Beta. This catches the bug where Self in Alpha's return type
    // resolves to Beta or Beta.Self instead of Alpha or Alpha.Self.
    var alpha_id: ?NodeId = null;
    var beta_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Alpha")) {
            alpha_id = @enumFromInt(idx);
        }
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Beta")) {
            beta_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(alpha_id != null);
    try std.testing.expect(beta_id != null);

    var alpha_init_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "init") and
            n.parent_id != null and n.parent_id.? == alpha_id.?)
        {
            alpha_init_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(alpha_init_id != null);

    // Check that no uses_type edge from Alpha.init targets Beta or any Beta-scoped node
    for (g.edges.items) |e| {
        if (e.source_id == alpha_init_id.? and e.edge_type == .uses_type) {
            // The target should not be Beta itself
            try std.testing.expect(e.target_id != beta_id.?);
            // The target should not have Beta as parent
            const target = g.getNode(e.target_id);
            if (target) |t| {
                if (t.parent_id) |pid| {
                    try std.testing.expect(pid != beta_id.?);
                }
            }
        }
    }
}

test "generic dual Self constants are filtered out" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.generic_dual_self, &g);

    // Assert: @This() aliases are filtered — no Self constants exist
    var self_count: usize = 0;
    for (g.nodes.items) |n| {
        if (n.kind == .constant and std.mem.eql(u8, n.name, "Self")) {
            self_count += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 0), self_count);
}

test "Stack.clear calls Stack.deinit not Queue.deinit" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.generic_dual_self, &g);

    // Assert: find Stack and Queue type_defs, their deinit methods, and clear
    var stack_id: ?NodeId = null;
    var queue_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Stack")) {
            stack_id = @enumFromInt(idx);
        }
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Queue")) {
            queue_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(stack_id != null);
    try std.testing.expect(queue_id != null);

    var stack_deinit_id: ?NodeId = null;
    var queue_deinit_id: ?NodeId = null;
    var clear_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "deinit")) {
            if (n.parent_id != null and n.parent_id.? == stack_id.?) {
                stack_deinit_id = @enumFromInt(idx);
            }
            if (n.parent_id != null and n.parent_id.? == queue_id.?) {
                queue_deinit_id = @enumFromInt(idx);
            }
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "clear")) {
            clear_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(stack_deinit_id != null);
    try std.testing.expect(queue_deinit_id != null);
    try std.testing.expect(clear_id != null);

    // clear (in Stack) should call Stack.deinit, not Queue.deinit
    var calls_stack_deinit = false;
    var calls_queue_deinit = false;
    for (g.edges.items) |e| {
        if (e.source_id == clear_id.? and e.edge_type == .calls) {
            if (e.target_id == stack_deinit_id.?) calls_stack_deinit = true;
            if (e.target_id == queue_deinit_id.?) calls_queue_deinit = true;
        }
    }
    try std.testing.expect(calls_stack_deinit);
    try std.testing.expect(!calls_queue_deinit);
}

test "Queue.flush calls Queue.deinit not Stack.deinit" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.generic_dual_self, &g);

    // Assert: find Stack and Queue type_defs, their deinit methods, and flush
    var stack_id: ?NodeId = null;
    var queue_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Stack")) {
            stack_id = @enumFromInt(idx);
        }
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Queue")) {
            queue_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(stack_id != null);
    try std.testing.expect(queue_id != null);

    var stack_deinit_id: ?NodeId = null;
    var queue_deinit_id: ?NodeId = null;
    var flush_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "deinit")) {
            if (n.parent_id != null and n.parent_id.? == stack_id.?) {
                stack_deinit_id = @enumFromInt(idx);
            }
            if (n.parent_id != null and n.parent_id.? == queue_id.?) {
                queue_deinit_id = @enumFromInt(idx);
            }
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "flush")) {
            flush_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(stack_deinit_id != null);
    try std.testing.expect(queue_deinit_id != null);
    try std.testing.expect(flush_id != null);

    // flush (in Queue) should call Queue.deinit, not Stack.deinit
    var calls_stack_deinit = false;
    var calls_queue_deinit = false;
    for (g.edges.items) |e| {
        if (e.source_id == flush_id.? and e.edge_type == .calls) {
            if (e.target_id == stack_deinit_id.?) calls_stack_deinit = true;
            if (e.target_id == queue_deinit_id.?) calls_queue_deinit = true;
        }
    }
    try std.testing.expect(calls_queue_deinit);
    try std.testing.expect(!calls_stack_deinit);
}

// =========================================================================
// External method collision tests (no false call edges)
// =========================================================================

test "externalInit does not create false calls edge to local init" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.external_method_collision, &g);

    // Assert: find Resource struct, Resource.init, and externalInit
    var resource_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Resource")) {
            resource_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(resource_id != null);

    var resource_init_id: ?NodeId = null;
    var external_init_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "init") and
            n.parent_id != null and n.parent_id.? == resource_id.?)
        {
            resource_init_id = @enumFromInt(idx);
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "externalInit")) {
            external_init_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(resource_init_id != null);
    try std.testing.expect(external_init_id != null);

    // externalInit should not have a calls edge to Resource.init
    var found_false_edge = false;
    for (g.edges.items) |e| {
        if (e.source_id == external_init_id.? and e.target_id == resource_init_id.? and e.edge_type == .calls) {
            found_false_edge = true;
            break;
        }
    }
    try std.testing.expect(!found_false_edge);
}

test "externalDeinit does not create false calls edge to local deinit" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.external_method_collision, &g);

    // Assert: find Resource struct, Resource.deinit, and externalDeinit
    var resource_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Resource")) {
            resource_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(resource_id != null);

    var resource_deinit_id: ?NodeId = null;
    var external_deinit_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "deinit") and
            n.parent_id != null and n.parent_id.? == resource_id.?)
        {
            resource_deinit_id = @enumFromInt(idx);
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "externalDeinit")) {
            external_deinit_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(resource_deinit_id != null);
    try std.testing.expect(external_deinit_id != null);

    // externalDeinit should not have a calls edge to Resource.deinit
    var found_false_edge = false;
    for (g.edges.items) |e| {
        if (e.source_id == external_deinit_id.? and e.target_id == resource_deinit_id.? and e.edge_type == .calls) {
            found_false_edge = true;
            break;
        }
    }
    try std.testing.expect(!found_false_edge);
}

test "createLocalResource creates genuine calls edge to local init" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.external_method_collision, &g);

    // Assert: find Resource struct, Resource.init, and createLocalResource
    var resource_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Resource")) {
            resource_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(resource_id != null);

    var resource_init_id: ?NodeId = null;
    var create_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "init") and
            n.parent_id != null and n.parent_id.? == resource_id.?)
        {
            resource_init_id = @enumFromInt(idx);
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "createLocalResource")) {
            create_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(resource_init_id != null);
    try std.testing.expect(create_id != null);

    // createLocalResource should have a calls edge to Resource.init
    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == create_id.? and e.target_id == resource_init_id.? and e.edge_type == .calls) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

// =========================================================================
// Outer generic declaration uses_type scope tests
// =========================================================================

test "Queue has no Self constant after filtering" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.generic_dual_self, &g);

    // Assert: no Self constants exist anywhere (all @This() aliases filtered)
    for (g.nodes.items) |n| {
        if (std.mem.eql(u8, n.name, "Self")) {
            try std.testing.expect(false); // Self should not exist
        }
    }
}

test "Result has no Self constant after filtering" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g);

    // Assert: no Self constants exist (all @This() aliases filtered)
    for (g.nodes.items) |n| {
        if (std.mem.eql(u8, n.name, "Self")) {
            try std.testing.expect(false); // Self should not exist
        }
    }
}

// =========================================================================
// Self-referencing uses_type on generic type-returning functions
// =========================================================================

test "Container has no self-referencing uses_type edge" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g);

    // Assert: no uses_type edge where source_id == target_id == Container
    var container_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Container")) {
            container_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(container_id != null);

    var has_self_loop = false;
    for (g.edges.items) |e| {
        if (e.source_id == container_id.? and e.target_id == container_id.? and e.edge_type == .uses_type) {
            has_self_loop = true;
            break;
        }
    }
    try std.testing.expect(!has_self_loop);
}

test "Result has no self-referencing uses_type edge" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g);

    // Assert: no uses_type edge where source_id == target_id == Result
    var result_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Result")) {
            result_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(result_id != null);

    var has_self_loop = false;
    for (g.edges.items) |e| {
        if (e.source_id == result_id.? and e.target_id == result_id.? and e.edge_type == .uses_type) {
            has_self_loop = true;
            break;
        }
    }
    try std.testing.expect(!has_self_loop);
}

test "Stack has no self-referencing uses_type edge" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.generic_dual_self, &g);

    // Assert: no uses_type edge where source_id == target_id == Stack
    var stack_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Stack")) {
            stack_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(stack_id != null);

    var has_self_loop = false;
    for (g.edges.items) |e| {
        if (e.source_id == stack_id.? and e.target_id == stack_id.? and e.edge_type == .uses_type) {
            has_self_loop = true;
            break;
        }
    }
    try std.testing.expect(!has_self_loop);
}

test "Queue has no self-referencing uses_type edge" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.generic_dual_self, &g);

    // Assert: no uses_type edge where source_id == target_id == Queue
    var queue_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Queue")) {
            queue_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(queue_id != null);

    var has_self_loop = false;
    for (g.edges.items) |e| {
        if (e.source_id == queue_id.? and e.target_id == queue_id.? and e.edge_type == .uses_type) {
            has_self_loop = true;
            break;
        }
    }
    try std.testing.expect(!has_self_loop);
}
