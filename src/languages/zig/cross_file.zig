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

/// A symbol origin: identifies a node within a target file by file id and access chain.
/// An empty chain refers to the module itself; a non-empty chain contains the
/// identifier segments extracted from post-import field accesses (e.g. ["MyType", "init"]).
pub const SymbolOrigin = struct {
    file_id: NodeId,
    chain: []const []const u8,
};

/// Context for cross-file edge creation during a single file parse.
/// Holds the file's node scope range and a fixed-capacity map of @import bindings
/// to their resolved target file NodeIds and extraction chains.
pub const EdgeContext = struct {
    scope_start: usize,
    scope_end: usize,
    import_names: [max_imports][]const u8 = undefined,
    import_targets: [max_imports]NodeId = undefined,
    import_chains: [max_imports][max_chain_depth][]const u8 = undefined,
    import_chain_lens: [max_imports]usize = undefined,
    import_count: usize = 0,

    /// Maximum number of tracked @import bindings per file for cross-file edge resolution.
    pub const max_imports = 32;

    /// Look up the target file NodeId for an import binding by name.
    /// Returns the file_id only, ignoring any extraction chain.
    pub fn findImportTarget(self: *const EdgeContext, name: []const u8) ?NodeId {
        for (self.import_names[0..self.import_count], self.import_targets[0..self.import_count]) |iname, itarget| {
            if (std.mem.eql(u8, iname, name)) return itarget;
        }
        return null;
    }

    /// Look up the full SymbolOrigin (file id + extraction chain) for an import binding by name.
    pub fn findImportOrigin(self: *const EdgeContext, name: []const u8) ?SymbolOrigin {
        for (0..self.import_count) |i| {
            if (std.mem.eql(u8, self.import_names[i], name)) {
                return .{
                    .file_id = self.import_targets[i],
                    .chain = self.import_chains[i][0..self.import_chain_lens[i]],
                };
            }
        }
        return null;
    }
};

/// Tracks variable-to-file bindings within a function scope.
/// Stores mappings from local variable names to the target file NodeId they
/// were assigned from via import-qualified expressions, so that later method
/// calls (e.g. `a.deinit()` where `a = alpha.Self.init(42)`) can be resolved
/// to the correct cross-file target.
pub const VarTracker = struct {
    var_names: [max_vars][]const u8 = undefined,
    var_targets: [max_vars]NodeId = undefined,
    var_count: usize = 0,

    /// Maximum number of tracked import-qualified variable bindings per function.
    pub const max_vars = 32;

    /// Record a variable-to-file binding. Logs a warning and drops the binding
    /// if the tracker is already at capacity.
    pub fn addBinding(self: *VarTracker, name: []const u8, target_file: NodeId, log: Logger) void {
        if (self.var_count >= max_vars) {
            log.warn("var tracker at capacity, some import-qualified variable edges will be missing", &.{
                Field.uint("capacity", max_vars),
                Field.string("dropped", name),
            });
            return;
        }
        self.var_names[self.var_count] = name;
        self.var_targets[self.var_count] = target_file;
        self.var_count += 1;
    }

    /// Return the target file NodeId associated with a variable name, or null if not tracked.
    pub fn findTarget(self: *const VarTracker, name: []const u8) ?NodeId {
        for (self.var_names[0..self.var_count], self.var_targets[0..self.var_count]) |vname, vtarget| {
            if (std.mem.eql(u8, vname, name)) return vtarget;
        }
        return null;
    }
};

/// Maximum depth for field_expression chains (e.g. `a.b.c.d`).
/// 8 covers realistic Zig qualified paths while bounding stack usage.
pub const max_chain_depth = 8;

/// Maximum AST depth for scanForCalls and scanForTypeIdentifiersScoped.
/// Bounds stack usage in recursive AST traversal.
pub const max_ast_scan_depth: u32 = 256;

/// Maximum AST depth when searching for `@import` calls inside a declaration.
/// Depth 5 covers realistic patterns (e.g. `@import` inside `if` or `orelse`)
/// while bounding stack usage.
pub const max_import_search_depth = 5;

/// Collect the chain of identifier segments from a (possibly nested) field_expression.
/// For `alpha.Self.init`, populates `out` with ["alpha", "Self", "init"] and returns 3.
/// Truncates silently at `max_chain_depth`.
pub fn collectFieldExprChain(source: []const u8, node: ts.Node, out: *[max_chain_depth][]const u8, k: *const KindIds) usize {
    return collectChainRecursive(source, node, out, k, 0);
}

/// Recursive helper for collectFieldExprChain. Descends into nested
/// field_expression nodes, appending each identifier segment to `out`.
fn collectChainRecursive(source: []const u8, node: ts.Node, out: *[max_chain_depth][]const u8, k: *const KindIds, depth: usize) usize {
    // Graceful cap: return truncated chain, remaining segments are ignored.
    if (depth >= max_chain_depth) return depth;
    const kid = node.kindId();
    if (kid == k.identifier or kid == k.property_identifier) {
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

/// Extract the import path string from a variable_declaration that contains `@import("...")`.
/// For `const alpha = @import("alpha.zig")`, returns `"alpha.zig"`.
/// Returns null if the declaration does not contain an @import call.
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

/// Extract the post-import field access chain from a variable declaration.
/// For `const T = @import("foo.zig").Bar.Baz`, populates `chain` with
/// ["Bar", "Baz"] and returns 2. Returns 0 if no field_expression follows the import.
pub fn extractImportExtractionChain(
    source: []const u8,
    var_decl: ts.Node,
    chain: *[max_chain_depth][]const u8,
    k: *const KindIds,
) usize {
    var i: u32 = 0;
    while (i < var_decl.namedChildCount()) : (i += 1) {
        const child = var_decl.namedChild(i) orelse continue;
        if (child.kindId() == k.field_expression) {
            // collectFieldExprChain skips nodes it doesn't recognize
            // (like builtin_function), so only the identifier segments
            // after the @import are collected.
            return collectFieldExprChain(source, child, chain, k);
        }
    }
    return 0;
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

/// Populate the import map in `ctx` by scanning root-level variable declarations.
/// For each declaration classified as import_decl, extracts the import path, resolves
/// it to a target file NodeId via `file_index`, and records the binding with its
/// extraction chain. Logs a warning when the import map reaches capacity.
pub fn buildImportMap(g: *const Graph, source: []const u8, root: ts.Node, ctx: *EdgeContext, file_index: *const FileIndex, importer_path: ?[]const u8, k: *const KindIds, log: Logger) void {
    var i: u32 = 0;
    while (i < root.childCount()) : (i += 1) {
        const child = root.child(i) orelse continue;
        if (!child.isNamed()) continue;
        if (child.kindId() != k.variable_declaration) continue;
        if (ctx.import_count >= EdgeContext.max_imports) {
            log.warn("import map at capacity, some cross-file edges will be missing", &.{
                Field.uint("capacity", EdgeContext.max_imports),
            });
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

        // Find the target file node in the graph using directory-relative resolution.
        if (file_index.resolve(importer_path, import_path)) |target_id| {
            ctx.import_names[ctx.import_count] = name;
            ctx.import_targets[ctx.import_count] = target_id;
            // Extract post-import field chain.
            var ext_chain: [max_chain_depth][]const u8 = undefined;
            const ext_len = extractImportExtractionChain(source, child, &ext_chain, k);
            for (0..ext_len) |ci| {
                ctx.import_chains[ctx.import_count][ci] = ext_chain[ci];
            }
            ctx.import_chain_lens[ctx.import_count] = ext_len;
            ctx.import_count += 1;
        } else {
            log.trace("import target file not found", &.{Field.string("path", import_path)});
        }
    }
}

/// Resolve an import-qualified identifier chain against a target file and create edges.
/// Walks the chain segment by segment, narrowing scope to direct children of each
/// resolved node. Creates `uses_type` edges for type containers and `calls` edges
/// for terminal function references when `is_call` is true. Handles `Self` aliases
/// and mid-chain function calls by following return types.
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
            if (scope_node.kind.isTypeContainer() or scope_node.kind == .file) {
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
            _ = try g.addEdgeIfNew(.{ .source_id = caller_id, .target_id = resolved_id, .edge_type = .calls });
        } else if (!is_last and resolved_node.kind == .function) {
            // Mid-chain function call (e.g., getClient() in svc.getClient().send()).
            // Create calls edge and follow the function's return type.
            if (is_call) {
                _ = try g.addEdgeIfNew(.{ .source_id = caller_id, .target_id = resolved_id, .edge_type = .calls });
            }
            if (resolveReturnTypeScope(g, resolved_id, scope_index, file_index)) |return_type_id| {
                current_scope_id = return_type_id;
                continue;
            }
            log.trace("qualified call: return type unresolvable", &.{});
            return; // Cannot resolve return type; stop chain.
        } else {
            const is_type = resolved_node.kind.isTypeContainer();
            const is_type_alias = resolved_node.kind == .constant and
                resolved_node.name.len > 0 and resolved_node.name[0] >= 'A' and resolved_node.name[0] <= 'Z';
            if (is_type or is_type_alias) {
                _ = try g.addEdgeIfNew(.{ .source_id = caller_id, .target_id = resolved_id, .edge_type = .uses_type });
            }
        }

        // Narrow scope for next segment.
        if (resolved_node.kind.isTypeContainer()) {
            // Type container: next segment is a child of this type.
            current_scope_id = resolved_id;
        } else {
            // Constant/alias (e.g. renamed alias): next segment is a sibling (same parent).
            current_scope_id = resolved_node.parent_id orelse return;
        }
    }
}

/// Check whether a variable_declaration's initializer is rooted in an import-qualified expression.
/// Scans the declaration's children for call, field, or try expressions whose root
/// identifier matches a known import name in `ctx`.
/// Returns the target file NodeId if found, null otherwise.
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

/// Recursively extract the root import target from an expression node.
/// Unwraps call_expression, field_expression, and try_expression wrappers,
/// collects the identifier chain, and looks up the first segment in `ctx`.
/// Returns the target file NodeId if the root identifier is a known import, null otherwise.
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

/// Resolve a function's return type to a type node in the graph.
/// Parses the return type from the function's stored signature text, stripping
/// error unions, pointers, and optional markers. Handles import-qualified types
/// (e.g. `svc_mod.Service`) by resolving the import in the containing file.
/// Returns the type's NodeId for use as scope in further chain resolution,
/// or null if the return type cannot be resolved.
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
    // Get the file node's file_path for directory-relative resolution.
    const file_node = g.getNode(file_id) orelse return null;
    const importer_path = file_node.file_path;

    for (scope_index.childrenOf(file_id)) |child_idx| {
        const n = g.nodes.items[child_idx];
        if (n.kind != .import_decl) continue;
        if (!std.mem.eql(u8, n.name, import_name)) continue;
        const import_path = n.signature orelse continue;
        return file_index.resolve(importer_path, import_path);
    }
    return null;
}

/// Find a type container child (type_def, enum_def, or union_def) of a scope node with the given name.
fn findTypeInFile(g: *const Graph, scope_id: NodeId, type_name: []const u8, scope_index: *const ScopeIndex) ?NodeId {
    for (scope_index.childrenOf(scope_id)) |child_idx| {
        const n = g.nodes.items[child_idx];
        if (!n.kind.isTypeContainer()) continue;
        if (std.mem.eql(u8, n.name, type_name)) return @enumFromInt(child_idx);
    }
    return null;
}

/// Resolve a variable's target file through the return type of its initializer.
/// For a variable assigned from an import-qualified function call, extracts the
/// full chain, walks it in the target file to locate the called function, then
/// resolves that function's return type to find the file containing the result type.
/// Returns the resolved file NodeId, or null if resolution fails (caller should
/// fall back to the original import target).
pub fn resolveVarTargetThroughReturnType(
    g: *const Graph,
    source: []const u8,
    var_decl: ts.Node,
    ctx: *const EdgeContext,
    k: *const KindIds,
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

    if (chain_len == 0) {
        log.trace("var target: chain extraction failed", &.{});
        return null;
    }

    // Look up the origin for the root name (includes extraction chain).
    const origin = ctx.findImportOrigin(chain[0]) orelse return null;

    // Build effective chain: origin.chain ++ chain[1..chain_len].
    var effective: [max_chain_depth][]const u8 = undefined;
    var eff_len: usize = 0;
    for (origin.chain) |seg| {
        if (eff_len >= max_chain_depth) break;
        effective[eff_len] = seg;
        eff_len += 1;
    }
    for (chain[1..chain_len]) |seg| {
        if (eff_len >= max_chain_depth) break;
        effective[eff_len] = seg;
        eff_len += 1;
    }

    if (eff_len == 0) return null;

    // Walk effective chain to find the function node in the target file.
    var scope_id = origin.file_id;
    var last_fn_id: ?NodeId = null;

    for (effective[0..eff_len]) |segment| {
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
        if (node.kind.isTypeContainer()) {
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
