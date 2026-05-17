const std = @import("std");
const graph_mod = @import("../../core/graph.zig");
const logging = @import("../../logging.zig");
const node_mod = @import("../../core/node.zig");
const edge_mod = @import("../../core/edge.zig");
const types = @import("../../core/types.zig");
const ts = @import("tree-sitter");
const ts_api = @import("../../parser/tree_sitter_api.zig");
const source_scan = @import("../../parser/source_scan.zig");
const ast = @import("ast_analysis.zig");
const pc = @import("parse_context.zig");
const shared_types = @import("../shared/types.zig");
const shared_resolve = @import("../shared/resolve.zig");

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
const GraphIndex = @import("../../core/graph_index.zig").GraphIndex;
const FileIndex = @import("../../core/file_index.zig").FileIndex;

pub const SymbolOrigin = shared_types.SymbolOrigin;
pub const ImportEntry = shared_types.ImportEntry;
pub const EdgeContext = shared_types.EdgeContext;
pub const ResolvedEdge = shared_types.ResolvedEdge;
pub const max_chain_depth = shared_types.max_chain_depth;
pub const max_ast_scan_depth = shared_types.max_ast_scan_depth;
pub const resolveQualifiedCall = shared_resolve.resolveQualifiedCall;

/// Maximum AST depth when searching for `@import` calls inside a declaration.
/// Depth 5 covers realistic patterns (nested `@import` inside conditionals)
/// while bounding stack usage.
pub const max_import_search_depth = 5;

/// Collect the chain of identifier segments from a (possibly nested) field_expression.
/// Populates `out` with each segment in order and returns the count.
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

/// Extract the import path string from a variable_declaration that contains an @import call.
/// Returns the path argument as a string slice, or null if no @import is found.
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
/// Populates `chain` with the identifier segments after the @import call and
/// returns the count. Returns 0 if no field_expression follows the import.
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
            // collectFieldExprChain skips unrecognized node kinds, so
            // only the identifier segments after the @import are collected.
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
pub fn buildImportMap(allocator: std.mem.Allocator, g: *const Graph, source: []const u8, root: ts.Node, ctx: *EdgeContext, file_index: *const FileIndex, importer_path: ?[]const u8, k: *const KindIds, log: Logger) !void {
    var i: u32 = 0;
    while (i < root.childCount()) : (i += 1) {
        const child = root.child(i) orelse continue;
        if (!child.isNamed()) continue;
        if (child.kindId() != k.variable_declaration) continue;

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
        if (pc.resolveFileImport(file_index, importer_path, import_path)) |target_id| {
            var entry = ImportEntry{ .name = name, .file_id = target_id };
            var ext_chain: [max_chain_depth][]const u8 = undefined;
            const ext_len = extractImportExtractionChain(source, child, &ext_chain, k);
            for (0..ext_len) |ci| {
                entry.chain[ci] = ext_chain[ci];
            }
            entry.chain_len = ext_len;
            try ctx.imports.append(allocator, entry);
        } else {
            log.trace("import target file not found", &.{Field.string("path", import_path)});
        }
    }

    try buildAliasMemberMap(allocator, source, root, ctx, k);
}

/// Second pass over root-level variable declarations.
/// For each `const X = mod.member;` pattern whose root is a known import,
/// registers X as an alias pointing to the same target file with the combined chain.
/// This allows bare calls to re-exported symbols to resolve across files.
fn buildAliasMemberMap(
    allocator: std.mem.Allocator,
    source: []const u8,
    root: ts.Node,
    ctx: *EdgeContext,
    k: *const KindIds,
) !void {
    var i: u32 = 0;
    while (i < root.childCount()) : (i += 1) {
        const child = root.child(i) orelse continue;
        if (!child.isNamed()) continue;
        if (child.kindId() != k.variable_declaration) continue;

        const alias_name = ast.getIdentifierName(source, child, k) orelse continue;
        if (ctx.findImportOrigin(alias_name) != null) continue;

        var j: u32 = 0;
        while (j < child.namedChildCount()) : (j += 1) {
            const val = child.namedChild(j) orelse continue;
            if (val.kindId() != k.field_expression) continue;

            var chain: [max_chain_depth][]const u8 = undefined;
            const chain_len = collectFieldExprChain(source, val, &chain, k);
            if (chain_len < 2) break;

            const origin = ctx.findImportOrigin(chain[0]) orelse break;

            var entry = ImportEntry{ .name = alias_name, .file_id = origin.file_id };
            var new_len: usize = 0;
            for (origin.chain) |seg| {
                if (new_len >= max_chain_depth) break;
                entry.chain[new_len] = seg;
                new_len += 1;
            }
            for (chain[1..chain_len]) |seg| {
                if (new_len >= max_chain_depth) break;
                entry.chain[new_len] = seg;
                new_len += 1;
            }
            entry.chain_len = new_len;
            try ctx.imports.append(allocator, entry);
            break;
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
        if (extractExpressionImportRoot(source, child, ctx, k, 0)) |target| return target;
    }
    return null;
}

/// Recursively extract the root import target from an expression node.
/// Unwraps call_expression, field_expression, and try_expression wrappers,
/// collects the identifier chain, and looks up the first segment in `ctx`.
/// Returns the target file NodeId if the root identifier is a known import, null otherwise.
pub fn extractExpressionImportRoot(source: []const u8, node: ts.Node, ctx: *const EdgeContext, k: *const KindIds, depth: u32) ?NodeId {
    if (depth >= max_ast_scan_depth) return null;
    const kid = node.kindId();
    if (kid == k.call_expression or kid == k.field_expression) {
        var chain: [max_chain_depth][]const u8 = undefined;
        const chain_len = collectFieldExprChain(source, node, &chain, k);
        if (chain_len > 0) {
            return ctx.findImportTarget(chain[0]);
        }
    }
    if (kid == k.try_expression) {
        var i: u32 = 0;
        while (i < node.namedChildCount()) : (i += 1) {
            const child = node.namedChild(i) orelse continue;
            if (extractExpressionImportRoot(source, child, ctx, k, depth + 1)) |target| return target;
        }
    }
    return null;
}

/// Zig-specific return-type signature parser. Matches the
/// `shared_resolve.ParseReturnSegmentsFn` contract.
pub fn parseReturnTypeSegments(
    sig: []const u8,
    out: *[max_chain_depth][]const u8,
    count: *usize,
) void {
    count.* = 0;

    // Return type is the text after the last ')' in the signature.
    const paren_pos = std.mem.lastIndexOfScalar(u8, sig, ')') orelse return;
    if (paren_pos + 1 >= sig.len) return;
    var return_text = std.mem.trim(u8, sig[paren_pos + 1 ..], " \t\n\r");
    if (return_text.len == 0) return;

    if (std.mem.indexOfScalar(u8, return_text, '!')) |bang_pos| {
        return_text = return_text[bang_pos + 1 ..];
    }

    while (return_text.len > 0 and (return_text[0] == '*' or return_text[0] == '?')) {
        return_text = return_text[1..];
    }
    if (std.mem.startsWith(u8, return_text, "const ")) {
        return_text = return_text[6..];
    }
    return_text = std.mem.trim(u8, return_text, " \t\n\r");
    if (return_text.len == 0) return;

    var iter = std.mem.splitScalar(u8, return_text, '.');
    while (iter.next()) |seg| {
        if (count.* >= max_chain_depth) break;
        var s = std.mem.trim(u8, seg, " \t\n\r");
        var end: usize = 0;
        while (end < s.len and source_scan.isIdentChar(s[end])) : (end += 1) {}
        if (end == 0) continue;
        out[count.*] = s[0..end];
        count.* += 1;
    }
}

/// Zig-specific `import_decl` resolver. Matches the
/// `shared_resolve.ResolveModuleTargetFn` contract.
pub fn resolveModuleTarget(
    file_index: *const @import("../../core/file_index.zig").FileIndex,
    importer_path: ?[]const u8,
    decl: @import("../../core/node.zig").Node,
) ?NodeId {
    const import_path = decl.signature orelse return null;
    return pc.resolveFileImport(file_index, importer_path, import_path);
}

/// The Zig instance of `shared_resolve.ReturnTypeResolver`.
pub const return_type_resolver = shared_resolve.ReturnTypeResolver{
    .parse_return_type_segments = parseReturnTypeSegments,
    .resolve_module_target = resolveModuleTarget,
};

/// Resolve a function's return type to a type node using Zig's signature
/// parser and module-target resolver.
pub fn resolveReturnTypeScope(g: *const Graph, fn_id: NodeId, graph_index: *const GraphIndex) ?NodeId {
    return shared_resolve.resolveReturnTypeScope(g, fn_id, graph_index, &return_type_resolver);
}

/// Resolve a variable's target file through the return type of its initializer.
/// Extracts the qualified chain from the Zig AST then delegates to the shared
/// algorithm.
pub fn resolveVarTargetThroughReturnType(
    g: *const Graph,
    source: []const u8,
    var_decl: ts.Node,
    ctx: *const EdgeContext,
    k: *const KindIds,
    graph_index: *const GraphIndex,
    log: Logger,
) ?NodeId {
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

    return shared_resolve.resolveVarTargetThroughReturnType(
        g,
        ctx,
        chain[0..chain_len],
        graph_index,
        &return_type_resolver,
        log,
    );
}
