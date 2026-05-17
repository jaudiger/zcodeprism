const std = @import("std");
const graph_mod = @import("../../core/graph.zig");
const node_mod = @import("../../core/node.zig");
const types_mod = @import("../../core/types.zig");
const graph_index_mod = @import("../../core/graph_index.zig");
const file_index_mod = @import("../../core/file_index.zig");
const logging = @import("../../logging.zig");
const shared_types = @import("types.zig");

const Graph = graph_mod.Graph;
const Node = node_mod.Node;
const NodeId = types_mod.NodeId;
const GraphIndex = graph_index_mod.GraphIndex;
const FileIndex = file_index_mod.FileIndex;
const Logger = logging.Logger;
const Field = logging.Field;
const ResolvedEdge = shared_types.ResolvedEdge;
const SymbolOrigin = shared_types.SymbolOrigin;
const EdgeContext = shared_types.EdgeContext;
const max_chain_depth = shared_types.max_chain_depth;

/// Splits a function's return-type text out of `sig` and writes the type-name
/// segments into `out` in left-to-right order, setting `count` to the number
/// of segments written. The caller provides a buffer of size `max_chain_depth`.
/// Each language strips its own wrapper syntax and splits on its own scope
/// separator before emitting segments.
pub const ParseReturnSegmentsFn = *const fn (
    sig: []const u8,
    out: *[max_chain_depth][]const u8,
    count: *usize,
) void;

/// Resolves an `import_decl` graph node to the file it imports.
/// `importer_path` is the path of the file containing the import (for relative
/// resolution); `decl` is the `import_decl` node. Returning null means the
/// import target could not be located in `file_index`.
pub const ResolveModuleTargetFn = *const fn (
    file_index: *const FileIndex,
    importer_path: ?[]const u8,
    decl: Node,
) ?NodeId;

/// Function type for finding a named member within a type scope. Languages
/// where methods live outside the type node (impl blocks, extension blocks)
/// provide a callback; languages where methods are direct children pass null.
pub const FindInTypeScopeFn = *const fn (*const Graph, NodeId, []const u8, *const GraphIndex) ?NodeId;

/// The language-specific surface needed to resolve a function's return type
/// to a type node: a signature parser and an `import_decl` target resolver.
pub const ReturnTypeResolver = struct {
    parse_return_type_segments: ParseReturnSegmentsFn,
    resolve_module_target: ResolveModuleTargetFn,
};

/// Bundles the language-specific strategy and shared lookup state that all
/// resolve functions need. Created once per file in the edge builder and
/// passed by const pointer to each resolution call. Each callback documents
/// its contract on the corresponding `*Fn` typedef above.
pub const ResolveContext = struct {
    graph_index: *const GraphIndex,
    io: std.Io,
    log: Logger,
    return_type_resolver: ReturnTypeResolver,
    find_in_type_scope: ?FindInTypeScopeFn,
};

/// Find an import_decl child of a file node that matches the given name,
/// and return the target file's NodeId by resolving via the language-provided
/// `resolve_target` callback.
pub fn findImportInFile(
    g: *const Graph,
    file_id: NodeId,
    import_name: []const u8,
    graph_index: *const GraphIndex,
    resolve_target: ResolveModuleTargetFn,
) ?NodeId {
    const scope_index = &graph_index.scope;
    const file_index = &graph_index.files;
    const file_node = g.getNode(file_id) orelse return null;
    const importer_path = file_node.file_path;

    for (scope_index.childrenOf(file_id)) |child_idx| {
        const n = g.nodes.items[child_idx];
        if (n.kind != .import_decl) continue;
        if (!std.mem.eql(u8, n.name, import_name)) continue;
        if (resolve_target(file_index, importer_path, n)) |target_id| {
            return target_id;
        }
    }
    return null;
}

/// Resolve a function's return type to a type node in the graph.
/// Defers signature parsing to `resolver.parse_return_type_segments`, then
/// walks the resulting segments. Module-qualified return types resolve
/// through `resolver.resolve_module_target`.
pub fn resolveReturnTypeScope(
    g: *const Graph,
    fn_id: NodeId,
    graph_index: *const GraphIndex,
    resolver: *const ReturnTypeResolver,
) ?NodeId {
    const scope_index = &graph_index.scope;
    const fn_node = g.getNode(fn_id) orelse return null;
    const sig = fn_node.signature orelse return null;

    var segments: [max_chain_depth][]const u8 = undefined;
    var seg_count: usize = 0;
    resolver.parse_return_type_segments(sig, &segments, &seg_count);
    if (seg_count == 0) return null;

    if (seg_count >= 2) {
        const fn_file_id = g.findContainingFile(fn_id) orelse return null;
        const target_file_id = findImportInFile(g, fn_file_id, segments[0], graph_index, resolver.resolve_module_target) orelse return null;
        return g.findTypeAmongChildren(scope_index.childrenOf(target_file_id), segments[seg_count - 1]);
    }

    const fn_parent = fn_node.parent_id orelse return null;
    return g.findTypeAmongChildren(scope_index.childrenOf(fn_parent), segments[0]);
}

/// Resolve a variable's target file through the return type of its initializer.
/// The caller has already extracted the qualified chain from the variable's
/// initializer. Looks up the root via `ctx.findImportOrigin`, walks the
/// effective chain to find the called function, then resolves the function's
/// return type and returns the file containing that type.
pub fn resolveVarTargetThroughReturnType(
    g: *const Graph,
    ctx: *const EdgeContext,
    chain: []const []const u8,
    graph_index: *const GraphIndex,
    resolver: *const ReturnTypeResolver,
    log: Logger,
) ?NodeId {
    const scope_index = &graph_index.scope;

    if (chain.len == 0) {
        log.trace("var target: chain extraction failed", &.{});
        return null;
    }

    const origin = ctx.findImportOrigin(chain[0]) orelse return null;

    var effective: [max_chain_depth][]const u8 = undefined;
    var eff_len: usize = 0;
    for (origin.chain) |seg| {
        if (eff_len >= max_chain_depth) break;
        effective[eff_len] = seg;
        eff_len += 1;
    }
    for (chain[1..]) |seg| {
        if (eff_len >= max_chain_depth) break;
        effective[eff_len] = seg;
        eff_len += 1;
    }

    if (eff_len == 0) return null;

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
    const return_type_id = resolveReturnTypeScope(g, fn_id, graph_index, resolver) orelse return null;
    return g.findContainingFile(return_type_id);
}

/// Resolve an import-qualified identifier chain against a target file.
/// Walks the chain segment by segment, narrowing scope to direct children of each
/// resolved node. Emits uses_type for type containers and calls for terminal
/// function references when is_call is true. Handles Self aliases and
/// mid-chain function calls by following return types via the resolver in rctx.
///
/// Returns the number of resolved edges written to `out`.
pub fn resolveQualifiedCall(
    g: *const Graph,
    target_file_id: NodeId,
    chain: []const []const u8,
    is_call: bool,
    rctx: *const ResolveContext,
    out: []ResolvedEdge,
) usize {
    const scope_index = &rctx.graph_index.scope;
    var current_scope_id = target_file_id;
    var count: usize = 0;

    for (chain, 0..) |segment, seg_idx| {
        const is_last = (seg_idx == chain.len - 1);

        var matched_id: ?NodeId = null;
        for (scope_index.childrenOf(current_scope_id)) |child_idx| {
            const n = g.nodes.items[child_idx];
            if (!std.mem.eql(u8, n.name, segment)) continue;
            matched_id = @enumFromInt(child_idx);
            break;
        }

        if (matched_id == null) {
            matched_id = scope_index.findUniqueDescendant(g.nodes.items, current_scope_id, segment);
        }

        // Fallback: ask the language to search impl blocks or equivalent.
        if (matched_id == null) {
            if (rctx.find_in_type_scope) |find_fn| {
                matched_id = find_fn(g, current_scope_id, segment, rctx.graph_index);
            }
        }

        if (matched_id == null and std.mem.eql(u8, segment, "Self")) {
            const scope_node = g.getNode(current_scope_id) orelse return count;
            if (scope_node.kind.isTypeContainer() or scope_node.kind == .file) {
                continue;
            }
            return count;
        }

        const resolved_id = matched_id orelse {
            rctx.log.trace("qualified call: segment not found", &.{Field.string("segment", segment)});
            return count;
        };

        const resolved_node = g.getNode(resolved_id) orelse return count;

        if (is_last and is_call and resolved_node.kind == .function) {
            if (count < out.len) {
                out[count] = .{ .target_id = resolved_id, .edge_type = .calls };
                count += 1;
            }
        } else if (!is_last and resolved_node.kind == .function) {
            if (is_call and count < out.len) {
                out[count] = .{ .target_id = resolved_id, .edge_type = .calls };
                count += 1;
            }
            if (resolveReturnTypeScope(g, resolved_id, rctx.graph_index, &rctx.return_type_resolver)) |return_type_id| {
                current_scope_id = return_type_id;
                continue;
            }
            rctx.log.trace("qualified call: return type unresolvable", &.{});
            return count;
        } else {
            const is_type = resolved_node.kind.isTypeContainer();
            const is_type_alias = resolved_node.kind == .constant and
                resolved_node.name.len > 0 and resolved_node.name[0] >= 'A' and resolved_node.name[0] <= 'Z';
            if ((is_type or is_type_alias) and count < out.len) {
                out[count] = .{ .target_id = resolved_id, .edge_type = .uses_type };
                count += 1;
            }
        }

        if (resolved_node.kind.isTypeContainer()) {
            current_scope_id = resolved_id;
        } else {
            current_scope_id = resolved_node.parent_id orelse return count;
        }
    }
    return count;
}

/// Resolve a qualified chain and add the resulting edges to the graph.
/// Returns true if at least one edge was resolved.
pub fn addResolvedEdges(
    allocator: std.mem.Allocator,
    graph: *Graph,
    caller_id: NodeId,
    target_file_id: NodeId,
    chain: []const []const u8,
    is_call: bool,
    rctx: *const ResolveContext,
) !bool {
    var edge_buf: [max_chain_depth]ResolvedEdge = undefined;
    const edge_count = resolveQualifiedCall(
        graph,
        target_file_id,
        chain,
        is_call,
        rctx,
        &edge_buf,
    );
    for (edge_buf[0..edge_count]) |edge| {
        _ = try graph.addEdgeIfNew(allocator, .{
            .source_id = caller_id,
            .target_id = edge.target_id,
            .edge_type = edge.edge_type,
        });
    }
    return edge_count > 0;
}

/// Merge origin chain with call chain, then call addResolvedEdges.
/// Returns true if at least one edge was resolved.
pub fn resolveOriginCall(
    allocator: std.mem.Allocator,
    graph: *Graph,
    caller_id: NodeId,
    origin: SymbolOrigin,
    call_chain: []const []const u8,
    is_call: bool,
    rctx: *const ResolveContext,
) !bool {
    var merged: [max_chain_depth][]const u8 = undefined;
    var len: usize = 0;
    for (origin.chain) |seg| {
        if (len >= max_chain_depth) break;
        merged[len] = seg;
        len += 1;
    }
    for (call_chain) |seg| {
        if (len >= max_chain_depth) break;
        merged[len] = seg;
        len += 1;
    }
    if (len == 0) return false;
    return try addResolvedEdges(allocator, graph, caller_id, origin.file_id, merged[0..len], is_call, rctx);
}
