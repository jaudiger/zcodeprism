const std = @import("std");
const graph_mod = @import("../../core/graph.zig");
const types_mod = @import("../../core/types.zig");
const graph_index_mod = @import("../../core/graph_index.zig");
const logging = @import("../../logging.zig");
const shared_types = @import("types.zig");

const Graph = graph_mod.Graph;
const NodeId = types_mod.NodeId;
const GraphIndex = graph_index_mod.GraphIndex;
const Logger = logging.Logger;
const Field = logging.Field;
const ResolvedEdge = shared_types.ResolvedEdge;
const SymbolOrigin = shared_types.SymbolOrigin;
const max_chain_depth = shared_types.max_chain_depth;

/// Function type for resolving a function's return type to a type node.
/// Each language provides its own implementation (Zig: after `)`, Rust: after `->`).
pub const ResolveReturnTypeFn = *const fn (*const Graph, NodeId, *const GraphIndex) ?NodeId;

/// Function type for finding a named member within a type scope. Languages
/// where methods live outside the type node (impl blocks, extension blocks)
/// provide a callback; languages where methods are direct children pass null.
pub const FindInTypeScopeFn = *const fn (*const Graph, NodeId, []const u8, *const GraphIndex) ?NodeId;

/// Bundles the language-specific strategy and shared lookup state that all
/// resolve functions need. Created once per file in the edge builder and
/// passed by const pointer to each resolution call.
pub const ResolveContext = struct {
    graph_index: *const GraphIndex,
    log: Logger,
    resolve_return_type: ResolveReturnTypeFn,
    find_in_type_scope: ?FindInTypeScopeFn,
};

/// Resolve an import-qualified identifier chain against a target file.
/// Walks the chain segment by segment, narrowing scope to direct children of each
/// resolved node. Emits uses_type for type containers and calls for terminal
/// function references when is_call is true. Handles Self aliases and
/// mid-chain function calls by following return types via resolve_return_type.
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
            if (rctx.resolve_return_type(g, resolved_id, rctx.graph_index)) |return_type_id| {
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
