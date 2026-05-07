const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const types = @import("../core/types.zig");
const node_mod = @import("../core/node.zig");

const Graph = graph_mod.Graph;
const FrozenGraph = graph_mod.FrozenGraph;
const Node = node_mod.Node;
const NodeId = types.NodeId;
const EdgeType = types.EdgeType;
const NodeKind = types.NodeKind;

pub const ImpactDependent = struct {
    node_id: NodeId,
    name: []const u8,
    kind: NodeKind,
    file_path: ?[]const u8,
};

pub const ImpactAnalysis = struct {
    total_impacted: u32,
    dependents: []const ImpactDependent,

    pub fn deinit(self: ImpactAnalysis, allocator: std.mem.Allocator) void {
        allocator.free(self.dependents);
    }
};

pub const ImpactAnalysisOptions = struct {
    max_depth: u32 = 10,
    edge_types: ?[]const EdgeType = null,
    include_parent_chain: bool = true,
};

/// Compute the combined reverse-impact set for one or more seed nodes.
/// The result includes all nodes that transitively call/use the seeds (up to
/// max_depth semantic-edge hops) and, when include_parent_chain is set, the
/// owning file/module ancestors of every impacted node.
pub fn analyzeImpact(allocator: std.mem.Allocator, fg: FrozenGraph, node_ids: []const NodeId, options: ImpactAnalysisOptions) !ImpactAnalysis {
    const g = fg.graph;
    if (node_ids.len == 0) return .{ .total_impacted = 0, .dependents = &.{} };

    const default_types = [_]EdgeType{ .calls, .uses_type, .accesses_field };
    const allowed_types: []const EdgeType = options.edge_types orelse &default_types;

    var visited = std.AutoHashMapUnmanaged(u64, void){};
    defer visited.deinit(allocator);

    const QEntry = struct { node: u64, depth: u32 };
    var queue = std.ArrayList(QEntry).empty;
    defer queue.deinit(allocator);

    // Seed all starting nodes at depth 0.
    for (node_ids) |nid| {
        if (g.getNode(nid) == null) continue;
        const raw = @intFromEnum(nid);
        const gop = try visited.getOrPut(allocator, raw);
        if (!gop.found_existing) {
            try queue.append(allocator, .{ .node = raw, .depth = 0 });
        }
    }

    // Reverse BFS following incoming semantic edges.
    var front: usize = 0;
    while (front < queue.items.len) {
        const entry = queue.items[front];
        front += 1;

        if (entry.depth < options.max_depth) {
            const in_edges = g.inEdges(@enumFromInt(entry.node));
            for (in_edges) |eid| {
                const edge = g.edges.items[@intFromEnum(eid)];

                var allowed = false;
                for (allowed_types) |et| {
                    if (edge.edge_type == et) {
                        allowed = true;
                        break;
                    }
                }
                if (!allowed) continue;

                const source_raw = @intFromEnum(edge.source_id);
                if (visited.contains(source_raw)) continue;

                try visited.put(allocator, source_raw, {});
                try queue.append(allocator, .{ .node = source_raw, .depth = entry.depth + 1 });
            }
        }
    }

    // Remove seed nodes from the impact set.
    for (node_ids) |nid| {
        _ = visited.remove(@intFromEnum(nid));
    }

    if (options.include_parent_chain) {
        var keys_buf = try allocator.alloc(u64, visited.count());
        defer allocator.free(keys_buf);
        var ki: usize = 0;
        var kit = visited.keyIterator();
        while (kit.next()) |k| {
            keys_buf[ki] = k.*;
            ki += 1;
        }
        for (keys_buf[0..ki]) |raw| {
            var parent_opt: ?NodeId = if (g.getNode(@enumFromInt(raw))) |n| n.parent_id else null;
            while (parent_opt) |pid| {
                if (pid == .root) break;
                const praw = @intFromEnum(pid);
                var is_seed = false;
                for (node_ids) |sid| {
                    if (@intFromEnum(sid) == praw) {
                        is_seed = true;
                        break;
                    }
                }
                if (!is_seed) {
                    const gop = try visited.getOrPut(allocator, praw);
                    if (gop.found_existing) break;
                }
                parent_opt = if (g.getNode(pid)) |pn| pn.parent_id else null;
            }
        }
    }

    const count = visited.count();
    if (count == 0) return .{ .total_impacted = 0, .dependents = &.{} };

    const dependents = try allocator.alloc(ImpactDependent, count);
    errdefer allocator.free(dependents);

    var pos: usize = 0;
    var it = visited.iterator();
    while (it.next()) |entry| {
        const id: NodeId = @enumFromInt(entry.key_ptr.*);
        const n = g.getNode(id).?;
        dependents[pos] = .{
            .node_id = id,
            .name = n.name,
            .kind = n.kind,
            .file_path = n.file_path,
        };
        pos += 1;
    }
    std.debug.assert(pos == count);
    errdefer comptime unreachable;

    return .{
        .total_impacted = @intCast(count),
        .dependents = dependents,
    };
}
