const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const types = @import("../core/types.zig");
const node_mod = @import("../core/node.zig");

const Graph = graph_mod.Graph;
const Node = node_mod.Node;
const NodeId = types.NodeId;
const NodeKind = types.NodeKind;

pub const CouplingPair = struct {
    file_a: NodeId,
    file_b: NodeId,
    module_a: []const u8,
    module_b: []const u8,
    shared_edges: u32,
    score: f64,
};

pub const CouplingResult = struct {
    pairs: []const CouplingPair,

    pub fn deinit(self: CouplingResult, allocator: std.mem.Allocator) void {
        if (self.pairs.len > 0) allocator.free(self.pairs);
    }
};

pub const CouplingOptions = struct {
    min_coupling: f64 = 1.0,
    top_n: u32 = 20,
};

/// Find pairs of files that share cross-file edges.
pub fn findCoupling(allocator: std.mem.Allocator, g: *const Graph, options: CouplingOptions) !CouplingResult {
    // Precompute node -> owning file for every node.
    const file_of = try buildFileOwnerMap(allocator, g);
    defer allocator.free(file_of);

    // Count cross-file edges per ordered file pair.
    var pair_counts = std.AutoHashMapUnmanaged(u64, u32){};
    defer pair_counts.deinit(allocator);

    for (g.edges.items) |edge| {
        const src_idx = @intFromEnum(edge.source_id);
        const tgt_idx = @intFromEnum(edge.target_id);
        if (src_idx >= file_of.len or tgt_idx >= file_of.len) continue;

        const source_file = file_of[src_idx];
        const target_file = file_of[tgt_idx];
        if (source_file == std.math.maxInt(u32) or target_file == std.math.maxInt(u32)) continue;
        if (source_file == target_file) continue;

        const key = packPair(@min(source_file, target_file), @max(source_file, target_file));

        const gop = try pair_counts.getOrPut(allocator, key);
        if (!gop.found_existing) gop.value_ptr.* = 0;
        gop.value_ptr.* += 1;
    }

    var pairs = std.ArrayList(CouplingPair){};
    defer pairs.deinit(allocator);

    var it = pair_counts.iterator();
    while (it.next()) |entry| {
        const count = entry.value_ptr.*;
        const score: f64 = @floatFromInt(count);
        if (score < options.min_coupling) continue;

        const ab = unpackPair(entry.key_ptr.*);
        const file_a: NodeId = @enumFromInt(@as(u64, ab[0]));
        const file_b: NodeId = @enumFromInt(@as(u64, ab[1]));
        const node_a = g.getNode(file_a) orelse continue;
        const node_b = g.getNode(file_b) orelse continue;

        try pairs.append(allocator, .{
            .file_a = file_a,
            .file_b = file_b,
            .module_a = node_a.name,
            .module_b = node_b.name,
            .shared_edges = count,
            .score = score,
        });
    }

    if (pairs.items.len == 0) return .{ .pairs = &.{} };

    std.mem.sort(CouplingPair, pairs.items, {}, struct {
        fn lessThan(_: void, a: CouplingPair, b: CouplingPair) bool {
            return a.score > b.score;
        }
    }.lessThan);

    const take = @min(pairs.items.len, options.top_n);
    const result = try allocator.alloc(CouplingPair, take);
    @memcpy(result, pairs.items[0..take]);
    return .{ .pairs = result };
}

/// Build a flat array mapping node index -> owning file node index.
/// Nodes without a file ancestor get `maxInt(u32)`.
fn buildFileOwnerMap(allocator: std.mem.Allocator, g: *const Graph) ![]u32 {
    const n = g.nodes.items.len;
    const map = try allocator.alloc(u32, n);
    @memset(map, std.math.maxInt(u32));

    for (g.nodes.items, 0..) |node, i| {
        if (node.kind == .file) {
            map[i] = @intCast(i);
        }
    }

    // Propagate file ownership down the parent chain. Nodes are stored in
    // tree order (parent index < child index), so a forward pass suffices.
    for (g.nodes.items, 0..) |node, i| {
        if (map[i] != std.math.maxInt(u32)) continue;
        const pid = node.parent_id orelse continue;
        const pi = @intFromEnum(pid);
        if (pi < n) map[i] = map[pi];
    }

    return map;
}

fn packPair(a: u32, b: u32) u64 {
    return (@as(u64, a) << 32) | @as(u64, b);
}

fn unpackPair(key: u64) [2]u32 {
    return .{ @truncate(key >> 32), @truncate(key) };
}
