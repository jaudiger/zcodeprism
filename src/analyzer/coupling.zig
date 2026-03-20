const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const types = @import("../core/types.zig");
const node_mod = @import("../core/node.zig");
const scope_mod = @import("../core/scope.zig");

const Graph = graph_mod.Graph;
const FrozenGraph = graph_mod.FrozenGraph;
const Node = node_mod.Node;
const NodeId = types.NodeId;
const NodeKind = types.NodeKind;
const Language = types.Language;
const Scope = scope_mod.Scope;

pub const Granularity = enum { file, directory };

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
        allocator.free(self.pairs);
    }
};

pub const CouplingOptions = struct {
    min_coupling: f64 = 1.0,
    top_n: u32 = 20,
    scope: ?[]const u8 = null,
    granularity: Granularity = .file,
    include_external: bool = false,
    language: ?Language = null,
};

/// Find pairs of files (or directories) that share cross-unit edges.
pub fn findCoupling(allocator: std.mem.Allocator, fg: FrozenGraph, options: CouplingOptions) !CouplingResult {
    const g = fg.graph;
    const scope_filter: ?Scope = if (options.scope) |s| Scope.parse(s) else null;

    // unit_of maps every node index to its owning unit (file or directory index).
    const unit_of = switch (options.granularity) {
        .file => try buildFileOwnerMap(allocator, g),
        .directory => try buildDirectoryOwnerMap(allocator, g),
    };
    defer allocator.free(unit_of);

    // Count cross-unit edges per ordered unit pair.
    var pair_counts = std.AutoHashMapUnmanaged(u64, u32){};
    defer pair_counts.deinit(allocator);

    for (g.edges.items) |edge| {
        const src_idx = @intFromEnum(edge.source_id);
        const tgt_idx = @intFromEnum(edge.target_id);
        if (src_idx >= unit_of.len or tgt_idx >= unit_of.len) continue;

        const source_unit = unit_of[src_idx];
        const target_unit = unit_of[tgt_idx];
        if (source_unit == std.math.maxInt(u32) or target_unit == std.math.maxInt(u32)) continue;
        if (source_unit == target_unit) continue;

        const unit_a_node = g.getNode(@enumFromInt(@as(u64, source_unit))) orelse continue;
        const unit_b_node = g.getNode(@enumFromInt(@as(u64, target_unit))) orelse continue;

        if (!options.include_external) {
            if (unit_a_node.external != .none or unit_b_node.external != .none) continue;
        }

        if (options.language) |lf| {
            if (unit_a_node.language == null or unit_a_node.language.? != lf) continue;
        }

        if (scope_filter) |sf| {
            const path_a = unit_a_node.file_path orelse continue;
            if (!sf.matches(path_a)) continue;
        }

        const key = packPair(@min(source_unit, target_unit), @max(source_unit, target_unit));

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

/// Maps every node to the index of its nearest directory ancestor (or itself if it is one).
fn buildDirectoryOwnerMap(allocator: std.mem.Allocator, g: *const Graph) ![]u32 {
    const n = g.nodes.items.len;
    const map = try allocator.alloc(u32, n);
    @memset(map, std.math.maxInt(u32));

    for (g.nodes.items, 0..) |node, i| {
        if (node.kind == .directory) map[i] = @intCast(i);
    }

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
