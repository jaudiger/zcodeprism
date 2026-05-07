const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const types = @import("../core/types.zig");
const node_mod = @import("../core/node.zig");
const scope_mod = @import("../core/scope.zig");
const filter = @import("filter.zig");
const pagination = @import("pagination.zig");

const Graph = graph_mod.Graph;
const FrozenGraph = graph_mod.FrozenGraph;
const Node = node_mod.Node;
const NodeId = types.NodeId;
const NodeKind = types.NodeKind;
const Language = types.Language;
const ExternalInfo = @import("../core/lang_meta.zig").ExternalInfo;
const Scope = scope_mod.Scope;

pub const DuplicateMember = struct {
    node_id: NodeId,
    name: []const u8,
    file_path: ?[]const u8,
};

pub const DuplicateGroup = struct {
    structural_hash: u64,
    similarity: f64,
    members: []const DuplicateMember,
};

pub const DuplicateResult = struct {
    total_groups: u32,
    groups: []const DuplicateGroup,

    pub fn deinit(self: DuplicateResult, allocator: std.mem.Allocator) void {
        for (self.groups) |group| {
            allocator.free(group.members);
        }
        allocator.free(self.groups);
    }
};

pub const DuplicateOptions = struct {
    min_lines: u32 = 3,
    scope: ?[]const u8 = null,
    language: ?Language = null,
    offset: u32 = 0,
    limit: u32 = 10,
};

pub const Fingerprint = [512]u16;

pub const FuzzyCandidate = struct {
    node_id: NodeId,
    structural_hash: u64,
    fingerprint: Fingerprint,
    valid: bool,
};

pub const FuzzyDuplicateOptions = struct {
    threshold: f64 = 0.75,
    offset: u32 = 0,
    limit: u32 = 10,
};

/// Find groups of functions with identical structural hashes.
pub fn findDuplicates(allocator: std.mem.Allocator, fg: FrozenGraph, options: DuplicateOptions) !DuplicateResult {
    const g = fg.graph;
    const scope_filter: ?Scope = if (options.scope) |s| Scope.parse(s) else null;

    var arena_state = std.heap.ArenaAllocator.init(allocator);
    defer arena_state.deinit();
    const a = arena_state.allocator();

    // Single pass: map hash -> list of qualifying node indices.
    var hash_map = std.AutoHashMapUnmanaged(u64, std.ArrayList(u64)){};
    for (g.nodes.items, 0..) |n, i| {
        if (n.kind != .function) continue;
        if (n.external != .none) continue;
        if (options.language) |lf| {
            if (n.language == null or n.language.? != lf) continue;
        }
        if (!filter.passesScope(scope_filter, n.file_path)) continue;
        const m = n.metrics orelse continue;
        if (m.structural_hash == 0) continue;
        if (m.lines < options.min_lines) continue;

        const gop = try hash_map.getOrPut(a, m.structural_hash);
        if (!gop.found_existing) gop.value_ptr.* = .empty;
        try gop.value_ptr.append(a, @as(u64, i));
    }

    // Collect multi-member groups into a flat sortable list.
    const GroupBuild = struct { hash: u64, indices: []u64 };
    var group_list = std.ArrayList(GroupBuild).empty;
    var hit = hash_map.iterator();
    while (hit.next()) |entry| {
        if (entry.value_ptr.items.len < 2) continue;
        try group_list.append(a, .{
            .hash = entry.key_ptr.*,
            .indices = entry.value_ptr.items,
        });
    }

    std.mem.sort(GroupBuild, group_list.items, {}, struct {
        fn lt(_: void, x: GroupBuild, y: GroupBuild) bool {
            return x.indices.len > y.indices.len;
        }
    }.lt);

    const total: u32 = @intCast(group_list.items.len);
    const pg = pagination.paginate(total, options.offset, options.limit);
    if (pg.len == 0) return .{ .total_groups = total, .groups = &.{} };

    // Materialize member slices only for the page window.
    const groups = try allocator.alloc(DuplicateGroup, pg.len);
    var materialized: usize = 0;
    errdefer {
        for (groups[0..materialized]) |gr| allocator.free(gr.members);
        allocator.free(groups);
    }

    for (group_list.items[pg.start .. pg.start + pg.len], 0..) |gb, gi| {
        const members = try allocator.alloc(DuplicateMember, gb.indices.len);
        for (gb.indices, 0..) |idx, mi| {
            const n = &g.nodes.items[idx];
            members[mi] = .{
                .node_id = @enumFromInt(idx),
                .name = n.name,
                .file_path = n.file_path,
            };
        }
        groups[gi] = .{ .structural_hash = gb.hash, .similarity = 1.0, .members = members };
        materialized += 1;
    }
    errdefer comptime unreachable;
    return .{ .total_groups = total, .groups = groups };
}

/// Cluster pre-fingerprinted candidates by Jaccard similarity using union-find.
pub fn findFuzzyDuplicates(allocator: std.mem.Allocator, fg: FrozenGraph, candidates: []const FuzzyCandidate, options: FuzzyDuplicateOptions) !DuplicateResult {
    const g = fg.graph;
    const nc = candidates.len;
    if (nc == 0) return .{ .total_groups = 0, .groups = &.{} };

    var arena_state = std.heap.ArenaAllocator.init(allocator);
    defer arena_state.deinit();
    const a = arena_state.allocator();

    const parents = try a.alloc(usize, nc);
    for (0..nc) |i| parents[i] = i;

    const min_sim_arr = try a.alloc(f64, nc);
    @memset(min_sim_arr, 1.0);

    // Pre-merge candidates with identical structural hashes (exact duplicates).
    var hash_rep = std.AutoHashMapUnmanaged(u64, usize){};
    for (0..nc) |i| {
        const h = candidates[i].structural_hash;
        if (h == 0) continue;
        const gop = try hash_rep.getOrPut(a, h);
        if (gop.found_existing) {
            const ri = unionFindRoot(parents, gop.value_ptr.*);
            const rj = unionFindRoot(parents, i);
            if (ri != rj) parents[ri] = rj;
        } else {
            gop.value_ptr.* = i;
        }
    }

    // Pairwise Jaccard on precomputed fingerprints.
    for (0..nc) |i| {
        if (!candidates[i].valid) continue;
        for (i + 1..nc) |j| {
            if (!candidates[j].valid) continue;
            const sim = countsJaccard(&candidates[i].fingerprint, &candidates[j].fingerprint);
            if (sim >= options.threshold) {
                const ri = unionFindRoot(parents, i);
                const rj = unionFindRoot(parents, j);
                if (ri != rj) {
                    parents[ri] = rj;
                    min_sim_arr[rj] = @min(@min(min_sim_arr[ri], min_sim_arr[rj]), sim);
                } else {
                    min_sim_arr[rj] = @min(min_sim_arr[rj], sim);
                }
            }
        }
    }

    // Map root -> member NodeId list.
    var group_map = std.AutoHashMapUnmanaged(usize, std.ArrayList(NodeId)){};
    for (0..nc) |i| {
        const root = unionFindRoot(parents, i);
        const gop = try group_map.getOrPut(a, root);
        if (!gop.found_existing) gop.value_ptr.* = .empty;
        try gop.value_ptr.append(a, candidates[i].node_id);
    }

    // Collect multi-member groups into a flat sortable list.
    const FuzzyGroupBuild = struct { root: usize, node_ids: []const NodeId, sim: f64 };
    var group_list = std.ArrayList(FuzzyGroupBuild).empty;
    var gmap_it = group_map.iterator();
    while (gmap_it.next()) |entry| {
        if (entry.value_ptr.items.len < 2) continue;
        const root = entry.key_ptr.*;
        try group_list.append(a, .{
            .root = root,
            .node_ids = entry.value_ptr.items,
            .sim = min_sim_arr[root],
        });
    }

    std.mem.sort(FuzzyGroupBuild, group_list.items, {}, struct {
        fn lt(_: void, x: FuzzyGroupBuild, y: FuzzyGroupBuild) bool {
            return x.node_ids.len > y.node_ids.len;
        }
    }.lt);

    const total: u32 = @intCast(group_list.items.len);
    const pg = pagination.paginate(total, options.offset, options.limit);
    if (pg.len == 0) return .{ .total_groups = total, .groups = &.{} };

    // Materialize member slices only for the page window.
    const groups = try allocator.alloc(DuplicateGroup, pg.len);
    var materialized: usize = 0;
    errdefer {
        for (groups[0..materialized]) |gr| allocator.free(gr.members);
        allocator.free(groups);
    }

    for (group_list.items[pg.start .. pg.start + pg.len], 0..) |gb, gi| {
        const members = try allocator.alloc(DuplicateMember, gb.node_ids.len);
        for (gb.node_ids, 0..) |nid, mi| {
            const n = g.getNode(nid).?;
            members[mi] = .{ .node_id = nid, .name = n.name, .file_path = n.file_path };
        }
        groups[gi] = .{ .structural_hash = 0, .similarity = gb.sim, .members = members };
        materialized += 1;
    }
    errdefer comptime unreachable;
    return .{ .total_groups = total, .groups = groups };
}

/// Multiset Jaccard similarity over two pre-computed frequency arrays.
fn countsJaccard(a: *const Fingerprint, b: *const Fingerprint) f64 {
    var intersection: u64 = 0;
    var union_sum: u64 = 0;
    for (0..512) |i| {
        intersection += @min(a[i], b[i]);
        union_sum += @max(a[i], b[i]);
    }
    if (union_sum == 0) return 1.0;
    return @as(f64, @floatFromInt(intersection)) / @as(f64, @floatFromInt(union_sum));
}

/// Path-compressing union-find root lookup.
fn unionFindRoot(parents: []usize, i: usize) usize {
    var x = i;
    while (parents[x] != x) {
        parents[x] = parents[parents[x]];
        x = parents[x];
    }
    return x;
}
