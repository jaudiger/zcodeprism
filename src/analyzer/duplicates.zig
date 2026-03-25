const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const types = @import("../core/types.zig");
const node_mod = @import("../core/node.zig");
const scope_mod = @import("../core/scope.zig");
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

    // Count how many qualifying functions share each hash.
    var counts = std.AutoHashMapUnmanaged(u64, u32){};
    defer counts.deinit(allocator);

    for (g.nodes.items) |n| {
        if (n.kind != .function) continue;
        if (n.external != .none) continue;
        if (options.language) |lf| {
            if (n.language == null or n.language.? != lf) continue;
        }
        if (scope_filter) |sf| {
            if (!sf.matches(n.file_path orelse continue)) continue;
        }
        const m = n.metrics orelse continue;
        if (m.structural_hash == 0) continue;
        if (m.lines < options.min_lines) continue;

        const gop = try counts.getOrPut(allocator, m.structural_hash);
        if (!gop.found_existing) gop.value_ptr.* = 0;
        gop.value_ptr.* += 1;
    }

    // Determine how many hashes have 2+ members.
    var group_count: usize = 0;
    var it = counts.iterator();
    while (it.next()) |entry| {
        if (entry.value_ptr.* >= 2) group_count += 1;
    }

    if (group_count == 0) return .{ .total_groups = 0, .groups = &.{} };

    // Allocate group array and per-group member arrays.
    const groups = try allocator.alloc(DuplicateGroup, group_count);
    errdefer {
        for (groups[0..group_count]) |gr| {
            allocator.free(gr.members);
        }
        allocator.free(groups);
    }

    var gi: usize = 0;
    it = counts.iterator();
    while (it.next()) |entry| {
        const cnt = entry.value_ptr.*;
        if (cnt < 2) continue;
        const members = try allocator.alloc(DuplicateMember, cnt);
        groups[gi] = .{ .structural_hash = entry.key_ptr.*, .similarity = 1.0, .members = members };
        // Reset to reuse as fill cursor below.
        entry.value_ptr.* = 0;
        gi += 1;
    }
    std.debug.assert(gi == group_count);

    // Map each qualifying hash to its group index.
    var hash_to_group = std.AutoHashMapUnmanaged(u64, usize){};
    defer hash_to_group.deinit(allocator);
    try hash_to_group.ensureTotalCapacity(allocator, @intCast(group_count));
    for (groups, 0..) |gr, idx| {
        hash_to_group.putAssumeCapacity(gr.structural_hash, idx);
    }

    // Fill member arrays.
    for (g.nodes.items, 0..) |n, i| {
        if (n.kind != .function) continue;
        if (n.external != .none) continue;
        if (options.language) |lf| {
            if (n.language == null or n.language.? != lf) continue;
        }
        if (scope_filter) |sf| {
            if (!sf.matches(n.file_path orelse continue)) continue;
        }
        const m = n.metrics orelse continue;
        if (m.structural_hash == 0) continue;
        if (m.lines < options.min_lines) continue;

        const gidx = hash_to_group.get(m.structural_hash) orelse continue;
        const pos_ptr = counts.getPtr(m.structural_hash).?;
        const pos = pos_ptr.*;
        const members: []DuplicateMember = @constCast(groups[gidx].members);
        members[pos] = .{
            .node_id = @enumFromInt(i),
            .name = n.name,
            .file_path = n.file_path,
        };
        pos_ptr.* += 1;
    }

    std.mem.sort(DuplicateGroup, groups, {}, struct {
        fn lessThan(_: void, a: DuplicateGroup, b: DuplicateGroup) bool {
            return a.members.len > b.members.len;
        }
    }.lessThan);

    const total: u32 = @intCast(group_count);
    const pg = pagination.paginate(total, options.offset, options.limit);

    if (pg.len == 0) return .{ .total_groups = total, .groups = &.{} };
    if (pg.start == 0 and pg.len == total) return .{ .total_groups = total, .groups = groups };

    const page = try allocator.alloc(DuplicateGroup, pg.len);
    @memcpy(page, groups[pg.start .. pg.start + pg.len]);
    // Free the non-page groups' member slices; page entries borrow the same slices.
    for (groups[0..pg.start]) |gr| allocator.free(gr.members);
    for (groups[pg.start + pg.len .. total]) |gr| allocator.free(gr.members);
    allocator.free(groups);
    return .{ .total_groups = total, .groups = page };
}

/// Cluster pre-fingerprinted candidates by Jaccard similarity using union-find.
pub fn findFuzzyDuplicates(allocator: std.mem.Allocator, fg: FrozenGraph, candidates: []const FuzzyCandidate, options: FuzzyDuplicateOptions) !DuplicateResult {
    const g = fg.graph;
    const nc = candidates.len;
    if (nc == 0) return .{ .total_groups = 0, .groups = &.{} };

    const parents = try allocator.alloc(usize, nc);
    defer allocator.free(parents);
    for (0..nc) |i| parents[i] = i;

    const min_sim_arr = try allocator.alloc(f64, nc);
    defer allocator.free(min_sim_arr);
    @memset(min_sim_arr, 1.0);

    // Pre-merge candidates with identical structural hashes (exact duplicates).
    var hash_rep = std.AutoHashMapUnmanaged(u64, usize){};
    defer hash_rep.deinit(allocator);
    for (0..nc) |i| {
        const h = candidates[i].structural_hash;
        if (h == 0) continue;
        const gop = try hash_rep.getOrPut(allocator, h);
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

    // Collect groups: map root -> list of member indices.
    var group_map = std.AutoHashMapUnmanaged(usize, std.ArrayList(NodeId)){};
    defer {
        var it = group_map.iterator();
        while (it.next()) |entry| entry.value_ptr.deinit(allocator);
        group_map.deinit(allocator);
    }
    for (0..nc) |i| {
        const root = unionFindRoot(parents, i);
        const gop = try group_map.getOrPut(allocator, root);
        if (!gop.found_existing) gop.value_ptr.* = .{};
        try gop.value_ptr.append(allocator, candidates[i].node_id);
    }

    // Build sorted group list (only multi-member groups).
    var fuzzy_groups = std.ArrayList(DuplicateGroup){};
    defer fuzzy_groups.deinit(allocator);
    var gmap_it = group_map.iterator();
    while (gmap_it.next()) |entry| {
        const members_list = entry.value_ptr;
        if (members_list.items.len < 2) continue;
        const root = entry.key_ptr.*;
        const members = try allocator.alloc(DuplicateMember, members_list.items.len);
        for (members_list.items, 0..) |nid, mi| {
            const n = g.getNode(nid) orelse continue;
            members[mi] = .{ .node_id = nid, .name = n.name, .file_path = n.file_path };
        }
        try fuzzy_groups.append(allocator, .{
            .structural_hash = 0,
            .similarity = min_sim_arr[root],
            .members = members,
        });
    }
    std.mem.sort(DuplicateGroup, fuzzy_groups.items, {}, struct {
        fn lt(_: void, a: DuplicateGroup, b: DuplicateGroup) bool {
            return a.members.len > b.members.len;
        }
    }.lt);

    const total: u32 = @intCast(fuzzy_groups.items.len);
    const pg = pagination.paginate(total, options.offset, options.limit);

    if (pg.len == 0) {
        for (fuzzy_groups.items) |gr| allocator.free(gr.members);
        return .{ .total_groups = total, .groups = &.{} };
    }

    const page = try allocator.alloc(DuplicateGroup, pg.len);
    @memcpy(page, fuzzy_groups.items[pg.start .. pg.start + pg.len]);
    for (fuzzy_groups.items[0..pg.start]) |gr| allocator.free(gr.members);
    for (fuzzy_groups.items[pg.start + pg.len .. total]) |gr| allocator.free(gr.members);
    return .{ .total_groups = total, .groups = page };
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
