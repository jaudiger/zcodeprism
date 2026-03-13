const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const types = @import("../core/types.zig");
const node_mod = @import("../core/node.zig");
const scope_mod = @import("../core/scope.zig");

const Graph = graph_mod.Graph;
const Node = node_mod.Node;
const NodeId = types.NodeId;
const NodeKind = types.NodeKind;
const Language = types.Language;
const ExternalInfo = @import("../languages/language.zig").ExternalInfo;
const Scope = scope_mod.Scope;

pub const DuplicateMember = struct {
    node_id: NodeId,
    name: []const u8,
    file_path: ?[]const u8,
};

pub const DuplicateGroup = struct {
    structural_hash: u32,
    similarity: f64,
    members: []const DuplicateMember,
};

pub const DuplicateResult = struct {
    total_groups: u32,
    groups: []const DuplicateGroup,

    pub fn deinit(self: DuplicateResult, allocator: std.mem.Allocator) void {
        for (self.groups) |group| {
            if (group.members.len > 0) allocator.free(group.members);
        }
        if (self.groups.len > 0) allocator.free(self.groups);
    }
};

pub const DuplicateOptions = struct {
    min_lines: u32 = 3,
    scope: ?[]const u8 = null,
    language: ?Language = null,
    offset: u32 = 0,
    limit: u32 = 10,
};

/// Find groups of functions with identical structural hashes.
pub fn findDuplicates(allocator: std.mem.Allocator, g: *const Graph, options: DuplicateOptions) !DuplicateResult {
    const scope_filter: ?Scope = if (options.scope) |s| Scope.parse(s) else null;

    // Count how many qualifying functions share each hash.
    var counts = std.AutoHashMapUnmanaged(u32, u32){};
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
            if (gr.members.len > 0) allocator.free(gr.members);
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
    var hash_to_group = std.AutoHashMapUnmanaged(u32, usize){};
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
    const offset = @min(options.offset, total);
    const end = @min(offset + options.limit, total);
    const page_len = end - offset;

    if (page_len == 0) return .{ .total_groups = total, .groups = &.{} };
    if (offset == 0 and end == total) return .{ .total_groups = total, .groups = groups };

    const page = try allocator.alloc(DuplicateGroup, page_len);
    @memcpy(page, groups[offset..end]);
    // Free the non-page groups' member slices; page entries borrow the same slices.
    for (groups[0..offset]) |gr| allocator.free(gr.members);
    for (groups[end..total]) |gr| allocator.free(gr.members);
    allocator.free(groups);
    return .{ .total_groups = total, .groups = page };
}
