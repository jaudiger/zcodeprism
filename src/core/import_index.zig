const std = @import("std");
const edge_mod = @import("edge.zig");
const types = @import("types.zig");

const Edge = edge_mod.Edge;
const NodeId = types.NodeId;

/// Pre-built index mapping file NodeIds to their import target NodeIds.
/// Built from the edge array after edge building completes, so phantom
/// resolution can look up a file's imports without scanning all edges.
pub const ImportIndex = struct {
    map: std.AutoHashMapUnmanaged(NodeId, Range) = .{},
    storage: []NodeId = &.{},

    const Range = struct { start: u32, len: u32 };

    /// Return the import target NodeIds for a given file.
    pub fn targetsOf(self: *const ImportIndex, file_id: NodeId) []const NodeId {
        const range = self.map.get(file_id) orelse return &.{};
        return self.storage[range.start .. range.start + range.len];
    }

    /// Build the import index from an edge array using the MAF pattern.
    pub fn build(allocator: std.mem.Allocator, edges: []const Edge) !ImportIndex {
        // Measure: count import edges per source file.
        var counts = std.AutoHashMapUnmanaged(NodeId, u32){};
        defer counts.deinit(allocator);
        var total: usize = 0;
        for (edges) |e| {
            if (e.edge_type != .imports) continue;
            const gop = try counts.getOrPut(allocator, e.source_id);
            if (!gop.found_existing) gop.value_ptr.* = 0;
            gop.value_ptr.* += 1;
            total += 1;
        }
        if (total == 0) return .{};

        // Allocate flat storage for all target NodeIds.
        const storage = try allocator.alloc(NodeId, total);
        errdefer allocator.free(storage);

        // Compute offsets via prefix sum and build the range map.
        var map = std.AutoHashMapUnmanaged(NodeId, Range){};
        errdefer map.deinit(allocator);
        {
            var running: u32 = 0;
            var it = counts.iterator();
            while (it.next()) |entry| {
                try map.put(allocator, entry.key_ptr.*, .{ .start = running, .len = entry.value_ptr.* });
                running += entry.value_ptr.*;
            }
            std.debug.assert(running == total);
        }

        // Fill: place target NodeIds into their source file's slot.
        // Reuse counts as write-position offsets (reset to 0 first).
        {
            var it = counts.valueIterator();
            while (it.next()) |v| v.* = 0;
        }
        for (edges) |e| {
            if (e.edge_type != .imports) continue;
            const range = map.get(e.source_id).?;
            const offset = counts.getPtr(e.source_id).?;
            storage[range.start + offset.*] = e.target_id;
            offset.* += 1;
        }

        // Assert fill completeness.
        {
            var it = map.iterator();
            while (it.next()) |entry| {
                std.debug.assert(counts.get(entry.key_ptr.*).? == entry.value_ptr.*.len);
            }
        }

        return .{ .map = map, .storage = storage };
    }

    /// Free the map and flat storage array.
    pub fn deinit(self: *ImportIndex, allocator: std.mem.Allocator) void {
        self.map.deinit(allocator);
        if (self.storage.len > 0) allocator.free(self.storage);
    }
};

test "targetsOf returns import targets and empty for absent files" {
    // Arrange
    const file_a: NodeId = @enumFromInt(0);
    const file_b: NodeId = @enumFromInt(1);
    const file_c: NodeId = @enumFromInt(2);
    const edges: []const Edge = &.{
        .{ .source_id = file_a, .target_id = file_b, .edge_type = .imports },
        .{ .source_id = file_a, .target_id = file_c, .edge_type = .imports },
        .{ .source_id = file_b, .target_id = file_c, .edge_type = .imports },
        .{ .source_id = file_a, .target_id = file_b, .edge_type = .calls },
    };

    // Act
    var idx = try ImportIndex.build(std.testing.allocator, edges);
    defer idx.deinit(std.testing.allocator);

    // Assert
    const a_targets = idx.targetsOf(file_a);
    try std.testing.expectEqual(@as(usize, 2), a_targets.len);

    const b_targets = idx.targetsOf(file_b);
    try std.testing.expectEqual(@as(usize, 1), b_targets.len);
    try std.testing.expectEqual(file_c, b_targets[0]);

    try std.testing.expectEqual(@as(usize, 0), idx.targetsOf(file_c).len);
    try std.testing.expectEqual(@as(usize, 0), idx.targetsOf(@enumFromInt(99)).len);
}

test "build on empty edges returns empty index" {
    // Act
    var idx = try ImportIndex.build(std.testing.allocator, &.{});
    defer idx.deinit(std.testing.allocator);

    // Assert
    try std.testing.expectEqual(@as(usize, 0), idx.targetsOf(.root).len);
    try std.testing.expectEqual(@as(usize, 0), idx.storage.len);
}

test "build ignores non-import edges" {
    // Arrange
    const edges: []const Edge = &.{
        .{ .source_id = .root, .target_id = @enumFromInt(1), .edge_type = .calls },
        .{ .source_id = .root, .target_id = @enumFromInt(2), .edge_type = .uses_type },
    };

    // Act
    var idx = try ImportIndex.build(std.testing.allocator, edges);
    defer idx.deinit(std.testing.allocator);

    // Assert
    try std.testing.expectEqual(@as(usize, 0), idx.targetsOf(.root).len);
    try std.testing.expectEqual(@as(usize, 0), idx.storage.len);
}
