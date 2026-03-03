const std = @import("std");
const node_mod = @import("node.zig");

const Node = node_mod.Node;

/// Pre-built index mapping node names to their graph indices.
/// Turns full-graph scans that match on name into a hash lookup
/// plus a short iteration over same-name nodes.
pub const NameIndex = struct {
    map: std.StringHashMapUnmanaged(Range) = .{},
    storage: []u64 = &.{},

    const Range = struct { start: u32, len: u32 };

    /// Return all node indices that share the given name.
    pub fn findByName(self: *const NameIndex, name: []const u8) []const u64 {
        const range = self.map.get(name) orelse return &.{};
        return self.storage[range.start .. range.start + range.len];
    }

    /// Build the name index from a node array using the MAF pattern.
    /// `offset` is the index of the first node to include.
    pub fn build(allocator: std.mem.Allocator, nodes: []const Node, offset: usize) !NameIndex {
        // Measure: count nodes per name.
        var name_counts = std.StringHashMapUnmanaged(u32){};
        defer name_counts.deinit(allocator);
        var total: usize = 0;
        for (nodes[offset..]) |n| {
            if (n.name.len == 0) continue;
            const gop = try name_counts.getOrPut(allocator, n.name);
            if (!gop.found_existing) gop.value_ptr.* = 0;
            gop.value_ptr.* += 1;
            total += 1;
        }
        if (total == 0) return .{};

        // Allocate: single storage array for all entries.
        const storage = try allocator.alloc(u64, total);
        errdefer allocator.free(storage);

        // Compute offsets: each name gets a contiguous slice.
        var offsets = std.StringHashMapUnmanaged(u32){};
        defer offsets.deinit(allocator);
        {
            var running: u32 = 0;
            var it = name_counts.iterator();
            while (it.next()) |entry| {
                try offsets.put(allocator, entry.key_ptr.*, running);
                running += entry.value_ptr.*;
            }
        }

        // Fill: populate storage with node indices.
        var write_pos = std.StringHashMapUnmanaged(u32){};
        defer write_pos.deinit(allocator);
        {
            var it = offsets.iterator();
            while (it.next()) |entry| {
                try write_pos.put(allocator, entry.key_ptr.*, entry.value_ptr.*);
            }
        }
        for (nodes[offset..], offset..) |n, i| {
            if (n.name.len == 0) continue;
            if (write_pos.getPtr(n.name)) |pos| {
                storage[pos.*] = i;
                pos.* += 1;
            }
        }

        // Build the final map with Range values.
        var map = std.StringHashMapUnmanaged(Range){};
        errdefer map.deinit(allocator);
        {
            var it = offsets.iterator();
            while (it.next()) |entry| {
                const name = entry.key_ptr.*;
                const start = entry.value_ptr.*;
                const count = name_counts.get(name).?;
                try map.put(allocator, name, .{ .start = start, .len = count });
            }
        }

        return .{ .map = map, .storage = storage };
    }

    /// Free the map and storage array.
    pub fn deinit(self: *NameIndex, allocator: std.mem.Allocator) void {
        self.map.deinit(allocator);
        if (self.storage.len > 0) allocator.free(self.storage);
    }
};

// -- Tests --

test "findByName returns matches and empty for absent names" {
    // Arrange
    const nodes: []const Node = &.{
        .{ .id = @enumFromInt(0), .name = "init", .kind = .function, .language = .zig },
        .{ .id = @enumFromInt(1), .name = "deinit", .kind = .function, .language = .zig },
        .{ .id = @enumFromInt(2), .name = "init", .kind = .type_def, .language = .zig },
    };
    var idx = try NameIndex.build(std.testing.allocator, nodes, 0);
    defer idx.deinit(std.testing.allocator);

    // Assert: single match returns correct index
    const deinits = idx.findByName("deinit");
    try std.testing.expectEqual(@as(usize, 1), deinits.len);
    try std.testing.expectEqual(@as(u64, 1), deinits[0]);

    // Assert: duplicate names return all matches
    try std.testing.expectEqual(@as(usize, 2), idx.findByName("init").len);

    // Assert: absent name returns empty
    try std.testing.expectEqual(@as(usize, 0), idx.findByName("nonexistent").len);

    // Assert: storage covers all named nodes
    try std.testing.expectEqual(@as(usize, 3), idx.storage.len);
}

test "build on empty nodes or nodes with empty names" {
    // Arrange: empty array
    var idx1 = try NameIndex.build(std.testing.allocator, &.{}, 0);
    defer idx1.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 0), idx1.storage.len);

    // Arrange: empty-name nodes are skipped
    const nodes: []const Node = &.{
        .{ .id = @enumFromInt(0), .name = "", .kind = .function, .language = .zig },
        .{ .id = @enumFromInt(1), .name = "real", .kind = .function, .language = .zig },
    };
    var idx2 = try NameIndex.build(std.testing.allocator, nodes, 0);
    defer idx2.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 1), idx2.storage.len);
    try std.testing.expectEqual(@as(usize, 0), idx2.findByName("").len);
    try std.testing.expectEqual(@as(usize, 1), idx2.findByName("real").len);
}

test "build with offset skips earlier nodes" {
    // Arrange
    const nodes: []const Node = &.{
        .{ .id = @enumFromInt(0), .name = "skipped", .kind = .function, .language = .zig },
        .{ .id = @enumFromInt(1), .name = "included", .kind = .function, .language = .zig },
    };

    // Act: offset=1 skips node 0
    var idx = try NameIndex.build(std.testing.allocator, nodes, 1);
    defer idx.deinit(std.testing.allocator);

    // Assert
    try std.testing.expectEqual(@as(usize, 0), idx.findByName("skipped").len);
    try std.testing.expectEqual(@as(usize, 1), idx.findByName("included").len);
    try std.testing.expectEqual(@as(u64, 1), idx.findByName("included")[0]);
}
