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
        // Single map reused across three passes: count, prefix-sum, fill.
        var map = std.StringHashMapUnmanaged(Range){};
        errdefer map.deinit(allocator);

        // Count pass: accumulate node count in range.len; range.start stays 0.
        var total: usize = 0;
        for (nodes[offset..]) |n| {
            if (n.name.len == 0) continue;
            const gop = try map.getOrPut(allocator, n.name);
            if (!gop.found_existing) gop.value_ptr.* = .{ .start = 0, .len = 0 };
            gop.value_ptr.len += 1;
            total += 1;
        }
        if (total == 0) return .{};

        // Allocate storage for all entries in one shot.
        const storage = try allocator.alloc(u64, total);
        errdefer allocator.free(storage);

        // Prefix-sum pass: convert len (count) into start offset, reset len to 0.
        {
            var running: u32 = 0;
            var it = map.iterator();
            while (it.next()) |entry| {
                const count = entry.value_ptr.len;
                entry.value_ptr.start = running;
                entry.value_ptr.len = 0;
                running += count;
            }
        }

        // Fill pass: write node indices into storage; len tracks write position.
        for (nodes[offset..], offset..) |n, i| {
            if (n.name.len == 0) continue;
            const range = map.getPtr(n.name).?;
            storage[range.start + range.len] = i;
            range.len += 1;
        }

        return .{ .map = map, .storage = storage };
    }

    /// Free the map and storage array.
    pub fn deinit(self: *NameIndex, allocator: std.mem.Allocator) void {
        self.map.deinit(allocator);
        if (self.storage.len > 0) allocator.free(self.storage);
    }
};

test "findByName returns matches and empty for absent names" {
    // Arrange
    const nodes: []const Node = &.{
        .{ .id = @enumFromInt(0), .name = "init", .kind = .function, .language = .zig },
        .{ .id = @enumFromInt(1), .name = "deinit", .kind = .function, .language = .zig },
        .{ .id = @enumFromInt(2), .name = "init", .kind = .type_def, .language = .zig },
    };

    // Act
    var idx = try NameIndex.build(std.testing.allocator, nodes, 0);
    defer idx.deinit(std.testing.allocator);

    // Assert
    const deinits = idx.findByName("deinit");
    try std.testing.expectEqual(@as(usize, 1), deinits.len);
    try std.testing.expectEqual(@as(u64, 1), deinits[0]);
    try std.testing.expectEqual(@as(usize, 2), idx.findByName("init").len);
    try std.testing.expectEqual(@as(usize, 0), idx.findByName("nonexistent").len);
    try std.testing.expectEqual(@as(usize, 3), idx.storage.len);
}

test "build on empty nodes returns empty index" {
    // Arrange / Act
    var idx = try NameIndex.build(std.testing.allocator, &.{}, 0);
    defer idx.deinit(std.testing.allocator);

    // Assert
    try std.testing.expectEqual(@as(usize, 0), idx.storage.len);
}

test "build skips nodes with empty names" {
    // Arrange
    const nodes: []const Node = &.{
        .{ .id = @enumFromInt(0), .name = "", .kind = .function, .language = .zig },
        .{ .id = @enumFromInt(1), .name = "real", .kind = .function, .language = .zig },
    };

    // Act
    var idx = try NameIndex.build(std.testing.allocator, nodes, 0);
    defer idx.deinit(std.testing.allocator);

    // Assert
    try std.testing.expectEqual(@as(usize, 1), idx.storage.len);
    try std.testing.expectEqual(@as(usize, 0), idx.findByName("").len);
    try std.testing.expectEqual(@as(usize, 1), idx.findByName("real").len);
}

test "build with offset skips earlier nodes" {
    // Arrange
    const nodes: []const Node = &.{
        .{ .id = @enumFromInt(0), .name = "skipped", .kind = .function, .language = .zig },
        .{ .id = @enumFromInt(1), .name = "included", .kind = .function, .language = .zig },
    };

    // Act
    var idx = try NameIndex.build(std.testing.allocator, nodes, 1);
    defer idx.deinit(std.testing.allocator);

    // Assert
    try std.testing.expectEqual(@as(usize, 0), idx.findByName("skipped").len);
    try std.testing.expectEqual(@as(usize, 1), idx.findByName("included").len);
    try std.testing.expectEqual(@as(u64, 1), idx.findByName("included")[0]);
}
