const std = @import("std");
const node_mod = @import("node.zig");
const types = @import("types.zig");

const Node = node_mod.Node;
const NodeId = types.NodeId;

/// Pre-built index mapping parent node IDs to their direct children.
/// Replaces full scans of `g.nodes.items` that filter by `parent_id`
/// with a hash lookup and small slice iteration.
pub const ScopeIndex = struct {
    map: std.AutoHashMapUnmanaged(u64, Range) = .{},
    storage: []u64 = &.{},

    const Range = struct { start: u32, len: u32 };

    /// Return the child node indices for a given parent.
    pub fn childrenOf(self: *const ScopeIndex, parent_id: NodeId) []const u64 {
        const key = @intFromEnum(parent_id);
        const range = self.map.get(key) orelse return &.{};
        return self.storage[range.start .. range.start + range.len];
    }

    /// Build the scope index from a node array using the MAF pattern.
    /// `offset` is the index of the first node to include (typically 0
    /// for all nodes, or scope_start for file-scoped indices).
    pub fn build(allocator: std.mem.Allocator, nodes: []const Node, offset: usize) !ScopeIndex {
        // Single map reused across three passes: count, prefix-sum, fill.
        var map = std.AutoHashMapUnmanaged(u64, Range){};
        errdefer map.deinit(allocator);

        // Count pass: accumulate child count in range.len; range.start stays 0.
        var total_children: usize = 0;
        for (nodes[offset..]) |n| {
            if (n.parent_id) |pid| {
                const key = @intFromEnum(pid);
                const gop = try map.getOrPut(allocator, key);
                if (!gop.found_existing) gop.value_ptr.* = .{ .start = 0, .len = 0 };
                gop.value_ptr.len += 1;
                total_children += 1;
            }
        }
        if (total_children == 0) return .{};

        // Allocate storage for all children in one shot.
        const storage = try allocator.alloc(u64, total_children);
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

        // Fill pass: write child indices into storage; len tracks write position.
        for (nodes[offset..], offset..) |n, i| {
            if (n.parent_id) |pid| {
                const key = @intFromEnum(pid);
                const range = map.getPtr(key).?;
                storage[range.start + range.len] = i;
                range.len += 1;
            }
        }

        return .{ .map = map, .storage = storage };
    }

    /// Free the map and storage array.
    pub fn deinit(self: *ScopeIndex, allocator: std.mem.Allocator) void {
        self.map.deinit(allocator);
        if (self.storage.len > 0) allocator.free(self.storage);
    }

    /// Search all descendants of `parent_id` for a node named `name`.
    /// Returns the matching NodeId only if exactly one descendant matches;
    /// returns null on zero matches or ambiguity (multiple matches).
    pub fn findUniqueDescendant(self: *const ScopeIndex, nodes: []const Node, parent_id: NodeId, name: []const u8) ?NodeId {
        var result: ?NodeId = null;
        var count: usize = 0;
        self.findDescendantImpl(nodes, parent_id, name, &result, &count);
        return if (count == 1) result else null;
    }

    fn findDescendantImpl(self: *const ScopeIndex, nodes: []const Node, parent_id: NodeId, name: []const u8, result: *?NodeId, count: *usize) void {
        if (count.* > 1) return;
        for (self.childrenOf(parent_id)) |child_idx| {
            if (count.* > 1) return;
            const n = nodes[child_idx];
            if (std.mem.eql(u8, n.name, name)) {
                result.* = @enumFromInt(child_idx);
                count.* += 1;
                if (count.* > 1) return;
            }
            self.findDescendantImpl(nodes, @enumFromInt(child_idx), name, result, count);
        }
    }
};

// -- Tests --

test "childrenOf returns direct children and excludes grandchildren" {
    // Arrange
    const file_id: NodeId = @enumFromInt(0);
    const struct_id: NodeId = @enumFromInt(1);
    const nodes: []const Node = &.{
        .{ .id = @enumFromInt(0), .name = "file", .kind = .file, .language = .zig },
        .{ .id = @enumFromInt(1), .name = "MyStruct", .kind = .type_def, .language = .zig, .parent_id = file_id },
        .{ .id = @enumFromInt(2), .name = "fn_a", .kind = .function, .language = .zig, .parent_id = file_id },
        .{ .id = @enumFromInt(3), .name = "method", .kind = .function, .language = .zig, .parent_id = struct_id },
    };
    var idx = try ScopeIndex.build(std.testing.allocator, nodes, 0);
    defer idx.deinit(std.testing.allocator);

    // Assert: file has 2 direct children (struct + fn_a), not the grandchild method
    const file_children = idx.childrenOf(file_id);
    try std.testing.expectEqual(@as(usize, 2), file_children.len);

    // Assert: struct has 1 child (method)
    const struct_children = idx.childrenOf(struct_id);
    try std.testing.expectEqual(@as(usize, 1), struct_children.len);
    try std.testing.expectEqual(@as(u64, 3), struct_children[0]);

    // Assert: unknown parent returns empty
    try std.testing.expectEqual(@as(usize, 0), idx.childrenOf(@enumFromInt(99)).len);

    // Assert: storage covers all parent-child pairs
    try std.testing.expectEqual(@as(usize, 3), idx.storage.len);
}

test "build on empty or parentless nodes returns empty index" {
    // Arrange: empty array
    var idx1 = try ScopeIndex.build(std.testing.allocator, &.{}, 0);
    defer idx1.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 0), idx1.storage.len);

    // Arrange: all nodes are parentless
    const nodes: []const Node = &.{
        .{ .id = @enumFromInt(0), .name = "a", .kind = .file, .language = .zig },
        .{ .id = @enumFromInt(1), .name = "b", .kind = .file, .language = .zig },
    };
    var idx2 = try ScopeIndex.build(std.testing.allocator, nodes, 0);
    defer idx2.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 0), idx2.storage.len);
}

test "build with offset skips earlier nodes" {
    // Arrange
    const parent: NodeId = @enumFromInt(0);
    const nodes: []const Node = &.{
        .{ .id = @enumFromInt(0), .name = "file", .kind = .file, .language = .zig },
        .{ .id = @enumFromInt(1), .name = "skipped", .kind = .function, .language = .zig, .parent_id = parent },
        .{ .id = @enumFromInt(2), .name = "included", .kind = .function, .language = .zig, .parent_id = parent },
    };

    // Act: offset=2 skips node 1
    var idx = try ScopeIndex.build(std.testing.allocator, nodes, 2);
    defer idx.deinit(std.testing.allocator);

    // Assert
    const children = idx.childrenOf(parent);
    try std.testing.expectEqual(@as(usize, 1), children.len);
    try std.testing.expectEqual(@as(u64, 2), children[0]);
}

test "findUniqueDescendant finds nested match and rejects ambiguity" {
    // Arrange
    const file_id: NodeId = @enumFromInt(0);
    const struct_id: NodeId = @enumFromInt(1);
    const nodes: []const Node = &.{
        .{ .id = @enumFromInt(0), .name = "file", .kind = .file, .language = .zig },
        .{ .id = @enumFromInt(1), .name = "MyStruct", .kind = .type_def, .language = .zig, .parent_id = file_id },
        .{ .id = @enumFromInt(2), .name = "target", .kind = .function, .language = .zig, .parent_id = struct_id },
        .{ .id = @enumFromInt(3), .name = "dup", .kind = .function, .language = .zig, .parent_id = file_id },
        .{ .id = @enumFromInt(4), .name = "dup", .kind = .function, .language = .zig, .parent_id = struct_id },
    };
    var idx = try ScopeIndex.build(std.testing.allocator, nodes, 0);
    defer idx.deinit(std.testing.allocator);

    // Assert: unique nested descendant found
    const found = idx.findUniqueDescendant(nodes, file_id, "target");
    try std.testing.expect(found != null);
    try std.testing.expectEqual(@as(u64, 2), @intFromEnum(found.?));

    // Assert: non-existent name returns null
    try std.testing.expectEqual(@as(?NodeId, null), idx.findUniqueDescendant(nodes, file_id, "nonexistent"));

    // Assert: ambiguous name (2 descendants named "dup") returns null
    try std.testing.expectEqual(@as(?NodeId, null), idx.findUniqueDescendant(nodes, file_id, "dup"));
}
