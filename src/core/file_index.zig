const std = @import("std");
const node_mod = @import("node.zig");
const types = @import("types.zig");

const Node = node_mod.Node;
const NodeId = types.NodeId;

/// Maps file node paths to their NodeId for O(1) cross-file lookup.
/// Keys are file_path when available, falling back to name for nodes
/// without a file_path.
pub const FileIndex = struct {
    map: std.StringHashMapUnmanaged(NodeId) = .{},

    /// Build the file index from a node array.
    pub fn build(allocator: std.mem.Allocator, nodes: []const Node) !FileIndex {
        var fi = FileIndex{};
        for (nodes, 0..) |n, i| {
            if (n.kind == .file) {
                const key = n.file_path orelse n.name;
                try fi.map.put(allocator, key, @enumFromInt(i));
            }
        }
        return fi;
    }

    /// Direct lookup by name or path.
    pub fn findByName(self: *const FileIndex, name: []const u8) ?NodeId {
        return self.map.get(name);
    }

    /// Free the internal hash map storage.
    pub fn deinit(self: *FileIndex, allocator: std.mem.Allocator) void {
        self.map.deinit(allocator);
    }
};

// -- Tests --

test "findByName returns file NodeIds and null for absent paths" {
    // Arrange: mix of file and non-file nodes
    const nodes: []const Node = &.{
        .{ .id = @enumFromInt(0), .name = "src/main.zig", .kind = .file, .language = .zig, .file_path = "src/main.zig" },
        .{ .id = @enumFromInt(1), .name = "fn_a", .kind = .function, .language = .zig, .file_path = "src/main.zig" },
        .{ .id = @enumFromInt(2), .name = "src/lib.zig", .kind = .file, .language = .zig, .file_path = "src/lib.zig" },
    };
    var idx = try FileIndex.build(std.testing.allocator, nodes);
    defer idx.deinit(std.testing.allocator);

    // Assert: correct NodeId for known file
    try std.testing.expectEqual(@as(u64, 0), @intFromEnum(idx.findByName("src/main.zig").?));
    try std.testing.expectEqual(@as(u64, 2), @intFromEnum(idx.findByName("src/lib.zig").?));

    // Assert: absent path returns null
    try std.testing.expectEqual(@as(?NodeId, null), idx.findByName("nonexistent.zig"));

    // Assert: non-file node's file_path is not indexed
    try std.testing.expectEqual(@as(?NodeId, null), idx.findByName("fn_a"));
}

test "build uses name as fallback when file_path is null" {
    // Arrange
    const nodes: []const Node = &.{
        .{ .id = @enumFromInt(0), .name = "virtual.zig", .kind = .file, .language = .zig },
    };
    var idx = try FileIndex.build(std.testing.allocator, nodes);
    defer idx.deinit(std.testing.allocator);

    // Assert
    try std.testing.expect(idx.findByName("virtual.zig") != null);
}

test "build on empty nodes and non-file-only nodes" {
    // Arrange: empty
    var idx1 = try FileIndex.build(std.testing.allocator, &.{});
    defer idx1.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(?NodeId, null), idx1.findByName("anything"));

    // Arrange: only non-file nodes
    const nodes: []const Node = &.{
        .{ .id = @enumFromInt(0), .name = "main", .kind = .function, .language = .zig, .file_path = "src/main.zig" },
        .{ .id = @enumFromInt(1), .name = "MyStruct", .kind = .type_def, .language = .zig },
    };
    var idx2 = try FileIndex.build(std.testing.allocator, nodes);
    defer idx2.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(?NodeId, null), idx2.findByName("src/main.zig"));
    try std.testing.expectEqual(@as(?NodeId, null), idx2.findByName("main"));
}
