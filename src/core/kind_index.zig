const std = @import("std");
const node_mod = @import("node.zig");
const types = @import("types.zig");

const Node = node_mod.Node;
const NodeKind = types.NodeKind;

const kind_count = @typeInfo(NodeKind).@"enum".field_names.len;

/// Pre-built index mapping NodeKind to their graph indices.
/// Uses a fixed-size array (one slot per kind) instead of a hash map
/// since NodeKind is a small enum with known cardinality.
pub const KindIndex = struct {
    ranges: [kind_count]Range = @splat(.{}),
    storage: []usize = &.{},

    const Range = struct { start: u32 = 0, len: u32 = 0 };

    /// Return all node indices with the given kind.
    pub fn findByKind(self: *const KindIndex, kind: NodeKind) []const usize {
        const range = self.ranges[@backingInt(kind)];
        if (range.len == 0) return &.{};
        return self.storage[range.start .. range.start + range.len];
    }

    /// Build the kind index from a node array using the MAF pattern.
    pub fn build(allocator: std.mem.Allocator, nodes: []const Node) !KindIndex {
        // Measure: count nodes per kind.
        var counts: [kind_count]u32 = @splat(0);
        for (nodes) |n| {
            counts[@backingInt(n.kind)] += 1;
        }

        var total: usize = 0;
        for (counts) |c| total += c;
        if (total == 0) return .{};

        // Allocate: single flat array for all entries.
        const storage = try allocator.alloc(usize, total);
        errdefer allocator.free(storage);

        // Compute offsets via prefix sum.
        var offsets: [kind_count]u32 = undefined;
        {
            var running: u32 = 0;
            for (0..kind_count) |i| {
                offsets[i] = running;
                running += counts[i];
            }
            std.debug.assert(running == total);
        }

        // Fill: place node indices into their kind slots.
        var write_pos = offsets;
        for (nodes, 0..) |n, i| {
            const k = @backingInt(n.kind);
            storage[write_pos[k]] = i;
            write_pos[k] += 1;
        }

        // Assert fill completeness.
        for (0..kind_count) |i| {
            std.debug.assert(write_pos[i] == offsets[i] + counts[i]);
        }

        // Build ranges.
        var ranges: [kind_count]Range = undefined;
        for (0..kind_count) |i| {
            ranges[i] = .{ .start = offsets[i], .len = counts[i] };
        }

        return .{ .ranges = ranges, .storage = storage };
    }

    /// Free the flat storage array.
    pub fn deinit(self: *KindIndex, allocator: std.mem.Allocator) void {
        if (self.storage.len > 0) allocator.free(self.storage);
    }
};

test "findByKind returns correct indices and empty for absent kinds" {
    // Arrange
    const nodes: []const Node = &.{
        .{ .id = @fromBackingInt(@intCast(0)), .name = "a", .kind = .function, .language = .zig },
        .{ .id = @fromBackingInt(@intCast(1)), .name = "b", .kind = .type_def, .language = .zig },
        .{ .id = @fromBackingInt(@intCast(2)), .name = "c", .kind = .function, .language = .zig },
    };

    // Act
    var idx = try KindIndex.build(std.testing.allocator, nodes);
    defer idx.deinit(std.testing.allocator);

    // Assert
    const fns = idx.findByKind(.function);
    try std.testing.expectEqual(@as(usize, 2), fns.len);
    try std.testing.expectEqual(@as(usize, 0), fns[0]);
    try std.testing.expectEqual(@as(usize, 2), fns[1]);

    const structs = idx.findByKind(.type_def);
    try std.testing.expectEqual(@as(usize, 1), structs.len);
    try std.testing.expectEqual(@as(usize, 1), structs[0]);

    try std.testing.expectEqual(@as(usize, 0), idx.findByKind(.file).len);
    try std.testing.expectEqual(@as(usize, 0), idx.findByKind(.enum_def).len);

    try std.testing.expectEqual(@as(usize, 3), idx.storage.len);
}

test "build on empty nodes returns empty index" {
    // Arrange
    const nodes: []const Node = &.{};

    // Act
    var idx = try KindIndex.build(std.testing.allocator, nodes);
    defer idx.deinit(std.testing.allocator);

    // Assert
    try std.testing.expectEqual(@as(usize, 0), idx.findByKind(.function).len);
    try std.testing.expectEqual(@as(usize, 0), idx.storage.len);
}

test "every NodeKind variant is indexed" {
    // Arrange
    const nodes: []const Node = &.{
        .{ .id = @fromBackingInt(@intCast(0)), .name = "f", .kind = .file, .language = .zig },
        .{ .id = @fromBackingInt(@intCast(1)), .name = "m", .kind = .module, .language = .zig },
        .{ .id = @fromBackingInt(@intCast(2)), .name = "fn", .kind = .function, .language = .zig },
        .{ .id = @fromBackingInt(@intCast(3)), .name = "st", .kind = .type_def, .language = .zig },
        .{ .id = @fromBackingInt(@intCast(4)), .name = "en", .kind = .enum_def, .language = .zig },
        .{ .id = @fromBackingInt(@intCast(5)), .name = "fi", .kind = .field, .language = .zig },
        .{ .id = @fromBackingInt(@intCast(6)), .name = "c", .kind = .constant, .language = .zig },
        .{ .id = @fromBackingInt(@intCast(7)), .name = "t", .kind = .test_def, .language = .zig },
        .{ .id = @fromBackingInt(@intCast(8)), .name = "e", .kind = .error_def, .language = .zig },
        .{ .id = @fromBackingInt(@intCast(9)), .name = "i", .kind = .import_decl, .language = .zig },
        .{ .id = @fromBackingInt(@intCast(10)), .name = "u", .kind = .union_def, .language = .zig },
        .{ .id = @fromBackingInt(@intCast(11)), .name = "d", .kind = .directory, .language = .zig },
        .{ .id = @fromBackingInt(@intCast(12)), .name = "p", .kind = .parameter, .language = .zig },
    };

    // Act
    var idx = try KindIndex.build(std.testing.allocator, nodes);
    defer idx.deinit(std.testing.allocator);

    // Assert
    inline for (@typeInfo(NodeKind).@"enum".field_values) |field_value| {
        const kind: NodeKind = @fromBackingInt(@intCast(field_value));
        try std.testing.expectEqual(@as(usize, 1), idx.findByKind(kind).len);
    }
}
