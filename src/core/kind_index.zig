const std = @import("std");
const node_mod = @import("node.zig");
const types = @import("types.zig");

const Node = node_mod.Node;
const NodeKind = types.NodeKind;

const kind_count = @typeInfo(NodeKind).@"enum".fields.len;

/// Pre-built index mapping NodeKind to their graph indices.
/// Uses a fixed-size array (one slot per kind) instead of a hash map
/// since NodeKind is a small enum with known cardinality.
pub const KindIndex = struct {
    ranges: [kind_count]Range = [_]Range{.{}} ** kind_count,
    storage: []usize = &.{},

    const Range = struct { start: u32 = 0, len: u32 = 0 };

    /// Return all node indices with the given kind.
    pub fn findByKind(self: *const KindIndex, kind: NodeKind) []const usize {
        const range = self.ranges[@intFromEnum(kind)];
        if (range.len == 0) return &.{};
        return self.storage[range.start .. range.start + range.len];
    }

    /// Build the kind index from a node array using the MAF pattern.
    pub fn build(allocator: std.mem.Allocator, nodes: []const Node) !KindIndex {
        // Measure: count nodes per kind.
        var counts: [kind_count]u32 = [_]u32{0} ** kind_count;
        for (nodes) |n| {
            counts[@intFromEnum(n.kind)] += 1;
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
            const k = @intFromEnum(n.kind);
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
        .{ .id = @enumFromInt(0), .name = "a", .kind = .function, .language = .zig },
        .{ .id = @enumFromInt(1), .name = "b", .kind = .type_def, .language = .zig },
        .{ .id = @enumFromInt(2), .name = "c", .kind = .function, .language = .zig },
    };

    // Act
    var idx = try KindIndex.build(std.testing.allocator, nodes);
    defer idx.deinit(std.testing.allocator);

    // Assert: matching kinds return correct indices in order
    const fns = idx.findByKind(.function);
    try std.testing.expectEqual(@as(usize, 2), fns.len);
    try std.testing.expectEqual(@as(usize, 0), fns[0]);
    try std.testing.expectEqual(@as(usize, 2), fns[1]);

    const structs = idx.findByKind(.type_def);
    try std.testing.expectEqual(@as(usize, 1), structs.len);
    try std.testing.expectEqual(@as(usize, 1), structs[0]);

    // Assert: absent kinds return empty
    try std.testing.expectEqual(@as(usize, 0), idx.findByKind(.file).len);
    try std.testing.expectEqual(@as(usize, 0), idx.findByKind(.enum_def).len);

    // Assert: storage covers all nodes
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
    // Arrange: one node per kind
    const nodes: []const Node = &.{
        .{ .id = @enumFromInt(0), .name = "f", .kind = .file, .language = .zig },
        .{ .id = @enumFromInt(1), .name = "m", .kind = .module, .language = .zig },
        .{ .id = @enumFromInt(2), .name = "fn", .kind = .function, .language = .zig },
        .{ .id = @enumFromInt(3), .name = "st", .kind = .type_def, .language = .zig },
        .{ .id = @enumFromInt(4), .name = "en", .kind = .enum_def, .language = .zig },
        .{ .id = @enumFromInt(5), .name = "fi", .kind = .field, .language = .zig },
        .{ .id = @enumFromInt(6), .name = "c", .kind = .constant, .language = .zig },
        .{ .id = @enumFromInt(7), .name = "t", .kind = .test_def, .language = .zig },
        .{ .id = @enumFromInt(8), .name = "e", .kind = .error_def, .language = .zig },
        .{ .id = @enumFromInt(9), .name = "i", .kind = .import_decl, .language = .zig },
        .{ .id = @enumFromInt(10), .name = "u", .kind = .union_def, .language = .zig },
        .{ .id = @enumFromInt(11), .name = "d", .kind = .directory, .language = .zig },
    };

    // Act
    var idx = try KindIndex.build(std.testing.allocator, nodes);
    defer idx.deinit(std.testing.allocator);

    // Assert
    inline for (@typeInfo(NodeKind).@"enum".fields) |field| {
        const kind: NodeKind = @enumFromInt(field.value);
        try std.testing.expectEqual(@as(usize, 1), idx.findByKind(kind).len);
    }
}
