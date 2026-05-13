const std = @import("std");
const fixtures = @import("test-fixtures");

const empty_mapped: [0]u8 align(std.heap.page_size_min) = .{};

/// Memory-map a file for zero-copy read access.
/// Returns the file content as a byte slice backed by the OS page cache.
/// The caller must call `unmapFile` when done with the slice.
pub fn mmapFile(io: std.Io, path: []const u8) ![]align(std.heap.page_size_min) const u8 {
    const file = try std.Io.Dir.cwd().openFile(io, path, .{});
    defer file.close(io);

    const stat = try file.stat(io);
    const size: usize = @intCast(stat.size);

    if (size == 0) return &empty_mapped;

    const mapped = try std.posix.mmap(
        null,
        size,
        .{ .READ = true },
        .{ .TYPE = .SHARED },
        file.handle,
        0,
    );
    return mapped[0..size];
}

/// Release a memory-mapped file slice returned by `mmapFile`.
pub fn unmapFile(mapped: []align(std.heap.page_size_min) const u8) void {
    if (mapped.len == 0) return;
    std.posix.munmap(mapped);
}

test "mmap reads file content" {
    // Arrange
    const expected = fixtures.zig.simple;

    // Act
    const content = try mmapFile(std.testing.io, "test/fixtures/zig/simple.zig");
    defer unmapFile(content);

    // Assert
    try std.testing.expectEqualStrings(expected, content);
}

test "mmap on empty file" {
    // Act
    const content = try mmapFile(std.testing.io, "test/fixtures/zig/edge_cases/empty.zig");
    defer unmapFile(content);

    // Assert: empty slice, no crash
    try std.testing.expectEqual(@as(usize, 0), content.len);
}
