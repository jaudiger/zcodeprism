const std = @import("std");

/// Memory-map a file for zero-copy read access.
/// Returns the file content as a byte slice backed by the OS page cache.
/// The caller must call `unmapFile` when done with the slice.
pub fn mmapFile(path: []const u8) anyerror![]const u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const stat = try file.stat();
    const size: usize = @intCast(stat.size);

    if (size == 0) return &.{};

    const mapped = try std.posix.mmap(
        null,
        size,
        std.posix.PROT.READ,
        .{ .TYPE = .SHARED },
        file.handle,
        0,
    );
    return mapped[0..size];
}

/// Release a memory-mapped file slice returned by `mmapFile`.
pub fn unmapFile(mapped: []const u8) void {
    if (mapped.len == 0) return;
    const aligned_ptr: [*]align(std.heap.page_size_min) const u8 = @alignCast(mapped.ptr);
    std.posix.munmap(aligned_ptr[0..mapped.len]);
}

// --- Tests ---

const fixtures = @import("test-fixtures");

test "mmap reads file content" {
    // Arrange
    const expected = fixtures.zig.simple;

    // Act
    const content = try mmapFile("tests/fixtures/zig/simple.zig");
    defer unmapFile(content);

    // Assert
    try std.testing.expectEqualStrings(expected, content);
}

test "mmap on empty file" {
    // Act
    const content = try mmapFile("tests/fixtures/zig/edge_cases/empty.zig");
    defer unmapFile(content);

    // Assert: empty slice, no crash
    try std.testing.expectEqual(@as(usize, 0), content.len);
}
