const std = @import("std");

pub const Page = struct {
    start: u32,
    len: u32,
};

/// Clamp offset and limit against total, returning the page window.
pub fn paginate(total: u32, offset: u32, limit: u32) Page {
    const clamped = @min(offset, total);
    const end = @min(clamped +| limit, total);
    return .{ .start = clamped, .len = end - clamped };
}

test "paginate clamps and saturates correctly" {
    // Arrange
    const cases = [_]struct { u32, u32, u32, u32, u32 }{
        .{ 100, 10, 20, 10, 20 },
        .{ 100, 10, 0, 10, 0 },
        .{ 5, 10, 20, 5, 0 },
        .{ 100, 1, std.math.maxInt(u32), 1, 99 },
        .{ 0, 0, 50, 0, 0 },
        .{ 50, 0, 50, 0, 50 },
    };

    for (cases) |c| {
        // Act
        const p = paginate(c[0], c[1], c[2]);

        // Assert
        try std.testing.expectEqual(c[3], p.start);
        try std.testing.expectEqual(c[4], p.len);
    }
}
