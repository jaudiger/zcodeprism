//! Atomic reference counter with ARC memory ordering.
const std = @import("std");

pub const RefCount = struct {
    value: std.atomic.Value(u32),

    pub fn init(initial: u32) RefCount {
        return .{ .value = std.atomic.Value(u32).init(initial) };
    }

    /// Increments the count.
    pub fn acquire(self: *RefCount) void {
        const prev = self.value.fetchAdd(1, .acquire);
        std.debug.assert(prev != std.math.maxInt(u32));
    }

    /// Decrements the count.
    pub fn release(self: *RefCount) void {
        const prev = self.value.fetchSub(1, .release);
        std.debug.assert(prev > 0);
    }

    /// Reads the count for the destroy decision.
    pub fn loadForReclaim(self: *const RefCount) u32 {
        return self.value.load(.acquire);
    }

    /// Reads the count without ordering guarantees. For debug and tests only.
    pub fn count(self: *const RefCount) u32 {
        return self.value.load(.monotonic);
    }
};

test "RefCount is the same size as its inner atomic" {
    comptime {
        std.debug.assert(@sizeOf(RefCount) == @sizeOf(std.atomic.Value(u32)));
    }
}

test "init stores initial count" {
    // Arrange / Act
    const rc = RefCount.init(3);

    // Assert
    try std.testing.expectEqual(@as(u32, 3), rc.count());
}

test "acquire increments by one" {
    // Arrange
    var rc = RefCount.init(0);

    // Act
    rc.acquire();

    // Assert
    try std.testing.expectEqual(@as(u32, 1), rc.count());
}

test "release decrements by one" {
    // Arrange
    var rc = RefCount.init(2);
    rc.acquire();

    // Act
    rc.release();

    // Assert
    try std.testing.expectEqual(@as(u32, 2), rc.count());
}

test "count reflects net acquire and release" {
    // Arrange
    var rc = RefCount.init(0);

    // Act
    rc.acquire();
    rc.acquire();
    rc.acquire();
    rc.release();

    // Assert
    try std.testing.expectEqual(@as(u32, 2), rc.count());
}

test "loadForReclaim returns zero after balanced acquire and release" {
    // Arrange
    var rc = RefCount.init(0);
    rc.acquire();
    rc.acquire();

    // Act
    rc.release();
    rc.release();

    // Assert
    try std.testing.expectEqual(@as(u32, 0), rc.loadForReclaim());
}

test "loadForReclaim returns nonzero with outstanding acquire" {
    // Arrange
    var rc = RefCount.init(0);
    rc.acquire();

    // Assert
    try std.testing.expect(rc.loadForReclaim() > 0);

    // Cleanup
    rc.release();
}
