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

    /// Decrements the count. Returns true when the count transitioned to
    /// zero, with an acquire load issued before returning.
    pub fn release(self: *RefCount) bool {
        const prev = self.value.fetchSub(1, .release);
        std.debug.assert(prev > 0);
        if (prev == 1) {
            _ = self.value.load(.acquire);
            return true;
        }
        return false;
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
    _ = rc.release();

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
    _ = rc.release();

    // Assert
    try std.testing.expectEqual(@as(u32, 2), rc.count());
}

test "loadForReclaim returns zero after balanced acquire and release" {
    // Arrange
    var rc = RefCount.init(0);
    rc.acquire();
    rc.acquire();

    // Act
    _ = rc.release();
    _ = rc.release();

    // Assert
    try std.testing.expectEqual(@as(u32, 0), rc.loadForReclaim());
}

test "loadForReclaim returns nonzero with outstanding acquire" {
    // Arrange
    var rc = RefCount.init(0);
    rc.acquire();
    defer _ = rc.release();

    // Act / Assert
    try std.testing.expect(rc.loadForReclaim() > 0);
}

test "release returns true only on the last reference" {
    // Arrange
    var rc = RefCount.init(0);
    rc.acquire();
    rc.acquire();

    // Act / Assert
    try std.testing.expectEqual(false, rc.release());
    try std.testing.expectEqual(true, rc.release());
}
