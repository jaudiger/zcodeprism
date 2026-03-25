const std = @import("std");

/// Debounces file system events by requiring a quiet period before
/// signaling readiness. Thread-safe through atomic operations.
pub const Debouncer = struct {
    delay_ns: i64,
    last_event_ns: std.atomic.Value(i64),

    pub fn init(delay_ms: u32) Debouncer {
        return .{
            .delay_ns = @as(i64, delay_ms) * std.time.ns_per_ms,
            .last_event_ns = std.atomic.Value(i64).init(0),
        };
    }

    fn nanoTimestamp() i64 {
        return @intCast(std.time.nanoTimestamp());
    }

    /// Record that an event just happened.
    pub fn trigger(self: *Debouncer) void {
        self.last_event_ns.store(nanoTimestamp(), .monotonic);
    }

    /// Returns true if enough silence has passed since the last trigger.
    pub fn isReady(self: *const Debouncer) bool {
        const last = self.last_event_ns.load(.monotonic);
        if (last == 0) return false;
        const elapsed = nanoTimestamp() - last;
        return elapsed >= self.delay_ns;
    }

    /// Milliseconds remaining until ready, or 0 if already ready.
    pub fn remainingMs(self: *const Debouncer) u32 {
        const last = self.last_event_ns.load(.monotonic);
        if (last == 0) return 0;
        const elapsed = nanoTimestamp() - last;
        if (elapsed >= self.delay_ns) return 0;
        const remaining_ns = self.delay_ns - elapsed;
        const remaining_ms = @divFloor(remaining_ns, std.time.ns_per_ms);
        return @intCast(@min(remaining_ms + 1, std.math.maxInt(u32)));
    }
};

test "init sets delay and zero last_event" {
    // Arrange / Act
    const d = Debouncer.init(500);

    // Assert
    try std.testing.expectEqual(@as(i64, 500 * std.time.ns_per_ms), d.delay_ns);
    try std.testing.expectEqual(@as(i64, 0), d.last_event_ns.load(.monotonic));
}

test "isReady returns false before any trigger" {
    // Arrange
    const d = Debouncer.init(500);

    // Act / Assert
    try std.testing.expect(!d.isReady());
}

test "isReady returns false immediately after trigger" {
    // Arrange
    var d = Debouncer.init(500);

    // Act
    d.trigger();

    // Assert
    try std.testing.expect(!d.isReady());
}

test "remainingMs returns 0 before any trigger" {
    // Arrange
    const d = Debouncer.init(500);

    // Act / Assert
    try std.testing.expectEqual(@as(u32, 0), d.remainingMs());
}

test "remainingMs returns positive value immediately after trigger" {
    // Arrange
    var d = Debouncer.init(500);

    // Act
    d.trigger();

    // Assert
    try std.testing.expect(d.remainingMs() > 0);
}

test "isReady returns true after delay with zero delay" {
    // Arrange
    var d = Debouncer.init(0);

    // Act
    d.trigger();

    // Assert
    try std.testing.expect(d.isReady());
}

test "remainingMs returns 0 with zero delay after trigger" {
    // Arrange
    var d = Debouncer.init(0);

    // Act
    d.trigger();

    // Assert
    try std.testing.expectEqual(@as(u32, 0), d.remainingMs());
}
