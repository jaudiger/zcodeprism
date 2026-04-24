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

    fn monotonicNs(io: std.Io) i64 {
        return @intCast(std.Io.Timestamp.now(io, .awake).nanoseconds);
    }

    /// Record that an event just happened.
    pub fn trigger(self: *Debouncer, io: std.Io) void {
        self.last_event_ns.store(monotonicNs(io), .monotonic);
    }

    /// Returns true if enough silence has passed since the last trigger.
    pub fn isReady(self: *const Debouncer, io: std.Io) bool {
        const last = self.last_event_ns.load(.monotonic);
        if (last == 0) return false;
        const elapsed = monotonicNs(io) - last;
        return elapsed >= self.delay_ns;
    }

    /// Milliseconds remaining until ready, or 0 if already ready.
    pub fn remainingMs(self: *const Debouncer, io: std.Io) u32 {
        const last = self.last_event_ns.load(.monotonic);
        if (last == 0) return 0;
        const elapsed = monotonicNs(io) - last;
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
    try std.testing.expect(!d.isReady(std.testing.io));
}

test "isReady returns false immediately after trigger" {
    // Arrange
    var d = Debouncer.init(500);

    // Act
    d.trigger(std.testing.io);

    // Assert
    try std.testing.expect(!d.isReady(std.testing.io));
}

test "remainingMs returns 0 before any trigger" {
    // Arrange
    const d = Debouncer.init(500);

    // Act / Assert
    try std.testing.expectEqual(@as(u32, 0), d.remainingMs(std.testing.io));
}

test "remainingMs returns positive value immediately after trigger" {
    // Arrange
    var d = Debouncer.init(500);

    // Act
    d.trigger(std.testing.io);

    // Assert
    try std.testing.expect(d.remainingMs(std.testing.io) > 0);
}

test "isReady returns true after delay with zero delay" {
    // Arrange
    var d = Debouncer.init(0);

    // Act
    d.trigger(std.testing.io);

    // Assert
    try std.testing.expect(d.isReady(std.testing.io));
}

test "remainingMs returns 0 with zero delay after trigger" {
    // Arrange
    var d = Debouncer.init(0);

    // Act
    d.trigger(std.testing.io);

    // Assert
    try std.testing.expectEqual(@as(u32, 0), d.remainingMs(std.testing.io));
}
