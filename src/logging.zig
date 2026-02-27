const std = @import("std");

/// Log severity levels, ordered from most verbose (trace=0) to least (err=4).
pub const Level = enum(u8) {
    trace = 0,
    debug = 1,
    info = 2,
    warn = 3,
    err = 4,

    /// Return the human-readable label for this level (e.g. "TRACE", "ERROR").
    pub fn asText(self: Level) []const u8 {
        return switch (self) {
            .trace => "TRACE",
            .debug => "DEBUG",
            .info => "INFO",
            .warn => "WARN",
            .err => "ERROR",
        };
    }
};

/// Tagged union carrying one structured log field value.
pub const FieldValue = union(enum) {
    string: []const u8,
    int: i64,
    uint: u64,
    boolean: bool,
};

/// A key-value pair attached to a structured log message.
pub const Field = struct {
    key: []const u8,
    value: FieldValue,

    /// Create a string-valued field.
    pub fn string(key: []const u8, val: []const u8) Field {
        return .{ .key = key, .value = .{ .string = val } };
    }

    /// Create a signed-integer-valued field.
    pub fn int(key: []const u8, val: i64) Field {
        return .{ .key = key, .value = .{ .int = val } };
    }

    /// Create an unsigned-integer-valued field.
    pub fn uint(key: []const u8, val: u64) Field {
        return .{ .key = key, .value = .{ .uint = val } };
    }

    /// Create a boolean-valued field.
    pub fn boolean(key: []const u8, val: bool) Field {
        return .{ .key = key, .value = .{ .boolean = val } };
    }
};

/// Vtable-based logger interface.
///
/// Zero-cost when using the `noop` instance (all methods short-circuit).
/// Concrete implementations (e.g. `TextStderrLogger`) provide a VTable and
/// are accessed through the `ptr` field. Pass loggers by value; they are
/// small (pointer-sized) and cheap to copy.
pub const Logger = struct {
    ptr: ?*anyopaque,
    vtable: *const VTable,
    scope: []const u8,
    min_level: u8,

    /// Dispatch table for a concrete logger backend.
    pub const VTable = struct {
        log: *const fn (ptr: ?*anyopaque, level: Level, scope: []const u8, msg: []const u8, fields: []const Field) void,
    };

    const noop_vtable = VTable{
        .log = &noopLog,
    };

    fn noopLog(_: ?*anyopaque, _: Level, _: []const u8, _: []const u8, _: []const Field) void {}

    /// A logger that discards all messages.
    /// All convenience methods short-circuit because `min_level` is maxInt(u8).
    pub const noop = Logger{
        .ptr = null,
        .vtable = &noop_vtable,
        .scope = "",
        .min_level = std.math.maxInt(u8),
    };

    /// Return a copy of this logger with a different scope tag.
    /// The vtable, ptr, and min_level are preserved.
    pub fn withScope(self: Logger, scope: []const u8) Logger {
        return .{
            .ptr = self.ptr,
            .vtable = self.vtable,
            .scope = scope,
            .min_level = self.min_level,
        };
    }

    fn dispatch(self: Logger, level: Level, msg: []const u8, fields: []const Field) void {
        if (@intFromEnum(level) < self.min_level) return;
        self.vtable.log(self.ptr, level, self.scope, msg, fields);
    }

    /// Emit a trace-level log message (most verbose).
    pub fn trace(self: Logger, msg: []const u8, fields: []const Field) void {
        self.dispatch(.trace, msg, fields);
    }

    /// Emit a debug-level log message.
    pub fn debug(self: Logger, msg: []const u8, fields: []const Field) void {
        self.dispatch(.debug, msg, fields);
    }

    /// Emit an info-level log message.
    pub fn info(self: Logger, msg: []const u8, fields: []const Field) void {
        self.dispatch(.info, msg, fields);
    }

    /// Emit a warn-level log message.
    pub fn warn(self: Logger, msg: []const u8, fields: []const Field) void {
        self.dispatch(.warn, msg, fields);
    }

    /// Emit an error-level log message (least verbose).
    pub fn err(self: Logger, msg: []const u8, fields: []const Field) void {
        self.dispatch(.err, msg, fields);
    }
};

/// Concrete logger backend that writes structured text lines to stderr.
///
/// Output format: `YYYY-MM-DDTHH:MM:SSZ LEVEL [scope] message key=val ...`
/// Uses a fixed 4096-byte stack buffer; messages exceeding that are truncated.
pub const TextStderrLogger = struct {
    min_level: Level,

    const vtable = Logger.VTable{
        .log = &logImpl,
    };

    /// Create a TextStderrLogger that emits messages at `min_level` and above.
    pub fn init(min_level: Level) TextStderrLogger {
        return .{ .min_level = min_level };
    }

    /// Return a Logger interface backed by this TextStderrLogger.
    /// The returned Logger borrows `self`, so `self` must outlive it.
    pub fn logger(self: *TextStderrLogger) Logger {
        return .{
            .ptr = @ptrCast(self),
            .vtable = &vtable,
            .scope = "",
            .min_level = @intFromEnum(self.min_level),
        };
    }

    fn logImpl(_: ?*anyopaque, level: Level, scope: []const u8, msg: []const u8, fields: []const Field) void {
        var stderr_buf: [4096]u8 = undefined;
        var stderr_writer = std.fs.File.stderr().writer(&stderr_buf);
        const w = &stderr_writer.interface;

        // Timestamp.
        const epoch = std.time.timestamp();
        const es = std.time.epoch.EpochSeconds{ .secs = @intCast(@max(0, epoch)) };
        const day = es.getEpochDay();
        const yd = day.calculateYearDay();
        const md = yd.calculateMonthDay();
        const ds = es.getDaySeconds();

        w.print("{d:0>4}-{d:0>2}-{d:0>2}T{d:0>2}:{d:0>2}:{d:0>2}Z {s}", .{
            yd.year,
            @as(u32, @intFromEnum(md.month)),
            @as(u32, md.day_index) + 1,
            ds.getHoursIntoDay(),
            ds.getMinutesIntoHour(),
            ds.getSecondsIntoMinute(),
            level.asText(),
        }) catch return;

        if (scope.len > 0) {
            w.print(" [{s}]", .{scope}) catch return;
        }

        w.print(" {s}", .{msg}) catch return;

        for (fields) |f| {
            switch (f.value) {
                .string => |v| w.print(" {s}={s}", .{ f.key, v }) catch return,
                .int => |v| w.print(" {s}={d}", .{ f.key, v }) catch return,
                .uint => |v| w.print(" {s}={d}", .{ f.key, v }) catch return,
                .boolean => |v| w.print(" {s}={}", .{ f.key, v }) catch return,
            }
        }

        w.print("\n", .{}) catch return;
        w.flush() catch return;
    }
};

test "Level has correct integer values" {
    comptime {
        std.debug.assert(@intFromEnum(Level.trace) == 0);
        std.debug.assert(@intFromEnum(Level.debug) == 1);
        std.debug.assert(@intFromEnum(Level.info) == 2);
        std.debug.assert(@intFromEnum(Level.warn) == 3);
        std.debug.assert(@intFromEnum(Level.err) == 4);
    }
}

test "Level.asText returns correct strings" {
    try std.testing.expectEqualStrings("TRACE", Level.trace.asText());
    try std.testing.expectEqualStrings("DEBUG", Level.debug.asText());
    try std.testing.expectEqualStrings("INFO", Level.info.asText());
    try std.testing.expectEqualStrings("WARN", Level.warn.asText());
    try std.testing.expectEqualStrings("ERROR", Level.err.asText());
}

test "Field constructors create correct fields" {
    // Arrange / Act
    const fs = Field.string("name", "hello");
    const fi = Field.int("count", -42);
    const fu = Field.uint("size", 100);
    const fb = Field.boolean("enabled", true);

    // Assert: string
    try std.testing.expectEqualStrings("name", fs.key);
    switch (fs.value) {
        .string => |v| try std.testing.expectEqualStrings("hello", v),
        else => return error.UnexpectedVariant,
    }

    // Assert: int
    try std.testing.expectEqualStrings("count", fi.key);
    switch (fi.value) {
        .int => |v| try std.testing.expectEqual(@as(i64, -42), v),
        else => return error.UnexpectedVariant,
    }

    // Assert: uint
    try std.testing.expectEqualStrings("size", fu.key);
    switch (fu.value) {
        .uint => |v| try std.testing.expectEqual(@as(u64, 100), v),
        else => return error.UnexpectedVariant,
    }

    // Assert: boolean
    try std.testing.expectEqualStrings("enabled", fb.key);
    switch (fb.value) {
        .boolean => |v| try std.testing.expectEqual(true, v),
        else => return error.UnexpectedVariant,
    }
}

test "Logger.noop does not crash on any method" {
    // Act: call every level, none should crash
    const log = Logger.noop;
    log.trace("msg", &.{});
    log.debug("msg", &.{});
    log.info("msg", &.{});
    log.warn("msg", &.{});
    log.err("msg", &.{});
}

test "Logger.noop has maxInt min_level" {
    comptime {
        std.debug.assert(Logger.noop.min_level == std.math.maxInt(u8));
    }
}

test "Logger.withScope returns different scope" {
    // Arrange
    const base = Logger.noop;

    // Act
    const scoped = base.withScope("test-scope");

    // Assert
    try std.testing.expectEqualStrings("test-scope", scoped.scope);
    try std.testing.expectEqual(base.min_level, scoped.min_level);
}

test "Logger filters by min_level" {
    // Arrange: a spy logger that counts calls via the ptr field
    var call_count: usize = 0;

    const spy_vtable = Logger.VTable{
        .log = &struct {
            fn logFn(ptr: ?*anyopaque, _: Level, _: []const u8, _: []const u8, _: []const Field) void {
                const count: *usize = @ptrCast(@alignCast(ptr.?));
                count.* += 1;
            }
        }.logFn,
    };
    const log = Logger{
        .ptr = @ptrCast(&call_count),
        .vtable = &spy_vtable,
        .scope = "",
        .min_level = @intFromEnum(Level.warn),
    };

    // Act
    log.trace("filtered", &.{});
    log.debug("filtered", &.{});
    log.info("filtered", &.{});
    log.warn("passed", &.{});
    log.err("passed", &.{});

    // Assert: only warn and err get through
    try std.testing.expectEqual(@as(usize, 2), call_count);
}

test "TextStderrLogger.init sets min_level" {
    // Arrange / Act
    var text_logger = TextStderrLogger.init(.info);
    const log = text_logger.logger();

    // Assert
    try std.testing.expectEqual(@as(u8, @intFromEnum(Level.info)), log.min_level);
}

test "TextStderrLogger.logger returns correct vtable and scope" {
    // Arrange
    var text_logger = TextStderrLogger.init(.debug);

    // Act
    const log = text_logger.logger().withScope("test");

    // Assert
    try std.testing.expectEqual(@as(u8, @intFromEnum(Level.debug)), log.min_level);
    try std.testing.expectEqualStrings("test", log.scope);
    try std.testing.expect(log.vtable == &TextStderrLogger.vtable);
}

test "Logger struct size is reasonable" {
    comptime {
        // ptr + vtable ptr + slice (ptr+len) + u8 = reasonable size
        std.debug.assert(@sizeOf(Logger) <= 48);
    }
}
