//! Two generic type-returning functions to test scope-aware name resolution
//! in generic contexts. Each produces a struct with Self, init, deinit,
//! and a method that calls deinit via self.

/// Generic stack type.
pub fn Stack(comptime T: type) type {
    return struct {
        items: []T,

        const Self = @This();

        pub fn init() Self {
            return .{ .items = &.{} };
        }

        pub fn deinit(self: *Self) void {
            _ = self;
        }

        /// Calls self.deinit() internally.
        pub fn clear(self: *Self) void {
            self.deinit();
        }
    };
}

/// Generic queue type.
pub fn Queue(comptime T: type) type {
    return struct {
        data: []T,

        const Self = @This();

        pub fn init() Self {
            return .{ .data = &.{} };
        }

        pub fn deinit(self: *Self) void {
            _ = self;
        }

        /// Calls self.deinit() internally.
        pub fn flush(self: *Self) void {
            self.deinit();
        }
    };
}
