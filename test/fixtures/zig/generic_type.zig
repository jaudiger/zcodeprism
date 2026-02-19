/// A generic container that wraps a slice of items.
/// This is the idiomatic Zig pattern for generics.
pub fn Container(comptime T: type) type {
    return struct {
        items: []T,
        len: usize,

        const Self = @This();

        /// Inner type representing a key-value entry.
        pub const Entry = struct {
            key: usize,
            value: T,
        };

        /// Create a new empty container.
        pub fn init() Self {
            return .{ .items = &.{}, .len = 0 };
        }

        /// Release all resources held by the container.
        pub fn deinit(self: *Self) void {
            _ = self;
        }

        /// Return the number of items in the container.
        pub fn count(self: Self) usize {
            return self.len;
        }

        /// Check whether the container is empty.
        /// Calls count() internally to determine emptiness.
        pub fn isEmpty(self: Self) bool {
            return self.count() == 0;
        }

        fn validate(self: Self) bool {
            _ = self;
            return true;
        }
    };
}

/// A generic result type representing success or failure.
pub fn Result(comptime T: type, comptime E: type) type {
    return union(enum) {
        ok: T,
        err: E,

        const Self = @This();

        /// Check whether the result is a success.
        pub fn isOk(self: Self) bool {
            return self == .ok;
        }
    };
}

/// A simple non-generic configuration struct for comparison.
pub const Config = struct {
    name: []const u8,
    verbose: bool,

    /// Create a default configuration.
    pub fn defaults() Config {
        return .{ .name = "default", .verbose = false };
    }
};
