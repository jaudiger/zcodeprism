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

        /// Reset to a fresh empty container via Self.init().
        pub fn reset(self: *Self) Self {
            _ = self;
            return Self.init();
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

/// A generic enum whose variants depend on a comptime flag.
pub fn StatusEnum(comptime has_pending: bool) type {
    return if (has_pending) enum { ok, pending, fail } else enum { ok, fail };
}

/// A generic union wrapping a value or an empty sentinel.
pub fn ValueUnion(comptime T: type) type {
    return union { value: T, empty: void };
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
