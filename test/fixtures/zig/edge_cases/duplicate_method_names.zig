//! Two structs with identical method names to test scope-aware name resolution.
//! The visitor must not resolve Alpha.deinit to Beta.deinit (or vice versa)
//! just because they share the name "deinit".

/// First struct with common method names.
pub const Alpha = struct {
    value: i32,

    const Self = @This();

    pub fn init(v: i32) Self {
        return .{ .value = v };
    }

    pub fn deinit(self: *Self) void {
        self.value = 0;
    }

    /// Resets by calling deinit on self.
    pub fn reset(self: *Self) void {
        self.deinit();
    }
};

/// Second struct with identical method names.
pub const Beta = struct {
    count: u32,

    const Self = @This();

    pub fn init(c: u32) Self {
        return .{ .count = c };
    }

    pub fn deinit(self: *Self) void {
        self.count = 0;
    }

    /// Clears by calling deinit on self.
    pub fn clear(self: *Self) void {
        self.deinit();
    }
};
