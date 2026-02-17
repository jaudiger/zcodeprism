//! A simple counter backed by an integer.

value: i32 = 0,
name: []const u8 = "default",

const Self = @This();

pub fn init(v: i32, n: []const u8) Self {
    return .{ .value = v, .name = n };
}

/// Get the current value.
pub fn getValue(self: Self) i32 {
    return self.value;
}

fn validate(self: Self) bool {
    return self.value >= 0;
}

pub fn isValid(self: Self) bool {
    return self.validate();
}

pub const Config = struct {
    max: i32 = 100,
};

test "basic" {
    var c = Self.init(5, "test");
    _ = c.getValue();
}
