pub const Widget = struct {
    count: i32,

    const Self = @This();

    pub fn init() Self {
        return .{ .count = 0 };
    }

    pub fn increment(self: *Self) void {
        self.count += 1;
    }
};

pub fn standalone() void {}
