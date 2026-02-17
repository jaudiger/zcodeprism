const Self = @This();

value: u32 = 0,

pub fn init(v: u32) Self {
    return .{ .value = v };
}

pub fn deinit(self: *Self) void {
    _ = self;
}
