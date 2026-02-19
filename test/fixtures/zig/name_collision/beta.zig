const Self = @This();

name: []const u8 = "",

pub fn init(n: []const u8) Self {
    return .{ .name = n };
}

pub fn deinit(self: *Self) void {
    _ = self;
}
