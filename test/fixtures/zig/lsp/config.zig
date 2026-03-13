pub const Config = struct {
    path: []const u8,
    max_size: usize,
};

pub fn defaultConfig() Config {
    return .{ .path = "config.txt", .max_size = 1024 };
}
