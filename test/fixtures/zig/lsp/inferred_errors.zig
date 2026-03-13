const std = @import("std");
const config = @import("config.zig");

pub fn readConfig(path: []const u8) ![]const u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    return try file.readToEndAlloc(std.heap.page_allocator, 1024);
}

pub fn processConfig() !void {
    const cfg = config.defaultConfig();
    const data = try readConfig(cfg.path);
    _ = data;
}

pub fn getMaxSize() usize {
    const cfg: config.Config = config.defaultConfig();
    return cfg.max_size;
}
