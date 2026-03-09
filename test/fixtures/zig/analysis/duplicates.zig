const std = @import("std");

/// Process a list of items by validating, transforming, and collecting results.
pub fn processItems(allocator: std.mem.Allocator, items: []const []const u8) ![][]const u8 {
    var result = std.ArrayList([]const u8).init(allocator);
    errdefer result.deinit();

    for (items) |item| {
        if (item.len == 0) continue;
        if (item[0] == '#') continue;

        var buf: [256]u8 = undefined;
        const len = @min(item.len, buf.len);
        @memcpy(buf[0..len], item[0..len]);

        try result.append(buf[0..len]);
    }

    return result.toOwnedSlice();
}

/// Process a list of entries by validating, transforming, and collecting results.
pub fn processEntries(allocator: std.mem.Allocator, entries: []const []const u8) ![][]const u8 {
    var result = std.ArrayList([]const u8).init(allocator);
    errdefer result.deinit();

    for (entries) |entry| {
        if (entry.len == 0) continue;
        if (entry[0] == '#') continue;

        var buf: [256]u8 = undefined;
        const len = @min(entry.len, buf.len);
        @memcpy(buf[0..len], entry[0..len]);

        try result.append(buf[0..len]);
    }

    return result.toOwnedSlice();
}

/// Short helper with completely different structure.
pub fn isEmpty(data: []const u8) bool {
    return data.len == 0;
}
