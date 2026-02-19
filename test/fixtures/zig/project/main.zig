const std = @import("std");
const parser = @import("parser.zig");
const utils = @import("utils.zig");

/// Process input data using the parser and format the output.
pub fn processInput(allocator: std.mem.Allocator, input: []const u8) !void {
    var p = parser.Parser.init(allocator);
    defer p.deinit();
    const count = try p.parse(input);
    utils.formatOutput(count);
}

fn validateInput(data: []const u8) bool {
    return data.len > 0 and utils.isValid(data);
}
