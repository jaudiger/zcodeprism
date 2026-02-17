const std = @import("std");
const utils = @import("utils.zig");

/// A simple token parser.
pub const Parser = struct {
    allocator: std.mem.Allocator,
    position: usize,

    pub fn init(allocator: std.mem.Allocator) Parser {
        return .{ .allocator = allocator, .position = 0 };
    }

    pub fn deinit(self: *Parser) void {
        _ = self;
    }

    /// Parse input and return the number of tokens found.
    pub fn parse(self: *Parser, input: []const u8) !usize {
        if (input.len == 0) return 0;
        var count: usize = 0;
        for (input) |c| {
            if (utils.isDelimiter(c)) {
                count += 1;
            }
            self.position += 1;
        }
        return count;
    }
};
