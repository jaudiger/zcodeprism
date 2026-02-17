const std = @import("std");

/// Format and output a numeric result.
pub fn formatOutput(value: usize) void {
    std.debug.print("Result: {}\n", .{value});
}

/// Check if a byte is a delimiter character.
pub fn isDelimiter(c: u8) bool {
    return c == ' ' or c == '\n' or c == '\t';
}

/// Validate that data contains only printable characters.
pub fn isValid(data: []const u8) bool {
    for (data) |c| {
        if (c < 0x20 or c > 0x7E) return false;
    }
    return true;
}
