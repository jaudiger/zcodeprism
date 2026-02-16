const std = @import("std");

pub fn main() !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    var args = std.process.args();
    _ = args.next(); // skip program name

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--version")) {
            try stdout.print("zcodeprism 0.1.0\n", .{});
            try stdout.flush();
            return;
        }
    }

    try stdout.print("zcodeprism 0.1.0\nUsage: zcodeprism [command] [options]\n", .{});
    try stdout.flush();
}
