//! Shared utilities for the debug tools in tools/.

const std = @import("std");
const logging = @import("zcodeprism").logging;

/// Standard stdout buffer size for all debug tools.
pub const stdout_buffer_size: usize = 65536;

/// Parses a single CLI arg and returns how many verbosity levels it adds.
/// Handles --verbose (1) and -v/-vv/-vvv style flags.
pub fn countVerbosity(arg: []const u8) u8 {
    if (std.mem.eql(u8, arg, "--verbose")) return 1;
    if (arg.len >= 2 and arg[0] == '-' and arg[1] != '-') {
        for (arg[1..]) |c| {
            if (c != 'v') return 0;
        }
        return @intCast(arg.len - 1);
    }
    return 0;
}

/// Maps a verbosity count to the minimum log level for TextStderrLogger.
pub fn verbosityToLevel(verbosity: u8) logging.Level {
    return switch (verbosity) {
        0 => .warn,
        1 => .info,
        2 => .debug,
        else => .trace,
    };
}
