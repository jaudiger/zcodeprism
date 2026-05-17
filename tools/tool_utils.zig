//! Shared utilities for the debug tools in tools/.

const std = @import("std");
const zcodeprism = @import("zcodeprism");
const logging = zcodeprism.logging;

/// Standard stdout buffer size for all debug tools.
pub const stdout_buffer_size: usize = 65536;

/// Write a tool's help text to stdout and flush.
pub fn printHelp(stdout: *std.Io.Writer, help_text: []const u8) !void {
    try stdout.writeAll(help_text);
    try stdout.flush();
}

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

/// Flags shared across all directory-indexing debug tools.
pub const CommonFlags = struct {
    exclude: std.ArrayList([]const u8),
    verbosity: u8,
    /// When false, LSP enrichment is skipped (--without-lsp).
    lsp: bool,

    pub fn init() CommonFlags {
        return .{ .exclude = .empty, .verbosity = 0, .lsp = true };
    }

    pub fn deinit(self: *CommonFlags, allocator: std.mem.Allocator) void {
        self.exclude.deinit(allocator);
    }
};

/// Thin iterator over a string slice, compatible with parseCommonFlag.
pub const SliceIter = struct {
    items: []const []const u8,
    idx: usize,

    pub fn init(items: []const []const u8) SliceIter {
        return .{ .items = items, .idx = 0 };
    }

    pub fn next(self: *SliceIter) ?[]const u8 {
        if (self.idx >= self.items.len) return null;
        defer self.idx += 1;
        return self.items[self.idx];
    }
};

/// Try to consume `arg` as a common flag. Advances `args` when the flag
/// takes a value (--exclude). Returns true when the arg was consumed.
/// Compatible with *std.process.ArgIterator and *SliceIter.
pub fn parseCommonFlag(
    allocator: std.mem.Allocator,
    arg: []const u8,
    args: anytype,
    flags: *CommonFlags,
) !bool {
    if (std.mem.eql(u8, arg, "--exclude")) {
        if (args.next()) |csv| {
            var it = std.mem.splitScalar(u8, csv, ',');
            while (it.next()) |p| {
                if (p.len > 0) try flags.exclude.append(allocator, p);
            }
        }
        return true;
    } else if (std.mem.eql(u8, arg, "--without-lsp")) {
        flags.lsp = false;
        return true;
    } else {
        const v = countVerbosity(arg);
        if (v > 0) {
            flags.verbosity +|= v;
            return true;
        }
    }
    return false;
}

/// Run LSP enrichment over all registered languages and print a summary
/// line. Thin convenience wrapper for debug tools; delegates to
/// `zcodeprism.lsp.enricher.enrichAllLanguages` for the actual work.
pub fn runLspEnrichment(
    allocator: std.mem.Allocator,
    io: std.Io,
    graph: *zcodeprism.Graph,
    wl: *zcodeprism.lsp.worklist.LspWorklist,
    log: logging.Logger,
    stdout: *std.Io.Writer,
) !void {
    var lsp_pool = zcodeprism.lsp.pool.LspPool.init(.{});
    defer lsp_pool.deinit(allocator, io);

    const result = try zcodeprism.lsp.enricher.enrichAllLanguages(allocator, io, graph, wl, &lsp_pool, .{ .logger = log });
    result.format(stdout) catch {};
}
