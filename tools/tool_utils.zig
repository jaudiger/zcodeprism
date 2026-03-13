//! Shared utilities for the debug tools in tools/.

const std = @import("std");
const zcodeprism = @import("zcodeprism");
const logging = zcodeprism.logging;

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

/// Flags shared across all directory-indexing debug tools.
pub const CommonFlags = struct {
    exclude: std.ArrayList([]const u8),
    verbosity: u8,
    /// When false, LSP enrichment is skipped (--without-lsp).
    lsp: bool,

    pub fn init() CommonFlags {
        return .{ .exclude = .{}, .verbosity = 0, .lsp = true };
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

/// Run LSP enrichment over all registered languages and print a summary line.
pub fn runLspEnrichment(
    allocator: std.mem.Allocator,
    graph: *zcodeprism.Graph,
    log: logging.Logger,
    stdout: *std.Io.Writer,
) !void {
    const Registry = zcodeprism.registry.Registry;
    const EnrichResult = zcodeprism.language_support.EnrichResult;
    var result = EnrichResult{};

    for (Registry.allLanguages()) |ls| {
        var empty_wl = zcodeprism.lsp.worklist.LspWorklist{};
        defer empty_wl.deinit(allocator);
        const r = try zcodeprism.lsp.enricher.enrich(allocator, graph, ls, &empty_wl, .{ .logger = log });
        result.accumulate(r);
    }

    printEnrichSummary(stdout, result);
}

fn printEnrichSummary(stdout: *std.Io.Writer, result: zcodeprism.language_support.EnrichResult) void {
    const fields = .{
        .{ result.edges_promoted, "edges promoted" },
        .{ result.edges_added, "edges added" },
        .{ result.errors_inferred, "errors inferred" },
        .{ result.phantoms_enriched, "phantoms enriched" },
    };

    var has_any = false;
    inline for (fields) |f| {
        if (f[0] > 0) has_any = true;
    }
    if (!has_any) return;

    stdout.writeAll("LSP enrichment:") catch return;
    var first = true;
    inline for (fields) |f| {
        if (f[0] > 0) {
            stdout.print("{s}{} {s}", .{
                if (first) @as([]const u8, " ") else @as([]const u8, ", "),
                f[0],
                f[1],
            }) catch return;
            first = false;
        }
    }
    stdout.writeAll("\n") catch {};
}
