//! Standalone tool to render a code graph in CTG or Mermaid format.
//! Usage: zig build render-graph -- <directory> [--format ctg|mermaid] [--name <project-name>] [--exclude path1,path2] [--test-nodes] [--external-nodes] [--help]
//!
//! Indexes all .zig files in the given directory, renders the full
//! code graph in the chosen format, and writes to stdout.

const std = @import("std");
const zcodeprism = @import("zcodeprism");

const Graph = zcodeprism.Graph;
const indexer = zcodeprism.indexer;
const ctg = zcodeprism.ctg;
const logging = zcodeprism.logging;
const mermaid = zcodeprism.mermaid;

const Format = enum { ctg, mermaid_fmt };

fn countVerbosity(arg: []const u8) u8 {
    if (std.mem.eql(u8, arg, "--verbose")) return 1;
    if (arg.len >= 2 and arg[0] == '-' and arg[1] != '-') {
        for (arg[1..]) |c| {
            if (c != 'v') return 0;
        }
        return @intCast(arg.len - 1);
    }
    return 0;
}

fn printHelp(stdout: *std.Io.Writer) !void {
    try stdout.print(
        \\render-graph - Index and render a code graph in CTG or Mermaid format.
        \\
        \\USAGE:
        \\    zig build render-graph -- <directory> [OPTIONS]
        \\
        \\ARGUMENTS:
        \\    <directory>              Path to the directory to index
        \\
        \\OPTIONS:
        \\    --format ctg|mermaid     Output format (default: ctg)
        \\    --name <project-name>    Project name (default: directory basename)
        \\    --exclude path1,path2    Comma-separated paths to exclude from indexation
        \\    --test-nodes             Include test nodes in output
        \\    --external-nodes         Include external nodes in output
        \\    -v                       Increase verbosity (-v info, -vv debug, -vvv trace)
        \\    --verbose                Same as -v
        \\    -h, --help               Show this help message
        \\
    , .{});
    try stdout.flush();
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var stdout_buffer: [8192]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    // Parse CLI arguments.
    var args = std.process.args();
    _ = args.next(); // skip program name

    var dir_arg: ?[]const u8 = null;
    var format: Format = .ctg;
    var project_name: ?[]const u8 = null;
    var include_test_nodes: bool = false;
    var include_external_nodes: bool = false;
    var verbosity: u8 = 0;
    var exclude_list: std.ArrayList([]const u8) = .{};
    defer exclude_list.deinit(allocator);

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--format")) {
            const fmt_str = args.next() orelse {
                try stdout.print("Error: --format requires a value (ctg or mermaid)\n", .{});
                try stdout.flush();
                return;
            };
            if (std.mem.eql(u8, fmt_str, "ctg")) {
                format = .ctg;
            } else if (std.mem.eql(u8, fmt_str, "mermaid")) {
                format = .mermaid_fmt;
            } else {
                try stdout.print("Error: unknown format '{s}', expected 'ctg' or 'mermaid'\n", .{fmt_str});
                try stdout.flush();
                return;
            }
        } else if (std.mem.eql(u8, arg, "--name")) {
            project_name = args.next() orelse {
                try stdout.print("Error: --name requires a value\n", .{});
                try stdout.flush();
                return;
            };
        } else if (std.mem.eql(u8, arg, "--exclude")) {
            if (args.next()) |csv| {
                var it = std.mem.splitScalar(u8, csv, ',');
                while (it.next()) |path| {
                    if (path.len > 0) {
                        try exclude_list.append(allocator, path);
                    }
                }
            }
        } else if (std.mem.eql(u8, arg, "--test-nodes")) {
            include_test_nodes = true;
        } else if (std.mem.eql(u8, arg, "--external-nodes")) {
            include_external_nodes = true;
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            try printHelp(stdout);
            return;
        } else {
            const v = countVerbosity(arg);
            if (v > 0) {
                verbosity +|= v;
            } else {
                dir_arg = arg;
            }
        }
    }

    const dir_path_arg = dir_arg orelse {
        try printHelp(stdout);
        return;
    };

    // Resolve to absolute path.
    const dir_path = std.fs.cwd().realpathAlloc(allocator, dir_path_arg) catch |err| {
        try stdout.print("Error resolving path '{s}': {}\n", .{ dir_path_arg, err });
        try stdout.flush();
        return;
    };
    defer allocator.free(dir_path);

    // Derive project name from directory basename if not provided.
    const name = project_name orelse std.fs.path.basename(dir_path);

    // Index the directory.
    var graph = Graph.init(allocator, dir_path);
    defer graph.deinit();

    const min_level: logging.Level = switch (verbosity) {
        0 => .warn,
        1 => .info,
        2 => .debug,
        else => .trace,
    };
    var text_logger = logging.TextStderrLogger.init(min_level);
    const log = if (verbosity > 0) text_logger.logger() else logging.Logger.noop;

    _ = indexer.indexDirectory(allocator, dir_path, &graph, .{
        .exclude_paths = exclude_list.items,
        .logger = log,
    }) catch |err| {
        try stdout.print("Index error: {}\n", .{err});
        try stdout.flush();
        return;
    };

    // Render.
    var out: std.ArrayList(u8) = .{};
    defer out.deinit(allocator);

    const render_common = zcodeprism.render_common;
    const filter = render_common.FilterOptions{
        .include_test_nodes = include_test_nodes,
        .include_external_nodes = include_external_nodes,
    };

    switch (format) {
        .ctg => {
            ctg.renderCtg(allocator, &graph, .{
                .project_name = name,
                .filter = filter,
            }, &out) catch |err| {
                try stdout.print("Render error: {}\n", .{err});
                try stdout.flush();
                return;
            };
        },
        .mermaid_fmt => {
            mermaid.renderMermaid(allocator, &graph, .{
                .project_name = name,
                .filter = filter,
            }, &out) catch |err| {
                try stdout.print("Render error: {}\n", .{err});
                try stdout.flush();
                return;
            };
        },
    }

    // Write output to stdout.
    try stdout.writeAll(out.items);
    try stdout.flush();
}
