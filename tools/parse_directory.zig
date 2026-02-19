//! Standalone tool to test multi-file directory indexation.
//! Usage: zig build parse-directory -- <directory> [--exclude path1,path2] [--help]
//!
//! Indexes all .zig files in the given directory, builds the full
//! code graph (cross-file edges, phantom nodes, metrics), and dumps
//! everything to stdout.

const std = @import("std");
const zcodeprism = @import("zcodeprism");

const Graph = zcodeprism.Graph;
const NodeKind = zcodeprism.NodeKind;
const EdgeType = zcodeprism.EdgeType;
const Visibility = zcodeprism.Visibility;
const indexer = zcodeprism.indexer;
const logging = zcodeprism.logging;

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
        \\parse-directory — Index all .zig files in a directory and dump the code graph.
        \\
        \\USAGE:
        \\    zig build parse-directory -- <directory> [OPTIONS]
        \\
        \\ARGUMENTS:
        \\    <directory>              Path to the directory to index
        \\
        \\OPTIONS:
        \\    --exclude path1,path2    Comma-separated paths to exclude from indexation
        \\    -v                       Increase verbosity (-v info, -vv debug, -vvv trace)
        \\    --verbose                Same as -v
        \\    -h, --help               Show this help message
        \\
        \\Indexes all .zig files in the given directory, builds the full code graph
        \\(cross-file edges, phantom nodes, metrics), and dumps everything to stdout.
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
    const dir_arg = args.next() orelse {
        try printHelp(stdout);
        return;
    };

    if (std.mem.eql(u8, dir_arg, "--help") or std.mem.eql(u8, dir_arg, "-h")) {
        try printHelp(stdout);
        return;
    }

    // Resolve to absolute path.
    const dir_path = std.fs.cwd().realpathAlloc(allocator, dir_arg) catch |err| {
        try stdout.print("Error resolving path '{s}': {}\n", .{ dir_arg, err });
        try stdout.flush();
        return;
    };
    defer allocator.free(dir_path);

    // Parse optional flags.
    var exclude_list: std.ArrayListUnmanaged([]const u8) = .{};
    defer exclude_list.deinit(allocator);
    var verbosity: u8 = 0;

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--exclude")) {
            if (args.next()) |csv| {
                var it = std.mem.splitScalar(u8, csv, ',');
                while (it.next()) |p| {
                    if (p.len > 0) {
                        try exclude_list.append(allocator, p);
                    }
                }
            }
        } else {
            const v = countVerbosity(arg);
            if (v > 0) {
                verbosity +|= v;
            }
        }
    }

    const min_level: logging.Level = switch (verbosity) {
        0 => .warn,
        1 => .info,
        2 => .debug,
        else => .trace,
    };
    var text_logger = logging.TextStderrLogger.init(min_level);
    const log = if (verbosity > 0) text_logger.logger() else logging.Logger.noop;

    const options = indexer.IndexOptions{
        .exclude_paths = exclude_list.items,
        .logger = log,
    };

    // Index the directory.
    var graph = Graph.init(allocator, dir_path);
    defer graph.deinit();

    const result = indexer.indexDirectory(allocator, dir_path, &graph, options) catch |err| {
        try stdout.print("Index error: {}\n", .{err});
        try stdout.flush();
        return;
    };

    // Summary.
    try stdout.print("=== Directory: {s} ===\n", .{dir_path});
    try stdout.print("Files indexed: {}\n", .{result.files_indexed});
    try stdout.print("Files skipped: {}\n", .{result.files_skipped});
    try stdout.print("Nodes: {}\n", .{graph.nodes.items.len});
    try stdout.print("Edges: {}\n\n", .{graph.edges.items.len});

    // Count by kind.
    var kind_counts: [@typeInfo(NodeKind).@"enum".fields.len]u32 = .{0} ** @typeInfo(NodeKind).@"enum".fields.len;
    for (graph.nodes.items) |n| {
        kind_counts[@intFromEnum(n.kind)] += 1;
    }
    try stdout.print("--- Node counts by kind ---\n", .{});
    inline for (@typeInfo(NodeKind).@"enum".fields, 0..) |f, i| {
        if (kind_counts[i] > 0) {
            try stdout.print("  {s}: {}\n", .{ f.name, kind_counts[i] });
        }
    }

    // Count by edge type.
    var edge_counts: [@typeInfo(EdgeType).@"enum".fields.len]u32 = .{0} ** @typeInfo(EdgeType).@"enum".fields.len;
    for (graph.edges.items) |e| {
        edge_counts[@intFromEnum(e.edge_type)] += 1;
    }
    try stdout.print("\n--- Edge counts by type ---\n", .{});
    inline for (@typeInfo(EdgeType).@"enum".fields, 0..) |f, i| {
        if (edge_counts[i] > 0) {
            try stdout.print("  {s}: {}\n", .{ f.name, edge_counts[i] });
        }
    }

    // List all nodes.
    try stdout.print("\n--- All nodes ---\n", .{});
    for (graph.nodes.items) |n| {
        const vis_str: []const u8 = if (n.visibility == .public) "pub" else "prv";
        try stdout.print("  [{d:>3}] {s:<12} {s} \"{s}\"", .{
            @intFromEnum(n.id),
            @tagName(n.kind),
            vis_str,
            n.name,
        });
        if (n.line_start) |ls| {
            if (n.line_end) |le| {
                try stdout.print("  L{}-{}", .{ ls, le });
            } else {
                try stdout.print("  L{}", .{ls});
            }
        }
        if (n.parent_id) |pid| {
            try stdout.print("  parent={}", .{@intFromEnum(pid)});
        }
        if (n.file_path) |fp| {
            try stdout.print("  file=\"{s}\"", .{fp});
        }
        if (n.content_hash) |hash| {
            try stdout.print("  hash=", .{});
            for (hash) |byte| {
                try stdout.print("{x:0>2}", .{byte});
            }
        }
        switch (n.external) {
            .stdlib => try stdout.print("  [phantom/stdlib]", .{}),
            .dependency => try stdout.print("  [phantom/dep]", .{}),
            .none => {},
        }
        if (n.metrics) |m| {
            try stdout.print("  metrics(C={},L={})", .{ m.complexity, m.lines });
        }
        if (n.doc != null) {
            try stdout.print("  [has doc]", .{});
        }
        if (n.signature) |sig| {
            try stdout.print("  sig=\"{s}\"", .{sig[0..@min(sig.len, 60)]});
        }
        switch (n.lang_meta) {
            .zig => |zm| {
                if (zm.is_comptime) try stdout.print("  [comptime]", .{});
            },
            else => {},
        }
        try stdout.print("\n", .{});
    }

    // List all edges.
    if (graph.edges.items.len > 0) {
        try stdout.print("\n--- All edges ---\n", .{});
        for (graph.edges.items) |e| {
            const src_name = if (graph.getNode(e.source_id)) |n| n.name else "?";
            const tgt_name = if (graph.getNode(e.target_id)) |n| n.name else "?";
            try stdout.print("  {d} ({s}) --[{s}/{s}]--> {d} ({s})\n", .{
                @intFromEnum(e.source_id),
                src_name,
                @tagName(e.edge_type),
                @tagName(e.source),
                @intFromEnum(e.target_id),
                tgt_name,
            });
        }
    }

    try stdout.print("\n", .{});
    try stdout.flush();
}
