//! Standalone tool to test multi-file directory indexation.
//! Usage: zig build parse-directory -- <directory> [--exclude path1,path2] [--help]
//!
//! Indexes all .zig files in the given directory, builds the full
//! code graph (cross-file edges, phantom nodes, metrics), and dumps
//! everything to stdout.

const std = @import("std");
const zcodeprism = @import("zcodeprism");

const Graph = zcodeprism.Graph;
const NodeId = zcodeprism.types.NodeId;
const NodeKind = zcodeprism.NodeKind;
const EdgeType = zcodeprism.EdgeType;
const Visibility = zcodeprism.Visibility;
const indexer = zcodeprism.indexer;
const logging = zcodeprism.logging;

const NO_FILE: usize = std.math.maxInt(usize);

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

fn fileDisplayName(g: *const Graph, file_idx: usize) []const u8 {
    if (file_idx == NO_FILE) return "<ext>";
    const n = g.nodes.items[file_idx];
    return n.file_path orelse n.name;
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
                if (zm.is_mutable) try stdout.print("  [mutable]", .{});
                if (zm.is_extern) try stdout.print("  [extern]", .{});
                if (zm.is_packed) try stdout.print("  [packed]", .{});
                if (zm.is_inline) try stdout.print("  [inline]", .{});
                if (zm.calling_convention) |cc| try stdout.print("  [callconv={s}]", .{cc});
            },
            else => {},
        }
        try stdout.print("\n", .{});
    }

    // Build file ancestor lookup table.
    // Invariant: parent indices are always less than child indices, so a single
    // forward pass resolves every node's owning file.
    const file_of = try allocator.alloc(usize, graph.nodes.items.len);
    defer allocator.free(file_of);
    for (graph.nodes.items, 0..) |n, i| {
        if (n.kind == .file) {
            file_of[i] = i;
        } else if (n.parent_id) |pid| {
            file_of[i] = file_of[@intFromEnum(pid)];
        } else {
            file_of[i] = NO_FILE;
        }
    }

    // List all edges with file annotations.
    if (graph.edges.items.len > 0) {
        try stdout.print("\n--- All edges ---\n", .{});

        const SummaryKey = struct {
            src_file: usize,
            tgt_file: usize,
            edge_type: EdgeType,
        };
        var summary_map = std.AutoHashMapUnmanaged(SummaryKey, u32){};
        defer summary_map.deinit(allocator);

        var cross_file_count: u32 = 0;

        for (graph.edges.items) |e| {
            const src_name = if (graph.getNode(e.source_id)) |n| n.name else "?";
            const tgt_name = if (graph.getNode(e.target_id)) |n| n.name else "?";
            const src_file_idx = file_of[@intFromEnum(e.source_id)];
            const tgt_file_idx = file_of[@intFromEnum(e.target_id)];
            const src_file_name = fileDisplayName(&graph, src_file_idx);
            const tgt_file_name = fileDisplayName(&graph, tgt_file_idx);

            const is_cross_file = src_file_idx != tgt_file_idx;

            try stdout.print("  {d} ({s} @ {s}) --[{s}/{s}]--> {d} ({s} @ {s})", .{
                @intFromEnum(e.source_id),
                src_name,
                src_file_name,
                @tagName(e.edge_type),
                @tagName(e.source),
                @intFromEnum(e.target_id),
                tgt_name,
                tgt_file_name,
            });

            if (is_cross_file) {
                try stdout.print("  [cross-file]", .{});
                cross_file_count += 1;
                const gop = try summary_map.getOrPut(allocator, .{
                    .src_file = src_file_idx,
                    .tgt_file = tgt_file_idx,
                    .edge_type = e.edge_type,
                });
                if (gop.found_existing) {
                    gop.value_ptr.* += 1;
                } else {
                    gop.value_ptr.* = 1;
                }
            }

            try stdout.print("\n", .{});
        }

        // Cross-file edge summary.
        if (cross_file_count > 0) {
            // Collect entries into a sortable list.
            const Entry = struct {
                key: SummaryKey,
                count: u32,
            };
            var entries = std.ArrayListUnmanaged(Entry){};
            defer entries.deinit(allocator);
            try entries.ensureTotalCapacity(allocator, summary_map.count());

            var it = summary_map.iterator();
            while (it.next()) |kv| {
                entries.appendAssumeCapacity(.{ .key = kv.key_ptr.*, .count = kv.value_ptr.* });
            }

            // Sort by (src_file_name, tgt_file_name, edge_type) for deterministic output.
            const g_ptr: *const Graph = &graph;
            std.mem.sort(Entry, entries.items, g_ptr, struct {
                fn lessThan(g: *const Graph, a: Entry, b: Entry) bool {
                    const src_a = fileDisplayName(g, a.key.src_file);
                    const src_b = fileDisplayName(g, b.key.src_file);
                    const src_ord = std.mem.order(u8, src_a, src_b);
                    if (src_ord != .eq) return src_ord == .lt;

                    const tgt_a = fileDisplayName(g, a.key.tgt_file);
                    const tgt_b = fileDisplayName(g, b.key.tgt_file);
                    const tgt_ord = std.mem.order(u8, tgt_a, tgt_b);
                    if (tgt_ord != .eq) return tgt_ord == .lt;

                    return @intFromEnum(a.key.edge_type) < @intFromEnum(b.key.edge_type);
                }
            }.lessThan);

            // Print grouped by file pair.
            try stdout.print("\n--- Cross-file edge summary ---\n", .{});
            var ei: usize = 0;
            while (ei < entries.items.len) {
                const cur_src = entries.items[ei].key.src_file;
                const cur_tgt = entries.items[ei].key.tgt_file;
                const src_name_display = fileDisplayName(&graph, cur_src);
                const tgt_name_display = fileDisplayName(&graph, cur_tgt);

                try stdout.print("  {s} -> {s}:", .{ src_name_display, tgt_name_display });

                var first = true;
                while (ei < entries.items.len and
                    entries.items[ei].key.src_file == cur_src and
                    entries.items[ei].key.tgt_file == cur_tgt)
                {
                    if (!first) {
                        try stdout.print(",", .{});
                    }
                    try stdout.print(" {} {s}", .{
                        entries.items[ei].count,
                        @tagName(entries.items[ei].key.edge_type),
                    });
                    first = false;
                    ei += 1;
                }
                try stdout.print("\n", .{});
            }
        }
    }

    try stdout.print("\n", .{});
    try stdout.flush();
}
