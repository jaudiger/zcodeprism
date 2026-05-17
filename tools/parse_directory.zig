//! Standalone tool to test multi-file directory indexation.
//! Usage: zig build parse-directory -- <directory> [--exclude path1,path2] [--help]
//!
//! Indexes all supported source files in the given directory, builds the full
//! code graph (cross-file edges, phantom nodes, metrics), and dumps
//! everything to stdout.

const std = @import("std");
const zcodeprism = @import("zcodeprism");
const tool_utils = @import("tool-utils");

const Graph = zcodeprism.Graph;
const NodeId = zcodeprism.types.NodeId;
const NodeKind = zcodeprism.NodeKind;
const EdgeType = zcodeprism.EdgeType;
const Visibility = zcodeprism.Visibility;
const indexer = zcodeprism.indexer;
const logging = zcodeprism.logging;

const NO_FILE: usize = std.math.maxInt(usize);

const help_text =
    \\parse-directory - Index all supported source files in a directory and dump the code graph.
    \\
    \\USAGE:
    \\    zig build parse-directory -- <directory> [OPTIONS]
    \\
    \\ARGUMENTS:
    \\    <directory>              Path to the directory to index
    \\
    \\OPTIONS:
    \\    --exclude path1,path2    Comma-separated paths to exclude from indexation
    \\    --without-lsp            Skip LSP enrichment
    \\    -v                       Increase verbosity (-v info, -vv debug, -vvv trace)
    \\    --verbose                Same as -v
    \\    -h, --help               Show this help message
    \\
    \\Indexes all supported source files in the given directory, builds the full
    \\code graph (cross-file edges, phantom nodes, metrics), and dumps everything
    \\to stdout.
    \\
;

fn fileDisplayName(g: *const Graph, file_idx: usize) []const u8 {
    if (file_idx == NO_FILE) return "<ext>";
    const n = g.nodes.items[file_idx];
    return n.file_path orelse n.name;
}

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const allocator = init.gpa;

    var stdout_buffer: [tool_utils.stdout_buffer_size]u8 = undefined;
    var stdout_writer = std.Io.File.stdout().writer(io, &stdout_buffer);
    const stdout = &stdout_writer.interface;

    // Parse CLI arguments.
    var args = init.minimal.args.iterate();
    _ = args.next(); // skip program name
    const dir_arg = args.next() orelse {
        try tool_utils.printHelp(stdout, help_text);
        return;
    };

    if (std.mem.eql(u8, dir_arg, "--help") or std.mem.eql(u8, dir_arg, "-h")) {
        try tool_utils.printHelp(stdout, help_text);
        return;
    }

    // Resolve to absolute path.
    const dir_path = std.Io.Dir.cwd().realPathFileAlloc(io, dir_arg, allocator) catch |err| {
        try stdout.print("Error resolving path '{s}': {}\n", .{ dir_arg, err });
        try stdout.flush();
        std.process.exit(1);
    };
    defer allocator.free(dir_path);

    // Parse optional flags.
    var common_flags = tool_utils.CommonFlags.init();
    defer common_flags.deinit(allocator);

    while (args.next()) |arg| {
        _ = try tool_utils.parseCommonFlag(allocator, arg, &args, &common_flags);
    }

    var text_logger = logging.TextStderrLogger.init(io, logging.verbosityToLevel(common_flags.verbosity));
    const log = if (common_flags.verbosity > 0) text_logger.logger() else logging.Logger.noop;

    const options = indexer.IndexOptions{
        .exclude_paths = common_flags.exclude.items,
        .logger = log,
    };

    // Index the directory.
    var graph = Graph.init(dir_path);
    defer graph.deinit(allocator);

    var wl = zcodeprism.lsp.worklist.LspWorklist{};
    defer wl.deinit(allocator);

    const result = indexer.indexDirectory(allocator, io, dir_path, &graph, &wl, options) catch |err| {
        try stdout.print("Index error: {}\n", .{err});
        try stdout.flush();
        std.process.exit(1);
    };

    if (common_flags.lsp) {
        try tool_utils.runLspEnrichment(allocator, io, &graph, &wl, log, stdout);
    }

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
        if (n.col_start) |cs| {
            if (n.col_end) |ce| {
                try stdout.print("  C{}-{}", .{ cs, ce });
            } else {
                try stdout.print("  C{}", .{cs});
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
            try stdout.print("  metrics(C={},L={}", .{ m.complexity, m.lines });
            if (m.structural_hash != 0) try stdout.print(",H={x:0>16}", .{m.structural_hash});
            if (m.fan_in != 0) try stdout.print(",FI={}", .{m.fan_in});
            if (m.fan_out != 0) try stdout.print(",FO={}", .{m.fan_out});
            if (m.branches != 0) try stdout.print(",B={}", .{m.branches});
            if (m.loops != 0) try stdout.print(",LP={}", .{m.loops});
            if (m.error_paths != 0) try stdout.print(",EP={}", .{m.error_paths});
            if (m.nesting_depth_max != 0) try stdout.print(",ND={}", .{m.nesting_depth_max});
            try stdout.print(")", .{});
        }
        if (n.doc != null) {
            try stdout.print("  [has doc]", .{});
        }
        if (n.signature) |sig| {
            try stdout.print("  sig=\"{s}\"", .{sig});
        }
        try zcodeprism.lang_meta.writeDebug(n, stdout);
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
            const pid_idx = @intFromEnum(pid);
            file_of[i] = if (pid_idx < file_of.len) file_of[pid_idx] else NO_FILE;
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
            const src_id_idx = @intFromEnum(e.source_id);
            const tgt_id_idx = @intFromEnum(e.target_id);
            const src_file_idx = if (src_id_idx < file_of.len) file_of[src_id_idx] else NO_FILE;
            const tgt_file_idx = if (tgt_id_idx < file_of.len) file_of[tgt_id_idx] else NO_FILE;
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
            var entries = std.ArrayList(Entry).empty;
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
