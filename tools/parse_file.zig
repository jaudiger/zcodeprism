//! Quick standalone tool to test the single-file visitor for any supported language.
//! Usage: zig build parse-file -- <path-to-source-file> [--help]
//!
//! Reads a source file, dispatches to the appropriate language visitor
//! via the Registry, and dumps all nodes and edges to stdout.

const std = @import("std");
const zcodeprism = @import("zcodeprism");
const tool_utils = @import("tool-utils");

const Graph = zcodeprism.Graph;
const Node = zcodeprism.Node;
const NodeKind = zcodeprism.NodeKind;
const EdgeType = zcodeprism.EdgeType;
const Visibility = zcodeprism.Visibility;
const Registry = zcodeprism.registry.Registry;
const source_map = zcodeprism.source_map;

const logging = zcodeprism.logging;

fn printHelp(stdout: *std.Io.Writer) !void {
    try stdout.print(
        \\parse-file - Parse a source file and dump the semantic graph.
        \\
        \\USAGE:
        \\    zig build parse-file -- <path-to-source-file>
        \\
        \\ARGUMENTS:
        \\    <path-to-source-file>    Path to a supported source file
        \\
        \\OPTIONS:
        \\    -v                       Increase verbosity (-v info, -vv debug, -vvv trace)
        \\    --verbose                Same as -v
        \\    -h, --help               Show this help message
        \\
        \\Parses a source file through the language-specific visitor and dumps all
        \\nodes (with kind, visibility, parent chain, doc, lang_meta) and all
        \\edges (with source/target names and edge type).
        \\
    , .{});
    try stdout.flush();
}

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const allocator = init.gpa;

    var stdout_buffer: [tool_utils.stdout_buffer_size]u8 = undefined;
    var stdout_writer = std.Io.File.stdout().writer(io, &stdout_buffer);
    const stdout = &stdout_writer.interface;

    // Get file path from args.
    var args = init.minimal.args.iterate();
    _ = args.next(); // skip program name

    const path_arg = args.next() orelse {
        try printHelp(stdout);
        return;
    };
    if (std.mem.eql(u8, path_arg, "--help") or std.mem.eql(u8, path_arg, "-h")) {
        try printHelp(stdout);
        return;
    }
    const path = path_arg;

    var verbosity: u8 = 0;
    while (args.next()) |arg| {
        verbosity +|= tool_utils.countVerbosity(arg);
    }

    // Resolve to absolute path so the graph root is accurate.
    const abs_path = std.Io.Dir.cwd().realPathFileAlloc(io, path, allocator) catch |err| {
        try stdout.print("Error resolving path '{s}': {}\n", .{ path, err });
        try stdout.flush();
        std.process.exit(1);
    };
    defer allocator.free(abs_path);

    // Look up language by file extension.
    const ext = std.fs.path.extension(abs_path);
    const lang_support = Registry.getByExtension(ext) orelse {
        try stdout.print("Unsupported file extension: '{s}'\n", .{ext});
        try stdout.flush();
        std.process.exit(1);
    };
    const parseFn = lang_support.parseFn;

    // Read the file.
    const source = source_map.mmapFile(io, abs_path) catch |err| {
        try stdout.print("Error opening file '{s}': {}\n", .{ abs_path, err });
        try stdout.flush();
        std.process.exit(1);
    };
    defer source_map.unmapFile(source);

    // Parse.
    var graph = Graph.init(std.fs.path.dirname(abs_path) orelse abs_path);
    defer graph.deinit(allocator);

    var text_logger = logging.TextStderrLogger.init(io, logging.verbosityToLevel(verbosity));
    const log = if (verbosity > 0) text_logger.logger() else logging.Logger.noop;

    parseFn(allocator, io, source, &graph, null, log) catch |err| {
        try stdout.print("Parse error: {}\n", .{err});
        try stdout.flush();
        std.process.exit(1);
    };

    // Build edges (separated from parsing so the indexer can defer edge
    // building to a post-parse pass with the complete graph).
    if (lang_support.buildEdgesFn) |build_edges| {
        var graph_index = zcodeprism.graph_index_mod.GraphIndex.build(allocator, graph.nodes.items) catch |err| {
            try stdout.print("Graph index error: {}\n", .{err});
            try stdout.flush();
            std.process.exit(1);
        };
        defer graph_index.deinit(allocator);
        var phantom_mgr = zcodeprism.phantom.PhantomManager.init(&graph);
        defer phantom_mgr.deinit(allocator);
        var wl = zcodeprism.lsp.worklist.LspWorklist{};
        defer wl.deinit(allocator);
        var node_type_map = zcodeprism.language_support.NodeTypeMap{};
        defer node_type_map.deinit(allocator);
        build_edges(allocator, io, source, &graph, 0, graph.nodeCount(), null, &graph_index, &phantom_mgr, &node_type_map, &wl, log) catch |err| {
            try stdout.print("Edge building error: {}\n", .{err});
            try stdout.flush();
            std.process.exit(1);
        };
    }

    // Dump results.
    try stdout.print("=== File: {s} ===\n", .{abs_path});
    try stdout.print("Source size: {} bytes\n", .{source.len});
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
        if (n.doc != null) {
            try stdout.print("  [has doc]", .{});
        }
        if (n.signature) |sig| {
            try stdout.print("  sig=\"{s}\"", .{sig});
        }
        try zcodeprism.lang_meta.writeDebug(n, stdout);
        try stdout.print("\n", .{});
    }

    // List all edges.
    if (graph.edges.items.len > 0) {
        try stdout.print("\n--- All edges ---\n", .{});
        for (graph.edges.items) |e| {
            // Get node names for readability.
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
