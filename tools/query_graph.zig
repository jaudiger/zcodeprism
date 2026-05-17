//! Standalone tool to exercise the query engine against an indexed directory.
//! Usage: zig build query-graph -- <directory> <command> [OPTIONS]

const std = @import("std");
const zcodeprism = @import("zcodeprism");
const tool_utils = @import("tool-utils");

const FrozenGraph = zcodeprism.FrozenGraph;
const Graph = zcodeprism.Graph;
const Node = zcodeprism.Node;
const NodeId = zcodeprism.types.NodeId;
const NodeKind = zcodeprism.NodeKind;
const EdgeType = zcodeprism.EdgeType;
const indexer = zcodeprism.indexer;
const logging = zcodeprism.logging;
const query = zcodeprism.query;

const ParsedFlags = struct {
    common: tool_utils.CommonFlags,
    kind: ?NodeKind = null,
    scope: ?[]const u8 = null,
    include_tests: bool = false,
    include_external: bool = false,
    limit: u32 = 50,
    positional: std.ArrayList([]const u8),

    fn deinit(self: *ParsedFlags, allocator: std.mem.Allocator) void {
        self.common.deinit(allocator);
        self.positional.deinit(allocator);
    }
};

fn parseFlags(allocator: std.mem.Allocator, raw_args: []const []const u8) !ParsedFlags {
    var flags = ParsedFlags{ .common = tool_utils.CommonFlags.init(), .positional = .empty };
    errdefer flags.deinit(allocator);
    var iter = tool_utils.SliceIter.init(raw_args);
    while (iter.next()) |a| {
        if (try tool_utils.parseCommonFlag(allocator, a, &iter, &flags.common)) continue;
        if (std.mem.eql(u8, a, "--kind")) {
            if (iter.next()) |v| flags.kind = parseNodeKind(v);
        } else if (std.mem.eql(u8, a, "--scope")) {
            if (iter.next()) |v| flags.scope = v;
        } else if (std.mem.eql(u8, a, "--include-tests")) {
            flags.include_tests = true;
        } else if (std.mem.eql(u8, a, "--include-external")) {
            flags.include_external = true;
        } else if (std.mem.eql(u8, a, "--limit")) {
            if (iter.next()) |v| flags.limit = std.fmt.parseInt(u32, v, 10) catch 50;
        } else {
            try flags.positional.append(allocator, a);
        }
    }
    return flags;
}

fn requirePositionalId(positional: []const []const u8, idx: usize, stdout: *std.Io.Writer, usage: []const u8) !?NodeId {
    if (idx >= positional.len) {
        try stdout.print("{s}\n", .{usage});
        try stdout.flush();
        return null;
    }
    return parseNodeId(positional[idx]) orelse {
        try stdout.print("Invalid node ID: {s}\n", .{positional[idx]});
        try stdout.flush();
        return null;
    };
}

// -- Parsing helpers --

fn parseNodeId(s: []const u8) ?NodeId {
    return zcodeprism.types.parseNodeId(s, 10);
}

fn parseNodeKind(s: []const u8) ?NodeKind {
    return zcodeprism.types.parseEnum(NodeKind, s);
}

// -- Display helpers --

fn printNodeSummary(stdout: *std.Io.Writer, g: FrozenGraph, id: NodeId) !void {
    const n = g.getNode(id) orelse {
        try stdout.print("  [{d}] <not found>\n", .{@intFromEnum(id)});
        return;
    };
    const vis_str: []const u8 = if (n.visibility == .public) "pub" else "prv";
    try stdout.print("  [{d:>3}] {s:<12} {s} \"{s}\"", .{
        @intFromEnum(id), @tagName(n.kind), vis_str, n.name,
    });
    if (n.file_path) |fp| try stdout.print("  file=\"{s}\"", .{fp});
    if (n.line_start) |ls| {
        if (n.line_end) |le| try stdout.print("  L{}-{}", .{ ls, le });
    }
    if (n.metrics) |m| try stdout.print("  C={} L={}", .{ m.complexity, m.lines });
    switch (n.external) {
        .stdlib => try stdout.print("  [stdlib]", .{}),
        .dependency => try stdout.print("  [dep]", .{}),
        .none => {},
    }
    try stdout.print("\n", .{});
}

fn printNodeDetail(stdout: *std.Io.Writer, nd: query.NodeDetail) !void {
    const n = nd.node;
    try stdout.print("Node {d}:\n", .{@intFromEnum(nd.id)});
    try stdout.print("  name:       \"{s}\"\n", .{n.name});
    try stdout.print("  kind:       {s}\n", .{@tagName(n.kind)});
    try stdout.print("  visibility: {s}\n", .{@tagName(n.visibility)});
    if (n.language) |l| try stdout.print("  language:   {s}\n", .{@tagName(l)});
    if (n.file_path) |fp| try stdout.print("  file:       {s}\n", .{fp});
    if (n.line_start) |ls| {
        if (n.line_end) |le| try stdout.print("  lines:      {d}-{d}\n", .{ ls, le });
    }
    if (n.parent_id) |pid| try stdout.print("  parent:     {d}\n", .{@intFromEnum(pid)});
    if (n.doc) |d| try stdout.print("  doc:        \"{s}\"\n", .{d});
    if (n.signature) |s| try stdout.print("  signature:  \"{s}\"\n", .{s});
    if (n.metrics) |m| {
        try stdout.print("  metrics:    C={d} L={d}", .{ m.complexity, m.lines });
        if (m.fan_in > 0) try stdout.print(" fan_in={d}", .{m.fan_in});
        if (m.fan_out > 0) try stdout.print(" fan_out={d}", .{m.fan_out});
        if (m.branches > 0) try stdout.print(" branches={d}", .{m.branches});
        if (m.loops > 0) try stdout.print(" loops={d}", .{m.loops});
        if (m.error_paths > 0) try stdout.print(" error_paths={d}", .{m.error_paths});
        if (m.nesting_depth_max > 0) try stdout.print(" nesting={d}", .{m.nesting_depth_max});
        if (m.structural_hash != 0) try stdout.print(" hash={x:0>16}", .{m.structural_hash});
        try stdout.print("\n", .{});
    }
    switch (n.external) {
        .stdlib => try stdout.print("  external:   stdlib\n", .{}),
        .dependency => try stdout.print("  external:   dependency\n", .{}),
        .none => {},
    }
    try stdout.print("  edges in:   {d}\n", .{nd.in_edge_ids.len});
    try stdout.print("  edges out:  {d}\n", .{nd.out_edge_ids.len});
}

fn printStats(stdout: *std.Io.Writer, stats: query.Stats) !void {
    try stdout.print("--- Node counts ---\n", .{});
    inline for (@typeInfo(NodeKind).@"enum".fields, 0..) |f, i| {
        if (stats.node_counts[i] > 0) try stdout.print("  {s}: {d}\n", .{ f.name, stats.node_counts[i] });
    }
    try stdout.print("\n--- Edge counts ---\n", .{});
    inline for (@typeInfo(EdgeType).@"enum".fields, 0..) |f, i| {
        if (stats.edge_counts[i] > 0) try stdout.print("  {s}: {d}\n", .{ f.name, stats.edge_counts[i] });
    }
    try stdout.print("\nTotal lines: {d}\n", .{stats.total_lines});
}

// -- Command handlers --

fn cmdSearch(allocator: std.mem.Allocator, g: FrozenGraph, flags: ParsedFlags, stdout: *std.Io.Writer) !void {
    const q: ?[]const u8 = if (flags.positional.items.len > 0) flags.positional.items[0] else null;
    const result = try query.search(allocator, g, .{
        .query = q,
        .kind = flags.kind,
        .scope = flags.scope,
        .include_tests = flags.include_tests,
        .external = if (flags.include_external) .include else .exclude,
        .limit = flags.limit,
    });
    defer result.deinit(allocator);

    try stdout.print("Search results: {d} matches (showing {d})\n", .{ result.total_matches, result.nodes.len });
    for (result.nodes) |id| try printNodeSummary(stdout, g, id);
}

fn cmdStats(allocator: std.mem.Allocator, g: FrozenGraph, flags: ParsedFlags, stdout: *std.Io.Writer) !void {
    const stats = try query.computeStats(allocator, g, .{
        .scope = flags.scope,
        .include_tests = flags.include_tests,
        .include_external = flags.include_external,
    });
    try printStats(stdout, stats);
}

fn cmdAncestors(allocator: std.mem.Allocator, g: FrozenGraph, flags: ParsedFlags, stdout: *std.Io.Writer) !void {
    const id = try requirePositionalId(flags.positional.items, 0, stdout, "Usage: ancestors <node_id>") orelse return;
    const ancestors = try query.getAncestors(allocator, g, id);
    defer if (ancestors.len > 0) allocator.free(ancestors);

    try stdout.print("Ancestors of node {d}:\n", .{@intFromEnum(id)});
    for (ancestors) |aid| try printNodeSummary(stdout, g, aid);
    if (ancestors.len == 0) try stdout.print("  (root, no ancestors)\n", .{});
}

fn cmdImpact(allocator: std.mem.Allocator, g: FrozenGraph, flags: ParsedFlags, stdout: *std.Io.Writer) !void {
    const id = try requirePositionalId(flags.positional.items, 0, stdout, "Usage: impact <node_id>") orelse return;
    const result = try query.getImpact(allocator, g, id, .{});
    defer result.deinit(allocator);

    try stdout.print("Impact of node {d}: {d} dependents\n", .{ @intFromEnum(id), result.total_impacted });
    for (result.impacted) |iid| try printNodeSummary(stdout, g, iid);
}

fn cmdPath(allocator: std.mem.Allocator, g: FrozenGraph, flags: ParsedFlags, stdout: *std.Io.Writer) !void {
    const from = try requirePositionalId(flags.positional.items, 0, stdout, "Usage: path <from_id> <to_id>") orelse return;
    const to = try requirePositionalId(flags.positional.items, 1, stdout, "Usage: path <from_id> <to_id>") orelse return;
    const result = try query.findPaths(allocator, g, from, to, .{});
    defer result.deinit(allocator);

    try stdout.print("Paths from {d} to {d}: {d} found\n", .{ @intFromEnum(from), @intFromEnum(to), result.paths.len });
    for (result.paths, 0..) |p, pi| {
        try stdout.print("\n  Path {d} ({d} nodes):\n", .{ pi + 1, p.node_ids.len });
        for (p.node_ids, 0..) |path_nid, ni| {
            try printNodeSummary(stdout, g, path_nid);
            if (ni < p.edge_types.len) {
                try stdout.print("    --({s})->\n", .{@tagName(p.edge_types[ni])});
            }
        }
    }
}

fn cmdEdges(allocator: std.mem.Allocator, g: FrozenGraph, flags: ParsedFlags, stdout: *std.Io.Writer) !void {
    const id = try requirePositionalId(flags.positional.items, 0, stdout, "Usage: edges <node_id>") orelse return;
    const ids = [_]NodeId{id};
    const result = try query.getEdges(allocator, g, &ids, .{
        .include_external = flags.include_external,
        .limit = flags.limit,
    });
    defer result.deinit(allocator);

    try stdout.print("Edges for node {d}: {d} total ({d} shown)\n", .{ @intFromEnum(id), result.total_count, result.edges.len });
    for (result.edges) |e| {
        const src_name = if (g.getNode(e.source_id)) |n| n.name else "?";
        const tgt_name = if (g.getNode(e.target_id)) |n| n.name else "?";
        try stdout.print("  [{d}] \"{s}\" --({s})-> [{d}] \"{s}\"\n", .{
            @intFromEnum(e.source_id), src_name, @tagName(e.edge_type),
            @intFromEnum(e.target_id), tgt_name,
        });
    }
}

fn cmdNode(allocator: std.mem.Allocator, g: FrozenGraph, flags: ParsedFlags, stdout: *std.Io.Writer) !void {
    const id = try requirePositionalId(flags.positional.items, 0, stdout, "Usage: node <node_id>") orelse return;
    const ids = [_]NodeId{id};
    const result = try query.getNodes(allocator, g, &ids, .{});
    defer result.deinit(allocator);

    if (result.nodes.len == 0) {
        try stdout.print("Node {d} not found\n", .{@intFromEnum(id)});
    } else {
        try printNodeDetail(stdout, result.nodes[0]);
    }
}

const QueryCommand = enum {
    search,
    stats,
    ancestors,
    impact,
    path,
    edges,
    node,
};

const help_text =
    \\query-graph - Index a directory and run query engine operations on it.
    \\
    \\USAGE:
    \\    zig build query-graph -- <directory> <command> [OPTIONS]
    \\
    \\ARGUMENTS:
    \\    <directory>              Path to the directory to index
    \\
    \\COMMANDS:
    \\    search [QUERY]           Search nodes by name regex
    \\    stats                    Compute graph statistics
    \\    ancestors <node_id>      Show ancestor chain for a node
    \\    impact <node_id>         Show transitive dependents of a node
    \\    path <from> <to>         Find shortest path between two nodes
    \\    edges <node_id>          Show edges for a node
    \\    node <node_id>           Show full detail for a node
    \\
    \\OPTIONS:
    \\    --kind <kind>            Filter by node kind (function, type_def, etc.)
    \\    --scope <prefix>         Restrict to files matching prefix
    \\    --include-tests          Include test_def nodes
    \\    --include-external       Include external/phantom nodes
    \\    --exclude path1,path2    Exclude paths from indexation
    \\    --without-lsp            Skip LSP enrichment
    \\    --limit N                Max results (default 50)
    \\    -v                       Increase verbosity
    \\    -h, --help               Show this help message
    \\
;

// -- Entry point --

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const allocator = init.gpa;

    var stdout_buffer: [tool_utils.stdout_buffer_size]u8 = undefined;
    var stdout_writer = std.Io.File.stdout().writer(io, &stdout_buffer);
    const stdout = &stdout_writer.interface;

    var args = init.minimal.args.iterate();
    _ = args.next();
    const dir_arg = args.next() orelse {
        try tool_utils.printHelp(stdout, help_text);
        return;
    };
    if (std.mem.eql(u8, dir_arg, "--help") or std.mem.eql(u8, dir_arg, "-h")) {
        try tool_utils.printHelp(stdout, help_text);
        return;
    }

    const command = args.next() orelse {
        try tool_utils.printHelp(stdout, help_text);
        return;
    };
    if (std.mem.eql(u8, command, "--help") or std.mem.eql(u8, command, "-h")) {
        try tool_utils.printHelp(stdout, help_text);
        return;
    }

    // Collect remaining CLI tokens and parse flags.
    var raw_remaining = std.ArrayList([]const u8).empty;
    defer raw_remaining.deinit(allocator);
    while (args.next()) |a| try raw_remaining.append(allocator, a);

    var flags = try parseFlags(allocator, raw_remaining.items);
    defer flags.deinit(allocator);

    // Resolve and index.
    const dir_path = std.Io.Dir.cwd().realPathFileAlloc(io, dir_arg, allocator) catch |err| {
        try stdout.print("Error resolving path '{s}': {}\n", .{ dir_arg, err });
        try stdout.flush();
        std.process.exit(1);
    };
    defer allocator.free(dir_path);

    var text_logger = logging.TextStderrLogger.init(io, logging.verbosityToLevel(flags.common.verbosity));
    const log = if (flags.common.verbosity > 0) text_logger.logger() else logging.Logger.noop;

    var graph = Graph.init(dir_path);
    defer graph.deinit(allocator);

    var wl = zcodeprism.lsp.worklist.LspWorklist{};
    defer wl.deinit(allocator);

    const idx_result = indexer.indexDirectory(allocator, io, dir_path, &graph, &wl, .{
        .exclude_paths = flags.common.exclude.items,
        .logger = log,
    }) catch |err| {
        try stdout.print("Index error: {}\n", .{err});
        try stdout.flush();
        std.process.exit(1);
    };

    if (flags.common.lsp) {
        try tool_utils.runLspEnrichment(allocator, io, &graph, &wl, log, stdout);
    }

    try stdout.print("Indexed {d} files ({d} nodes, {d} edges)\n\n", .{
        idx_result.files_indexed, graph.nodes.items.len, graph.edges.items.len,
    });

    const cmd = std.meta.stringToEnum(QueryCommand, command) orelse {
        try stdout.print("Unknown command: {s}\n\n", .{command});
        try tool_utils.printHelp(stdout, help_text);
        return;
    };

    const fg = FrozenGraph{ .graph = &graph };
    switch (cmd) {
        .search => try cmdSearch(allocator, fg, flags, stdout),
        .stats => try cmdStats(allocator, fg, flags, stdout),
        .ancestors => try cmdAncestors(allocator, fg, flags, stdout),
        .impact => try cmdImpact(allocator, fg, flags, stdout),
        .path => try cmdPath(allocator, fg, flags, stdout),
        .edges => try cmdEdges(allocator, fg, flags, stdout),
        .node => try cmdNode(allocator, fg, flags, stdout),
    }

    try stdout.flush();
}
