//! Standalone tool to run analysis algorithms on an indexed directory.
//! Usage: zig build analyze-graph -- <directory> <command> [OPTIONS]

const std = @import("std");
const zcodeprism = @import("zcodeprism");
const tool_utils = @import("tool-utils");

const Graph = zcodeprism.Graph;
const NodeId = zcodeprism.types.NodeId;
const NodeKind = zcodeprism.NodeKind;
const indexer = zcodeprism.indexer;
const logging = zcodeprism.logging;
const analyzer = zcodeprism.analyzer;

const ParsedFlags = struct {
    common: tool_utils.CommonFlags,
    scope: ?[]const u8 = null,
    include_public: bool = false,
    limit: u32 = 20,
    min_lines: u32 = 3,
    max_depth: u32 = 10,
    max_cycle_length: u32 = 20,
    min_coupling: f64 = 1.0,
    positional: std.ArrayList([]const u8),

    fn deinit(self: *ParsedFlags, allocator: std.mem.Allocator) void {
        self.common.deinit(allocator);
        self.positional.deinit(allocator);
    }
};

fn parseFlags(allocator: std.mem.Allocator, raw_args: []const []const u8) !ParsedFlags {
    var flags = ParsedFlags{ .common = tool_utils.CommonFlags.init(), .positional = .{} };
    errdefer flags.deinit(allocator);
    var iter = tool_utils.SliceIter.init(raw_args);
    while (iter.next()) |a| {
        if (try tool_utils.parseCommonFlag(allocator, a, &iter, &flags.common)) continue;
        if (std.mem.eql(u8, a, "--scope")) {
            if (iter.next()) |v| flags.scope = v;
        } else if (std.mem.eql(u8, a, "--include-public")) {
            flags.include_public = true;
        } else if (std.mem.eql(u8, a, "--limit")) {
            if (iter.next()) |v| flags.limit = std.fmt.parseInt(u32, v, 10) catch 20;
        } else if (std.mem.eql(u8, a, "--min-lines")) {
            if (iter.next()) |v| flags.min_lines = std.fmt.parseInt(u32, v, 10) catch 3;
        } else if (std.mem.eql(u8, a, "--max-depth")) {
            if (iter.next()) |v| flags.max_depth = std.fmt.parseInt(u32, v, 10) catch 10;
        } else if (std.mem.eql(u8, a, "--max-cycle-length")) {
            if (iter.next()) |v| flags.max_cycle_length = std.fmt.parseInt(u32, v, 10) catch 20;
        } else if (std.mem.eql(u8, a, "--min-coupling")) {
            if (iter.next()) |v| flags.min_coupling = std.fmt.parseFloat(f64, v) catch 1.0;
        } else {
            try flags.positional.append(allocator, a);
        }
    }
    return flags;
}

fn parseNodeId(s: []const u8) ?NodeId {
    return zcodeprism.types.parseNodeId(s, 10);
}

// -- Command handlers --

fn cmdComplexity(allocator: std.mem.Allocator, g: *const Graph, flags: ParsedFlags, stdout: *std.Io.Writer) !void {
    const result = try analyzer.complexity.findComplex(allocator, g, .{
        .top_n = flags.limit,
        .scope = flags.scope,
    });
    defer result.deinit(allocator);

    try stdout.print("Top {d} most complex functions:\n\n", .{result.nodes.len});
    for (result.nodes, 0..) |entry, i| {
        try stdout.print("  {d}. [{d}] \"{s}\"  C={d}", .{
            i + 1,
            @intFromEnum(entry.node_id),
            entry.name,
            entry.complexity,
        });
        if (entry.file_path) |fp| try stdout.print("  file=\"{s}\"", .{fp});
        try stdout.print("\n", .{});
    }
    if (result.nodes.len == 0) try stdout.print("  (none)\n", .{});
}

fn cmdDeadCode(allocator: std.mem.Allocator, g: *const Graph, flags: ParsedFlags, stdout: *std.Io.Writer) !void {
    const result = try analyzer.dead_code.findDeadCode(allocator, g, .{
        .include_public = flags.include_public,
        .scope = flags.scope,
        .limit = flags.limit,
    });
    defer result.deinit(allocator);

    try stdout.print("Dead code ({d} symbols):\n\n", .{result.total_count});
    for (result.nodes) |entry| {
        try stdout.print("  [{d}] {s} {s} \"{s}\"", .{
            @intFromEnum(entry.node_id),
            @tagName(entry.visibility),
            @tagName(entry.kind),
            entry.name,
        });
        if (entry.file_path) |fp| try stdout.print("  file=\"{s}\"", .{fp});
        try stdout.print("\n", .{});
    }
    if (result.nodes.len == 0) try stdout.print("  (none)\n", .{});
}

fn cmdDuplicates(allocator: std.mem.Allocator, g: *const Graph, flags: ParsedFlags, stdout: *std.Io.Writer) !void {
    const result = try analyzer.duplicates.findDuplicates(allocator, g, .{
        .min_lines = flags.min_lines,
        .scope = flags.scope,
        .limit = flags.limit,
    });
    defer result.deinit(allocator);

    try stdout.print("Duplicate function groups ({d}):\n", .{result.total_groups});
    for (result.groups, 0..) |group, gi| {
        try stdout.print("\n  Group {d} (hash={x:0>16}, {d} members):\n", .{
            gi + 1,
            group.structural_hash,
            group.members.len,
        });
        for (group.members) |member| {
            try stdout.print("    [{d}] \"{s}\"", .{ @intFromEnum(member.node_id), member.name });
            if (member.file_path) |fp| try stdout.print("  file=\"{s}\"", .{fp});
            try stdout.print("\n", .{});
        }
    }
    if (result.groups.len == 0) try stdout.print("  (none)\n", .{});
}

fn cmdCycles(allocator: std.mem.Allocator, g: *const Graph, flags: ParsedFlags, stdout: *std.Io.Writer) !void {
    const result = try analyzer.cycles.findCycles(allocator, g, .{
        .max_cycle_length = flags.max_cycle_length,
        .scope = flags.scope,
    });
    defer result.deinit(allocator);

    try stdout.print("Dependency cycles ({d}):\n", .{result.cycles.len});
    for (result.cycles, 0..) |cycle, ci| {
        try stdout.print("\n  Cycle {d} ({d} files):\n", .{ ci + 1, cycle.nodes.len });
        for (cycle.nodes) |cn| {
            try stdout.print("    [{d}] \"{s}\"", .{ @intFromEnum(cn.node_id), cn.name });
            if (cn.file_path) |fp| try stdout.print("  file=\"{s}\"", .{fp});
            try stdout.print("\n", .{});
        }
    }
    if (result.cycles.len == 0) try stdout.print("  (none)\n", .{});
}

fn cmdCoupling(allocator: std.mem.Allocator, g: *const Graph, flags: ParsedFlags, stdout: *std.Io.Writer) !void {
    const result = try analyzer.coupling.findCoupling(allocator, g, .{
        .min_coupling = flags.min_coupling,
        .top_n = flags.limit,
        .scope = flags.scope,
    });
    defer result.deinit(allocator);

    try stdout.print("Coupled file pairs ({d}):\n\n", .{result.pairs.len});
    for (result.pairs, 0..) |pair, i| {
        try stdout.print("  {d}. \"{s}\" <-> \"{s}\"  edges={d} score={d:.1}\n", .{
            i + 1,
            pair.module_a,
            pair.module_b,
            pair.shared_edges,
            pair.score,
        });
    }
    if (result.pairs.len == 0) try stdout.print("  (none)\n", .{});
}

fn cmdImpact(allocator: std.mem.Allocator, g: *const Graph, flags: ParsedFlags, stdout: *std.Io.Writer) !void {
    if (flags.positional.items.len == 0) {
        try stdout.print("Usage: impact <node_id> [<node_id> ...]\n", .{});
        try stdout.flush();
        return;
    }

    var ids = std.ArrayList(NodeId){};
    defer ids.deinit(allocator);
    for (flags.positional.items) |arg| {
        const nid = parseNodeId(arg) orelse {
            try stdout.print("Invalid node ID: {s}\n", .{arg});
            try stdout.flush();
            return;
        };
        try ids.append(allocator, nid);
    }

    const result = try analyzer.impact.analyzeImpact(allocator, g, ids.items, .{
        .max_depth = flags.max_depth,
    });
    defer result.deinit(allocator);

    try stdout.print("Impact analysis: {d} dependents\n\n", .{result.total_impacted});
    for (result.dependents) |dep| {
        try stdout.print("  [{d}] {s} \"{s}\"", .{
            @intFromEnum(dep.node_id),
            @tagName(dep.kind),
            dep.name,
        });
        if (dep.file_path) |fp| try stdout.print("  file=\"{s}\"", .{fp});
        try stdout.print("\n", .{});
    }
    if (result.total_impacted == 0) try stdout.print("  (none)\n", .{});
}

const AnalyzeCommand = enum {
    complexity,
    @"dead-code",
    duplicates,
    cycles,
    coupling,
    impact,
};

// -- Help --

fn printHelp(stdout: *std.Io.Writer) !void {
    try stdout.print(
        \\analyze-graph - Index a directory and run analysis algorithms on it.
        \\
        \\USAGE:
        \\    zig build analyze-graph -- <directory> <command> [OPTIONS]
        \\
        \\COMMANDS:
        \\    complexity               Top N most complex functions
        \\    dead-code                Symbols with zero non-test references
        \\    duplicates               Groups of structurally identical functions
        \\    cycles                   Circular import dependencies among files
        \\    coupling                 File pairs ranked by shared edge count
        \\    impact <node_id> [...]   Transitive reverse dependents of given nodes
        \\
        \\OPTIONS:
        \\    --scope <prefix>         Restrict to files matching prefix
        \\    --include-public         Include public symbols in dead-code results
        \\    --exclude path1,path2    Exclude paths from indexation
        \\    --without-lsp            Skip LSP enrichment
        \\    --limit N                Max results (default 20)
        \\    --min-lines N            Min function lines for duplicates (default 3)
        \\    --max-depth N            Max BFS depth for impact (default 10)
        \\    --max-cycle-length N     Max cycle size (default 20)
        \\    --min-coupling N         Min coupling score (default 1.0)
        \\    -v                       Increase verbosity
        \\    -h, --help               Show this help message
        \\
    , .{});
    try stdout.flush();
}

// -- Entry point --

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var stdout_buffer: [tool_utils.stdout_buffer_size]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    var args = std.process.args();
    _ = args.next();
    const dir_arg = args.next() orelse {
        try printHelp(stdout);
        return;
    };
    if (std.mem.eql(u8, dir_arg, "--help") or std.mem.eql(u8, dir_arg, "-h")) {
        try printHelp(stdout);
        return;
    }

    const command = args.next() orelse {
        try printHelp(stdout);
        return;
    };
    if (std.mem.eql(u8, command, "--help") or std.mem.eql(u8, command, "-h")) {
        try printHelp(stdout);
        return;
    }

    var raw_remaining = std.ArrayList([]const u8){};
    defer raw_remaining.deinit(allocator);
    while (args.next()) |a| try raw_remaining.append(allocator, a);

    var flags = try parseFlags(allocator, raw_remaining.items);
    defer flags.deinit(allocator);

    const dir_path = std.fs.cwd().realpathAlloc(allocator, dir_arg) catch |err| {
        try stdout.print("Error resolving path '{s}': {}\n", .{ dir_arg, err });
        try stdout.flush();
        std.process.exit(1);
    };
    defer allocator.free(dir_path);

    var text_logger = logging.TextStderrLogger.init(tool_utils.verbosityToLevel(flags.common.verbosity));
    const log = if (flags.common.verbosity > 0) text_logger.logger() else logging.Logger.noop;

    var graph = Graph.init(dir_path);
    defer graph.deinit(allocator);

    var wl = zcodeprism.lsp.worklist.LspWorklist{};
    defer wl.deinit(allocator);

    const idx_result = indexer.indexDirectory(allocator, dir_path, &graph, &wl, .{
        .exclude_paths = flags.common.exclude.items,
        .logger = log,
    }) catch |err| {
        try stdout.print("Index error: {}\n", .{err});
        try stdout.flush();
        std.process.exit(1);
    };

    if (flags.common.lsp) {
        try tool_utils.runLspEnrichment(allocator, &graph, &wl, log, stdout);
    }

    try stdout.print("Indexed {d} files ({d} nodes, {d} edges)\n\n", .{
        idx_result.files_indexed, graph.nodes.items.len, graph.edges.items.len,
    });

    const cmd = std.meta.stringToEnum(AnalyzeCommand, command) orelse {
        try stdout.print("Unknown command: {s}\n\n", .{command});
        try printHelp(stdout);
        return;
    };

    switch (cmd) {
        .complexity => try cmdComplexity(allocator, &graph, flags, stdout),
        .@"dead-code" => try cmdDeadCode(allocator, &graph, flags, stdout),
        .duplicates => try cmdDuplicates(allocator, &graph, flags, stdout),
        .cycles => try cmdCycles(allocator, &graph, flags, stdout),
        .coupling => try cmdCoupling(allocator, &graph, flags, stdout),
        .impact => try cmdImpact(allocator, &graph, flags, stdout),
    }

    try stdout.flush();
}
