//! Standalone tool to render a code graph in CTG or Mermaid format.
//! Usage: zig build render-graph -- <path> [--format ctg|mermaid] [--name <project-name>]
//!        [--exclude path1,path2] [--scope <prefix>] [--depth N]
//!        [--test-nodes] [--external-nodes] [--help]
//!
//! Accepts a directory or a single source file. For a directory, all
//! supported source files are indexed. Renders the code graph in the
//! chosen format and writes to stdout.

const std = @import("std");
const zcodeprism = @import("zcodeprism");
const tool_utils = @import("tool-utils");

const Graph = zcodeprism.Graph;
const indexer = zcodeprism.indexer;
const ctg = zcodeprism.ctg;
const logging = zcodeprism.logging;
const mermaid = zcodeprism.mermaid;
const Registry = zcodeprism.registry.Registry;

const Format = enum { ctg, mermaid_fmt };

fn printHelp(stdout: *std.Io.Writer) !void {
    try stdout.print(
        \\render-graph - Index and render a code graph in CTG or Mermaid format.
        \\
        \\USAGE:
        \\    zig build render-graph -- <path> [OPTIONS]
        \\
        \\ARGUMENTS:
        \\    <path>                   Path to a directory or a single source file
        \\
        \\OPTIONS:
        \\    --format ctg|mermaid     Output format (default: ctg)
        \\    --name <project-name>    Project name (default: path basename)
        \\    --exclude path1,path2    Comma-separated paths to exclude (directory mode only)
        \\    --scope <prefix>         Restrict output to nodes whose file path starts with prefix
        \\    --depth N                Limit output to N levels of nesting below file nodes
        \\    --test-nodes             Include test nodes in output
        \\    --external-nodes         Include external nodes in output
        \\    --without-lsp            Skip LSP enrichment (directory mode only)
        \\    -v                       Increase verbosity (-v info, -vv debug, -vvv trace)
        \\    --verbose                Same as -v
        \\    -h, --help               Show this help message
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

    // Parse CLI arguments.
    var args = init.minimal.args.iterate();
    _ = args.next(); // skip program name

    const input_path_arg = args.next() orelse {
        try printHelp(stdout);
        return;
    };
    if (std.mem.eql(u8, input_path_arg, "--help") or std.mem.eql(u8, input_path_arg, "-h")) {
        try printHelp(stdout);
        return;
    }

    var format: Format = .ctg;
    var project_name: ?[]const u8 = null;
    var include_test_nodes: bool = false;
    var include_external_nodes: bool = false;
    var common_flags = tool_utils.CommonFlags.init();
    defer common_flags.deinit(allocator);
    var scope_arg: ?[]const u8 = null;
    var depth_arg: ?u32 = null;

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--format")) {
            const fmt_str = args.next() orelse {
                try stdout.print("Error: --format requires a value (ctg or mermaid)\n", .{});
                try stdout.flush();
                std.process.exit(1);
            };
            if (std.mem.eql(u8, fmt_str, "ctg")) {
                format = .ctg;
            } else if (std.mem.eql(u8, fmt_str, "mermaid")) {
                format = .mermaid_fmt;
            } else {
                try stdout.print("Error: unknown format '{s}', expected 'ctg' or 'mermaid'\n", .{fmt_str});
                try stdout.flush();
                std.process.exit(1);
            }
        } else if (std.mem.eql(u8, arg, "--name")) {
            project_name = args.next() orelse {
                try stdout.print("Error: --name requires a value\n", .{});
                try stdout.flush();
                std.process.exit(1);
            };
        } else if (std.mem.eql(u8, arg, "--scope")) {
            scope_arg = args.next() orelse {
                try stdout.print("Error: --scope requires a value\n", .{});
                try stdout.flush();
                std.process.exit(1);
            };
        } else if (std.mem.eql(u8, arg, "--depth")) {
            const depth_str = args.next() orelse {
                try stdout.print("Error: --depth requires a numeric value\n", .{});
                try stdout.flush();
                std.process.exit(1);
            };
            depth_arg = std.fmt.parseInt(u32, depth_str, 10) catch {
                try stdout.print("Error: --depth value must be a non-negative integer, got '{s}'\n", .{depth_str});
                try stdout.flush();
                std.process.exit(1);
            };
        } else if (std.mem.eql(u8, arg, "--test-nodes")) {
            include_test_nodes = true;
        } else if (std.mem.eql(u8, arg, "--external-nodes")) {
            include_external_nodes = true;
        } else if (try tool_utils.parseCommonFlag(allocator, arg, &args, &common_flags)) {
            // consumed
        }
    }

    // Resolve to absolute path.
    const input_path = std.Io.Dir.cwd().realPathFileAlloc(io, input_path_arg, allocator) catch |err| {
        try stdout.print("Error resolving path '{s}': {}\n", .{ input_path_arg, err });
        try stdout.flush();
        std.process.exit(1);
    };
    defer allocator.free(input_path);

    var text_logger = logging.TextStderrLogger.init(tool_utils.verbosityToLevel(common_flags.verbosity));
    const log = if (common_flags.verbosity > 0) text_logger.logger() else logging.Logger.noop;

    // Determine whether the input is a directory or a regular file.
    // openDirAbsolute returns error.NotDir when the path points to a file.
    const is_file = blk: {
        var d = std.Io.Dir.openDirAbsolute(io, input_path, .{}) catch |err| switch (err) {
            error.NotDir => break :blk true,
            else => {
                try stdout.print("Error opening path '{s}': {}\n", .{ input_path, err });
                try stdout.flush();
                std.process.exit(1);
            },
        };
        d.close(io);
        break :blk false;
    };

    // --exclude is only meaningful in directory mode.
    if (is_file and common_flags.exclude.items.len > 0) {
        try stdout.print("Error: --exclude is not valid when <path> is a single file\n", .{});
        try stdout.flush();
        std.process.exit(1);
    }

    // Derive project name from the path basename if not provided.
    const name = project_name orelse std.fs.path.basename(input_path);

    var graph = Graph.init(if (is_file)
        std.fs.path.dirname(input_path) orelse input_path
    else
        input_path);
    defer graph.deinit(allocator);

    if (is_file) {
        // Single-file path: look up the language, parse, build edges.
        const ext = std.fs.path.extension(input_path);
        const lang_support = Registry.getByExtension(ext) orelse {
            try stdout.print("Unsupported file extension: '{s}'\n", .{ext});
            try stdout.flush();
            std.process.exit(1);
        };

        const source = blk: {
            const f = std.Io.Dir.openFileAbsolute(io, input_path, .{}) catch |err| {
                try stdout.print("Error reading file '{s}': {}\n", .{ input_path, err });
                try stdout.flush();
                std.process.exit(1);
            };
            defer f.close(io);
            var read_buf: [4096]u8 = undefined;
            var f_reader = f.reader(io, &read_buf);
            const content = f_reader.interface.allocRemaining(allocator, .limited(indexer.max_source_bytes)) catch |err| {
                if (err == error.StreamTooLong) {
                    try stdout.print("Error reading file '{s}': file exceeds 10 MiB read limit\n", .{input_path});
                } else {
                    try stdout.print("Error reading file '{s}': {}\n", .{ input_path, err });
                }
                try stdout.flush();
                std.process.exit(1);
            };
            try graph.addOwnedBuffer(allocator, content);
            break :blk content;
        };

        lang_support.parseFn(allocator, io, source, &graph, null, log) catch |err| {
            try stdout.print("Parse error: {}\n", .{err});
            try stdout.flush();
            std.process.exit(1);
        };

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

        _ = try graph.freeze(allocator);
    } else {
        // Directory path: full multi-file indexation.
        var wl = zcodeprism.lsp.worklist.LspWorklist{};
        defer wl.deinit(allocator);

        _ = indexer.indexDirectory(allocator, io, input_path, &graph, &wl, .{
            .exclude_paths = common_flags.exclude.items,
            .logger = log,
        }) catch |err| {
            try stdout.print("Index error: {}\n", .{err});
            try stdout.flush();
            std.process.exit(1);
        };

        if (common_flags.lsp) {
            try tool_utils.runLspEnrichment(allocator, io, &graph, &wl, log, stdout);
        }
    }

    // Render.
    var out: std.ArrayList(u8) = .empty;
    defer out.deinit(allocator);

    const render_common = zcodeprism.render_common;
    const filter = render_common.FilterOptions{
        .include_test_nodes = include_test_nodes,
        .include_external_nodes = include_external_nodes,
        .depth = depth_arg,
    };

    const fg = zcodeprism.FrozenGraph{ .graph = &graph };
    switch (format) {
        .ctg => {
            ctg.renderCtg(allocator, io, fg, .{
                .project_name = name,
                .scope = scope_arg,
                .filter = filter,
            }, &out) catch |err| {
                try stdout.print("Render error: {}\n", .{err});
                try stdout.flush();
                std.process.exit(1);
            };
        },
        .mermaid_fmt => {
            mermaid.renderMermaid(allocator, io, fg, .{
                .project_name = name,
                .scope = scope_arg,
                .filter = filter,
            }, &out) catch |err| {
                try stdout.print("Render error: {}\n", .{err});
                try stdout.flush();
                std.process.exit(1);
            };
        },
    }

    // Write output to stdout.
    try std.Io.File.stdout().writeStreamingAll(io, out.items);
}
