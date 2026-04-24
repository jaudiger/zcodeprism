const std = @import("std");
const zcodeprism = @import("zcodeprism");

const config = zcodeprism.config;
const ctg = zcodeprism.ctg;
const generation_mod = zcodeprism.generation;
const indexer = zcodeprism.indexer;
const mcp = zcodeprism.mcp;
const mermaid = zcodeprism.mermaid;
const render_common = zcodeprism.render_common;
const snapshot = zcodeprism.storage.snapshot;
const snapshot_diff = zcodeprism.diff.snapshot_diff;
const lang_support = zcodeprism.language_support;
const lsp_enricher = zcodeprism.lsp.enricher;
const registry = zcodeprism.registry;
const storage = zcodeprism.storage;
const types = zcodeprism.types;
const workspace_mod = zcodeprism.workspace;
const FrozenGraph = zcodeprism.FrozenGraph;
const Graph = zcodeprism.Graph;
const NodeKind = zcodeprism.NodeKind;
const EdgeType = zcodeprism.EdgeType;
const logging = zcodeprism.logging;
const EnrichResult = lang_support.EnrichResult;

const gen_manager_mod = zcodeprism.watcher.generation_manager;
const watcher_mod = zcodeprism.watcher.watcher;
const debouncer_mod = zcodeprism.watcher.debouncer;
const GenerationManager = gen_manager_mod.GenerationManager;
const FileWatcher = watcher_mod.FileWatcher;
const Debouncer = debouncer_mod.Debouncer;
const GraphGeneration = generation_mod.GraphGeneration;

const version_string = "zcodeprism 0.1.0\n";

const usage_text =
    \\Usage: zcodeprism <command> [options]
    \\
    \\Commands:
    \\  init       Initialize a new project (.zcodeprism.zon + .zcodeprism/)
    \\  index      Index the codebase and build the code graph
    \\  export     Export the graph (--ctg, --mermaid, or --jsonl)
    \\  snapshot   Save a named snapshot of the current graph
    \\  diff       Semantic diff between two snapshots
    \\  serve      Start the MCP server (JSON-RPC over stdio)
    \\  status     Show project status and graph statistics
    \\
    \\Options:
    \\  --version            Print version and exit
    \\  --help               Show this help message
    \\  --force              Force overwrite (with init)
    \\  --name TAG           Snapshot tag name (with snapshot)
    \\  --snapshot TAG       Load a snapshot instead of current graph (with export)
    \\  --project-root PATH  Set the project root directory
    \\  --workspace PATH     Workspace config file (with serve, status)
    \\  -v                   Increase verbosity (up to -vvv)
    \\
;

var stdin_fd: std.posix.fd_t = 0;

fn handleSigterm(_: std.c.SIG) callconv(.c) void {
    // Close stdin to unblock the read loop (read retries on EINTR).
    _ = std.c.close(stdin_fd);
}

const ExportFormat = enum { ctg_fmt, mermaid_fmt, jsonl_fmt };

const Command = enum {
    init,
    index,
    @"export",
    snapshot,
    diff,
    serve,
    status,
    help,
    version,
};

const CliArgs = struct {
    command: ?Command = null,
    force: bool = false,
    verbosity: u8 = 0,
    project_root: ?[]const u8 = null,
    export_format: ?ExportFormat = null,
    scope: ?[]const u8 = null,
    output: ?[]const u8 = null,
    name: ?[]const u8 = null,
    snapshot: ?[]const u8 = null,
    workspace: ?[]const u8 = null,
    include_test_nodes: bool = false,
    include_external_nodes: bool = false,
    positional_args: [2]?[]const u8 = .{ null, null },
    positional_count: usize = 0,
};

fn requireArg(args: *std.process.Args.Iterator, flag: []const u8, stderr: *std.Io.Writer) []const u8 {
    return args.next() orelse {
        stderr.print("{s} requires an argument\n", .{flag}) catch {};
        stderr.flush() catch {};
        std.process.exit(2);
    };
}

fn parseArgs(args_src: std.process.Args, stderr: *std.Io.Writer) CliArgs {
    var cli = CliArgs{};
    var args = args_src.iterate();
    _ = args.next();

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--project-root")) {
            cli.project_root = requireArg(&args, "--project-root", stderr);
        } else if (std.mem.eql(u8, arg, "--version")) {
            cli.command = .version;
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            cli.command = .help;
        } else if (std.mem.eql(u8, arg, "--force")) {
            cli.force = true;
        } else if (std.mem.eql(u8, arg, "--name")) {
            cli.name = requireArg(&args, "--name", stderr);
        } else if (std.mem.eql(u8, arg, "--workspace")) {
            cli.workspace = requireArg(&args, "--workspace", stderr);
        } else if (std.mem.eql(u8, arg, "--snapshot")) {
            cli.snapshot = requireArg(&args, "--snapshot", stderr);
        } else if (std.mem.eql(u8, arg, "--ctg")) {
            cli.export_format = .ctg_fmt;
        } else if (std.mem.eql(u8, arg, "--mermaid")) {
            cli.export_format = .mermaid_fmt;
        } else if (std.mem.eql(u8, arg, "--jsonl")) {
            cli.export_format = .jsonl_fmt;
        } else if (std.mem.eql(u8, arg, "--scope")) {
            cli.scope = requireArg(&args, "--scope", stderr);
        } else if (std.mem.eql(u8, arg, "--output")) {
            cli.output = requireArg(&args, "--output", stderr);
        } else if (std.mem.eql(u8, arg, "--test-nodes")) {
            cli.include_test_nodes = true;
        } else if (std.mem.eql(u8, arg, "--external-nodes")) {
            cli.include_external_nodes = true;
        } else if (std.mem.startsWith(u8, arg, "-v")) {
            var count: u8 = 0;
            for (arg[1..]) |c| {
                if (c == 'v') count += 1 else break;
            }
            cli.verbosity = @max(cli.verbosity, count);
        } else if (arg[0] == '-') {
            stderr.print("unknown option: {s}\n", .{arg}) catch {};
            stderr.flush() catch {};
            std.process.exit(2);
        } else {
            if (cli.command == null) {
                cli.command = std.meta.stringToEnum(Command, arg) orelse {
                    stderr.print("unknown command: {s}\n", .{arg}) catch {};
                    stderr.flush() catch {};
                    std.process.exit(2);
                };
            } else if (cli.positional_count < 2) {
                cli.positional_args[cli.positional_count] = arg;
                cli.positional_count += 1;
            }
        }
    }

    return cli;
}

pub fn main(init: std.process.Init) void {
    const io = init.io;

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.Io.File.stdout().writer(io, &stdout_buffer);
    const stdout = &stdout_writer.interface;

    var stderr_buffer: [4096]u8 = undefined;
    var stderr_writer = std.Io.File.stderr().writer(io, &stderr_buffer);
    const stderr = &stderr_writer.interface;

    const cli = parseArgs(init.minimal.args, stderr);

    if (cli.project_root) |root| {
        std.process.setCurrentPath(io, root) catch {
            stderr.print("cannot chdir to: {s}\n", .{root}) catch {};
            stderr.flush() catch {};
            std.process.exit(1);
        };
    }

    const cmd = cli.command orelse {
        stdout.writeAll(usage_text) catch {};
        stdout.flush() catch {};
        return;
    };

    switch (cmd) {
        .version => {
            stdout.writeAll(version_string) catch {};
            stdout.flush() catch {};
        },
        .help => {
            stdout.writeAll(usage_text) catch {};
            stdout.flush() catch {};
        },
        .init => runInit(io, stdout, stderr, cli.force, cli.workspace),
        .index => runIndex(init.gpa, io, stdout, stderr, cli.verbosity),
        .@"export" => runExport(init.gpa, io, stdout, stderr, cli.export_format, cli.scope, cli.output, cli.snapshot, cli.include_test_nodes, cli.include_external_nodes),
        .snapshot => runSnapshot(init.gpa, io, stdout, stderr, cli.name),
        .diff => runDiff(init.gpa, io, stdout, stderr, cli.positional_args[0], cli.positional_args[1]),
        .serve => runServe(init.gpa, io, stderr, cli.workspace, cli.verbosity),
        .status => runStatus(init.gpa, io, stdout, stderr, cli.workspace),
    }
}

fn runInit(io: std.Io, stdout: *std.Io.Writer, stderr: *std.Io.Writer, force: bool, workspace_arg: ?[]const u8) void {
    const cwd = std.Io.Dir.cwd();

    if (workspace_arg != null) {
        if (force) {
            cwd.deleteFile(io, "zcodeprism-workspace.zon") catch {};
        }
        config.writeDefaultWorkspaceConfig(io, cwd) catch |err| {
            switch (err) {
                error.PathAlreadyExists => {
                    stderr.writeAll("workspace already initialized (use --force to reinitialize)\n") catch {};
                },
                else => {
                    stderr.print("init failed: {s}\n", .{@errorName(err)}) catch {};
                },
            }
            stderr.flush() catch {};
            std.process.exit(1);
        };
        stdout.writeAll("initialized zcodeprism workspace\n") catch {};
        stdout.flush() catch {};
        return;
    }

    if (force) {
        cwd.deleteFile(io, ".zcodeprism.zon") catch {};
        cwd.deleteTree(io, ".zcodeprism") catch {};
    }

    config.writeDefaultConfig(io, cwd) catch |err| {
        switch (err) {
            error.PathAlreadyExists => {
                stderr.writeAll("already initialized (use --force to reinitialize)\n") catch {};
            },
            else => {
                stderr.print("init failed: {s}\n", .{@errorName(err)}) catch {};
            },
        }
        stderr.flush() catch {};
        std.process.exit(1);
    };

    config.createDataDir(io, cwd) catch |err| {
        stderr.print("failed to create data directory: {s}\n", .{@errorName(err)}) catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };

    stdout.writeAll("initialized zcodeprism project\n") catch {};
    stdout.flush() catch {};
}

fn runIndex(allocator: std.mem.Allocator, io: std.Io, stdout: *std.Io.Writer, stderr: *std.Io.Writer, verbosity: u8) void {
    const project_root = std.Io.Dir.cwd().realPathFileAlloc(io, ".", allocator) catch |err| {
        stderr.print("failed to resolve project root: {s}\n", .{@errorName(err)}) catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };
    defer allocator.free(project_root);

    // Load config, fall back to all-defaults if no config file.
    const cfg = loadConfig(allocator, io) catch |err| {
        stderr.print("failed to load config: {s}\n", .{@errorName(err)}) catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };
    defer config.deinit(cfg, allocator);
    const full = config.withDefaults(cfg);

    // Set up logger based on verbosity.
    const log_level: logging.Level = switch (verbosity) {
        0 => .warn,
        1 => .info,
        2 => .debug,
        else => .trace,
    };
    var text_logger = logging.TextStderrLogger.init(log_level);
    const logger = text_logger.logger();

    var graph = Graph.init(project_root);
    defer graph.deinit(allocator);

    var wl = zcodeprism.lsp.worklist.LspWorklist{};
    defer wl.deinit(allocator);

    const idx_result = indexer.indexDirectory(allocator, io, project_root, &graph, &wl, .{
        .exclude_paths = full.exclude_paths orelse config.defaultExcludePaths(),
        .logger = logger,
        .budget_bytes = if (full.memory) |m| if (m.budget_mb) |mb| @as(u64, mb) * 1024 * 1024 else null else null,
    }) catch |err| {
        stderr.print("indexing failed: {s}\n", .{@errorName(err)}) catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };

    // LSP enrichment pass.
    var lsp_pool = zcodeprism.lsp.pool.LspPool.init(.{});
    defer lsp_pool.deinit(allocator, io);

    var lsp_result = EnrichResult{};
    for (registry.Registry.allLanguages()) |ls| {
        const result = lsp_enricher.enrich(allocator, io, &graph, ls, &wl, &lsp_pool, .{
            .logger = logger,
            .project_root = project_root,
        }) catch |err| {
            stderr.print("LSP enrichment failed: {s}\n", .{@errorName(err)}) catch {};
            stderr.flush() catch {};
            std.process.exit(1);
        };
        lsp_result.accumulate(result);
    }

    const frozen = FrozenGraph{ .graph = &graph };
    const fmt = if (full.storage) |s| s.format orelse .binary else .binary;
    switch (fmt) {
        .binary => {
            storage.binary.save(allocator, io, frozen, ".zcodeprism/graph.bin") catch |err| {
                stderr.print("failed to save binary graph: {s}\n", .{@errorName(err)}) catch {};
                stderr.flush() catch {};
                std.process.exit(1);
            };
        },
        .jsonl => {
            saveJsonl(allocator, io, frozen) catch |err| {
                stderr.print("failed to save JSONL graph: {s}\n", .{@errorName(err)}) catch {};
                stderr.flush() catch {};
                std.process.exit(1);
            };
        },
    }

    stdout.print("indexed {d} files ({d} nodes, {d} edges)\n", .{
        idx_result.files_indexed,
        graph.nodeCount(),
        graph.edgeCount(),
    }) catch {};
    printEnrichSummary(stdout, lsp_result);
    stdout.flush() catch {};
}

fn runExport(
    allocator: std.mem.Allocator,
    io: std.Io,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
    format_arg: ?ExportFormat,
    scope_arg: ?[]const u8,
    output_arg: ?[]const u8,
    snapshot_arg: ?[]const u8,
    include_test_nodes: bool,
    include_external_nodes: bool,
) void {
    const format = format_arg orelse {
        stderr.writeAll("export requires a format flag: --ctg, --mermaid, or --jsonl\n") catch {};
        stderr.flush() catch {};
        std.process.exit(2);
    };

    var graph = if (snapshot_arg) |tag|
        snapshot.loadSnapshotGraph(allocator, io, tag, ".zcodeprism") catch |err| {
            switch (err) {
                error.SnapshotNotFound => stderr.print("snapshot not found: {s}\n", .{tag}) catch {},
                error.InvalidTagName => stderr.print("invalid snapshot tag: {s}\n", .{tag}) catch {},
                else => stderr.print("failed to load snapshot: {s}\n", .{@errorName(err)}) catch {},
            }
            stderr.flush() catch {};
            std.process.exit(1);
        }
    else
        storage.binary.load(allocator, io, ".zcodeprism/graph.bin") catch {
            stderr.writeAll("failed to load graph (run 'index' first)\n") catch {};
            stderr.flush() catch {};
            std.process.exit(1);
        };
    defer graph.deinit(allocator);

    const project_name = blk: {
        const base = std.fs.path.basename(graph.project_root);
        if (base.len > 0) break :blk base;
        var cwd_buf: [std.fs.max_path_bytes]u8 = undefined;
        const n = std.process.currentPath(io, &cwd_buf) catch break :blk "project";
        break :blk std.fs.path.basename(cwd_buf[0..n]);
    };

    const export_frozen = FrozenGraph{ .graph = &graph };
    switch (format) {
        .ctg_fmt => {
            var out: std.ArrayList(u8) = .empty;
            defer out.deinit(allocator);

            ctg.renderCtg(allocator, io, export_frozen, .{
                .project_name = project_name,
                .scope = scope_arg,
                .filter = .{
                    .include_test_nodes = include_test_nodes,
                    .include_external_nodes = include_external_nodes,
                },
            }, &out) catch |err| {
                stderr.print("render failed: {s}\n", .{@errorName(err)}) catch {};
                stderr.flush() catch {};
                std.process.exit(1);
            };

            writeOutput(io, stdout, stderr, output_arg, out.items);
        },
        .mermaid_fmt => {
            var out: std.ArrayList(u8) = .empty;
            defer out.deinit(allocator);

            mermaid.renderMermaid(allocator, io, export_frozen, .{
                .project_name = project_name,
                .scope = scope_arg,
                .filter = .{
                    .include_test_nodes = include_test_nodes,
                    .include_external_nodes = include_external_nodes,
                },
            }, &out) catch |err| {
                stderr.print("render failed: {s}\n", .{@errorName(err)}) catch {};
                stderr.flush() catch {};
                std.process.exit(1);
            };

            writeOutput(io, stdout, stderr, output_arg, out.items);
        },
        .jsonl_fmt => {
            if (output_arg) |path| {
                var af = std.Io.Dir.cwd().createFileAtomic(io, path, .{ .replace = true }) catch |err| {
                    stderr.print("cannot create output file: {s}\n", .{@errorName(err)}) catch {};
                    stderr.flush() catch {};
                    std.process.exit(1);
                };
                defer af.deinit(io);
                var write_buf: [8192]u8 = undefined;
                var af_writer = af.file.writer(io, &write_buf);
                const export_fg = FrozenGraph{ .graph = &graph };
                storage.jsonl.exportJsonl(allocator, export_fg, &af_writer.interface) catch |err| {
                    stderr.print("export failed: {s}\n", .{@errorName(err)}) catch {};
                    stderr.flush() catch {};
                    std.process.exit(1);
                };
                af_writer.interface.flush() catch {};
                af.file.sync(io) catch {};
                af.replace(io) catch |err| {
                    stderr.print("rename failed: {s}\n", .{@errorName(err)}) catch {};
                    stderr.flush() catch {};
                    std.process.exit(1);
                };
            } else {
                var buf: [8192]u8 = undefined;
                var writer = std.Io.File.stdout().writer(io, &buf);
                const stdout_fg = FrozenGraph{ .graph = &graph };
                storage.jsonl.exportJsonl(allocator, stdout_fg, &writer.interface) catch |err| {
                    stderr.print("export failed: {s}\n", .{@errorName(err)}) catch {};
                    stderr.flush() catch {};
                    std.process.exit(1);
                };
                writer.interface.flush() catch {};
            }
        },
    }
}

fn writeOutput(io: std.Io, stdout: *std.Io.Writer, stderr: *std.Io.Writer, output_arg: ?[]const u8, data: []const u8) void {
    if (output_arg) |path| {
        var af = std.Io.Dir.cwd().createFileAtomic(io, path, .{ .replace = true }) catch |err| {
            stderr.print("cannot create output file: {s}\n", .{@errorName(err)}) catch {};
            stderr.flush() catch {};
            std.process.exit(1);
        };
        defer af.deinit(io);
        af.file.writeStreamingAll(io, data) catch |err| {
            stderr.print("write failed: {s}\n", .{@errorName(err)}) catch {};
            stderr.flush() catch {};
            std.process.exit(1);
        };
        af.file.sync(io) catch {};
        af.replace(io) catch |err| {
            stderr.print("rename failed: {s}\n", .{@errorName(err)}) catch {};
            stderr.flush() catch {};
            std.process.exit(1);
        };
    } else {
        stdout.writeAll(data) catch {};
        stdout.flush() catch {};
    }
}

fn runSnapshot(allocator: std.mem.Allocator, io: std.Io, stdout: *std.Io.Writer, stderr: *std.Io.Writer, name_arg: ?[]const u8) void {
    const tag = name_arg orelse {
        stderr.writeAll("snapshot requires --name <tag>\n") catch {};
        stderr.flush() catch {};
        std.process.exit(2);
    };

    var graph = storage.binary.load(allocator, io, ".zcodeprism/graph.bin") catch {
        stderr.writeAll("failed to load graph (run 'index' first)\n") catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };
    defer graph.deinit(allocator);

    const snap_fg = FrozenGraph{ .graph = &graph };
    snapshot.saveSnapshot(allocator, io, snap_fg, tag, ".zcodeprism") catch |err| {
        switch (err) {
            error.InvalidTagName => stderr.print("invalid snapshot tag: {s}\n", .{tag}) catch {},
            error.TagTooLong => stderr.print("snapshot tag too long (max {d}): {s}\n", .{ snapshot.MAX_TAG_LENGTH, tag }) catch {},
            else => stderr.print("failed to save snapshot: {s}\n", .{@errorName(err)}) catch {},
        }
        stderr.flush() catch {};
        std.process.exit(1);
    };

    stdout.print("snapshot saved: {s}\n", .{tag}) catch {};
    stdout.flush() catch {};
}

fn runDiff(allocator: std.mem.Allocator, io: std.Io, stdout: *std.Io.Writer, stderr: *std.Io.Writer, tag_a_arg: ?[]const u8, tag_b_arg: ?[]const u8) void {
    const tag_a = tag_a_arg orelse {
        stderr.writeAll("diff requires two snapshot tags: zcodeprism diff <snap-a> <snap-b>\n") catch {};
        stderr.flush() catch {};
        std.process.exit(2);
    };
    const tag_b = tag_b_arg orelse {
        stderr.writeAll("diff requires two snapshot tags: zcodeprism diff <snap-a> <snap-b>\n") catch {};
        stderr.flush() catch {};
        std.process.exit(2);
    };

    var graph_a = snapshot.loadSnapshotGraph(allocator, io, tag_a, ".zcodeprism") catch |err| {
        switch (err) {
            error.SnapshotNotFound => stderr.print("snapshot not found: {s}\n", .{tag_a}) catch {},
            error.InvalidTagName => stderr.print("invalid snapshot tag: {s}\n", .{tag_a}) catch {},
            else => stderr.print("failed to load snapshot: {s}\n", .{@errorName(err)}) catch {},
        }
        stderr.flush() catch {};
        std.process.exit(1);
    };
    defer graph_a.deinit(allocator);

    var graph_b = snapshot.loadSnapshotGraph(allocator, io, tag_b, ".zcodeprism") catch |err| {
        switch (err) {
            error.SnapshotNotFound => stderr.print("snapshot not found: {s}\n", .{tag_b}) catch {},
            error.InvalidTagName => stderr.print("invalid snapshot tag: {s}\n", .{tag_b}) catch {},
            else => stderr.print("failed to load snapshot: {s}\n", .{@errorName(err)}) catch {},
        }
        stderr.flush() catch {};
        std.process.exit(1);
    };
    defer graph_b.deinit(allocator);

    const diff_fg_a = FrozenGraph{ .graph = &graph_a };
    const diff_fg_b = FrozenGraph{ .graph = &graph_b };
    var report = snapshot_diff.diffGraphs(allocator, diff_fg_a, diff_fg_b) catch |err| {
        stderr.print("diff failed: {s}\n", .{@errorName(err)}) catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };
    defer report.deinit(allocator);

    var out: std.ArrayList(u8) = .empty;
    defer out.deinit(allocator);
    snapshot_diff.renderDiffReport(allocator, &report, &out) catch |err| {
        stderr.print("render failed: {s}\n", .{@errorName(err)}) catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };

    stdout.writeAll(out.items) catch {};
    stdout.flush() catch {};
}

fn runServe(allocator: std.mem.Allocator, io: std.Io, stderr: *std.Io.Writer, workspace_arg: ?[]const u8, verbosity: u8) void {
    stdin_fd = std.Io.File.stdin().handle;
    std.posix.sigaction(std.posix.SIG.TERM, &.{
        .handler = .{ .handler = handleSigterm },
        .mask = std.posix.sigemptyset(),
        .flags = 0,
    }, null);

    const log_level: logging.Level = switch (verbosity) {
        0 => .warn,
        1 => .info,
        2 => .debug,
        else => .trace,
    };
    var text_logger = logging.TextStderrLogger.init(log_level);
    const logger = text_logger.logger();

    // Load config for exclude paths.
    const cfg = loadConfig(allocator, io) catch |err| {
        stderr.print("failed to load config: {s}\n", .{@errorName(err)}) catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };
    defer config.deinit(cfg, allocator);
    const full = config.withDefaults(cfg);
    const exclude_paths = full.exclude_paths orelse config.defaultExcludePaths();

    const project_root = std.Io.Dir.cwd().realPathFileAlloc(io, ".", allocator) catch |err| {
        stderr.print("failed to resolve project root: {s}\n", .{@errorName(err)}) catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };
    defer allocator.free(project_root);

    // Heap-allocate the initial generation so old/new can coexist.
    const initial_gen = GraphGeneration.create(allocator, io, 1, .{0} ** types.hash_len) catch {
        stderr.writeAll("out of memory\n") catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };

    // Initial index.
    var wl = zcodeprism.lsp.worklist.LspWorklist{};
    defer wl.deinit(allocator);

    if (workspace_arg) |ws_path| {
        initial_gen.graph = loadWorkspaceGraph(initial_gen.arena.allocator(), io, ws_path, stderr);
    } else {
        _ = indexer.indexDirectory(allocator, io, project_root, &initial_gen.graph, &wl, .{
            .exclude_paths = exclude_paths,
            .logger = logger,
            .budget_bytes = if (full.memory) |m| if (m.budget_mb) |mb| @as(u64, mb) * 1024 * 1024 else null else null,
        }) catch |err| {
            stderr.print("initial indexing failed: {s}\n", .{@errorName(err)}) catch {};
            stderr.flush() catch {};
            initial_gen.destroy(allocator);
            std.process.exit(1);
        };
    }

    // LSP pool shared across re-indexes.
    var lsp_pool = zcodeprism.lsp.pool.LspPool.init(.{});
    defer lsp_pool.deinit(allocator, io);

    if (workspace_arg == null) {
        for (registry.Registry.allLanguages()) |ls| {
            _ = lsp_enricher.enrich(allocator, io, &initial_gen.graph, ls, &wl, &lsp_pool, .{
                .logger = logger,
                .project_root = project_root,
            }) catch {};
        }
    }

    computeSourceHash(initial_gen);

    const initial_guard = initial_gen.acquire();
    defer initial_guard.deinit();

    var gen_manager = GenerationManager.init(initial_gen);

    var server = mcp.server.Server.init(&gen_manager);
    defer server.deinit();

    // Stdout mutex shared between main read loop and watcher thread.
    var stdout_mutex: std.Io.Mutex = .init;

    // Determine watch root: for workspace, watch the workspace directory.
    const watch_root = if (workspace_arg) |ws_path|
        std.Io.Dir.cwd().realPathFileAlloc(io, std.fs.path.dirname(ws_path) orelse ".", allocator) catch project_root
    else
        project_root;
    defer if (workspace_arg != null) allocator.free(watch_root);

    const budget_bytes: ?u64 = if (full.memory) |m| if (m.budget_mb) |mb| @as(u64, mb) * 1024 * 1024 else null else null;

    // Spawn watcher thread.
    const watcher_thread = std.Thread.spawn(.{}, watcherThreadFn, .{
        allocator,
        io,
        &gen_manager,
        &lsp_pool,
        project_root,
        exclude_paths,
        &stdout_mutex,
        logger,
        workspace_arg,
        stderr,
        watch_root,
        budget_bytes,
    }) catch {
        stderr.writeAll("failed to spawn watcher thread\n") catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };
    defer watcher_thread.join();

    serveStdioLoop(allocator, io, &server, &stdout_mutex);
}

fn watcherThreadFn(
    allocator: std.mem.Allocator,
    io: std.Io,
    gen_manager: *GenerationManager,
    lsp_pool: *zcodeprism.lsp.pool.LspPool,
    project_root: []const u8,
    exclude_paths: []const []const u8,
    stdout_mutex: *std.Io.Mutex,
    logger: logging.Logger,
    workspace_arg: ?[]const u8,
    stderr: *std.Io.Writer,
    watch_root: []const u8,
    budget_bytes: ?u64,
) void {
    var file_watcher = FileWatcher.init(allocator, io, watch_root, exclude_paths) catch return;
    defer file_watcher.deinit(allocator);

    var debouncer = Debouncer.init(500);
    var generation_id: u64 = 1;

    while (true) {
        if (!file_watcher.waitForEvents()) break;
        debouncer.trigger(io);

        // Drain additional events during debounce window.
        while (!debouncer.isReady(io)) {
            std.Io.sleep(io, .fromNanoseconds(@intCast(@as(i96, debouncer.remainingMs(io)) * std.time.ns_per_ms)), .real) catch break;
            if (debouncer.isReady(io)) break;
        }

        generation_id += 1;
        const new_gen = GraphGeneration.create(allocator, io, generation_id, .{0} ** types.hash_len) catch continue;

        if (workspace_arg) |ws_path| {
            new_gen.graph = loadWorkspaceGraph(new_gen.arena.allocator(), io, ws_path, stderr);
        } else {
            var wl = zcodeprism.lsp.worklist.LspWorklist{};
            defer wl.deinit(allocator);

            _ = indexer.indexDirectory(allocator, io, project_root, &new_gen.graph, &wl, .{
                .exclude_paths = exclude_paths,
                .logger = logger,
                .budget_bytes = budget_bytes,
            }) catch {
                new_gen.destroy(allocator);
                continue;
            };

            for (registry.Registry.allLanguages()) |ls| {
                _ = lsp_enricher.enrich(allocator, io, &new_gen.graph, ls, &wl, lsp_pool, .{
                    .logger = logger,
                    .project_root = project_root,
                }) catch {};
            }
        }

        computeSourceHash(new_gen);

        // Acquire a guard on new_gen to keep it alive during notification.
        const new_guard = new_gen.acquire();

        const old_gen = gen_manager.swap(io, new_gen);
        if (old_gen.ref_count.load(.monotonic) == 0) {
            old_gen.destroy(allocator);
        }

        // Send notification under stdout mutex.
        const notification = mcp.server.Server.buildNotification(
            allocator,
            "graph/updated",
            new_gen.generation_id,
            new_gen.source_hash,
        ) catch {
            new_guard.deinit();
            continue;
        };
        defer allocator.free(notification);

        {
            stdout_mutex.lockUncancelable(io);
            defer stdout_mutex.unlock(io);
            var stdout_buffer: [4096]u8 = undefined;
            var stdout_writer = std.Io.File.stdout().writer(io, &stdout_buffer);
            const stdout = &stdout_writer.interface;
            stdout.writeAll(notification) catch {};
            stdout.writeAll("\n") catch {};
            stdout.flush() catch {};
        }

        new_guard.deinit();
    }
}

fn serveStdioLoop(allocator: std.mem.Allocator, io: std.Io, server: *mcp.server.Server, stdout_mutex: *std.Io.Mutex) void {
    var stdin_buffer: [4096]u8 = undefined;
    var stdin_reader = std.Io.File.stdin().readerStreaming(io, &stdin_buffer);
    const reader = &stdin_reader.interface;

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.Io.File.stdout().writer(io, &stdout_buffer);
    const stdout = &stdout_writer.interface;

    var line_buf: std.ArrayList(u8) = .empty;
    defer line_buf.deinit(allocator);

    while (true) {
        const line = readLine(reader, &line_buf, allocator) orelse break;
        if (line.len == 0) continue;

        const response = server.handleMessage(allocator, io, line) catch continue;
        if (response) |resp| {
            defer allocator.free(resp);
            stdout_mutex.lockUncancelable(io);
            defer stdout_mutex.unlock(io);
            stdout.writeAll(resp) catch break;
            stdout.writeAll("\n") catch break;
            stdout.flush() catch break;
        }
    }
}

fn computeSourceHash(gen: *GraphGeneration) void {
    var hasher = std.crypto.hash.Blake3.init(.{});
    for (gen.graph.nodes.items) |n| {
        if (n.kind == .file) {
            if (n.content_hash) |h| {
                hasher.update(&h);
            }
        }
    }
    hasher.final(&gen.source_hash);
}

fn readLine(reader: *std.Io.Reader, line_buf: *std.ArrayList(u8), allocator: std.mem.Allocator) ?[]const u8 {
    line_buf.clearRetainingCapacity();
    while (true) {
        const available = reader.peekGreedy(1) catch |err| switch (err) {
            error.EndOfStream => {
                if (line_buf.items.len > 0) return line_buf.items;
                return null;
            },
            error.ReadFailed => return null,
        };
        if (std.mem.indexOfScalar(u8, available, '\n')) |pos| {
            line_buf.appendSlice(allocator, available[0..pos]) catch return null;
            reader.toss(pos + 1);
            return line_buf.items;
        }
        line_buf.appendSlice(allocator, available) catch return null;
        reader.toss(available.len);
    }
}

fn saveJsonl(allocator: std.mem.Allocator, io: std.Io, fg: FrozenGraph) !void {
    const file = try std.Io.Dir.cwd().createFile(io, ".zcodeprism/graph.jsonl", .{});
    defer file.close(io);
    var buf: [8192]u8 = undefined;
    var writer = file.writer(io, &buf);
    try storage.jsonl.exportJsonl(allocator, fg, &writer.interface);
    try writer.interface.flush();
}

/// Build a unified graph from a workspace config file.
/// Exits on any error (config, validation, missing project graphs).
fn loadWorkspaceGraph(allocator: std.mem.Allocator, io: std.Io, ws_path: []const u8, stderr: *std.Io.Writer) Graph {
    const ws_dir = std.fs.path.dirname(ws_path) orelse ".";

    const file = std.Io.Dir.cwd().openFile(io, ws_path, .{}) catch {
        stderr.print("cannot open workspace config: {s}\n", .{ws_path}) catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };
    defer file.close(io);

    var read_buf: [4096]u8 = undefined;
    var fr = file.reader(io, &read_buf);
    const content = fr.interface.allocRemaining(allocator, .limited(1024 * 1024)) catch {
        stderr.writeAll("failed to read workspace config\n") catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };
    defer allocator.free(content);
    const content_z = allocator.dupeZ(u8, content) catch {
        stderr.writeAll("out of memory\n") catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };
    defer allocator.free(content_z);

    const ws = workspace_mod.parseWorkspaceConfig(allocator, content_z, ws_dir) catch |err| {
        stderr.print("invalid workspace config: {s}\n", .{@errorName(err)}) catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };
    defer workspace_mod.freeWorkspace(allocator, &ws);

    workspace_mod.validateWorkspace(io, &ws, ws_dir) catch |err| {
        stderr.print("workspace validation failed: {s}\n", .{@errorName(err)}) catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };

    var project_graphs = allocator.alloc(Graph, ws.projects.len) catch {
        stderr.writeAll("out of memory\n") catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };
    defer allocator.free(project_graphs);

    for (ws.projects, 0..) |proj, i| {
        var path_buf: [std.fs.max_path_bytes]u8 = undefined;
        const graph_path = std.fmt.bufPrint(&path_buf, "{s}/{s}/.zcodeprism/graph.bin", .{ ws_dir, proj.path }) catch {
            stderr.print("path too long for project: {s}\n", .{proj.name}) catch {};
            stderr.flush() catch {};
            std.process.exit(1);
        };
        project_graphs[i] = storage.binary.load(allocator, io, graph_path) catch {
            stderr.print("failed to load graph for project '{s}' (run 'index' first)\n", .{proj.name}) catch {};
            stderr.flush() catch {};
            std.process.exit(1);
        };
    }

    const assembled = workspace_mod.assembleWorkspace(allocator, &ws, project_graphs) catch {
        stderr.writeAll("failed to assemble workspace graph\n") catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };

    const graph = assembled.graph;
    allocator.free(assembled.project_ranges);
    return graph;
}

fn loadConfig(allocator: std.mem.Allocator, io: std.Io) !config.Config {
    const file = std.Io.Dir.cwd().openFile(io, ".zcodeprism.zon", .{}) catch |err| {
        if (err == error.FileNotFound) return config.Config{};
        return err;
    };
    defer file.close(io);

    var read_buf: [4096]u8 = undefined;
    var fr = file.reader(io, &read_buf);
    const content = fr.interface.allocRemaining(allocator, .limited(1024 * 1024)) catch |err| {
        return err;
    };
    defer allocator.free(content);
    const content_z = try allocator.dupeZ(u8, content);
    defer allocator.free(content_z);

    return config.parseFromSlice(allocator, content_z);
}

fn runStatus(allocator: std.mem.Allocator, io: std.Io, stdout: *std.Io.Writer, stderr: *std.Io.Writer, workspace_arg: ?[]const u8) void {
    var graph = if (workspace_arg) |ws_path|
        loadWorkspaceGraph(allocator, io, ws_path, stderr)
    else
        storage.binary.load(allocator, io, ".zcodeprism/graph.bin") catch {
            stderr.writeAll("not initialized or not indexed\n") catch {};
            stderr.flush() catch {};
            std.process.exit(1);
        };
    defer graph.deinit(allocator);

    // Count nodes by kind.
    var file_count: usize = 0;
    var function_count: usize = 0;
    var type_count: usize = 0;
    var other_count: usize = 0;
    for (graph.nodes.items) |n| {
        switch (n.kind) {
            .file => file_count += 1,
            .function => function_count += 1,
            .type_def, .enum_def, .union_def => type_count += 1,
            else => other_count += 1,
        }
    }

    var hasher = std.crypto.hash.Blake3.init(.{});
    for (graph.nodes.items) |n| {
        if (n.kind == .file) {
            if (n.content_hash) |h| {
                hasher.update(&h);
            }
        }
    }
    var source_hash: types.ContentHash = undefined;
    hasher.final(&source_hash);
    const source_hex = types.formatHash(source_hash);

    stdout.print(
        "nodes: {d} ({d} files, {d} functions, {d} types)\n" ++
            "edges: {d}\n" ++
            "source_hash: {s}\n",
        .{
            graph.nodeCount(),
            file_count,
            function_count,
            type_count,
            graph.edgeCount(),
            &source_hex,
        },
    ) catch {};
    stdout.flush() catch {};
}

fn printEnrichSummary(stdout: *std.Io.Writer, result: EnrichResult) void {
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
