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
const workspace_mod = zcodeprism.workspace;
const FrozenGraph = zcodeprism.FrozenGraph;
const Graph = zcodeprism.Graph;
const NodeKind = zcodeprism.NodeKind;
const EdgeType = zcodeprism.EdgeType;
const logging = zcodeprism.logging;
const EnrichResult = lang_support.EnrichResult;

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
    \\  --full               Full re-index (with index, default)
    \\  --json               Output in JSON format
    \\  --name TAG           Snapshot tag name (with snapshot)
    \\  --snapshot TAG       Load a snapshot instead of current graph (with export)
    \\  --project-root PATH  Set the project root directory
    \\  --workspace PATH     Workspace config file (with serve, status)
    \\  -v                   Increase verbosity (up to -vvv)
    \\
;

var stdin_fd: std.posix.fd_t = 0;

fn handleSigterm(_: c_int) callconv(.c) void {
    // Close stdin to unblock the read loop (read retries on EINTR).
    std.posix.close(stdin_fd);
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

fn requireArg(args: *std.process.ArgIterator, flag: []const u8, stderr: *std.Io.Writer) []const u8 {
    return args.next() orelse {
        stderr.print("{s} requires an argument\n", .{flag}) catch {};
        stderr.flush() catch {};
        std.process.exit(2);
    };
}

fn parseArgs(stderr: *std.Io.Writer) CliArgs {
    var cli = CliArgs{};
    var args = std.process.args();
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
        } else if (std.mem.eql(u8, arg, "--full") or std.mem.eql(u8, arg, "--incremental") or std.mem.eql(u8, arg, "--json")) {
            // Accepted but not yet used beyond index.
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

pub fn main() void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    var stderr_buffer: [4096]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
    const stderr = &stderr_writer.interface;

    const cli = parseArgs(stderr);

    if (cli.project_root) |root| {
        std.posix.chdir(root) catch {
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
        .init => runInit(stdout, stderr, cli.force, cli.workspace),
        .index => runIndex(stdout, stderr, cli.verbosity),
        .@"export" => runExport(stdout, stderr, cli.export_format, cli.scope, cli.output, cli.snapshot, cli.include_test_nodes, cli.include_external_nodes),
        .snapshot => runSnapshot(stdout, stderr, cli.name),
        .diff => runDiff(stdout, stderr, cli.positional_args[0], cli.positional_args[1]),
        .serve => runServe(stderr, cli.workspace),
        .status => runStatus(stdout, stderr, cli.workspace),
    }
}

fn runInit(stdout: *std.Io.Writer, stderr: *std.Io.Writer, force: bool, workspace_arg: ?[]const u8) void {
    const cwd = std.fs.cwd();

    if (workspace_arg != null) {
        if (force) {
            cwd.deleteFile("zcodeprism-workspace.zon") catch {};
        }
        config.writeDefaultWorkspaceConfig(cwd) catch |err| {
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
        cwd.deleteFile(".zcodeprism.zon") catch {};
        cwd.deleteTree(".zcodeprism") catch {};
    }

    config.writeDefaultConfig(cwd) catch |err| {
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

    config.createDataDir(cwd) catch |err| {
        stderr.print("failed to create data directory: {s}\n", .{@errorName(err)}) catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };

    stdout.writeAll("initialized zcodeprism project\n") catch {};
    stdout.flush() catch {};
}

fn runIndex(stdout: *std.Io.Writer, stderr: *std.Io.Writer, verbosity: u8) void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const project_root = std.fs.cwd().realpathAlloc(allocator, ".") catch |err| {
        stderr.print("failed to resolve project root: {s}\n", .{@errorName(err)}) catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };
    defer allocator.free(project_root);

    // Load config, fall back to all-defaults if no config file.
    const cfg = loadConfig(allocator) catch |err| {
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

    const idx_result = indexer.indexDirectory(allocator, project_root, &graph, &wl, .{
        .exclude_paths = full.exclude_paths orelse config.defaultExcludePaths(),
        .logger = logger,
    }) catch |err| {
        stderr.print("indexing failed: {s}\n", .{@errorName(err)}) catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };

    // LSP enrichment pass.
    var lsp_result = EnrichResult{};
    for (registry.Registry.allLanguages()) |ls| {
        const result = lsp_enricher.enrich(allocator, &graph, ls, &wl, .{
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
            storage.binary.save(allocator, frozen, ".zcodeprism/graph.bin") catch |err| {
                stderr.print("failed to save binary graph: {s}\n", .{@errorName(err)}) catch {};
                stderr.flush() catch {};
                std.process.exit(1);
            };
        },
        .jsonl => {
            saveJsonl(allocator, frozen) catch |err| {
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

    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var graph = if (snapshot_arg) |tag|
        snapshot.loadSnapshotGraph(allocator, tag, ".zcodeprism") catch |err| {
            switch (err) {
                error.SnapshotNotFound => stderr.print("snapshot not found: {s}\n", .{tag}) catch {},
                error.InvalidTagName => stderr.print("invalid snapshot tag: {s}\n", .{tag}) catch {},
                else => stderr.print("failed to load snapshot: {s}\n", .{@errorName(err)}) catch {},
            }
            stderr.flush() catch {};
            std.process.exit(1);
        }
    else
        storage.binary.load(allocator, ".zcodeprism/graph.bin") catch {
            stderr.writeAll("failed to load graph (run 'index' first)\n") catch {};
            stderr.flush() catch {};
            std.process.exit(1);
        };
    defer graph.deinit(allocator);

    const project_name = blk: {
        const base = std.fs.path.basename(graph.project_root);
        if (base.len > 0) break :blk base;
        var cwd_buf: [std.fs.max_path_bytes]u8 = undefined;
        const cwd = std.posix.getcwd(&cwd_buf) catch break :blk "project";
        break :blk std.fs.path.basename(cwd);
    };

    const export_frozen = FrozenGraph{ .graph = &graph };
    switch (format) {
        .ctg_fmt => {
            var out: std.ArrayList(u8) = .{};
            defer out.deinit(allocator);

            ctg.renderCtg(allocator, export_frozen, .{
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

            writeOutput(stdout, stderr, output_arg, out.items);
        },
        .mermaid_fmt => {
            var out: std.ArrayList(u8) = .{};
            defer out.deinit(allocator);

            mermaid.renderMermaid(allocator, export_frozen, .{
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

            writeOutput(stdout, stderr, output_arg, out.items);
        },
        .jsonl_fmt => {
            if (output_arg) |path| {
                var write_buf: [8192]u8 = undefined;
                var af = std.fs.cwd().atomicFile(path, .{ .write_buffer = &write_buf }) catch |err| {
                    stderr.print("cannot create output file: {s}\n", .{@errorName(err)}) catch {};
                    stderr.flush() catch {};
                    std.process.exit(1);
                };
                defer af.deinit();
                const export_fg = FrozenGraph{ .graph = &graph };
                storage.jsonl.exportJsonl(allocator, export_fg, &af.file_writer.interface) catch |err| {
                    stderr.print("export failed: {s}\n", .{@errorName(err)}) catch {};
                    stderr.flush() catch {};
                    std.process.exit(1);
                };
                atomicFinishWithSync(&af, stderr);
            } else {
                var buf: [8192]u8 = undefined;
                var writer = std.fs.File.stdout().writer(&buf);
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

fn writeOutput(stdout: *std.Io.Writer, stderr: *std.Io.Writer, output_arg: ?[]const u8, data: []const u8) void {
    if (output_arg) |path| {
        var write_buf: [8192]u8 = undefined;
        var af = std.fs.cwd().atomicFile(path, .{ .write_buffer = &write_buf }) catch |err| {
            stderr.print("cannot create output file: {s}\n", .{@errorName(err)}) catch {};
            stderr.flush() catch {};
            std.process.exit(1);
        };
        defer af.deinit();
        af.file_writer.interface.writeAll(data) catch |err| {
            stderr.print("write failed: {s}\n", .{@errorName(err)}) catch {};
            stderr.flush() catch {};
            std.process.exit(1);
        };
        atomicFinishWithSync(&af, stderr);
    } else {
        stdout.writeAll(data) catch {};
        stdout.flush() catch {};
    }
}

fn atomicFinishWithSync(af: *std.fs.AtomicFile, stderr: *std.Io.Writer) void {
    af.flush() catch |err| {
        stderr.print("write failed: {s}\n", .{@errorName(err)}) catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };
    af.file_writer.file.sync() catch |err| {
        stderr.print("sync failed: {s}\n", .{@errorName(err)}) catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };
    af.renameIntoPlace() catch |err| {
        stderr.print("rename failed: {s}\n", .{@errorName(err)}) catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };
}

fn runSnapshot(stdout: *std.Io.Writer, stderr: *std.Io.Writer, name_arg: ?[]const u8) void {
    const tag = name_arg orelse {
        stderr.writeAll("snapshot requires --name <tag>\n") catch {};
        stderr.flush() catch {};
        std.process.exit(2);
    };

    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var graph = storage.binary.load(allocator, ".zcodeprism/graph.bin") catch {
        stderr.writeAll("failed to load graph (run 'index' first)\n") catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };
    defer graph.deinit(allocator);

    const snap_fg = FrozenGraph{ .graph = &graph };
    snapshot.saveSnapshot(allocator, snap_fg, tag, ".zcodeprism") catch |err| {
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

fn runDiff(stdout: *std.Io.Writer, stderr: *std.Io.Writer, tag_a_arg: ?[]const u8, tag_b_arg: ?[]const u8) void {
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

    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var graph_a = snapshot.loadSnapshotGraph(allocator, tag_a, ".zcodeprism") catch |err| {
        switch (err) {
            error.SnapshotNotFound => stderr.print("snapshot not found: {s}\n", .{tag_a}) catch {},
            error.InvalidTagName => stderr.print("invalid snapshot tag: {s}\n", .{tag_a}) catch {},
            else => stderr.print("failed to load snapshot: {s}\n", .{@errorName(err)}) catch {},
        }
        stderr.flush() catch {};
        std.process.exit(1);
    };
    defer graph_a.deinit(allocator);

    var graph_b = snapshot.loadSnapshotGraph(allocator, tag_b, ".zcodeprism") catch |err| {
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

    var out: std.ArrayList(u8) = .{};
    defer out.deinit(allocator);
    snapshot_diff.renderDiffReport(allocator, &report, &out) catch |err| {
        stderr.print("render failed: {s}\n", .{@errorName(err)}) catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };

    stdout.writeAll(out.items) catch {};
    stdout.flush() catch {};
}

fn runServe(stderr: *std.Io.Writer, workspace_arg: ?[]const u8) void {
    stdin_fd = std.fs.File.stdin().handle;
    std.posix.sigaction(std.posix.SIG.TERM, &.{
        .handler = .{ .handler = handleSigterm },
        .mask = std.posix.sigemptyset(),
        .flags = 0,
    }, null);

    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var gen = generation_mod.GraphGeneration.init(allocator, 1, .{0} ** 12);

    if (workspace_arg) |ws_path| {
        gen.graph = loadWorkspaceGraph(gen.arena.allocator(), ws_path, stderr);
    } else {
        gen.graph = storage.binary.load(gen.arena.allocator(), ".zcodeprism/graph.bin") catch {
            stderr.writeAll("failed to load graph (run 'index' first)\n") catch {};
            stderr.flush() catch {};
            std.process.exit(1);
        };
    }

    // Compute source hash from file content hashes.
    var hasher = std.hash.XxHash3.init(0);
    for (gen.graph.nodes.items) |n| {
        if (n.kind == .file) {
            if (n.content_hash) |h| {
                hasher.update(&h);
            }
        }
    }
    const hash_u64 = hasher.final();
    @memcpy(gen.source_hash[0..8], std.mem.asBytes(&hash_u64));

    const guard = gen.acquire();
    defer guard.deinit();

    var server = mcp.server.Server.init(&gen);
    defer server.deinit();

    var stdin_buffer: [4096]u8 = undefined;
    var stdin_reader = std.fs.File.stdin().readerStreaming(&stdin_buffer);
    const reader = &stdin_reader.interface;

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    var line_buf: std.ArrayList(u8) = .{};
    defer line_buf.deinit(allocator);

    while (true) {
        const line = readLine(reader, &line_buf, allocator) orelse break;
        if (line.len == 0) continue;

        const response = server.handleMessage(allocator, line) catch continue;
        if (response) |resp| {
            defer allocator.free(resp);
            stdout.writeAll(resp) catch break;
            stdout.writeAll("\n") catch break;
            stdout.flush() catch break;
        }
    }
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

fn saveJsonl(allocator: std.mem.Allocator, fg: FrozenGraph) !void {
    const file = try std.fs.cwd().createFile(".zcodeprism/graph.jsonl", .{});
    defer file.close();
    var buf: [8192]u8 = undefined;
    var writer = file.writer(&buf);
    try storage.jsonl.exportJsonl(allocator, fg, &writer.interface);
    try writer.interface.flush();
}

/// Build a unified graph from a workspace config file.
/// Exits on any error (config, validation, missing project graphs).
fn loadWorkspaceGraph(allocator: std.mem.Allocator, ws_path: []const u8, stderr: *std.Io.Writer) Graph {
    const ws_dir = std.fs.path.dirname(ws_path) orelse ".";

    const file = std.fs.cwd().openFile(ws_path, .{}) catch {
        stderr.print("cannot open workspace config: {s}\n", .{ws_path}) catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };
    defer file.close();

    const content = file.readToEndAllocOptions(allocator, 1024 * 1024, null, .of(u8), 0) catch {
        stderr.writeAll("failed to read workspace config\n") catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };
    defer allocator.free(content);

    const ws = workspace_mod.parseWorkspaceConfig(allocator, content, ws_dir) catch |err| {
        stderr.print("invalid workspace config: {s}\n", .{@errorName(err)}) catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };
    defer workspace_mod.freeWorkspace(allocator, &ws);

    workspace_mod.validateWorkspace(&ws, ws_dir) catch |err| {
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
        project_graphs[i] = storage.binary.load(allocator, graph_path) catch {
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

fn loadConfig(allocator: std.mem.Allocator) !config.Config {
    const file = std.fs.cwd().openFile(".zcodeprism.zon", .{}) catch |err| {
        if (err == error.FileNotFound) return config.Config{};
        return err;
    };
    defer file.close();

    const content = file.readToEndAllocOptions(allocator, 1024 * 1024, null, .of(u8), 0) catch |err| {
        return err;
    };
    defer allocator.free(content);

    return config.parseFromSlice(allocator, content);
}

fn runStatus(stdout: *std.Io.Writer, stderr: *std.Io.Writer, workspace_arg: ?[]const u8) void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var graph = if (workspace_arg) |ws_path|
        loadWorkspaceGraph(allocator, ws_path, stderr)
    else
        storage.binary.load(allocator, ".zcodeprism/graph.bin") catch {
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

    // Compute source_hash from file nodes' content_hashes.
    var hasher = std.hash.XxHash3.init(0);
    for (graph.nodes.items) |n| {
        if (n.kind == .file) {
            if (n.content_hash) |h| {
                hasher.update(&h);
            }
        }
    }
    const source_hash = hasher.final();

    stdout.print(
        "nodes: {d} ({d} files, {d} functions, {d} types)\n" ++
            "edges: {d}\n" ++
            "source_hash: {x:0>16}\n",
        .{
            graph.nodeCount(),
            file_count,
            function_count,
            type_count,
            graph.edgeCount(),
            source_hash,
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
