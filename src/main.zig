const std = @import("std");
const zcodeprism = @import("zcodeprism");

const commands = zcodeprism.commands;
const config = zcodeprism.config;
const logging = zcodeprism.logging;
const snapshot = zcodeprism.storage.snapshot;
const types = zcodeprism.types;

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

const ExportFormat = enum { ctg, mermaid, jsonl };

const InitArgs = struct {
    force: bool = false,
    workspace: ?[]const u8 = null,
};

const ExportArgs = struct {
    format: ExportFormat,
    scope: ?[]const u8 = null,
    output: ?[]const u8 = null,
    snapshot: ?[]const u8 = null,
    include_test_nodes: bool = false,
    include_external_nodes: bool = false,
};

const SnapshotArgs = struct {
    name: []const u8,
};

const DiffArgs = struct {
    tag_a: []const u8,
    tag_b: []const u8,
};

const ServeArgs = struct {
    workspace: ?[]const u8 = null,
};

const StatusArgs = struct {
    workspace: ?[]const u8 = null,
};

const Command = union(enum) {
    init: InitArgs,
    index,
    @"export": ExportArgs,
    snapshot: SnapshotArgs,
    diff: DiffArgs,
    serve: ServeArgs,
    status: StatusArgs,
    help,
    version,
};

const CliArgs = struct {
    project_root: ?[]const u8 = null,
    verbosity: u8 = 0,
    command: Command,
};

const ParseCtx = struct {
    project_root: ?[]const u8 = null,
    verbosity: u8 = 0,
    stderr: *std.Io.Writer,
};

fn requireArg(args: *std.process.Args.Iterator, flag: []const u8, stderr: *std.Io.Writer) []const u8 {
    return args.next() orelse {
        stderr.print("{s} requires an argument\n", .{flag}) catch {};
        stderr.flush() catch {};
        std.process.exit(2);
    };
}

fn parseGlobalFlag(arg: []const u8, args: *std.process.Args.Iterator, ctx: *ParseCtx) bool {
    if (std.mem.eql(u8, arg, "--project-root")) {
        ctx.project_root = requireArg(args, "--project-root", ctx.stderr);
        return true;
    }
    if (std.mem.startsWith(u8, arg, "-v")) {
        var count: u8 = 0;
        for (arg[1..]) |c| {
            if (c == 'v') count += 1 else break;
        }
        ctx.verbosity = @max(ctx.verbosity, count);
        return true;
    }
    return false;
}

fn parseInit(args: *std.process.Args.Iterator, ctx: *ParseCtx) InitArgs {
    var force = false;
    var workspace: ?[]const u8 = null;
    while (args.next()) |arg| {
        if (parseGlobalFlag(arg, args, ctx)) continue;
        if (std.mem.eql(u8, arg, "--force")) {
            force = true;
        } else if (std.mem.eql(u8, arg, "--workspace")) {
            workspace = requireArg(args, "--workspace", ctx.stderr);
        } else {
            ctx.stderr.print("unknown option for init: {s}\n", .{arg}) catch {};
            ctx.stderr.flush() catch {};
            std.process.exit(2);
        }
    }
    return .{ .force = force, .workspace = workspace };
}

fn parseIndex(args: *std.process.Args.Iterator, ctx: *ParseCtx) void {
    while (args.next()) |arg| {
        if (parseGlobalFlag(arg, args, ctx)) continue;
        ctx.stderr.print("unknown option for index: {s}\n", .{arg}) catch {};
        ctx.stderr.flush() catch {};
        std.process.exit(2);
    }
}

fn parseExport(args: *std.process.Args.Iterator, ctx: *ParseCtx) ExportArgs {
    var format: ?ExportFormat = null;
    var scope: ?[]const u8 = null;
    var output: ?[]const u8 = null;
    var snap: ?[]const u8 = null;
    var include_test_nodes = false;
    var include_external_nodes = false;
    while (args.next()) |arg| {
        if (parseGlobalFlag(arg, args, ctx)) continue;
        if (std.mem.eql(u8, arg, "--ctg")) {
            format = .ctg;
        } else if (std.mem.eql(u8, arg, "--mermaid")) {
            format = .mermaid;
        } else if (std.mem.eql(u8, arg, "--jsonl")) {
            format = .jsonl;
        } else if (std.mem.eql(u8, arg, "--scope")) {
            scope = requireArg(args, "--scope", ctx.stderr);
        } else if (std.mem.eql(u8, arg, "--output")) {
            output = requireArg(args, "--output", ctx.stderr);
        } else if (std.mem.eql(u8, arg, "--snapshot")) {
            snap = requireArg(args, "--snapshot", ctx.stderr);
        } else if (std.mem.eql(u8, arg, "--test-nodes")) {
            include_test_nodes = true;
        } else if (std.mem.eql(u8, arg, "--external-nodes")) {
            include_external_nodes = true;
        } else {
            ctx.stderr.print("unknown option for export: {s}\n", .{arg}) catch {};
            ctx.stderr.flush() catch {};
            std.process.exit(2);
        }
    }
    const fmt = format orelse {
        ctx.stderr.writeAll("export requires a format flag: --ctg, --mermaid, or --jsonl\n") catch {};
        ctx.stderr.flush() catch {};
        std.process.exit(2);
    };
    return .{
        .format = fmt,
        .scope = scope,
        .output = output,
        .snapshot = snap,
        .include_test_nodes = include_test_nodes,
        .include_external_nodes = include_external_nodes,
    };
}

fn parseSnapshot(args: *std.process.Args.Iterator, ctx: *ParseCtx) SnapshotArgs {
    var name: ?[]const u8 = null;
    while (args.next()) |arg| {
        if (parseGlobalFlag(arg, args, ctx)) continue;
        if (std.mem.eql(u8, arg, "--name")) {
            name = requireArg(args, "--name", ctx.stderr);
        } else {
            ctx.stderr.print("unknown option for snapshot: {s}\n", .{arg}) catch {};
            ctx.stderr.flush() catch {};
            std.process.exit(2);
        }
    }
    const tag = name orelse {
        ctx.stderr.writeAll("snapshot requires --name <tag>\n") catch {};
        ctx.stderr.flush() catch {};
        std.process.exit(2);
    };
    return .{ .name = tag };
}

fn parseDiff(args: *std.process.Args.Iterator, ctx: *ParseCtx) DiffArgs {
    var tag_a: ?[]const u8 = null;
    var tag_b: ?[]const u8 = null;
    var count: usize = 0;
    while (args.next()) |arg| {
        if (parseGlobalFlag(arg, args, ctx)) continue;
        if (arg.len > 0 and arg[0] == '-') {
            ctx.stderr.print("unknown option for diff: {s}\n", .{arg}) catch {};
            ctx.stderr.flush() catch {};
            std.process.exit(2);
        }
        count += 1;
        if (count == 1) tag_a = arg;
        if (count == 2) tag_b = arg;
    }
    if (count != 2) {
        ctx.stderr.writeAll("diff requires two snapshot tags: zcodeprism diff <snap-a> <snap-b>\n") catch {};
        ctx.stderr.flush() catch {};
        std.process.exit(2);
    }
    return .{ .tag_a = tag_a.?, .tag_b = tag_b.? };
}

fn parseServe(args: *std.process.Args.Iterator, ctx: *ParseCtx) ServeArgs {
    var workspace: ?[]const u8 = null;
    while (args.next()) |arg| {
        if (parseGlobalFlag(arg, args, ctx)) continue;
        if (std.mem.eql(u8, arg, "--workspace")) {
            workspace = requireArg(args, "--workspace", ctx.stderr);
        } else {
            ctx.stderr.print("unknown option for serve: {s}\n", .{arg}) catch {};
            ctx.stderr.flush() catch {};
            std.process.exit(2);
        }
    }
    return .{ .workspace = workspace };
}

fn parseStatus(args: *std.process.Args.Iterator, ctx: *ParseCtx) StatusArgs {
    var workspace: ?[]const u8 = null;
    while (args.next()) |arg| {
        if (parseGlobalFlag(arg, args, ctx)) continue;
        if (std.mem.eql(u8, arg, "--workspace")) {
            workspace = requireArg(args, "--workspace", ctx.stderr);
        } else {
            ctx.stderr.print("unknown option for status: {s}\n", .{arg}) catch {};
            ctx.stderr.flush() catch {};
            std.process.exit(2);
        }
    }
    return .{ .workspace = workspace };
}

fn parseArgs(args_src: std.process.Args, stderr: *std.Io.Writer) CliArgs {
    var ctx = ParseCtx{ .stderr = stderr };
    var args = args_src.iterate();
    _ = args.next();

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--version")) {
            return .{ .project_root = ctx.project_root, .verbosity = ctx.verbosity, .command = .version };
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            return .{ .project_root = ctx.project_root, .verbosity = ctx.verbosity, .command = .help };
        } else if (parseGlobalFlag(arg, &args, &ctx)) {
            // consumed
        } else if (arg.len > 0 and arg[0] == '-') {
            stderr.print("unknown option: {s}\n", .{arg}) catch {};
            stderr.flush() catch {};
            std.process.exit(2);
        } else {
            const Tag = std.meta.Tag(Command);
            const cmd_tag = std.meta.stringToEnum(Tag, arg) orelse {
                stderr.print("unknown command: {s}\n", .{arg}) catch {};
                stderr.flush() catch {};
                std.process.exit(2);
            };
            const cmd: Command = switch (cmd_tag) {
                .init => .{ .init = parseInit(&args, &ctx) },
                .index => blk: {
                    parseIndex(&args, &ctx);
                    break :blk .index;
                },
                .@"export" => .{ .@"export" = parseExport(&args, &ctx) },
                .snapshot => .{ .snapshot = parseSnapshot(&args, &ctx) },
                .diff => .{ .diff = parseDiff(&args, &ctx) },
                .serve => .{ .serve = parseServe(&args, &ctx) },
                .status => .{ .status = parseStatus(&args, &ctx) },
                .help => .help,
                .version => .version,
            };
            return .{ .project_root = ctx.project_root, .verbosity = ctx.verbosity, .command = cmd };
        }
    }

    return .{ .project_root = ctx.project_root, .verbosity = ctx.verbosity, .command = .help };
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

    switch (cli.command) {
        .version => {
            stdout.writeAll(version_string) catch {};
            stdout.flush() catch {};
        },
        .help => {
            stdout.writeAll(usage_text) catch {};
            stdout.flush() catch {};
        },
        .init => |args| runInit(io, stdout, stderr, args),
        .index => runIndex(init.gpa, io, stdout, stderr, cli.verbosity),
        .@"export" => |args| runExport(init.gpa, io, stdout, stderr, args),
        .snapshot => |args| runSnapshot(init.gpa, io, stdout, stderr, args.name),
        .diff => |args| runDiff(init.gpa, io, stdout, stderr, args),
        .serve => |args| runServe(init.gpa, io, stderr, args.workspace, cli.verbosity),
        .status => |args| runStatus(init.gpa, io, stdout, stderr, args.workspace),
    }
}

fn die(stderr: *std.Io.Writer, comptime fmt: []const u8, args: anytype) noreturn {
    stderr.print(fmt, args) catch {};
    stderr.flush() catch {};
    std.process.exit(1);
}

fn runInit(io: std.Io, stdout: *std.Io.Writer, stderr: *std.Io.Writer, args: InitArgs) void {
    const outcome = commands.init.run(io, .{
        .force = args.force,
        .workspace_template = args.workspace != null,
    }) catch |err| switch (err) {
        error.AlreadyInitialized => die(stderr, "{s} (use --force to reinitialize)\n", .{
            if (args.workspace != null) "workspace already initialized" else "already initialized",
        }),
        else => die(stderr, "init failed: {s}\n", .{@errorName(err)}),
    };

    switch (outcome) {
        .workspace_initialized => stdout.writeAll("initialized zcodeprism workspace\n") catch {},
        .project_initialized => stdout.writeAll("initialized zcodeprism project\n") catch {},
    }
    stdout.flush() catch {};
}

fn runIndex(allocator: std.mem.Allocator, io: std.Io, stdout: *std.Io.Writer, stderr: *std.Io.Writer, verbosity: u8) void {
    const project_root = std.Io.Dir.cwd().realPathFileAlloc(io, ".", allocator) catch |err|
        die(stderr, "failed to resolve project root: {s}\n", .{@errorName(err)});
    defer allocator.free(project_root);

    const cfg = loadProjectConfig(allocator, io) catch |err|
        die(stderr, "failed to load config: {s}\n", .{@errorName(err)});
    defer config.deinit(cfg, allocator);
    const full = config.withDefaults(cfg);

    var text_logger = logging.TextStderrLogger.init(io, logging.verbosityToLevel(verbosity));
    const logger = text_logger.logger();

    const result = commands.index.run(allocator, io, .{
        .project_root = project_root,
        .exclude_paths = full.exclude_paths orelse config.defaultExcludePaths(),
        .budget_bytes = budgetBytes(full),
        .storage_format = if (full.storage) |s| s.format orelse .binary else .binary,
        .logger = logger,
    }) catch |err| die(stderr, "indexing failed: {s}\n", .{@errorName(err)});

    stdout.print("indexed {d} files ({d} nodes, {d} edges)\n", .{
        result.files_indexed,
        result.node_count,
        result.edge_count,
    }) catch {};
    result.lsp.format(stdout) catch {};
    stdout.flush() catch {};
}

fn runExport(
    allocator: std.mem.Allocator,
    io: std.Io,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
    args: ExportArgs,
) void {
    const fmt: commands.@"export".Format = switch (args.format) {
        .ctg => .ctg,
        .mermaid => .mermaid,
        .jsonl => .jsonl,
    };
    const options = commands.@"export".Options{
        .format = fmt,
        .scope = args.scope,
        .include_test_nodes = args.include_test_nodes,
        .include_external_nodes = args.include_external_nodes,
        .snapshot_tag = args.snapshot,
    };

    if (args.output) |path| {
        var write_buf: [8192]u8 = undefined;
        var aw = zcodeprism.storage.atomic_file.AtomicWriter.init(io, std.Io.Dir.cwd(), path, &write_buf) catch |err|
            die(stderr, "cannot create output file: {s}\n", .{@errorName(err)});
        defer aw.deinit(io);
        commands.@"export".run(allocator, io, options, aw.writer()) catch |err|
            dieExport(stderr, err, args.snapshot);
        aw.commit(io) catch |err| die(stderr, "write failed: {s}\n", .{@errorName(err)});
    } else {
        commands.@"export".run(allocator, io, options, stdout) catch |err|
            dieExport(stderr, err, args.snapshot);
        stdout.flush() catch {};
    }
}

fn dieExport(stderr: *std.Io.Writer, err: anyerror, snapshot_tag: ?[]const u8) noreturn {
    switch (err) {
        error.SnapshotNotFound => die(stderr, "snapshot not found: {s}\n", .{snapshot_tag orelse ""}),
        error.InvalidTagName => die(stderr, "invalid snapshot tag: {s}\n", .{snapshot_tag orelse ""}),
        error.FileNotFound => die(stderr, "failed to load graph (run 'index' first)\n", .{}),
        else => die(stderr, "export failed: {s}\n", .{@errorName(err)}),
    }
}

fn runSnapshot(allocator: std.mem.Allocator, io: std.Io, stdout: *std.Io.Writer, stderr: *std.Io.Writer, name: []const u8) void {
    commands.snapshot.run(allocator, io, .{ .tag = name }) catch |err| switch (err) {
        error.InvalidTagName => die(stderr, "invalid snapshot tag: {s}\n", .{name}),
        error.TagTooLong => die(stderr, "snapshot tag too long (max {d}): {s}\n", .{ snapshot.MAX_TAG_LENGTH, name }),
        error.FileNotFound => die(stderr, "failed to load graph (run 'index' first)\n", .{}),
        else => die(stderr, "failed to save snapshot: {s}\n", .{@errorName(err)}),
    };
    stdout.print("snapshot saved: {s}\n", .{name}) catch {};
    stdout.flush() catch {};
}

fn runDiff(allocator: std.mem.Allocator, io: std.Io, stdout: *std.Io.Writer, stderr: *std.Io.Writer, args: DiffArgs) void {
    var out: std.ArrayList(u8) = .empty;
    defer out.deinit(allocator);

    commands.diff.run(allocator, io, .{ .tag_a = args.tag_a, .tag_b = args.tag_b }, &out) catch |err| switch (err) {
        error.SnapshotNotFound, error.InvalidTagName => die(stderr, "failed to load snapshot: {s}\n", .{@errorName(err)}),
        else => die(stderr, "diff failed: {s}\n", .{@errorName(err)}),
    };

    stdout.writeAll(out.items) catch {};
    stdout.flush() catch {};
}

fn runServe(allocator: std.mem.Allocator, io: std.Io, stderr: *std.Io.Writer, workspace_arg: ?[]const u8, verbosity: u8) void {
    const project_root = std.Io.Dir.cwd().realPathFileAlloc(io, ".", allocator) catch |err|
        die(stderr, "failed to resolve project root: {s}\n", .{@errorName(err)});
    defer allocator.free(project_root);

    const cfg = loadProjectConfig(allocator, io) catch |err|
        die(stderr, "failed to load config: {s}\n", .{@errorName(err)});
    defer config.deinit(cfg, allocator);
    const full = config.withDefaults(cfg);

    var text_logger = logging.TextStderrLogger.init(io, logging.verbosityToLevel(verbosity));
    const logger = text_logger.logger();

    commands.serve.run(allocator, io, .{
        .project_root = project_root,
        .workspace_path = workspace_arg,
        .exclude_paths = full.exclude_paths orelse config.defaultExcludePaths(),
        .budget_bytes = budgetBytes(full),
        .logger = logger,
    }) catch |err| die(stderr, "serve failed: {s}\n", .{@errorName(err)});
}

fn runStatus(allocator: std.mem.Allocator, io: std.Io, stdout: *std.Io.Writer, stderr: *std.Io.Writer, workspace_arg: ?[]const u8) void {
    const result = commands.status.run(allocator, io, .{ .workspace_path = workspace_arg }) catch
        die(stderr, "not initialized or not indexed\n", .{});

    const source_hex = types.formatHash(result.source_hash);
    stdout.print(
        "nodes: {d} ({d} files, {d} functions, {d} types)\n" ++
            "edges: {d}\n" ++
            "source_hash: {s}\n",
        .{
            result.node_count,
            result.file_count,
            result.function_count,
            result.type_count,
            result.edge_count,
            &source_hex,
        },
    ) catch {};
    stdout.flush() catch {};
}

fn budgetBytes(full: config.Config) ?u64 {
    if (full.memory) |m| if (m.budget_mb) |mb| return @as(u64, mb) * 1024 * 1024;
    return null;
}

fn loadProjectConfig(allocator: std.mem.Allocator, io: std.Io) !config.Config {
    const file = std.Io.Dir.cwd().openFile(io, ".zcodeprism.zon", .{}) catch |err| {
        if (err == error.FileNotFound) return config.Config{};
        return err;
    };
    defer file.close(io);

    var read_buf: [4096]u8 = undefined;
    var fr = file.reader(io, &read_buf);
    const content = try fr.interface.allocRemaining(allocator, .limited(1024 * 1024));
    defer allocator.free(content);
    const content_z = try allocator.dupeZ(u8, content);
    defer allocator.free(content_z);

    return config.parseFromSlice(allocator, content_z);
}
