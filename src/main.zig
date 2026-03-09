const std = @import("std");
const zcodeprism = @import("zcodeprism");

const config = zcodeprism.config;
const indexer = zcodeprism.indexer;
const storage = zcodeprism.storage;
const Graph = zcodeprism.Graph;
const NodeKind = zcodeprism.NodeKind;
const EdgeType = zcodeprism.EdgeType;
const logging = zcodeprism.logging;

const version_string = "zcodeprism 0.1.0\n";

const usage_text =
    \\Usage: zcodeprism <command> [options]
    \\
    \\Commands:
    \\  init       Initialize a new project (.zcodeprism.zon + .zcodeprism/)
    \\  index      Index the codebase and build the code graph
    \\  status     Show project status and graph statistics
    \\
    \\Options:
    \\  --version  Print version and exit
    \\  --help     Show this help message
    \\  --force    Force overwrite (with init)
    \\  --full     Full re-index (with index, default)
    \\  --json     Output in JSON format
    \\  -v         Increase verbosity (up to -vvv)
    \\
;

const Command = enum {
    init,
    index,
    status,
    help,
    version,
};

pub fn main() void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    var stderr_buffer: [4096]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
    const stderr = &stderr_writer.interface;

    var args = std.process.args();
    _ = args.next();

    var command: ?Command = null;
    var force = false;
    var verbosity: u8 = 0;

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--version")) {
            command = .version;
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            command = .help;
        } else if (std.mem.eql(u8, arg, "--force")) {
            force = true;
        } else if (std.mem.eql(u8, arg, "--full") or std.mem.eql(u8, arg, "--incremental") or std.mem.eql(u8, arg, "--json")) {
            // Accepted but not yet used beyond index.
        } else if (std.mem.startsWith(u8, arg, "-v")) {
            // Count v's: -v, -vv, -vvv
            var count: u8 = 0;
            for (arg[1..]) |c| {
                if (c == 'v') count += 1 else break;
            }
            verbosity = @max(verbosity, count);
        } else if (arg[0] == '-') {
            stderr.print("unknown option: {s}\n", .{arg}) catch {};
            stderr.flush() catch {};
            std.process.exit(2);
        } else {
            // Subcommand name.
            if (command == null) {
                if (std.mem.eql(u8, arg, "init")) {
                    command = .init;
                } else if (std.mem.eql(u8, arg, "index")) {
                    command = .index;
                } else if (std.mem.eql(u8, arg, "status")) {
                    command = .status;
                } else {
                    stderr.print("unknown command: {s}\n", .{arg}) catch {};
                    stderr.flush() catch {};
                    std.process.exit(2);
                }
            }
        }
    }

    const cmd = command orelse {
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
        .init => runInit(stdout, stderr, force),
        .index => runIndex(stdout, stderr, verbosity),
        .status => runStatus(stdout, stderr),
    }
}

fn runInit(stdout: *std.Io.Writer, stderr: *std.Io.Writer, force: bool) void {
    const cwd = std.fs.cwd();

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

    const idx_result = indexer.indexDirectory(allocator, project_root, &graph, .{
        .exclude_paths = full.exclude_paths orelse config.defaultExcludePaths(),
        .logger = logger,
    }) catch |err| {
        stderr.print("indexing failed: {s}\n", .{@errorName(err)}) catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    };

    const fmt = if (full.storage) |s| s.format orelse .binary else .binary;
    switch (fmt) {
        .binary => {
            storage.binary.save(allocator, &graph, ".zcodeprism/graph.bin") catch |err| {
                stderr.print("failed to save binary graph: {s}\n", .{@errorName(err)}) catch {};
                stderr.flush() catch {};
                std.process.exit(1);
            };
        },
        .jsonl => {
            saveJsonl(allocator, &graph) catch |err| {
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
    stdout.flush() catch {};
}

fn saveJsonl(allocator: std.mem.Allocator, graph: *const Graph) !void {
    const file = try std.fs.cwd().createFile(".zcodeprism/graph.jsonl", .{});
    defer file.close();
    var buf: [8192]u8 = undefined;
    var writer = file.writer(&buf);
    try storage.jsonl.exportJsonl(allocator, graph, &writer.interface);
    try writer.interface.flush();
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

fn runStatus(stdout: *std.Io.Writer, stderr: *std.Io.Writer) void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var graph = storage.binary.load(allocator, ".zcodeprism/graph.bin") catch {
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
