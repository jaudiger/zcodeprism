const std = @import("std");
const types = @import("../core/types.zig");
const generation_mod = @import("../core/generation.zig");
const workspace_loader = @import("../storage/workspace_loader.zig");
const logging = @import("../logging.zig");
const indexer = @import("../parser/indexer.zig");
const lsp_enricher = @import("../lsp/enricher.zig");
const lsp_pool_mod = @import("../lsp/pool.zig");
const lsp_worklist_mod = @import("../lsp/worklist.zig");
const lang_support = @import("../languages/language_support.zig");
const watcher_mod = @import("../watcher/watcher.zig");
const debouncer_mod = @import("../watcher/debouncer.zig");
const gen_manager_mod = @import("../watcher/generation_manager.zig");
const mcp = @import("../mcp/mcp.zig");
const source_hash = @import("source_hash.zig");

const GraphGeneration = generation_mod.GraphGeneration;
const GenerationManager = gen_manager_mod.GenerationManager;
const FileWatcher = watcher_mod.FileWatcher;
const Debouncer = debouncer_mod.Debouncer;
const LspPool = lsp_pool_mod.LspPool;
const LspWorklist = lsp_worklist_mod.LspWorklist;
const Logger = logging.Logger;
const EnrichResult = lang_support.EnrichResult;
const Server = mcp.server.Server;

const zero_hash: types.ContentHash = .{0} ** types.hash_len;

/// Options for `serve`.
pub const Options = struct {
    project_root: []const u8,
    workspace_path: ?[]const u8 = null,
    exclude_paths: []const []const u8 = &.{},
    budget_bytes: ?u64 = null,
    logger: Logger = Logger.noop,
};

/// Module-private fd used by `handleSigterm` to break the stdio loop.
var sigterm_stdin_fd: std.posix.fd_t = 0;

fn handleSigterm(_: std.c.SIG) callconv(.c) void {
    _ = std.c.close(sigterm_stdin_fd);
}

/// Start the MCP server on stdio with background indexing.
/// Returns when stdin reaches EOF (natural close or SIGTERM-driven).
pub fn run(allocator: std.mem.Allocator, io: std.Io, options: Options) !void {
    sigterm_stdin_fd = std.Io.File.stdin().handle;
    std.posix.sigaction(std.posix.SIG.TERM, &.{
        .handler = .{ .handler = handleSigterm },
        .mask = std.posix.sigemptyset(),
        .flags = 0,
    }, null);

    const initial_gen = try GraphGeneration.create(allocator, io, 1, zero_hash);

    var lsp_pool = LspPool.init(.{});
    defer lsp_pool.deinit(allocator, io);

    var gen_manager = GenerationManager.init(initial_gen);
    defer gen_manager.deinit();

    var server = Server.init(&gen_manager);
    defer server.deinit();

    var stdout_mutex: std.Io.Mutex = .init;

    var watch_root_owned: ?[]const u8 = null;
    defer if (watch_root_owned) |buf| allocator.free(buf);

    const watch_root = if (options.workspace_path) |ws| blk: {
        const resolved = std.Io.Dir.cwd().realPathFileAlloc(
            io,
            std.fs.path.dirname(ws) orelse ".",
            allocator,
        ) catch break :blk options.project_root;
        watch_root_owned = resolved;
        break :blk resolved;
    } else options.project_root;

    var ctx = WatcherContext{
        .allocator = allocator,
        .io = io,
        .gen_manager = &gen_manager,
        .lsp_pool = &lsp_pool,
        .project_root = options.project_root,
        .exclude_paths = options.exclude_paths,
        .stdout_mutex = &stdout_mutex,
        .logger = options.logger,
        .workspace_path = options.workspace_path,
        .watch_root = watch_root,
        .budget_bytes = options.budget_bytes,
    };

    const watcher_thread = try std.Thread.spawn(.{}, watcherThreadFn, .{&ctx});
    defer watcher_thread.join();

    stdioLoop(allocator, io, &server, &stdout_mutex);
}

const WatcherContext = struct {
    allocator: std.mem.Allocator,
    io: std.Io,
    gen_manager: *GenerationManager,
    lsp_pool: *LspPool,
    project_root: []const u8,
    exclude_paths: []const []const u8,
    stdout_mutex: *std.Io.Mutex,
    logger: Logger,
    workspace_path: ?[]const u8,
    watch_root: []const u8,
    budget_bytes: ?u64,
};

fn watcherThreadFn(ctx: *WatcherContext) void {
    var generation_id: u64 = 1;

    generation_id += 1;
    _ = reindexAndSwap(ctx, generation_id);

    var file_watcher = FileWatcher.init(ctx.allocator, ctx.io, ctx.watch_root, ctx.exclude_paths) catch return;
    defer file_watcher.deinit(ctx.allocator);

    var debouncer = Debouncer.init(500);

    while (true) {
        if (!file_watcher.waitForEvents()) break;
        debouncer.trigger(ctx.io);

        while (!debouncer.isReady(ctx.io)) {
            std.Io.sleep(
                ctx.io,
                .fromNanoseconds(@intCast(@as(i96, debouncer.remainingMs(ctx.io)) * std.time.ns_per_ms)),
                .real,
            ) catch break;
            if (debouncer.isReady(ctx.io)) break;
        }

        generation_id += 1;
        _ = reindexAndSwap(ctx, generation_id);
    }
}

fn reindexAndSwap(ctx: *WatcherContext, generation_id: u64) bool {
    const new_gen = GraphGeneration.create(ctx.allocator, ctx.io, generation_id, zero_hash) catch return false;
    if (!reindexInto(ctx, new_gen)) {
        new_gen.release();
        return false;
    }

    new_gen.source_hash = source_hash.computeRuntimeSourceHash(&new_gen.graph);

    const guard = new_gen.acquire();
    defer guard.deinit();
    ctx.gen_manager.swap(ctx.io, new_gen);
    notifyGraphUpdated(ctx, new_gen);
    return true;
}

fn reindexInto(ctx: *WatcherContext, gen: *GraphGeneration) bool {
    if (ctx.workspace_path) |ws| {
        gen.graph = workspace_loader.loadAndAssemble(ctx.allocator, ctx.io, ws) catch return false;
        return true;
    }

    var wl = LspWorklist{};
    defer wl.deinit(ctx.allocator);

    _ = indexer.indexDirectory(ctx.allocator, ctx.io, ctx.project_root, &gen.graph, &wl, .{
        .exclude_paths = ctx.exclude_paths,
        .logger = ctx.logger,
        .budget_bytes = ctx.budget_bytes,
    }) catch return false;

    _ = lsp_enricher.enrichAllLanguages(ctx.allocator, ctx.io, &gen.graph, &wl, ctx.lsp_pool, .{
        .logger = ctx.logger,
        .project_root = ctx.project_root,
    }) catch EnrichResult{};
    return true;
}

fn notifyGraphUpdated(ctx: *WatcherContext, gen: *GraphGeneration) void {
    const notification = Server.buildNotification(
        ctx.allocator,
        "graph/updated",
        gen.generation_id,
        gen.source_hash,
    ) catch return;
    defer ctx.allocator.free(notification);

    ctx.stdout_mutex.lockUncancelable(ctx.io);
    defer ctx.stdout_mutex.unlock(ctx.io);

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.Io.File.stdout().writer(ctx.io, &stdout_buffer);
    const stdout = &stdout_writer.interface;
    stdout.writeAll(notification) catch {};
    stdout.writeAll("\n") catch {};
    stdout.flush() catch {};
}

fn stdioLoop(allocator: std.mem.Allocator, io: std.Io, server: *Server, stdout_mutex: *std.Io.Mutex) void {
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
