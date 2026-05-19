//! WatcherService: owns the background reindex thread and the platform
//! FileWatcher. `start` performs one initial reindex on the background
//! thread and then enters a debounced file-watch loop. `stop` signals
//! the watcher via its self-pipe, joins the thread, then frees the
//! watcher in that order.

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
const watcher_mod = @import("watcher.zig");
const debouncer_mod = @import("debouncer.zig");
const gen_manager_mod = @import("generation_manager.zig");
const mcp = @import("../mcp/mcp.zig");
const source_hash = @import("../commands/source_hash.zig");

const GraphGeneration = generation_mod.GraphGeneration;
const GenerationManager = gen_manager_mod.GenerationManager;
const FileWatcher = watcher_mod.FileWatcher;
const Debouncer = debouncer_mod.Debouncer;
const LspPool = lsp_pool_mod.LspPool;
const LspWorklist = lsp_worklist_mod.LspWorklist;
const Logger = logging.Logger;
const Field = logging.Field;
const EnrichResult = lang_support.EnrichResult;
const Server = mcp.server.Server;
const IndexAllocators = indexer.IndexAllocators;

const zero_hash: types.ContentHash = .{0} ** types.hash_len;

/// All fields are borrowed and must outlive the service.
pub const ReindexContext = struct {
    /// Long-lived allocator. Owns the LspPool's connection state, the
    /// per-reindex LspWorklist, transient indexer scratch, and stdout
    /// notification buffers.
    allocator: std.mem.Allocator,
    io: std.Io,
    gen_manager: *GenerationManager,
    lsp_pool: *LspPool,
    project_root: []const u8,
    exclude_paths: []const []const u8,
    stdout_mutex: *std.Io.Mutex,
    logger: Logger,
    workspace_path: ?[]const u8,
    /// Filesystem subtree the watcher monitors. Equal to `project_root`
    /// in single-project mode; the directory holding
    /// `zcodeprism-workspace.zon` in workspace mode.
    watch_root: []const u8,
    budget_bytes: ?u64,
    enabled_languages: ?[]const types.Language = null,
};

/// Construct via `start`, destroy via `stop`. `stop` may be called
/// exactly once whether or not `FileWatcher.init` succeeded.
pub const WatcherService = struct {
    thread: std.Thread,
    /// Null when `FileWatcher.init` failed; the thread runs the initial
    /// reindex and exits.
    file_watcher: ?*FileWatcher,
    allocator: std.mem.Allocator,
    ctx: *ReindexContext,

    pub fn start(allocator: std.mem.Allocator, ctx: *ReindexContext) !WatcherService {
        const fw_ptr: ?*FileWatcher = blk: {
            const heap_fw = allocator.create(FileWatcher) catch break :blk null;
            heap_fw.* = FileWatcher.init(allocator, ctx.io, ctx.watch_root, ctx.exclude_paths) catch |err| {
                ctx.logger.warn("file watcher unavailable", &.{
                    Field.string("error", @errorName(err)),
                    Field.string("watch_root", ctx.watch_root),
                });
                allocator.destroy(heap_fw);
                break :blk null;
            };
            break :blk heap_fw;
        };
        errdefer if (fw_ptr) |fw| {
            fw.deinit(allocator);
            allocator.destroy(fw);
        };

        const thread = try std.Thread.spawn(.{}, threadMain, .{ ctx, fw_ptr });
        return .{
            .thread = thread,
            .file_watcher = fw_ptr,
            .allocator = allocator,
            .ctx = ctx,
        };
    }

    /// Signal the watcher, join the thread, then deinit and free the
    /// FileWatcher. Safe to call exactly once.
    pub fn stop(self: *WatcherService) void {
        if (self.file_watcher) |fw| fw.stop();
        self.thread.join();
        if (self.file_watcher) |fw| {
            fw.deinit(self.allocator);
            self.allocator.destroy(fw);
            self.file_watcher = null;
        }
    }
};

fn threadMain(ctx: *ReindexContext, file_watcher: ?*FileWatcher) void {
    var generation_id: u64 = 1;

    generation_id += 1;
    _ = reindexAndSwap(ctx, generation_id);

    const fw = file_watcher orelse return;

    var debouncer = Debouncer.init(500);

    while (true) {
        if (!fw.waitForEvents()) break;
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

fn reindexAndSwap(ctx: *ReindexContext, generation_id: u64) bool {
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

fn reindexInto(ctx: *ReindexContext, gen: *GraphGeneration) bool {
    const allocs = IndexAllocators{
        .graph = gen.arena.allocator(),
        .scratch = ctx.allocator,
    };

    if (ctx.workspace_path) |ws| {
        gen.graph = workspace_loader.loadAndAssemble(allocs, ctx.io, ws) catch return false;
        return true;
    }

    var wl = LspWorklist{};
    defer wl.deinit(allocs.graph);

    _ = indexer.indexDirectory(allocs, ctx.io, ctx.project_root, &gen.graph, &wl, .{
        .exclude_paths = ctx.exclude_paths,
        .logger = ctx.logger,
        .budget_bytes = ctx.budget_bytes,
        .enabled_languages = ctx.enabled_languages,
    }) catch return false;

    _ = lsp_enricher.enrichAllLanguages(allocs, ctx.io, &gen.graph, &wl, ctx.lsp_pool, .{
        .logger = ctx.logger,
        .project_root = ctx.project_root,
        .enabled_languages = ctx.enabled_languages,
    }) catch EnrichResult{};
    return true;
}

fn notifyGraphUpdated(ctx: *ReindexContext, gen: *GraphGeneration) void {
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
