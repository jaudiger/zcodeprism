//! LSP enricher: top-level orchestrator that acquires a pooled LSP
//! connection, opens project files if needed, delegates to the
//! language-specific enrichFn callback, and re-runs post-freeze
//! enrichment.

const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const lang_support = @import("../languages/language_support.zig");
const registry_mod = @import("../languages/registry.zig");
const enrichment = @import("../enrichment/enrichment.zig");
const logging = @import("../logging.zig");
const client_mod = @import("client.zig");
const pool_mod = @import("pool.zig");

const worklist_mod = @import("worklist.zig");
const LspWorklist = worklist_mod.LspWorklist;

const Graph = graph_mod.Graph;
const LanguageSupport = lang_support.LanguageSupport;
const EnrichResult = lang_support.EnrichResult;
const Logger = logging.Logger;
const Field = logging.Field;
const LspClient = client_mod.LspClient;
const LspPool = pool_mod.LspPool;
const Registry = registry_mod.Registry;

/// Options for the LSP enrichment pass.
pub const EnrichOptions = struct {
    logger: Logger = Logger.noop,
    /// Project root path for LSP workspace initialization.
    project_root: ?[]const u8 = null,
    /// Milliseconds to wait for server readiness signals after opening files.
    warmup_timeout_ms: u32 = 5000,
};

/// Top-level LSP enrichment orchestrator. Acquires a pooled connection,
/// opens project files on cold start, delegates to the language's enrichFn
/// callback, re-freezes the graph, and re-runs post-freeze enrichment.
/// Returns a zero-valued result when lsp_config or enrichFn is null.
pub fn enrich(
    allocator: std.mem.Allocator,
    io: std.Io,
    graph: *Graph,
    language_support_val: *const LanguageSupport,
    wl: *const LspWorklist,
    pool: *LspPool,
    options: EnrichOptions,
) !EnrichResult {
    const log = options.logger;
    const lsp_config = language_support_val.lsp_config orelse return .{};
    const enrich_fn = lsp_config.enrichFn orelse return .{};

    const root = options.project_root orelse graph.project_root;
    const lang_id: []const u8 = switch (language_support_val.language) {
        .zig => "zig",
        .rust => "rust",
    };

    const conn = pool.acquire(allocator, io, language_support_val.language, &lsp_config, root, log) catch {
        log.warn("LSP: failed to acquire connection", &.{Field.string("server", lsp_config.server_name)});
        return .{};
    };
    defer pool.release(io, language_support_val.language);

    var warmup_elapsed: u64 = 0;
    if (conn.openedFileCount() == 0) {
        conn.openAllFiles(allocator, io, graph, lang_id, root, log);

        const warmup = conn.client.drainNotifications(allocator, io, options.warmup_timeout_ms, log);
        log.debug("LSP warmup", &.{
            Field.uint("notifications", @as(u64, warmup.notifications_drained)),
            Field.uint("ms", warmup.elapsed_ms),
            Field.boolean("ready", warmup.ready_signal),
        });
        warmup_elapsed = warmup.elapsed_ms;
    }

    var result = try enrich_fn(allocator, io, graph, &conn.client, wl, log);
    result.warmup_ms = warmup_elapsed;

    log.info("LSP enrichment complete", &.{
        Field.uint("worklist_resolved", @as(u64, result.worklist_resolved)),
        Field.uint("worklist_total", @as(u64, result.worklist_total)),
        Field.uint("phantoms_remaining", @as(u64, result.phantoms_remaining)),
        Field.uint("edges_promoted", @as(u64, result.edges_promoted)),
        Field.uint("edges_added", @as(u64, result.edges_added)),
        Field.uint("errors_inferred", @as(u64, result.errors_inferred)),
        Field.uint("phantoms_enriched", @as(u64, result.phantoms_enriched)),
    });
    log.debug("LSP enrichment queries", &.{
        Field.uint("definition_queries", @as(u64, result.definition_queries)),
        Field.uint("definition_successes", @as(u64, result.definition_successes)),
        Field.uint("type_definition_queries", @as(u64, result.type_definition_queries)),
        Field.uint("type_definition_successes", @as(u64, result.type_definition_successes)),
        Field.uint("hover_queries", @as(u64, result.hover_queries)),
        Field.uint("hover_successes", @as(u64, result.hover_successes)),
        Field.uint("reference_queries", @as(u64, result.reference_queries)),
        Field.uint("reference_successes", @as(u64, result.reference_successes)),
    });

    // Re-freeze to rebuild adjacency with any new edges.
    _ = graph.freeze(allocator) catch {
        log.warn("LSP: re-freeze failed", &.{});
        return result;
    };

    // Re-run post-freeze enrichment.
    enrichment.enrichPostFreeze(allocator, graph, .{ .logger = log }) catch {
        log.warn("LSP: post-freeze enrichment failed", &.{});
    };

    return result;
}

/// Run `enrich` over every registered language, accumulating results.
/// Returns the first error encountered.
pub fn enrichAllLanguages(
    allocator: std.mem.Allocator,
    io: std.Io,
    graph: *Graph,
    wl: *const LspWorklist,
    pool: *LspPool,
    options: EnrichOptions,
) !EnrichResult {
    var result = EnrichResult{};
    for (Registry.allLanguages()) |ls| {
        const r = try enrich(allocator, io, graph, ls, wl, pool, options);
        result.accumulate(r);
    }
    return result;
}

test "enricher handles no-op cases" {
    // Arrange
    const no_lsp = comptime blk: {
        var ls: LanguageSupport = undefined;
        ls.language = .zig;
        ls.extensions = &.{".zig"};
        ls.parseFn = &stubParse;
        ls.lsp_config = null;
        ls.excluded_dirs = &.{};
        ls.build_files = &.{};
        ls.import_granularity = .file;
        ls.extractImportsFn = null;
        ls.resolveImportPathFn = null;
        ls.parseBuildConfigFn = null;
        ls.resolvePhantomsFn = null;
        ls.buildEdgesFn = null;
        ls.grammarFn = undefined;
        break :blk ls;
    };

    var graph = Graph.init(".");
    defer graph.deinit(std.testing.allocator);

    var pool = LspPool.init(.{});
    defer pool.deinit(std.testing.allocator, std.testing.io);

    // Act
    var wl = worklist_mod.LspWorklist{};
    defer wl.deinit(std.testing.allocator);
    const result = try enrich(std.testing.allocator, std.testing.io, &graph, &no_lsp, &wl, &pool, .{});

    // Assert
    try std.testing.expectEqual(@as(usize, 0), result.edges_promoted);
    try std.testing.expectEqual(@as(usize, 0), result.edges_added);
    try std.testing.expectEqual(@as(usize, 0), result.errors_inferred);
    try std.testing.expectEqual(@as(usize, 0), graph.nodeCount());
    try std.testing.expectEqual(@as(usize, 0), graph.edgeCount());
    try std.testing.expectEqual(@as(u32, 0), pool.connectionCount());
}

fn stubParse(_: std.mem.Allocator, _: std.Io, _: []const u8, _: *Graph, _: ?[]const u8, _: Logger) error{OutOfMemory}!void {
    @panic("unexpected parse call in enricher test");
}
