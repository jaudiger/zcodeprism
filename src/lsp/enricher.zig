//! LSP enricher: top-level orchestrator that spawns an LSP client,
//! opens all project files, waits for server readiness, delegates to
//! the language-specific enrichFn callback, re-runs post-freeze
//! enrichment, and shuts down the client.

const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const lang_support = @import("../languages/language_support.zig");
const enrichment = @import("../enrichment/enrichment.zig");
const logging = @import("../logging.zig");
const client_mod = @import("client.zig");

const worklist_mod = @import("worklist.zig");
const LspWorklist = worklist_mod.LspWorklist;

const Graph = graph_mod.Graph;
const LanguageSupport = lang_support.LanguageSupport;
const EnrichResult = lang_support.EnrichResult;
const Logger = logging.Logger;
const Field = logging.Field;
const LspClient = client_mod.LspClient;

/// Options for the LSP enrichment pass.
pub const EnrichOptions = struct {
    logger: Logger = Logger.noop,
    /// Project root path for LSP workspace initialization.
    project_root: ?[]const u8 = null,
    /// Milliseconds to wait for server readiness signals after opening files.
    warmup_timeout_ms: u32 = 5000,
};

/// Top-level LSP enrichment orchestrator. Spawns an LSP client, opens
/// all project files for server analysis, delegates to the language's
/// enrichFn callback, re-freezes the graph, and re-runs post-freeze
/// enrichment. Returns a zero-valued result when lsp_config or enrichFn
/// is null.
pub fn enrich(
    allocator: std.mem.Allocator,
    graph: *Graph,
    language_support_val: *const LanguageSupport,
    wl: *const LspWorklist,
    options: EnrichOptions,
) !EnrichResult {
    const log = options.logger;
    const lsp_config = language_support_val.lsp_config orelse return .{};
    const enrich_fn = lsp_config.enrichFn orelse return .{};

    const root = options.project_root orelse graph.project_root;

    var client = LspClient.init(log);
    defer client.deinit();

    client.start(allocator, lsp_config.server_command) catch {
        log.warn("LSP: failed to start server", &.{Field.string("server", lsp_config.server_name)});
        return .{};
    };

    client.initialize(allocator, root) catch {
        log.warn("LSP: initialize handshake failed", &.{});
        return .{};
    };

    // Open all files so the server can analyze them.
    for (graph.nodes.items) |n| {
        if (n.kind != .file) continue;
        const file_path = n.file_path orelse continue;

        // Graph stores paths relative to project root; build absolute path.
        const abs_path = std.fs.path.join(allocator, &.{ root, file_path }) catch continue;
        defer allocator.free(abs_path);

        const source = std.fs.openFileAbsolute(abs_path, .{}) catch {
            log.debug("could not open file", &.{Field.string("path", abs_path)});
            continue;
        };
        defer source.close();
        const text = source.readToEndAlloc(allocator, 10 * 1024 * 1024) catch continue;
        defer allocator.free(text);

        const uri = client_mod.pathToUri(allocator, abs_path) catch continue;
        defer allocator.free(uri);

        const lang_id: []const u8 = switch (language_support_val.language) {
            .zig => "zig",
            .rust => "rust",
        };
        client.textDocumentDidOpen(allocator, uri, text, lang_id) catch continue;
    }

    // Drain server notifications until a readiness signal arrives or the timeout expires.
    const warmup = client.drainNotifications(allocator, options.warmup_timeout_ms, log);
    log.debug("LSP warmup", &.{
        Field.uint("notifications", @as(u64, warmup.notifications_drained)),
        Field.uint("ms", warmup.elapsed_ms),
        Field.boolean("ready", warmup.ready_signal),
    });

    // Call the language-specific enrichment callback.
    var result = try enrich_fn(allocator, graph, &client, wl, log);
    result.warmup_ms = warmup.elapsed_ms;

    // One-line summary at info level: resolution ratio and phantom backlog.
    log.info("LSP enrichment complete", &.{
        Field.uint("worklist_resolved", @as(u64, result.worklist_resolved)),
        Field.uint("worklist_total", @as(u64, result.worklist_total)),
        Field.uint("phantoms_remaining", @as(u64, result.phantoms_remaining)),
        Field.uint("edges_promoted", @as(u64, result.edges_promoted)),
        Field.uint("edges_added", @as(u64, result.edges_added)),
        Field.uint("errors_inferred", @as(u64, result.errors_inferred)),
        Field.uint("phantoms_enriched", @as(u64, result.phantoms_enriched)),
    });
    // Per-query-kind breakdown at debug level.
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

    // Stop server before re-freeze.
    client.stop();

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

test "enricher handles no-op cases" {
    // Arrange: language support with null lsp_config
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

    // Act
    var wl = worklist_mod.LspWorklist{};
    defer wl.deinit(std.testing.allocator);
    const result = try enrich(std.testing.allocator, &graph, &no_lsp, &wl, .{});

    // Assert: zero-valued result, graph unchanged
    try std.testing.expectEqual(@as(usize, 0), result.edges_promoted);
    try std.testing.expectEqual(@as(usize, 0), result.edges_added);
    try std.testing.expectEqual(@as(usize, 0), result.errors_inferred);
    try std.testing.expectEqual(@as(usize, 0), graph.nodeCount());
    try std.testing.expectEqual(@as(usize, 0), graph.edgeCount());
}

fn stubParse(_: std.mem.Allocator, _: []const u8, _: *Graph, _: ?[]const u8, _: Logger) error{OutOfMemory}!void {
    @panic("unexpected parse call in enricher test");
}
