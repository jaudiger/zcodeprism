const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const config = @import("../core/config.zig");
const logging = @import("../logging.zig");
const indexer = @import("../parser/indexer.zig");
const storage = @import("../storage/storage.zig");
const types = @import("../core/types.zig");
const lang_support = @import("../languages/language_support.zig");
const lsp_enricher = @import("../lsp/enricher.zig");
const lsp_pool_mod = @import("../lsp/pool.zig");
const worklist_mod = @import("../lsp/worklist.zig");

const Graph = graph_mod.Graph;
const FrozenGraph = graph_mod.FrozenGraph;
const Logger = logging.Logger;
const EnrichResult = lang_support.EnrichResult;

/// Options for `index`.
pub const Options = struct {
    project_root: []const u8,
    exclude_paths: []const []const u8 = &.{},
    budget_bytes: ?u64 = null,
    storage_format: config.StorageFormat = .binary,
    storage_path: []const u8 = storage.data_dir,
    enabled_languages: ?[]const types.Language = null,
    lsp_paths: lsp_pool_mod.LspServerPaths = .{},
    logger: Logger = Logger.noop,
};

/// Stats reported after a successful `index` run.
pub const Result = struct {
    files_indexed: usize,
    node_count: usize,
    edge_count: usize,
    lsp: EnrichResult,
};

/// Index the project at `options.project_root`, run LSP enrichment for
/// every registered language, and persist the resulting graph in the
/// requested storage format. Returns counters describing the run.
pub fn run(allocator: std.mem.Allocator, io: std.Io, options: Options) !Result {
    var graph = Graph.init(options.project_root);
    defer graph.deinit(allocator);

    var wl = worklist_mod.LspWorklist{};
    defer wl.deinit(allocator);

    const allocs = indexer.IndexAllocators.single(allocator);

    const idx_result = try indexer.indexDirectory(allocs, io, options.project_root, &graph, &wl, .{
        .exclude_paths = options.exclude_paths,
        .logger = options.logger,
        .budget_bytes = options.budget_bytes,
        .enabled_languages = options.enabled_languages,
    });

    var pool = lsp_pool_mod.LspPool.init(.{ .server_paths = options.lsp_paths });
    defer pool.deinit(allocator, io);

    const lsp_result = try lsp_enricher.enrichAllLanguages(allocs, io, &graph, &wl, &pool, .{
        .logger = options.logger,
        .project_root = options.project_root,
        .enabled_languages = options.enabled_languages,
    });

    const fg = FrozenGraph{ .graph = &graph };
    var layout = try storage.Layout.init(allocator, options.storage_path);
    defer layout.deinit();
    switch (options.storage_format) {
        .binary => try storage.binary.save(allocator, io, fg, layout.graph_binary),
        .jsonl => try saveJsonl(allocator, io, fg, layout.graph_jsonl),
    }

    return .{
        .files_indexed = idx_result.files_indexed,
        .node_count = graph.nodeCount(),
        .edge_count = graph.edgeCount(),
        .lsp = lsp_result,
    };
}

fn saveJsonl(allocator: std.mem.Allocator, io: std.Io, fg: FrozenGraph, path: []const u8) !void {
    var write_buf: [8192]u8 = undefined;
    var aw = try storage.atomic_file.AtomicWriter.init(io, std.Io.Dir.cwd(), path, &write_buf);
    defer aw.deinit(io);
    try storage.jsonl.exportJsonl(allocator, fg, aw.writer());
    try aw.commit(io);
}
