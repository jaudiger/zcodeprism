const std = @import("std");
const types = @import("../core/types.zig");
const graph_mod = @import("../core/graph.zig");
const storage = @import("../storage/storage.zig");
const source_hash = @import("source_hash.zig");
const indexer = @import("../parser/indexer.zig");

const Graph = graph_mod.Graph;

/// Options for `status`.
pub const Options = struct {
    /// When set, load and assemble the workspace at this config path
    /// instead of the single-project graph.
    workspace_path: ?[]const u8 = null,
    /// Data directory containing the persisted graph.
    storage_path: []const u8 = storage.data_dir,
};

/// Aggregate statistics over the loaded graph.
pub const Result = struct {
    node_count: usize,
    edge_count: usize,
    file_count: usize,
    function_count: usize,
    type_count: usize,
    other_count: usize,
    source_hash: types.ContentHash,
};

/// Load the current graph (single-project or workspace) and compute
/// summary counters and a runtime source hash.
pub fn run(allocator: std.mem.Allocator, io: std.Io, options: Options) !Result {
    var graph = if (options.workspace_path) |ws_path|
        try storage.workspace_loader.loadAndAssemble(indexer.IndexAllocators.single(allocator), io, ws_path)
    else blk: {
        var layout = try storage.Layout.init(allocator, options.storage_path);
        defer layout.deinit();
        break :blk try storage.loadGraph(allocator, io, layout);
    };
    defer graph.deinit(allocator);

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

    return .{
        .node_count = graph.nodeCount(),
        .edge_count = graph.edgeCount(),
        .file_count = file_count,
        .function_count = function_count,
        .type_count = type_count,
        .other_count = other_count,
        .source_hash = source_hash.computeRuntimeSourceHash(&graph),
    };
}
