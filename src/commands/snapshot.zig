const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const storage = @import("../storage/storage.zig");

const Graph = graph_mod.Graph;
const FrozenGraph = graph_mod.FrozenGraph;

/// Options for `snapshot`.
pub const Options = struct {
    /// Snapshot tag name. Validated by `storage.snapshot.validateTag`.
    tag: []const u8,
    /// Data directory containing the persisted graph and snapshots.
    storage_path: []const u8 = storage.data_dir,
};

/// Load the current graph and write it as a named snapshot under
/// `<storage_path>/snapshots/<tag>.bin`.
pub fn run(allocator: std.mem.Allocator, io: std.Io, options: Options) !void {
    var layout = try storage.Layout.init(allocator, options.storage_path);
    defer layout.deinit();
    var graph = try storage.loadGraph(allocator, io, layout);
    defer graph.deinit(allocator);

    const fg = FrozenGraph{ .graph = &graph };
    try storage.snapshot.saveSnapshot(allocator, io, fg, options.tag, options.storage_path);
}
