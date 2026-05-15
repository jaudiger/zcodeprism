const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const storage = @import("../storage/storage.zig");

const Graph = graph_mod.Graph;
const FrozenGraph = graph_mod.FrozenGraph;

/// Options for `snapshot`.
pub const Options = struct {
    /// Snapshot tag name. Validated by `storage.snapshot.validateTag`.
    tag: []const u8,
};

/// Load the current graph from `.zcodeprism/graph.bin` and write it as a
/// named snapshot under `.zcodeprism/snapshots/<tag>.bin`.
pub fn run(allocator: std.mem.Allocator, io: std.Io, options: Options) !void {
    var graph = try storage.binary.load(allocator, io, storage.graph_binary_path);
    defer graph.deinit(allocator);

    const fg = FrozenGraph{ .graph = &graph };
    try storage.snapshot.saveSnapshot(allocator, io, fg, options.tag, storage.data_dir);
}
