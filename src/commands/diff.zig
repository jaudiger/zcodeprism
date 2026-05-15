const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const storage = @import("../storage/storage.zig");
const snapshot_diff = @import("../diff/snapshot_diff.zig");

const Graph = graph_mod.Graph;
const FrozenGraph = graph_mod.FrozenGraph;

/// Options for `diff`.
pub const Options = struct {
    tag_a: []const u8,
    tag_b: []const u8,
};

/// Load two snapshots, run the semantic diff, and write the rendered
/// report into `out`.
pub fn run(
    allocator: std.mem.Allocator,
    io: std.Io,
    options: Options,
    out: *std.ArrayList(u8),
) !void {
    var graph_a = try storage.snapshot.loadSnapshotGraph(allocator, io, options.tag_a, storage.data_dir);
    defer graph_a.deinit(allocator);

    var graph_b = try storage.snapshot.loadSnapshotGraph(allocator, io, options.tag_b, storage.data_dir);
    defer graph_b.deinit(allocator);

    const fg_a = FrozenGraph{ .graph = &graph_a };
    const fg_b = FrozenGraph{ .graph = &graph_b };
    var report = try snapshot_diff.diffGraphs(allocator, fg_a, fg_b);
    defer report.deinit(allocator);

    try snapshot_diff.renderDiffReport(allocator, &report, out);
}
