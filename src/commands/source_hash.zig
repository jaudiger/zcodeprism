const std = @import("std");
const types = @import("../core/types.zig");
const graph_mod = @import("../core/graph.zig");

const Graph = graph_mod.Graph;

/// Blake3 fingerprint of every file node's `content_hash`. Ignores file
/// paths. Distinct from `storage.snapshot.computeSourceHash`.
pub fn computeRuntimeSourceHash(graph: *const Graph) types.ContentHash {
    var hasher = std.crypto.hash.Blake3.init(.{});
    for (graph.nodes.items) |n| {
        if (n.kind == .file) {
            if (n.content_hash) |h| hasher.update(&h);
        }
    }
    var result: types.ContentHash = undefined;
    hasher.final(&result);
    return result;
}
