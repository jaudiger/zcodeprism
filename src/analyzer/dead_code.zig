const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const types = @import("../core/types.zig");
const node_mod = @import("../core/node.zig");
const scope_mod = @import("../core/scope.zig");

const Graph = graph_mod.Graph;
const Node = node_mod.Node;
const NodeId = types.NodeId;
const EdgeId = types.EdgeId;
const NodeKind = types.NodeKind;
const Visibility = types.Visibility;
const ExternalInfo = @import("../languages/language.zig").ExternalInfo;
const Scope = scope_mod.Scope;

pub const DeadCodeEntry = struct {
    node_id: NodeId,
    name: []const u8,
    kind: NodeKind,
    file_path: ?[]const u8,
    visibility: Visibility,
};

pub const DeadCodeResult = struct {
    nodes: []const DeadCodeEntry,

    pub fn deinit(self: DeadCodeResult, allocator: std.mem.Allocator) void {
        if (self.nodes.len > 0) allocator.free(self.nodes);
    }
};

pub const DeadCodeOptions = struct {
    include_public: bool = false,
    scope: ?[]const u8 = null,
};

/// Find declaration nodes with zero non-test incoming edges.
pub fn findDeadCode(allocator: std.mem.Allocator, g: *const Graph, options: DeadCodeOptions) !DeadCodeResult {
    const scope_filter: ?Scope = if (options.scope) |s| Scope.parse(s) else null;

    var candidates = std.ArrayList(DeadCodeEntry){};
    defer candidates.deinit(allocator);

    for (g.nodes.items, 0..) |n, i| {
        switch (n.kind) {
            .file, .module, .import_decl, .directory, .test_def => continue,
            else => {},
        }

        if (n.external != .none) continue;
        if (!options.include_public and n.visibility == .public) continue;

        if (scope_filter) |sf| {
            if (!sf.matches(n.file_path orelse continue)) continue;
        }

        const in_edges = g.inEdges(@enumFromInt(i));
        var has_non_test_ref = false;
        for (in_edges) |eid| {
            const edge = g.edges.items[@intFromEnum(eid)];
            const source_node = g.getNode(edge.source_id) orelse continue;
            if (source_node.kind != .test_def) {
                has_non_test_ref = true;
                break;
            }
        }

        if (!has_non_test_ref) {
            try candidates.append(allocator, .{
                .node_id = @enumFromInt(i),
                .name = n.name,
                .kind = n.kind,
                .file_path = n.file_path,
                .visibility = n.visibility,
            });
        }
    }

    if (candidates.items.len == 0) return .{ .nodes = &.{} };

    const result = try allocator.alloc(DeadCodeEntry, candidates.items.len);
    @memcpy(result, candidates.items);
    return .{ .nodes = result };
}
