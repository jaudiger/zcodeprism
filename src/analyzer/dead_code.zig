const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const types = @import("../core/types.zig");
const node_mod = @import("../core/node.zig");
const scope_mod = @import("../core/scope.zig");
const external_mod = @import("../core/external.zig");
const rust_meta = @import("../languages/rust/meta.zig");
const filter = @import("filter.zig");
const pagination = @import("pagination.zig");

const Graph = graph_mod.Graph;
const FrozenGraph = graph_mod.FrozenGraph;
const Node = node_mod.Node;
const NodeId = types.NodeId;
const EdgeId = types.EdgeId;
const NodeKind = types.NodeKind;
const Visibility = types.Visibility;
const Language = types.Language;
const ExternalInfo = external_mod.ExternalInfo;
const Scope = scope_mod.Scope;

pub const DeadCodeEntry = struct {
    node_id: NodeId,
    name: []const u8,
    kind: NodeKind,
    file_path: ?[]const u8,
    visibility: Visibility,
};

pub const DeadCodeResult = struct {
    total_count: u32,
    nodes: []const DeadCodeEntry,

    pub fn deinit(self: DeadCodeResult, allocator: std.mem.Allocator) void {
        allocator.free(self.nodes);
    }
};

pub const DeadCodeOptions = struct {
    include_public: bool = false,
    scope: ?[]const u8 = null,
    include_test_only: bool = false,
    kind: ?NodeKind = null,
    language: ?Language = null,
    offset: u32 = 0,
    limit: u32 = 50,
};

/// Find declaration nodes with zero incoming edges.
pub fn findDeadCode(allocator: std.mem.Allocator, fg: FrozenGraph, options: DeadCodeOptions) !DeadCodeResult {
    const g = fg.graph;
    const scope_filter: ?Scope = if (options.scope) |s| Scope.parse(s) else null;

    var candidates = std.ArrayList(DeadCodeEntry).empty;
    defer candidates.deinit(allocator);

    for (g.nodes.items, 0..) |n, i| {
        switch (n.kind) {
            .file, .module, .import_decl, .directory, .parameter => continue,
            .test_def => if (!options.include_test_only) continue,
            else => {},
        }

        if (options.kind) |kf| {
            if (n.kind != kf) continue;
        }

        if (options.language) |lf| {
            if (n.language == null or n.language.? != lf) continue;
        }

        // Impl blocks are organizational containers, not referenceable entities.
        if (rust_meta.metaOf(&n)) |m| if (m.sub_kind == .impl_block) continue;

        if (n.external != .none) continue;
        if (!options.include_public and n.visibility == .public) continue;

        if (!filter.passesScope(scope_filter, n.file_path)) continue;

        const node_id: NodeId = @enumFromInt(i);
        const in_edges = g.inEdges(node_id);
        var has_ref = false;

        for (in_edges) |eid| {
            const et = g.edges.items[@intFromEnum(eid)].edge_type;
            const is_ref = if (n.kind == .field)
                et == .accesses_field
            else
                et.isSemanticRef();
            if (is_ref) {
                has_ref = true;
                break;
            }
        }

        if (!has_ref) {
            try candidates.append(allocator, .{
                .node_id = node_id,
                .name = n.name,
                .kind = n.kind,
                .file_path = n.file_path,
                .visibility = n.visibility,
            });
        }
    }

    const total_count: u32 = @intCast(candidates.items.len);
    if (total_count == 0) return .{ .total_count = 0, .nodes = &.{} };

    const page = pagination.paginate(total_count, options.offset, options.limit);
    if (page.len == 0) return .{ .total_count = total_count, .nodes = &.{} };

    const result = try allocator.alloc(DeadCodeEntry, page.len);
    @memcpy(result, candidates.items[page.start .. page.start + page.len]);
    return .{ .total_count = total_count, .nodes = result };
}
