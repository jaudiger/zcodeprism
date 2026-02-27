const std = @import("std");
const types = @import("types.zig");
const node_mod = @import("node.zig");
const edge_mod = @import("edge.zig");

const NodeId = types.NodeId;
const EdgeId = types.EdgeId;
const Node = node_mod.Node;
const Edge = edge_mod.Edge;

/// Half-open range [offset .. offset+len) into a flat backing array.
///
/// Used by `Adjacency` to delimit per-node slices within the CSR storage
/// arrays without any per-node heap allocation.
pub const Span = struct {
    /// Index of the first element in the backing array.
    offset: u32,
    /// Number of elements in this span.
    len: u32,
};

/// Compressed Sparse Row (CSR) adjacency index for O(1) per-node edge and child lookups.
///
/// Built once from a frozen graph via `buildAdjacency`. Three independent
/// CSR structures share the same layout: one `Span` per node indexes into a
/// flat backing array of IDs. All slices are owned by this struct and freed
/// in `deinit`.
pub const Adjacency = struct {
    /// Per-node span into `out_edges` (outgoing edge IDs).
    out_spans: []Span,
    /// Flat storage of outgoing EdgeIds, indexed by `out_spans`.
    out_edges: []EdgeId,
    /// Per-node span into `in_edges` (incoming edge IDs).
    in_spans: []Span,
    /// Flat storage of incoming EdgeIds, indexed by `in_spans`.
    in_edges: []EdgeId,
    /// Per-node span into `children_storage` (direct child NodeIds).
    children_spans: []Span,
    /// Flat storage of child NodeIds, indexed by `children_spans`.
    children_storage: []NodeId,

    /// Returns the outgoing edge IDs for `node_id` (edges where this node is the source).
    ///
    /// Returns an empty slice when `node_id` is out of bounds.
    pub fn outEdges(self: *const Adjacency, node_id: NodeId) []const EdgeId {
        const idx = @intFromEnum(node_id);
        if (idx >= self.out_spans.len) return &.{};
        const span = self.out_spans[idx];
        return self.out_edges[span.offset..][0..span.len];
    }

    /// Returns the incoming edge IDs for `node_id` (edges where this node is the target).
    ///
    /// Returns an empty slice when `node_id` is out of bounds.
    pub fn inEdges(self: *const Adjacency, node_id: NodeId) []const EdgeId {
        const idx = @intFromEnum(node_id);
        if (idx >= self.in_spans.len) return &.{};
        const span = self.in_spans[idx];
        return self.in_edges[span.offset..][0..span.len];
    }

    /// Returns the direct children of `node_id` (nodes whose `parent_id` equals this node).
    ///
    /// Returns an empty slice when `node_id` is out of bounds.
    pub fn childrenOf(self: *const Adjacency, node_id: NodeId) []const NodeId {
        const idx = @intFromEnum(node_id);
        if (idx >= self.children_spans.len) return &.{};
        const span = self.children_spans[idx];
        return self.children_storage[span.offset..][0..span.len];
    }

    /// Frees all six backing slices and poisons the struct.
    ///
    /// `allocator` must be the same allocator passed to `buildAdjacency`.
    pub fn deinit(self: *Adjacency, allocator: std.mem.Allocator) void {
        allocator.free(self.out_spans);
        allocator.free(self.out_edges);
        allocator.free(self.in_spans);
        allocator.free(self.in_edges);
        allocator.free(self.children_spans);
        allocator.free(self.children_storage);
        self.* = undefined;
    }
};

/// Builds a CSR adjacency index from the graph's node and edge arrays.
///
/// Follows the MAF pattern (Measure / Allocate / Fill): counts per-node
/// degrees, allocates flat backing arrays in one shot, fills them via
/// prefix-sum offsets, and asserts fill completeness.
///
/// Edges that reference out-of-bounds node IDs are silently excluded from
/// the corresponding span (source side or target side, independently).
///
/// The caller owns the returned `Adjacency` and must call `deinit` with the
/// same `allocator` to release it.
pub fn buildAdjacency(
    allocator: std.mem.Allocator,
    nodes: []const Node,
    edges: []const Edge,
) !Adjacency {
    const node_count: u32 = @intCast(nodes.len);

    // Measure

    // Count outgoing/incoming edges per node and children per parent.
    const out_counts = try allocator.alloc(u32, node_count);
    defer allocator.free(out_counts);
    @memset(out_counts, 0);

    const in_counts = try allocator.alloc(u32, node_count);
    defer allocator.free(in_counts);
    @memset(in_counts, 0);

    const children_counts = try allocator.alloc(u32, node_count);
    defer allocator.free(children_counts);
    @memset(children_counts, 0);

    for (edges) |e| {
        const src: u32 = @intCast(@intFromEnum(e.source_id));
        const tgt: u32 = @intCast(@intFromEnum(e.target_id));
        if (src < node_count) out_counts[src] += 1;
        if (tgt < node_count) in_counts[tgt] += 1;
    }

    var total_out: u32 = 0;
    for (out_counts) |c| total_out += c;

    var total_in: u32 = 0;
    for (in_counts) |c| total_in += c;

    var total_children: u32 = 0;
    for (nodes) |n| {
        if (n.parent_id) |pid| {
            const pidx: u32 = @intCast(@intFromEnum(pid));
            if (pidx < node_count) {
                children_counts[pidx] += 1;
                total_children += 1;
            }
        }
    }

    // Allocate

    const out_spans = try allocator.alloc(Span, node_count);
    errdefer allocator.free(out_spans);

    const out_edges_arr = try allocator.alloc(EdgeId, total_out);
    errdefer allocator.free(out_edges_arr);

    const in_spans = try allocator.alloc(Span, node_count);
    errdefer allocator.free(in_spans);

    const in_edges_arr = try allocator.alloc(EdgeId, total_in);
    errdefer allocator.free(in_edges_arr);

    const children_spans = try allocator.alloc(Span, node_count);
    errdefer allocator.free(children_spans);

    const children_storage = try allocator.alloc(NodeId, total_children);
    errdefer allocator.free(children_storage);

    // After this point no more fallible allocations.
    errdefer comptime unreachable;

    // Fill: compute prefix-sum offsets

    // Out spans
    {
        var offset: u32 = 0;
        for (out_counts, 0..) |count, i| {
            out_spans[i] = .{ .offset = offset, .len = 0 };
            offset += count;
        }
        std.debug.assert(offset == total_out);
    }

    // In spans
    {
        var offset: u32 = 0;
        for (in_counts, 0..) |count, i| {
            in_spans[i] = .{ .offset = offset, .len = 0 };
            offset += count;
        }
        std.debug.assert(offset == total_in);
    }

    // Children spans
    {
        var offset: u32 = 0;
        for (children_counts, 0..) |count, i| {
            children_spans[i] = .{ .offset = offset, .len = 0 };
            offset += count;
        }
        std.debug.assert(offset == total_children);
    }

    // Fill: place edge/child IDs into their spans

    for (edges, 0..) |e, edge_idx| {
        const eid: EdgeId = @enumFromInt(edge_idx);

        // Outgoing
        const src: u32 = @intCast(@intFromEnum(e.source_id));
        if (src < node_count) {
            const span = &out_spans[src];
            out_edges_arr[span.offset + span.len] = eid;
            span.len += 1;
        }

        // Incoming
        const tgt: u32 = @intCast(@intFromEnum(e.target_id));
        if (tgt < node_count) {
            const span = &in_spans[tgt];
            in_edges_arr[span.offset + span.len] = eid;
            span.len += 1;
        }
    }

    // Children
    for (nodes, 0..) |n, i| {
        if (n.parent_id) |pid| {
            const pidx: u32 = @intCast(@intFromEnum(pid));
            if (pidx < node_count) {
                const span = &children_spans[pidx];
                children_storage[span.offset + span.len] = @enumFromInt(i);
                span.len += 1;
            }
        }
    }

    // Assert fill completeness

    for (out_spans, out_counts) |span, count| {
        std.debug.assert(span.len == count);
    }
    for (in_spans, in_counts) |span, count| {
        std.debug.assert(span.len == count);
    }
    for (children_spans, children_counts) |span, count| {
        std.debug.assert(span.len == count);
    }

    return .{
        .out_spans = out_spans,
        .out_edges = out_edges_arr,
        .in_spans = in_spans,
        .in_edges = in_edges_arr,
        .children_spans = children_spans,
        .children_storage = children_storage,
    };
}

test "empty graph produces empty adjacency" {
    // Arrange
    const nodes: []const Node = &.{};
    const edges: []const Edge = &.{};

    // Act
    var adj = try buildAdjacency(std.testing.allocator, nodes, edges);
    defer adj.deinit(std.testing.allocator);

    // Assert
    try std.testing.expectEqual(@as(usize, 0), adj.out_spans.len);
    try std.testing.expectEqual(@as(usize, 0), adj.in_spans.len);
    try std.testing.expectEqual(@as(usize, 0), adj.children_spans.len);
    try std.testing.expectEqual(@as(usize, 0), adj.out_edges.len);
    try std.testing.expectEqual(@as(usize, 0), adj.in_edges.len);
    try std.testing.expectEqual(@as(usize, 0), adj.children_storage.len);
}

test "single node no edges" {
    // Arrange
    const nodes: []const Node = &.{
        .{ .id = .root, .name = "a", .kind = .function, .language = .zig },
    };
    const edges: []const Edge = &.{};

    // Act
    var adj = try buildAdjacency(std.testing.allocator, nodes, edges);
    defer adj.deinit(std.testing.allocator);

    // Assert
    try std.testing.expectEqual(@as(usize, 1), adj.out_spans.len);
    try std.testing.expectEqual(@as(usize, 0), adj.outEdges(.root).len);
    try std.testing.expectEqual(@as(usize, 0), adj.inEdges(.root).len);
    try std.testing.expectEqual(@as(usize, 0), adj.childrenOf(.root).len);
}

test "outEdges returns correct edge ids" {
    // Arrange
    const nodes: []const Node = &.{
        .{ .id = @enumFromInt(0), .name = "a", .kind = .function, .language = .zig },
        .{ .id = @enumFromInt(1), .name = "b", .kind = .function, .language = .zig },
        .{ .id = @enumFromInt(2), .name = "c", .kind = .function, .language = .zig },
    };
    const edges: []const Edge = &.{
        .{ .source_id = @enumFromInt(0), .target_id = @enumFromInt(1), .edge_type = .calls },
        .{ .source_id = @enumFromInt(0), .target_id = @enumFromInt(2), .edge_type = .calls },
        .{ .source_id = @enumFromInt(1), .target_id = @enumFromInt(2), .edge_type = .calls },
    };

    // Act
    var adj = try buildAdjacency(std.testing.allocator, nodes, edges);
    defer adj.deinit(std.testing.allocator);

    // Assert
    const out_0 = adj.outEdges(@enumFromInt(0));
    try std.testing.expectEqual(@as(usize, 2), out_0.len);
    try std.testing.expectEqual(@as(u64, 0), @intFromEnum(out_0[0]));
    try std.testing.expectEqual(@as(u64, 1), @intFromEnum(out_0[1]));

    const out_1 = adj.outEdges(@enumFromInt(1));
    try std.testing.expectEqual(@as(usize, 1), out_1.len);
    try std.testing.expectEqual(@as(u64, 2), @intFromEnum(out_1[0]));

    const out_2 = adj.outEdges(@enumFromInt(2));
    try std.testing.expectEqual(@as(usize, 0), out_2.len);
}

test "inEdges returns correct edge ids" {
    // Arrange
    const nodes: []const Node = &.{
        .{ .id = @enumFromInt(0), .name = "a", .kind = .function, .language = .zig },
        .{ .id = @enumFromInt(1), .name = "b", .kind = .function, .language = .zig },
    };
    const edges: []const Edge = &.{
        .{ .source_id = @enumFromInt(0), .target_id = @enumFromInt(1), .edge_type = .calls },
        .{ .source_id = @enumFromInt(0), .target_id = @enumFromInt(1), .edge_type = .uses_type },
    };

    // Act
    var adj = try buildAdjacency(std.testing.allocator, nodes, edges);
    defer adj.deinit(std.testing.allocator);

    // Assert
    const in_0 = adj.inEdges(@enumFromInt(0));
    try std.testing.expectEqual(@as(usize, 0), in_0.len);

    const in_1 = adj.inEdges(@enumFromInt(1));
    try std.testing.expectEqual(@as(usize, 2), in_1.len);
    try std.testing.expectEqual(@as(u64, 0), @intFromEnum(in_1[0]));
    try std.testing.expectEqual(@as(u64, 1), @intFromEnum(in_1[1]));
}

test "childrenOf returns correct child ids" {
    // Arrange
    const parent: NodeId = @enumFromInt(0);
    const nodes: []const Node = &.{
        .{ .id = @enumFromInt(0), .name = "parent", .kind = .type_def, .language = .zig },
        .{ .id = @enumFromInt(1), .name = "child1", .kind = .function, .language = .zig, .parent_id = parent },
        .{ .id = @enumFromInt(2), .name = "child2", .kind = .function, .language = .zig, .parent_id = parent },
        .{ .id = @enumFromInt(3), .name = "other", .kind = .function, .language = .zig },
    };
    const edges: []const Edge = &.{};

    // Act
    var adj = try buildAdjacency(std.testing.allocator, nodes, edges);
    defer adj.deinit(std.testing.allocator);

    // Assert
    const children = adj.childrenOf(parent);
    try std.testing.expectEqual(@as(usize, 2), children.len);
    try std.testing.expectEqual(@as(u64, 1), @intFromEnum(children[0]));
    try std.testing.expectEqual(@as(u64, 2), @intFromEnum(children[1]));

    // Node 3 has no children
    try std.testing.expectEqual(@as(usize, 0), adj.childrenOf(@enumFromInt(3)).len);
}

test "out-of-bounds node id returns empty" {
    // Arrange
    const nodes: []const Node = &.{
        .{ .id = .root, .name = "a", .kind = .function, .language = .zig },
    };
    const edges: []const Edge = &.{};

    // Act
    var adj = try buildAdjacency(std.testing.allocator, nodes, edges);
    defer adj.deinit(std.testing.allocator);

    // Assert
    try std.testing.expectEqual(@as(usize, 0), adj.outEdges(@enumFromInt(99)).len);
    try std.testing.expectEqual(@as(usize, 0), adj.inEdges(@enumFromInt(99)).len);
    try std.testing.expectEqual(@as(usize, 0), adj.childrenOf(@enumFromInt(99)).len);
}

test "edges with OOB node ids are excluded from adjacency" {
    // Arrange: 2 nodes, but edge references node id 99 (out of bounds)
    const nodes: []const Node = &.{
        .{ .id = @enumFromInt(0), .name = "a", .kind = .function, .language = .zig },
        .{ .id = @enumFromInt(1), .name = "b", .kind = .function, .language = .zig },
    };
    const edges: []const Edge = &.{
        // Valid edge: 0 -> 1
        .{ .source_id = @enumFromInt(0), .target_id = @enumFromInt(1), .edge_type = .calls },
        // OOB source: 99 -> 1
        .{ .source_id = @enumFromInt(99), .target_id = @enumFromInt(1), .edge_type = .calls },
        // OOB target: 0 -> 99
        .{ .source_id = @enumFromInt(0), .target_id = @enumFromInt(99), .edge_type = .uses_type },
        // Both OOB: 50 -> 99
        .{ .source_id = @enumFromInt(50), .target_id = @enumFromInt(99), .edge_type = .imports },
    };

    // Act
    var adj = try buildAdjacency(std.testing.allocator, nodes, edges);
    defer adj.deinit(std.testing.allocator);

    // Assert: each endpoint is indexed independently.
    // Node 0 has 2 outgoing: edge 0 (0->1, valid) and edge 2 (0->99, OOB target).
    try std.testing.expectEqual(@as(usize, 2), adj.outEdges(@enumFromInt(0)).len);
    // Node 1 has 2 incoming: edge 0 (0->1, valid) and edge 1 (99->1, OOB source).
    try std.testing.expectEqual(@as(usize, 2), adj.inEdges(@enumFromInt(1)).len);

    // Flat arrays are sized to counted totals, not raw edge_count (4).
    // total_out = out_counts[0]+out_counts[1] = 2+0 = 2
    // total_in  = in_counts[0]+in_counts[1]  = 0+2 = 2
    try std.testing.expectEqual(@as(usize, 2), adj.out_edges.len);
    try std.testing.expectEqual(@as(usize, 2), adj.in_edges.len);
}

test "Span is 8 bytes" {
    comptime {
        std.debug.assert(@sizeOf(Span) == 8);
    }
}
