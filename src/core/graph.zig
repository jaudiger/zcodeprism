const std = @import("std");
const types = @import("types.zig");
const node_mod = @import("node.zig");
const edge_mod = @import("edge.zig");
const adjacency_mod = @import("adjacency.zig");
const lang = @import("../languages/language.zig");

const NodeId = types.NodeId;
const EdgeId = types.EdgeId;
const NodeKind = types.NodeKind;
const Language = types.Language;
const Visibility = types.Visibility;
const Node = node_mod.Node;
const Edge = edge_mod.Edge;
const EdgeKey = edge_mod.EdgeKey;
const Adjacency = adjacency_mod.Adjacency;
const Direction = types.Direction;

/// Type-erased buffer entry that preserves the original allocation alignment.
/// Captures pointer, byte length, and alignment from any typed slice.
pub const OwnedBuffer = struct {
    ptr: [*]u8,
    len: usize,
    alignment: std.mem.Alignment,

    pub fn fromSlice(comptime T: type, buf: []const T) OwnedBuffer {
        return .{
            .ptr = @ptrCast(@constCast(buf.ptr)),
            .len = buf.len * @sizeOf(T),
            .alignment = comptime std.mem.Alignment.of(T),
        };
    }
};

/// The core code graph: a mutable collection of semantic nodes and edges.
/// Build phase: populate with addNode/addEdgeIfNew.
/// Query phase: call freeze() once, then use getNode, getChildren,
/// outEdges, inEdges, neighbors.
pub const Graph = struct {
    nodes: std.ArrayList(Node),
    edges: std.ArrayList(Edge),
    project_root: []const u8,
    // Pre-computed CSR adjacency index. Built by freeze(), enables O(1) lookups.
    adjacency: ?Adjacency = null,
    // Tracks allocated buffers (source files, duped strings) that node slices point into.
    // Freed on deinit; node name/doc/signature slices borrow into these.
    owned_buffers: std.ArrayList(OwnedBuffer),
    // Hash index for edge deduplication. Maps (source, target, type) to void.
    edge_index: std.AutoHashMapUnmanaged(EdgeKey, void),

    /// Create an empty graph rooted at the given project directory.
    /// The caller owns the returned Graph and must call deinit() when done.
    /// `project_root` is borrowed, not duped -- it must outlive the Graph.
    pub fn init(project_root: []const u8) Graph {
        return .{
            .nodes = .{},
            .edges = .{},
            .project_root = project_root,
            .owned_buffers = .{},
            .edge_index = .{},
        };
    }

    /// Release all memory owned by the graph.
    /// Frees owned buffers, node/edge arrays, the adjacency index, and
    /// the edge dedup map. The Graph is left in an undefined state.
    /// `allocator` must be the same allocator used for all prior mutations.
    pub fn deinit(self: *Graph, allocator: std.mem.Allocator) void {
        // Free owned buffers first -- node name/doc/signature slices
        // may point into these, so they must outlive the node array.
        for (self.owned_buffers.items) |ob| {
            allocator.rawFree(ob.ptr[0..ob.len], ob.alignment, @returnAddress());
        }
        self.owned_buffers.deinit(allocator);
        self.nodes.deinit(allocator);
        self.edges.deinit(allocator);
        // Adjacency is optional; only present after freeze().
        if (self.adjacency) |*adj| adj.deinit(allocator);
        self.edge_index.deinit(allocator);
        self.* = undefined;
    }

    /// Register an allocator-owned slice to be freed on deinit().
    /// Accepts any slice type; the element alignment is captured at comptime
    /// so rawFree receives the correct alignment on deallocation.
    /// Each buffer must be added exactly once; adding the same pointer twice
    /// causes a double-free.
    pub fn addOwnedSlice(self: *Graph, allocator: std.mem.Allocator, comptime T: type, buf: []const T) !void {
        const ob = OwnedBuffer.fromSlice(T, buf);
        if (ob.len == 0) return;
        if (std.debug.runtime_safety) {
            for (self.owned_buffers.items) |existing| {
                std.debug.assert(existing.ptr != ob.ptr);
            }
        }
        try self.owned_buffers.append(allocator, ob);
    }

    /// Convenience wrapper for the common case of registering a []const u8.
    pub fn addOwnedBuffer(self: *Graph, allocator: std.mem.Allocator, buf: []const u8) !void {
        return self.addOwnedSlice(allocator, u8, buf);
    }

    /// Move all owned buffers from `source` into this graph. After the call,
    /// `source` no longer owns those buffers and must not free them.
    pub fn takeOwnedBuffers(self: *Graph, allocator: std.mem.Allocator, source: *Graph) !void {
        for (source.owned_buffers.items) |ob| {
            try self.owned_buffers.append(allocator, ob);
        }
        source.owned_buffers.clearRetainingCapacity();
    }

    /// Append a node and return its assigned NodeId. Overwrites the node's
    /// `id` field. Multi-line signatures are collapsed to a single line.
    pub fn addNode(self: *Graph, allocator: std.mem.Allocator, node: Node) !NodeId {
        const id: NodeId = @enumFromInt(self.nodes.items.len);
        var stored = node;
        stored.id = id;
        if (stored.signature) |sig| {
            if (std.mem.indexOfAny(u8, sig, "\n\r") != null) {
                const normalized = try collapseWhitespace(allocator, sig);
                self.addOwnedBuffer(allocator, normalized) catch |err| {
                    allocator.free(normalized);
                    return err;
                };
                stored.signature = normalized;
            }
        }
        try self.nodes.append(allocator, stored);
        return id;
    }

    /// Add an edge only if no edge with the same (source, target, type) triple
    /// already exists. Self-loops (source == target) are silently rejected.
    /// Returns true if the edge was inserted, false if it was a duplicate
    /// or a self-loop. Returns `error.OutOfMemory` on allocation failure.
    pub fn addEdgeIfNew(self: *Graph, allocator: std.mem.Allocator, edge: Edge) !bool {
        if (edge.source_id == edge.target_id) return false;
        const k = edge.key();
        const gop = try self.edge_index.getOrPut(allocator, k);
        if (gop.found_existing) return false;
        errdefer self.edge_index.removeByPtr(gop.key_ptr);
        try self.edges.append(allocator, edge);
        return true;
    }

    /// Rebuild the edge dedup index from the current edges list.
    /// Call this after bulk-loading edges (from storage deserialization or
    /// similar paths) that bypassed addEdgeIfNew. Clears the existing index and
    /// repopulates it in one pass. Returns `error.OutOfMemory` on failure.
    pub fn rebuildEdgeIndex(self: *Graph, allocator: std.mem.Allocator) !void {
        self.edge_index.clearRetainingCapacity();
        try self.edge_index.ensureTotalCapacity(allocator, @intCast(self.edges.items.len));
        for (self.edges.items) |e| {
            self.edge_index.putAssumeCapacity(e.key(), {});
        }
    }

    /// Pre-allocate capacity for nodes, edges, owned buffers, and the edge
    /// dedup index. Avoids incremental reallocation during bulk insertion.
    pub fn ensureCapacity(
        self: *Graph,
        allocator: std.mem.Allocator,
        est_nodes: u32,
        est_edges: u32,
        est_buffers: u32,
    ) !void {
        try self.nodes.ensureTotalCapacity(allocator, est_nodes);
        try self.edges.ensureTotalCapacity(allocator, est_edges);
        try self.edge_index.ensureTotalCapacity(allocator, est_edges);
        try self.owned_buffers.ensureTotalCapacity(allocator, est_buffers);
    }

    /// Build the pre-computed CSR adjacency index from current nodes and edges.
    /// Must be called after the mutation phase (addNode/addEdgeIfNew) is complete
    /// and before any query that depends on adjacency (getChildren, outEdges,
    /// inEdges, neighbors). Safe to call multiple times; each call frees the
    /// previous index and rebuilds from scratch.
    /// Returns a FrozenGraph view guaranteeing adjacency is present.
    pub fn freeze(self: *Graph, allocator: std.mem.Allocator) !FrozenGraph {
        // Free the old index if re-freezing after incremental mutations.
        if (self.adjacency) |*adj| adj.deinit(allocator);
        self.adjacency = try adjacency_mod.buildAdjacency(
            allocator,
            self.nodes.items,
            self.edges.items,
        );
        return .{ .graph = self };
    }

    /// Return a frozen view of this graph. The graph must already be frozen
    /// (adjacency built). Panics in debug builds if adjacency is absent.
    pub fn asFrozen(self: *const Graph) FrozenGraph {
        std.debug.assert(self.adjacency != null);
        return .{ .graph = self };
    }

    /// Look up a node by its id.
    /// Returns a const pointer into the backing array, or null if the id
    /// is out of range. The pointer is invalidated by any subsequent
    /// addNode call that triggers reallocation -- do not hold it across
    /// mutations.
    pub fn getNode(self: *const Graph, id: NodeId) ?*const Node {
        const index = @intFromEnum(id);
        if (index >= self.nodes.items.len) return null;
        return &self.nodes.items[index];
    }

    /// Return the direct children of a node (nodes whose parent_id equals `parent_id`).
    /// Requires freeze() to have been called; returns an empty slice if the
    /// adjacency index has not been built. The returned slice borrows into
    /// the adjacency's flat array and is valid until the next freeze() or deinit().
    pub fn getChildren(self: *const Graph, parent_id: NodeId) []const NodeId {
        const adj = self.adjacency orelse return &.{};
        return adj.childrenOf(parent_id);
    }

    /// Return the parent_id of a node.
    /// Returns null in two cases: the node has no parent (it is a root-level
    /// declaration or a file node), or `node_id` is out of range. Callers
    /// that need to distinguish "no parent" from "not found" should check
    /// getNode() first.
    pub fn getParent(self: *const Graph, node_id: NodeId) ?NodeId {
        const index = @intFromEnum(node_id);
        if (index >= self.nodes.items.len) return null;
        return self.nodes.items[index].parent_id;
    }

    /// Return outgoing edge ids from `node_id` (edges where node_id is the source).
    /// Requires freeze(); returns an empty slice if the adjacency index is absent.
    /// The returned slice borrows into the adjacency and is valid until
    /// the next freeze() or deinit().
    pub fn outEdges(self: *const Graph, node_id: NodeId) []const EdgeId {
        const adj = self.adjacency orelse return &.{};
        return adj.outEdges(node_id);
    }

    /// Return incoming edge ids to `node_id` (edges where node_id is the target).
    /// Requires freeze(); returns an empty slice if the adjacency index is absent.
    /// The returned slice borrows into the adjacency and is valid until
    /// the next freeze() or deinit().
    pub fn inEdges(self: *const Graph, node_id: NodeId) []const EdgeId {
        const adj = self.adjacency orelse return &.{};
        return adj.inEdges(node_id);
    }

    /// Collect neighbor node ids reachable from `node_id` in the given direction.
    /// `.out` yields targets of outgoing edges, `.in` yields sources of incoming
    /// edges, `.both` yields the union. Requires freeze(); returns an empty
    /// (but allocated) slice if the adjacency index is absent.
    /// The caller owns the returned slice and must free it with `allocator.free()`,
    /// even when the slice is empty. Uses the MAF pattern internally.
    pub fn neighbors(self: *const Graph, allocator: std.mem.Allocator, node_id: NodeId, direction: Direction) ![]NodeId {
        const adj = self.adjacency orelse return try allocator.alloc(NodeId, 0);

        // Measure
        var count: usize = 0;
        switch (direction) {
            .out => count = adj.outEdges(node_id).len,
            .in => count = adj.inEdges(node_id).len,
            .both => count = adj.outEdges(node_id).len + adj.inEdges(node_id).len,
        }

        // Allocate
        const result = try allocator.alloc(NodeId, count);
        errdefer allocator.free(result);

        // Fill
        var pos: usize = 0;
        if (direction == .out or direction == .both) {
            for (adj.outEdges(node_id)) |eid| {
                result[pos] = self.edges.items[@intFromEnum(eid)].target_id;
                pos += 1;
            }
        }
        if (direction == .in or direction == .both) {
            for (adj.inEdges(node_id)) |eid| {
                result[pos] = self.edges.items[@intFromEnum(eid)].source_id;
                pos += 1;
            }
        }
        std.debug.assert(pos == count);
        return result;
    }

    /// Walk the parent chain from node_id to find the containing file node.
    pub fn findContainingFile(self: *const Graph, node_id: NodeId) ?NodeId {
        var current = node_id;
        var hops: usize = 0;
        while (hops < 100) : (hops += 1) {
            const node = self.getNode(current) orelse return null;
            if (node.kind == .file) return current;
            current = node.parent_id orelse return null;
        }
        return null;
    }

    /// Find a type container child among the given child indices matching name.
    /// The children slice is typically from ScopeIndex.childrenOf().
    pub fn findTypeAmongChildren(self: *const Graph, children: []const u64, type_name: []const u8) ?NodeId {
        for (children) |child_idx| {
            const n = self.nodes.items[child_idx];
            if (!n.kind.isTypeContainer()) continue;
            if (std.mem.eql(u8, n.name, type_name)) return @enumFromInt(child_idx);
        }
        return null;
    }

    /// Return the total number of nodes in the graph.
    pub fn nodeCount(self: *const Graph) usize {
        return self.nodes.items.len;
    }

    /// Return the total number of edges in the graph.
    pub fn edgeCount(self: *const Graph) usize {
        return self.edges.items.len;
    }
};

/// Read-only view of a frozen Graph with adjacency guaranteed present.
/// Obtained from Graph.freeze() or Graph.asFrozen(). Borrows the Graph;
/// the Graph must outlive this view.
pub const FrozenGraph = struct {
    graph: *const Graph,

    pub fn getNode(self: FrozenGraph, id: NodeId) ?*const Node {
        return self.graph.getNode(id);
    }

    pub fn getChildren(self: FrozenGraph, parent_id: NodeId) []const NodeId {
        return self.graph.adjacency.?.childrenOf(parent_id);
    }

    pub fn getParent(self: FrozenGraph, node_id: NodeId) ?NodeId {
        return self.graph.getParent(node_id);
    }

    pub fn outEdges(self: FrozenGraph, node_id: NodeId) []const EdgeId {
        return self.graph.adjacency.?.outEdges(node_id);
    }

    pub fn inEdges(self: FrozenGraph, node_id: NodeId) []const EdgeId {
        return self.graph.adjacency.?.inEdges(node_id);
    }

    pub fn neighbors(self: FrozenGraph, allocator: std.mem.Allocator, node_id: NodeId, direction: Direction) ![]NodeId {
        const adj = self.graph.adjacency.?;
        const g = self.graph;

        // Measure
        var count: usize = 0;
        switch (direction) {
            .out => count = adj.outEdges(node_id).len,
            .in => count = adj.inEdges(node_id).len,
            .both => count = adj.outEdges(node_id).len + adj.inEdges(node_id).len,
        }

        // Allocate
        const result = try allocator.alloc(NodeId, count);
        errdefer allocator.free(result);

        // Fill
        var pos: usize = 0;
        if (direction == .out or direction == .both) {
            for (adj.outEdges(node_id)) |eid| {
                result[pos] = g.edges.items[@intFromEnum(eid)].target_id;
                pos += 1;
            }
        }
        if (direction == .in or direction == .both) {
            for (adj.inEdges(node_id)) |eid| {
                result[pos] = g.edges.items[@intFromEnum(eid)].source_id;
                pos += 1;
            }
        }
        std.debug.assert(pos == count);
        return result;
    }

    pub fn findContainingFile(self: FrozenGraph, node_id: NodeId) ?NodeId {
        return self.graph.findContainingFile(node_id);
    }

    pub fn findTypeAmongChildren(self: FrozenGraph, children: []const u64, type_name: []const u8) ?NodeId {
        return self.graph.findTypeAmongChildren(children, type_name);
    }

    pub fn nodeCount(self: FrozenGraph) usize {
        return self.graph.nodes.items.len;
    }

    pub fn edgeCount(self: FrozenGraph) usize {
        return self.graph.edges.items.len;
    }
};

/// Collapse runs of whitespace (spaces, tabs, newlines) into single spaces,
/// trimming any trailing space. The caller owns the returned buffer.
fn collapseWhitespace(allocator: std.mem.Allocator, input: []const u8) ![]const u8 {
    // Measure
    var len: usize = 0;
    var in_ws = false;
    for (input) |c| {
        if (c == ' ' or c == '\t' or c == '\n' or c == '\r') {
            if (!in_ws) {
                len += 1;
                in_ws = true;
            }
        } else {
            len += 1;
            in_ws = false;
        }
    }
    if (len > 0 and in_ws) len -= 1;

    // Allocate
    const buf = try allocator.alloc(u8, len);

    // Fill
    var pos: usize = 0;
    in_ws = false;
    for (input) |c| {
        if (c == ' ' or c == '\t' or c == '\n' or c == '\r') {
            if (!in_ws) {
                buf[pos] = ' ';
                pos += 1;
                in_ws = true;
            }
        } else {
            buf[pos] = c;
            pos += 1;
            in_ws = false;
        }
    }
    if (pos > 0 and buf[pos - 1] == ' ') pos -= 1;
    std.debug.assert(pos == len);

    return buf[0..len];
}

// Nominal tests (fail: NotImplemented stubs)

test "addNode returns sequential ids" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    const n1 = Node{ .id = .root, .name = "a", .kind = .function, .language = .zig };
    const n2 = Node{ .id = .root, .name = "b", .kind = .function, .language = .zig };
    const n3 = Node{ .id = .root, .name = "c", .kind = .function, .language = .zig };

    // Act
    const id1 = try g.addNode(std.testing.allocator, n1);
    const id2 = try g.addNode(std.testing.allocator, n2);
    const id3 = try g.addNode(std.testing.allocator, n3);

    // Assert
    try std.testing.expectEqual(@as(u64, 0), @intFromEnum(id1));
    try std.testing.expectEqual(@as(u64, 1), @intFromEnum(id2));
    try std.testing.expectEqual(@as(u64, 2), @intFromEnum(id3));
}

test "addEdgeIfNew creates edge between existing nodes" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    const n1 = Node{ .id = .root, .name = "a", .kind = .function, .language = .zig };
    const n2 = Node{ .id = .root, .name = "b", .kind = .function, .language = .zig };
    const id1 = try g.addNode(std.testing.allocator, n1);
    const id2 = try g.addNode(std.testing.allocator, n2);

    // Act
    const inserted = try g.addEdgeIfNew(std.testing.allocator, .{
        .source_id = id1,
        .target_id = id2,
        .edge_type = .calls,
    });

    // Assert
    try std.testing.expect(inserted);
    try std.testing.expectEqual(@as(usize, 1), g.edgeCount());
}

test "getNode returns the added node" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    const n = Node{ .id = .root, .name = "foo", .kind = .function, .language = .zig };
    const id = try g.addNode(std.testing.allocator, n);

    // Act
    const result = g.getNode(id);

    // Assert
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("foo", result.?.name);
}

test "getChildren returns direct children" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    const parent_id = try g.addNode(std.testing.allocator, .{ .id = .root, .name = "parent", .kind = .type_def, .language = .zig });
    _ = try g.addNode(std.testing.allocator, .{ .id = .root, .name = "child1", .kind = .function, .language = .zig, .parent_id = parent_id });
    _ = try g.addNode(std.testing.allocator, .{ .id = .root, .name = "child2", .kind = .function, .language = .zig, .parent_id = parent_id });
    const fg = try g.freeze(std.testing.allocator);

    // Act
    const children = fg.getChildren(parent_id);

    // Assert
    try std.testing.expectEqual(@as(usize, 2), children.len);
}

test "getParent returns parent node" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    const parent_id = try g.addNode(std.testing.allocator, .{ .id = .root, .name = "parent", .kind = .file, .language = .zig });
    const child_id = try g.addNode(std.testing.allocator, .{ .id = .root, .name = "child", .kind = .function, .language = .zig, .parent_id = parent_id });

    // Act
    const result = g.getParent(child_id);

    // Assert
    try std.testing.expect(result != null);
    try std.testing.expectEqual(parent_id, result.?);
}

test "neighbors out returns outgoing edges" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    const a = try g.addNode(std.testing.allocator, .{ .id = .root, .name = "a", .kind = .function, .language = .zig });
    const b = try g.addNode(std.testing.allocator, .{ .id = .root, .name = "b", .kind = .function, .language = .zig });
    const c = try g.addNode(std.testing.allocator, .{ .id = .root, .name = "c", .kind = .function, .language = .zig });
    _ = try g.addEdgeIfNew(std.testing.allocator, .{ .source_id = a, .target_id = b, .edge_type = .calls });
    _ = try g.addEdgeIfNew(std.testing.allocator, .{ .source_id = a, .target_id = c, .edge_type = .calls });
    const fg = try g.freeze(std.testing.allocator);

    // Act
    const result = try fg.neighbors(std.testing.allocator, a, .out);
    defer std.testing.allocator.free(result);

    // Assert
    try std.testing.expectEqual(@as(usize, 2), result.len);
}

test "neighbors in returns incoming edges" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    const a = try g.addNode(std.testing.allocator, .{ .id = .root, .name = "a", .kind = .function, .language = .zig });
    const b = try g.addNode(std.testing.allocator, .{ .id = .root, .name = "b", .kind = .function, .language = .zig });
    const c = try g.addNode(std.testing.allocator, .{ .id = .root, .name = "c", .kind = .function, .language = .zig });
    _ = try g.addEdgeIfNew(std.testing.allocator, .{ .source_id = b, .target_id = a, .edge_type = .calls });
    _ = try g.addEdgeIfNew(std.testing.allocator, .{ .source_id = c, .target_id = a, .edge_type = .calls });
    const fg = try g.freeze(std.testing.allocator);

    // Act
    const result = try fg.neighbors(std.testing.allocator, a, .in);
    defer std.testing.allocator.free(result);

    // Assert
    try std.testing.expectEqual(@as(usize, 2), result.len);
}

test "neighbors both returns all edges" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    const a = try g.addNode(std.testing.allocator, .{ .id = .root, .name = "a", .kind = .function, .language = .zig });
    const b = try g.addNode(std.testing.allocator, .{ .id = .root, .name = "b", .kind = .function, .language = .zig });
    const c = try g.addNode(std.testing.allocator, .{ .id = .root, .name = "c", .kind = .function, .language = .zig });
    _ = try g.addEdgeIfNew(std.testing.allocator, .{ .source_id = a, .target_id = b, .edge_type = .calls });
    _ = try g.addEdgeIfNew(std.testing.allocator, .{ .source_id = c, .target_id = a, .edge_type = .calls });
    const fg = try g.freeze(std.testing.allocator);

    // Act
    const result = try fg.neighbors(std.testing.allocator, a, .both);
    defer std.testing.allocator.free(result);

    // Assert
    try std.testing.expectEqual(@as(usize, 2), result.len);
}

// Empty/zero tests (pass: stubs return null/empty)

test "empty graph has zero nodes and edges" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Assert
    try std.testing.expectEqual(@as(usize, 0), g.nodeCount());
    try std.testing.expectEqual(@as(usize, 0), g.edgeCount());
}

test "getNode returns null for non-existent id" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act / Assert: .root on empty graph
    try std.testing.expectEqual(@as(?*const Node, null), g.getNode(.root));
    // Act / Assert: arbitrary non-existent id
    try std.testing.expectEqual(@as(?*const Node, null), g.getNode(@enumFromInt(99)));
}

test "getChildren returns empty for missing parent" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);
    const fg = try g.freeze(std.testing.allocator);

    // Act / Assert: .root on empty graph
    try std.testing.expectEqual(@as(usize, 0), fg.getChildren(.root).len);
    // Act / Assert: non-existent parent id
    try std.testing.expectEqual(@as(usize, 0), fg.getChildren(@enumFromInt(99)).len);
}

test "neighbors on empty graph returns empty" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);
    const fg = try g.freeze(std.testing.allocator);

    // Act
    const result = try fg.neighbors(std.testing.allocator, .root, .both);
    defer std.testing.allocator.free(result);

    // Assert
    try std.testing.expectEqual(@as(usize, 0), result.len);
}

// Error/boundary tests

test "getParent on root node returns null" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act: root node has no parent
    const result = g.getParent(.root);

    // Assert
    try std.testing.expectEqual(@as(?NodeId, null), result);
}

test "node with all optional fields null" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    const n = Node{
        .id = .root,
        .name = "bare",
        .kind = .function,
        .language = .zig,
        // All optional fields use their default null values
    };

    // Act
    const id = try g.addNode(std.testing.allocator, n);
    const result = g.getNode(id);

    // Assert
    try std.testing.expect(result != null);
    try std.testing.expectEqual(@as(?[]const u8, null), result.?.doc);
    try std.testing.expectEqual(@as(?[]const u8, null), result.?.signature);
    try std.testing.expectEqual(@as(?[12]u8, null), result.?.content_hash);
    try std.testing.expectEqual(lang.LangMeta.none, result.?.lang_meta);
}

// Optional fields via graph (fail: needs addNode)

test "node stores doc and signature via graph" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    const id = try g.addNode(std.testing.allocator, .{
        .id = .root,
        .name = "documented",
        .kind = .function,
        .language = .zig,
        .doc = "/// My doc",
        .signature = "pub fn documented() void",
    });

    // Act
    const result = g.getNode(id);

    // Assert
    try std.testing.expect(result != null);
    try std.testing.expect(result.?.doc != null);
    try std.testing.expectEqualStrings("/// My doc", result.?.doc.?);
    try std.testing.expect(result.?.signature != null);
    try std.testing.expectEqualStrings("pub fn documented() void", result.?.signature.?);
}

test "addNode normalizes multi-line signature to single line" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    const id = try g.addNode(std.testing.allocator, .{
        .id = .root,
        .name = "parse",
        .kind = .function,
        .language = .rust,
        .signature = "pub fn parse(\n    &self,\n    instructions: impl Into<String>,\n) -> Result<Vec<LexerToken>, LexerError>",
    });

    // Act
    const result = g.getNode(id);

    // Assert
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings(
        "pub fn parse( &self, instructions: impl Into<String>, ) -> Result<Vec<LexerToken>, LexerError>",
        result.?.signature.?,
    );
}

test "addNode preserves single-line signature unchanged" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    const sig = "pub fn foo(a: u32, b: u32) void";
    const id = try g.addNode(std.testing.allocator, .{
        .id = .root,
        .name = "foo",
        .kind = .function,
        .language = .zig,
        .signature = sig,
    });

    // Act
    const result = g.getNode(id);

    // Assert: same pointer, no allocation
    try std.testing.expectEqual(sig.ptr, result.?.signature.?.ptr);
}

test "addNode normalizes signature with mixed whitespace" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    const id = try g.addNode(std.testing.allocator, .{
        .id = .root,
        .name = "new",
        .kind = .function,
        .language = .rust,
        .signature = "pub const fn new(\r\n\ttoken_mode: LexerTokenMode,\r\n\tboundness_mode: ParserBoundnessMode,\r\n) -> Self",
    });

    // Act
    const result = g.getNode(id);

    // Assert
    try std.testing.expectEqualStrings(
        "pub const fn new( token_mode: LexerTokenMode, boundness_mode: ParserBoundnessMode, ) -> Self",
        result.?.signature.?,
    );
}
