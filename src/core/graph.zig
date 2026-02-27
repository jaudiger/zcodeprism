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

/// Traversal direction for neighbor queries on the code graph.
pub const Direction = enum {
    /// Follow outgoing edges (source -> target).
    out,
    /// Follow incoming edges (target <- source).
    in,
    /// Follow edges in both directions.
    both,
};

/// The core code graph: a mutable collection of semantic nodes and edges.
/// Build phase: populate with addNode/addEdge/addEdgeIfNew.
/// Query phase: call freeze() once, then use getNode, getChildren,
/// outEdges, inEdges, neighbors.
pub const Graph = struct {
    allocator: std.mem.Allocator,
    nodes: std.ArrayListUnmanaged(Node),
    edges: std.ArrayListUnmanaged(Edge),
    project_root: []const u8,
    // Pre-computed CSR adjacency index. Built by freeze(), enables O(1) lookups.
    adjacency: ?Adjacency = null,
    // Tracks allocated buffers (source files, duped strings) that node slices point into.
    // Freed on deinit so that node name/doc/signature slices remain valid for the graph's lifetime.
    owned_buffers: std.ArrayListUnmanaged([]const u8),
    // Hash index for edge deduplication. Maps (source, target, type) to void.
    edge_index: std.AutoHashMapUnmanaged(EdgeKey, void),

    /// Create an empty graph rooted at the given project directory.
    /// The caller owns the returned Graph and must call deinit() when done.
    /// `project_root` is borrowed, not duped -- it must outlive the Graph.
    pub fn init(allocator: std.mem.Allocator, project_root: []const u8) Graph {
        return .{
            .allocator = allocator,
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
    pub fn deinit(self: *Graph) void {
        // Free owned buffers first -- node name/doc/signature slices
        // may point into these, so they must outlive the node array.
        for (self.owned_buffers.items) |buf| {
            self.allocator.free(buf);
        }
        self.owned_buffers.deinit(self.allocator);
        self.nodes.deinit(self.allocator);
        self.edges.deinit(self.allocator);
        // Adjacency is optional; only present after freeze().
        if (self.adjacency) |*adj| adj.deinit(self.allocator);
        self.edge_index.deinit(self.allocator);
        self.* = undefined;
    }

    /// Register an allocator-owned buffer to be freed on deinit().
    /// Use this for source file contents or duped strings that node
    /// name/doc/signature slices borrow into. Each buffer must be added
    /// exactly once; adding the same pointer twice causes a double-free.
    /// Returns `error.OutOfMemory` if the internal list cannot grow.
    pub fn addOwnedBuffer(self: *Graph, buf: []const u8) !void {
        if (std.debug.runtime_safety) {
            for (self.owned_buffers.items) |existing| {
                std.debug.assert(existing.ptr != buf.ptr);
            }
        }
        try self.owned_buffers.append(self.allocator, buf);
    }

    /// Append a node to the graph and return its sequentially assigned NodeId.
    /// The node's `id` field is overwritten with the assigned value.
    /// Returns `error.OutOfMemory` if the backing array cannot grow.
    pub fn addNode(self: *Graph, node: Node) !NodeId {
        const id: NodeId = @enumFromInt(self.nodes.items.len);
        var stored = node;
        stored.id = id;
        try self.nodes.append(self.allocator, stored);
        return id;
    }

    /// Append an edge unconditionally and return its assigned EdgeId.
    /// Also inserts the edge's (source, target, type) key into the
    /// dedup index. Does not check for duplicates -- use addEdgeIfNew()
    /// when deduplication is needed.
    /// Returns `error.OutOfMemory` if the backing array or hash map cannot grow.
    pub fn addEdge(self: *Graph, edge: Edge) !EdgeId {
        const id: EdgeId = @enumFromInt(self.edges.items.len);
        try self.edges.append(self.allocator, edge);
        errdefer _ = self.edges.pop();
        try self.edge_index.put(self.allocator, edge.key(), {});
        return id;
    }

    /// Add an edge only if no edge with the same (source, target, type) triple
    /// already exists. Self-loops (source == target) are silently rejected.
    /// Returns true if the edge was inserted, false if it was a duplicate
    /// or a self-loop. Returns `error.OutOfMemory` on allocation failure.
    pub fn addEdgeIfNew(self: *Graph, edge: Edge) !bool {
        if (edge.source_id == edge.target_id) return false;
        const k = edge.key();
        const gop = try self.edge_index.getOrPut(self.allocator, k);
        if (gop.found_existing) return false;
        errdefer self.edge_index.removeByPtr(gop.key_ptr);
        try self.edges.append(self.allocator, edge);
        return true;
    }

    /// Rebuild the edge dedup index from the current edges list.
    /// Call this after bulk-loading edges (e.g. from storage deserialization)
    /// that bypassed addEdge/addEdgeIfNew. Clears the existing index and
    /// repopulates it in one pass. Returns `error.OutOfMemory` on failure.
    pub fn rebuildEdgeIndex(self: *Graph) !void {
        self.edge_index.clearRetainingCapacity();
        try self.edge_index.ensureTotalCapacity(self.allocator, @intCast(self.edges.items.len));
        for (self.edges.items) |e| {
            self.edge_index.putAssumeCapacity(e.key(), {});
        }
    }

    /// Build the pre-computed CSR adjacency index from current nodes and edges.
    /// Must be called after the mutation phase (addNode/addEdge) is complete
    /// and before any query that depends on adjacency (getChildren, outEdges,
    /// inEdges, neighbors). Safe to call multiple times; each call frees the
    /// previous index and rebuilds from scratch.
    /// Returns `error.OutOfMemory` if the index cannot be allocated.
    pub fn freeze(self: *Graph) !void {
        // Free the old index if re-freezing after incremental mutations.
        if (self.adjacency) |*adj| adj.deinit(self.allocator);
        self.adjacency = try adjacency_mod.buildAdjacency(
            self.allocator,
            self.nodes.items,
            self.edges.items,
        );
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

    /// Return the total number of nodes in the graph.
    pub fn nodeCount(self: *const Graph) usize {
        return self.nodes.items.len;
    }

    /// Return the total number of edges in the graph.
    pub fn edgeCount(self: *const Graph) usize {
        return self.edges.items.len;
    }
};

// Nominal tests (fail: NotImplemented stubs)

test "addNode returns sequential ids" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    const n1 = Node{ .id = .root, .name = "a", .kind = .function, .language = .zig };
    const n2 = Node{ .id = .root, .name = "b", .kind = .function, .language = .zig };
    const n3 = Node{ .id = .root, .name = "c", .kind = .function, .language = .zig };

    // Act
    const id1 = try g.addNode(n1);
    const id2 = try g.addNode(n2);
    const id3 = try g.addNode(n3);

    // Assert
    try std.testing.expectEqual(@as(u64, 0), @intFromEnum(id1));
    try std.testing.expectEqual(@as(u64, 1), @intFromEnum(id2));
    try std.testing.expectEqual(@as(u64, 2), @intFromEnum(id3));
}

test "addEdge creates edge between existing nodes" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    const n1 = Node{ .id = .root, .name = "a", .kind = .function, .language = .zig };
    const n2 = Node{ .id = .root, .name = "b", .kind = .function, .language = .zig };
    const id1 = try g.addNode(n1);
    const id2 = try g.addNode(n2);

    // Act
    const edge_id = try g.addEdge(.{
        .source_id = id1,
        .target_id = id2,
        .edge_type = .calls,
    });

    // Assert
    try std.testing.expectEqual(@as(u64, 0), @intFromEnum(edge_id));
    try std.testing.expectEqual(@as(usize, 1), g.edgeCount());
}

test "getNode returns the added node" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    const n = Node{ .id = .root, .name = "foo", .kind = .function, .language = .zig };
    const id = try g.addNode(n);

    // Act
    const result = g.getNode(id);

    // Assert
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("foo", result.?.name);
}

test "getChildren returns direct children" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    const parent_id = try g.addNode(.{ .id = .root, .name = "parent", .kind = .type_def, .language = .zig });
    _ = try g.addNode(.{ .id = .root, .name = "child1", .kind = .function, .language = .zig, .parent_id = parent_id });
    _ = try g.addNode(.{ .id = .root, .name = "child2", .kind = .function, .language = .zig, .parent_id = parent_id });
    try g.freeze();

    // Act
    const children = g.getChildren(parent_id);

    // Assert
    try std.testing.expectEqual(@as(usize, 2), children.len);
}

test "getParent returns parent node" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    const parent_id = try g.addNode(.{ .id = .root, .name = "parent", .kind = .file, .language = .zig });
    const child_id = try g.addNode(.{ .id = .root, .name = "child", .kind = .function, .language = .zig, .parent_id = parent_id });

    // Act
    const result = g.getParent(child_id);

    // Assert
    try std.testing.expect(result != null);
    try std.testing.expectEqual(parent_id, result.?);
}

test "neighbors out returns outgoing edges" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    const a = try g.addNode(.{ .id = .root, .name = "a", .kind = .function, .language = .zig });
    const b = try g.addNode(.{ .id = .root, .name = "b", .kind = .function, .language = .zig });
    const c = try g.addNode(.{ .id = .root, .name = "c", .kind = .function, .language = .zig });
    _ = try g.addEdge(.{ .source_id = a, .target_id = b, .edge_type = .calls });
    _ = try g.addEdge(.{ .source_id = a, .target_id = c, .edge_type = .calls });
    try g.freeze();

    // Act
    const result = try g.neighbors(std.testing.allocator, a, .out);
    defer std.testing.allocator.free(result);

    // Assert
    try std.testing.expectEqual(@as(usize, 2), result.len);
}

test "neighbors in returns incoming edges" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    const a = try g.addNode(.{ .id = .root, .name = "a", .kind = .function, .language = .zig });
    const b = try g.addNode(.{ .id = .root, .name = "b", .kind = .function, .language = .zig });
    const c = try g.addNode(.{ .id = .root, .name = "c", .kind = .function, .language = .zig });
    _ = try g.addEdge(.{ .source_id = b, .target_id = a, .edge_type = .calls });
    _ = try g.addEdge(.{ .source_id = c, .target_id = a, .edge_type = .calls });
    try g.freeze();

    // Act
    const result = try g.neighbors(std.testing.allocator, a, .in);
    defer std.testing.allocator.free(result);

    // Assert
    try std.testing.expectEqual(@as(usize, 2), result.len);
}

test "neighbors both returns all edges" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    const a = try g.addNode(.{ .id = .root, .name = "a", .kind = .function, .language = .zig });
    const b = try g.addNode(.{ .id = .root, .name = "b", .kind = .function, .language = .zig });
    const c = try g.addNode(.{ .id = .root, .name = "c", .kind = .function, .language = .zig });
    _ = try g.addEdge(.{ .source_id = a, .target_id = b, .edge_type = .calls });
    _ = try g.addEdge(.{ .source_id = c, .target_id = a, .edge_type = .calls });
    try g.freeze();

    // Act
    const result = try g.neighbors(std.testing.allocator, a, .both);
    defer std.testing.allocator.free(result);

    // Assert
    try std.testing.expectEqual(@as(usize, 2), result.len);
}

// Empty/zero tests (pass: stubs return null/empty)

test "empty graph has zero nodes and edges" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Assert
    try std.testing.expectEqual(@as(usize, 0), g.nodeCount());
    try std.testing.expectEqual(@as(usize, 0), g.edgeCount());
}

test "getNode returns null for non-existent id" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act / Assert: .root on empty graph
    try std.testing.expectEqual(@as(?*const Node, null), g.getNode(.root));
    // Act / Assert: arbitrary non-existent id
    try std.testing.expectEqual(@as(?*const Node, null), g.getNode(@enumFromInt(99)));
}

test "getChildren returns empty for missing parent" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    try g.freeze();

    // Act / Assert: .root on empty graph
    try std.testing.expectEqual(@as(usize, 0), g.getChildren(.root).len);
    // Act / Assert: non-existent parent id
    try std.testing.expectEqual(@as(usize, 0), g.getChildren(@enumFromInt(99)).len);
}

test "neighbors on empty graph returns empty" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    try g.freeze();

    // Act
    const result = try g.neighbors(std.testing.allocator, .root, .both);
    defer std.testing.allocator.free(result);

    // Assert
    try std.testing.expectEqual(@as(usize, 0), result.len);
}

// Error/boundary tests

test "getParent on root node returns null" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act: root node has no parent
    const result = g.getParent(.root);

    // Assert
    try std.testing.expectEqual(@as(?NodeId, null), result);
}

test "node with all optional fields null" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    const n = Node{
        .id = .root,
        .name = "bare",
        .kind = .function,
        .language = .zig,
        // All optional fields use their default null values
    };

    // Act
    const id = try g.addNode(n);
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
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    const id = try g.addNode(.{
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
