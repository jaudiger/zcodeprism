const std = @import("std");
const types = @import("types.zig");
const node_mod = @import("node.zig");
const edge_mod = @import("edge.zig");
const lang = @import("../languages/language.zig");

const NodeId = types.NodeId;
const EdgeId = types.EdgeId;
const NodeKind = types.NodeKind;
const Language = types.Language;
const Visibility = types.Visibility;
const Node = node_mod.Node;
const Edge = edge_mod.Edge;
const EdgeKey = edge_mod.EdgeKey;

/// Traversal direction for neighbor queries.
pub const Direction = enum {
    out,
    in,
    both,
};

/// The core code graph data structure.
/// Stores nodes (semantic code elements) and edges (relationships).
pub const Graph = struct {
    allocator: std.mem.Allocator,
    nodes: std.ArrayListUnmanaged(Node),
    edges: std.ArrayListUnmanaged(Edge),
    project_root: []const u8,
    // Temporary scratch buffer for getChildren/neighbors results.
    // Will be replaced by pre-computed adjacency once the build→freeze→query lifecycle exists.
    scratch_buf: std.ArrayListUnmanaged(NodeId),
    // Tracks allocated buffers (source files, duped strings) that node slices point into.
    // Freed on deinit so that node name/doc/signature slices remain valid for the graph's lifetime.
    owned_buffers: std.ArrayListUnmanaged([]const u8),
    // Hash index for edge deduplication. Maps (source, target, type) to void.
    edge_index: std.AutoHashMapUnmanaged(EdgeKey, void),

    pub fn init(allocator: std.mem.Allocator, project_root: []const u8) Graph {
        return .{
            .allocator = allocator,
            .nodes = .{},
            .edges = .{},
            .project_root = project_root,
            .scratch_buf = .{},
            .owned_buffers = .{},
            .edge_index = .{},
        };
    }

    pub fn deinit(self: *Graph) void {
        // Free owned buffers before nodes/edges. Node name/doc/signature slices may point into these.
        for (self.owned_buffers.items) |buf| {
            self.allocator.free(buf);
        }
        self.owned_buffers.deinit(self.allocator);
        self.nodes.deinit(self.allocator);
        self.edges.deinit(self.allocator);
        self.scratch_buf.deinit(self.allocator);
        self.edge_index.deinit(self.allocator);
    }

    /// Register a buffer to be freed when the graph is deinitialized.
    /// Use this for source file contents or duped strings that node slices point into.
    pub fn addOwnedBuffer(self: *Graph, buf: []const u8) !void {
        try self.owned_buffers.append(self.allocator, buf);
    }

    /// Add a node to the graph and return its assigned NodeId.
    pub fn addNode(self: *Graph, node: Node) !NodeId {
        const id: NodeId = @enumFromInt(self.nodes.items.len);
        var stored = node;
        stored.id = id;
        try self.nodes.append(self.allocator, stored);
        return id;
    }

    /// Add an edge to the graph and return its assigned EdgeId.
    /// Also updates the edge_index for deduplication tracking.
    pub fn addEdge(self: *Graph, edge: Edge) !EdgeId {
        const id: EdgeId = @enumFromInt(self.edges.items.len);
        try self.edges.append(self.allocator, edge);
        try self.edge_index.put(self.allocator, edge.key(), {});
        return id;
    }

    /// Add an edge only if no edge with the same (source, target, type) triple
    /// already exists. Returns true if the edge was added, false if duplicate.
    pub fn addEdgeIfNew(self: *Graph, edge: Edge) !bool {
        const k = edge.key();
        const gop = try self.edge_index.getOrPut(self.allocator, k);
        if (gop.found_existing) return false;
        try self.edges.append(self.allocator, edge);
        return true;
    }

    /// Rebuild the edge_index from the current edges list. Call this after
    /// bulk-loading edges (e.g. from storage) that bypassed addEdge.
    pub fn rebuildEdgeIndex(self: *Graph) !void {
        self.edge_index.clearRetainingCapacity();
        try self.edge_index.ensureTotalCapacity(self.allocator, @intCast(self.edges.items.len));
        for (self.edges.items) |e| {
            self.edge_index.putAssumeCapacity(e.key(), {});
        }
    }

    /// Get a node by its id, or null if not found.
    pub fn getNode(self: *const Graph, id: NodeId) ?*const Node {
        const index = @intFromEnum(id);
        if (index >= self.nodes.items.len) return null;
        return &self.nodes.items[index];
    }

    /// Get the direct children of a node (nodes whose parent_id equals the given id).
    /// Returns a slice backed by an internal scratch buffer. The caller must
    /// consume the result before calling `getChildren` or `neighbors` again.
    pub fn getChildren(self: *Graph, parent_id: NodeId) []const NodeId {
        // Measure
        var count: usize = 0;
        for (self.nodes.items) |n| {
            if (n.parent_id) |pid| {
                if (pid == parent_id) count += 1;
            }
        }
        if (count == 0) return &.{};

        // Allocate
        self.scratch_buf.clearRetainingCapacity();
        self.scratch_buf.ensureTotalCapacity(self.allocator, count) catch return &.{};

        // Fill
        for (self.nodes.items, 0..) |n, i| {
            if (n.parent_id) |pid| {
                if (pid == parent_id) {
                    self.scratch_buf.appendAssumeCapacity(@enumFromInt(i));
                }
            }
        }
        return self.scratch_buf.items;
    }

    /// Get the parent of a node, or null if the node has no parent or the node doesn't exist.
    pub fn getParent(self: *const Graph, node_id: NodeId) ?NodeId {
        const index = @intFromEnum(node_id);
        if (index >= self.nodes.items.len) return null;
        return self.nodes.items[index].parent_id;
    }

    /// Get neighbors of a node in the given direction.
    /// Returns a slice backed by an internal scratch buffer. The caller must
    /// consume the result before calling `getChildren` or `neighbors` again.
    pub fn neighbors(self: *Graph, node_id: NodeId, direction: Direction) []const NodeId {
        // Measure
        var count: usize = 0;
        for (self.edges.items) |e| {
            switch (direction) {
                .out => {
                    if (e.source_id == node_id) count += 1;
                },
                .in => {
                    if (e.target_id == node_id) count += 1;
                },
                .both => {
                    if (e.source_id == node_id) count += 1;
                    if (e.target_id == node_id) count += 1;
                },
            }
        }
        if (count == 0) return &.{};

        // Allocate
        self.scratch_buf.clearRetainingCapacity();
        self.scratch_buf.ensureTotalCapacity(self.allocator, count) catch return &.{};

        // Fill
        for (self.edges.items) |e| {
            switch (direction) {
                .out => {
                    if (e.source_id == node_id)
                        self.scratch_buf.appendAssumeCapacity(e.target_id);
                },
                .in => {
                    if (e.target_id == node_id)
                        self.scratch_buf.appendAssumeCapacity(e.source_id);
                },
                .both => {
                    if (e.source_id == node_id)
                        self.scratch_buf.appendAssumeCapacity(e.target_id);
                    if (e.target_id == node_id)
                        self.scratch_buf.appendAssumeCapacity(e.source_id);
                },
            }
        }
        return self.scratch_buf.items;
    }

    /// Number of nodes in the graph.
    pub fn nodeCount(self: *const Graph) usize {
        return self.nodes.items.len;
    }

    /// Number of edges in the graph.
    pub fn edgeCount(self: *const Graph) usize {
        return self.edges.items.len;
    }
};

// --- Tests ---

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

    // Act
    const result = g.neighbors(a, .out);

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

    // Act
    const result = g.neighbors(a, .in);

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

    // Act
    const result = g.neighbors(a, .both);

    // Assert
    try std.testing.expectEqual(@as(usize, 2), result.len);
}

// Empty/zero tests (pass: stubs return null/empty)

test "empty graph has zero nodes" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Assert
    try std.testing.expectEqual(@as(usize, 0), g.nodeCount());
}

test "empty graph has zero edges" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Assert
    try std.testing.expectEqual(@as(usize, 0), g.edgeCount());
}

test "getNode on empty graph returns null" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    const result = g.getNode(.root);

    // Assert
    try std.testing.expectEqual(@as(?*const Node, null), result);
}

test "getChildren on empty graph returns empty" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    const children = g.getChildren(.root);

    // Assert
    try std.testing.expectEqual(@as(usize, 0), children.len);
}

test "getParent on empty graph returns null" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    const result = g.getParent(.root);

    // Assert
    try std.testing.expectEqual(@as(?NodeId, null), result);
}

test "neighbors on empty graph returns empty" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    const result = g.neighbors(.root, .both);

    // Assert
    try std.testing.expectEqual(@as(usize, 0), result.len);
}

// Error/boundary tests

test "getNode with non-existent id returns null" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act: id 99 does not exist
    const result = g.getNode(@enumFromInt(99));

    // Assert
    try std.testing.expectEqual(@as(?*const Node, null), result);
}

test "getChildren with non-existent parent returns empty" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    const children = g.getChildren(@enumFromInt(99));

    // Assert
    try std.testing.expectEqual(@as(usize, 0), children.len);
}

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

test "node stores doc via graph" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    const n = Node{
        .id = .root,
        .name = "documented",
        .kind = .function,
        .language = .zig,
        .doc = "/// My doc",
    };

    // Act
    const id = try g.addNode(n);
    const result = g.getNode(id);

    // Assert
    try std.testing.expect(result != null);
    try std.testing.expect(result.?.doc != null);
    try std.testing.expectEqualStrings("/// My doc", result.?.doc.?);
}

test "node stores signature via graph" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    const n = Node{
        .id = .root,
        .name = "signed",
        .kind = .function,
        .language = .zig,
        .signature = "pub fn signed() void",
    };

    // Act
    const id = try g.addNode(n);
    const result = g.getNode(id);

    // Assert
    try std.testing.expect(result != null);
    try std.testing.expect(result.?.signature != null);
    try std.testing.expectEqualStrings("pub fn signed() void", result.?.signature.?);
}
