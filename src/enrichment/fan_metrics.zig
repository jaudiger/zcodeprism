//! Fan-in and fan-out computation from graph edges. Both functions are
//! idempotent (reset counters before counting) and allocate nothing.

const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const types = @import("../core/types.zig");

const Graph = graph_mod.Graph;
const EdgeType = types.EdgeType;

/// Reset fan_out to 0 for all nodes, then count outgoing calls and
/// uses_type edges. Saturates at maxInt(u16). No allocations.
pub fn computeFanOut(graph: *Graph) void {
    resetFanOut(graph);
    for (graph.edges.items) |e| {
        if (!isFanEdge(e.edge_type)) continue;
        const idx = @intFromEnum(e.source_id);
        if (idx >= graph.nodes.items.len) continue;
        const n = &graph.nodes.items[idx];
        if (n.metrics) |*m| {
            m.fan_out +|= 1;
        }
    }
}

/// Reset fan_in to 0 for all nodes, then count incoming calls and
/// uses_type edges. Saturates at maxInt(u16). No allocations.
pub fn computeFanIn(graph: *Graph) void {
    resetFanIn(graph);
    for (graph.edges.items) |e| {
        if (!isFanEdge(e.edge_type)) continue;
        const idx = @intFromEnum(e.target_id);
        if (idx >= graph.nodes.items.len) continue;
        const n = &graph.nodes.items[idx];
        if (n.metrics) |*m| {
            m.fan_in +|= 1;
        }
    }
}

/// Only calls and uses_type count as fan edges.
fn isFanEdge(t: EdgeType) bool {
    return t == .calls or t == .uses_type;
}

fn resetFanOut(graph: *Graph) void {
    for (graph.nodes.items) |*n| {
        if (n.metrics) |*m| m.fan_out = 0;
    }
}

fn resetFanIn(graph: *Graph) void {
    for (graph.nodes.items) |*n| {
        if (n.metrics) |*m| m.fan_in = 0;
    }
}

test "fan_out counts outgoing calls edges" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = Graph.init("/tmp");
    defer g.deinit(allocator);

    const a = try g.addNode(allocator, .{ .id = .root, .name = "a", .kind = .function, .language = .zig, .metrics = .{} });
    const b = try g.addNode(allocator, .{ .id = .root, .name = "b", .kind = .function, .language = .zig, .metrics = .{} });
    const c = try g.addNode(allocator, .{ .id = .root, .name = "c", .kind = .function, .language = .zig, .metrics = .{} });
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = a, .target_id = b, .edge_type = .calls });
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = a, .target_id = c, .edge_type = .calls });

    // Act
    computeFanOut(&g);

    // Assert
    try std.testing.expectEqual(@as(u16, 2), g.nodes.items[@intFromEnum(a)].metrics.?.fan_out);
    try std.testing.expectEqual(@as(u16, 0), g.nodes.items[@intFromEnum(b)].metrics.?.fan_out);
}

test "fan_in counts incoming calls edges" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = Graph.init("/tmp");
    defer g.deinit(allocator);

    const a = try g.addNode(allocator, .{ .id = .root, .name = "a", .kind = .function, .language = .zig, .metrics = .{} });
    const b = try g.addNode(allocator, .{ .id = .root, .name = "b", .kind = .function, .language = .zig, .metrics = .{} });
    const c = try g.addNode(allocator, .{ .id = .root, .name = "c", .kind = .function, .language = .zig, .metrics = .{} });
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = b, .target_id = a, .edge_type = .calls });
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = c, .target_id = a, .edge_type = .calls });

    // Act
    computeFanIn(&g);

    // Assert
    try std.testing.expectEqual(@as(u16, 2), g.nodes.items[@intFromEnum(a)].metrics.?.fan_in);
    try std.testing.expectEqual(@as(u16, 0), g.nodes.items[@intFromEnum(b)].metrics.?.fan_in);
}

test "fan metrics ignore import edges" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = Graph.init("/tmp");
    defer g.deinit(allocator);

    const a = try g.addNode(allocator, .{ .id = .root, .name = "a", .kind = .file, .language = .zig, .metrics = .{} });
    const b = try g.addNode(allocator, .{ .id = .root, .name = "b", .kind = .file, .language = .zig, .metrics = .{} });
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = a, .target_id = b, .edge_type = .imports });

    // Act
    computeFanOut(&g);
    computeFanIn(&g);

    // Assert
    try std.testing.expectEqual(@as(u16, 0), g.nodes.items[@intFromEnum(a)].metrics.?.fan_out);
    try std.testing.expectEqual(@as(u16, 0), g.nodes.items[@intFromEnum(b)].metrics.?.fan_in);
}

test "fan metrics count uses_type edges" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = Graph.init("/tmp");
    defer g.deinit(allocator);

    const a = try g.addNode(allocator, .{ .id = .root, .name = "a", .kind = .function, .language = .zig, .metrics = .{} });
    const b = try g.addNode(allocator, .{ .id = .root, .name = "b", .kind = .type_def, .language = .zig, .metrics = .{} });
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = a, .target_id = b, .edge_type = .uses_type });

    // Act
    computeFanOut(&g);
    computeFanIn(&g);

    // Assert
    try std.testing.expectEqual(@as(u16, 1), g.nodes.items[@intFromEnum(a)].metrics.?.fan_out);
    try std.testing.expectEqual(@as(u16, 1), g.nodes.items[@intFromEnum(b)].metrics.?.fan_in);
}

test "fan metrics are idempotent" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = Graph.init("/tmp");
    defer g.deinit(allocator);

    const a = try g.addNode(allocator, .{ .id = .root, .name = "a", .kind = .function, .language = .zig, .metrics = .{} });
    const b = try g.addNode(allocator, .{ .id = .root, .name = "b", .kind = .function, .language = .zig, .metrics = .{} });
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = a, .target_id = b, .edge_type = .calls });

    // Act: run twice
    computeFanOut(&g);
    computeFanOut(&g);

    // Assert: same result, not doubled
    try std.testing.expectEqual(@as(u16, 1), g.nodes.items[@intFromEnum(a)].metrics.?.fan_out);
}

test "fan metrics skip nodes without metrics" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = Graph.init("/tmp");
    defer g.deinit(allocator);

    const a = try g.addNode(allocator, .{ .id = .root, .name = "a", .kind = .function, .language = .zig });
    const b = try g.addNode(allocator, .{ .id = .root, .name = "b", .kind = .function, .language = .zig, .metrics = .{} });
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = a, .target_id = b, .edge_type = .calls });

    // Act
    computeFanOut(&g);
    computeFanIn(&g);

    // Assert: node a has no metrics, so fan_out is not set
    try std.testing.expectEqual(@as(?@import("../core/metrics.zig").Metrics, null), g.nodes.items[@intFromEnum(a)].metrics);
    try std.testing.expectEqual(@as(u16, 1), g.nodes.items[@intFromEnum(b)].metrics.?.fan_in);
}
