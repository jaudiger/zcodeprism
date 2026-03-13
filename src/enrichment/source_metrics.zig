//! Source-text metrics for function nodes: structural_hash and lines.
//! Intrinsic control-flow metrics are computed by the language visitors.

const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const metrics_mod = @import("../core/metrics.zig");
const source_scan = @import("../parser/source_scan.zig");

const Graph = graph_mod.Graph;
const Metrics = metrics_mod.Metrics;

/// Fill structural_hash and lines on function nodes in [file_idx, file_end_idx).
/// Merges into existing visitor-populated metrics when present.
pub fn computeAllSourceMetrics(graph: *Graph, source: []const u8, file_idx: usize, file_end_idx: usize) void {
    const end = @min(file_end_idx, graph.nodes.items.len);
    for (graph.nodes.items[file_idx..end]) |*n| {
        if (n.kind != .function) continue;
        const ls = n.line_start orelse continue;
        const le = n.line_end orelse continue;

        const lines: u32 = le - ls + 1;
        const fn_source = source_scan.extractLineRange(source, ls, le);
        const sh = source_scan.computeStructuralHash(fn_source);

        if (n.metrics) |*m| {
            m.lines = lines;
            m.structural_hash = sh;
        } else {
            n.metrics = Metrics{
                .lines = lines,
                .structural_hash = sh,
            };
        }
    }
}

test "visitor-populated metrics are preserved" {
    // Arrange
    const alloc = std.testing.allocator;
    var g = Graph.init("/tmp/project");
    defer g.deinit(alloc);

    const source = "fn foo() void {\n    if (x) {}\n}\n";

    _ = try g.addNode(alloc, .{
        .id = .root,
        .name = "test.zig",
        .kind = .file,
        .line_start = 1,
        .line_end = 3,
    });
    _ = try g.addNode(alloc, .{
        .id = .root,
        .name = "foo",
        .kind = .function,
        .line_start = 1,
        .line_end = 3,
        .metrics = Metrics{ .complexity = 2, .branches = 1 },
    });

    // Act
    computeAllSourceMetrics(&g, source, 0, g.nodeCount());

    // Assert
    const m = g.getNode(@enumFromInt(1)).?.metrics.?;
    try std.testing.expectEqual(@as(u16, 2), m.complexity);
    try std.testing.expectEqual(@as(u16, 1), m.branches);
    try std.testing.expectEqual(@as(u32, 3), m.lines);
    try std.testing.expect(m.structural_hash != 0);
}

test "default metrics when visitor did not populate" {
    // Arrange
    const alloc = std.testing.allocator;
    var g = Graph.init("/tmp/project");
    defer g.deinit(alloc);

    const source = "fn bar() void {}\n";

    _ = try g.addNode(alloc, .{
        .id = .root,
        .name = "test.zig",
        .kind = .file,
        .line_start = 1,
        .line_end = 1,
    });
    _ = try g.addNode(alloc, .{
        .id = .root,
        .name = "bar",
        .kind = .function,
        .line_start = 1,
        .line_end = 1,
    });

    // Act
    computeAllSourceMetrics(&g, source, 0, g.nodeCount());

    // Assert
    const m = g.getNode(@enumFromInt(1)).?.metrics.?;
    try std.testing.expectEqual(@as(u16, 0), m.complexity);
    try std.testing.expectEqual(@as(u32, 1), m.lines);
    try std.testing.expect(m.structural_hash != 0);
}
