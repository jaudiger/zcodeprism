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
        const syntax = source_scan.CommentSyntax.forLanguage(n.language);
        const sh = source_scan.computeStructuralHash(fn_source, syntax);

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

test "structural hash: deterministic, normalized identifiers, structure-sensitive" {
    // Arrange: two functions with identical structure but different variable names,
    // and a third with different branching structure.
    const alloc = std.testing.allocator;

    const src = "fn foo() void {\n    if (x) {}\n}\nfn bar() void {\n    if (y) {}\n}\nfn baz() void {\n    if (a) {}\n    if (b) {}\n    while (c) { break; }\n}\n";

    var g = Graph.init("/tmp/project");
    defer g.deinit(alloc);

    _ = try g.addNode(alloc, .{ .id = .root, .name = "test.zig", .kind = .file, .line_start = 1, .line_end = 11 });
    _ = try g.addNode(alloc, .{ .id = .root, .name = "foo", .kind = .function, .line_start = 1, .line_end = 3 });
    _ = try g.addNode(alloc, .{ .id = .root, .name = "bar", .kind = .function, .line_start = 4, .line_end = 6 });
    _ = try g.addNode(alloc, .{ .id = .root, .name = "baz", .kind = .function, .line_start = 7, .line_end = 11 });

    // Act
    computeAllSourceMetrics(&g, src, 0, g.nodeCount());

    const hash_foo = g.getNode(@enumFromInt(1)).?.metrics.?.structural_hash;
    const hash_bar = g.getNode(@enumFromInt(2)).?.metrics.?.structural_hash;
    const hash_baz = g.getNode(@enumFromInt(3)).?.metrics.?.structural_hash;

    // Assert: identical structure with different names produces same hash
    try std.testing.expectEqual(hash_foo, hash_bar);

    // Assert: different structure produces different hash
    try std.testing.expect(hash_foo != hash_baz);

    // Assert: deterministic (run again on a fresh graph)
    var g2 = Graph.init("/tmp/project");
    defer g2.deinit(alloc);

    _ = try g2.addNode(alloc, .{ .id = .root, .name = "test.zig", .kind = .file, .line_start = 1, .line_end = 11 });
    _ = try g2.addNode(alloc, .{ .id = .root, .name = "foo", .kind = .function, .line_start = 1, .line_end = 3 });

    computeAllSourceMetrics(&g2, src, 0, g2.nodeCount());

    try std.testing.expectEqual(hash_foo, g2.getNode(@enumFromInt(1)).?.metrics.?.structural_hash);
}

test "structural hash: comment and whitespace changes do not affect hash" {
    // Arrange: same function with a comment and different indentation.
    const alloc = std.testing.allocator;

    const src_plain = "fn foo() void {\n    if (x) {}\n}\n";
    const src_commented = "fn foo() void {\n    // a comment\n    if (x) {}\n}\n";
    const src_reindented = "fn foo() void {\n\tif (x) {}\n}\n";

    var g1 = Graph.init("/tmp/project");
    defer g1.deinit(alloc);
    _ = try g1.addNode(alloc, .{ .id = .root, .name = "test.zig", .kind = .file, .line_start = 1, .line_end = 3 });
    _ = try g1.addNode(alloc, .{ .id = .root, .name = "foo", .kind = .function, .line_start = 1, .line_end = 3 });
    computeAllSourceMetrics(&g1, src_plain, 0, g1.nodeCount());

    var g2 = Graph.init("/tmp/project");
    defer g2.deinit(alloc);
    _ = try g2.addNode(alloc, .{ .id = .root, .name = "test.zig", .kind = .file, .line_start = 1, .line_end = 4 });
    _ = try g2.addNode(alloc, .{ .id = .root, .name = "foo", .kind = .function, .line_start = 1, .line_end = 4 });
    computeAllSourceMetrics(&g2, src_commented, 0, g2.nodeCount());

    var g3 = Graph.init("/tmp/project");
    defer g3.deinit(alloc);
    _ = try g3.addNode(alloc, .{ .id = .root, .name = "test.zig", .kind = .file, .line_start = 1, .line_end = 3 });
    _ = try g3.addNode(alloc, .{ .id = .root, .name = "foo", .kind = .function, .line_start = 1, .line_end = 3 });
    computeAllSourceMetrics(&g3, src_reindented, 0, g3.nodeCount());

    // Act
    const hash_plain = g1.getNode(@enumFromInt(1)).?.metrics.?.structural_hash;
    const hash_commented = g2.getNode(@enumFromInt(1)).?.metrics.?.structural_hash;
    const hash_reindented = g3.getNode(@enumFromInt(1)).?.metrics.?.structural_hash;

    // Assert
    try std.testing.expectEqual(hash_plain, hash_commented);
    try std.testing.expectEqual(hash_plain, hash_reindented);
}
