const std = @import("std");
const zcodeprism = @import("zcodeprism");
const helpers = @import("test-helpers");

const Graph = zcodeprism.graph.Graph;
const EdgeSource = zcodeprism.types.EdgeSource;
const NodeKind = zcodeprism.types.NodeKind;
const zig_meta = zcodeprism.zig_meta;
const indexer = zcodeprism.indexer;
const enricher = zcodeprism.lsp.enricher;

test "graph is complete without LSP" {
    // Arrange
    const allocator = std.testing.allocator;
    const fixture_path = try resolveFixturePath(allocator, "test/fixtures/zig/lsp");
    defer allocator.free(fixture_path);

    var graph = Graph.init(fixture_path);
    defer graph.deinit(allocator);

    // Act
    _ = try indexer.indexDirectory(indexer.IndexAllocators.single(allocator), std.testing.io, fixture_path, &graph, null, .{});

    // Assert
    try std.testing.expect(graph.nodeCount() > 0);
    try std.testing.expect(graph.edgeCount() > 0);
    try std.testing.expect(!helpers.hasEdgeWithSource(&graph, .lsp));

    var has_fan_out = false;
    for (graph.nodes.items) |n| {
        if (n.metrics) |m| {
            if (m.fan_out > 0) {
                has_fan_out = true;
                break;
            }
        }
    }
    try std.testing.expect(has_fan_out);

    const process_fn = helpers.findNode(&graph, "processConfig", .function);
    try std.testing.expect(process_fn != null);
    if (process_fn) |f| {
        if (zig_meta.metaOf(f)) |zm| {
            try std.testing.expectEqual(@as(?[]const []const u8, null), zm.inferred_errors);
        }
    }
}

test "LSP enrichment adds edges and populates errors" {
    // Arrange
    const allocator = std.testing.allocator;
    const zls_available = blk: {
        var child = std.process.spawn(std.testing.io, .{
            .argv = &.{ "zls", "--version" },
            .stderr = .ignore,
            .stdout = .ignore,
        }) catch break :blk false;
        const term = child.wait(std.testing.io) catch break :blk false;
        break :blk term == .exited and term.exited == 0;
    };
    if (!zls_available) return;

    const fixture_path = try resolveFixturePath(allocator, "test/fixtures/zig/lsp");
    defer allocator.free(fixture_path);

    var graph = Graph.init(fixture_path);
    defer graph.deinit(allocator);

    var wl = zcodeprism.lsp.worklist.LspWorklist{};
    defer wl.deinit(allocator);
    _ = try indexer.indexDirectory(indexer.IndexAllocators.single(allocator), std.testing.io, fixture_path, &graph, &wl, .{});

    const pre_lsp_edge_count = graph.edgeCount();

    // Act
    const zig_support = zcodeprism.registry.Registry.getByExtension(".zig").?;
    var lsp_pool = zcodeprism.lsp.pool.LspPool.init(.{});
    defer lsp_pool.deinit(allocator, std.testing.io);
    const result = try enricher.enrich(indexer.IndexAllocators.single(allocator), std.testing.io, &graph, zig_support, &wl, &lsp_pool, .{
        .project_root = fixture_path,
    });

    // Assert
    try std.testing.expect(graph.edgeCount() >= pre_lsp_edge_count);

    const lsp_edge_count = helpers.countEdgesBySource(&graph, .lsp);
    try std.testing.expectEqual(result.edges_promoted + result.edges_added, lsp_edge_count);

    var has_fan_in = false;
    for (graph.nodes.items) |n| {
        if (n.metrics) |m| {
            if (m.fan_in > 0) {
                has_fan_in = true;
                break;
            }
        }
    }
    try std.testing.expect(has_fan_in);

    var has_inferred = false;
    for (graph.nodes.items) |n| {
        if (n.kind != .function) continue;
        if (zig_meta.metaOf(&n)) |zm| {
            if (zm.inferred_errors != null) {
                has_inferred = true;
                break;
            }
        }
    }
    try std.testing.expect(has_inferred);
}

test "rust graph is complete without rust-analyzer" {
    // Arrange
    const allocator = std.testing.allocator;
    const fixture_path = try resolveFixturePath(allocator, "test/fixtures/rust/project");
    defer allocator.free(fixture_path);

    var graph = Graph.init(fixture_path);
    defer graph.deinit(allocator);

    // Act
    _ = try indexer.indexDirectory(indexer.IndexAllocators.single(allocator), std.testing.io, fixture_path, &graph, null, .{});

    // Assert
    try std.testing.expect(graph.nodeCount() > 0);
    try std.testing.expect(graph.edgeCount() > 0);
    try std.testing.expect(!helpers.hasEdgeWithSource(&graph, .lsp));
}

test "rust-analyzer enrichment adds edges" {
    // Arrange
    const allocator = std.testing.allocator;
    const ra_available = blk: {
        var child = std.process.spawn(std.testing.io, .{
            .argv = &.{ "rust-analyzer", "--version" },
            .stderr = .ignore,
            .stdout = .ignore,
        }) catch break :blk false;
        const term = child.wait(std.testing.io) catch break :blk false;
        break :blk term == .exited and term.exited == 0;
    };
    if (!ra_available) return;

    const fixture_path = try resolveFixturePath(allocator, "test/fixtures/rust_project");
    defer allocator.free(fixture_path);

    var graph = Graph.init(fixture_path);
    defer graph.deinit(allocator);

    var wl = zcodeprism.lsp.worklist.LspWorklist{};
    defer wl.deinit(allocator);
    _ = try indexer.indexDirectory(indexer.IndexAllocators.single(allocator), std.testing.io, fixture_path, &graph, &wl, .{});

    const pre_lsp_edge_count = graph.edgeCount();

    // Act
    const rust_support = zcodeprism.registry.Registry.getByExtension(".rs").?;
    var lsp_pool = zcodeprism.lsp.pool.LspPool.init(.{});
    defer lsp_pool.deinit(allocator, std.testing.io);
    const result = try enricher.enrich(indexer.IndexAllocators.single(allocator), std.testing.io, &graph, rust_support, &wl, &lsp_pool, .{
        .project_root = fixture_path,
    });

    // Assert
    try std.testing.expect(graph.edgeCount() >= pre_lsp_edge_count);

    const lsp_edge_count = helpers.countEdgesBySource(&graph, .lsp);
    try std.testing.expectEqual(result.edges_promoted + result.edges_added, lsp_edge_count);
}

/// Resolve a project-relative path to an absolute path.
fn resolveFixturePath(allocator: std.mem.Allocator, rel: []const u8) ![]const u8 {
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const n = try std.process.currentPath(std.testing.io, &buf);
    return try std.fs.path.join(allocator, &.{ buf[0..n], rel });
}
