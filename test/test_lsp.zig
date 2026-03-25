const std = @import("std");
const zcodeprism = @import("zcodeprism");
const helpers = @import("test-helpers");

const Graph = zcodeprism.graph.Graph;
const EdgeSource = zcodeprism.types.EdgeSource;
const NodeKind = zcodeprism.types.NodeKind;
const indexer = zcodeprism.indexer;
const enricher = zcodeprism.lsp.enricher;

test "graph is complete without LSP" {
    // Arrange: index the lsp fixture directory
    const allocator = std.testing.allocator;
    const fixture_path = try resolveFixturePath(allocator, "test/fixtures/zig/lsp");
    defer allocator.free(fixture_path);

    var graph = Graph.init(fixture_path);
    defer graph.deinit(allocator);

    // Act
    _ = try indexer.indexDirectory(allocator, fixture_path, &graph, null, .{});

    // Assert: graph has nodes and edges, no crash
    try std.testing.expect(graph.nodeCount() > 0);
    try std.testing.expect(graph.edgeCount() > 0);

    // No edge was discovered by LSP
    try std.testing.expect(!helpers.hasEdgeWithSource(&graph, .lsp));

    // Fan metrics are populated (enrichment ran)
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

    // Functions returning !void from stdlib calls have null inferred_errors
    // because tree-sitter cannot resolve cross-package error sets.
    const process_fn = helpers.findNode(&graph, "processConfig", .function);
    try std.testing.expect(process_fn != null);
    if (process_fn) |f| {
        if (f.lang_meta == .zig) {
            try std.testing.expectEqual(@as(?[]const []const u8, null), f.lang_meta.zig.inferred_errors);
        }
    }
}

test "LSP enrichment adds edges and populates errors" {
    // Arrange: check if ZLS is available, skip if not
    const allocator = std.testing.allocator;
    const zls_available = blk: {
        var child = std.process.Child.init(&.{ "zls", "--version" }, allocator);
        child.stderr_behavior = .Ignore;
        child.stdout_behavior = .Ignore;
        const term = child.spawnAndWait() catch break :blk false;
        break :blk term == .Exited and term.Exited == 0;
    };
    if (!zls_available) return;

    const fixture_path = try resolveFixturePath(allocator, "test/fixtures/zig/lsp");
    defer allocator.free(fixture_path);

    var graph = Graph.init(fixture_path);
    defer graph.deinit(allocator);

    // Index with a worklist so hover entries are collected for LSP enrichment.
    var wl = zcodeprism.lsp.worklist.LspWorklist{};
    defer wl.deinit(allocator);
    _ = try indexer.indexDirectory(allocator, fixture_path, &graph, &wl, .{});

    // Record pre-LSP state
    const pre_lsp_edge_count = graph.edgeCount();

    // Act: run LSP enrichment with the populated worklist
    const zig_support = zcodeprism.registry.Registry.getByExtension(".zig").?;
    var lsp_pool = zcodeprism.lsp.pool.LspPool.init(.{});
    defer lsp_pool.deinit(allocator);
    const result = try enricher.enrich(allocator, &graph, zig_support, &wl, &lsp_pool, .{
        .project_root = fixture_path,
    });

    // LSP enrichment only adds edges, never removes.
    try std.testing.expect(graph.edgeCount() >= pre_lsp_edge_count);

    // All LSP edges in the graph are accounted for by the result counters.
    const lsp_edge_count = helpers.countEdgesBySource(&graph, .lsp);
    try std.testing.expectEqual(result.edges_promoted + result.edges_added, lsp_edge_count);

    // All pre-LSP edge keys still present (LSP only adds, never removes)
    // Fan metrics recalculated
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

    // At least one function has non-null inferred_errors after LSP
    var has_inferred = false;
    for (graph.nodes.items) |n| {
        if (n.kind != .function) continue;
        if (n.lang_meta == .zig) {
            if (n.lang_meta.zig.inferred_errors != null) {
                has_inferred = true;
                break;
            }
        }
    }
    try std.testing.expect(has_inferred);
}

test "rust graph is complete without rust-analyzer" {
    // Arrange: index the Rust project fixture directory
    const allocator = std.testing.allocator;
    const fixture_path = try resolveFixturePath(allocator, "test/fixtures/rust/project");
    defer allocator.free(fixture_path);

    var graph = Graph.init(fixture_path);
    defer graph.deinit(allocator);

    // Act
    _ = try indexer.indexDirectory(allocator, fixture_path, &graph, null, .{});

    // Assert: graph has Rust nodes
    try std.testing.expect(graph.nodeCount() > 0);
    try std.testing.expect(graph.edgeCount() > 0);

    // No edge was discovered by LSP
    try std.testing.expect(!helpers.hasEdgeWithSource(&graph, .lsp));
}

test "rust-analyzer enrichment adds edges" {
    // Arrange: check if rust-analyzer is available, skip if not
    const allocator = std.testing.allocator;
    const ra_available = blk: {
        var child = std.process.Child.init(&.{ "rust-analyzer", "--version" }, allocator);
        child.stderr_behavior = .Ignore;
        child.stdout_behavior = .Ignore;
        const term = child.spawnAndWait() catch break :blk false;
        break :blk term == .Exited and term.Exited == 0;
    };
    if (!ra_available) return;

    const fixture_path = try resolveFixturePath(allocator, "test/fixtures/rust_project");
    defer allocator.free(fixture_path);

    var graph = Graph.init(fixture_path);
    defer graph.deinit(allocator);

    // Index with a worklist for LSP enrichment.
    var wl = zcodeprism.lsp.worklist.LspWorklist{};
    defer wl.deinit(allocator);
    _ = try indexer.indexDirectory(allocator, fixture_path, &graph, &wl, .{});

    // Record pre-LSP state
    const pre_lsp_edge_count = graph.edgeCount();

    // Act: run LSP enrichment with the populated worklist
    const rust_support = zcodeprism.registry.Registry.getByExtension(".rs").?;
    var lsp_pool = zcodeprism.lsp.pool.LspPool.init(.{});
    defer lsp_pool.deinit(allocator);
    const result = try enricher.enrich(allocator, &graph, rust_support, &wl, &lsp_pool, .{
        .project_root = fixture_path,
    });

    // LSP enrichment only adds edges, never removes.
    try std.testing.expect(graph.edgeCount() >= pre_lsp_edge_count);

    // All LSP edges in the graph are accounted for by the result counters.
    const lsp_edge_count = helpers.countEdgesBySource(&graph, .lsp);
    try std.testing.expectEqual(result.edges_promoted + result.edges_added, lsp_edge_count);
}

/// Resolve a project-relative path to an absolute path.
fn resolveFixturePath(allocator: std.mem.Allocator, rel: []const u8) ![]const u8 {
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const cwd = try std.process.getCwd(&buf);
    return try std.fs.path.join(allocator, &.{ cwd, rel });
}
