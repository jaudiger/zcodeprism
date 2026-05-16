const std = @import("std");
const zcodeprism = @import("zcodeprism");
const fixtures = @import("test-fixtures");
const helpers = @import("test-helpers");

const Graph = zcodeprism.graph.Graph;
const Node = zcodeprism.node.Node;
const NodeId = zcodeprism.types.NodeId;
const NodeKind = zcodeprism.types.NodeKind;
const EdgeType = zcodeprism.types.EdgeType;
const EdgeSource = zcodeprism.types.EdgeSource;
const Language = zcodeprism.types.Language;

const query_mod = zcodeprism.query;
const coupling_mod = zcodeprism.analyzer.coupling;

const indexDirectory = zcodeprism.indexer.indexDirectory;
const writeFixtureFiles = helpers.writeFixtureFiles;

const mixed_project_files: []const helpers.FileEntry = &.{
    .{ .sub_path = "src/main.zig", .data = fixtures.mixed_project.main_zig },
    .{ .sub_path = "src/lib.rs", .data = fixtures.mixed_project.lib_rs },
};

fn setupMixedProject(tmp_dir: *std.testing.TmpDir) ![:0]const u8 {
    try writeFixtureFiles(std.testing.io, tmp_dir.dir, mixed_project_files);
    return try tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
}

fn indexMixedProject(graph: *Graph, tmp_dir: *std.testing.TmpDir) !zcodeprism.indexer.IndexResult {
    const project_root = try setupMixedProject(tmp_dir);
    defer std.testing.allocator.free(project_root);
    return indexDirectory(std.testing.allocator, std.testing.io, project_root, graph, null, .{});
}

test "unified graph contains both zig and rust file nodes" {
    // Arrange
    var g = Graph.init("/tmp/mixed");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    const result = try indexMixedProject(&g, &tmp_dir);

    // Assert
    try std.testing.expect(result.files_indexed >= 2);
    try std.testing.expect(helpers.findNode(&g, "main.zig", .file) != null);
    try std.testing.expect(helpers.findNode(&g, "lib.rs", .file) != null);

    var has_zig = false;
    var has_rust = false;
    for (g.nodes.items) |n| {
        if (n.language) |lang| {
            if (lang == .zig) has_zig = true;
            if (lang == .rust) has_rust = true;
        }
    }
    try std.testing.expect(has_zig);
    try std.testing.expect(has_rust);
}

test "FFI edge from zig extern declaration to rust extern definition" {
    // Arrange
    var g = Graph.init("/tmp/mixed");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = try indexMixedProject(&g, &tmp_dir);

    // Assert
    const zig_file = helpers.findNode(&g, "main.zig", .file) orelse return error.TestExpectedEqual;
    const zig_rust_add = helpers.findNodeInFile(&g, "rust_add", .function, zig_file.id) orelse
        return error.TestExpectedEqual;

    const rs_file = helpers.findNode(&g, "lib.rs", .file) orelse return error.TestExpectedEqual;
    const rs_rust_add = helpers.findNodeInFile(&g, "rust_add", .function, rs_file.id) orelse
        return error.TestExpectedEqual;

    try std.testing.expect(helpers.hasEdge(&g, zig_rust_add, rs_rust_add, .calls));
}

test "FFI edge exists for all matching extern symbols" {
    // Arrange
    var g = Graph.init("/tmp/mixed");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = try indexMixedProject(&g, &tmp_dir);

    // Assert
    const zig_file = helpers.findNode(&g, "main.zig", .file) orelse return error.TestExpectedEqual;
    const rs_file = helpers.findNode(&g, "lib.rs", .file) orelse return error.TestExpectedEqual;

    const zig_mul = helpers.findNodeInFile(&g, "rust_multiply", .function, zig_file.id) orelse
        return error.TestExpectedEqual;
    const rs_mul = helpers.findNodeInFile(&g, "rust_multiply", .function, rs_file.id) orelse
        return error.TestExpectedEqual;

    try std.testing.expect(helpers.hasEdge(&g, zig_mul, rs_mul, .calls));
}

test "FFI edge has source workspace" {
    // Arrange
    var g = Graph.init("/tmp/mixed");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = try indexMixedProject(&g, &tmp_dir);

    // Assert
    const zig_file = helpers.findNode(&g, "main.zig", .file) orelse return error.TestExpectedEqual;
    const rs_file = helpers.findNode(&g, "lib.rs", .file) orelse return error.TestExpectedEqual;
    const zig_fn = helpers.findNodeInFile(&g, "rust_add", .function, zig_file.id) orelse
        return error.TestExpectedEqual;
    const rs_fn = helpers.findNodeInFile(&g, "rust_add", .function, rs_file.id) orelse
        return error.TestExpectedEqual;

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == zig_fn and e.target_id == rs_fn and e.edge_type == .calls) {
            try std.testing.expectEqual(EdgeSource.workspace, e.source);
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "search with language zig returns only zig nodes" {
    // Arrange
    var g = Graph.init("/tmp/mixed");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    _ = try indexMixedProject(&g, &tmp_dir);
    const fg = g.asFrozen();

    // Act
    const result = try query_mod.search(std.testing.allocator, fg, .{
        .language = .zig,
        .external = .exclude,
    });
    defer result.deinit(std.testing.allocator);

    // Assert
    try std.testing.expect(result.total_matches > 0);
    for (result.nodes) |nid| {
        const n = g.getNode(nid) orelse continue;
        try std.testing.expectEqual(@as(?Language, .zig), n.language);
    }
}

test "search with language rust returns only rust nodes" {
    // Arrange
    var g = Graph.init("/tmp/mixed");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    _ = try indexMixedProject(&g, &tmp_dir);
    const fg = g.asFrozen();

    // Act
    const result = try query_mod.search(std.testing.allocator, fg, .{
        .language = .rust,
        .external = .exclude,
    });
    defer result.deinit(std.testing.allocator);

    // Assert
    try std.testing.expect(result.total_matches > 0);
    for (result.nodes) |nid| {
        const n = g.getNode(nid) orelse continue;
        try std.testing.expectEqual(@as(?Language, .rust), n.language);
    }
}

test "search without language filter returns nodes from both languages" {
    // Arrange
    var g = Graph.init("/tmp/mixed");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    _ = try indexMixedProject(&g, &tmp_dir);
    const fg = g.asFrozen();

    // Act
    const result = try query_mod.search(std.testing.allocator, fg, .{
        .external = .exclude,
        .limit = 200,
    });
    defer result.deinit(std.testing.allocator);

    // Assert
    var found_zig = false;
    var found_rust = false;
    for (result.nodes) |nid| {
        const n = g.getNode(nid) orelse continue;
        if (n.language) |lang| {
            if (lang == .zig) found_zig = true;
            if (lang == .rust) found_rust = true;
        }
    }
    try std.testing.expect(found_zig);
    try std.testing.expect(found_rust);
}

test "coupling between zig and rust files is positive" {
    // Arrange
    var g = Graph.init("/tmp/mixed");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    _ = try indexMixedProject(&g, &tmp_dir);
    const fg = g.asFrozen();

    // Act
    const result = try coupling_mod.findCoupling(std.testing.allocator, fg, .{
        .min_coupling = 0.5,
    });
    defer result.deinit(std.testing.allocator);

    // Assert
    try std.testing.expect(result.pairs.len >= 1);
    var found_cross_lang_pair = false;
    for (result.pairs) |pair| {
        const a_name = pair.module_a;
        const b_name = pair.module_b;
        const a_is_zig = std.mem.endsWith(u8, a_name, ".zig");
        const b_is_rs = std.mem.endsWith(u8, b_name, ".rs");
        const a_is_rs = std.mem.endsWith(u8, a_name, ".rs");
        const b_is_zig = std.mem.endsWith(u8, b_name, ".zig");
        if ((a_is_zig and b_is_rs) or (a_is_rs and b_is_zig)) {
            found_cross_lang_pair = true;
            try std.testing.expect(pair.score > 0);
            break;
        }
    }
    try std.testing.expect(found_cross_lang_pair);
}

test "stats report both languages present" {
    // Arrange
    var g = Graph.init("/tmp/mixed");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    _ = try indexMixedProject(&g, &tmp_dir);
    const fg = g.asFrozen();

    // Act
    const stats = try query_mod.computeStats(std.testing.allocator, fg, .{});

    // Assert
    try std.testing.expect(stats.has_zig);
    try std.testing.expect(stats.has_rust);
    try std.testing.expect(stats.node_counts[@intFromEnum(NodeKind.file)] >= 2);
}
