const std = @import("std");
const zcodeprism = @import("zcodeprism");
const fixtures = @import("test-fixtures");
const helpers = @import("test-helpers");

const Graph = zcodeprism.graph.Graph;
const NodeId = zcodeprism.types.NodeId;
const NodeKind = zcodeprism.types.NodeKind;
const EdgeType = zcodeprism.types.EdgeType;
const EdgeSource = zcodeprism.types.EdgeSource;

const indexDirectory = zcodeprism.indexer.indexDirectory;
const IndexAllocators = zcodeprism.indexer.IndexAllocators;

const writeFixtureFiles = helpers.writeFixtureFiles;

fn setupBuildParsingFixtures(tmp_dir: *std.testing.TmpDir) ![:0]const u8 {
    try writeFixtureFiles(std.testing.io, tmp_dir.dir, &.{
        .{ .sub_path = "build.zig", .data = fixtures.zig.build_parsing.build_zig },
        .{ .sub_path = "build.zig.zon", .data = fixtures.zig.build_parsing.build_zig_zon },
        .{ .sub_path = "src/lib.zig", .data = fixtures.zig.build_parsing.src_lib_zig },
        .{ .sub_path = "src/main.zig", .data = fixtures.zig.build_parsing.src_main_zig },
    });
    return try tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
}

fn setupBuildNoDepFixtures(tmp_dir: *std.testing.TmpDir) ![:0]const u8 {
    try writeFixtureFiles(std.testing.io, tmp_dir.dir, &.{
        .{ .sub_path = "build.zig", .data = fixtures.zig.build_no_deps.build_zig },
        .{ .sub_path = "build.zig.zon", .data = fixtures.zig.build_no_deps.build_zig_zon },
        .{ .sub_path = "src/main.zig", .data = fixtures.zig.build_no_deps.src_main_zig },
    });
    return try tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
}

test "parses build.zig into module nodes" {
    // Arrange
    var g = Graph.init("/tmp/build_parsing");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupBuildParsingFixtures(&tmp_dir);
    defer std.testing.allocator.free(project_root);

    // Act
    _ = try indexDirectory(IndexAllocators.single(std.testing.allocator), std.testing.io, project_root, &g, null, .{});

    // Assert
    const module_count = helpers.countNodesByKind(&g, .module);
    try std.testing.expect(module_count >= 2);
}

test "module has contains edge to file" {
    // Arrange
    var g = Graph.init("/tmp/build_parsing");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupBuildParsingFixtures(&tmp_dir);
    defer std.testing.allocator.free(project_root);

    // Act
    _ = try indexDirectory(IndexAllocators.single(std.testing.allocator), std.testing.io, project_root, &g, null, .{});

    // Assert
    const lib_mod = helpers.findNode(&g, "lib_mod", .module) orelse return error.TestExpectedEqual;
    const lib_file = helpers.findNode(&g, "lib.zig", .file) orelse return error.TestExpectedEqual;
    try std.testing.expect(helpers.hasEdge(&g, lib_mod.id, lib_file.id, .contains));
}

test "file parent_id still points to directory" {
    // Arrange
    var g = Graph.init("/tmp/build_parsing");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupBuildParsingFixtures(&tmp_dir);
    defer std.testing.allocator.free(project_root);

    // Act
    _ = try indexDirectory(IndexAllocators.single(std.testing.allocator), std.testing.io, project_root, &g, null, .{});

    // Assert
    const lib_file = helpers.findNode(&g, "lib.zig", .file) orelse return error.TestExpectedEqual;
    const parent_id = lib_file.parent_id orelse return error.TestExpectedEqual;
    const parent = g.getNode(parent_id) orelse return error.TestExpectedEqual;
    try std.testing.expectEqual(NodeKind.directory, parent.kind);
}

test "dependencies become phantom modules" {
    // Arrange
    var g = Graph.init("/tmp/build_parsing");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupBuildParsingFixtures(&tmp_dir);
    defer std.testing.allocator.free(project_root);

    // Act
    _ = try indexDirectory(IndexAllocators.single(std.testing.allocator), std.testing.io, project_root, &g, null, .{});

    // Assert
    const dep_node = helpers.findNode(&g, "tree-sitter", .module) orelse return error.TestExpectedEqual;
    switch (dep_node.external) {
        .dependency => {},
        else => return error.TestExpectedEqual,
    }
}

test "phantom dep has version URL" {
    // Arrange
    var g = Graph.init("/tmp/build_parsing");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupBuildParsingFixtures(&tmp_dir);
    defer std.testing.allocator.free(project_root);

    // Act
    _ = try indexDirectory(IndexAllocators.single(std.testing.allocator), std.testing.io, project_root, &g, null, .{});

    // Assert
    const dep_node = helpers.findNode(&g, "tree-sitter", .module) orelse return error.TestExpectedEqual;
    switch (dep_node.external) {
        .dependency => |d| {
            try std.testing.expect(d.version != null);
            const version = d.version.?;
            try std.testing.expect(std.mem.indexOf(u8, version, "v0.24.0") != null);
        },
        else => return error.TestExpectedEqual,
    }
}

test "build.zig with no deps has module node but no phantom dep modules" {
    // Arrange
    var g = Graph.init("/tmp/build_no_deps");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupBuildNoDepFixtures(&tmp_dir);
    defer std.testing.allocator.free(project_root);

    // Act
    _ = try indexDirectory(IndexAllocators.single(std.testing.allocator), std.testing.io, project_root, &g, null, .{});

    // Assert
    const module_count = helpers.countNodesByKind(&g, .module);
    try std.testing.expect(module_count >= 1);

    // No dependency phantom modules should exist.
    var dep_count: usize = 0;
    for (g.nodes.items) |n| {
        if (n.kind == .module) {
            switch (n.external) {
                .dependency => dep_count += 1,
                else => {},
            }
        }
    }
    try std.testing.expectEqual(@as(usize, 0), dep_count);
}

test "multiple targets produce multiple module nodes" {
    // Arrange
    var g = Graph.init("/tmp/build_parsing");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupBuildParsingFixtures(&tmp_dir);
    defer std.testing.allocator.free(project_root);

    // Act
    _ = try indexDirectory(IndexAllocators.single(std.testing.allocator), std.testing.io, project_root, &g, null, .{});

    // Assert
    const lib_mod = helpers.findNode(&g, "lib_mod", .module);
    const exe_mod = helpers.findNode(&g, "exe_mod", .module);
    try std.testing.expect(lib_mod != null);
    try std.testing.expect(exe_mod != null);
}

test "module-to-file contains edge has workspace source" {
    // Arrange
    var g = Graph.init("/tmp/build_parsing");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupBuildParsingFixtures(&tmp_dir);
    defer std.testing.allocator.free(project_root);

    // Act
    _ = try indexDirectory(IndexAllocators.single(std.testing.allocator), std.testing.io, project_root, &g, null, .{});

    // Assert
    const lib_mod = helpers.findNode(&g, "lib_mod", .module) orelse return error.TestExpectedEqual;
    const lib_file = helpers.findNode(&g, "lib.zig", .file) orelse return error.TestExpectedEqual;
    for (g.edges.items) |e| {
        if (e.edge_type == .contains and e.source_id == lib_mod.id and e.target_id == lib_file.id) {
            try std.testing.expectEqual(EdgeSource.workspace, e.source);
            return;
        }
    }
    return error.TestExpectedEqual;
}

test "missing build.zig produces no module nodes" {
    // Arrange
    var g = Graph.init("/tmp/project_no_build");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    // Write only source files, no build.zig.
    try writeFixtureFiles(std.testing.io, tmp_dir.dir, &.{
        .{ .sub_path = "main.zig", .data = fixtures.zig.project.main_zig },
        .{ .sub_path = "parser.zig", .data = fixtures.zig.project.parser_zig },
        .{ .sub_path = "utils.zig", .data = fixtures.zig.project.utils_zig },
    });
    const project_root = try tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
    defer std.testing.allocator.free(project_root);

    // Act
    _ = try indexDirectory(IndexAllocators.single(std.testing.allocator), std.testing.io, project_root, &g, null, .{});

    // Assert
    var non_phantom_modules: usize = 0;
    for (g.nodes.items) |n| {
        if (n.kind == .module) {
            switch (n.external) {
                .none => non_phantom_modules += 1,
                else => {},
            }
        }
    }
    try std.testing.expectEqual(@as(usize, 0), non_phantom_modules);
}

test "module node has correct name" {
    // Arrange
    var g = Graph.init("/tmp/build_parsing");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupBuildParsingFixtures(&tmp_dir);
    defer std.testing.allocator.free(project_root);

    // Act
    _ = try indexDirectory(IndexAllocators.single(std.testing.allocator), std.testing.io, project_root, &g, null, .{});

    // Assert
    const lib_mod = helpers.findNode(&g, "lib_mod", .module) orelse return error.TestExpectedEqual;
    try std.testing.expectEqualStrings("lib_mod", lib_mod.name);
}
