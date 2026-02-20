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

const indexDirectory = zcodeprism.indexer.indexDirectory;

// ---------------------------------------------------------------------------
// Fixture setup helpers
// ---------------------------------------------------------------------------

/// Write project fixture files into a temporary directory and return the real path.
fn setupProjectFixtures(tmp_dir: *std.testing.TmpDir) ![]const u8 {
    try tmp_dir.dir.writeFile(.{ .sub_path = "main.zig", .data = fixtures.zig.project.main_zig });
    try tmp_dir.dir.writeFile(.{ .sub_path = "parser.zig", .data = fixtures.zig.project.parser_zig });
    try tmp_dir.dir.writeFile(.{ .sub_path = "utils.zig", .data = fixtures.zig.project.utils_zig });
    return try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
}

/// Index the project fixtures using default options.
fn indexProjectFixtures(graph: *Graph, tmp_dir: *std.testing.TmpDir) !zcodeprism.indexer.IndexResult {
    const project_root = try setupProjectFixtures(tmp_dir);
    defer std.testing.allocator.free(project_root);
    return indexDirectory(std.testing.allocator, project_root, graph, .{});
}

/// Write name collision fixture files into a temporary directory.
fn writeCollisionFixtures(dir: std.fs.Dir) !void {
    try dir.writeFile(.{ .sub_path = "alpha.zig", .data = fixtures.zig.name_collision.alpha_zig });
    try dir.writeFile(.{ .sub_path = "beta.zig", .data = fixtures.zig.name_collision.beta_zig });
    try dir.writeFile(.{ .sub_path = "consumer.zig", .data = fixtures.zig.name_collision.consumer_zig });
}

/// Write basic param_method_call fixtures.
fn writeBasicParamFixtures(dir: std.fs.Dir) !void {
    try dir.writeFile(.{ .sub_path = "service.zig", .data = fixtures.zig.param_method_call.service_zig });
    try dir.writeFile(.{ .sub_path = "consumer.zig", .data = fixtures.zig.param_method_call.consumer_zig });
}

/// Write service.zig + pointer_param.zig fixtures.
fn writePointerParamFixtures(dir: std.fs.Dir) !void {
    try dir.writeFile(.{ .sub_path = "service.zig", .data = fixtures.zig.param_method_call.service_zig });
    try dir.writeFile(.{ .sub_path = "pointer_param.zig", .data = fixtures.zig.param_method_call.pointer_param_zig });
}

/// Write service.zig + optional_param.zig fixtures.
fn writeOptionalParamFixtures(dir: std.fs.Dir) !void {
    try dir.writeFile(.{ .sub_path = "service.zig", .data = fixtures.zig.param_method_call.service_zig });
    try dir.writeFile(.{ .sub_path = "optional_param.zig", .data = fixtures.zig.param_method_call.optional_param_zig });
}

/// Write service.zig + client.zig + service_with_client.zig + chained.zig fixtures.
fn writeChainedParamFixtures(dir: std.fs.Dir) !void {
    try dir.writeFile(.{ .sub_path = "client.zig", .data = fixtures.zig.param_method_call.client_zig });
    try dir.writeFile(.{ .sub_path = "service_with_client.zig", .data = fixtures.zig.param_method_call.service_with_client_zig });
    try dir.writeFile(.{ .sub_path = "chained.zig", .data = fixtures.zig.param_method_call.chained_zig });
}

/// Write service.zig + client.zig + multi_param.zig fixtures.
fn writeMultiParamFixtures(dir: std.fs.Dir) !void {
    try dir.writeFile(.{ .sub_path = "service.zig", .data = fixtures.zig.param_method_call.service_zig });
    try dir.writeFile(.{ .sub_path = "client.zig", .data = fixtures.zig.param_method_call.client_zig });
    try dir.writeFile(.{ .sub_path = "multi_param.zig", .data = fixtures.zig.param_method_call.multi_param_zig });
}

/// Write service.zig + self_calls_param.zig fixtures.
fn writeSelfCallsParamFixtures(dir: std.fs.Dir) !void {
    try dir.writeFile(.{ .sub_path = "service.zig", .data = fixtures.zig.param_method_call.service_zig });
    try dir.writeFile(.{ .sub_path = "self_calls_param.zig", .data = fixtures.zig.param_method_call.self_calls_param_zig });
}

/// Write service.zig + factory.zig + return_value.zig fixtures.
fn writeReturnValueFixtures(dir: std.fs.Dir) !void {
    try dir.writeFile(.{ .sub_path = "service.zig", .data = fixtures.zig.param_method_call.service_zig });
    try dir.writeFile(.{ .sub_path = "factory.zig", .data = fixtures.zig.param_method_call.factory_zig });
    try dir.writeFile(.{ .sub_path = "return_value.zig", .data = fixtures.zig.param_method_call.return_value_zig });
}

/// Write service.zig + no_calls.zig fixtures (negative test).
fn writeNoCallsFixtures(dir: std.fs.Dir) !void {
    try dir.writeFile(.{ .sub_path = "service.zig", .data = fixtures.zig.param_method_call.service_zig });
    try dir.writeFile(.{ .sub_path = "no_calls.zig", .data = fixtures.zig.param_method_call.no_calls_zig });
}

// ===========================================================================
// Project fixture: file nodes, imports, calls, phantom nodes, metrics
// ===========================================================================

test "project fixture: file nodes, imports, calls, phantom nodes, metrics" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = indexProjectFixtures(&g, &tmp_dir) catch |err| return err;

    // --- file nodes ---
    try std.testing.expectEqual(@as(usize, 3), helpers.countNodesByKind(&g, .file));

    // --- import edges ---
    const main_file = helpers.findNode(&g, "main.zig", .file) orelse return error.TestExpectedEqual;
    const parser_file = helpers.findNode(&g, "parser.zig", .file) orelse return error.TestExpectedEqual;
    const utils_file = helpers.findNode(&g, "utils.zig", .file) orelse return error.TestExpectedEqual;
    try std.testing.expect(helpers.hasEdge(&g, main_file.id, parser_file.id, .imports));
    try std.testing.expect(helpers.hasEdge(&g, main_file.id, utils_file.id, .imports));

    // --- cross-file calls edge ---
    const process_fn = helpers.findNode(&g, "processInput", .function) orelse return error.TestExpectedEqual;
    const parse_fn = helpers.findNode(&g, "parse", .function) orelse return error.TestExpectedEqual;
    try std.testing.expect(helpers.hasEdge(&g, process_fn.id, parse_fn.id, .calls));

    // --- cross-file uses_type edge ---
    const parser_type = helpers.findNode(&g, "Parser", .type_def) orelse return error.TestExpectedEqual;
    try std.testing.expect(helpers.hasEdge(&g, process_fn.id, parser_type.id, .uses_type));

    // --- phantom nodes for std ---
    var found_stdlib = false;
    for (g.nodes.items) |n| {
        switch (n.external) {
            .stdlib => {
                found_stdlib = true;
                break;
            },
            else => {},
        }
    }
    try std.testing.expect(found_stdlib);

    // --- phantom nodes have no file_path ---
    for (g.nodes.items) |n| {
        switch (n.external) {
            .stdlib => try std.testing.expectEqual(@as(?[]const u8, null), n.file_path),
            else => {},
        }
    }

    // --- phantom nodes have no line numbers ---
    for (g.nodes.items) |n| {
        switch (n.external) {
            .stdlib => {
                try std.testing.expectEqual(@as(?u32, null), n.line_start);
                try std.testing.expectEqual(@as(?u32, null), n.line_end);
            },
            else => {},
        }
    }

    // --- phantom edges have source phantom ---
    for (g.edges.items) |e| {
        const target = g.getNode(e.target_id) orelse continue;
        switch (target.external) {
            .stdlib => try std.testing.expectEqual(EdgeSource.phantom, e.source),
            else => {},
        }
    }

    // --- file nodes have content_hash ---
    for (g.nodes.items) |n| {
        if (n.kind == .file) {
            try std.testing.expect(n.content_hash != null);
        }
    }

    // --- all non-phantom nodes have language=.zig ---
    for (g.nodes.items) |n| {
        switch (n.external) {
            .none => try std.testing.expectEqual(Language.zig, n.language),
            else => {},
        }
    }

    // --- parent_id chain is consistent ---
    for (g.nodes.items) |n| {
        if (n.parent_id) |pid| {
            try std.testing.expect(g.getNode(pid) != null);
        }
    }

    // --- no parent_id cycles ---
    for (g.nodes.items) |n| {
        var current_id: ?NodeId = n.parent_id;
        var hops: usize = 0;
        while (current_id) |cid| {
            hops += 1;
            if (hops > 100) return error.TestExpectedEqual;
            const parent = g.getNode(cid) orelse break;
            current_id = parent.parent_id;
        }
    }

    // --- metrics computed ---
    var found_complexity = false;
    for (g.nodes.items) |n| {
        if (n.kind == .function) {
            if (n.metrics) |m| {
                if (m.complexity > 0) {
                    found_complexity = true;
                    break;
                }
            }
        }
    }
    try std.testing.expect(found_complexity);

    // --- metrics lines counted ---
    for (g.nodes.items) |n| {
        if (n.kind == .function) {
            if (n.metrics) |m| {
                if (n.line_start != null and n.line_end != null and m.lines > 0) {
                    const expected = n.line_end.? - n.line_start.? + 1;
                    try std.testing.expectEqual(expected, m.lines);
                }
            }
        }
    }

    // --- utils.zig has no project import edges ---
    for (g.edges.items) |e| {
        if (e.source_id == utils_file.id and e.edge_type == .imports) {
            const target = g.getNode(e.target_id) orelse continue;
            switch (target.external) {
                .none => return error.TestExpectedEqual,
                else => {},
            }
        }
    }
}

// ===========================================================================
// Incremental indexing
// ===========================================================================

test "incremental indexing: skip unchanged, detect changes" {
    // --- skips unchanged files ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        var tmp_dir = std.testing.tmpDir(.{});
        defer tmp_dir.cleanup();
        const project_root = setupProjectFixtures(&tmp_dir) catch return error.SkipZigTest;
        defer std.testing.allocator.free(project_root);

        _ = indexDirectory(std.testing.allocator, project_root, &g, .{ .incremental = true }) catch |err| return err;
        const result2 = indexDirectory(std.testing.allocator, project_root, &g, .{ .incremental = true }) catch |err| return err;

        try std.testing.expect(result2.files_skipped > 0);
    }

    // --- detects changed file ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        var tmp_dir = std.testing.tmpDir(.{});
        defer tmp_dir.cleanup();
        const project_root = setupProjectFixtures(&tmp_dir) catch return error.SkipZigTest;
        defer std.testing.allocator.free(project_root);

        _ = indexDirectory(std.testing.allocator, project_root, &g, .{ .incremental = true }) catch |err| return err;

        try tmp_dir.dir.writeFile(.{
            .sub_path = "utils.zig",
            .data = "pub fn changed() void {}\n",
        });

        const result2 = indexDirectory(std.testing.allocator, project_root, &g, .{ .incremental = true }) catch |err| return err;

        try std.testing.expect(result2.files_indexed > 0);
    }

    // --- content_hash changes when file changes ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        var tmp_dir = std.testing.tmpDir(.{});
        defer tmp_dir.cleanup();
        const project_root = setupProjectFixtures(&tmp_dir) catch return error.SkipZigTest;
        defer std.testing.allocator.free(project_root);

        _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

        const utils_node = helpers.findNode(&g, "utils.zig", .file) orelse return error.TestExpectedEqual;
        const old_hash = utils_node.content_hash orelse return error.TestExpectedEqual;

        try tmp_dir.dir.writeFile(.{
            .sub_path = "utils.zig",
            .data = "pub fn changed() void {}\n",
        });

        var g2 = Graph.init(std.testing.allocator, "/tmp/project");
        defer g2.deinit();
        _ = indexDirectory(std.testing.allocator, project_root, &g2, .{}) catch |err| return err;

        const utils_node2 = helpers.findNode(&g2, "utils.zig", .file) orelse return error.TestExpectedEqual;
        const new_hash = utils_node2.content_hash orelse return error.TestExpectedEqual;
        try std.testing.expect(!std.mem.eql(u8, &old_hash, &new_hash));
    }
}

// ===========================================================================
// Edge cases
// ===========================================================================

test "edge cases: single file, no zig files, exclude paths" {
    // --- single file project ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        var tmp_dir = std.testing.tmpDir(.{});
        defer tmp_dir.cleanup();

        try tmp_dir.dir.writeFile(.{
            .sub_path = "single.zig",
            .data = fixtures.zig.edge_cases.project_single_file,
        });
        const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
        defer std.testing.allocator.free(project_root);

        _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

        try std.testing.expectEqual(@as(usize, 1), helpers.countNodesByKind(&g, .file));
        try std.testing.expect(g.nodeCount() > 1);
    }

    // --- directory with no zig files ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        var tmp_dir = std.testing.tmpDir(.{});
        defer tmp_dir.cleanup();

        try tmp_dir.dir.writeFile(.{
            .sub_path = "readme.txt",
            .data = "no zig here",
        });
        const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
        defer std.testing.allocator.free(project_root);

        _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

        try std.testing.expectEqual(@as(usize, 0), helpers.countNodesByKind(&g, .file));
        try std.testing.expectEqual(@as(usize, 0), g.nodeCount());
    }

    // --- respects exclude_paths ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        var tmp_dir = std.testing.tmpDir(.{});
        defer tmp_dir.cleanup();
        const project_root = setupProjectFixtures(&tmp_dir) catch return error.SkipZigTest;
        defer std.testing.allocator.free(project_root);

        const exclude = [_][]const u8{"parser.zig"};
        _ = indexDirectory(std.testing.allocator, project_root, &g, .{
            .exclude_paths = &exclude,
        }) catch |err| return err;

        try std.testing.expectEqual(@as(?*const Node, null), helpers.findNode(&g, "parser.zig", .file));
        try std.testing.expect(helpers.findNode(&g, "main.zig", .file) != null);
        try std.testing.expect(helpers.findNode(&g, "utils.zig", .file) != null);
    }
}

// ===========================================================================
// Phantom nodes: deduplication, parent chain
// ===========================================================================

test "phantom nodes: deduplication, parent chain" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    _ = indexProjectFixtures(&g, &tmp_dir) catch |err| return err;

    // --- phantom node for std.mem.Allocator exists ---
    var found_allocator = false;
    for (g.nodes.items) |n| {
        switch (n.external) {
            .stdlib => {
                if (n.kind == .type_def and std.mem.indexOf(u8, n.name, "Allocator") != null) {
                    found_allocator = true;
                    break;
                }
            },
            else => {},
        }
    }
    try std.testing.expect(found_allocator);

    // --- phantom nodes are deduplicated ---
    var alloc_count: usize = 0;
    for (g.nodes.items) |n| {
        switch (n.external) {
            .stdlib => {
                if (std.mem.indexOf(u8, n.name, "Allocator") != null) {
                    alloc_count += 1;
                }
            },
            else => {},
        }
    }
    try std.testing.expectEqual(@as(usize, 1), alloc_count);

    // --- phantom node parent chain: Allocator → mem → std ---
    var allocator_node: ?*const Node = null;
    for (g.nodes.items) |*n| {
        switch (n.external) {
            .stdlib => {
                if (n.kind == .type_def and std.mem.indexOf(u8, n.name, "Allocator") != null) {
                    allocator_node = n;
                    break;
                }
            },
            else => {},
        }
    }
    const alloc_n = allocator_node orelse return error.TestExpectedEqual;

    try std.testing.expect(alloc_n.parent_id != null);
    const mem_node = g.getNode(alloc_n.parent_id.?) orelse return error.TestExpectedEqual;
    try std.testing.expect(std.mem.indexOf(u8, mem_node.name, "mem") != null);

    try std.testing.expect(mem_node.parent_id != null);
    const std_node = g.getNode(mem_node.parent_id.?) orelse return error.TestExpectedEqual;
    try std.testing.expect(std.mem.indexOf(u8, std_node.name, "std") != null);
}

// ===========================================================================
// Name collision: cross-file init/deinit resolution
// ===========================================================================

test "name collision: cross-file init/deinit resolution" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/collision");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeCollisionFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    const alpha_file = helpers.findNode(&g, "alpha.zig", .file) orelse return error.TestExpectedEqual;
    const beta_file = helpers.findNode(&g, "beta.zig", .file) orelse return error.TestExpectedEqual;
    const consumer_file = helpers.findNode(&g, "consumer.zig", .file) orelse return error.TestExpectedEqual;

    const alpha_init = helpers.findNodeInFile(&g, "init", .function, alpha_file.id) orelse return error.TestExpectedEqual;
    const alpha_deinit = helpers.findNodeInFile(&g, "deinit", .function, alpha_file.id) orelse return error.TestExpectedEqual;
    const beta_init = helpers.findNodeInFile(&g, "init", .function, beta_file.id) orelse return error.TestExpectedEqual;
    const use_alpha = helpers.findNodeInFile(&g, "useAlpha", .function, consumer_file.id) orelse return error.TestExpectedEqual;
    const use_beta = helpers.findNodeInFile(&g, "useBeta", .function, consumer_file.id) orelse return error.TestExpectedEqual;

    // useAlpha calls alpha's init
    try std.testing.expect(helpers.hasEdge(&g, use_alpha, alpha_init, .calls));
    // useAlpha calls alpha's deinit
    try std.testing.expect(helpers.hasEdge(&g, use_alpha, alpha_deinit, .calls));
    // useBeta calls beta's init
    try std.testing.expect(helpers.hasEdge(&g, use_beta, beta_init, .calls));
    // useAlpha does NOT call beta's init
    try std.testing.expect(!helpers.hasEdge(&g, use_alpha, beta_init, .calls));
    // useBeta does NOT call alpha's init
    try std.testing.expect(!helpers.hasEdge(&g, use_beta, alpha_init, .calls));

    // Self constants are filtered
    try std.testing.expectEqual(@as(?NodeId, null), helpers.findNodeInFile(&g, "Self", .constant, alpha_file.id));
    try std.testing.expectEqual(@as(?NodeId, null), helpers.findNodeInFile(&g, "Self", .constant, beta_file.id));

    // init functions still exist in both files
    try std.testing.expect(helpers.findNodeInFile(&g, "init", .function, alpha_file.id) != null);
    try std.testing.expect(helpers.findNodeInFile(&g, "init", .function, beta_file.id) != null);
}

// ===========================================================================
// Test block: import-assigned variable method call resolution
// ===========================================================================

test "test block resolves method call on import-assigned variable" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(.{
        .sub_path = "provider.zig",
        .data = fixtures.zig.test_import_call.provider_zig,
    });
    try tmp_dir.dir.writeFile(.{
        .sub_path = "consumer.zig",
        .data = fixtures.zig.test_import_call.consumer_zig,
    });
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: the test node has a calls edge to the increment method
    const provider_file = helpers.findNode(&g, "provider.zig", .file) orelse return error.TestExpectedEqual;
    const increment_id = helpers.findNodeInFile(&g, "increment", .function, provider_file.id) orelse return error.TestExpectedEqual;

    var test_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .test_def and std.mem.eql(u8, n.name, "import var method call")) {
            test_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(test_id != null);

    try std.testing.expect(helpers.hasEdge(&g, test_id.?, increment_id, .calls));
}

// ===========================================================================
// Parameter method call: basic, pointer, optional, chained, multi, self,
//                        return value, negative
// ===========================================================================

test "parameter method call: basic parameter" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeBasicParamFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    const service_file = helpers.findNode(&g, "service.zig", .file) orelse return error.TestExpectedEqual;
    const consumer_file = helpers.findNode(&g, "consumer.zig", .file) orelse return error.TestExpectedEqual;
    const process_fn = helpers.findNodeInFile(&g, "process", .function, service_file.id) orelse return error.TestExpectedEqual;
    const reset_fn = helpers.findNodeInFile(&g, "reset", .function, service_file.id) orelse return error.TestExpectedEqual;
    const handle_fn = helpers.findNodeInFile(&g, "handle", .function, consumer_file.id) orelse return error.TestExpectedEqual;
    const service_type = helpers.findNodeInFile(&g, "Service", .type_def, service_file.id) orelse return error.TestExpectedEqual;

    // handle() calls Service.process()
    try std.testing.expect(helpers.hasEdge(&g, handle_fn, process_fn, .calls));
    // edge is calls, not uses_type
    try std.testing.expect(!helpers.hasEdge(&g, handle_fn, process_fn, .uses_type));
    // handle() also has uses_type to Service struct
    try std.testing.expect(helpers.hasEdge(&g, handle_fn, service_type, .uses_type));
    // direction is handle → process, not reverse
    try std.testing.expect(!helpers.hasEdge(&g, process_fn, handle_fn, .calls));
    // handle() does NOT call Service.reset() (only process() is called)
    try std.testing.expect(!helpers.hasEdge(&g, handle_fn, reset_fn, .calls));
}

test "parameter method call: pointer parameter" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writePointerParamFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    const service_file = helpers.findNode(&g, "service.zig", .file) orelse return error.TestExpectedEqual;
    const param_file = helpers.findNode(&g, "pointer_param.zig", .file) orelse return error.TestExpectedEqual;
    const reset_fn = helpers.findNodeInFile(&g, "reset", .function, service_file.id) orelse return error.TestExpectedEqual;
    const handle_ptr_fn = helpers.findNodeInFile(&g, "handlePtr", .function, param_file.id) orelse return error.TestExpectedEqual;
    const service_type = helpers.findNodeInFile(&g, "Service", .type_def, service_file.id) orelse return error.TestExpectedEqual;

    // handlePtr() calls Service.reset()
    try std.testing.expect(helpers.hasEdge(&g, handle_ptr_fn, reset_fn, .calls));
    // handlePtr() has uses_type to Service
    try std.testing.expect(helpers.hasEdge(&g, handle_ptr_fn, service_type, .uses_type));
}

test "parameter method call: optional parameter" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeOptionalParamFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    const service_file = helpers.findNode(&g, "service.zig", .file) orelse return error.TestExpectedEqual;
    const param_file = helpers.findNode(&g, "optional_param.zig", .file) orelse return error.TestExpectedEqual;
    const process_fn = helpers.findNodeInFile(&g, "process", .function, service_file.id) orelse return error.TestExpectedEqual;
    const handle_opt_fn = helpers.findNodeInFile(&g, "handleOpt", .function, param_file.id) orelse return error.TestExpectedEqual;

    // handleOpt() calls Service.process()
    try std.testing.expect(helpers.hasEdge(&g, handle_opt_fn, process_fn, .calls));
}

test "parameter method call: chained cross-file" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeChainedParamFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    const swc_file = helpers.findNode(&g, "service_with_client.zig", .file) orelse return error.TestExpectedEqual;
    const client_file = helpers.findNode(&g, "client.zig", .file) orelse return error.TestExpectedEqual;
    const chained_file = helpers.findNode(&g, "chained.zig", .file) orelse return error.TestExpectedEqual;
    const get_client_fn = helpers.findNodeInFile(&g, "getClient", .function, swc_file.id) orelse return error.TestExpectedEqual;
    const send_fn = helpers.findNodeInFile(&g, "send", .function, client_file.id) orelse return error.TestExpectedEqual;
    const handle_chained_fn = helpers.findNodeInFile(&g, "handleChained", .function, chained_file.id) orelse return error.TestExpectedEqual;

    // handleChained() calls ServiceWithClient.getClient()
    try std.testing.expect(helpers.hasEdge(&g, handle_chained_fn, get_client_fn, .calls));
    // handleChained() calls Client.send() transitively
    try std.testing.expect(helpers.hasEdge(&g, handle_chained_fn, send_fn, .calls));
}

test "parameter method call: multiple parameters" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeMultiParamFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    const service_file = helpers.findNode(&g, "service.zig", .file) orelse return error.TestExpectedEqual;
    const client_file = helpers.findNode(&g, "client.zig", .file) orelse return error.TestExpectedEqual;
    const multi_file = helpers.findNode(&g, "multi_param.zig", .file) orelse return error.TestExpectedEqual;
    const process_fn = helpers.findNodeInFile(&g, "process", .function, service_file.id) orelse return error.TestExpectedEqual;
    const send_fn = helpers.findNodeInFile(&g, "send", .function, client_file.id) orelse return error.TestExpectedEqual;
    const disconnect_fn = helpers.findNodeInFile(&g, "disconnect", .function, client_file.id) orelse return error.TestExpectedEqual;
    const handle_multi_fn = helpers.findNodeInFile(&g, "handleMulti", .function, multi_file.id) orelse return error.TestExpectedEqual;
    const service_type = helpers.findNodeInFile(&g, "Service", .type_def, service_file.id) orelse return error.TestExpectedEqual;
    const client_type = helpers.findNodeInFile(&g, "Client", .type_def, client_file.id) orelse return error.TestExpectedEqual;

    // calls edges to both methods
    try std.testing.expect(helpers.hasEdge(&g, handle_multi_fn, process_fn, .calls));
    try std.testing.expect(helpers.hasEdge(&g, handle_multi_fn, send_fn, .calls));
    // uses_type edges to both structs
    try std.testing.expect(helpers.hasEdge(&g, handle_multi_fn, service_type, .uses_type));
    try std.testing.expect(helpers.hasEdge(&g, handle_multi_fn, client_type, .uses_type));
    // no spurious calls edge to uncalled method
    try std.testing.expect(!helpers.hasEdge(&g, handle_multi_fn, disconnect_fn, .calls));
}

test "parameter method call: self method calling parameter method" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeSelfCallsParamFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    const service_file = helpers.findNode(&g, "service.zig", .file) orelse return error.TestExpectedEqual;
    const self_file = helpers.findNode(&g, "self_calls_param.zig", .file) orelse return error.TestExpectedEqual;
    const process_fn = helpers.findNodeInFile(&g, "process", .function, service_file.id) orelse return error.TestExpectedEqual;
    const execute_fn = helpers.findNodeInFile(&g, "execute", .function, self_file.id) orelse return error.TestExpectedEqual;
    const service_type = helpers.findNodeInFile(&g, "Service", .type_def, service_file.id) orelse return error.TestExpectedEqual;

    // Handler.execute() calls Service.process()
    try std.testing.expect(helpers.hasEdge(&g, execute_fn, process_fn, .calls));
    // Handler.execute() has uses_type to Service
    try std.testing.expect(helpers.hasEdge(&g, execute_fn, service_type, .uses_type));
}

test "parameter method call: return value from imported function" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeReturnValueFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    const factory_file = helpers.findNode(&g, "factory.zig", .file) orelse return error.TestExpectedEqual;
    const service_file = helpers.findNode(&g, "service.zig", .file) orelse return error.TestExpectedEqual;
    const rv_file = helpers.findNode(&g, "return_value.zig", .file) orelse return error.TestExpectedEqual;
    const create_fn = helpers.findNodeInFile(&g, "create", .function, factory_file.id) orelse return error.TestExpectedEqual;
    const process_fn = helpers.findNodeInFile(&g, "process", .function, service_file.id) orelse return error.TestExpectedEqual;
    const use_factory_fn = helpers.findNodeInFile(&g, "useFactory", .function, rv_file.id) orelse return error.TestExpectedEqual;

    // useFactory() calls factory.create()
    try std.testing.expect(helpers.hasEdge(&g, use_factory_fn, create_fn, .calls));
    // useFactory() calls Service.process() via return type of create()
    try std.testing.expect(helpers.hasEdge(&g, use_factory_fn, process_fn, .calls));
}

test "parameter method call: negative tests" {
    // --- no calls edge when parameter method is not called ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/param");
        defer g.deinit();
        var tmp_dir = std.testing.tmpDir(.{});
        defer tmp_dir.cleanup();
        try writeNoCallsFixtures(tmp_dir.dir);
        const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
        defer std.testing.allocator.free(project_root);

        _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

        const service_file = helpers.findNode(&g, "service.zig", .file) orelse return error.TestExpectedEqual;
        const no_calls_file = helpers.findNode(&g, "no_calls.zig", .file) orelse return error.TestExpectedEqual;
        const process_fn = helpers.findNodeInFile(&g, "process", .function, service_file.id) orelse return error.TestExpectedEqual;
        const no_calls_fn = helpers.findNodeInFile(&g, "noMethodCalls", .function, no_calls_file.id) orelse return error.TestExpectedEqual;
        const service_type = helpers.findNodeInFile(&g, "Service", .type_def, service_file.id) orelse return error.TestExpectedEqual;

        // no calls edge to process
        try std.testing.expect(!helpers.hasEdge(&g, no_calls_fn, process_fn, .calls));
        // but still has uses_type to Service
        try std.testing.expect(helpers.hasEdge(&g, no_calls_fn, service_type, .uses_type));
    }
}
