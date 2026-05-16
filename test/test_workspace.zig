const std = @import("std");
const zcodeprism = @import("zcodeprism");
const test_helpers = @import("test-helpers");

const FrozenGraph = zcodeprism.FrozenGraph;
const Graph = zcodeprism.graph.Graph;
const Node = zcodeprism.node.Node;
const Edge = zcodeprism.edge.Edge;
const NodeId = zcodeprism.types.NodeId;
const NodeKind = zcodeprism.types.NodeKind;
const EdgeType = zcodeprism.types.EdgeType;
const Language = zcodeprism.types.Language;
const Workspace = zcodeprism.workspace.Workspace;
const WorkspaceProject = zcodeprism.workspace.WorkspaceProject;
const WorkspaceError = zcodeprism.workspace.WorkspaceError;
const AssembledWorkspace = zcodeprism.workspace.AssembledWorkspace;
const ProjectRange = zcodeprism.workspace.ProjectRange;

const parseWorkspaceConfig = zcodeprism.workspace.parseWorkspaceConfig;
const freeWorkspace = zcodeprism.workspace.freeWorkspace;
const validateWorkspace = zcodeprism.workspace.validateWorkspace;
const assembleWorkspace = zcodeprism.workspace.assembleWorkspace;
const splitPrefixedId = zcodeprism.workspace.splitPrefixedId;
const formatPrefixedId = zcodeprism.workspace.formatPrefixedId;

const findNode = test_helpers.findNode;
const countNodesByKind = test_helpers.countNodesByKind;

fn buildProjectGraph(allocator: std.mem.Allocator, root: []const u8, file_name: []const u8, func_name: []const u8) !Graph {
    var g = Graph.init(root);
    errdefer g.deinit(allocator);

    const file_id = try g.addNode(allocator, .{
        .id = .root,
        .name = file_name,
        .kind = .file,
        .language = .zig,
        .file_path = file_name,
        .visibility = .public,
    });

    _ = try g.addNode(allocator, .{
        .id = .root,
        .name = func_name,
        .kind = .function,
        .language = .zig,
        .file_path = file_name,
        .line_start = 1,
        .line_end = 5,
        .parent_id = file_id,
        .visibility = .public,
    });

    return g;
}

fn buildGraphWithEdge(allocator: std.mem.Allocator, root: []const u8) !Graph {
    var g = Graph.init(root);
    errdefer g.deinit(allocator);

    const file_id = try g.addNode(allocator, .{
        .id = .root,
        .name = "lib.zig",
        .kind = .file,
        .language = .zig,
        .file_path = "src/lib.zig",
        .visibility = .public,
    });

    const fn_a = try g.addNode(allocator, .{
        .id = .root,
        .name = "compute",
        .kind = .function,
        .language = .zig,
        .file_path = "src/lib.zig",
        .line_start = 1,
        .line_end = 10,
        .parent_id = file_id,
        .visibility = .public,
    });

    const fn_b = try g.addNode(allocator, .{
        .id = .root,
        .name = "helper",
        .kind = .function,
        .language = .zig,
        .file_path = "src/lib.zig",
        .line_start = 12,
        .line_end = 20,
        .parent_id = file_id,
        .visibility = .private,
    });

    _ = try g.addEdgeIfNew(allocator, .{
        .source_id = fn_a,
        .target_id = fn_b,
        .edge_type = .calls,
    });

    return g;
}

fn makeWs(comptime n: usize, names: [n][]const u8, paths: [n][]const u8) Workspace {
    var projects: [n]WorkspaceProject = undefined;
    for (names, paths, 0..) |name, path, i| {
        projects[i] = .{ .name = name, .path = path };
    }
    return Workspace{ .name = "ws", .projects = &projects };
}

test "parses valid workspace config with name defaulting to directory" {
    // Arrange
    const allocator = std.testing.allocator;

    const named: [:0]const u8 =
        \\.{
        \\    .name = "test-workspace",
        \\    .projects = .{
        \\        .{ .name = "alpha", .path = "project/" },
        \\        .{ .name = "beta", .path = "test_import_call/" },
        \\    },
        \\}
    ;

    const unnamed: [:0]const u8 =
        \\.{
        \\    .projects = .{
        \\        .{ .name = "self", .path = "." },
        \\    },
        \\}
    ;

    // Act
    const ws1 = try parseWorkspaceConfig(allocator, named, ".");
    defer freeWorkspace(allocator, &ws1);

    const ws2 = try parseWorkspaceConfig(allocator, unnamed, "/home/user/my-workspace");
    defer freeWorkspace(allocator, &ws2);

    // Assert -- explicit name
    try std.testing.expectEqualStrings("test-workspace", ws1.name);
    try std.testing.expectEqual(@as(usize, 2), ws1.projects.len);
    try std.testing.expectEqualStrings("alpha", ws1.projects[0].name);
    try std.testing.expectEqualStrings("beta", ws1.projects[1].name);

    // Assert -- name defaults to directory basename
    try std.testing.expectEqualStrings("my-workspace", ws2.name);
    try std.testing.expectEqualStrings(".", ws2.projects[0].path);
}

test "rejects duplicate names, colon in name, and duplicate paths" {
    // Arrange
    const allocator = std.testing.allocator;

    const cases = .{
        .{
            \\.{
            \\    .name = "ws",
            \\    .projects = .{
            \\        .{ .name = "foo", .path = "a/" },
            \\        .{ .name = "foo", .path = "b/" },
            \\    },
            \\}
            ,
            error.DuplicateProjectName,
        },
        .{
            \\.{
            \\    .name = "ws",
            \\    .projects = .{
            \\        .{ .name = "foo:bar", .path = "a/" },
            \\    },
            \\}
            ,
            error.InvalidProjectName,
        },
        .{
            \\.{
            \\    .name = "ws",
            \\    .projects = .{
            \\        .{ .name = "a", .path = "same/" },
            \\        .{ .name = "b", .path = "same/" },
            \\    },
            \\}
            ,
            error.DuplicatePath,
        },
    };

    // Act / Assert
    inline for (cases) |case| {
        const source: [:0]const u8 = case[0];
        const expected = case[1];
        const result = parseWorkspaceConfig(allocator, source, ".");
        if (result) |ws| {
            freeWorkspace(allocator, &ws);
            return error.TestUnexpectedResult;
        } else |err| {
            try std.testing.expectEqual(expected, err);
        }
    }
}

test "validateWorkspace rejects empty name, name over 32 chars, and non-existent path" {
    // Arrange
    const empty_name = Workspace{
        .name = "ws",
        .projects = &[_]WorkspaceProject{.{ .name = "", .path = "." }},
    };
    const long_name = Workspace{
        .name = "ws",
        .projects = &[_]WorkspaceProject{.{ .name = "a" ** 65, .path = "." }},
    };
    const bad_path = Workspace{
        .name = "ws",
        .projects = &[_]WorkspaceProject{.{ .name = "ghost", .path = "nonexistent_dir_xyz/" }},
    };

    // Act / Assert
    try std.testing.expectError(error.InvalidProjectName, validateWorkspace(std.testing.io, &empty_name, "."));
    try std.testing.expectError(error.InvalidProjectName, validateWorkspace(std.testing.io, &long_name, "."));
    try std.testing.expectError(error.PathNotFound, validateWorkspace(std.testing.io, &bad_path, "."));
}

test "assembled graph has virtual root, project children, all nodes, and preserved edges" {
    // Arrange
    const allocator = std.testing.allocator;
    const graph_a = try buildProjectGraph(allocator, "/a", "main.zig", "stepFn");
    const graph_with_edge = try buildGraphWithEdge(allocator, "/proj");
    var graphs = [_]Graph{ graph_a, graph_with_edge };

    const ws = Workspace{
        .name = "test-ws",
        .projects = &[_]WorkspaceProject{
            .{ .name = "alpha", .path = "a/" },
            .{ .name = "proj", .path = "proj/" },
        },
    };

    // Act
    var assembled = try assembleWorkspace(allocator, &ws, &graphs);
    defer assembled.deinit(allocator);
    _ = try assembled.graph.freeze(allocator);

    // Assert -- virtual root
    const root_node = assembled.graph.getNode(.root);
    try std.testing.expect(root_node != null);
    try std.testing.expectEqual(NodeKind.module, root_node.?.kind);
    try std.testing.expectEqualStrings("test-ws", root_node.?.name);

    // Assert -- root has 2 project children
    const children = assembled.graph.getChildren(.root);
    try std.testing.expectEqual(@as(usize, 2), children.len);

    // Assert -- total nodes: root(1) + alpha_mod(1) + alpha_file(1) + alpha_func(1)
    //                       + proj_mod(1) + proj_file(1) + proj_compute(1) + proj_helper(1) = 8
    try std.testing.expectEqual(@as(usize, 8), assembled.graph.nodeCount());

    // Assert -- the calls edge from compute -> helper still exists
    var found_calls = false;
    for (assembled.graph.edges.items) |e| {
        if (e.edge_type == .calls) {
            found_calls = true;
            break;
        }
    }
    try std.testing.expect(found_calls);
}

test "root formats as root, project nodes are prefixed, splitPrefixedId round-trips" {
    // Arrange
    const allocator = std.testing.allocator;
    const graph_a = try buildProjectGraph(allocator, "/a", "main.zig", "fn_a");
    var graphs = [_]Graph{graph_a};

    const ws = Workspace{
        .name = "ws",
        .projects = &[_]WorkspaceProject{
            .{ .name = "alpha", .path = "a/" },
        },
    };

    // Act
    var assembled = try assembleWorkspace(allocator, &ws, &graphs);
    defer assembled.deinit(allocator);

    // Assert
    var buf: [64]u8 = undefined;
    try std.testing.expectEqualStrings("root", formatPrefixedId(&buf, &assembled, .root));
    try std.testing.expectEqual(@as(?@TypeOf(splitPrefixedId(&assembled, .root).?), null), splitPrefixedId(&assembled, .root));

    const file_node = findNode(&assembled.graph, "main.zig", .file);
    try std.testing.expect(file_node != null);
    const formatted = formatPrefixedId(&buf, &assembled, file_node.?.id);
    try std.testing.expect(std.mem.startsWith(u8, formatted, "alpha:"));

    const split = splitPrefixedId(&assembled, file_node.?.id);
    try std.testing.expect(split != null);
    try std.testing.expectEqualStrings("alpha", split.?.project_name);
}

test "scope filters by project, no scope returns all, non-existent scope returns empty" {
    // Arrange
    const allocator = std.testing.allocator;
    const graph_a = try buildProjectGraph(allocator, "/a", "main.zig", "compute");
    const graph_b = try buildProjectGraph(allocator, "/b", "lib.zig", "compute");
    var graphs = [_]Graph{ graph_a, graph_b };

    const ws = Workspace{
        .name = "ws",
        .projects = &[_]WorkspaceProject{
            .{ .name = "alpha", .path = "a/" },
            .{ .name = "beta", .path = "b/" },
        },
    };

    var assembled = try assembleWorkspace(allocator, &ws, &graphs);
    defer assembled.deinit(allocator);
    _ = try assembled.graph.freeze(allocator);

    // Act
    const ws_fg = FrozenGraph{ .graph = &assembled.graph };
    const all = try zcodeprism.query.search(allocator, ws_fg, .{ .query = "compute", .kind = .function });
    defer all.deinit(allocator);

    const scoped = try zcodeprism.query.search(allocator, ws_fg, .{ .query = "compute", .scope = "alpha/", .kind = .function });
    defer scoped.deinit(allocator);

    const empty = try zcodeprism.query.search(allocator, ws_fg, .{ .scope = "nonexistent/" });
    defer empty.deinit(allocator);

    // Assert
    try std.testing.expectEqual(@as(u32, 2), all.total_matches);
    try std.testing.expectEqual(@as(u32, 1), scoped.total_matches);
    try std.testing.expectEqual(@as(u32, 0), empty.total_matches);
}

test "stats counts all projects without scope and one project with scope" {
    // Arrange
    const allocator = std.testing.allocator;
    const graph_a = try buildProjectGraph(allocator, "/a", "main.zig", "stepFn");
    const graph_b = try buildProjectGraph(allocator, "/b", "lib.zig", "scaleFn");
    var graphs = [_]Graph{ graph_a, graph_b };

    const ws = Workspace{
        .name = "ws",
        .projects = &[_]WorkspaceProject{
            .{ .name = "alpha", .path = "a/" },
            .{ .name = "beta", .path = "b/" },
        },
    };

    var assembled = try assembleWorkspace(allocator, &ws, &graphs);
    defer assembled.deinit(allocator);
    _ = try assembled.graph.freeze(allocator);

    // Act
    const ws_fg = FrozenGraph{ .graph = &assembled.graph };
    const all_stats = try zcodeprism.query.computeStats(allocator, ws_fg, .{});
    const alpha_stats = try zcodeprism.query.computeStats(allocator, ws_fg, .{ .scope = "alpha/" });

    // Assert
    try std.testing.expectEqual(@as(u32, 2), all_stats.node_counts[@intFromEnum(NodeKind.file)]);
    try std.testing.expectEqual(@as(u32, 2), all_stats.node_counts[@intFromEnum(NodeKind.function)]);
    try std.testing.expectEqual(@as(u32, 1), alpha_stats.node_counts[@intFromEnum(NodeKind.file)]);
    try std.testing.expectEqual(@as(u32, 1), alpha_stats.node_counts[@intFromEnum(NodeKind.function)]);
}

test "file paths are prefixed with project name" {
    // Arrange
    const allocator = std.testing.allocator;
    const graph_a = try buildProjectGraph(allocator, "/a", "main.zig", "fn_a");
    var graphs = [_]Graph{graph_a};

    const ws = Workspace{
        .name = "ws",
        .projects = &[_]WorkspaceProject{
            .{ .name = "alpha", .path = "a/" },
        },
    };

    var assembled = try assembleWorkspace(allocator, &ws, &graphs);
    defer assembled.deinit(allocator);

    // Assert
    const file_node = findNode(&assembled.graph, "main.zig", .file);
    try std.testing.expect(file_node != null);
    try std.testing.expectEqualStrings("alpha/main.zig", file_node.?.file_path.?);
}

test "project_ranges has one entry per project with non-overlapping spans" {
    // Arrange
    const allocator = std.testing.allocator;
    const graph_a = try buildProjectGraph(allocator, "/a", "main.zig", "fn_a");
    const graph_b = try buildProjectGraph(allocator, "/b", "lib.zig", "fn_b");
    var graphs = [_]Graph{ graph_a, graph_b };

    const ws = Workspace{
        .name = "ws",
        .projects = &[_]WorkspaceProject{
            .{ .name = "alpha", .path = "a/" },
            .{ .name = "beta", .path = "b/" },
        },
    };

    var assembled = try assembleWorkspace(allocator, &ws, &graphs);
    defer assembled.deinit(allocator);

    // Assert
    try std.testing.expectEqual(@as(usize, 2), assembled.project_ranges.len);
    try std.testing.expectEqualStrings("alpha", assembled.project_ranges[0].name);
    try std.testing.expectEqualStrings("beta", assembled.project_ranges[1].name);
    try std.testing.expect(@intFromEnum(assembled.project_ranges[0].end_id) <= @intFromEnum(assembled.project_ranges[1].start_id));
}

test "workspace types have expected fields and error count" {
    comptime {
        const err_info = @typeInfo(WorkspaceError);
        std.debug.assert(err_info == .error_set);
        std.debug.assert(err_info.error_set.?.len == 5);

        std.debug.assert(@hasField(Workspace, "name"));
        std.debug.assert(@hasField(Workspace, "projects"));
        std.debug.assert(@hasField(WorkspaceProject, "name"));
        std.debug.assert(@hasField(WorkspaceProject, "path"));
        std.debug.assert(@hasField(AssembledWorkspace, "graph"));
        std.debug.assert(@hasField(AssembledWorkspace, "project_ranges"));
        std.debug.assert(@hasField(ProjectRange, "name"));
        std.debug.assert(@hasField(ProjectRange, "start_id"));
        std.debug.assert(@hasField(ProjectRange, "end_id"));
    }
}
