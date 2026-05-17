const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const workspace_mod = @import("../core/workspace.zig");
const binary = @import("binary.zig");
const indexer_mod = @import("../parser/indexer.zig");

const Graph = graph_mod.Graph;
const IndexAllocators = indexer_mod.IndexAllocators;

/// Open a workspace config, parse, validate, load every project graph,
/// and return the assembled `Graph`.
///
/// `allocs.graph` owns the returned `Graph` and every slice it borrows;
/// the caller must deinit with the same `allocs.graph`. `allocs.scratch`
/// owns transient buffers freed before this function returns.
pub fn loadAndAssemble(
    allocs: IndexAllocators,
    io: std.Io,
    ws_path: []const u8,
) !Graph {
    const ws_dir = std.fs.path.dirname(ws_path) orelse ".";

    const file = try std.Io.Dir.cwd().openFile(io, ws_path, .{});
    defer file.close(io);

    var read_buf: [4096]u8 = undefined;
    var fr = file.reader(io, &read_buf);
    const content = try fr.interface.allocRemaining(allocs.scratch, .limited(1024 * 1024));
    defer allocs.scratch.free(content);
    const content_z = try allocs.scratch.dupeZ(u8, content);
    defer allocs.scratch.free(content_z);

    const ws = try workspace_mod.parseWorkspaceConfig(allocs.scratch, content_z, ws_dir);
    defer workspace_mod.freeWorkspace(allocs.scratch, &ws);

    try workspace_mod.validateWorkspace(io, &ws, ws_dir);

    const project_graphs = try allocs.scratch.alloc(Graph, ws.projects.len);
    defer allocs.scratch.free(project_graphs);

    var loaded: usize = 0;
    {
        errdefer for (project_graphs[0..loaded]) |*g| g.deinit(allocs.graph);
        for (ws.projects, 0..) |proj, i| {
            var path_buf: [std.fs.max_path_bytes]u8 = undefined;
            const graph_path = try std.fmt.bufPrint(
                &path_buf,
                "{s}/{s}/.zcodeprism/graph.bin",
                .{ ws_dir, proj.path },
            );
            project_graphs[i] = try binary.load(allocs.graph, io, graph_path);
            loaded += 1;
        }
    }

    const assembled = try workspace_mod.assembleWorkspace(allocs.graph, &ws, project_graphs);
    allocs.graph.free(assembled.project_ranges);
    return assembled.graph;
}
