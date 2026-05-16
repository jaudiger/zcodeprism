const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const workspace_mod = @import("../core/workspace.zig");
const binary = @import("binary.zig");

const Graph = graph_mod.Graph;

/// Open a workspace config, parse, validate, load every project graph,
/// and return the assembled `Graph`. Caller deinits with the same allocator.
pub fn loadAndAssemble(
    allocator: std.mem.Allocator,
    io: std.Io,
    ws_path: []const u8,
) !Graph {
    const ws_dir = std.fs.path.dirname(ws_path) orelse ".";

    const file = try std.Io.Dir.cwd().openFile(io, ws_path, .{});
    defer file.close(io);

    var read_buf: [4096]u8 = undefined;
    var fr = file.reader(io, &read_buf);
    const content = try fr.interface.allocRemaining(allocator, .limited(1024 * 1024));
    defer allocator.free(content);
    const content_z = try allocator.dupeZ(u8, content);
    defer allocator.free(content_z);

    const ws = try workspace_mod.parseWorkspaceConfig(allocator, content_z, ws_dir);
    defer workspace_mod.freeWorkspace(allocator, &ws);

    try workspace_mod.validateWorkspace(io, &ws, ws_dir);

    const project_graphs = try allocator.alloc(Graph, ws.projects.len);
    defer allocator.free(project_graphs);

    var loaded: usize = 0;
    {
        errdefer for (project_graphs[0..loaded]) |*g| g.deinit(allocator);
        for (ws.projects, 0..) |proj, i| {
            var path_buf: [std.fs.max_path_bytes]u8 = undefined;
            const graph_path = try std.fmt.bufPrint(
                &path_buf,
                "{s}/{s}/.zcodeprism/graph.bin",
                .{ ws_dir, proj.path },
            );
            project_graphs[i] = try binary.load(allocator, io, graph_path);
            loaded += 1;
        }
    }

    const assembled = try workspace_mod.assembleWorkspace(allocator, &ws, project_graphs);
    allocator.free(assembled.project_ranges);
    return assembled.graph;
}
