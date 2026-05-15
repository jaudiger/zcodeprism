const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const storage = @import("../storage/storage.zig");
const ctg = @import("../render/ctg.zig");
const mermaid = @import("../render/mermaid.zig");
const render_common = @import("../render/common.zig");

const Graph = graph_mod.Graph;
const FrozenGraph = graph_mod.FrozenGraph;

/// Output format selector.
pub const Format = enum { ctg, mermaid, jsonl };

/// Options for `export`.
pub const Options = struct {
    format: Format,
    /// Restrict export to nodes whose file path starts with this prefix.
    scope: ?[]const u8 = null,
    /// Include test nodes in the output.
    include_test_nodes: bool = false,
    /// Include external/phantom nodes in the output.
    include_external_nodes: bool = false,
    /// When set, load this snapshot instead of `.zcodeprism/graph.bin`.
    snapshot_tag: ?[]const u8 = null,
};

/// Load the current graph (or a named snapshot) and render it to `writer`
/// in the requested format.
pub fn run(
    allocator: std.mem.Allocator,
    io: std.Io,
    options: Options,
    writer: *std.Io.Writer,
) !void {
    var graph = if (options.snapshot_tag) |tag|
        try storage.snapshot.loadSnapshotGraph(allocator, io, tag, storage.data_dir)
    else
        try storage.binary.load(allocator, io, storage.graph_binary_path);
    defer graph.deinit(allocator);

    try renderGraph(allocator, io, &graph, options, writer);
}

/// Render `graph` to `writer` in the requested format.
pub fn renderGraph(
    allocator: std.mem.Allocator,
    io: std.Io,
    graph: *const Graph,
    options: Options,
    writer: *std.Io.Writer,
) !void {
    const fg = FrozenGraph{ .graph = @constCast(graph) };
    const project_name = projectName(io, graph.project_root);
    const filter = render_common.FilterOptions{
        .include_test_nodes = options.include_test_nodes,
        .include_external_nodes = options.include_external_nodes,
    };

    switch (options.format) {
        .ctg => {
            var buf: std.ArrayList(u8) = .empty;
            defer buf.deinit(allocator);
            try ctg.renderCtg(allocator, io, fg, .{
                .project_name = project_name,
                .scope = options.scope,
                .filter = filter,
            }, &buf);
            try writer.writeAll(buf.items);
        },
        .mermaid => {
            var buf: std.ArrayList(u8) = .empty;
            defer buf.deinit(allocator);
            try mermaid.renderMermaid(allocator, io, fg, .{
                .project_name = project_name,
                .scope = options.scope,
                .filter = filter,
            }, &buf);
            try writer.writeAll(buf.items);
        },
        .jsonl => try storage.jsonl.exportJsonl(allocator, fg, writer),
    }
}

fn projectName(io: std.Io, project_root: []const u8) []const u8 {
    const base = std.fs.path.basename(project_root);
    if (base.len > 0) return base;
    var cwd_buf: [std.fs.max_path_bytes]u8 = undefined;
    const n = std.process.currentPath(io, &cwd_buf) catch return "project";
    return std.fs.path.basename(cwd_buf[0..n]);
}
