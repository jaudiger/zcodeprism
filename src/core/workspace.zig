const std = @import("std");
const graph_mod = @import("graph.zig");
const types = @import("types.zig");
const node_mod = @import("node.zig");

const Graph = graph_mod.Graph;
const Node = node_mod.Node;
const NodeId = types.NodeId;
const NodeKind = types.NodeKind;

/// A single project within a workspace.
pub const WorkspaceProject = struct {
    /// Unique name within the workspace (used as ID prefix).
    name: []const u8,
    /// Path relative to the workspace root directory.
    path: []const u8,
};

/// Parsed workspace configuration from `zcodeprism-workspace.zon`.
pub const Workspace = struct {
    /// Display name for the workspace. Falls back to the directory name.
    name: []const u8,
    /// Projects composing the workspace.
    projects: []const WorkspaceProject,
};

/// Errors specific to workspace config parsing and validation.
pub const WorkspaceError = error{
    /// Two projects share the same name.
    DuplicateProjectName,
    /// A project name contains the reserved ':' separator.
    InvalidProjectName,
    /// Two projects resolve to the same filesystem path.
    DuplicatePath,
    /// A project path does not exist on disk.
    PathNotFound,
    /// The workspace .zon file cannot be parsed.
    InvalidConfig,
};

/// Result of assembling multiple project graphs into a workspace graph.
/// The caller owns the assembled graph and must deinit it.
pub const AssembledWorkspace = struct {
    /// Unified graph with virtual root and all project subgraphs.
    graph: Graph,
    /// Map from project name to the range of node IDs belonging to that project.
    /// Used for ID prefixing in the serialization layer.
    project_ranges: []const ProjectRange,

    pub fn deinit(self: *AssembledWorkspace, allocator: std.mem.Allocator) void {
        self.graph.deinit(allocator);
        allocator.free(self.project_ranges);
        self.* = undefined;
    }
};

/// Describes the node ID range for a project within the assembled graph.
pub const ProjectRange = struct {
    /// Project name (borrowed from workspace config).
    name: []const u8,
    /// First node ID belonging to this project (inclusive).
    start_id: NodeId,
    /// One past the last node ID belonging to this project (exclusive).
    end_id: NodeId,
};

/// Intermediate type matching the ZON schema where name is optional.
const ZonProject = struct {
    name: []const u8,
    path: []const u8,
};

const ZonWorkspace = struct {
    name: ?[]const u8 = null,
    projects: []const ZonProject,
};

/// Parse a `zcodeprism-workspace.zon` from raw text content.
/// Validates structure (names, duplicates) but not filesystem paths.
/// Caller must free via `freeWorkspace`.
pub fn parseWorkspaceConfig(
    allocator: std.mem.Allocator,
    source: [:0]const u8,
    workspace_dir: []const u8,
) (WorkspaceError || error{OutOfMemory})!Workspace {
    const parsed = std.zon.parse.fromSliceAlloc(ZonWorkspace, allocator, source, null, .{}) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ParseZon => return error.InvalidConfig,
    };
    defer std.zon.parse.free(allocator, parsed);

    const effective_name = parsed.name orelse std.fs.path.basename(workspace_dir);
    const name = allocator.dupe(u8, effective_name) catch return error.OutOfMemory;

    const projects = dupeProjects(allocator, parsed.projects) catch {
        allocator.free(name);
        return error.OutOfMemory;
    };

    const result = Workspace{ .name = name, .projects = projects };

    validateStructure(&result) catch |err| {
        freeWorkspace(allocator, &result);
        return err;
    };

    return result;
}

/// Dupe all project name/path slices into allocator-owned memory.
fn dupeProjects(allocator: std.mem.Allocator, zon_projects: []const ZonProject) error{OutOfMemory}![]WorkspaceProject {
    const projects = try allocator.alloc(WorkspaceProject, zon_projects.len);
    var filled: usize = 0;
    errdefer {
        for (projects[0..filled]) |p| {
            allocator.free(@constCast(p.name));
            allocator.free(@constCast(p.path));
        }
        allocator.free(projects);
    }

    for (zon_projects, 0..) |zp, i| {
        const pname = try allocator.dupe(u8, zp.name);
        errdefer allocator.free(pname);
        const ppath = try allocator.dupe(u8, zp.path);
        projects[i] = .{ .name = pname, .path = ppath };
        filled += 1;
    }
    return projects;
}

/// Release memory allocated by `parseWorkspaceConfig`.
pub fn freeWorkspace(allocator: std.mem.Allocator, ws: *const Workspace) void {
    for (ws.projects) |p| {
        allocator.free(@constCast(p.name));
        allocator.free(@constCast(p.path));
    }
    allocator.free(ws.projects);
    allocator.free(@constCast(ws.name));
}

/// Name constraints and uniqueness checks. No filesystem I/O.
fn validateStructure(ws: *const Workspace) WorkspaceError!void {
    for (ws.projects) |p| {
        if (p.name.len == 0 or p.name.len > 64) return error.InvalidProjectName;
        if (std.mem.indexOfScalar(u8, p.name, ':') != null) return error.InvalidProjectName;
    }

    for (ws.projects, 0..) |a, i| {
        for (ws.projects[i + 1 ..]) |b| {
            if (std.mem.eql(u8, a.name, b.name)) return error.DuplicateProjectName;
        }
    }

    for (ws.projects, 0..) |a, i| {
        for (ws.projects[i + 1 ..]) |b| {
            if (std.mem.eql(u8, a.path, b.path)) return error.DuplicatePath;
        }
    }
}

/// Structural checks plus filesystem path existence.
pub fn validateWorkspace(io: std.Io, ws: *const Workspace, workspace_dir: []const u8) WorkspaceError!void {
    try validateStructure(ws);

    var ws_dir = std.Io.Dir.cwd().openDir(io, workspace_dir, .{}) catch return error.PathNotFound;
    defer ws_dir.close(io);

    for (ws.projects) |p| {
        if (std.mem.eql(u8, p.path, ".")) continue;
        const trimmed = std.mem.trimEnd(u8, p.path, "/");
        const check_path = if (trimmed.len > 0) trimmed else ".";
        ws_dir.access(io, check_path, .{}) catch return error.PathNotFound;
    }
}

/// Assemble multiple project graphs into a unified workspace graph.
/// Each project graph is grafted under a virtual root node.
/// The input graphs are consumed (their nodes/edges are moved, not copied).
/// `project_graphs` entries must correspond 1:1 to `ws.projects`.
pub fn assembleWorkspace(
    allocator: std.mem.Allocator,
    ws: *const Workspace,
    project_graphs: []Graph,
) error{OutOfMemory}!AssembledWorkspace {
    var graph = Graph.init(ws.name);
    errdefer graph.deinit(allocator);

    _ = try graph.addNode(allocator, .{
        .id = .root,
        .name = ws.name,
        .kind = .module,
        .visibility = .public,
    });

    const project_ranges = try allocator.alloc(ProjectRange, ws.projects.len);
    errdefer allocator.free(project_ranges);

    for (ws.projects, 0..) |proj, i| {
        const proj_mod_id = try graph.addNode(allocator, .{
            .id = .root,
            .name = proj.name,
            .kind = .module,
            .parent_id = .root,
            .visibility = .public,
        });

        const offset = graph.nodeCount();

        for (project_graphs[i].nodes.items) |original_node| {
            var node = original_node;

            if (node.parent_id) |pid| {
                node.parent_id = if (pid == .root) proj_mod_id else @enumFromInt(@intFromEnum(pid) + offset);
            } else {
                node.parent_id = proj_mod_id;
            }

            if (node.file_path) |fp| {
                const prefixed = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ proj.name, fp });
                try graph.addOwnedBuffer(allocator, prefixed);
                node.file_path = prefixed;
            }

            node.id = .root;
            _ = try graph.addNode(allocator, node);
        }

        for (project_graphs[i].edges.items) |original_edge| {
            var edge = original_edge;
            edge.source_id = @enumFromInt(@intFromEnum(edge.source_id) + offset);
            edge.target_id = @enumFromInt(@intFromEnum(edge.target_id) + offset);
            _ = try graph.addEdgeIfNew(allocator, edge);
        }

        project_ranges[i] = .{
            .name = proj.name,
            .start_id = @enumFromInt(offset),
            .end_id = @enumFromInt(graph.nodeCount()),
        };

        // Transfer owned buffers, then dismantle the project graph.
        try graph.takeOwnedBuffers(allocator, &project_graphs[i]);
        project_graphs[i].deinit(allocator);
    }

    return .{ .graph = graph, .project_ranges = project_ranges };
}

/// Return the project name and local ID for a node in the assembled
/// graph. Null for the virtual root or project module nodes.
pub fn splitPrefixedId(
    assembled: *const AssembledWorkspace,
    node_id: NodeId,
) ?struct { project_name: []const u8, local_id: NodeId } {
    if (node_id == .root) return null;
    const raw = @intFromEnum(node_id);
    for (assembled.project_ranges) |range| {
        if (raw >= @intFromEnum(range.start_id) and raw < @intFromEnum(range.end_id)) {
            return .{
                .project_name = range.name,
                .local_id = @enumFromInt(raw - @intFromEnum(range.start_id)),
            };
        }
    }
    return null;
}

/// Format a node ID with its project prefix for workspace mode.
/// Root node returns "root". Project nodes return "project_name:hex_id".
pub fn formatPrefixedId(
    buf: []u8,
    assembled: *const AssembledWorkspace,
    node_id: NodeId,
) []const u8 {
    if (node_id == .root) {
        const root_str = "root";
        @memcpy(buf[0..root_str.len], root_str);
        return buf[0..root_str.len];
    }
    if (splitPrefixedId(assembled, node_id)) |split| {
        return std.fmt.bufPrint(buf, "{s}:{x}", .{ split.project_name, @intFromEnum(node_id) }) catch "";
    }
    return std.fmt.bufPrint(buf, "{x}", .{@intFromEnum(node_id)}) catch "";
}
