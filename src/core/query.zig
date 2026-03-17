const std = @import("std");
const graph_mod = @import("graph.zig");
const types = @import("types.zig");
const node_mod = @import("node.zig");
const edge_mod = @import("edge.zig");
const scope_mod = @import("scope.zig");
const regex_mod = @import("regex.zig");
const lang = @import("../languages/language.zig");

const Graph = graph_mod.Graph;
const Direction = graph_mod.Direction;
const Node = node_mod.Node;
const Edge = edge_mod.Edge;
const NodeId = types.NodeId;
const EdgeId = types.EdgeId;
const NodeKind = types.NodeKind;
const EdgeType = types.EdgeType;
const Visibility = types.Visibility;
const Language = types.Language;
const ExternalInfo = lang.ExternalInfo;
const Regex = regex_mod.Regex;
const Scope = scope_mod.Scope;

/// Controls how phantom/external nodes are included in results.
pub const ExternalFilter = enum {
    /// Include both internal and external nodes (default).
    include,
    /// Exclude external nodes from results.
    exclude,
    /// Return only external nodes.
    only,
};

/// Options for the `search` function.
pub const SearchOptions = struct {
    /// Regex pattern matched against node names. Supports . * + ? ^ $ \ and character classes. Null matches all.
    query: ?[]const u8 = null,
    /// Filter by semantic kind.
    kind: ?NodeKind = null,
    /// Filter by visibility.
    visibility: ?Visibility = null,
    /// Filter by programming language.
    language: ?Language = null,
    /// Controls inclusion of phantom/external nodes.
    external: ExternalFilter = .include,
    /// Include test_def nodes in results (default false).
    include_tests: bool = false,
    /// Restrict to subtree matching this scope string.
    scope: ?[]const u8 = null,
    /// Filter by minimum cyclomatic complexity.
    min_complexity: ?u16 = null,
    /// Filter by minimum line count.
    min_lines: ?u32 = null,
    /// Only return nodes that have an outgoing edge of this type.
    has_edge: ?EdgeType = null,
    /// Pagination start offset.
    offset: u32 = 0,
    /// Maximum number of results (capped at max_limit).
    limit: u32 = 50,
    /// When non-null, scan only these node IDs instead of the whole graph.
    node_ids: ?[]const NodeId = null,

    pub const max_limit: u32 = 200;
};

/// Result returned by `search`.
pub const SearchResult = struct {
    /// Total number of nodes matching all filters (before pagination).
    total_matches: u32,
    /// Node IDs for the current page. Caller owns the slice.
    nodes: []const NodeId,

    pub fn deinit(self: SearchResult, allocator: std.mem.Allocator) void {
        if (self.nodes.len > 0) allocator.free(self.nodes);
    }
};

/// Options for `findPaths`.
pub const PathOptions = struct {
    /// Restrict traversal to these edge types. Null means all types.
    edge_types: ?[]const EdgeType = null,
    /// Maximum path length.
    max_depth: u32 = 10,
    /// Maximum number of paths to return.
    max_paths: u32 = 3,

    pub const max_max_depth: u32 = 20;
    pub const max_max_paths: u32 = 10;
};

/// A single path between two nodes.
pub const Path = struct {
    /// Ordered sequence of node IDs from source to target.
    node_ids: []const NodeId,
    /// Edge types along the path (length = node_ids.len - 1).
    edge_types: []const EdgeType,
};

/// Result returned by `findPaths`.
pub const PathsResult = struct {
    /// All paths found. Caller owns the memory.
    paths: []const Path,

    pub fn deinit(self: PathsResult, allocator: std.mem.Allocator) void {
        for (self.paths) |p| {
            if (p.node_ids.len > 0) allocator.free(p.node_ids);
            if (p.edge_types.len > 0) allocator.free(p.edge_types);
        }
        if (self.paths.len > 0) allocator.free(self.paths);
    }
};

/// Options for `computeStats`.
pub const StatsOptions = struct {
    /// Restrict stats to a scope. Null means whole graph.
    scope: ?[]const u8 = null,
    /// Filter by programming language.
    language: ?Language = null,
    /// Include test_def nodes in counts.
    include_tests: bool = false,
    /// Include external/phantom nodes in counts.
    include_external: bool = false,
};

/// Aggregated statistics over (a subset of) the graph.
pub const Stats = struct {
    /// Count of nodes for each NodeKind.
    node_counts: [node_kind_count]u32 = [_]u32{0} ** node_kind_count,
    /// Count of edges for each EdgeType.
    edge_counts: [edge_type_count]u32 = [_]u32{0} ** edge_type_count,
    /// Total source lines across matched nodes.
    total_lines: u64 = 0,
    /// Whether any matched node has a given language.
    has_zig: bool = false,
    has_rust: bool = false,
    /// External node counts among matched nodes.
    stdlib_count: u32 = 0,
    dep_count: u32 = 0,
};

const node_kind_count = @typeInfo(NodeKind).@"enum".fields.len;
const edge_type_count = @typeInfo(EdgeType).@"enum".fields.len;

/// Options for `getImpact`.
pub const ImpactOptions = struct {
    /// Restrict traversal to these edge types. Null means calls + uses_type + accesses_field.
    edge_types: ?[]const EdgeType = null,
    /// Maximum traversal depth.
    max_depth: u32 = 10,
};

/// Result returned by `getImpact`.
pub const ImpactResult = struct {
    /// All transitively impacted node IDs. Caller owns the slice.
    impacted: []const NodeId,
    /// Total count of impacted nodes.
    total_impacted: u32,

    pub fn deinit(self: ImpactResult, allocator: std.mem.Allocator) void {
        if (self.impacted.len > 0) allocator.free(self.impacted);
    }
};

/// Options for `getNodes`.
pub const GetNodesOptions = struct {
    /// If true, include edge ID slices for each node.
    include_edges: bool = true,
};

/// Full detail for a single node returned by `getNodes`.
/// Borrows into the graph's node and adjacency arrays.
pub const NodeDetail = struct {
    id: NodeId,
    node: *const Node,
    in_edge_ids: []const EdgeId,
    out_edge_ids: []const EdgeId,
};

/// Result returned by `getNodes`. Caller owns the `nodes` slice.
pub const GetNodesResult = struct {
    nodes: []const NodeDetail,

    pub fn deinit(self: GetNodesResult, allocator: std.mem.Allocator) void {
        if (self.nodes.len > 0) allocator.free(self.nodes);
    }
};

/// Options for `getEdges`.
pub const GetEdgesOptions = struct {
    /// Traversal direction relative to the queried node(s).
    direction: Direction = .both,
    /// Filter by edge type. Null means all types.
    edge_type: ?EdgeType = null,
    /// Include edges that touch external (phantom) nodes.
    include_external: bool = false,
    /// Pagination start offset.
    offset: u32 = 0,
    /// Maximum number of results (capped at max_limit).
    limit: u32 = 50,

    pub const max_limit: u32 = 200;
};

/// Result returned by `getEdges`. Caller owns the `edges` slice.
pub const GetEdgesResult = struct {
    /// Total matching edges before pagination.
    total_count: u32,
    /// Paginated edge copies. Caller owns the slice.
    edges: []const Edge,

    pub fn deinit(self: GetEdgesResult, allocator: std.mem.Allocator) void {
        if (self.edges.len > 0) allocator.free(self.edges);
    }
};

// -- Private helpers --

fn isNodeExternal(n: Node) bool {
    return switch (n.external) {
        .none => false,
        .stdlib, .dependency => true,
    };
}

fn nodeInScope(file_path: ?[]const u8, scope: []const u8) bool {
    const fp = file_path orelse return false;
    return std.mem.startsWith(u8, fp, scope);
}

fn nodeMatchesSearch(
    g: *const Graph,
    n: Node,
    node_id: NodeId,
    compiled_re: ?Regex,
    scope: ?[]const u8,
    opts: SearchOptions,
) bool {
    if (!opts.include_tests and n.kind == .test_def) return false;

    switch (opts.external) {
        .include => {},
        .exclude => {
            if (isNodeExternal(n)) return false;
        },
        .only => {
            if (!isNodeExternal(n)) return false;
        },
    }

    if (opts.kind) |k| {
        if (n.kind != k) return false;
    }
    if (opts.visibility) |v| {
        if (n.visibility != v) return false;
    }
    if (opts.language) |l| {
        const nl = n.language orelse return false;
        if (nl != l) return false;
    }
    if (scope) |s| {
        if (!nodeInScope(n.file_path, s)) return false;
    }
    if (compiled_re) |re| {
        if (!re.matches(n.name)) return false;
    }
    if (opts.min_complexity) |mc| {
        const m = n.metrics orelse return false;
        if (m.complexity < mc) return false;
    }
    if (opts.min_lines) |ml| {
        const m = n.metrics orelse return false;
        if (m.lines < ml) return false;
    }
    if (opts.has_edge) |et| {
        const out = g.outEdges(node_id);
        var found = false;
        for (out) |eid| {
            if (g.edges.items[@intFromEnum(eid)].edge_type == et) {
                found = true;
                break;
            }
        }
        if (!found) return false;
    }

    return true;
}

fn edgeMatchesGetEdgesFilter(g: *const Graph, e: Edge, options: GetEdgesOptions) bool {
    if (options.edge_type) |et| {
        if (e.edge_type != et) return false;
    }
    if (!options.include_external) {
        const src_idx = @intFromEnum(e.source_id);
        const tgt_idx = @intFromEnum(e.target_id);
        if (src_idx < g.nodes.items.len and isNodeExternal(g.nodes.items[src_idx])) return false;
        if (tgt_idx < g.nodes.items.len and isNodeExternal(g.nodes.items[tgt_idx])) return false;
    }
    return true;
}

fn nodePassesStatsFilter(n: Node, scope: ?[]const u8, options: StatsOptions) bool {
    if (!options.include_tests and n.kind == .test_def) return false;
    if (!options.include_external) {
        if (isNodeExternal(n)) return false;
    }
    if (scope) |s| {
        if (!nodeInScope(n.file_path, s)) return false;
    }
    if (options.language) |l| {
        const nl = n.language orelse return false;
        if (nl != l) return false;
    }
    return true;
}

// -- Public query functions --

/// Search the graph for nodes matching the given filters. When options.node_ids is set,
/// only those nodes are scanned; otherwise the whole graph is scanned.
/// Returns a paginated result. Caller owns the returned SearchResult.
pub fn search(allocator: std.mem.Allocator, g: *const Graph, options: SearchOptions) !SearchResult {
    const effective_limit = @min(options.limit, SearchOptions.max_limit);
    const effective_scope: ?[]const u8 = if (options.scope) |s| (if (s.len == 0) null else s) else null;

    // Compile the query as a regex pattern
    var compiled_re: ?Regex = null;
    defer if (compiled_re) |re| re.deinit(allocator);
    if (options.query) |q| {
        compiled_re = Regex.compile(allocator, q) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.InvalidRegex => return .{ .total_matches = 0, .nodes = &.{} },
        };
    }

    if (options.node_ids) |ids| {
        // Measure
        var total_matches: u32 = 0;
        for (ids) |id| {
            const n = g.getNode(id) orelse continue;
            if (nodeMatchesSearch(g, n.*, id, compiled_re, effective_scope, options)) total_matches += 1;
        }

        if (total_matches == 0 or effective_limit == 0 or options.offset >= total_matches) {
            return .{ .total_matches = total_matches, .nodes = &.{} };
        }

        // Allocate + fill
        const result_count: usize = @min(effective_limit, total_matches - options.offset);
        const result = try allocator.alloc(NodeId, result_count);
        errdefer allocator.free(result);

        var skipped: u32 = 0;
        var collected: usize = 0;
        for (ids) |id| {
            const n = g.getNode(id) orelse continue;
            if (nodeMatchesSearch(g, n.*, id, compiled_re, effective_scope, options)) {
                if (skipped < options.offset) {
                    skipped += 1;
                } else {
                    result[collected] = id;
                    collected += 1;
                    if (collected >= result_count) break;
                }
            }
        }

        return .{ .total_matches = total_matches, .nodes = result[0..collected] };
    }

    // Measure
    var total_matches: u32 = 0;
    for (g.nodes.items, 0..) |n, i| {
        if (nodeMatchesSearch(g, n, @enumFromInt(i), compiled_re, effective_scope, options)) {
            total_matches += 1;
        }
    }

    if (total_matches == 0 or effective_limit == 0 or options.offset >= total_matches) {
        return .{ .total_matches = total_matches, .nodes = &.{} };
    }

    // Allocate + fill
    const result_count: usize = @min(effective_limit, total_matches - options.offset);
    const result = try allocator.alloc(NodeId, result_count);
    errdefer allocator.free(result);

    var skipped: u32 = 0;
    var collected: usize = 0;
    for (g.nodes.items, 0..) |n, i| {
        if (nodeMatchesSearch(g, n, @enumFromInt(i), compiled_re, effective_scope, options)) {
            if (skipped < options.offset) {
                skipped += 1;
            } else {
                result[collected] = @enumFromInt(i);
                collected += 1;
                if (collected >= result_count) break;
            }
        }
    }

    return .{ .total_matches = total_matches, .nodes = result[0..collected] };
}

/// Find shortest path(s) between two nodes in the graph via BFS.
/// Caller owns the returned PathsResult.
pub fn findPaths(allocator: std.mem.Allocator, g: *const Graph, from: NodeId, to: NodeId, options: PathOptions) !PathsResult {
    if (g.getNode(from) == null or g.getNode(to) == null) return .{ .paths = &.{} };

    // Same node: trivial path
    if (from == to) {
        const node_ids = try allocator.alloc(NodeId, 1);
        errdefer allocator.free(node_ids);
        node_ids[0] = from;
        const paths = try allocator.alloc(Path, 1);
        paths[0] = .{ .node_ids = node_ids, .edge_types = &.{} };
        return .{ .paths = paths };
    }

    const max_depth = @min(options.max_depth, PathOptions.max_max_depth);

    // BFS with parent tracking
    const ParentInfo = struct { parent: u64, edge_type: EdgeType };
    var visited = std.AutoHashMapUnmanaged(u64, ?ParentInfo){};
    defer visited.deinit(allocator);

    const QueueEntry = struct { node: u64, depth: u32 };
    var queue = std.ArrayList(QueueEntry){};
    defer queue.deinit(allocator);

    const from_raw = @intFromEnum(from);
    const to_raw = @intFromEnum(to);

    try visited.put(allocator, from_raw, null);
    try queue.append(allocator, .{ .node = from_raw, .depth = 0 });

    var found = false;
    var front: usize = 0;

    while (front < queue.items.len) {
        const entry = queue.items[front];
        front += 1;

        if (entry.depth >= max_depth) continue;

        const out = g.outEdges(@enumFromInt(entry.node));
        for (out) |eid| {
            const edge = g.edges.items[@intFromEnum(eid)];

            if (options.edge_types) |allowed| {
                var ok = false;
                for (allowed) |et| {
                    if (edge.edge_type == et) {
                        ok = true;
                        break;
                    }
                }
                if (!ok) continue;
            }

            const target_raw = @intFromEnum(edge.target_id);
            if (visited.contains(target_raw)) continue;

            try visited.put(allocator, target_raw, .{
                .parent = entry.node,
                .edge_type = edge.edge_type,
            });

            if (target_raw == to_raw) {
                found = true;
                break;
            }

            try queue.append(allocator, .{ .node = target_raw, .depth = entry.depth + 1 });
        }

        if (found) break;
    }

    if (!found) return .{ .paths = &.{} };

    // Reconstruct path by walking parent pointers backwards
    var path_nodes = std.ArrayList(NodeId){};
    defer path_nodes.deinit(allocator);
    var path_edges = std.ArrayList(EdgeType){};
    defer path_edges.deinit(allocator);

    var current: u64 = to_raw;
    while (true) {
        try path_nodes.append(allocator, @enumFromInt(current));
        const info = visited.get(current).? orelse break;
        try path_edges.append(allocator, info.edge_type);
        current = info.parent;
    }

    std.mem.reverse(NodeId, path_nodes.items);
    std.mem.reverse(EdgeType, path_edges.items);

    const node_ids = try allocator.dupe(NodeId, path_nodes.items);
    errdefer allocator.free(node_ids);
    const edge_types_slice = try allocator.dupe(EdgeType, path_edges.items);
    errdefer allocator.free(edge_types_slice);
    const paths = try allocator.alloc(Path, 1);
    paths[0] = .{ .node_ids = node_ids, .edge_types = edge_types_slice };

    return .{ .paths = paths };
}

/// Compute aggregated statistics over the graph (optionally scoped).
pub fn computeStats(allocator: std.mem.Allocator, g: *const Graph, options: StatsOptions) !Stats {
    _ = allocator;
    var stats = Stats{};

    const effective_scope: ?[]const u8 = if (options.scope) |s| (if (s.len == 0) null else s) else null;

    for (g.nodes.items) |n| {
        if (!nodePassesStatsFilter(n, effective_scope, options)) continue;
        stats.node_counts[@intFromEnum(n.kind)] += 1;
        if (n.metrics) |m| {
            stats.total_lines += m.lines;
        }
        if (n.language) |l| switch (l) {
            .zig => stats.has_zig = true,
            .rust => stats.has_rust = true,
        };
        switch (n.external) {
            .none => {},
            .stdlib => stats.stdlib_count += 1,
            .dependency => stats.dep_count += 1,
        }
    }

    for (g.edges.items) |e| {
        const src_idx = @intFromEnum(e.source_id);
        const tgt_idx = @intFromEnum(e.target_id);
        if (src_idx >= g.nodes.items.len or tgt_idx >= g.nodes.items.len) continue;
        const src_node = g.nodes.items[src_idx];
        const tgt_node = g.nodes.items[tgt_idx];
        if (!nodePassesStatsFilter(src_node, effective_scope, options)) continue;
        if (!nodePassesStatsFilter(tgt_node, effective_scope, options)) continue;
        stats.edge_counts[@intFromEnum(e.edge_type)] += 1;
    }

    return stats;
}

/// Return the ancestor chain from a node up to the root.
/// The result is ordered from immediate parent to root.
/// Caller owns the returned slice.
pub fn getAncestors(allocator: std.mem.Allocator, g: *const Graph, node_id: NodeId) ![]NodeId {
    // Measure
    var count: usize = 0;
    {
        var current = node_id;
        for (0..100) |_| {
            current = g.getParent(current) orelse break;
            count += 1;
        }
    }

    if (count == 0) return &.{};

    // Allocate
    const result = try allocator.alloc(NodeId, count);
    errdefer comptime unreachable;

    // Fill
    var current = node_id;
    var pos: usize = 0;
    for (0..100) |_| {
        current = g.getParent(current) orelse break;
        result[pos] = current;
        pos += 1;
    }
    std.debug.assert(pos == count);

    return result;
}

/// Compute the transitive reverse-impact set: all nodes that
/// depend on `node_id` via calls or uses_type edges.
/// Caller owns the returned ImpactResult.
pub fn getImpact(allocator: std.mem.Allocator, g: *const Graph, node_id: NodeId, options: ImpactOptions) !ImpactResult {
    if (g.getNode(node_id) == null) return .{ .impacted = &.{}, .total_impacted = 0 };

    const default_types = [_]EdgeType{ .calls, .uses_type, .accesses_field };
    const allowed_types: []const EdgeType = options.edge_types orelse &default_types;

    // Reverse BFS
    var visited = std.AutoHashMapUnmanaged(u64, void){};
    defer visited.deinit(allocator);

    const QEntry = struct { node: u64, depth: u32 };
    var queue = std.ArrayList(QEntry){};
    defer queue.deinit(allocator);

    const start_raw = @intFromEnum(node_id);
    try visited.put(allocator, start_raw, {});
    try queue.append(allocator, .{ .node = start_raw, .depth = 0 });

    var front: usize = 0;
    while (front < queue.items.len) {
        const entry = queue.items[front];
        front += 1;

        if (entry.depth >= options.max_depth) continue;

        const in_edges = g.inEdges(@enumFromInt(entry.node));
        for (in_edges) |eid| {
            const edge = g.edges.items[@intFromEnum(eid)];

            var allowed = false;
            for (allowed_types) |et| {
                if (edge.edge_type == et) {
                    allowed = true;
                    break;
                }
            }
            if (!allowed) continue;

            const source_raw = @intFromEnum(edge.source_id);
            if (visited.contains(source_raw)) continue;

            try visited.put(allocator, source_raw, {});
            try queue.append(allocator, .{ .node = source_raw, .depth = entry.depth + 1 });
        }
    }

    // Collect results (exclude start node)
    const impacted_count = visited.count() - 1;
    if (impacted_count == 0) return .{ .impacted = &.{}, .total_impacted = 0 };

    const result = try allocator.alloc(NodeId, impacted_count);
    errdefer allocator.free(result);

    var pos: usize = 0;
    var it = visited.iterator();
    while (it.next()) |entry| {
        if (entry.key_ptr.* == start_raw) continue;
        result[pos] = @enumFromInt(entry.key_ptr.*);
        pos += 1;
    }
    std.debug.assert(pos == impacted_count);

    return .{ .impacted = result, .total_impacted = @intCast(impacted_count) };
}

/// Batch lookup of nodes by ID. Returns full node details with optional
/// edge ID slices. Non-existent IDs are silently skipped.
/// Caller owns the returned GetNodesResult.
pub fn getNodes(allocator: std.mem.Allocator, g: *const Graph, node_ids: []const NodeId, options: GetNodesOptions) !GetNodesResult {
    // Measure: count valid IDs
    var count: usize = 0;
    for (node_ids) |id| {
        if (g.getNode(id) != null) count += 1;
    }

    if (count == 0) return .{ .nodes = &.{} };

    // Allocate
    const result = try allocator.alloc(NodeDetail, count);
    errdefer allocator.free(result);

    // Fill
    var pos: usize = 0;
    for (node_ids) |id| {
        const node = g.getNode(id) orelse continue;
        result[pos] = .{
            .id = id,
            .node = node,
            .in_edge_ids = if (options.include_edges) g.inEdges(id) else &.{},
            .out_edge_ids = if (options.include_edges) g.outEdges(id) else &.{},
        };
        pos += 1;
    }
    std.debug.assert(pos == count);

    return .{ .nodes = result };
}

/// Return edges connected to one or more nodes, filtered by direction,
/// edge type, and external status, with pagination.
/// Caller owns the returned GetEdgesResult.
pub fn getEdges(allocator: std.mem.Allocator, g: *const Graph, node_ids: []const NodeId, options: GetEdgesOptions) !GetEdgesResult {
    const effective_limit = @min(options.limit, GetEdgesOptions.max_limit);

    // Measure
    var total_count: u32 = 0;
    for (node_ids) |query_nid| {
        if (g.getNode(query_nid) == null) continue;

        if (options.direction == .out or options.direction == .both) {
            for (g.outEdges(query_nid)) |eid| {
                const e = g.edges.items[@intFromEnum(eid)];
                if (edgeMatchesGetEdgesFilter(g, e, options)) total_count += 1;
            }
        }
        if (options.direction == .in or options.direction == .both) {
            for (g.inEdges(query_nid)) |eid| {
                const e = g.edges.items[@intFromEnum(eid)];
                if (edgeMatchesGetEdgesFilter(g, e, options)) total_count += 1;
            }
        }
    }

    if (total_count == 0 or effective_limit == 0 or options.offset >= total_count) {
        return .{ .total_count = total_count, .edges = &.{} };
    }

    // Allocate + fill
    const result_count: usize = @min(effective_limit, total_count - options.offset);
    const result = try allocator.alloc(Edge, result_count);
    errdefer allocator.free(result);

    var skipped: u32 = 0;
    var collected: usize = 0;
    outer: for (node_ids) |query_nid| {
        if (g.getNode(query_nid) == null) continue;

        if (options.direction == .out or options.direction == .both) {
            for (g.outEdges(query_nid)) |eid| {
                const e = g.edges.items[@intFromEnum(eid)];
                if (!edgeMatchesGetEdgesFilter(g, e, options)) continue;
                if (skipped < options.offset) {
                    skipped += 1;
                    continue;
                }
                result[collected] = e;
                collected += 1;
                if (collected >= result_count) break :outer;
            }
        }
        if (options.direction == .in or options.direction == .both) {
            for (g.inEdges(query_nid)) |eid| {
                const e = g.edges.items[@intFromEnum(eid)];
                if (!edgeMatchesGetEdgesFilter(g, e, options)) continue;
                if (skipped < options.offset) {
                    skipped += 1;
                    continue;
                }
                result[collected] = e;
                collected += 1;
                if (collected >= result_count) break :outer;
            }
        }
    }

    return .{ .total_count = total_count, .edges = result[0..collected] };
}

// ---------------------------------------------------------------------------
// Test helpers
// ---------------------------------------------------------------------------

const testing = std.testing;

/// Builds a frozen test graph with known structure:
///   0: "test-project" directory (root, no parent)
///   1: "src/parser.zig" file (zig, public, parent=0)
///   2: "parse" function (zig, public, parent=1, 10 lines, complexity=5)
///   3: "Token" type_def (zig, public, parent=1, 4 lines)
///   4: "helper" function (zig, private, parent=1, 3 lines, complexity=2)
///   5: "src/main.zig" file (zig, public, parent=0)
///   6: "main" function (zig, public, parent=5, 20 lines, complexity=8)
///   7: "test_parse" test_def (zig, private, parent=1, 5 lines)
///   8: "std.mem.Allocator" type_def (zig, public, external=stdlib)
///
/// Edges: parse->Token (uses_type), main->parse (calls),
///         parse->Allocator (uses_type/phantom), main->Allocator (uses_type/phantom)
fn buildTestGraph(allocator: std.mem.Allocator) !Graph {
    var g = Graph.init("test-project");

    _ = try g.addNode(allocator, .{ .id = .root, .name = "test-project", .kind = .directory });
    _ = try g.addNode(allocator, .{ .id = .root, .name = "src/parser.zig", .kind = .file, .language = .zig, .visibility = .public, .parent_id = @enumFromInt(0), .file_path = "src/parser.zig" });
    _ = try g.addNode(allocator, .{ .id = .root, .name = "parse", .kind = .function, .language = .zig, .visibility = .public, .parent_id = @enumFromInt(1), .file_path = "src/parser.zig", .line_start = 10, .line_end = 19, .metrics = .{ .lines = 10, .complexity = 5 } });
    _ = try g.addNode(allocator, .{ .id = .root, .name = "Token", .kind = .type_def, .language = .zig, .visibility = .public, .parent_id = @enumFromInt(1), .file_path = "src/parser.zig", .line_start = 1, .line_end = 4, .metrics = .{ .lines = 4 } });
    _ = try g.addNode(allocator, .{ .id = .root, .name = "helper", .kind = .function, .language = .zig, .visibility = .private, .parent_id = @enumFromInt(1), .file_path = "src/parser.zig", .line_start = 20, .line_end = 22, .metrics = .{ .lines = 3, .complexity = 2 } });
    _ = try g.addNode(allocator, .{ .id = .root, .name = "src/main.zig", .kind = .file, .language = .zig, .visibility = .public, .parent_id = @enumFromInt(0), .file_path = "src/main.zig" });
    _ = try g.addNode(allocator, .{ .id = .root, .name = "main", .kind = .function, .language = .zig, .visibility = .public, .parent_id = @enumFromInt(5), .file_path = "src/main.zig", .line_start = 1, .line_end = 20, .metrics = .{ .lines = 20, .complexity = 8 } });
    _ = try g.addNode(allocator, .{ .id = .root, .name = "test_parse", .kind = .test_def, .language = .zig, .visibility = .private, .parent_id = @enumFromInt(1), .file_path = "src/parser.zig", .line_start = 25, .line_end = 29, .metrics = .{ .lines = 5 } });
    _ = try g.addNode(allocator, .{ .id = .root, .name = "std.mem.Allocator", .kind = .type_def, .language = .zig, .visibility = .public, .external = .{ .stdlib = {} } });

    _ = try g.addEdgeIfNew(allocator, .{ .source_id = @enumFromInt(2), .target_id = @enumFromInt(3), .edge_type = .uses_type });
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = @enumFromInt(6), .target_id = @enumFromInt(2), .edge_type = .calls });
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = @enumFromInt(2), .target_id = @enumFromInt(8), .edge_type = .uses_type, .source = .phantom });
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = @enumFromInt(6), .target_id = @enumFromInt(8), .edge_type = .uses_type, .source = .phantom });

    try g.freeze(allocator);
    return g;
}

fn nid(v: u64) NodeId {
    return @enumFromInt(v);
}

// ===========================================================================
// search -- single filters and combined
// ===========================================================================

test "search by name regex, kind, visibility, and language individually" {
    // Arrange
    var g = try buildTestGraph(testing.allocator);
    defer g.deinit(testing.allocator);

    // Act
    const by_name = try search(testing.allocator, &g, .{ .query = "parse" });
    defer by_name.deinit(testing.allocator);
    const by_kind = try search(testing.allocator, &g, .{ .kind = .function });
    defer by_kind.deinit(testing.allocator);
    const by_vis = try search(testing.allocator, &g, .{ .visibility = .public });
    defer by_vis.deinit(testing.allocator);
    const by_lang = try search(testing.allocator, &g, .{ .language = .zig });
    defer by_lang.deinit(testing.allocator);

    // Assert: name regex finds nodes containing "parse"
    try testing.expect(by_name.total_matches >= 1);
    for (by_name.nodes) |id| {
        try testing.expect(std.mem.indexOf(u8, g.getNode(id).?.name, "parse") != null);
    }

    // Assert: kind filter returns only functions
    try testing.expect(by_kind.total_matches >= 1);
    for (by_kind.nodes) |id| {
        try testing.expectEqual(NodeKind.function, g.getNode(id).?.kind);
    }

    // Assert: visibility filter returns only public
    try testing.expect(by_vis.total_matches >= 1);
    for (by_vis.nodes) |id| {
        try testing.expectEqual(Visibility.public, g.getNode(id).?.visibility);
    }

    // Assert: language filter returns only zig
    try testing.expect(by_lang.total_matches >= 1);
    for (by_lang.nodes) |id| {
        try testing.expectEqual(Language.zig, g.getNode(id).?.language.?);
    }
}

test "search combines filters into intersection" {
    // Arrange
    var g = try buildTestGraph(testing.allocator);
    defer g.deinit(testing.allocator);

    // Act: public zig functions matching "parse"
    const result = try search(testing.allocator, &g, .{
        .query = "parse",
        .kind = .function,
        .visibility = .public,
    });
    defer result.deinit(testing.allocator);

    // Assert: only "parse" matches (test_parse is test_def + private)
    try testing.expectEqual(@as(u32, 1), result.total_matches);
    try testing.expectEqualStrings("parse", g.getNode(result.nodes[0]).?.name);
}

// ===========================================================================
// search -- external filter variants
// ===========================================================================

test "search external filter: include, exclude, only" {
    // Arrange
    var g = try buildTestGraph(testing.allocator);
    defer g.deinit(testing.allocator);

    // Act
    const incl = try search(testing.allocator, &g, .{ .kind = .type_def, .external = .include });
    defer incl.deinit(testing.allocator);
    const excl = try search(testing.allocator, &g, .{ .kind = .type_def, .external = .exclude });
    defer excl.deinit(testing.allocator);
    const only = try search(testing.allocator, &g, .{ .external = .only });
    defer only.deinit(testing.allocator);

    // Assert include: both Token and std.mem.Allocator
    try testing.expect(incl.total_matches >= 2);

    // Assert exclude: no phantom nodes
    for (excl.nodes) |id| {
        try testing.expectEqual(ExternalInfo.none, g.getNode(id).?.external);
    }

    // Assert only: all results are external
    try testing.expect(only.total_matches >= 1);
    for (only.nodes) |id| {
        try testing.expect(g.getNode(id).?.external != .none);
    }
}

// ===========================================================================
// search -- pagination
// ===========================================================================

test "search pagination: offset, limit, boundaries" {
    // Arrange
    var g = try buildTestGraph(testing.allocator);
    defer g.deinit(testing.allocator);

    // Act
    const all = try search(testing.allocator, &g, .{ .limit = 200 });
    defer all.deinit(testing.allocator);
    const beyond = try search(testing.allocator, &g, .{ .offset = 9999 });
    defer beyond.deinit(testing.allocator);
    const zero_lim = try search(testing.allocator, &g, .{ .limit = 0 });
    defer zero_lim.deinit(testing.allocator);

    // Assert: max limit returns up to 200
    try testing.expect(all.nodes.len <= 200);

    // Assert: offset beyond total gives empty page
    try testing.expectEqual(@as(usize, 0), beyond.nodes.len);

    // Assert: limit=0 gives empty page but total_matches still computed
    try testing.expectEqual(@as(usize, 0), zero_lim.nodes.len);

    // Assert: paginated subset preserves total_matches
    if (all.total_matches > 2) {
        const page = try search(testing.allocator, &g, .{ .offset = 1, .limit = 2 });
        defer page.deinit(testing.allocator);
        try testing.expectEqual(all.total_matches, page.total_matches);
        try testing.expect(page.nodes.len <= 2);
    }
}

// ===========================================================================
// search -- empty/boundary
// ===========================================================================

test "search on empty graph and with no matches" {
    // Arrange: empty graph
    var empty = Graph.init("empty");
    defer empty.deinit(testing.allocator);
    try empty.freeze(testing.allocator);

    const empty_result = try search(testing.allocator, &empty, .{});
    defer empty_result.deinit(testing.allocator);

    try testing.expectEqual(@as(u32, 0), empty_result.total_matches);
    try testing.expectEqual(@as(usize, 0), empty_result.nodes.len);

    // Arrange: populated graph, impossible query
    var g = try buildTestGraph(testing.allocator);
    defer g.deinit(testing.allocator);

    const no_match = try search(testing.allocator, &g, .{ .query = "nonexistent_xyz_42" });
    defer no_match.deinit(testing.allocator);

    try testing.expectEqual(@as(u32, 0), no_match.total_matches);

    // Escaped dot matches literal dot via regex
    const regex = try search(testing.allocator, &g, .{ .query = "std\\.mem" });
    defer regex.deinit(testing.allocator);
    try testing.expect(regex.total_matches >= 1);

    // Regex with invalid pattern returns zero results
    const invalid = try search(testing.allocator, &g, .{ .query = "*invalid" });
    defer invalid.deinit(testing.allocator);
    try testing.expectEqual(@as(u32, 0), invalid.total_matches);

    // Regex metacharacters actually work
    const dot_star = try search(testing.allocator, &g, .{ .query = "p.*se" });
    defer dot_star.deinit(testing.allocator);
    try testing.expect(dot_star.total_matches >= 1);
    for (dot_star.nodes) |id| {
        const name = g.getNode(id).?.name;
        try testing.expect(std.mem.indexOf(u8, name, "p") != null);
    }

    // Anchored regex for exact match
    const exact = try search(testing.allocator, &g, .{ .query = "^parse$" });
    defer exact.deinit(testing.allocator);
    try testing.expectEqual(@as(u32, 1), exact.total_matches);
    try testing.expectEqualStrings("parse", g.getNode(exact.nodes[0]).?.name);
}

// ===========================================================================
// search -- scope integration
// ===========================================================================

test "search with scope: prefix, empty, no matches" {
    // Arrange
    var g = try buildTestGraph(testing.allocator);
    defer g.deinit(testing.allocator);

    // Act
    const scoped = try search(testing.allocator, &g, .{ .scope = "src/parser" });
    defer scoped.deinit(testing.allocator);
    const empty_scope = try search(testing.allocator, &g, .{ .scope = "" });
    defer empty_scope.deinit(testing.allocator);
    const unscoped = try search(testing.allocator, &g, .{});
    defer unscoped.deinit(testing.allocator);
    const dead_scope = try search(testing.allocator, &g, .{ .scope = "nonexistent/" });
    defer dead_scope.deinit(testing.allocator);

    // Assert: prefix scope restricts to matching files
    for (scoped.nodes) |id| {
        const n = g.getNode(id).?;
        if (n.file_path) |fp| {
            try testing.expect(std.mem.startsWith(u8, fp, "src/parser"));
        }
    }

    // Assert: empty scope matches everything
    try testing.expectEqual(unscoped.total_matches, empty_scope.total_matches);

    // Assert: impossible scope gives zero results
    try testing.expectEqual(@as(u32, 0), dead_scope.total_matches);
}

// ===========================================================================
// findPaths
// ===========================================================================

test "findPaths: direct, multi-hop, and unconnected" {
    // Arrange
    var g = try buildTestGraph(testing.allocator);
    defer g.deinit(testing.allocator);

    // Act: direct path main(6) -> parse(2)
    const direct = try findPaths(testing.allocator, &g, nid(6), nid(2), .{});
    defer direct.deinit(testing.allocator);

    // Act: multi-hop main(6) -> parse(2) -> Token(3)
    const multi = try findPaths(testing.allocator, &g, nid(6), nid(3), .{});
    defer multi.deinit(testing.allocator);

    // Act: no path helper(4) -> main(6)
    const none = try findPaths(testing.allocator, &g, nid(4), nid(6), .{});
    defer none.deinit(testing.allocator);

    // Assert: direct path found with 2 nodes
    try testing.expect(direct.paths.len >= 1);
    try testing.expectEqual(@as(usize, 2), direct.paths[0].node_ids.len);
    try testing.expectEqual(nid(6), direct.paths[0].node_ids[0]);
    try testing.expectEqual(nid(2), direct.paths[0].node_ids[1]);

    // Assert: multi-hop path found with 3+ nodes
    try testing.expect(multi.paths.len >= 1);
    try testing.expect(multi.paths[0].node_ids.len >= 3);

    // Assert: no path between unconnected nodes
    try testing.expectEqual(@as(usize, 0), none.paths.len);
}

test "findPaths: max_depth and edge_types restrict results" {
    // Arrange
    var g = try buildTestGraph(testing.allocator);
    defer g.deinit(testing.allocator);

    // Act: max_depth=1 cannot reach Token(3) from main(6) (needs 2 hops)
    const shallow = try findPaths(testing.allocator, &g, nid(6), nid(3), .{ .max_depth = 1 });
    defer shallow.deinit(testing.allocator);

    // Act: calls-only cannot reach Token(3) since parse->Token is uses_type
    const only_calls = [_]EdgeType{.calls};
    const filtered = try findPaths(testing.allocator, &g, nid(6), nid(3), .{ .edge_types = &only_calls });
    defer filtered.deinit(testing.allocator);

    // Assert
    try testing.expectEqual(@as(usize, 0), shallow.paths.len);
    try testing.expectEqual(@as(usize, 0), filtered.paths.len);
}

test "findPaths: same node and non-existent node" {
    // Arrange
    var g = try buildTestGraph(testing.allocator);
    defer g.deinit(testing.allocator);

    // Act
    const self_path = try findPaths(testing.allocator, &g, nid(2), nid(2), .{});
    defer self_path.deinit(testing.allocator);
    const bad = try findPaths(testing.allocator, &g, nid(9999), nid(2), .{});
    defer bad.deinit(testing.allocator);

    // Assert: self-path is trivial or empty
    if (self_path.paths.len > 0) {
        try testing.expect(self_path.paths[0].node_ids.len <= 1);
    }

    // Assert: non-existent source gives empty result
    try testing.expectEqual(@as(usize, 0), bad.paths.len);
}

// ===========================================================================
// getChildren / getAncestors
// ===========================================================================

test "getChildren returns direct children, leaf returns empty" {
    // Arrange
    var g = try buildTestGraph(testing.allocator);
    defer g.deinit(testing.allocator);

    // Assert: root(0) has file children
    const root_children = g.getChildren(nid(0));
    try testing.expect(root_children.len >= 2);
    for (root_children) |cid| {
        try testing.expectEqual(NodeKind.file, g.getNode(cid).?.kind);
    }

    // Assert: leaf node helper(4) has no children
    try testing.expectEqual(@as(usize, 0), g.getChildren(nid(4)).len);
}

test "getAncestors returns full chain and empty for root" {
    // Arrange
    var g = try buildTestGraph(testing.allocator);
    defer g.deinit(testing.allocator);

    // Act: parse(2) has parent=file(1), grandparent=root(0)
    const ancestors = try getAncestors(testing.allocator, &g, nid(2));
    defer if (ancestors.len > 0) testing.allocator.free(ancestors);

    // Assert
    try testing.expectEqual(@as(usize, 2), ancestors.len);
    try testing.expectEqual(nid(1), ancestors[0]);
    try testing.expectEqual(nid(0), ancestors[1]);

    // Act: root(0) has no parent
    const root_anc = try getAncestors(testing.allocator, &g, nid(0));
    defer if (root_anc.len > 0) testing.allocator.free(root_anc);

    // Assert
    try testing.expectEqual(@as(usize, 0), root_anc.len);
}

// ===========================================================================
// getImpact
// ===========================================================================

test "impact: leaf has zero, core function includes callers" {
    // Arrange
    var g = try buildTestGraph(testing.allocator);
    defer g.deinit(testing.allocator);

    // Act
    const leaf = try getImpact(testing.allocator, &g, nid(4), .{});
    defer leaf.deinit(testing.allocator);
    const core = try getImpact(testing.allocator, &g, nid(2), .{});
    defer core.deinit(testing.allocator);

    // Assert: helper(4) has no dependents
    try testing.expectEqual(@as(u32, 0), leaf.total_impacted);

    // Assert: parse(2) is called by main(6)
    try testing.expect(core.total_impacted >= 1);
    var found_main = false;
    for (core.impacted) |id| {
        if (@intFromEnum(id) == 6) found_main = true;
    }
    try testing.expect(found_main);
}

test "impact is transitive and works on phantom nodes" {
    // Arrange
    var g = try buildTestGraph(testing.allocator);
    defer g.deinit(testing.allocator);

    // Act: Token(3) used by parse(2), parse called by main(6)
    const token_impact = try getImpact(testing.allocator, &g, nid(3), .{});
    defer token_impact.deinit(testing.allocator);

    // Act: std.mem.Allocator(8) used by parse(2) and main(6)
    const phantom_impact = try getImpact(testing.allocator, &g, nid(8), .{});
    defer phantom_impact.deinit(testing.allocator);

    // Assert: Token impacts both parse and main transitively
    try testing.expect(token_impact.total_impacted >= 2);

    // Assert: phantom node impacts its users
    try testing.expect(phantom_impact.total_impacted >= 2);
}

test "impact respects edge_types filter" {
    // Arrange
    var g = try buildTestGraph(testing.allocator);
    defer g.deinit(testing.allocator);

    // Act: Token(3) only has uses_type incoming, not calls
    const only_calls = [_]EdgeType{.calls};
    const result = try getImpact(testing.allocator, &g, nid(3), .{ .edge_types = &only_calls });
    defer result.deinit(testing.allocator);

    // Assert
    try testing.expectEqual(@as(u32, 0), result.total_impacted);
}

// ===========================================================================
// computeStats
// ===========================================================================

test "stats on full graph counts nodes, edges, and lines" {
    // Arrange
    var g = try buildTestGraph(testing.allocator);
    defer g.deinit(testing.allocator);

    // Act
    const stats = try computeStats(testing.allocator, &g, .{ .include_tests = true, .include_external = true });

    // Assert
    try testing.expect(stats.node_counts[@intFromEnum(NodeKind.function)] >= 3);
    try testing.expect(stats.node_counts[@intFromEnum(NodeKind.type_def)] >= 2);
    try testing.expect(stats.edge_counts[@intFromEnum(EdgeType.calls)] >= 1);
    try testing.expect(stats.edge_counts[@intFromEnum(EdgeType.uses_type)] >= 1);
    try testing.expect(stats.total_lines > 0);
}

test "stats with scope restricts counts" {
    // Arrange
    var g = try buildTestGraph(testing.allocator);
    defer g.deinit(testing.allocator);

    // Act
    const stats = try computeStats(testing.allocator, &g, .{ .scope = "src/main" });

    // Assert: only main's function
    try testing.expectEqual(@as(u32, 1), stats.node_counts[@intFromEnum(NodeKind.function)]);
}

test "stats on empty graph returns all zeros" {
    // Arrange
    var g = Graph.init("empty");
    defer g.deinit(testing.allocator);
    try g.freeze(testing.allocator);

    // Act
    const stats = try computeStats(testing.allocator, &g, .{});

    // Assert
    for (stats.node_counts) |c| try testing.expectEqual(@as(u32, 0), c);
    for (stats.edge_counts) |c| try testing.expectEqual(@as(u32, 0), c);
    try testing.expectEqual(@as(u64, 0), stats.total_lines);
}

// ===========================================================================
// getNodes
// ===========================================================================

test "getNodes returns details, skips invalid IDs, respects include_edges" {
    // Arrange
    var g = try buildTestGraph(testing.allocator);
    defer g.deinit(testing.allocator);

    // Act: batch lookup with one invalid ID
    const ids = [_]NodeId{ nid(2), nid(9999), nid(6) };
    const result = try getNodes(testing.allocator, &g, &ids, .{});
    defer result.deinit(testing.allocator);

    // Assert: only 2 valid nodes returned with edges populated
    try testing.expectEqual(@as(usize, 2), result.nodes.len);
    try testing.expectEqualStrings("parse", result.nodes[0].node.name);
    try testing.expectEqualStrings("main", result.nodes[1].node.name);
    try testing.expect(result.nodes[0].out_edge_ids.len > 0);

    // Act: same query with edges disabled
    const no_edges = try getNodes(testing.allocator, &g, &ids, .{ .include_edges = false });
    defer no_edges.deinit(testing.allocator);

    // Assert: nodes returned but edge slices are empty
    try testing.expectEqual(@as(usize, 2), no_edges.nodes.len);
    try testing.expectEqual(@as(usize, 0), no_edges.nodes[0].in_edge_ids.len);
    try testing.expectEqual(@as(usize, 0), no_edges.nodes[0].out_edge_ids.len);

    // Act: empty input
    const empty = try getNodes(testing.allocator, &g, &.{}, .{});
    defer empty.deinit(testing.allocator);

    // Assert
    try testing.expectEqual(@as(usize, 0), empty.nodes.len);
}

// ===========================================================================
// getEdges
// ===========================================================================

test "getEdges filters by direction, edge_type, and include_external" {
    // Arrange
    var g = try buildTestGraph(testing.allocator);
    defer g.deinit(testing.allocator);
    const ids = [_]NodeId{nid(2)};

    // Act: outgoing only
    const out_only = try getEdges(testing.allocator, &g, &ids, .{ .direction = .out, .include_external = true });
    defer out_only.deinit(testing.allocator);

    // Assert: parse(2) has outgoing uses_type edges to Token and Allocator
    try testing.expect(out_only.total_count >= 2);
    for (out_only.edges) |e| try testing.expectEqual(nid(2), e.source_id);

    // Act: incoming only
    const in_only = try getEdges(testing.allocator, &g, &ids, .{ .direction = .in, .include_external = true });
    defer in_only.deinit(testing.allocator);

    // Assert: parse(2) has incoming calls from main(6)
    try testing.expect(in_only.total_count >= 1);
    for (in_only.edges) |e| try testing.expectEqual(nid(2), e.target_id);

    // Act: uses_type out, exclude external
    const uses_no_ext = try getEdges(testing.allocator, &g, &ids, .{ .direction = .out, .edge_type = .uses_type, .include_external = false });
    defer uses_no_ext.deinit(testing.allocator);

    // Act: uses_type out, include external
    const uses_with_ext = try getEdges(testing.allocator, &g, &ids, .{ .direction = .out, .edge_type = .uses_type, .include_external = true });
    defer uses_with_ext.deinit(testing.allocator);

    // Assert: without external only parse->Token, with external also parse->Allocator
    try testing.expectEqual(@as(u32, 1), uses_no_ext.total_count);
    try testing.expect(uses_with_ext.total_count >= 2);

    // Act: non-existent node
    const bad_ids = [_]NodeId{nid(9999)};
    const bad = try getEdges(testing.allocator, &g, &bad_ids, .{});
    defer bad.deinit(testing.allocator);

    // Assert
    try testing.expectEqual(@as(u32, 0), bad.total_count);
}

// ===========================================================================
// Compile-time type assertions
// ===========================================================================

test "type invariants" {
    comptime {
        std.debug.assert(@typeInfo(ExternalFilter).@"enum".fields.len == 3);
        const s = Stats{};
        std.debug.assert(s.node_counts.len == @typeInfo(NodeKind).@"enum".fields.len);
        std.debug.assert(s.edge_counts.len == @typeInfo(EdgeType).@"enum".fields.len);
    }
}
