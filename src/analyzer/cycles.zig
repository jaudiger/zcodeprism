const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const types = @import("../core/types.zig");
const node_mod = @import("../core/node.zig");
const scope_mod = @import("../core/scope.zig");

const Graph = graph_mod.Graph;
const FrozenGraph = graph_mod.FrozenGraph;
const Node = node_mod.Node;
const NodeId = types.NodeId;
const EdgeId = types.EdgeId;
const NodeKind = types.NodeKind;
const EdgeType = types.EdgeType;
const Language = types.Language;
const Scope = scope_mod.Scope;
const UNDEFINED: u32 = std.math.maxInt(u32);

pub const CycleNode = struct {
    node_id: NodeId,
    name: []const u8,
    file_path: ?[]const u8,
};

pub const Cycle = struct {
    nodes: []const CycleNode,
};

pub const CycleResult = struct {
    cycles: []const Cycle,

    pub fn deinit(self: CycleResult, allocator: std.mem.Allocator) void {
        for (self.cycles) |cycle| {
            allocator.free(cycle.nodes);
        }
        allocator.free(self.cycles);
    }
};

pub const CycleOptions = struct {
    edge_types: ?[]const EdgeType = null,
    max_cycle_length: u32 = 20,
    scope: ?[]const u8 = null,
    language: ?Language = null,
};

/// Detect dependency cycles among file nodes using Tarjan SCC.
pub fn findCycles(allocator: std.mem.Allocator, fg: FrozenGraph, options: CycleOptions) !CycleResult {
    const g = fg.graph;
    const default_types = [_]EdgeType{.imports};
    const allowed_types: []const EdgeType = options.edge_types orelse &default_types;
    const scope_filter: ?Scope = if (options.scope) |s| Scope.parse(s) else null;

    // Collect file nodes and assign them dense indices 0..file_count-1.
    var file_nodes = std.ArrayList(NodeId).empty;
    defer file_nodes.deinit(allocator);

    var node_to_dense = std.AutoHashMapUnmanaged(u64, u32){};
    defer node_to_dense.deinit(allocator);

    for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .file) {
            if (options.language) |lf| {
                if (n.language == null or n.language.? != lf) continue;
            }
            if (scope_filter) |sf| {
                if (!sf.matches(n.file_path orelse "")) continue;
            }
            const dense: u32 = @intCast(file_nodes.items.len);
            try node_to_dense.put(allocator, @as(u64, i), dense);
            try file_nodes.append(allocator, @enumFromInt(i));
        }
    }

    const file_count = file_nodes.items.len;
    if (file_count == 0) return .{ .cycles = &.{} };

    // Build a flat file-owner map for resolving edge targets.
    const file_of = try buildFileOwnerMap(allocator, g);
    defer allocator.free(file_of);

    // Build CSR adjacency for the file subgraph with deduplicated edges.
    const adj = try buildFileAdjacency(allocator, g, file_nodes.items, &node_to_dense, file_of, allowed_types);
    defer allocator.free(adj.offsets);
    defer allocator.free(adj.neighbors);

    // Iterative Tarjan SCC on dense indices.
    var tarjan = try TarjanState.init(allocator, @intCast(file_count));
    defer tarjan.deinit(allocator);

    for (0..file_count) |i| {
        if (tarjan.index_of[@intCast(i)] == UNDEFINED) {
            try tarjan.strongConnect(allocator, @intCast(i), adj);
        }
    }

    // Filter SCCs to 2+ nodes, capped at max_cycle_length.
    var cycles = std.ArrayList(Cycle).empty;
    defer cycles.deinit(allocator);

    for (tarjan.sccs.items) |scc| {
        if (scc.items.len < 2) continue;
        if (scc.items.len > options.max_cycle_length) continue;

        const cycle_nodes = try allocator.alloc(CycleNode, scc.items.len);
        errdefer allocator.free(cycle_nodes);
        for (scc.items, 0..) |dense_idx, j| {
            const nid = file_nodes.items[dense_idx];
            const n = g.getNode(nid) orelse continue;
            cycle_nodes[j] = .{
                .node_id = nid,
                .name = n.name,
                .file_path = n.file_path,
            };
        }
        try cycles.append(allocator, .{ .nodes = cycle_nodes });
    }

    if (cycles.items.len == 0) return .{ .cycles = &.{} };

    const result = try allocator.alloc(Cycle, cycles.items.len);
    @memcpy(result, cycles.items);
    return .{ .cycles = result };
}

// -- File-owner map --

/// Flat array mapping node index -> owning file node index (u32).
/// Nodes without a file ancestor get `maxInt(u32)`.
fn buildFileOwnerMap(allocator: std.mem.Allocator, g: *const Graph) ![]u32 {
    const n = g.nodes.items.len;
    const map = try allocator.alloc(u32, n);
    @memset(map, std.math.maxInt(u32));

    for (g.nodes.items, 0..) |node, i| {
        if (node.kind == .file) map[i] = @intCast(i);
    }

    for (g.nodes.items, 0..) |node, i| {
        if (map[i] != std.math.maxInt(u32)) continue;
        const pid = node.parent_id orelse continue;
        const pi = @intFromEnum(pid);
        if (pi < n) map[i] = map[pi];
    }

    return map;
}

// -- CSR adjacency for file subgraph --

const FileAdj = struct {
    offsets: []u32,
    neighbors: []u32,
};

fn buildFileAdjacency(
    allocator: std.mem.Allocator,
    g: *const Graph,
    file_nodes: []const NodeId,
    node_to_dense: *const std.AutoHashMapUnmanaged(u64, u32),
    file_of: []const u32,
    allowed_types: []const EdgeType,
) !FileAdj {
    const file_count: u32 = @intCast(file_nodes.len);

    // Collect deduplicated (source_dense, target_dense) pairs.
    var edge_set = std.AutoHashMapUnmanaged(u64, void){};
    defer edge_set.deinit(allocator);

    for (file_nodes) |file_id| {
        const src_dense = node_to_dense.get(@intFromEnum(file_id)).?;
        const out_edges = g.outEdges(file_id);
        for (out_edges) |eid| {
            const edge = g.edges.items[@intFromEnum(eid)];

            var allowed = false;
            for (allowed_types) |et| {
                if (edge.edge_type == et) {
                    allowed = true;
                    break;
                }
            }
            if (!allowed) continue;

            const tgt_idx = @intFromEnum(edge.target_id);
            if (tgt_idx >= file_of.len) continue;
            const tgt_file_raw = file_of[tgt_idx];
            if (tgt_file_raw == std.math.maxInt(u32)) continue;
            const tgt_dense = node_to_dense.get(tgt_file_raw) orelse continue;
            if (tgt_dense == src_dense) continue;

            const pair_key = (@as(u64, src_dense) << 32) | @as(u64, tgt_dense);
            try edge_set.put(allocator, pair_key, {});
        }
    }

    // Build CSR from the edge set.
    const offsets = try allocator.alloc(u32, file_count + 1);
    errdefer allocator.free(offsets);
    @memset(offsets, 0);

    var eit = edge_set.iterator();
    while (eit.next()) |entry| {
        const src: u32 = @truncate(entry.key_ptr.* >> 32);
        offsets[src + 1] += 1;
    }
    for (1..file_count + 1) |i| {
        offsets[i] += offsets[i - 1];
    }

    const total_edges = offsets[file_count];
    const neighbors = try allocator.alloc(u32, total_edges);
    errdefer allocator.free(neighbors);

    // Temporary write cursors reusing offsets shifted by one.
    const cursors = try allocator.alloc(u32, file_count);
    defer allocator.free(cursors);
    @memcpy(cursors, offsets[0..file_count]);

    eit = edge_set.iterator();
    while (eit.next()) |entry| {
        const src: u32 = @truncate(entry.key_ptr.* >> 32);
        const tgt: u32 = @truncate(entry.key_ptr.*);
        neighbors[cursors[src]] = tgt;
        cursors[src] += 1;
    }

    return .{ .offsets = offsets, .neighbors = neighbors };
}

// -- Iterative Tarjan SCC on dense indices --

const TarjanState = struct {
    index_of: []u32,
    lowlink: []u32,
    on_stack: []bool,
    stack: std.ArrayList(u32),
    sccs: std.ArrayList(std.ArrayList(u32)),
    counter: u32,

    fn init(allocator: std.mem.Allocator, n: u32) !TarjanState {
        const index_of = try allocator.alloc(u32, n);
        @memset(index_of, UNDEFINED);
        const lowlink = try allocator.alloc(u32, n);
        @memset(lowlink, 0);
        const on_stack = try allocator.alloc(bool, n);
        @memset(on_stack, false);
        return .{
            .index_of = index_of,
            .lowlink = lowlink,
            .on_stack = on_stack,
            .stack = .empty,
            .sccs = .empty,
            .counter = 0,
        };
    }

    fn deinit(self: *TarjanState, allocator: std.mem.Allocator) void {
        for (self.sccs.items) |*scc| scc.deinit(allocator);
        self.sccs.deinit(allocator);
        self.stack.deinit(allocator);
        allocator.free(self.on_stack);
        allocator.free(self.lowlink);
        allocator.free(self.index_of);
    }

    /// Iterative strongConnect using an explicit call stack.
    fn strongConnect(self: *TarjanState, allocator: std.mem.Allocator, root: u32, adj: FileAdj) !void {
        const Frame = struct { v: u32, edge_pos: u32 };
        var call_stack = std.ArrayList(Frame).empty;
        defer call_stack.deinit(allocator);

        self.index_of[root] = self.counter;
        self.lowlink[root] = self.counter;
        self.counter += 1;
        self.on_stack[root] = true;
        try self.stack.append(allocator, root);
        try call_stack.append(allocator, .{ .v = root, .edge_pos = adj.offsets[root] });

        while (call_stack.items.len > 0) {
            const frame = &call_stack.items[call_stack.items.len - 1];
            const v = frame.v;
            const end = adj.offsets[v + 1];

            if (frame.edge_pos < end) {
                const w = adj.neighbors[frame.edge_pos];
                frame.edge_pos += 1;

                if (self.index_of[w] == UNDEFINED) {
                    self.index_of[w] = self.counter;
                    self.lowlink[w] = self.counter;
                    self.counter += 1;
                    self.on_stack[w] = true;
                    try self.stack.append(allocator, w);
                    try call_stack.append(allocator, .{ .v = w, .edge_pos = adj.offsets[w] });
                } else if (self.on_stack[w]) {
                    self.lowlink[v] = @min(self.lowlink[v], self.index_of[w]);
                }
            } else {
                if (self.lowlink[v] == self.index_of[v]) {
                    var scc = std.ArrayList(u32).empty;
                    while (self.stack.pop()) |w| {
                        self.on_stack[w] = false;
                        try scc.append(allocator, w);
                        if (w == v) break;
                    }
                    try self.sccs.append(allocator, scc);
                }

                _ = call_stack.pop();
                if (call_stack.items.len > 0) {
                    const parent = call_stack.items[call_stack.items.len - 1].v;
                    self.lowlink[parent] = @min(self.lowlink[parent], self.lowlink[v]);
                }
            }
        }
    }
};
