const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const types = @import("../core/types.zig");
const node_mod = @import("../core/node.zig");
const scope_mod = @import("../core/scope.zig");

const Graph = graph_mod.Graph;
const FrozenGraph = graph_mod.FrozenGraph;
const Node = node_mod.Node;
const NodeId = types.NodeId;
const NodeKind = types.NodeKind;
const Language = types.Language;
const ExternalInfo = @import("../languages/language.zig").ExternalInfo;
const Scope = scope_mod.Scope;

pub const ComplexityEntry = struct {
    node_id: NodeId,
    name: []const u8,
    file_path: ?[]const u8,
    complexity: u16,
};

pub const ComplexityResult = struct {
    nodes: []const ComplexityEntry,

    pub fn deinit(self: ComplexityResult, allocator: std.mem.Allocator) void {
        if (self.nodes.len > 0) allocator.free(self.nodes);
    }
};

pub const ComplexityOptions = struct {
    top_n: u32 = 10,
    scope: ?[]const u8 = null,
    kind: NodeKind = .function,
    language: ?Language = null,
};

/// Return the top-N most complex functions, sorted descending.
pub fn findComplex(allocator: std.mem.Allocator, fg: FrozenGraph, options: ComplexityOptions) !ComplexityResult {
    const g = fg.graph;
    const scope_filter: ?Scope = if (options.scope) |s| Scope.parse(s) else null;
    const cap = options.top_n;
    if (cap == 0) return .{ .nodes = &.{} };

    var heap_buf = try allocator.alloc(ComplexityEntry, cap);
    defer allocator.free(heap_buf);
    var heap_len: usize = 0;

    for (g.nodes.items, 0..) |n, i| {
        if (n.kind != options.kind) continue;
        if (n.external != .none) continue;
        if (options.language) |lf| {
            if (n.language == null or n.language.? != lf) continue;
        }
        const m = n.metrics orelse continue;
        if (options.kind != .file and m.complexity == 0) continue;

        if (scope_filter) |sf| {
            if (!sf.matches(n.file_path orelse continue)) continue;
        }

        const score: u16 = if (options.kind == .file)
            @intCast(@min(m.lines, std.math.maxInt(u16)))
        else
            m.complexity;

        const entry = ComplexityEntry{
            .node_id = @enumFromInt(i),
            .name = n.name,
            .file_path = n.file_path,
            .complexity = score,
        };

        if (heap_len < cap) {
            heap_buf[heap_len] = entry;
            heap_len += 1;
            siftUp(heap_buf[0..heap_len], heap_len - 1);
        } else if (score > heap_buf[0].complexity) {
            heap_buf[0] = entry;
            siftDown(heap_buf[0..heap_len], 0);
        }
    }

    if (heap_len == 0) return .{ .nodes = &.{} };

    // Extract in descending order.
    const result = try allocator.alloc(ComplexityEntry, heap_len);
    var write_pos: usize = heap_len;
    while (write_pos > 0) {
        write_pos -= 1;
        result[write_pos] = heap_buf[0];
        heap_len -= 1;
        if (heap_len > 0) {
            heap_buf[0] = heap_buf[heap_len];
            siftDown(heap_buf[0..heap_len], 0);
        }
    }

    return .{ .nodes = result };
}

fn siftUp(buf: []ComplexityEntry, start: usize) void {
    var idx = start;
    while (idx > 0) {
        const parent = (idx - 1) / 2;
        if (buf[idx].complexity < buf[parent].complexity) {
            std.mem.swap(ComplexityEntry, &buf[idx], &buf[parent]);
            idx = parent;
        } else break;
    }
}

fn siftDown(buf: []ComplexityEntry, start: usize) void {
    const len = buf.len;
    var idx = start;
    while (true) {
        var smallest = idx;
        const left = 2 * idx + 1;
        const right = 2 * idx + 2;
        if (left < len and buf[left].complexity < buf[smallest].complexity) smallest = left;
        if (right < len and buf[right].complexity < buf[smallest].complexity) smallest = right;
        if (smallest == idx) break;
        std.mem.swap(ComplexityEntry, &buf[idx], &buf[smallest]);
        idx = smallest;
    }
}
