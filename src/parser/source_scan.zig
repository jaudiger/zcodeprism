const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const types = @import("../core/types.zig");
const phantom_mod = @import("../core/phantom.zig");
const metrics_mod = @import("../core/metrics.zig");

const Graph = graph_mod.Graph;
const NodeId = types.NodeId;
const NodeKind = types.NodeKind;
const EdgeType = types.EdgeType;
const EdgeSource = types.EdgeSource;
const PhantomManager = phantom_mod.PhantomManager;
const Metrics = metrics_mod.Metrics;

pub fn isIdentChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_';
}

pub fn matchKeyword(text: []const u8, keyword: []const u8) bool {
    if (text.len < keyword.len) return false;
    if (!std.mem.startsWith(u8, text, keyword)) return false;
    if (text.len > keyword.len and isIdentChar(text[keyword.len])) return false;
    return true;
}

pub fn extractLineRange(source: []const u8, line_start: u32, line_end: u32) []const u8 {
    if (source.len == 0) return source;
    var current_line: u32 = 1;
    var start_byte: usize = 0;
    var found_start: bool = (line_start <= 1);

    for (source, 0..) |c, i| {
        if (c == '\n') {
            if (found_start and current_line >= line_end) {
                return source[start_byte .. i + 1];
            }
            current_line += 1;
            if (!found_start and current_line >= line_start) {
                start_byte = i + 1;
                found_start = true;
            }
        }
    }

    if (found_start) return source[start_byte..];
    return source;
}

pub fn isDescendantOf(graph: *const Graph, node_id: NodeId, ancestor_id: NodeId) bool {
    var current = node_id;
    var hops: usize = 0;
    while (hops < 100) : (hops += 1) {
        const node = graph.getNode(current) orelse return false;
        const pid = node.parent_id orelse return false;
        if (pid == ancestor_id) return true;
        current = pid;
    }
    return false;
}

pub fn addEdgeIfNew(graph: *Graph, source_id: NodeId, target_id: NodeId, edge_type: EdgeType, edge_source: EdgeSource) !void {
    for (graph.edges.items) |e| {
        if (e.source_id == source_id and e.target_id == target_id and e.edge_type == edge_type) return;
    }
    _ = try graph.addEdge(.{
        .source_id = source_id,
        .target_id = target_id,
        .edge_type = edge_type,
        .source = edge_source,
    });
}

pub fn computeMetricsForNodes(graph: *Graph, source: []const u8, file_idx: usize) void {
    const file_id: NodeId = @enumFromInt(file_idx);
    var idx: usize = file_idx;
    while (idx < graph.nodes.items.len) : (idx += 1) {
        var n = &graph.nodes.items[idx];
        if (n.kind != .function) continue;
        if (!isDescendantOf(graph, @enumFromInt(idx), file_id)) continue;
        const ls = n.line_start orelse continue;
        const le = n.line_end orelse continue;

        const lines: u32 = le - ls + 1;
        const fn_source = extractLineRange(source, ls, le);

        var complexity: u16 = 1; // base complexity
        var pos: usize = 0;
        while (pos < fn_source.len) : (pos += 1) {
            if (pos > 0 and isIdentChar(fn_source[pos - 1])) continue;
            const remaining = fn_source[pos..];
            if (matchKeyword(remaining, "if") or
                matchKeyword(remaining, "for") or
                matchKeyword(remaining, "while") or
                matchKeyword(remaining, "switch") or
                matchKeyword(remaining, "catch"))
            {
                complexity += 1;
            }
        }

        n.metrics = Metrics{ .complexity = complexity, .lines = lines };
    }
}

pub fn countLinesUpTo(source: []const u8, byte_pos: usize) u32 {
    var line: u32 = 1;
    const end = @min(byte_pos, source.len);
    for (source[0..end]) |c| {
        if (c == '\n') line += 1;
    }
    return line;
}

pub fn findContainingFunction(graph: *const Graph, file_id: NodeId, line: u32) ?NodeId {
    for (graph.nodes.items, 0..) |n, i| {
        if (n.kind != .function) continue;
        if (!isDescendantOf(graph, @enumFromInt(i), file_id)) continue;
        const ls = n.line_start orelse continue;
        const le = n.line_end orelse continue;
        if (line >= ls and line <= le) return @enumFromInt(i);
    }
    return null;
}

/// Check whether position `pos` in `source` falls inside a line comment.
/// Scans backwards from `pos` to the start of the current line looking for `//`.
fn isInsideLineComment(source: []const u8, pos: usize) bool {
    var i: usize = pos;
    while (i > 0) {
        i -= 1;
        if (source[i] == '\n') break;
        if (i > 0 and source[i - 1] == '/' and source[i] == '/') return true;
    }
    // Check if the line starts with "//"
    if (i == 0 and source.len > 1 and source[0] == '/' and source[1] == '/') return true;
    return false;
}

pub fn resolveStdPhantoms(
    graph: *Graph,
    source: []const u8,
    file_idx: usize,
    phantom: *PhantomManager,
    std_name: []const u8,
) !void {
    const file_id: NodeId = @enumFromInt(file_idx);

    var pos: usize = 0;
    while (pos < source.len) {
        // Look for std_name followed by '.'
        if (pos + std_name.len < source.len and
            std.mem.startsWith(u8, source[pos..], std_name) and
            source[pos + std_name.len] == '.')
        {
            // Check word boundary before.
            if (pos > 0 and isIdentChar(source[pos - 1])) {
                pos += 1;
                continue;
            }

            // Skip matches inside line comments (//, ///, //!).
            if (isInsideLineComment(source, pos)) {
                pos += 1;
                continue;
            }

            // Collect the full chain: std.X.Y.Z
            const chain_start = pos;
            pos += std_name.len + 1; // skip "std."
            while (pos < source.len and (isIdentChar(source[pos]) or source[pos] == '.')) {
                pos += 1;
            }
            // Remove trailing dot if any.
            var chain_end = pos;
            if (chain_end > chain_start and source[chain_end - 1] == '.') {
                chain_end -= 1;
            }

            const chain = source[chain_start..chain_end];

            // Skip if just "std" with no further segments.
            if (chain.len <= std_name.len + 1) continue;

            // Determine the kind of the leaf segment.
            const last_dot = std.mem.lastIndexOfScalar(u8, chain, '.') orelse continue;
            const leaf = chain[last_dot + 1 ..];
            if (leaf.len == 0) continue;

            const kind: NodeKind = if (leaf[0] >= 'A' and leaf[0] <= 'Z') .type_def else .module;

            const phantom_id = try phantom.getOrCreate(chain, kind);

            // For type references (PascalCase leaf), create uses_type edge from
            // the containing function.
            if (kind == .type_def) {
                const ref_line = countLinesUpTo(source, chain_start);
                if (findContainingFunction(graph, file_id, ref_line)) |fn_id| {
                    try addEdgeIfNew(graph, fn_id, phantom_id, .uses_type, .phantom);
                }
            }
        } else {
            pos += 1;
        }
    }
}
