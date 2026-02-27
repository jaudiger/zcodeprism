const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const node_mod = @import("../core/node.zig");
const edge_mod = @import("../core/edge.zig");
const types = @import("../core/types.zig");
const metrics_mod = @import("../core/metrics.zig");
const lang = @import("../languages/language.zig");

const Graph = graph_mod.Graph;
const Node = node_mod.Node;
const Edge = edge_mod.Edge;
const NodeId = types.NodeId;
const EdgeType = types.EdgeType;
const EdgeSource = types.EdgeSource;
const NodeKind = types.NodeKind;
const Visibility = types.Visibility;
const Metrics = metrics_mod.Metrics;
const LangMeta = lang.LangMeta;
const ExternalInfo = lang.ExternalInfo;

fn writeStr(writer: *std.Io.Writer, str: []const u8) !void {
    try writer.writeByte('"');
    var start: usize = 0;
    for (str, 0..) |c, i| {
        const esc: ?[]const u8 = switch (c) {
            '"' => "\\\"",
            '\\' => "\\\\",
            '\n' => "\\n",
            '\r' => "\\r",
            '\t' => "\\t",
            else => null,
        };
        if (esc) |e| {
            if (i > start) try writer.writeAll(str[start..i]);
            try writer.writeAll(e);
            start = i + 1;
        } else if (c < 0x20) {
            if (i > start) try writer.writeAll(str[start..i]);
            try writer.print("\\u{x:0>4}", .{c});
            start = i + 1;
        }
    }
    if (start < str.len) try writer.writeAll(str[start..]);
    try writer.writeByte('"');
}

fn writeOptStr(writer: *std.Io.Writer, val: ?[]const u8) !void {
    if (val) |s| {
        try writeStr(writer, s);
    } else {
        try writer.writeAll("null");
    }
}

fn writeOptInt(writer: *std.Io.Writer, val: ?u32) !void {
    if (val) |v| {
        try writer.print("{d}", .{@as(u64, v)});
    } else {
        try writer.writeAll("null");
    }
}

fn writeOptNodeId(writer: *std.Io.Writer, val: ?NodeId) !void {
    if (val) |v| {
        try writer.print("{d}", .{@intFromEnum(v)});
    } else {
        try writer.writeAll("null");
    }
}

fn writeNodeLine(writer: *std.Io.Writer, n: Node) !void {
    try writer.print("{{\"_type\":\"node\",\"id\":{d},\"name\":", .{@intFromEnum(n.id)});
    try writeStr(writer, n.name);
    try writer.print(",\"kind\":\"{t}\",\"language\":\"{t}\",\"file_path\":", .{ n.kind, n.language });
    try writeOptStr(writer, n.file_path);
    try writer.writeAll(",\"line_start\":");
    try writeOptInt(writer, n.line_start);
    try writer.writeAll(",\"line_end\":");
    try writeOptInt(writer, n.line_end);
    try writer.print(",\"visibility\":\"{t}\",\"parent_id\":", .{n.visibility});
    try writeOptNodeId(writer, n.parent_id);
    try writer.writeAll(",\"doc\":");
    try writeOptStr(writer, n.doc);
    try writer.writeAll(",\"signature\":");
    try writeOptStr(writer, n.signature);
    // content_hash
    try writer.writeAll(",\"content_hash\":");
    if (n.content_hash) |ch| {
        try writer.writeByte('"');
        for (ch) |byte| {
            try writer.print("{x:0>2}", .{byte});
        }
        try writer.writeByte('"');
    } else {
        try writer.writeAll("null");
    }
    // external
    try writer.writeAll(",\"external\":");
    switch (n.external) {
        .none => try writer.writeAll("null"),
        .stdlib => try writer.writeAll("\"stdlib\""),
        .dependency => |d| {
            try writer.writeAll("{\"type\":\"dependency\",\"version\":");
            try writeOptStr(writer, d.version);
            try writer.writeByte('}');
        },
    }
    // lang_meta
    try writer.writeAll(",\"lang_meta\":");
    try n.lang_meta.writeJson(writer);
    // metrics
    try writer.writeAll(",\"metrics\":");
    if (n.metrics) |m| {
        try writer.print("{{\"complexity\":{d},\"lines\":{d},\"fan_in\":{d},\"fan_out\":{d},\"branches\":{d},\"loops\":{d},\"error_paths\":{d},\"nesting_depth_max\":{d},\"structural_hash\":{d}}}", .{
            m.complexity,      m.lines, m.fan_in,      m.fan_out,
            m.branches,        m.loops, m.error_paths, m.nesting_depth_max,
            m.structural_hash,
        });
    } else {
        try writer.writeAll("null");
    }
    try writer.writeAll("}\n");
}

fn writeEdgeLine(writer: *std.Io.Writer, e: Edge) !void {
    try writer.print("{{\"_type\":\"edge\",\"source_id\":{d},\"target_id\":{d},\"edge_type\":\"{t}\",\"source\":\"{t}\"}}\n", .{
        @intFromEnum(e.source_id),
        @intFromEnum(e.target_id),
        e.edge_type,
        e.source,
    });
}

fn jsonStr(val: std.json.Value) ?[]const u8 {
    return switch (val) {
        .string => |s| s,
        else => null,
    };
}

fn jsonOptStr(val: std.json.Value) ?[]const u8 {
    return switch (val) {
        .string => |s| s,
        .null => null,
        else => null,
    };
}

fn jsonOptInt(val: std.json.Value) ?i64 {
    return switch (val) {
        .integer => |i| i,
        .null => null,
        else => null,
    };
}

fn dupeAndOwn(g: *Graph, str: []const u8) ![]const u8 {
    const duped = try g.allocator.dupe(u8, str);
    g.addOwnedBuffer(duped) catch {
        g.allocator.free(duped);
        return error.OutOfMemory;
    };
    return duped;
}

fn parseContentHash(hex: []const u8) ?[12]u8 {
    if (hex.len != 24) return null;
    var result: [12]u8 = undefined;
    for (0..12) |i| {
        result[i] = std.fmt.parseInt(u8, hex[i * 2 ..][0..2], 16) catch return null;
    }
    return result;
}

fn parseExternal(g: *Graph, val: std.json.Value) !ExternalInfo {
    switch (val) {
        .null => return .{ .none = {} },
        .string => |s| {
            if (std.mem.eql(u8, s, "stdlib")) return .{ .stdlib = {} };
            return .{ .none = {} };
        },
        .object => |obj| {
            const type_val = obj.get("type") orelse return .{ .none = {} };
            if (type_val != .string) return .{ .none = {} };
            if (std.mem.eql(u8, type_val.string, "dependency")) {
                const ver_val = obj.get("version") orelse return .{ .dependency = .{ .version = null } };
                const ver_str = jsonOptStr(ver_val);
                const ver = if (ver_str) |v| try dupeAndOwn(g, v) else null;
                return .{ .dependency = .{ .version = ver } };
            }
            return .{ .none = {} };
        },
        else => return .{ .none = {} },
    }
}

fn parseMetrics(val: std.json.Value) ?Metrics {
    switch (val) {
        .null => return null,
        .object => |obj| {
            return .{
                .complexity = if (obj.get("complexity")) |v| @intCast(switch (v) {
                    .integer => |i| i,
                    else => 0,
                }) else 0,
                .lines = if (obj.get("lines")) |v| @intCast(switch (v) {
                    .integer => |i| i,
                    else => 0,
                }) else 0,
                .fan_in = if (obj.get("fan_in")) |v| @intCast(switch (v) {
                    .integer => |i| i,
                    else => 0,
                }) else 0,
                .fan_out = if (obj.get("fan_out")) |v| @intCast(switch (v) {
                    .integer => |i| i,
                    else => 0,
                }) else 0,
                .branches = if (obj.get("branches")) |v| @intCast(switch (v) {
                    .integer => |i| i,
                    else => 0,
                }) else 0,
                .loops = if (obj.get("loops")) |v| @intCast(switch (v) {
                    .integer => |i| i,
                    else => 0,
                }) else 0,
                .error_paths = if (obj.get("error_paths")) |v| @intCast(switch (v) {
                    .integer => |i| i,
                    else => 0,
                }) else 0,
                .nesting_depth_max = if (obj.get("nesting_depth_max")) |v| @intCast(switch (v) {
                    .integer => |i| i,
                    else => 0,
                }) else 0,
                .structural_hash = if (obj.get("structural_hash")) |v| @intCast(switch (v) {
                    .integer => |i| i,
                    else => 0,
                }) else 0,
            };
        },
        else => return null,
    }
}

/// Comptime lookup table mapping each EdgeType discriminant to its alphabetical rank.
const edge_type_sort_rank = blk: {
    const fields = @typeInfo(EdgeType).@"enum".fields;
    const n = fields.len;
    var ranks: [n]u8 = undefined;
    for (fields, 0..) |f, i| {
        var rank: u8 = 0;
        for (fields) |other| {
            if (std.mem.order(u8, other.name, f.name) == .lt) {
                rank += 1;
            }
        }
        ranks[i] = rank;
    }
    break :blk ranks;
};

/// Canonical edge ordering: by edge_type alphabetical rank, then source_id, then target_id.
fn edgeLessThan(_: void, a: Edge, b: Edge) bool {
    const ra = edge_type_sort_rank[@intFromEnum(a.edge_type)];
    const rb = edge_type_sort_rank[@intFromEnum(b.edge_type)];
    if (ra != rb) return ra < rb;
    const as = @intFromEnum(a.source_id);
    const bs = @intFromEnum(b.source_id);
    if (as != bs) return as < bs;
    return @intFromEnum(a.target_id) < @intFromEnum(b.target_id);
}

/// Export a graph to JSONL format, writing one JSON object per line.
///
/// Nodes are emitted first (ordered by id), then edges (sorted alphabetically
/// by edge_type, then by source_id, then by target_id). Each line contains a
/// `_type` field set to "node" or "edge". The `allocator` is used for a
/// temporary sorted-edge copy; `g` is not modified.
pub fn exportJsonl(allocator: std.mem.Allocator, g: *const Graph, writer: *std.Io.Writer) !void {
    // Nodes (already sorted by id, sequential in the graph)
    for (g.nodes.items) |n| {
        try writeNodeLine(writer, n);
    }

    // Edges (sorted by edge_type alphabetically, then source_id, then target_id)
    if (g.edgeCount() > 0) {
        const sorted_edges = try allocator.alloc(Edge, g.edgeCount());
        defer allocator.free(sorted_edges);
        @memcpy(sorted_edges, g.edges.items);

        std.sort.block(Edge, sorted_edges, {}, edgeLessThan);

        for (sorted_edges) |e| {
            try writeEdgeLine(writer, e);
        }
    }
}

/// Import a graph from JSONL-formatted bytes.
///
/// Each non-empty line in `data` must be a JSON object with `_type` set to
/// "node" or "edge". Lines with unrecognized types or missing required fields
/// are silently skipped. Edges referencing out-of-bounds node ids are also
/// skipped. The caller owns the returned Graph and must call `deinit()` on it.
pub fn importJsonl(allocator: std.mem.Allocator, data: []const u8) !Graph {
    var g = Graph.init(allocator, "");
    errdefer g.deinit();

    // Pre-count lines for capacity hint (upper bound: not all lines are nodes or edges)
    var line_count: usize = 0;
    for (data) |c| {
        if (c == '\n') line_count += 1;
    }
    // Account for possible final line without trailing newline
    if (data.len > 0 and data[data.len - 1] != '\n') line_count += 1;
    try g.nodes.ensureTotalCapacity(allocator, line_count);
    try g.edges.ensureTotalCapacity(allocator, line_count);

    var line_iter = std.mem.splitScalar(u8, data, '\n');
    while (line_iter.next()) |line| {
        if (line.len == 0) continue;

        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, line, .{});
        defer parsed.deinit();
        const obj = parsed.value.object;

        const type_val = obj.get("_type") orelse continue;
        if (type_val != .string) continue;

        if (std.mem.eql(u8, type_val.string, "node")) {
            // Parse node fields
            const name_str = jsonStr(obj.get("name") orelse continue) orelse continue;
            const kind_str = jsonStr(obj.get("kind") orelse continue) orelse continue;
            const lang_str = jsonStr(obj.get("language") orelse continue) orelse continue;
            const vis_str = jsonStr(obj.get("visibility") orelse continue) orelse continue;

            const name = try dupeAndOwn(&g, name_str);
            const kind = std.meta.stringToEnum(NodeKind, kind_str) orelse continue;
            const language = std.meta.stringToEnum(types.Language, lang_str) orelse continue;
            const visibility = std.meta.stringToEnum(Visibility, vis_str) orelse continue;

            // Optional strings
            const file_path_str = if (obj.get("file_path")) |v| jsonOptStr(v) else null;
            const file_path = if (file_path_str) |s| try dupeAndOwn(&g, s) else null;

            const doc_str = if (obj.get("doc")) |v| jsonOptStr(v) else null;
            const doc = if (doc_str) |s| try dupeAndOwn(&g, s) else null;

            const sig_str = if (obj.get("signature")) |v| jsonOptStr(v) else null;
            const signature = if (sig_str) |s| try dupeAndOwn(&g, s) else null;

            // parent_id
            const parent_id: ?NodeId = if (obj.get("parent_id")) |v| blk: {
                const i = jsonOptInt(v) orelse break :blk null;
                break :blk @enumFromInt(@as(u64, @intCast(i)));
            } else null;

            // line_start / line_end
            const line_start: ?u32 = if (obj.get("line_start")) |v| blk: {
                const i = jsonOptInt(v) orelse break :blk null;
                break :blk @intCast(i);
            } else null;
            const line_end: ?u32 = if (obj.get("line_end")) |v| blk: {
                const i = jsonOptInt(v) orelse break :blk null;
                break :blk @intCast(i);
            } else null;

            // content_hash
            const content_hash: ?[12]u8 = if (obj.get("content_hash")) |v| blk: {
                const s = jsonOptStr(v) orelse break :blk null;
                break :blk parseContentHash(s);
            } else null;

            // external
            const external = if (obj.get("external")) |v| try parseExternal(&g, v) else ExternalInfo{ .none = {} };

            // lang_meta (parseJson dupes string data; register with graph)
            const lang_meta = if (obj.get("lang_meta")) |v| blk: {
                const meta = try LangMeta.parseJson(allocator, v);
                switch (meta) {
                    .zig => |zm| {
                        if (zm.calling_convention) |cc| {
                            g.addOwnedBuffer(cc) catch {
                                allocator.free(cc);
                                return error.OutOfMemory;
                            };
                        }
                    },
                    .none => {},
                }
                break :blk meta;
            } else LangMeta{ .none = {} };

            // metrics
            const metrics = if (obj.get("metrics")) |v| parseMetrics(v) else null;

            _ = try g.addNode(.{
                .id = .root, // overridden by addNode
                .name = name,
                .kind = kind,
                .language = language,
                .file_path = file_path,
                .line_start = line_start,
                .line_end = line_end,
                .parent_id = parent_id,
                .visibility = visibility,
                .doc = doc,
                .signature = signature,
                .content_hash = content_hash,
                .metrics = metrics,
                .lang_meta = lang_meta,
                .external = external,
            });
        } else if (std.mem.eql(u8, type_val.string, "edge")) {
            const src_id_val = obj.get("source_id") orelse continue;
            const tgt_id_val = obj.get("target_id") orelse continue;
            const et_val = obj.get("edge_type") orelse continue;

            const src_id = switch (src_id_val) {
                .integer => |i| @as(u64, @intCast(i)),
                else => continue,
            };
            const tgt_id = switch (tgt_id_val) {
                .integer => |i| @as(u64, @intCast(i)),
                else => continue,
            };
            const et_str = jsonStr(et_val) orelse continue;
            const edge_type = std.meta.stringToEnum(EdgeType, et_str) orelse continue;

            // edge source (optional, defaults to tree_sitter)
            const edge_source: EdgeSource = if (obj.get("source")) |v| blk: {
                const s = jsonStr(v) orelse break :blk .tree_sitter;
                break :blk std.meta.stringToEnum(EdgeSource, s) orelse .tree_sitter;
            } else .tree_sitter;

            // Skip edges that reference out-of-bounds node IDs.
            if (src_id >= g.nodes.items.len or tgt_id >= g.nodes.items.len) continue;

            _ = try g.addEdge(.{
                .source_id = @enumFromInt(src_id),
                .target_id = @enumFromInt(tgt_id),
                .edge_type = edge_type,
                .source = edge_source,
            });
        }
    }

    try g.rebuildEdgeIndex();
    try g.freeze();
    return g;
}

/// Build a test graph with 3 diverse nodes and 2 edges for use in tests.
fn createTestGraph(allocator: std.mem.Allocator) !Graph {
    var g = Graph.init(allocator, "/tmp/test-project");

    // Node 0: file node
    _ = try g.addNode(.{
        .id = .root,
        .name = "main.zig",
        .kind = .file,
        .language = .zig,
        .visibility = .public,
        .file_path = "src/main.zig",
        .line_start = 1,
        .line_end = 100,
    });

    // Node 1: function with metrics, doc, signature
    _ = try g.addNode(.{
        .id = .root,
        .name = "process",
        .kind = .function,
        .language = .zig,
        .visibility = .public,
        .file_path = "src/main.zig",
        .line_start = 10,
        .line_end = 50,
        .parent_id = @enumFromInt(0),
        .doc = "/// Process the input data.",
        .signature = "pub fn process(data: []const u8) !void",
        .content_hash = "abcdefghijkl".*,
        .metrics = .{
            .complexity = 5,
            .lines = 40,
            .fan_in = 2,
            .fan_out = 3,
        },
        .lang_meta = .{ .zig = .{ .is_comptime = true } },
    });

    // Node 2: type_def with external=stdlib (phantom)
    _ = try g.addNode(.{
        .id = .root,
        .name = "Allocator",
        .kind = .type_def,
        .language = .zig,
        .external = .{ .stdlib = {} },
    });

    // Edge 0: function uses type
    _ = try g.addEdge(.{
        .source_id = @enumFromInt(1),
        .target_id = @enumFromInt(2),
        .edge_type = .uses_type,
        .source = .tree_sitter,
    });

    // Edge 1: file exports function
    _ = try g.addEdge(.{
        .source_id = @enumFromInt(0),
        .target_id = @enumFromInt(1),
        .edge_type = .exports,
        .source = .tree_sitter,
    });

    return g;
}

// Nominal tests

test "jsonl round-trip preserves nodes and edges" {
    // Arrange
    var g = try createTestGraph(std.testing.allocator);
    defer g.deinit();

    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    try exportJsonl(std.testing.allocator, &g, &aw.writer);
    try aw.writer.flush();
    var loaded = try importJsonl(std.testing.allocator, aw.written());
    defer loaded.deinit();

    // Assert: nodes
    try std.testing.expectEqual(g.nodeCount(), loaded.nodeCount());
    for (g.nodes.items, loaded.nodes.items) |original, restored| {
        try std.testing.expectEqualStrings(original.name, restored.name);
        try std.testing.expectEqual(original.kind, restored.kind);
        try std.testing.expectEqual(original.language, restored.language);
        try std.testing.expectEqual(original.visibility, restored.visibility);
    }

    // Assert: edges (compare as sets via canonical sort order)
    try std.testing.expectEqual(g.edgeCount(), loaded.edgeCount());

    const orig_sorted = try std.testing.allocator.alloc(Edge, g.edgeCount());
    defer std.testing.allocator.free(orig_sorted);
    @memcpy(orig_sorted, g.edges.items);
    std.sort.block(Edge, orig_sorted, {}, edgeLessThan);

    const loaded_sorted = try std.testing.allocator.alloc(Edge, loaded.edgeCount());
    defer std.testing.allocator.free(loaded_sorted);
    @memcpy(loaded_sorted, loaded.edges.items);
    std.sort.block(Edge, loaded_sorted, {}, edgeLessThan);

    for (orig_sorted, loaded_sorted) |original, restored| {
        try std.testing.expectEqual(original.source_id, restored.source_id);
        try std.testing.expectEqual(original.target_id, restored.target_id);
        try std.testing.expectEqual(original.edge_type, restored.edge_type);
        try std.testing.expectEqual(original.source, restored.source);
    }
}

test "jsonl lines are valid json" {
    // Arrange
    var g = try createTestGraph(std.testing.allocator);
    defer g.deinit();

    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    try exportJsonl(std.testing.allocator, &g, &aw.writer);
    try aw.writer.flush();

    // Assert: each non-empty line parses as JSON
    var line_iter = std.mem.splitScalar(u8, aw.written(), '\n');
    while (line_iter.next()) |line| {
        if (line.len == 0) continue;
        const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, line, .{});
        defer parsed.deinit();
        try std.testing.expect(parsed.value == .object);
    }
}

test "jsonl records have correct _type field" {
    // Arrange
    var g = try createTestGraph(std.testing.allocator);
    defer g.deinit();

    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    try exportJsonl(std.testing.allocator, &g, &aw.writer);
    try aw.writer.flush();

    // Assert: count node and edge lines by _type
    var line_iter = std.mem.splitScalar(u8, aw.written(), '\n');
    var node_count: usize = 0;
    var edge_count: usize = 0;
    while (line_iter.next()) |line| {
        if (line.len == 0) continue;
        const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, line, .{});
        defer parsed.deinit();
        const obj = parsed.value.object;
        if (obj.get("_type")) |type_val| {
            if (std.mem.eql(u8, type_val.string, "node")) node_count += 1;
            if (std.mem.eql(u8, type_val.string, "edge")) edge_count += 1;
        }
    }
    try std.testing.expectEqual(g.nodeCount(), node_count);
    try std.testing.expectEqual(g.edgeCount(), edge_count);
}

test "jsonl output is sorted" {
    // Arrange
    var g = try createTestGraph(std.testing.allocator);
    defer g.deinit();

    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    try exportJsonl(std.testing.allocator, &g, &aw.writer);
    try aw.writer.flush();

    // Assert: node ids are in ascending order
    const EdgeKey = struct { edge_type: []const u8, source_id: i64, target_id: i64 };
    var edges: std.ArrayList(EdgeKey) = .{};
    defer edges.deinit(std.testing.allocator);

    var line_iter = std.mem.splitScalar(u8, aw.written(), '\n');
    var prev_node_id: ?i64 = null;
    while (line_iter.next()) |line| {
        if (line.len == 0) continue;
        const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, line, .{});
        defer parsed.deinit();
        const obj = parsed.value.object;
        const type_val = obj.get("_type") orelse continue;

        if (std.mem.eql(u8, type_val.string, "node")) {
            const id = obj.get("id").?.integer;
            if (prev_node_id) |prev| {
                try std.testing.expect(id > prev);
            }
            prev_node_id = id;
        } else if (std.mem.eql(u8, type_val.string, "edge")) {
            try edges.append(std.testing.allocator, .{
                .edge_type = try std.testing.allocator.dupe(u8, obj.get("edge_type").?.string),
                .source_id = obj.get("source_id").?.integer,
                .target_id = obj.get("target_id").?.integer,
            });
        }
    }
    defer for (edges.items) |e| std.testing.allocator.free(@constCast(e.edge_type));

    // Assert: edges are sorted by edge_type alphabetically, then source_id, then target_id
    for (0..edges.items.len -| 1) |i| {
        const a = edges.items[i];
        const b = edges.items[i + 1];
        const type_cmp = std.mem.order(u8, a.edge_type, b.edge_type);
        switch (type_cmp) {
            .lt => {},
            .gt => return error.NotSorted,
            .eq => {
                if (a.source_id > b.source_id) return error.NotSorted;
                if (a.source_id == b.source_id and a.target_id > b.target_id) return error.NotSorted;
            },
        }
    }
}

// Edge case tests

test "jsonl empty graph" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/test-project");
    defer g.deinit();

    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    try exportJsonl(std.testing.allocator, &g, &aw.writer);
    try aw.writer.flush();

    // Assert: no output lines
    var line_count: usize = 0;
    var line_iter = std.mem.splitScalar(u8, aw.written(), '\n');
    while (line_iter.next()) |line| {
        if (line.len > 0) line_count += 1;
    }
    try std.testing.expectEqual(@as(usize, 0), line_count);

    // Import the empty output, expect an empty graph
    var loaded = try importJsonl(std.testing.allocator, aw.written());
    defer loaded.deinit();
    try std.testing.expectEqual(@as(usize, 0), loaded.nodeCount());
    try std.testing.expectEqual(@as(usize, 0), loaded.edgeCount());
}

test "jsonl preserves null fields as explicit null" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/test-project");
    defer g.deinit();

    _ = try g.addNode(.{
        .id = .root,
        .name = "bare",
        .kind = .function,
        .language = .zig,
        // doc, signature, content_hash all null by default
    });

    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    try exportJsonl(std.testing.allocator, &g, &aw.writer);
    try aw.writer.flush();

    // Assert: null fields are serialized as explicit JSON null, not omitted
    var line_iter = std.mem.splitScalar(u8, aw.written(), '\n');
    while (line_iter.next()) |line| {
        if (line.len == 0) continue;
        const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, line, .{});
        defer parsed.deinit();
        const obj = parsed.value.object;
        const type_val = obj.get("_type") orelse continue;
        if (!std.mem.eql(u8, type_val.string, "node")) continue;
        // "doc" key must exist and be null
        const doc_val = obj.get("doc") orelse return error.MissingField;
        try std.testing.expect(doc_val == .null);
        // "signature" key must exist and be null
        const sig_val = obj.get("signature") orelse return error.MissingField;
        try std.testing.expect(sig_val == .null);
    }
}

test "jsonl preserves phantom nodes" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/test-project");
    defer g.deinit();

    _ = try g.addNode(.{
        .id = .root,
        .name = "std",
        .kind = .module,
        .language = .zig,
        .external = .{ .stdlib = {} },
    });

    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    try exportJsonl(std.testing.allocator, &g, &aw.writer);
    try aw.writer.flush();
    var loaded = try importJsonl(std.testing.allocator, aw.written());
    defer loaded.deinit();

    // Assert
    try std.testing.expectEqual(@as(usize, 1), loaded.nodeCount());
    try std.testing.expectEqual(ExternalInfo.stdlib, loaded.getNode(.root).?.external);
}

test "jsonl round-trip preserves union_def kind" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/test-project");
    defer g.deinit();

    _ = try g.addNode(.{
        .id = .root,
        .name = "MyUnion",
        .kind = .union_def,
        .language = .zig,
        .visibility = .public,
    });

    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    try exportJsonl(std.testing.allocator, &g, &aw.writer);
    try aw.writer.flush();
    var loaded = try importJsonl(std.testing.allocator, aw.written());
    defer loaded.deinit();

    // Assert
    try std.testing.expectEqual(@as(usize, 1), loaded.nodeCount());
    try std.testing.expectEqual(NodeKind.union_def, loaded.getNode(.root).?.kind);
}

test "jsonl round-trip preserves is_packed metadata" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/test-project");
    defer g.deinit();

    _ = try g.addNode(.{
        .id = .root,
        .name = "PackedStruct",
        .kind = .type_def,
        .language = .zig,
        .lang_meta = .{ .zig = .{ .is_packed = true } },
    });

    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    try exportJsonl(std.testing.allocator, &g, &aw.writer);
    try aw.writer.flush();
    var loaded = try importJsonl(std.testing.allocator, aw.written());
    defer loaded.deinit();

    // Assert
    try std.testing.expectEqual(@as(usize, 1), loaded.nodeCount());
    const meta = loaded.getNode(.root).?.lang_meta;
    try std.testing.expect(meta.zig.is_packed);
    try std.testing.expect(!meta.zig.is_extern);
}
