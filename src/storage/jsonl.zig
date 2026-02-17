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

// --- JSON writing helpers ---

fn appendStr(out: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, str: []const u8) !void {
    try out.append(allocator, '"');
    for (str) |c| {
        switch (c) {
            '"' => try out.appendSlice(allocator, "\\\""),
            '\\' => try out.appendSlice(allocator, "\\\\"),
            '\n' => try out.appendSlice(allocator, "\\n"),
            '\r' => try out.appendSlice(allocator, "\\r"),
            '\t' => try out.appendSlice(allocator, "\\t"),
            else => {
                if (c < 0x20) {
                    var esc_buf: [6]u8 = undefined;
                    _ = std.fmt.bufPrint(&esc_buf, "\\u{x:0>4}", .{c}) catch unreachable;
                    try out.appendSlice(allocator, &esc_buf);
                } else {
                    try out.append(allocator, c);
                }
            },
        }
    }
    try out.append(allocator, '"');
}

fn appendKey(out: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, key: []const u8) !void {
    try appendStr(out, allocator, key);
    try out.append(allocator, ':');
}

fn appendInt(out: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, val: u64) !void {
    var num_buf: [20]u8 = undefined;
    const num = std.fmt.bufPrint(&num_buf, "{d}", .{val}) catch unreachable;
    try out.appendSlice(allocator, num);
}

fn appendOptStr(out: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, val: ?[]const u8) !void {
    if (val) |s| {
        try appendStr(out, allocator, s);
    } else {
        try out.appendSlice(allocator, "null");
    }
}

fn appendOptInt(out: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, val: ?u32) !void {
    if (val) |v| {
        try appendInt(out, allocator, v);
    } else {
        try out.appendSlice(allocator, "null");
    }
}

fn appendOptNodeId(out: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, val: ?NodeId) !void {
    if (val) |v| {
        try appendInt(out, allocator, @intFromEnum(v));
    } else {
        try out.appendSlice(allocator, "null");
    }
}

fn appendBool(out: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, val: bool) !void {
    try out.appendSlice(allocator, if (val) "true" else "false");
}

// --- Node serialization ---

fn writeNodeLine(out: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, n: Node) !void {
    try out.append(allocator, '{');

    // _type
    try appendKey(out, allocator, "_type");
    try appendStr(out, allocator, "node");

    // id
    try out.append(allocator, ',');
    try appendKey(out, allocator, "id");
    try appendInt(out, allocator, @intFromEnum(n.id));

    // name
    try out.append(allocator, ',');
    try appendKey(out, allocator, "name");
    try appendStr(out, allocator, n.name);

    // kind
    try out.append(allocator, ',');
    try appendKey(out, allocator, "kind");
    try appendStr(out, allocator, @tagName(n.kind));

    // language
    try out.append(allocator, ',');
    try appendKey(out, allocator, "language");
    try appendStr(out, allocator, @tagName(n.language));

    // file_path
    try out.append(allocator, ',');
    try appendKey(out, allocator, "file_path");
    try appendOptStr(out, allocator, n.file_path);

    // line_start
    try out.append(allocator, ',');
    try appendKey(out, allocator, "line_start");
    try appendOptInt(out, allocator, n.line_start);

    // line_end
    try out.append(allocator, ',');
    try appendKey(out, allocator, "line_end");
    try appendOptInt(out, allocator, n.line_end);

    // visibility
    try out.append(allocator, ',');
    try appendKey(out, allocator, "visibility");
    try appendStr(out, allocator, @tagName(n.visibility));

    // parent_id
    try out.append(allocator, ',');
    try appendKey(out, allocator, "parent_id");
    try appendOptNodeId(out, allocator, n.parent_id);

    // doc
    try out.append(allocator, ',');
    try appendKey(out, allocator, "doc");
    try appendOptStr(out, allocator, n.doc);

    // signature
    try out.append(allocator, ',');
    try appendKey(out, allocator, "signature");
    try appendOptStr(out, allocator, n.signature);

    // content_hash
    try out.append(allocator, ',');
    try appendKey(out, allocator, "content_hash");
    if (n.content_hash) |ch| {
        try out.append(allocator, '"');
        for (ch) |byte| {
            var hex_buf: [2]u8 = undefined;
            _ = std.fmt.bufPrint(&hex_buf, "{x:0>2}", .{byte}) catch unreachable;
            try out.appendSlice(allocator, &hex_buf);
        }
        try out.append(allocator, '"');
    } else {
        try out.appendSlice(allocator, "null");
    }

    // external
    try out.append(allocator, ',');
    try appendKey(out, allocator, "external");
    switch (n.external) {
        .none => try out.appendSlice(allocator, "null"),
        .stdlib => try appendStr(out, allocator, "stdlib"),
        .dependency => |d| {
            try out.appendSlice(allocator, "{\"type\":\"dependency\",\"version\":");
            try appendOptStr(out, allocator, d.version);
            try out.append(allocator, '}');
        },
    }

    // lang_meta
    try out.append(allocator, ',');
    try appendKey(out, allocator, "lang_meta");
    switch (n.lang_meta) {
        .none => try out.appendSlice(allocator, "null"),
        .zig => |zm| {
            try out.appendSlice(allocator, "{\"type\":\"zig\"");
            try out.appendSlice(allocator, ",\"is_comptime\":");
            try appendBool(out, allocator, zm.is_comptime);
            try out.appendSlice(allocator, ",\"is_inline\":");
            try appendBool(out, allocator, zm.is_inline);
            try out.appendSlice(allocator, ",\"is_extern\":");
            try appendBool(out, allocator, zm.is_extern);
            try out.appendSlice(allocator, ",\"comptime_conditional\":");
            try appendBool(out, allocator, zm.comptime_conditional);
            try out.append(allocator, '}');
        },
    }

    // metrics
    try out.append(allocator, ',');
    try appendKey(out, allocator, "metrics");
    if (n.metrics) |m| {
        try out.append(allocator, '{');
        try appendKey(out, allocator, "complexity");
        try appendInt(out, allocator, m.complexity);
        try out.append(allocator, ',');
        try appendKey(out, allocator, "lines");
        try appendInt(out, allocator, m.lines);
        try out.append(allocator, ',');
        try appendKey(out, allocator, "fan_in");
        try appendInt(out, allocator, m.fan_in);
        try out.append(allocator, ',');
        try appendKey(out, allocator, "fan_out");
        try appendInt(out, allocator, m.fan_out);
        try out.append(allocator, ',');
        try appendKey(out, allocator, "branches");
        try appendInt(out, allocator, m.branches);
        try out.append(allocator, ',');
        try appendKey(out, allocator, "loops");
        try appendInt(out, allocator, m.loops);
        try out.append(allocator, ',');
        try appendKey(out, allocator, "error_paths");
        try appendInt(out, allocator, m.error_paths);
        try out.append(allocator, ',');
        try appendKey(out, allocator, "nesting_depth_max");
        try appendInt(out, allocator, m.nesting_depth_max);
        try out.append(allocator, ',');
        try appendKey(out, allocator, "structural_hash");
        try appendInt(out, allocator, m.structural_hash);
        try out.append(allocator, '}');
    } else {
        try out.appendSlice(allocator, "null");
    }

    try out.append(allocator, '}');
    try out.append(allocator, '\n');
}

// --- Edge serialization ---

fn writeEdgeLine(out: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, e: Edge) !void {
    try out.append(allocator, '{');

    try appendKey(out, allocator, "_type");
    try appendStr(out, allocator, "edge");

    try out.append(allocator, ',');
    try appendKey(out, allocator, "source_id");
    try appendInt(out, allocator, @intFromEnum(e.source_id));

    try out.append(allocator, ',');
    try appendKey(out, allocator, "target_id");
    try appendInt(out, allocator, @intFromEnum(e.target_id));

    try out.append(allocator, ',');
    try appendKey(out, allocator, "edge_type");
    try appendStr(out, allocator, @tagName(e.edge_type));

    try out.append(allocator, ',');
    try appendKey(out, allocator, "source");
    try appendStr(out, allocator, @tagName(e.source));

    try out.append(allocator, '}');
    try out.append(allocator, '\n');
}

// --- Import helpers ---

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

fn parseLangMeta(val: std.json.Value) LangMeta {
    switch (val) {
        .null => return .{ .none = {} },
        .object => |obj| {
            const type_val = obj.get("type") orelse return .{ .none = {} };
            if (type_val != .string) return .{ .none = {} };
            if (std.mem.eql(u8, type_val.string, "zig")) {
                return .{ .zig = .{
                    .is_comptime = if (obj.get("is_comptime")) |v| (v == .bool and v.bool) else false,
                    .is_inline = if (obj.get("is_inline")) |v| (v == .bool and v.bool) else false,
                    .is_extern = if (obj.get("is_extern")) |v| (v == .bool and v.bool) else false,
                    .comptime_conditional = if (obj.get("comptime_conditional")) |v| (v == .bool and v.bool) else false,
                } };
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

/// Canonical edge ordering: by edge_type name, then source_id, then target_id.
fn edgeLessThan(_: void, a: Edge, b: Edge) bool {
    const a_type = @tagName(a.edge_type);
    const b_type = @tagName(b.edge_type);
    const type_cmp = std.mem.order(u8, a_type, b_type);
    if (type_cmp != .eq) return type_cmp == .lt;
    const a_src = @intFromEnum(a.source_id);
    const b_src = @intFromEnum(b.source_id);
    if (a_src != b_src) return a_src < b_src;
    return @intFromEnum(a.target_id) < @intFromEnum(b.target_id);
}

// --- Public API ---

/// Export a graph to JSONL format, writing into the provided buffer.
/// Each line is a self-contained JSON object with a `_type` field
/// indicating whether it's a "node" or "edge".
pub fn exportJsonl(allocator: std.mem.Allocator, g: *const Graph, out: *std.ArrayListUnmanaged(u8)) !void {
    // Nodes (already sorted by id, sequential in the graph)
    for (g.nodes.items) |n| {
        try writeNodeLine(out, allocator, n);
    }

    // Edges (sorted by edge_type alphabetically, then source_id, then target_id)
    if (g.edgeCount() > 0) {
        const sorted_edges = try allocator.alloc(Edge, g.edgeCount());
        defer allocator.free(sorted_edges);
        @memcpy(sorted_edges, g.edges.items);

        std.sort.block(Edge, sorted_edges, {}, edgeLessThan);

        for (sorted_edges) |e| {
            try writeEdgeLine(out, allocator, e);
        }
    }
}

/// Import a graph from JSONL-formatted bytes.
pub fn importJsonl(allocator: std.mem.Allocator, data: []const u8) !Graph {
    var g = Graph.init(allocator, "");
    errdefer g.deinit();

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

            // lang_meta
            const lang_meta = if (obj.get("lang_meta")) |v| parseLangMeta(v) else LangMeta{ .none = {} };

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

            _ = try g.addEdge(.{
                .source_id = @enumFromInt(src_id),
                .target_id = @enumFromInt(tgt_id),
                .edge_type = edge_type,
                .source = edge_source,
            });
        }
    }

    return g;
}

// --- Test helpers ---

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

// --- Tests ---

// Nominal tests

test "jsonl round-trip preserves nodes" {
    // Arrange
    var g = try createTestGraph(std.testing.allocator);
    defer g.deinit();

    var buf: std.ArrayListUnmanaged(u8) = .{};
    defer buf.deinit(std.testing.allocator);

    // Act
    try exportJsonl(std.testing.allocator, &g, &buf);
    var loaded = try importJsonl(std.testing.allocator, buf.items);
    defer loaded.deinit();

    // Assert
    try std.testing.expectEqual(g.nodeCount(), loaded.nodeCount());
    for (g.nodes.items, loaded.nodes.items) |original, restored| {
        try std.testing.expectEqualStrings(original.name, restored.name);
        try std.testing.expectEqual(original.kind, restored.kind);
        try std.testing.expectEqual(original.language, restored.language);
        try std.testing.expectEqual(original.visibility, restored.visibility);
    }
}

test "jsonl round-trip preserves edges" {
    // Arrange
    var g = try createTestGraph(std.testing.allocator);
    defer g.deinit();

    var buf: std.ArrayListUnmanaged(u8) = .{};
    defer buf.deinit(std.testing.allocator);

    // Act
    try exportJsonl(std.testing.allocator, &g, &buf);
    var loaded = try importJsonl(std.testing.allocator, buf.items);
    defer loaded.deinit();

    // Assert: compare as sets: sort both sides by canonical edge order
    // because exportJsonl sorts edges and the original may have insertion order.
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

    var buf: std.ArrayListUnmanaged(u8) = .{};
    defer buf.deinit(std.testing.allocator);

    // Act
    try exportJsonl(std.testing.allocator, &g, &buf);

    // Assert: each non-empty line parses as JSON
    var line_iter = std.mem.splitScalar(u8, buf.items, '\n');
    while (line_iter.next()) |line| {
        if (line.len == 0) continue;
        const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, line, .{});
        defer parsed.deinit();
        try std.testing.expect(parsed.value == .object);
    }
}

test "jsonl nodes have _type node" {
    // Arrange
    var g = try createTestGraph(std.testing.allocator);
    defer g.deinit();

    var buf: std.ArrayListUnmanaged(u8) = .{};
    defer buf.deinit(std.testing.allocator);

    // Act
    try exportJsonl(std.testing.allocator, &g, &buf);

    // Assert: node lines have _type "node"
    var line_iter = std.mem.splitScalar(u8, buf.items, '\n');
    var node_count: usize = 0;
    while (line_iter.next()) |line| {
        if (line.len == 0) continue;
        const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, line, .{});
        defer parsed.deinit();
        const obj = parsed.value.object;
        if (obj.get("_type")) |type_val| {
            if (std.mem.eql(u8, type_val.string, "node")) {
                node_count += 1;
            }
        }
    }
    try std.testing.expectEqual(g.nodeCount(), node_count);
}

test "jsonl edges have _type edge" {
    // Arrange
    var g = try createTestGraph(std.testing.allocator);
    defer g.deinit();

    var buf: std.ArrayListUnmanaged(u8) = .{};
    defer buf.deinit(std.testing.allocator);

    // Act
    try exportJsonl(std.testing.allocator, &g, &buf);

    // Assert: edge lines have _type "edge"
    var line_iter = std.mem.splitScalar(u8, buf.items, '\n');
    var edge_count: usize = 0;
    while (line_iter.next()) |line| {
        if (line.len == 0) continue;
        const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, line, .{});
        defer parsed.deinit();
        const obj = parsed.value.object;
        if (obj.get("_type")) |type_val| {
            if (std.mem.eql(u8, type_val.string, "edge")) {
                edge_count += 1;
            }
        }
    }
    try std.testing.expectEqual(g.edgeCount(), edge_count);
}

test "jsonl nodes sorted by id" {
    // Arrange
    var g = try createTestGraph(std.testing.allocator);
    defer g.deinit();

    var buf: std.ArrayListUnmanaged(u8) = .{};
    defer buf.deinit(std.testing.allocator);

    // Act
    try exportJsonl(std.testing.allocator, &g, &buf);

    // Assert: node ids are in ascending order
    var line_iter = std.mem.splitScalar(u8, buf.items, '\n');
    var prev_id: ?i64 = null;
    while (line_iter.next()) |line| {
        if (line.len == 0) continue;
        const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, line, .{});
        defer parsed.deinit();
        const obj = parsed.value.object;
        const type_val = obj.get("_type") orelse continue;
        if (!std.mem.eql(u8, type_val.string, "node")) continue;
        const id = obj.get("id").?.integer;
        if (prev_id) |prev| {
            try std.testing.expect(id > prev);
        }
        prev_id = id;
    }
}

test "jsonl edges sorted by type then source then target" {
    // Arrange
    var g = try createTestGraph(std.testing.allocator);
    defer g.deinit();

    var buf: std.ArrayListUnmanaged(u8) = .{};
    defer buf.deinit(std.testing.allocator);

    // Act
    try exportJsonl(std.testing.allocator, &g, &buf);

    // Assert: edges are sorted by edge_type alphabetically, then source_id, then target_id
    const EdgeKey = struct { edge_type: []const u8, source_id: i64, target_id: i64 };
    var edges: std.ArrayListUnmanaged(EdgeKey) = .{};
    defer edges.deinit(std.testing.allocator);

    var line_iter = std.mem.splitScalar(u8, buf.items, '\n');
    while (line_iter.next()) |line| {
        if (line.len == 0) continue;
        const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, line, .{});
        defer parsed.deinit();
        const obj = parsed.value.object;
        const type_val = obj.get("_type") orelse continue;
        if (!std.mem.eql(u8, type_val.string, "edge")) continue;
        try edges.append(std.testing.allocator, .{
            .edge_type = try std.testing.allocator.dupe(u8, obj.get("edge_type").?.string),
            .source_id = obj.get("source_id").?.integer,
            .target_id = obj.get("target_id").?.integer,
        });
    }
    defer for (edges.items) |e| std.testing.allocator.free(@constCast(e.edge_type));

    // Verify sorted order
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

    var buf: std.ArrayListUnmanaged(u8) = .{};
    defer buf.deinit(std.testing.allocator);

    // Act
    try exportJsonl(std.testing.allocator, &g, &buf);

    // Assert: no output lines
    var line_count: usize = 0;
    var line_iter = std.mem.splitScalar(u8, buf.items, '\n');
    while (line_iter.next()) |line| {
        if (line.len > 0) line_count += 1;
    }
    try std.testing.expectEqual(@as(usize, 0), line_count);

    // Import the empty output → empty graph
    var loaded = try importJsonl(std.testing.allocator, buf.items);
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

    var buf: std.ArrayListUnmanaged(u8) = .{};
    defer buf.deinit(std.testing.allocator);

    // Act
    try exportJsonl(std.testing.allocator, &g, &buf);

    // Assert: null fields are serialized as explicit JSON null, not omitted
    var line_iter = std.mem.splitScalar(u8, buf.items, '\n');
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

    var buf: std.ArrayListUnmanaged(u8) = .{};
    defer buf.deinit(std.testing.allocator);

    // Act
    try exportJsonl(std.testing.allocator, &g, &buf);
    var loaded = try importJsonl(std.testing.allocator, buf.items);
    defer loaded.deinit();

    // Assert
    try std.testing.expectEqual(@as(usize, 1), loaded.nodeCount());
    try std.testing.expectEqual(ExternalInfo.stdlib, loaded.getNode(.root).?.external);
}
