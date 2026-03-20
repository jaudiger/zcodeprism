const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const node_mod = @import("../core/node.zig");
const edge_mod = @import("../core/edge.zig");
const types = @import("../core/types.zig");
const metrics_mod = @import("../core/metrics.zig");
const lang = @import("../languages/language.zig");

const Graph = graph_mod.Graph;
const FrozenGraph = graph_mod.FrozenGraph;
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

fn writeNodeLine(writer: *std.Io.Writer, n: Node) !void {
    var stream: std.json.Stringify = .{ .writer = writer };
    try stream.beginObject();
    try stream.objectField("_type");
    try stream.write("node");
    try stream.objectField("id");
    try stream.write(@intFromEnum(n.id));
    try stream.objectField("name");
    try stream.write(n.name);
    try stream.objectField("kind");
    try stream.write(@tagName(n.kind));
    try stream.objectField("language");
    if (n.language) |l| {
        try stream.write(@tagName(l));
    } else {
        try stream.write(null);
    }
    try stream.objectField("file_path");
    try stream.write(n.file_path);
    try stream.objectField("line_start");
    try stream.write(n.line_start);
    try stream.objectField("line_end");
    try stream.write(n.line_end);
    try stream.objectField("visibility");
    try stream.write(@tagName(n.visibility));
    try stream.objectField("parent_id");
    if (n.parent_id) |pid| {
        try stream.write(@intFromEnum(pid));
    } else {
        try stream.write(null);
    }
    try stream.objectField("doc");
    try stream.write(n.doc);
    try stream.objectField("signature");
    try stream.write(n.signature);
    try stream.objectField("content_hash");
    if (n.content_hash) |ch| {
        var hex_buf: [24]u8 = undefined;
        for (ch, 0..) |byte, i| {
            _ = std.fmt.bufPrint(hex_buf[i * 2 ..][0..2], "{x:0>2}", .{byte}) catch unreachable;
        }
        try stream.write(@as([]const u8, &hex_buf));
    } else {
        try stream.write(null);
    }
    try stream.objectField("external");
    switch (n.external) {
        .none => try stream.write(null),
        .stdlib => try stream.write("stdlib"),
        .dependency => |d| {
            try stream.beginObject();
            try stream.objectField("type");
            try stream.write("dependency");
            try stream.objectField("version");
            try stream.write(d.version);
            try stream.endObject();
        },
    }
    try stream.objectField("lang_meta");
    try n.lang_meta.writeJson(&stream);
    try stream.objectField("metrics");
    if (n.metrics) |m| {
        try m.writeJson(&stream);
    } else {
        try stream.write(null);
    }
    try stream.endObject();
    try writer.writeByte('\n');
}

fn writeEdgeLine(writer: *std.Io.Writer, e: Edge) !void {
    var stream: std.json.Stringify = .{ .writer = writer };
    try stream.beginObject();
    try stream.objectField("_type");
    try stream.write("edge");
    try stream.objectField("source_id");
    try stream.write(@intFromEnum(e.source_id));
    try stream.objectField("target_id");
    try stream.write(@intFromEnum(e.target_id));
    try stream.objectField("edge_type");
    try stream.write(@tagName(e.edge_type));
    try stream.objectField("source");
    try stream.write(@tagName(e.source));
    try stream.endObject();
    try writer.writeByte('\n');
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

fn dupeAndOwn(allocator: std.mem.Allocator, g: *Graph, str: []const u8) ![]const u8 {
    const duped = try allocator.dupe(u8, str);
    g.addOwnedBuffer(allocator, duped) catch {
        allocator.free(duped);
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

fn parseExternal(allocator: std.mem.Allocator, g: *Graph, val: std.json.Value) !ExternalInfo {
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
                const ver = if (ver_str) |v| try dupeAndOwn(allocator, g, v) else null;
                return .{ .dependency = .{ .version = ver } };
            }
            return .{ .none = {} };
        },
        else => return .{ .none = {} },
    }
}

/// Register heap-allocated buffers from a parsed LangMeta with the graph's
/// ownership tracker. On failure, frees any buffers not yet registered.
fn registerLangMetaBuffers(allocator: std.mem.Allocator, g: *Graph, meta: LangMeta) !void {
    const buffers: [3]?[]const u8 = switch (meta) {
        .rust => |rm| .{ rm.abi, rm.derives, rm.attributes },
        .zig => |zm| .{ zm.calling_convention, null, null },
        .none => .{ null, null, null },
    };
    var registered: usize = 0;
    errdefer {
        // Free any buffers that were not yet registered.
        for (buffers[registered..]) |maybe_buf| {
            if (maybe_buf) |b| allocator.free(b);
        }
    }
    for (buffers) |maybe_buf| {
        if (maybe_buf) |b| {
            try g.addOwnedBuffer(allocator, b);
        }
        registered += 1;
    }
}

/// Parse a single JSON node record and add it to the graph. Returns null on skip.
fn parseNodeFromJson(allocator: std.mem.Allocator, g: *Graph, obj: std.json.ObjectMap) !?NodeId {
    const name_str = jsonStr(obj.get("name") orelse return null) orelse return null;
    const kind_str = jsonStr(obj.get("kind") orelse return null) orelse return null;
    const lang_val = obj.get("language") orelse return null;
    const vis_str = jsonStr(obj.get("visibility") orelse return null) orelse return null;

    const name = try dupeAndOwn(allocator, g, name_str);
    const kind = std.meta.stringToEnum(NodeKind, kind_str) orelse return null;
    const language: ?types.Language = if (lang_val == .null) null else if (jsonStr(lang_val)) |ls| std.meta.stringToEnum(types.Language, ls) else return null;
    const visibility = std.meta.stringToEnum(Visibility, vis_str) orelse return null;

    const file_path_str = if (obj.get("file_path")) |v| jsonOptStr(v) else null;
    const file_path = if (file_path_str) |s| try dupeAndOwn(allocator, g, s) else null;

    const doc_str = if (obj.get("doc")) |v| jsonOptStr(v) else null;
    const doc = if (doc_str) |s| try dupeAndOwn(allocator, g, s) else null;

    const sig_str = if (obj.get("signature")) |v| jsonOptStr(v) else null;
    const signature = if (sig_str) |s| try dupeAndOwn(allocator, g, s) else null;

    const parent_id: ?NodeId = if (obj.get("parent_id")) |v| blk: {
        const i = jsonOptInt(v) orelse break :blk null;
        break :blk @enumFromInt(@as(u64, @intCast(i)));
    } else null;

    const line_start: ?u32 = if (obj.get("line_start")) |v| blk: {
        const i = jsonOptInt(v) orelse break :blk null;
        break :blk @intCast(i);
    } else null;
    const line_end: ?u32 = if (obj.get("line_end")) |v| blk: {
        const i = jsonOptInt(v) orelse break :blk null;
        break :blk @intCast(i);
    } else null;

    const content_hash: ?[12]u8 = if (obj.get("content_hash")) |v| blk: {
        const s = jsonOptStr(v) orelse break :blk null;
        break :blk parseContentHash(s);
    } else null;

    const external = if (obj.get("external")) |v| try parseExternal(allocator, g, v) else ExternalInfo{ .none = {} };

    const lang_meta = if (obj.get("lang_meta")) |v| blk: {
        const meta = try LangMeta.parseJson(allocator, v);
        try registerLangMetaBuffers(allocator, g, meta);
        break :blk meta;
    } else LangMeta{ .none = {} };

    const metrics = if (obj.get("metrics")) |v| Metrics.parseJson(v) else null;

    return try g.addNode(allocator, .{
        .id = .root,
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
}

/// Parse a single JSON edge record and add it to the graph. Returns false on skip.
fn parseEdgeFromJson(allocator: std.mem.Allocator, g: *Graph, obj: std.json.ObjectMap) !bool {
    const src_id_val = obj.get("source_id") orelse return false;
    const tgt_id_val = obj.get("target_id") orelse return false;
    const et_val = obj.get("edge_type") orelse return false;

    const src_id = switch (src_id_val) {
        .integer => |i| @as(u64, @intCast(i)),
        else => return false,
    };
    const tgt_id = switch (tgt_id_val) {
        .integer => |i| @as(u64, @intCast(i)),
        else => return false,
    };
    const et_str = jsonStr(et_val) orelse return false;
    const edge_type = std.meta.stringToEnum(EdgeType, et_str) orelse return false;

    const edge_source: EdgeSource = if (obj.get("source")) |v| blk: {
        const s = jsonStr(v) orelse break :blk .tree_sitter;
        break :blk std.meta.stringToEnum(EdgeSource, s) orelse .tree_sitter;
    } else .tree_sitter;

    if (src_id >= g.nodes.items.len or tgt_id >= g.nodes.items.len) return false;

    try g.edges.append(allocator, .{
        .source_id = @enumFromInt(src_id),
        .target_id = @enumFromInt(tgt_id),
        .edge_type = edge_type,
        .source = edge_source,
    });
    return true;
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
pub fn exportJsonl(allocator: std.mem.Allocator, fg: FrozenGraph, writer: *std.Io.Writer) !void {
    const g = fg.graph;
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
    var g = Graph.init("");
    errdefer g.deinit(allocator);

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
            _ = try parseNodeFromJson(allocator, &g, obj);
        } else if (std.mem.eql(u8, type_val.string, "edge")) {
            _ = try parseEdgeFromJson(allocator, &g, obj);
        }
    }

    try g.rebuildEdgeIndex(allocator);
    _ = try g.freeze(allocator);
    return g;
}

/// Build a test graph with 3 diverse nodes and 2 edges for use in tests.
fn createTestGraph(allocator: std.mem.Allocator) !Graph {
    var g = Graph.init("/tmp/test-project");

    // Node 0: file node
    _ = try g.addNode(allocator, .{
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
    _ = try g.addNode(allocator, .{
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
    _ = try g.addNode(allocator, .{
        .id = .root,
        .name = "Allocator",
        .kind = .type_def,
        .language = .zig,
        .external = .{ .stdlib = {} },
    });

    // Edge 0: function uses type
    _ = try g.addEdgeIfNew(allocator, .{
        .source_id = @enumFromInt(1),
        .target_id = @enumFromInt(2),
        .edge_type = .uses_type,
        .source = .tree_sitter,
    });

    // Edge 1: file exports function
    _ = try g.addEdgeIfNew(allocator, .{
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
    defer g.deinit(std.testing.allocator);

    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    const fg = try g.freeze(std.testing.allocator);
    try exportJsonl(std.testing.allocator, fg, &aw.writer);
    try aw.writer.flush();
    var loaded = try importJsonl(std.testing.allocator, aw.written());
    defer loaded.deinit(std.testing.allocator);

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
    defer g.deinit(std.testing.allocator);

    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    const fg = try g.freeze(std.testing.allocator);
    try exportJsonl(std.testing.allocator, fg, &aw.writer);
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
    defer g.deinit(std.testing.allocator);

    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    const fg = try g.freeze(std.testing.allocator);
    try exportJsonl(std.testing.allocator, fg, &aw.writer);
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
    defer g.deinit(std.testing.allocator);

    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    const fg = try g.freeze(std.testing.allocator);
    try exportJsonl(std.testing.allocator, fg, &aw.writer);
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
    var g = Graph.init("/tmp/test-project");
    defer g.deinit(std.testing.allocator);

    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    const fg = try g.freeze(std.testing.allocator);
    try exportJsonl(std.testing.allocator, fg, &aw.writer);
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
    defer loaded.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 0), loaded.nodeCount());
    try std.testing.expectEqual(@as(usize, 0), loaded.edgeCount());
}

test "jsonl preserves null fields as explicit null" {
    // Arrange
    var g = Graph.init("/tmp/test-project");
    defer g.deinit(std.testing.allocator);

    _ = try g.addNode(std.testing.allocator, .{
        .id = .root,
        .name = "bare",
        .kind = .function,
        .language = .zig,
        // doc, signature, content_hash all null by default
    });

    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    const fg = try g.freeze(std.testing.allocator);
    try exportJsonl(std.testing.allocator, fg, &aw.writer);
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
    var g = Graph.init("/tmp/test-project");
    defer g.deinit(std.testing.allocator);

    _ = try g.addNode(std.testing.allocator, .{
        .id = .root,
        .name = "std",
        .kind = .module,
        .language = .zig,
        .external = .{ .stdlib = {} },
    });

    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    const fg = try g.freeze(std.testing.allocator);
    try exportJsonl(std.testing.allocator, fg, &aw.writer);
    try aw.writer.flush();
    var loaded = try importJsonl(std.testing.allocator, aw.written());
    defer loaded.deinit(std.testing.allocator);

    // Assert
    try std.testing.expectEqual(@as(usize, 1), loaded.nodeCount());
    try std.testing.expectEqual(ExternalInfo.stdlib, loaded.getNode(.root).?.external);
}

test "jsonl round-trip preserves union_def kind" {
    // Arrange
    var g = Graph.init("/tmp/test-project");
    defer g.deinit(std.testing.allocator);

    _ = try g.addNode(std.testing.allocator, .{
        .id = .root,
        .name = "MyUnion",
        .kind = .union_def,
        .language = .zig,
        .visibility = .public,
    });

    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    const fg = try g.freeze(std.testing.allocator);
    try exportJsonl(std.testing.allocator, fg, &aw.writer);
    try aw.writer.flush();
    var loaded = try importJsonl(std.testing.allocator, aw.written());
    defer loaded.deinit(std.testing.allocator);

    // Assert
    try std.testing.expectEqual(@as(usize, 1), loaded.nodeCount());
    try std.testing.expectEqual(NodeKind.union_def, loaded.getNode(.root).?.kind);
}

test "jsonl round-trip preserves is_packed metadata" {
    // Arrange
    var g = Graph.init("/tmp/test-project");
    defer g.deinit(std.testing.allocator);

    _ = try g.addNode(std.testing.allocator, .{
        .id = .root,
        .name = "PackedStruct",
        .kind = .type_def,
        .language = .zig,
        .lang_meta = .{ .zig = .{ .is_packed = true } },
    });

    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    const fg = try g.freeze(std.testing.allocator);
    try exportJsonl(std.testing.allocator, fg, &aw.writer);
    try aw.writer.flush();
    var loaded = try importJsonl(std.testing.allocator, aw.written());
    defer loaded.deinit(std.testing.allocator);

    // Assert
    try std.testing.expectEqual(@as(usize, 1), loaded.nodeCount());
    const meta = loaded.getNode(.root).?.lang_meta;
    try std.testing.expect(meta.zig.is_packed);
    try std.testing.expect(!meta.zig.is_extern);
}
