const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const query_mod = @import("../core/query.zig");
const types = @import("../core/types.zig");
const node_mod = @import("../core/node.zig");
const edge_mod = @import("../core/edge.zig");
const generation_mod = @import("../core/generation.zig");
const metrics_mod = @import("../core/metrics.zig");
const lang_mod = @import("../languages/language.zig");
const source_map = @import("../parser/source_map.zig");
const cursor_manager_mod = @import("../explorer/cursor_manager.zig");
const tree_sitter_api = @import("../parser/tree_sitter_api.zig");
const ts = @import("tree-sitter");
const complexity_mod = @import("../analyzer/complexity.zig");
const dead_code_mod = @import("../analyzer/dead_code.zig");
const duplicates_mod = @import("../analyzer/duplicates.zig");
const impact_mod = @import("../analyzer/impact.zig");
const coupling_mod = @import("../analyzer/coupling.zig");
const cycles_mod = @import("../analyzer/cycles.zig");
const json_writer_mod = @import("json_writer.zig");

const Graph = graph_mod.Graph;
const Direction = types.Direction;
const Node = node_mod.Node;
const Edge = edge_mod.Edge;
const NodeId = types.NodeId;
const EdgeId = types.EdgeId;
const NodeKind = types.NodeKind;
const EdgeType = types.EdgeType;
const EdgeSource = types.EdgeSource;
const Visibility = types.Visibility;
const Language = types.Language;
const GraphGeneration = generation_mod.GraphGeneration;
const CursorManager = cursor_manager_mod.CursorManager;
const CursorOptions = cursor_manager_mod.CursorOptions;
const ExternalInfo = lang_mod.ExternalInfo;
const LangMeta = lang_mod.LangMeta;
const JsonWriter = json_writer_mod.JsonWriter;

/// Errors returned by MCP tool handlers.
pub const HandlerError = error{OutOfMemory};

// -- Param helpers --

fn parseNodeId(hex_str: []const u8) ?NodeId {
    return types.parseNodeId(hex_str, 16);
}

fn getArgs(params: ?std.json.Value) ?std.json.ObjectMap {
    const p = params orelse return null;
    if (p != .object) return null;
    const args_val = p.object.get("arguments") orelse return null;
    if (args_val != .object) return null;
    return args_val.object;
}

fn getOptionalString(args: ?std.json.ObjectMap, key: []const u8) ?[]const u8 {
    const a = args orelse return null;
    const val = a.get(key) orelse return null;
    if (val != .string) return null;
    return val.string;
}

fn getOptionalBool(args: ?std.json.ObjectMap, key: []const u8, default: bool) bool {
    const a = args orelse return default;
    const val = a.get(key) orelse return default;
    if (val != .bool) return default;
    return val.bool;
}

fn getOptionalInt(args: ?std.json.ObjectMap, key: []const u8, default: u32) u32 {
    const a = args orelse return default;
    const val = a.get(key) orelse return default;
    if (val != .integer) return default;
    const i = val.integer;
    if (i < 0) return default;
    return @intCast(@min(i, std.math.maxInt(u32)));
}

fn getOptionalFloat(args: ?std.json.ObjectMap, key: []const u8, default: f64) f64 {
    const a = args orelse return default;
    const val = a.get(key) orelse return default;
    return switch (val) {
        .float => val.float,
        .integer => @floatFromInt(val.integer),
        else => default,
    };
}

fn collectNodeIds(allocator: std.mem.Allocator, args: ?std.json.ObjectMap, key: []const u8) HandlerError![]const NodeId {
    const a = args orelse return &.{};
    const val = a.get(key) orelse return &.{};
    switch (val) {
        .string => |s| {
            const nid = parseNodeId(s) orelse return &.{};
            const result = try allocator.alloc(NodeId, 1);
            result[0] = nid;
            return result;
        },
        .array => |arr| {
            if (arr.items.len == 0) return &.{};
            var count: usize = 0;
            for (arr.items) |item| {
                if (item == .string) {
                    if (parseNodeId(item.string) != null) count += 1;
                }
            }
            if (count == 0) return &.{};
            const result = try allocator.alloc(NodeId, count);
            var pos: usize = 0;
            for (arr.items) |item| {
                if (item == .string) {
                    if (parseNodeId(item.string)) |nid| {
                        result[pos] = nid;
                        pos += 1;
                    }
                }
            }
            return result[0..pos];
        },
        else => return &.{},
    }
}

fn parseEdgeType(name: []const u8) ?EdgeType {
    return types.parseEnum(EdgeType, name);
}

fn parseNodeKind(name: []const u8) ?NodeKind {
    return types.parseEnum(NodeKind, name);
}

fn parseLanguage(name: []const u8) ?Language {
    return types.parseEnum(Language, name);
}

fn parseVisibility(name: []const u8) ?Visibility {
    return types.parseEnum(Visibility, name);
}

/// Parse the "edge_types" JSON array into buf and return the filled slice.
fn parseEdgeTypesArray(args: ?std.json.ObjectMap, buf: []EdgeType) []EdgeType {
    const a = args orelse return buf[0..0];
    const val = a.get("edge_types") orelse return buf[0..0];
    if (val != .array) return buf[0..0];
    var count: usize = 0;
    for (val.array.items) |item| {
        if (item == .string) {
            if (parseEdgeType(item.string)) |et| {
                if (count < buf.len) {
                    buf[count] = et;
                    count += 1;
                }
            }
        }
    }
    return buf[0..count];
}

fn parseDirection(name: []const u8) Direction {
    if (std.mem.eql(u8, name, "in")) return .in;
    if (std.mem.eql(u8, name, "out")) return .out;
    return .both;
}

fn edgeTypeInSlice(et: EdgeType, slice: []const EdgeType) bool {
    for (slice) |s| {
        if (s == et) return true;
    }
    return false;
}

/// BFS from start, following edges in both directions and the parent-child
/// hierarchy, up to max_depth hops.
fn collectReachable(
    allocator: std.mem.Allocator,
    g: *const Graph,
    start: NodeId,
    max_depth: u32,
    out: *std.AutoHashMapUnmanaged(NodeId, void),
) !void {
    const Entry = struct { id: NodeId, depth: u32 };
    var queue = std.ArrayList(Entry){};
    defer queue.deinit(allocator);

    try out.put(allocator, start, {});
    try queue.append(allocator, .{ .id = start, .depth = 0 });

    var front: usize = 0;
    while (front < queue.items.len) {
        const current = queue.items[front];
        front += 1;
        if (current.depth >= max_depth) continue;

        const next_depth = current.depth + 1;

        for (g.outEdges(current.id)) |eid| {
            const e = g.edges.items[@intFromEnum(eid)];
            const gop = try out.getOrPut(allocator, e.target_id);
            if (!gop.found_existing) try queue.append(allocator, .{ .id = e.target_id, .depth = next_depth });
        }
        for (g.inEdges(current.id)) |eid| {
            const e = g.edges.items[@intFromEnum(eid)];
            const gop = try out.getOrPut(allocator, e.source_id);
            if (!gop.found_existing) try queue.append(allocator, .{ .id = e.source_id, .depth = next_depth });
        }
        if (g.getParent(current.id)) |pid| {
            const gop = try out.getOrPut(allocator, pid);
            if (!gop.found_existing) try queue.append(allocator, .{ .id = pid, .depth = next_depth });
        }
        for (g.getChildren(current.id)) |cid| {
            const gop = try out.getOrPut(allocator, cid);
            if (!gop.found_existing) try queue.append(allocator, .{ .id = cid, .depth = next_depth });
        }
    }
}

// -- JSON writing helpers --

/// Format a nanoTimestamp as ISO 8601 UTC into the provided buffer.
fn formatIso8601(nanos: i128, buf: *[30]u8) usize {
    const epoch = std.time.epoch.EpochSeconds{
        .secs = @intCast(@divTrunc(nanos, std.time.ns_per_s)),
    };
    const day_secs = epoch.getDaySeconds();
    const year_day = epoch.getEpochDay().calculateYearDay();
    const month_day = year_day.calculateMonthDay();
    return (std.fmt.bufPrint(buf, "{d:0>4}-{d:0>2}-{d:0>2}T{d:0>2}:{d:0>2}:{d:0>2}Z", .{
        year_day.year,
        month_day.month.numeric(),
        @as(u6, month_day.day_index) + 1,
        day_secs.getHoursIntoDay(),
        day_secs.getMinutesIntoHour(),
        day_secs.getSecondsIntoMinute(),
    }) catch unreachable).len;
}

fn relativePath(file_path: ?[]const u8, project_root: []const u8) ?[]const u8 {
    const fp = file_path orelse return null;
    if (project_root.len > 0 and std.mem.startsWith(u8, fp, project_root)) {
        var offset = project_root.len;
        if (offset < fp.len and fp[offset] == '/') offset += 1;
        return fp[offset..];
    }
    return fp;
}

/// Write the external field value (none -> null, stdlib -> "stdlib",
/// dependency -> version string or "dependency").
fn writeExternalValue(w: JsonWriter, external: ExternalInfo) HandlerError!void {
    try w.field("external");
    switch (external) {
        .none => try w.write(null),
        .stdlib => try w.write("stdlib"),
        .dependency => |d| {
            if (d.version) |v| {
                try w.write(v);
            } else {
                try w.write("dependency");
            }
        },
    }
}

/// Fields shared by both summary and full node representations:
/// id, name, kind, language, file, line_start, line_end, col_start, col_end, visibility.
fn writeNodeCoreFields(w: JsonWriter, n: *const Node, id: NodeId, project_root: []const u8) HandlerError!void {
    try w.fieldNodeIdHex("id", id);
    try w.fieldValue("name", n.name);
    try w.tagFieldValue("kind", n.kind);
    try w.optionalTagFieldValue("language", n.language);
    try w.fieldValue("file", relativePath(n.file_path, project_root));
    try w.optionalFieldValue("line_start", n.line_start);
    try w.optionalFieldValue("line_end", n.line_end);
    try w.optionalFieldValue("col_start", n.col_start);
    try w.optionalFieldValue("col_end", n.col_end);
    try w.tagFieldValue("visibility", n.visibility);
}

fn writeOptionalMetrics(w: JsonWriter, metrics: ?metrics_mod.Metrics) HandlerError!void {
    try w.field("metrics");
    if (metrics) |m| {
        m.writeJson(w.s) catch return error.OutOfMemory;
    } else {
        try w.write(null);
    }
}

fn writeNodeSummary(w: JsonWriter, n: *const Node, id: NodeId, project_root: []const u8) HandlerError!void {
    try w.beginObject();
    try writeNodeCoreFields(w, n, id, project_root);
    try writeExternalValue(w, n.external);
    try w.optionalFieldValue("signature", n.signature);
    try writeOptionalMetrics(w, n.metrics);
    try w.endObject();
}

fn writeFullNode(w: JsonWriter, n: *const Node, id: NodeId, project_root: []const u8, source_text: ?[]const u8) HandlerError!void {
    try w.beginObject();
    try writeNodeCoreFields(w, n, id, project_root);
    try w.optionalFieldNodeIdHex("parent_id", n.parent_id);
    try w.optionalFieldValue("doc", n.doc);
    try w.optionalFieldValue("signature", n.signature);
    try writeExternalValue(w, n.external);
    try w.optionalFieldHashHex("content_hash", n.content_hash);
    try writeOptionalMetrics(w, n.metrics);
    try w.field("lang_meta");
    n.lang_meta.writeJson(w.s) catch return error.OutOfMemory;
    if (source_text) |src| {
        try w.fieldValue("source", src);
    }
    try w.endObject();
}

// -- Source extraction --

fn extractSource(allocator: std.mem.Allocator, project_root: []const u8, file_path: []const u8, line_start: u32, line_end: u32, context_lines: u32, part: []const u8) ?[]const u8 {
    // file_path may be relative; resolve against project_root.
    const abs_path = if (std.fs.path.isAbsolute(file_path))
        file_path
    else blk: {
        break :blk std.fs.path.join(allocator, &.{ project_root, file_path }) catch return null;
    };
    defer if (!std.fs.path.isAbsolute(file_path)) allocator.free(abs_path);
    const content = source_map.mmapFile(abs_path) catch return null;
    defer source_map.unmapFile(content);
    if (content.len == 0) return null;

    const start_1 = if (line_start > 0) line_start else 1;
    const end_1 = if (line_end > 0) line_end else start_1;

    const ctx_start = if (start_1 > context_lines) start_1 - context_lines else 1;
    const ctx_end = end_1 + context_lines;

    var line_num: u32 = 1;
    var region_start: usize = 0;
    var region_end: usize = content.len;
    var sig_end: usize = 0;
    var body_start: usize = 0;

    var i: usize = 0;
    while (i <= content.len) : (i += 1) {
        const at_end = i == content.len;
        const is_newline = !at_end and content[i] == '\n';

        if (is_newline or at_end) {
            if (line_num == ctx_start) region_start = blk: {
                var s = i;
                while (s > 0 and content[s - 1] != '\n') s -= 1;
                break :blk s;
            };
            if (line_num == start_1) {
                sig_end = i;
                body_start = if (at_end) i else i + 1;
            }
            if (line_num == ctx_end or at_end) {
                region_end = if (at_end) content.len else i;
                break;
            }
            line_num += 1;
        }
    }

    const slice = if (std.mem.eql(u8, part, "signature")) blk: {
        if (sig_end <= region_start) return null;
        break :blk content[region_start..sig_end];
    } else if (std.mem.eql(u8, part, "body")) blk: {
        if (body_start >= region_end) return null;
        break :blk content[body_start..region_end];
    } else content[region_start..region_end];

    return allocator.dupe(u8, slice) catch return null;
}

// -- Handler dispatch --

/// Dispatch a named MCP tool call to the appropriate handler.
/// Returns JSON response bytes, or null if the tool is unknown.
pub fn handleToolCall(
    allocator: std.mem.Allocator,
    gen: *GraphGeneration,
    cursor_mgr: *CursorManager,
    tool_name: []const u8,
    params: ?std.json.Value,
) HandlerError!?[]const u8 {
    if (std.mem.eql(u8, tool_name, "graph.stats")) {
        return try handleStats(allocator, gen, params);
    } else if (std.mem.eql(u8, tool_name, "graph.search")) {
        return try handleSearch(allocator, gen, params);
    } else if (std.mem.eql(u8, tool_name, "graph.get_nodes")) {
        return try handleGetNodes(allocator, gen, params);
    } else if (std.mem.eql(u8, tool_name, "graph.get_source")) {
        return try handleGetSource(allocator, gen, params);
    } else if (std.mem.eql(u8, tool_name, "graph.get_edges")) {
        return try handleGetEdges(allocator, gen, params);
    } else if (std.mem.eql(u8, tool_name, "graph.path")) {
        return try handlePath(allocator, gen, params);
    } else if (std.mem.eql(u8, tool_name, "explorer.cursor_create")) {
        return try handleCursorCreate(allocator, gen, cursor_mgr, params);
    } else if (std.mem.eql(u8, tool_name, "explorer.cursor_move")) {
        return try handleCursorMove(allocator, gen, cursor_mgr, params);
    } else if (std.mem.eql(u8, tool_name, "explorer.cursor_close")) {
        return try handleCursorClose(allocator, cursor_mgr, params);
    } else if (std.mem.eql(u8, tool_name, "explorer.cursor_expand")) {
        return try handleCursorExpand(allocator, gen, cursor_mgr, params);
    } else if (std.mem.eql(u8, tool_name, "explorer.cursor_query")) {
        return try handleCursorQuery(allocator, gen, cursor_mgr, params);
    } else if (std.mem.eql(u8, tool_name, "explorer.diff")) {
        return try handleDiff(allocator, gen, params);
    } else if (std.mem.eql(u8, tool_name, "explorer.annotate")) {
        return try handleAnnotate(allocator, cursor_mgr, params);
    } else if (std.mem.eql(u8, tool_name, "explorer.annotations")) {
        return try handleAnnotations(allocator, cursor_mgr, params);
    } else if (std.mem.eql(u8, tool_name, "analysis.duplicates")) {
        return try handleDuplicates(allocator, gen, params);
    } else if (std.mem.eql(u8, tool_name, "analysis.complexity")) {
        return try handleComplexity(allocator, gen, params);
    } else if (std.mem.eql(u8, tool_name, "analysis.dead_code")) {
        return try handleDeadCode(allocator, gen, params);
    } else if (std.mem.eql(u8, tool_name, "analysis.dependency_cycles")) {
        return try handleDependencyCycles(allocator, gen, params);
    } else if (std.mem.eql(u8, tool_name, "analysis.coupling")) {
        return try handleCoupling(allocator, gen, params);
    } else if (std.mem.eql(u8, tool_name, "analysis.impact")) {
        return try handleImpact(allocator, gen, params);
    }
    return null;
}

// -- graph.stats --

fn handleStats(allocator: std.mem.Allocator, gen: *GraphGeneration, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const g = &gen.graph;

    const scope = getOptionalString(args, "scope");
    const language_str = getOptionalString(args, "language");
    const include_tests = getOptionalBool(args, "include_tests", false);
    const include_external = getOptionalBool(args, "include_external_nodes", false);

    const stats = query_mod.computeStats(allocator, g, .{
        .scope = scope,
        .language = if (language_str) |ls| parseLanguage(ls) else null,
        .include_tests = include_tests,
        .include_external = include_external,
    }) catch return error.OutOfMemory;

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    const w: JsonWriter = .{ .s = &stream };

    try w.beginObject();

    try w.fieldValue("project_root", g.project_root);

    try w.field("languages");
    try w.beginArray();
    if (stats.has_zig) try w.write("zig");
    if (stats.has_rust) try w.write("rust");
    try w.endArray();

    try w.fieldValue("total_files", stats.node_counts[@intFromEnum(NodeKind.file)]);
    try w.fieldValue("total_lines", stats.total_lines);

    try w.field("source_hash");
    try w.hashHex(gen.source_hash);

    try w.field("last_indexed");
    var ts_buf: [30]u8 = undefined;
    const ts_len = formatIso8601(gen.indexed_at, &ts_buf);
    try w.write(@as([]const u8, ts_buf[0..ts_len]));

    // Node counts by kind
    try w.field("nodes");
    try w.beginObject();
    inline for (@typeInfo(NodeKind).@"enum".fields) |f| {
        const count = stats.node_counts[f.value];
        if (count > 0) {
            try w.fieldValue(f.name, count);
        }
    }
    try w.endObject();

    // Edge counts by type
    try w.field("edges");
    try w.beginObject();
    inline for (@typeInfo(EdgeType).@"enum".fields) |f| {
        const count = stats.edge_counts[f.value];
        if (count > 0) {
            try w.fieldValue(f.name, count);
        }
    }
    try w.endObject();

    try w.field("externals");
    try w.beginObject();
    try w.fieldValue("stdlib_symbols", stats.stdlib_count);
    try w.fieldValue("dependency_symbols", stats.dep_count);
    try w.endObject();

    try w.endObject();

    return aw.toOwnedSlice() catch return error.OutOfMemory;
}

// -- graph.search --

fn handleSearch(allocator: std.mem.Allocator, gen: *GraphGeneration, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const g = &gen.graph;

    const query_str = getOptionalString(args, "query");
    const kind_str = getOptionalString(args, "kind");
    const visibility_str = getOptionalString(args, "visibility");
    const language_str = getOptionalString(args, "language");
    const scope = getOptionalString(args, "scope");
    const include_tests = getOptionalBool(args, "include_tests", false);
    const offset = getOptionalInt(args, "offset", 0);
    const limit = getOptionalInt(args, "limit", 50);

    const external_str = getOptionalString(args, "external");
    const ext_filter: query_mod.ExternalFilter = if (external_str) |es| blk: {
        if (std.mem.eql(u8, es, "exclude")) break :blk .exclude;
        if (std.mem.eql(u8, es, "only")) break :blk .only;
        break :blk .include;
    } else .include;

    const result = query_mod.search(allocator, g, .{
        .query = query_str,
        .kind = if (kind_str) |ks| parseNodeKind(ks) else null,
        .visibility = if (visibility_str) |vs| parseVisibility(vs) else null,
        .language = if (language_str) |ls| parseLanguage(ls) else null,
        .external = ext_filter,
        .include_tests = include_tests,
        .scope = scope,
        .offset = offset,
        .limit = limit,
    }) catch return error.OutOfMemory;
    defer result.deinit(allocator);

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    const w: JsonWriter = .{ .s = &stream };

    try w.beginObject();
    try w.fieldValue("total_matches", result.total_matches);

    try w.field("nodes");
    try w.beginArray();
    for (result.nodes) |nid| {
        const n = g.getNode(nid) orelse continue;
        try writeNodeSummary(w, n, nid, g.project_root);
    }
    try w.endArray();

    try w.endObject();

    return aw.toOwnedSlice() catch return error.OutOfMemory;
}

// -- graph.get_nodes --

fn handleGetNodes(allocator: std.mem.Allocator, gen: *GraphGeneration, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const g = &gen.graph;

    const node_ids = try collectNodeIds(allocator, args, "node_ids");
    defer if (node_ids.len > 0) allocator.free(node_ids);

    const include_source = getOptionalBool(args, "include_source", false);

    const result = query_mod.getNodes(allocator, g, node_ids, .{}) catch return error.OutOfMemory;
    defer result.deinit(allocator);

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    const w: JsonWriter = .{ .s = &stream };

    try w.beginObject();
    try w.field("nodes");
    try w.beginArray();

    for (result.nodes) |detail| {
        const n = detail.node;
        var source_text: ?[]const u8 = null;
        defer if (source_text) |s| allocator.free(s);

        if (include_source) {
            if (n.external != .none) {
                source_text = null;
            } else if (n.file_path) |fp| {
                const ls = n.line_start orelse 1;
                const le = n.line_end orelse ls;
                source_text = extractSource(allocator, g.project_root, fp, ls, le, 0, "full");
            }
        }

        try writeFullNode(w, n, detail.id, g.project_root, if (include_source) source_text orelse @as(?[]const u8, null) else null);
    }

    try w.endArray();
    try w.endObject();

    return aw.toOwnedSlice() catch return error.OutOfMemory;
}

// -- graph.get_source --

fn handleGetSource(allocator: std.mem.Allocator, gen: *GraphGeneration, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const g = &gen.graph;

    const node_ids = try collectNodeIds(allocator, args, "node_ids");
    defer if (node_ids.len > 0) allocator.free(node_ids);

    const context_lines = getOptionalInt(args, "context_lines", 0);
    const part = getOptionalString(args, "part") orelse "full";

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    const w: JsonWriter = .{ .s = &stream };

    try w.beginObject();
    try w.field("sources");
    try w.beginArray();

    for (node_ids) |nid| {
        const n = g.getNode(nid) orelse continue;

        try w.beginObject();
        try w.fieldNodeIdHex("id", nid);

        try w.field("source");
        if (n.external != .none) {
            try w.write(null);
        } else if (n.file_path) |fp| {
            const ls = n.line_start orelse 1;
            const le = n.line_end orelse ls;
            const src = extractSource(allocator, g.project_root, fp, ls, le, context_lines, part);
            defer if (src) |s| allocator.free(s);
            if (src) |s| {
                try w.write(s);
            } else {
                try w.write(null);
            }
        } else {
            try w.write(null);
        }
        try w.endObject();
    }

    try w.endArray();
    try w.endObject();

    return aw.toOwnedSlice() catch return error.OutOfMemory;
}

// -- graph.get_edges --

fn handleGetEdges(allocator: std.mem.Allocator, gen: *GraphGeneration, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const g = &gen.graph;

    const node_ids = try collectNodeIds(allocator, args, "node_ids");
    defer if (node_ids.len > 0) allocator.free(node_ids);

    const direction_str = getOptionalString(args, "direction") orelse "both";
    const edge_type_str = getOptionalString(args, "edge_type");
    const include_external = getOptionalBool(args, "include_external", true);
    const offset = getOptionalInt(args, "offset", 0);
    const limit = getOptionalInt(args, "limit", 50);

    const result = query_mod.getEdges(allocator, g, node_ids, .{
        .direction = parseDirection(direction_str),
        .edge_type = if (edge_type_str) |es| parseEdgeType(es) else null,
        .include_external = include_external,
        .offset = offset,
        .limit = limit,
    }) catch return error.OutOfMemory;
    defer result.deinit(allocator);

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    const w: JsonWriter = .{ .s = &stream };

    try w.beginObject();
    try w.fieldValue("total_count", result.total_count);

    try w.field("edges");
    try w.beginArray();

    for (result.edges) |e| {
        try w.beginObject();
        try w.fieldNodeIdHex("from", e.source_id);
        try w.fieldNodeIdHex("to", e.target_id);
        try w.tagFieldValue("type", e.edge_type);
        try w.tagFieldValue("source", e.source);

        // Connected node info
        if (g.getNode(e.target_id)) |tn| {
            try w.field("to_node");
            try writeNodeRef(w, e.target_id, tn);
        }
        if (g.getNode(e.source_id)) |sn| {
            try w.field("from_node");
            try writeNodeRef(w, e.source_id, sn);
        }

        try w.endObject();
    }

    try w.endArray();
    try w.endObject();

    return aw.toOwnedSlice() catch return error.OutOfMemory;
}

/// Minimal node reference: id, name, kind.
fn writeNodeRef(w: JsonWriter, id: NodeId, n: *const Node) HandlerError!void {
    try w.beginObject();
    try w.fieldNodeIdHex("id", id);
    try w.fieldValue("name", n.name);
    try w.tagFieldValue("kind", n.kind);
    try w.endObject();
}

// -- graph.path --

fn handlePath(allocator: std.mem.Allocator, gen: *GraphGeneration, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const g = &gen.graph;

    const from_str = getOptionalString(args, "from_id") orelse return try emptyPathsResult(allocator);
    const to_str = getOptionalString(args, "to_id") orelse return try emptyPathsResult(allocator);
    const from_id = parseNodeId(from_str) orelse return try emptyPathsResult(allocator);
    const to_id = parseNodeId(to_str) orelse return try emptyPathsResult(allocator);

    const max_depth = getOptionalInt(args, "max_depth", 10);
    const max_paths = getOptionalInt(args, "max_paths", 3);

    // Parse edge_types array
    var edge_types_buf: [16]EdgeType = undefined;
    var edge_types_count: usize = 0;
    const edge_types_val: ?[]const EdgeType = blk: {
        const a = args orelse break :blk null;
        const et_val = a.get("edge_types") orelse break :blk null;
        if (et_val != .array) break :blk null;
        for (et_val.array.items) |item| {
            if (item == .string) {
                if (parseEdgeType(item.string)) |et| {
                    if (edge_types_count < edge_types_buf.len) {
                        edge_types_buf[edge_types_count] = et;
                        edge_types_count += 1;
                    }
                }
            }
        }
        if (edge_types_count == 0) break :blk null;
        break :blk edge_types_buf[0..edge_types_count];
    };

    const result = query_mod.findPaths(allocator, g, from_id, to_id, .{
        .edge_types = edge_types_val,
        .max_depth = max_depth,
        .max_paths = max_paths,
    }) catch return error.OutOfMemory;
    defer result.deinit(allocator);

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    const w: JsonWriter = .{ .s = &stream };

    try w.beginObject();
    try w.field("paths");
    try w.beginArray();

    for (result.paths) |path| {
        try w.beginObject();
        try w.fieldValue("length", path.edge_types.len);

        try w.field("nodes");
        try w.beginArray();
        for (path.node_ids) |nid| {
            try w.nodeIdHex(nid);
        }
        try w.endArray();

        try w.field("edges");
        try w.beginArray();
        for (path.edge_types, 0..) |et, idx| {
            try w.beginObject();
            try w.fieldNodeIdHex("from", path.node_ids[idx]);
            try w.fieldNodeIdHex("to", path.node_ids[idx + 1]);
            try w.tagFieldValue("type", et);
            try w.endObject();
        }
        try w.endArray();

        try w.endObject();
    }

    try w.endArray();
    try w.endObject();

    return aw.toOwnedSlice() catch return error.OutOfMemory;
}

fn emptyPathsResult(allocator: std.mem.Allocator) HandlerError![]const u8 {
    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    const w: JsonWriter = .{ .s = &stream };
    try w.beginObject();
    try w.field("paths");
    try w.beginArray();
    try w.endArray();
    try w.endObject();
    return aw.toOwnedSlice() catch return error.OutOfMemory;
}

// -- explorer.cursor_create --

fn writeCursorResponse(w: JsonWriter, cursor_id: []const u8, position: NodeId, g: *const Graph) HandlerError!void {
    try w.beginObject();
    try w.fieldValue("cursor_id", cursor_id);

    try w.field("position");
    if (g.getNode(position)) |n| {
        try writeNodeRef(w, position, n);
    } else {
        try w.beginObject();
        try w.fieldNodeIdHex("id", position);
        try w.fieldValue("kind", "root");
        try w.fieldValue("name", "root");
        try w.endObject();
    }

    try w.field("neighborhood");
    try w.beginObject();
    try w.field("children");
    try w.beginArray();
    const children = g.getChildren(position);
    for (children) |child_id| {
        if (g.getNode(child_id)) |cn| {
            try writeNodeRef(w, child_id, cn);
        }
    }
    try w.endArray();
    try w.field("stats");
    try w.beginObject();
    try w.fieldValue("visible_nodes", g.nodes.items.len);
    try w.fieldValue("visible_edges", g.edges.items.len);
    try w.endObject();
    try w.endObject();

    try w.fieldValue("expires_in_seconds", @as(u32, 600));

    try w.endObject();
}

fn handleCursorCreate(allocator: std.mem.Allocator, gen: *GraphGeneration, cursor_mgr: *CursorManager, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const g = &gen.graph;

    const start_str = getOptionalString(args, "start_node_id");
    const position: NodeId = if (start_str) |s| parseNodeId(s) orelse .root else .root;

    const cursor_id = cursor_mgr.createCursor(position, .{
        .scope = getOptionalString(args, "scope"),
        .include_tests = getOptionalBool(args, "include_tests", false),
        .include_external_nodes = getOptionalBool(args, "include_external_nodes", false),
    }) catch return error.OutOfMemory;

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    const w: JsonWriter = .{ .s = &stream };

    try writeCursorResponse(w, cursor_id, position, g);

    return aw.toOwnedSlice() catch return error.OutOfMemory;
}

// -- explorer.cursor_move --

fn handleCursorMove(allocator: std.mem.Allocator, gen: *GraphGeneration, cursor_mgr: *CursorManager, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const g = &gen.graph;

    const cursor_id = getOptionalString(args, "cursor_id") orelse return try errorResult(allocator, "invalid_cursor");
    const node_id_str = getOptionalString(args, "node_id") orelse return try errorResult(allocator, "missing node_id");
    const node_id = parseNodeId(node_id_str) orelse return try errorResult(allocator, "invalid node_id");

    const cursor = cursor_mgr.getCursor(cursor_id) orelse return try errorResult(allocator, "invalid_cursor");

    if (g.getNode(node_id) == null) return try errorResult(allocator, "node not found");

    cursor.position = node_id;

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    const w: JsonWriter = .{ .s = &stream };

    try writeCursorResponse(w, cursor_id, node_id, g);

    return aw.toOwnedSlice() catch return error.OutOfMemory;
}

// -- explorer.cursor_close --

fn handleCursorClose(allocator: std.mem.Allocator, cursor_mgr: *CursorManager, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const cursor_id = getOptionalString(args, "cursor_id") orelse return try errorResult(allocator, "invalid_cursor");

    const removed = cursor_mgr.closeCursor(cursor_id);

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    const w: JsonWriter = .{ .s = &stream };

    try w.beginObject();
    try w.fieldValue("success", removed);
    try w.endObject();

    return aw.toOwnedSlice() catch return error.OutOfMemory;
}

// -- explorer.cursor_expand --

fn handleCursorExpand(allocator: std.mem.Allocator, gen: *GraphGeneration, cursor_mgr: *CursorManager, params: ?std.json.Value) HandlerError![]const u8 {
    const max_depth: u32 = 5;

    const args = getArgs(params);
    const g = &gen.graph;

    const cursor_id = getOptionalString(args, "cursor_id") orelse return try errorResult(allocator, "invalid_cursor");
    const requested_depth = getOptionalInt(args, "depth", 2);
    const effective_depth = @min(requested_depth, max_depth);
    const truncated = requested_depth > max_depth;
    const direction_str = getOptionalString(args, "direction") orelse "out";
    const direction = parseDirection(direction_str);

    var edge_types_buf: [16]EdgeType = undefined;
    const edge_types = parseEdgeTypesArray(args, &edge_types_buf);

    const cursor = cursor_mgr.getCursor(cursor_id) orelse return try errorResult(allocator, "invalid_cursor");
    const start = cursor.position;

    var visited = std.AutoHashMapUnmanaged(NodeId, void){};
    defer visited.deinit(allocator);
    try visited.put(allocator, start, {});

    const ExpandEdge = struct { from: NodeId, to: NodeId, edge_type: EdgeType, source: EdgeSource };
    const FrontierEntry = struct { id: NodeId, remaining: u32 };

    var collected_edges: std.ArrayList(ExpandEdge) = .{};
    defer collected_edges.deinit(allocator);

    var frontier: std.ArrayList(FrontierEntry) = .{};
    defer frontier.deinit(allocator);
    try frontier.append(allocator, .{ .id = start, .remaining = effective_depth });

    while (frontier.items.len > 0) {
        const current = frontier.orderedRemove(0);
        if (current.remaining == 0) continue;

        if (direction == .out or direction == .both) {
            for (g.outEdges(current.id)) |eid| {
                const e = g.edges.items[@intFromEnum(eid)];
                if (edge_types.len > 0 and !edgeTypeInSlice(e.edge_type, edge_types)) continue;
                if (g.getNode(e.target_id)) |tn| {
                    if (tn.kind == .test_def and !cursor.include_tests) continue;
                    if (tn.external != .none and !cursor.include_external_nodes) continue;
                }
                try collected_edges.append(allocator, .{ .from = e.source_id, .to = e.target_id, .edge_type = e.edge_type, .source = e.source });
                const gop = try visited.getOrPut(allocator, e.target_id);
                if (!gop.found_existing) {
                    try frontier.append(allocator, .{ .id = e.target_id, .remaining = current.remaining - 1 });
                }
            }
        }
        if (direction == .in or direction == .both) {
            for (g.inEdges(current.id)) |eid| {
                const e = g.edges.items[@intFromEnum(eid)];
                if (edge_types.len > 0 and !edgeTypeInSlice(e.edge_type, edge_types)) continue;
                if (g.getNode(e.source_id)) |sn| {
                    if (sn.kind == .test_def and !cursor.include_tests) continue;
                    if (sn.external != .none and !cursor.include_external_nodes) continue;
                }
                try collected_edges.append(allocator, .{ .from = e.source_id, .to = e.target_id, .edge_type = e.edge_type, .source = e.source });
                const gop = try visited.getOrPut(allocator, e.source_id);
                if (!gop.found_existing) {
                    try frontier.append(allocator, .{ .id = e.source_id, .remaining = current.remaining - 1 });
                }
            }
        }
    }

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    const w: JsonWriter = .{ .s = &stream };

    try w.beginObject();
    try w.fieldValue("cursor_id", cursor_id);
    try w.field("position");
    try w.nodeIdHex(start);

    try w.field("subgraph");
    try w.beginObject();

    try w.field("nodes");
    try w.beginArray();
    var it = visited.keyIterator();
    while (it.next()) |nid_ptr| {
        const nid = nid_ptr.*;
        if (g.getNode(nid)) |n| {
            try writeNodeSummary(w, n, nid, g.project_root);
        }
    }
    try w.endArray();

    try w.field("edges");
    try w.beginArray();
    for (collected_edges.items) |e| {
        try w.beginObject();
        try w.fieldNodeIdHex("from", e.from);
        try w.fieldNodeIdHex("to", e.to);
        try w.tagFieldValue("type", e.edge_type);
        try w.tagFieldValue("source", e.source);
        try w.endObject();
    }
    try w.endArray();

    try w.endObject();

    try w.fieldValue("truncated", truncated);
    try w.fieldValue("total_nodes_in_expansion", visited.count());

    try w.endObject();

    return aw.toOwnedSlice() catch return error.OutOfMemory;
}

// -- explorer.cursor_query --

fn handleCursorQuery(allocator: std.mem.Allocator, gen: *GraphGeneration, cursor_mgr: *CursorManager, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const g = &gen.graph;

    const cursor_id = getOptionalString(args, "cursor_id") orelse return try errorResult(allocator, "invalid_cursor");
    const cursor = cursor_mgr.getCursor(cursor_id) orelse return try errorResult(allocator, "invalid_cursor");

    const kind_str = getOptionalString(args, "kind");
    const query_str = getOptionalString(args, "query");
    const limit = getOptionalInt(args, "limit", 20);
    const min_complexity_raw = getOptionalInt(args, "min_complexity", 0);
    const max_depth = getOptionalInt(args, "max_depth_from_position", 5);

    // Collect nodes reachable within max_depth hops from cursor position.
    var reachable = std.AutoHashMapUnmanaged(NodeId, void){};
    defer reachable.deinit(allocator);
    collectReachable(allocator, g, cursor.position, max_depth, &reachable) catch return error.OutOfMemory;

    // Convert the reachable set into a slice for searchIn.
    const reachable_ids = try allocator.alloc(NodeId, reachable.count());
    defer allocator.free(reachable_ids);
    var ri: usize = 0;
    var rit = reachable.keyIterator();
    while (rit.next()) |k| {
        reachable_ids[ri] = k.*;
        ri += 1;
    }

    // Apply all filters over the reachable set only, bypassing the global search cap.
    const filtered = query_mod.search(allocator, g, .{
        .node_ids = reachable_ids,
        .query = query_str,
        .kind = if (kind_str) |ks| parseNodeKind(ks) else null,
        .scope = cursor.scope,
        .include_tests = cursor.include_tests,
        .external = if (cursor.include_external_nodes) .include else .exclude,
        .min_complexity = if (min_complexity_raw > 0) @as(?u16, @intCast(@min(min_complexity_raw, std.math.maxInt(u16)))) else null,
        .limit = limit,
    }) catch return error.OutOfMemory;
    defer filtered.deinit(allocator);

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    const w: JsonWriter = .{ .s = &stream };

    try w.beginObject();
    try w.field("nodes");
    try w.beginArray();
    for (filtered.nodes) |nid| {
        const n = g.getNode(nid) orelse continue;
        try writeNodeSummary(w, n, nid, g.project_root);
    }
    try w.endArray();
    try w.endObject();

    return aw.toOwnedSlice() catch return error.OutOfMemory;
}

// -- explorer.diff --

fn flattenKindIds(node: ts.Node, buf: []u16) []u16 {
    var pos: usize = 0;
    var stack: [256]ts.Node = undefined;
    var stack_len: usize = 1;
    stack[0] = node;
    while (stack_len > 0) {
        stack_len -= 1;
        const current = stack[stack_len];
        if (pos < buf.len) {
            buf[pos] = current.kindId();
            pos += 1;
        }
        // Push children in reverse order so left-to-right traversal
        var i: u32 = current.childCount();
        while (i > 0) {
            i -= 1;
            if (current.child(i)) |c| {
                if (stack_len < stack.len) {
                    stack[stack_len] = c;
                    stack_len += 1;
                }
            }
        }
    }
    return buf[0..pos];
}

fn multisetJaccard(a: []const u16, b: []const u16) f64 {
    if (a.len == 0 and b.len == 0) return 1.0;
    if (a.len == 0 or b.len == 0) return 0.0;

    // Count occurrences using a bounded approach
    var counts_a: [512]u16 = .{0} ** 512;
    var counts_b: [512]u16 = .{0} ** 512;
    for (a) |k| {
        const idx = k % 512;
        counts_a[idx] +|= 1;
    }
    for (b) |k| {
        const idx = k % 512;
        counts_b[idx] +|= 1;
    }

    var intersection: u64 = 0;
    var union_sum: u64 = 0;
    for (0..512) |i| {
        intersection += @min(counts_a[i], counts_b[i]);
        union_sum += @max(counts_a[i], counts_b[i]);
    }

    if (union_sum == 0) return 1.0;
    return @as(f64, @floatFromInt(intersection)) / @as(f64, @floatFromInt(union_sum));
}

fn computeNodeSimilarity(allocator: std.mem.Allocator, g: *const Graph, id_a: NodeId, id_b: NodeId) f64 {
    if (id_a == id_b) return 1.0;

    const node_a = g.getNode(id_a) orelse return 0.0;
    const node_b = g.getNode(id_b) orelse return 0.0;

    // Both need source to compare structurally
    const src_a = extractNodeSource(allocator, g, node_a) orelse return 0.0;
    defer allocator.free(src_a);
    const src_b = extractNodeSource(allocator, g, node_b) orelse return 0.0;
    defer allocator.free(src_b);

    if (std.mem.eql(u8, src_a, src_b)) return 1.0;

    // Determine language for parsing
    const lang_a = node_a.language orelse return 0.0;
    const lang_b = node_b.language orelse return 0.0;
    if (lang_a != lang_b) return 0.0;

    const ts_lang = switch (lang_a) {
        .zig => tree_sitter_api.tree_sitter_zig(),
        .rust => tree_sitter_api.tree_sitter_rust(),
    };

    const tree_a = tree_sitter_api.parseSource(ts_lang, src_a) orelse return 0.0;
    defer tree_a.destroy();
    const tree_b = tree_sitter_api.parseSource(ts_lang, src_b) orelse return 0.0;
    defer tree_b.destroy();

    var buf_a: [4096]u16 = undefined;
    var buf_b: [4096]u16 = undefined;
    const kinds_a = flattenKindIds(tree_a.rootNode(), &buf_a);
    const kinds_b = flattenKindIds(tree_b.rootNode(), &buf_b);

    return multisetJaccard(kinds_a, kinds_b);
}

fn extractNodeSource(allocator: std.mem.Allocator, g: *const Graph, n: *const Node) ?[]const u8 {
    if (n.external != .none) return null;
    const fp = n.file_path orelse return null;
    const ls = n.line_start orelse 1;
    const le = n.line_end orelse ls;
    return extractSource(allocator, g.project_root, fp, ls, le, 0, "full");
}

fn handleDiff(allocator: std.mem.Allocator, gen: *GraphGeneration, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const g = &gen.graph;

    const node_ids = try collectNodeIds(allocator, args, "node_ids");
    defer if (node_ids.len > 0) allocator.free(node_ids);

    if (node_ids.len < 2) return try errorResult(allocator, "need at least 2 node_ids");

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    const w: JsonWriter = .{ .s = &stream };

    try w.beginObject();

    if (node_ids.len > 2) {
        // NxN matrix
        try w.field("matrix");
        try w.beginObject();
        try w.field("node_ids");
        try w.beginArray();
        for (node_ids) |nid| {
            try w.nodeIdHex(nid);
        }
        try w.endArray();
        try w.field("similarities");
        try w.beginArray();
        for (node_ids) |nid_a| {
            try w.beginArray();
            for (node_ids) |nid_b| {
                const sim = computeNodeSimilarity(allocator, g, nid_a, nid_b);
                try w.write(sim);
            }
            try w.endArray();
        }
        try w.endArray();
        try w.endObject();
    }

    // Pairwise pairs
    try w.field("pairs");
    try w.beginArray();
    for (node_ids, 0..) |nid_a, i| {
        for (node_ids[i + 1 ..]) |nid_b| {
            const sim = computeNodeSimilarity(allocator, g, nid_a, nid_b);
            try w.beginObject();
            try w.fieldNodeIdHex("a", nid_a);
            try w.fieldNodeIdHex("b", nid_b);
            try w.fieldValue("similarity", sim);
            try w.endObject();
        }
    }
    try w.endArray();

    try w.endObject();

    return aw.toOwnedSlice() catch return error.OutOfMemory;
}

// -- explorer.annotate --

fn handleAnnotate(allocator: std.mem.Allocator, cursor_mgr: *CursorManager, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);

    const cursor_id = getOptionalString(args, "cursor_id") orelse return try errorResult(allocator, "invalid_cursor");
    const tag = getOptionalString(args, "tag") orelse return try errorResult(allocator, "missing tag");
    const note = getOptionalString(args, "note");

    const cursor = cursor_mgr.getCursor(cursor_id) orelse return try errorResult(allocator, "invalid_cursor");

    const node_ids = try collectNodeIds(allocator, args, "node_ids");
    defer if (node_ids.len > 0) allocator.free(node_ids);

    // Dupe tag and note into cursor manager arena
    const arena_alloc = cursor_mgr.arena.allocator();
    const duped_tag = arena_alloc.dupe(u8, tag) catch return error.OutOfMemory;
    const duped_note: ?[]const u8 = if (note) |n| arena_alloc.dupe(u8, n) catch return error.OutOfMemory else null;

    for (node_ids) |nid| {
        cursor.addAnnotation(cursor_mgr.arena.allocator(), nid, duped_tag, duped_note) catch return error.OutOfMemory;
    }

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    const w: JsonWriter = .{ .s = &stream };

    try w.beginObject();
    try w.fieldValue("success", true);
    try w.fieldValue("count", node_ids.len);
    try w.endObject();

    return aw.toOwnedSlice() catch return error.OutOfMemory;
}

// -- explorer.annotations --

fn handleAnnotations(allocator: std.mem.Allocator, cursor_mgr: *CursorManager, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);

    const cursor_id = getOptionalString(args, "cursor_id") orelse return try errorResult(allocator, "invalid_cursor");
    const tag_filter = getOptionalString(args, "tag");

    const cursor = cursor_mgr.getCursor(cursor_id) orelse return try errorResult(allocator, "invalid_cursor");

    const all_annotations = cursor.getAnnotations();

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    const w: JsonWriter = .{ .s = &stream };

    try w.beginObject();
    try w.field("annotations");
    try w.beginArray();

    for (all_annotations) |ann| {
        if (tag_filter) |tf| {
            if (!std.mem.eql(u8, ann.tag, tf)) continue;
        }
        try w.beginObject();
        try w.fieldNodeIdHex("node_id", ann.node_id);
        try w.fieldValue("tag", ann.tag);
        try w.fieldValue("note", ann.note);
        try w.endObject();
    }

    try w.endArray();
    try w.endObject();

    return aw.toOwnedSlice() catch return error.OutOfMemory;
}

// -- analysis.duplicates --

/// Emit total_groups + groups array into stream, optionally including source text.
fn writeDuplicateGroups(
    w: JsonWriter,
    g: *const Graph,
    allocator: std.mem.Allocator,
    groups_slice: []const duplicates_mod.DuplicateGroup,
    total_groups: u32,
    include_source: bool,
) HandlerError!void {
    try w.fieldValue("total_groups", total_groups);
    try w.field("groups");
    try w.beginArray();
    for (groups_slice) |group| {
        try w.beginObject();
        try w.fieldValue("structural_hash", group.structural_hash);
        try w.fieldValue("similarity", group.similarity);
        try w.field("members");
        try w.beginArray();
        for (group.members) |member| {
            const n = g.getNode(member.node_id);
            try w.beginObject();
            try w.fieldNodeIdHex("node_id", member.node_id);
            try w.fieldValue("name", member.name);
            try w.fieldValue("file", relativePath(member.file_path, g.project_root));
            if (include_source) {
                try w.field("source");
                if (n) |node| {
                    const src = extractNodeSource(allocator, g, node);
                    defer if (src) |s| allocator.free(s);
                    try w.write(src);
                } else {
                    try w.write(null);
                }
            }
            try w.endObject();
        }
        try w.endArray();
        try w.endObject();
    }
    try w.endArray();
}

/// Extract source, parse the AST, and build a frequency fingerprint for one node.
fn buildFuzzyCandidate(allocator: std.mem.Allocator, g: *const Graph, nid: NodeId, node: Node, structural_hash: u32) duplicates_mod.FuzzyCandidate {
    const src = extractNodeSource(allocator, g, &node) orelse
        return .{ .node_id = nid, .structural_hash = structural_hash, .fingerprint = .{0} ** 512, .valid = false };
    defer allocator.free(src);

    const lang = node.language orelse
        return .{ .node_id = nid, .structural_hash = structural_hash, .fingerprint = .{0} ** 512, .valid = false };

    const ts_lang = switch (lang) {
        .zig => tree_sitter_api.tree_sitter_zig(),
        .rust => tree_sitter_api.tree_sitter_rust(),
    };
    const tree = tree_sitter_api.parseSource(ts_lang, src) orelse
        return .{ .node_id = nid, .structural_hash = structural_hash, .fingerprint = .{0} ** 512, .valid = false };
    defer tree.destroy();

    var kind_buf: [4096]u16 = undefined;
    const kinds = flattenKindIds(tree.rootNode(), &kind_buf);

    var fp: duplicates_mod.Fingerprint = .{0} ** 512;
    for (kinds) |k| {
        fp[k % 512] +|= 1;
    }
    return .{ .node_id = nid, .structural_hash = structural_hash, .fingerprint = fp, .valid = true };
}

fn handleDuplicates(allocator: std.mem.Allocator, gen: *GraphGeneration, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const g = &gen.graph;
    const min_lines = getOptionalInt(args, "min_lines", 3);
    const threshold = getOptionalFloat(args, "threshold", 0.75);
    const scope = getOptionalString(args, "scope");
    const language_str = getOptionalString(args, "language");
    const include_source = getOptionalBool(args, "include_source", false);
    const offset = getOptionalInt(args, "offset", 0);
    const limit = getOptionalInt(args, "limit", 10);

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    const w: JsonWriter = .{ .s = &stream };

    try w.beginObject();

    if (threshold >= 1.0) {
        const result = duplicates_mod.findDuplicates(allocator, g, .{
            .min_lines = min_lines,
            .scope = scope,
            .language = if (language_str) |ls| parseLanguage(ls) else null,
            .offset = offset,
            .limit = limit,
        }) catch return error.OutOfMemory;
        defer result.deinit(allocator);
        try writeDuplicateGroups(w, g, allocator, result.groups, result.total_groups, include_source);
    } else {
        // Fuzzy mode: build fingerprinted candidates, delegate clustering to analyzer.
        const lang_filter = if (language_str) |ls| parseLanguage(ls) else null;
        const scope_str = scope;
        const fuzzy_candidate_cap: usize = 300;

        var fuzzy_candidates = std.ArrayList(duplicates_mod.FuzzyCandidate){};
        defer fuzzy_candidates.deinit(allocator);

        for (g.nodes.items, 0..) |n, i| {
            if (fuzzy_candidates.items.len >= fuzzy_candidate_cap) break;
            if (n.kind != .function) continue;
            if (n.external != .none) continue;
            if (lang_filter) |lf| {
                if (n.language == null or n.language.? != lf) continue;
            }
            if (scope_str) |sc| {
                const fp = n.file_path orelse continue;
                if (!std.mem.startsWith(u8, fp, sc)) continue;
            }
            const m = n.metrics orelse continue;
            if (m.lines < min_lines) continue;

            const nid: NodeId = @enumFromInt(i);
            try fuzzy_candidates.append(allocator, buildFuzzyCandidate(allocator, g, nid, n, m.structural_hash));
        }

        const result = duplicates_mod.findFuzzyDuplicates(allocator, g, fuzzy_candidates.items, .{
            .threshold = threshold,
            .offset = offset,
            .limit = limit,
        }) catch return error.OutOfMemory;
        defer result.deinit(allocator);
        try writeDuplicateGroups(w, g, allocator, result.groups, result.total_groups, include_source);
    }

    try w.endObject();

    return aw.toOwnedSlice() catch return error.OutOfMemory;
}

// -- analysis.complexity --

fn handleComplexity(allocator: std.mem.Allocator, gen: *GraphGeneration, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const g = &gen.graph;
    const top_n = getOptionalInt(args, "top_n", 10);
    const scope = getOptionalString(args, "scope");
    const kind_str = getOptionalString(args, "kind") orelse "function";
    const language_str = getOptionalString(args, "language");

    const kind: NodeKind = if (std.mem.eql(u8, kind_str, "file")) .file else .function;

    const result = complexity_mod.findComplex(allocator, g, .{
        .top_n = top_n,
        .scope = scope,
        .kind = kind,
        .language = if (language_str) |ls| parseLanguage(ls) else null,
    }) catch return error.OutOfMemory;
    defer result.deinit(allocator);

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    const w: JsonWriter = .{ .s = &stream };

    try w.beginObject();
    try w.field("nodes");
    try w.beginArray();
    for (result.nodes) |entry| {
        try w.beginObject();
        try w.fieldNodeIdHex("node_id", entry.node_id);
        try w.fieldValue("name", entry.name);
        try w.fieldValue("file", relativePath(entry.file_path, g.project_root));
        try w.fieldValue("complexity", entry.complexity);
        try w.endObject();
    }
    try w.endArray();
    try w.endObject();

    return aw.toOwnedSlice() catch return error.OutOfMemory;
}

// -- analysis.dead_code --

fn handleDeadCode(allocator: std.mem.Allocator, gen: *GraphGeneration, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const g = &gen.graph;
    const include_public = getOptionalBool(args, "include_public", false);
    const include_test_only = getOptionalBool(args, "include_test_only", false);
    const scope = getOptionalString(args, "scope");
    const kind_str = getOptionalString(args, "kind") orelse "all";
    const language_str = getOptionalString(args, "language");
    const offset = getOptionalInt(args, "offset", 0);
    const limit = getOptionalInt(args, "limit", 50);

    const kind: ?NodeKind = if (std.mem.eql(u8, kind_str, "all")) null else parseNodeKind(kind_str);

    const result = dead_code_mod.findDeadCode(allocator, g, .{
        .include_public = include_public,
        .include_test_only = include_test_only,
        .scope = scope,
        .kind = kind,
        .language = if (language_str) |ls| parseLanguage(ls) else null,
        .offset = offset,
        .limit = limit,
    }) catch return error.OutOfMemory;
    defer result.deinit(allocator);

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    const w: JsonWriter = .{ .s = &stream };

    try w.beginObject();
    try w.fieldValue("total_count", result.total_count);
    try w.field("nodes");
    try w.beginArray();
    for (result.nodes) |entry| {
        try w.beginObject();
        try w.fieldNodeIdHex("node_id", entry.node_id);
        try w.fieldValue("name", entry.name);
        try w.tagFieldValue("kind", entry.kind);
        try w.fieldValue("file", relativePath(entry.file_path, g.project_root));
        try w.tagFieldValue("visibility", entry.visibility);
        try w.endObject();
    }
    try w.endArray();
    try w.endObject();

    return aw.toOwnedSlice() catch return error.OutOfMemory;
}

// -- analysis.dependency_cycles --

fn handleDependencyCycles(allocator: std.mem.Allocator, gen: *GraphGeneration, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const g = &gen.graph;
    const max_cycle_length = getOptionalInt(args, "max_cycle_length", 20);
    const scope = getOptionalString(args, "scope");
    const language_str = getOptionalString(args, "language");

    var et_buf: [16]EdgeType = undefined;
    const parsed_et = parseEdgeTypesArray(args, &et_buf);

    const result = cycles_mod.findCycles(allocator, g, .{
        .max_cycle_length = max_cycle_length,
        .edge_types = if (parsed_et.len > 0) parsed_et else null,
        .scope = scope,
        .language = if (language_str) |ls| parseLanguage(ls) else null,
    }) catch return error.OutOfMemory;
    defer result.deinit(allocator);

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    const w: JsonWriter = .{ .s = &stream };

    try w.beginObject();
    try w.field("cycles");
    try w.beginArray();
    for (result.cycles) |cycle| {
        try w.beginObject();
        try w.field("nodes");
        try w.beginArray();
        for (cycle.nodes) |cn| {
            try w.beginObject();
            try w.fieldNodeIdHex("node_id", cn.node_id);
            try w.fieldValue("name", cn.name);
            try w.fieldValue("file", relativePath(cn.file_path, g.project_root));
            try w.endObject();
        }
        try w.endArray();
        try w.endObject();
    }
    try w.endArray();
    try w.endObject();

    return aw.toOwnedSlice() catch return error.OutOfMemory;
}

// -- analysis.coupling --

fn handleCoupling(allocator: std.mem.Allocator, gen: *GraphGeneration, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const g = &gen.graph;
    const top_n = getOptionalInt(args, "top_n", 10);
    const min_coupling = getOptionalFloat(args, "min_coupling", 0.3);
    const scope = getOptionalString(args, "scope");
    const granularity_str = getOptionalString(args, "granularity") orelse "file";
    const external_str = getOptionalString(args, "external") orelse "exclude";
    const language_str = getOptionalString(args, "language");

    const granularity: coupling_mod.Granularity = if (std.mem.eql(u8, granularity_str, "file")) .file else .directory;
    const include_external = std.mem.eql(u8, external_str, "include");

    const result = coupling_mod.findCoupling(allocator, g, .{
        .min_coupling = min_coupling,
        .top_n = top_n,
        .scope = scope,
        .granularity = granularity,
        .include_external = include_external,
        .language = if (language_str) |ls| parseLanguage(ls) else null,
    }) catch return error.OutOfMemory;
    defer result.deinit(allocator);

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    const w: JsonWriter = .{ .s = &stream };

    try w.beginObject();
    try w.field("pairs");
    try w.beginArray();
    for (result.pairs) |pair| {
        try w.beginObject();
        try w.fieldValue("module_a", relativePath(pair.module_a, g.project_root));
        try w.fieldValue("module_b", relativePath(pair.module_b, g.project_root));
        try w.fieldValue("shared_edges", pair.shared_edges);
        try w.fieldValue("score", pair.score);
        try w.endObject();
    }
    try w.endArray();
    try w.endObject();

    return aw.toOwnedSlice() catch return error.OutOfMemory;
}

// -- analysis.impact --

fn handleImpact(allocator: std.mem.Allocator, gen: *GraphGeneration, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const g = &gen.graph;
    const max_depth = getOptionalInt(args, "max_depth", 10);
    const include_parent_chain = getOptionalBool(args, "include_parent_chain", true);

    var et_buf: [16]EdgeType = undefined;
    const parsed_et = parseEdgeTypesArray(args, &et_buf);

    const node_ids = try collectNodeIds(allocator, args, "node_ids");
    defer if (node_ids.len > 0) allocator.free(node_ids);

    if (node_ids.len == 0) {
        return try errorResult(allocator, "node_ids is required");
    }

    const result = impact_mod.analyzeImpact(allocator, g, node_ids, .{
        .max_depth = max_depth,
        .edge_types = if (parsed_et.len > 0) parsed_et else null,
        .include_parent_chain = include_parent_chain,
    }) catch return error.OutOfMemory;
    defer result.deinit(allocator);

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    const w: JsonWriter = .{ .s = &stream };

    try w.beginObject();

    // Emit source context so clients know what the analysis started from.
    try w.field("source_nodes");
    try w.beginArray();
    for (node_ids) |nid| {
        try w.nodeIdHex(nid);
    }
    try w.endArray();

    if (node_ids.len == 1) {
        if (g.getNode(node_ids[0])) |src_node| {
            try w.fieldValue("source_name", src_node.name);
            const ext_val: ?[]const u8 = switch (src_node.external) {
                .none => null,
                .stdlib => "stdlib",
                .dependency => "dependency",
            };
            try w.fieldValue("source_external", ext_val);
        }
    }

    try w.fieldValue("total_impacted", result.total_impacted);
    try w.field("impacted");
    try w.beginArray();
    for (result.dependents) |dep| {
        try w.beginObject();
        try w.fieldNodeIdHex("node_id", dep.node_id);
        try w.fieldValue("name", dep.name);
        try w.tagFieldValue("kind", dep.kind);
        try w.fieldValue("file", relativePath(dep.file_path, g.project_root));
        try w.endObject();
    }
    try w.endArray();
    try w.endObject();

    return aw.toOwnedSlice() catch return error.OutOfMemory;
}

// -- Error result helper --

fn errorResult(allocator: std.mem.Allocator, message: []const u8) HandlerError![]const u8 {
    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    const w: JsonWriter = .{ .s = &stream };
    try w.beginObject();
    try w.fieldValue("error", message);
    try w.endObject();
    return aw.toOwnedSlice() catch return error.OutOfMemory;
}
