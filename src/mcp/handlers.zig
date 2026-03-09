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

const Graph = graph_mod.Graph;
const Direction = graph_mod.Direction;
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
const ExternalInfo = lang_mod.ExternalInfo;
const LangMeta = lang_mod.LangMeta;

/// Errors returned by MCP tool handlers.
pub const HandlerError = error{OutOfMemory};

// -- Param helpers --

fn parseNodeId(hex_str: []const u8) ?NodeId {
    const val = std.fmt.parseInt(u64, hex_str, 16) catch return null;
    return @enumFromInt(val);
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
    inline for (@typeInfo(EdgeType).@"enum".fields) |f| {
        if (std.mem.eql(u8, name, f.name)) return @enumFromInt(f.value);
    }
    return null;
}

fn parseNodeKind(name: []const u8) ?NodeKind {
    inline for (@typeInfo(NodeKind).@"enum".fields) |f| {
        if (std.mem.eql(u8, name, f.name)) return @enumFromInt(f.value);
    }
    return null;
}

fn parseLanguage(name: []const u8) ?Language {
    inline for (@typeInfo(Language).@"enum".fields) |f| {
        if (std.mem.eql(u8, name, f.name)) return @enumFromInt(f.value);
    }
    return null;
}

fn parseVisibility(name: []const u8) ?Visibility {
    inline for (@typeInfo(Visibility).@"enum".fields) |f| {
        if (std.mem.eql(u8, name, f.name)) return @enumFromInt(f.value);
    }
    return null;
}

fn parseDirection(name: []const u8) Direction {
    if (std.mem.eql(u8, name, "in")) return .in;
    if (std.mem.eql(u8, name, "out")) return .out;
    return .both;
}

// -- JSON writing helpers --

fn writeNodeIdHex(stream: *std.json.Stringify, id: NodeId) HandlerError!void {
    var buf: [20]u8 = undefined;
    const s = std.fmt.bufPrint(&buf, "{x}", .{@intFromEnum(id)}) catch return error.OutOfMemory;
    stream.write(s) catch return error.OutOfMemory;
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

fn writeNodeSummary(stream: *std.json.Stringify, n: *const Node, id: NodeId, project_root: []const u8) HandlerError!void {
    stream.beginObject() catch return error.OutOfMemory;

    stream.objectField("id") catch return error.OutOfMemory;
    try writeNodeIdHex(stream, id);

    stream.objectField("name") catch return error.OutOfMemory;
    stream.write(n.name) catch return error.OutOfMemory;

    stream.objectField("kind") catch return error.OutOfMemory;
    stream.write(@tagName(n.kind)) catch return error.OutOfMemory;

    stream.objectField("language") catch return error.OutOfMemory;
    if (n.language) |l| {
        stream.write(@tagName(l)) catch return error.OutOfMemory;
    } else {
        stream.write(null) catch return error.OutOfMemory;
    }

    stream.objectField("file") catch return error.OutOfMemory;
    stream.write(relativePath(n.file_path, project_root)) catch return error.OutOfMemory;

    stream.objectField("line_start") catch return error.OutOfMemory;
    if (n.line_start) |ls| {
        stream.write(ls) catch return error.OutOfMemory;
    } else {
        stream.write(null) catch return error.OutOfMemory;
    }

    stream.objectField("line_end") catch return error.OutOfMemory;
    if (n.line_end) |le| {
        stream.write(le) catch return error.OutOfMemory;
    } else {
        stream.write(null) catch return error.OutOfMemory;
    }

    stream.objectField("visibility") catch return error.OutOfMemory;
    stream.write(@tagName(n.visibility)) catch return error.OutOfMemory;

    stream.objectField("external") catch return error.OutOfMemory;
    switch (n.external) {
        .none => stream.write(null) catch return error.OutOfMemory,
        .stdlib => stream.write("stdlib") catch return error.OutOfMemory,
        .dependency => |d| {
            if (d.version) |v| {
                stream.write(v) catch return error.OutOfMemory;
            } else {
                stream.write("dependency") catch return error.OutOfMemory;
            }
        },
    }

    stream.objectField("signature") catch return error.OutOfMemory;
    stream.write(n.signature) catch return error.OutOfMemory;

    stream.objectField("metrics") catch return error.OutOfMemory;
    if (n.metrics) |m| {
        m.writeJson(stream) catch return error.OutOfMemory;
    } else {
        stream.write(null) catch return error.OutOfMemory;
    }

    stream.endObject() catch return error.OutOfMemory;
}

fn writeFullNode(stream: *std.json.Stringify, n: *const Node, id: NodeId, project_root: []const u8, source_text: ?[]const u8) HandlerError!void {
    stream.beginObject() catch return error.OutOfMemory;

    stream.objectField("id") catch return error.OutOfMemory;
    try writeNodeIdHex(stream, id);

    stream.objectField("name") catch return error.OutOfMemory;
    stream.write(n.name) catch return error.OutOfMemory;

    stream.objectField("kind") catch return error.OutOfMemory;
    stream.write(@tagName(n.kind)) catch return error.OutOfMemory;

    stream.objectField("language") catch return error.OutOfMemory;
    if (n.language) |l| {
        stream.write(@tagName(l)) catch return error.OutOfMemory;
    } else {
        stream.write(null) catch return error.OutOfMemory;
    }

    stream.objectField("file") catch return error.OutOfMemory;
    stream.write(relativePath(n.file_path, project_root)) catch return error.OutOfMemory;

    stream.objectField("line_start") catch return error.OutOfMemory;
    if (n.line_start) |ls| {
        stream.write(ls) catch return error.OutOfMemory;
    } else {
        stream.write(null) catch return error.OutOfMemory;
    }

    stream.objectField("line_end") catch return error.OutOfMemory;
    if (n.line_end) |le| {
        stream.write(le) catch return error.OutOfMemory;
    } else {
        stream.write(null) catch return error.OutOfMemory;
    }

    stream.objectField("visibility") catch return error.OutOfMemory;
    stream.write(@tagName(n.visibility)) catch return error.OutOfMemory;

    stream.objectField("parent_id") catch return error.OutOfMemory;
    if (n.parent_id) |pid| {
        try writeNodeIdHex(stream, pid);
    } else {
        stream.write(null) catch return error.OutOfMemory;
    }

    stream.objectField("doc") catch return error.OutOfMemory;
    stream.write(n.doc) catch return error.OutOfMemory;

    stream.objectField("signature") catch return error.OutOfMemory;
    stream.write(n.signature) catch return error.OutOfMemory;

    stream.objectField("external") catch return error.OutOfMemory;
    switch (n.external) {
        .none => stream.write(null) catch return error.OutOfMemory,
        .stdlib => stream.write("stdlib") catch return error.OutOfMemory,
        .dependency => |d| {
            if (d.version) |v| {
                stream.write(v) catch return error.OutOfMemory;
            } else {
                stream.write("dependency") catch return error.OutOfMemory;
            }
        },
    }

    stream.objectField("content_hash") catch return error.OutOfMemory;
    if (n.content_hash) |ch| {
        var hex_buf: [24]u8 = undefined;
        for (ch, 0..) |byte, bi| {
            _ = std.fmt.bufPrint(hex_buf[bi * 2 ..][0..2], "{x:0>2}", .{byte}) catch unreachable;
        }
        stream.write(@as([]const u8, &hex_buf)) catch return error.OutOfMemory;
    } else {
        stream.write(null) catch return error.OutOfMemory;
    }

    stream.objectField("metrics") catch return error.OutOfMemory;
    if (n.metrics) |m| {
        m.writeJson(stream) catch return error.OutOfMemory;
    } else {
        stream.write(null) catch return error.OutOfMemory;
    }

    stream.objectField("lang_meta") catch return error.OutOfMemory;
    n.lang_meta.writeJson(stream) catch return error.OutOfMemory;

    if (source_text) |src| {
        stream.objectField("source") catch return error.OutOfMemory;
        stream.write(src) catch return error.OutOfMemory;
    }

    stream.endObject() catch return error.OutOfMemory;
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
/// Caller owns the returned slice.
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

    stream.beginObject() catch return error.OutOfMemory;

    stream.objectField("project_root") catch return error.OutOfMemory;
    stream.write(g.project_root) catch return error.OutOfMemory;

    // Collect languages from node scan
    stream.objectField("languages") catch return error.OutOfMemory;
    stream.beginArray() catch return error.OutOfMemory;
    var has_zig = false;
    var has_rust = false;
    for (g.nodes.items) |n| {
        if (n.language) |l| {
            switch (l) {
                .zig => {
                    if (!has_zig) {
                        has_zig = true;
                        stream.write("zig") catch return error.OutOfMemory;
                    }
                },
                .rust => {
                    if (!has_rust) {
                        has_rust = true;
                        stream.write("rust") catch return error.OutOfMemory;
                    }
                },
            }
        }
    }
    stream.endArray() catch return error.OutOfMemory;

    stream.objectField("total_files") catch return error.OutOfMemory;
    stream.write(stats.node_counts[@intFromEnum(NodeKind.file)]) catch return error.OutOfMemory;

    stream.objectField("total_lines") catch return error.OutOfMemory;
    stream.write(stats.total_lines) catch return error.OutOfMemory;

    stream.objectField("source_hash") catch return error.OutOfMemory;
    var hash_hex: [24]u8 = undefined;
    for (gen.source_hash, 0..) |byte, bi| {
        _ = std.fmt.bufPrint(hash_hex[bi * 2 ..][0..2], "{x:0>2}", .{byte}) catch unreachable;
    }
    stream.write(@as([]const u8, &hash_hex)) catch return error.OutOfMemory;

    // Node counts by kind
    stream.objectField("nodes") catch return error.OutOfMemory;
    stream.beginObject() catch return error.OutOfMemory;
    inline for (@typeInfo(NodeKind).@"enum".fields) |f| {
        const count = stats.node_counts[f.value];
        if (count > 0) {
            stream.objectField(f.name) catch return error.OutOfMemory;
            stream.write(count) catch return error.OutOfMemory;
        }
    }
    stream.endObject() catch return error.OutOfMemory;

    // Edge counts by type
    stream.objectField("edges") catch return error.OutOfMemory;
    stream.beginObject() catch return error.OutOfMemory;
    inline for (@typeInfo(EdgeType).@"enum".fields) |f| {
        const count = stats.edge_counts[f.value];
        if (count > 0) {
            stream.objectField(f.name) catch return error.OutOfMemory;
            stream.write(count) catch return error.OutOfMemory;
        }
    }
    stream.endObject() catch return error.OutOfMemory;

    // External node counts
    stream.objectField("externals") catch return error.OutOfMemory;
    stream.beginObject() catch return error.OutOfMemory;
    var stdlib_count: u32 = 0;
    var dep_count: u32 = 0;
    for (g.nodes.items) |n| {
        switch (n.external) {
            .none => {},
            .stdlib => stdlib_count += 1,
            .dependency => dep_count += 1,
        }
    }
    stream.objectField("stdlib_symbols") catch return error.OutOfMemory;
    stream.write(stdlib_count) catch return error.OutOfMemory;
    stream.objectField("dependency_symbols") catch return error.OutOfMemory;
    stream.write(dep_count) catch return error.OutOfMemory;
    stream.endObject() catch return error.OutOfMemory;

    stream.endObject() catch return error.OutOfMemory;

    return wrapToolResult(allocator, &aw);
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

    stream.beginObject() catch return error.OutOfMemory;

    stream.objectField("total_matches") catch return error.OutOfMemory;
    stream.write(result.total_matches) catch return error.OutOfMemory;

    stream.objectField("nodes") catch return error.OutOfMemory;
    stream.beginArray() catch return error.OutOfMemory;
    for (result.nodes) |nid| {
        const n = g.getNode(nid) orelse continue;
        try writeNodeSummary(&stream, n, nid, g.project_root);
    }
    stream.endArray() catch return error.OutOfMemory;

    stream.endObject() catch return error.OutOfMemory;

    return wrapToolResult(allocator, &aw);
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

    stream.beginObject() catch return error.OutOfMemory;
    stream.objectField("nodes") catch return error.OutOfMemory;
    stream.beginArray() catch return error.OutOfMemory;

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

        try writeFullNode(&stream, n, detail.id, g.project_root, if (include_source) source_text orelse @as(?[]const u8, null) else null);
    }

    stream.endArray() catch return error.OutOfMemory;
    stream.endObject() catch return error.OutOfMemory;

    return wrapToolResult(allocator, &aw);
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

    stream.beginObject() catch return error.OutOfMemory;
    stream.objectField("sources") catch return error.OutOfMemory;
    stream.beginArray() catch return error.OutOfMemory;

    for (node_ids) |nid| {
        const n = g.getNode(nid) orelse continue;

        stream.beginObject() catch return error.OutOfMemory;
        stream.objectField("id") catch return error.OutOfMemory;
        try writeNodeIdHex(&stream, nid);

        stream.objectField("source") catch return error.OutOfMemory;
        if (n.external != .none) {
            stream.write(null) catch return error.OutOfMemory;
        } else if (n.file_path) |fp| {
            const ls = n.line_start orelse 1;
            const le = n.line_end orelse ls;
            const src = extractSource(allocator, g.project_root, fp, ls, le, context_lines, part);
            defer if (src) |s| allocator.free(s);
            if (src) |s| {
                stream.write(s) catch return error.OutOfMemory;
            } else {
                stream.write(null) catch return error.OutOfMemory;
            }
        } else {
            stream.write(null) catch return error.OutOfMemory;
        }
        stream.endObject() catch return error.OutOfMemory;
    }

    stream.endArray() catch return error.OutOfMemory;
    stream.endObject() catch return error.OutOfMemory;

    return wrapToolResult(allocator, &aw);
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

    stream.beginObject() catch return error.OutOfMemory;
    stream.objectField("total_count") catch return error.OutOfMemory;
    stream.write(result.total_count) catch return error.OutOfMemory;

    stream.objectField("edges") catch return error.OutOfMemory;
    stream.beginArray() catch return error.OutOfMemory;

    for (result.edges) |e| {
        stream.beginObject() catch return error.OutOfMemory;

        stream.objectField("from") catch return error.OutOfMemory;
        try writeNodeIdHex(&stream, e.source_id);

        stream.objectField("to") catch return error.OutOfMemory;
        try writeNodeIdHex(&stream, e.target_id);

        stream.objectField("type") catch return error.OutOfMemory;
        stream.write(@tagName(e.edge_type)) catch return error.OutOfMemory;

        stream.objectField("source") catch return error.OutOfMemory;
        stream.write(@tagName(e.source)) catch return error.OutOfMemory;

        // Connected node info
        if (g.getNode(e.target_id)) |tn| {
            stream.objectField("to_node") catch return error.OutOfMemory;
            stream.beginObject() catch return error.OutOfMemory;
            stream.objectField("id") catch return error.OutOfMemory;
            try writeNodeIdHex(&stream, e.target_id);
            stream.objectField("name") catch return error.OutOfMemory;
            stream.write(tn.name) catch return error.OutOfMemory;
            stream.objectField("kind") catch return error.OutOfMemory;
            stream.write(@tagName(tn.kind)) catch return error.OutOfMemory;
            stream.endObject() catch return error.OutOfMemory;
        }
        if (g.getNode(e.source_id)) |sn| {
            stream.objectField("from_node") catch return error.OutOfMemory;
            stream.beginObject() catch return error.OutOfMemory;
            stream.objectField("id") catch return error.OutOfMemory;
            try writeNodeIdHex(&stream, e.source_id);
            stream.objectField("name") catch return error.OutOfMemory;
            stream.write(sn.name) catch return error.OutOfMemory;
            stream.objectField("kind") catch return error.OutOfMemory;
            stream.write(@tagName(sn.kind)) catch return error.OutOfMemory;
            stream.endObject() catch return error.OutOfMemory;
        }

        stream.endObject() catch return error.OutOfMemory;
    }

    stream.endArray() catch return error.OutOfMemory;
    stream.endObject() catch return error.OutOfMemory;

    return wrapToolResult(allocator, &aw);
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

    stream.beginObject() catch return error.OutOfMemory;
    stream.objectField("paths") catch return error.OutOfMemory;
    stream.beginArray() catch return error.OutOfMemory;

    for (result.paths) |path| {
        stream.beginObject() catch return error.OutOfMemory;
        stream.objectField("length") catch return error.OutOfMemory;
        stream.write(path.edge_types.len) catch return error.OutOfMemory;

        stream.objectField("nodes") catch return error.OutOfMemory;
        stream.beginArray() catch return error.OutOfMemory;
        for (path.node_ids) |nid| {
            try writeNodeIdHex(&stream, nid);
        }
        stream.endArray() catch return error.OutOfMemory;

        stream.objectField("edges") catch return error.OutOfMemory;
        stream.beginArray() catch return error.OutOfMemory;
        for (path.edge_types, 0..) |et, idx| {
            stream.beginObject() catch return error.OutOfMemory;
            stream.objectField("from") catch return error.OutOfMemory;
            try writeNodeIdHex(&stream, path.node_ids[idx]);
            stream.objectField("to") catch return error.OutOfMemory;
            try writeNodeIdHex(&stream, path.node_ids[idx + 1]);
            stream.objectField("type") catch return error.OutOfMemory;
            stream.write(@tagName(et)) catch return error.OutOfMemory;
            stream.endObject() catch return error.OutOfMemory;
        }
        stream.endArray() catch return error.OutOfMemory;

        stream.endObject() catch return error.OutOfMemory;
    }

    stream.endArray() catch return error.OutOfMemory;
    stream.endObject() catch return error.OutOfMemory;

    return wrapToolResult(allocator, &aw);
}

fn emptyPathsResult(allocator: std.mem.Allocator) HandlerError![]const u8 {
    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    stream.beginObject() catch return error.OutOfMemory;
    stream.objectField("paths") catch return error.OutOfMemory;
    stream.beginArray() catch return error.OutOfMemory;
    stream.endArray() catch return error.OutOfMemory;
    stream.endObject() catch return error.OutOfMemory;
    return wrapToolResult(allocator, &aw);
}

// -- explorer.cursor_create --

fn writeCursorResponse(stream: *std.json.Stringify, cursor_id: []const u8, position: NodeId, g: *const Graph) HandlerError!void {
    stream.beginObject() catch return error.OutOfMemory;

    stream.objectField("cursor_id") catch return error.OutOfMemory;
    stream.write(cursor_id) catch return error.OutOfMemory;

    stream.objectField("position") catch return error.OutOfMemory;
    if (g.getNode(position)) |n| {
        stream.beginObject() catch return error.OutOfMemory;
        stream.objectField("id") catch return error.OutOfMemory;
        try writeNodeIdHex(stream, position);
        stream.objectField("kind") catch return error.OutOfMemory;
        stream.write(@tagName(n.kind)) catch return error.OutOfMemory;
        stream.objectField("name") catch return error.OutOfMemory;
        stream.write(n.name) catch return error.OutOfMemory;
        stream.endObject() catch return error.OutOfMemory;
    } else {
        stream.beginObject() catch return error.OutOfMemory;
        stream.objectField("id") catch return error.OutOfMemory;
        try writeNodeIdHex(stream, position);
        stream.objectField("kind") catch return error.OutOfMemory;
        stream.write("root") catch return error.OutOfMemory;
        stream.objectField("name") catch return error.OutOfMemory;
        stream.write("root") catch return error.OutOfMemory;
        stream.endObject() catch return error.OutOfMemory;
    }

    stream.objectField("neighborhood") catch return error.OutOfMemory;
    stream.beginObject() catch return error.OutOfMemory;
    stream.objectField("children") catch return error.OutOfMemory;
    stream.beginArray() catch return error.OutOfMemory;
    const children = g.getChildren(position);
    for (children) |child_id| {
        if (g.getNode(child_id)) |cn| {
            stream.beginObject() catch return error.OutOfMemory;
            stream.objectField("id") catch return error.OutOfMemory;
            try writeNodeIdHex(stream, child_id);
            stream.objectField("kind") catch return error.OutOfMemory;
            stream.write(@tagName(cn.kind)) catch return error.OutOfMemory;
            stream.objectField("name") catch return error.OutOfMemory;
            stream.write(cn.name) catch return error.OutOfMemory;
            stream.endObject() catch return error.OutOfMemory;
        }
    }
    stream.endArray() catch return error.OutOfMemory;
    stream.objectField("stats") catch return error.OutOfMemory;
    stream.beginObject() catch return error.OutOfMemory;
    stream.objectField("visible_nodes") catch return error.OutOfMemory;
    stream.write(g.nodes.items.len) catch return error.OutOfMemory;
    stream.objectField("visible_edges") catch return error.OutOfMemory;
    stream.write(g.edges.items.len) catch return error.OutOfMemory;
    stream.endObject() catch return error.OutOfMemory;
    stream.endObject() catch return error.OutOfMemory;

    stream.objectField("expires_in_seconds") catch return error.OutOfMemory;
    stream.write(@as(u32, 600)) catch return error.OutOfMemory;

    stream.endObject() catch return error.OutOfMemory;
}

fn handleCursorCreate(allocator: std.mem.Allocator, gen: *GraphGeneration, cursor_mgr: *CursorManager, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const g = &gen.graph;

    const start_str = getOptionalString(args, "start_node_id");
    const position: NodeId = if (start_str) |s| parseNodeId(s) orelse .root else .root;

    const cursor_id = cursor_mgr.createCursor(position) catch return error.OutOfMemory;

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };

    try writeCursorResponse(&stream, cursor_id, position, g);

    return wrapToolResult(allocator, &aw);
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

    try writeCursorResponse(&stream, cursor_id, node_id, g);

    return wrapToolResult(allocator, &aw);
}

// -- explorer.cursor_close --

fn handleCursorClose(allocator: std.mem.Allocator, cursor_mgr: *CursorManager, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const cursor_id = getOptionalString(args, "cursor_id") orelse return try errorResult(allocator, "invalid_cursor");

    const removed = cursor_mgr.closeCursor(cursor_id);

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };

    stream.beginObject() catch return error.OutOfMemory;
    stream.objectField("success") catch return error.OutOfMemory;
    stream.write(removed) catch return error.OutOfMemory;
    stream.endObject() catch return error.OutOfMemory;

    return wrapToolResult(allocator, &aw);
}

// -- explorer.cursor_expand --

fn handleCursorExpand(allocator: std.mem.Allocator, gen: *GraphGeneration, cursor_mgr: *CursorManager, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const g = &gen.graph;

    const cursor_id = getOptionalString(args, "cursor_id") orelse return try errorResult(allocator, "invalid_cursor");
    const depth = getOptionalInt(args, "depth", 2);
    const direction_str = getOptionalString(args, "direction") orelse "out";
    const direction = parseDirection(direction_str);

    const cursor = cursor_mgr.getCursor(cursor_id) orelse return try errorResult(allocator, "invalid_cursor");
    const start = cursor.position;

    // BFS expansion from start position
    var visited = std.AutoHashMapUnmanaged(NodeId, void){};
    defer visited.deinit(allocator);
    try visited.put(allocator, start, {});

    const ExpandEdge = struct { from: NodeId, to: NodeId, edge_type: EdgeType, source: EdgeSource };
    const FrontierEntry = struct { id: NodeId, remaining: u32 };

    // Collect edges in the expansion
    var collected_edges: std.ArrayList(ExpandEdge) = .{};
    defer collected_edges.deinit(allocator);

    // BFS frontier: store node + remaining depth
    var frontier: std.ArrayList(FrontierEntry) = .{};
    defer frontier.deinit(allocator);
    try frontier.append(allocator, .{ .id = start, .remaining = depth });

    while (frontier.items.len > 0) {
        const current = frontier.orderedRemove(0);
        if (current.remaining == 0) continue;

        // Collect edges based on direction
        if (direction == .out or direction == .both) {
            for (g.outEdges(current.id)) |eid| {
                const idx = @intFromEnum(eid);
                if (idx >= g.edges.items.len) continue;
                const e = g.edges.items[idx];
                try collected_edges.append(allocator, .{ .from = e.source_id, .to = e.target_id, .edge_type = e.edge_type, .source = e.source });
                const gop = try visited.getOrPut(allocator, e.target_id);
                if (!gop.found_existing) {
                    try frontier.append(allocator, .{ .id = e.target_id, .remaining = current.remaining - 1 });
                }
            }
        }
        if (direction == .in or direction == .both) {
            for (g.inEdges(current.id)) |eid| {
                const idx = @intFromEnum(eid);
                if (idx >= g.edges.items.len) continue;
                const e = g.edges.items[idx];
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

    stream.beginObject() catch return error.OutOfMemory;

    stream.objectField("cursor_id") catch return error.OutOfMemory;
    stream.write(cursor_id) catch return error.OutOfMemory;

    stream.objectField("position") catch return error.OutOfMemory;
    try writeNodeIdHex(&stream, start);

    stream.objectField("subgraph") catch return error.OutOfMemory;
    stream.beginObject() catch return error.OutOfMemory;

    stream.objectField("nodes") catch return error.OutOfMemory;
    stream.beginArray() catch return error.OutOfMemory;
    var it = visited.keyIterator();
    while (it.next()) |nid_ptr| {
        const nid = nid_ptr.*;
        if (g.getNode(nid)) |n| {
            try writeNodeSummary(&stream, n, nid, g.project_root);
        }
    }
    stream.endArray() catch return error.OutOfMemory;

    stream.objectField("edges") catch return error.OutOfMemory;
    stream.beginArray() catch return error.OutOfMemory;
    for (collected_edges.items) |e| {
        stream.beginObject() catch return error.OutOfMemory;
        stream.objectField("from") catch return error.OutOfMemory;
        try writeNodeIdHex(&stream, e.from);
        stream.objectField("to") catch return error.OutOfMemory;
        try writeNodeIdHex(&stream, e.to);
        stream.objectField("type") catch return error.OutOfMemory;
        stream.write(@tagName(e.edge_type)) catch return error.OutOfMemory;
        stream.objectField("source") catch return error.OutOfMemory;
        stream.write(@tagName(e.source)) catch return error.OutOfMemory;
        stream.endObject() catch return error.OutOfMemory;
    }
    stream.endArray() catch return error.OutOfMemory;

    stream.endObject() catch return error.OutOfMemory;

    stream.objectField("truncated") catch return error.OutOfMemory;
    stream.write(false) catch return error.OutOfMemory;

    stream.objectField("total_nodes_in_expansion") catch return error.OutOfMemory;
    stream.write(visited.count()) catch return error.OutOfMemory;

    stream.endObject() catch return error.OutOfMemory;

    return wrapToolResult(allocator, &aw);
}

// -- explorer.cursor_query --

fn handleCursorQuery(allocator: std.mem.Allocator, gen: *GraphGeneration, cursor_mgr: *CursorManager, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const g = &gen.graph;

    const cursor_id = getOptionalString(args, "cursor_id") orelse return try errorResult(allocator, "invalid_cursor");
    _ = cursor_mgr.getCursor(cursor_id) orelse return try errorResult(allocator, "invalid_cursor");

    const kind_str = getOptionalString(args, "kind");
    const query_str = getOptionalString(args, "query");
    const limit = getOptionalInt(args, "limit", 20);

    const result = query_mod.search(allocator, g, .{
        .query = query_str,
        .kind = if (kind_str) |ks| parseNodeKind(ks) else null,
        .limit = limit,
    }) catch return error.OutOfMemory;
    defer result.deinit(allocator);

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };

    stream.beginObject() catch return error.OutOfMemory;
    stream.objectField("nodes") catch return error.OutOfMemory;
    stream.beginArray() catch return error.OutOfMemory;
    for (result.nodes) |nid| {
        const n = g.getNode(nid) orelse continue;
        try writeNodeSummary(&stream, n, nid, g.project_root);
    }
    stream.endArray() catch return error.OutOfMemory;
    stream.endObject() catch return error.OutOfMemory;

    return wrapToolResult(allocator, &aw);
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

    stream.beginObject() catch return error.OutOfMemory;

    if (node_ids.len > 2) {
        // NxN matrix
        stream.objectField("matrix") catch return error.OutOfMemory;
        stream.beginObject() catch return error.OutOfMemory;
        stream.objectField("node_ids") catch return error.OutOfMemory;
        stream.beginArray() catch return error.OutOfMemory;
        for (node_ids) |nid| {
            try writeNodeIdHex(&stream, nid);
        }
        stream.endArray() catch return error.OutOfMemory;
        stream.objectField("similarities") catch return error.OutOfMemory;
        stream.beginArray() catch return error.OutOfMemory;
        for (node_ids) |nid_a| {
            stream.beginArray() catch return error.OutOfMemory;
            for (node_ids) |nid_b| {
                const sim = computeNodeSimilarity(allocator, g, nid_a, nid_b);
                stream.write(sim) catch return error.OutOfMemory;
            }
            stream.endArray() catch return error.OutOfMemory;
        }
        stream.endArray() catch return error.OutOfMemory;
        stream.endObject() catch return error.OutOfMemory;
    }

    // Pairwise pairs
    stream.objectField("pairs") catch return error.OutOfMemory;
    stream.beginArray() catch return error.OutOfMemory;
    for (node_ids, 0..) |nid_a, i| {
        for (node_ids[i + 1 ..]) |nid_b| {
            const sim = computeNodeSimilarity(allocator, g, nid_a, nid_b);
            stream.beginObject() catch return error.OutOfMemory;
            stream.objectField("a") catch return error.OutOfMemory;
            try writeNodeIdHex(&stream, nid_a);
            stream.objectField("b") catch return error.OutOfMemory;
            try writeNodeIdHex(&stream, nid_b);
            stream.objectField("similarity") catch return error.OutOfMemory;
            stream.write(sim) catch return error.OutOfMemory;
            stream.endObject() catch return error.OutOfMemory;
        }
    }
    stream.endArray() catch return error.OutOfMemory;

    stream.endObject() catch return error.OutOfMemory;

    return wrapToolResult(allocator, &aw);
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

    stream.beginObject() catch return error.OutOfMemory;
    stream.objectField("success") catch return error.OutOfMemory;
    stream.write(true) catch return error.OutOfMemory;
    stream.objectField("count") catch return error.OutOfMemory;
    stream.write(node_ids.len) catch return error.OutOfMemory;
    stream.endObject() catch return error.OutOfMemory;

    return wrapToolResult(allocator, &aw);
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

    stream.beginObject() catch return error.OutOfMemory;
    stream.objectField("annotations") catch return error.OutOfMemory;
    stream.beginArray() catch return error.OutOfMemory;

    for (all_annotations) |ann| {
        if (tag_filter) |tf| {
            if (!std.mem.eql(u8, ann.tag, tf)) continue;
        }
        stream.beginObject() catch return error.OutOfMemory;
        stream.objectField("node_id") catch return error.OutOfMemory;
        try writeNodeIdHex(&stream, ann.node_id);
        stream.objectField("tag") catch return error.OutOfMemory;
        stream.write(ann.tag) catch return error.OutOfMemory;
        stream.objectField("note") catch return error.OutOfMemory;
        stream.write(ann.note) catch return error.OutOfMemory;
        stream.endObject() catch return error.OutOfMemory;
    }

    stream.endArray() catch return error.OutOfMemory;
    stream.endObject() catch return error.OutOfMemory;

    return wrapToolResult(allocator, &aw);
}

// -- analysis.duplicates --

fn handleDuplicates(allocator: std.mem.Allocator, gen: *GraphGeneration, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const g = &gen.graph;
    const min_lines = getOptionalInt(args, "min_lines", 3);

    const result = duplicates_mod.findDuplicates(allocator, g, .{
        .min_lines = min_lines,
    }) catch return error.OutOfMemory;
    defer result.deinit(allocator);

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };

    stream.beginObject() catch return error.OutOfMemory;
    stream.objectField("groups") catch return error.OutOfMemory;
    stream.beginArray() catch return error.OutOfMemory;
    for (result.groups) |group| {
        stream.beginObject() catch return error.OutOfMemory;
        stream.objectField("structural_hash") catch return error.OutOfMemory;
        stream.write(group.structural_hash) catch return error.OutOfMemory;
        stream.objectField("members") catch return error.OutOfMemory;
        stream.beginArray() catch return error.OutOfMemory;
        for (group.members) |member| {
            stream.beginObject() catch return error.OutOfMemory;
            stream.objectField("node_id") catch return error.OutOfMemory;
            try writeNodeIdHex(&stream, member.node_id);
            stream.objectField("name") catch return error.OutOfMemory;
            stream.write(member.name) catch return error.OutOfMemory;
            stream.objectField("file") catch return error.OutOfMemory;
            stream.write(relativePath(member.file_path, g.project_root)) catch return error.OutOfMemory;
            stream.endObject() catch return error.OutOfMemory;
        }
        stream.endArray() catch return error.OutOfMemory;
        stream.endObject() catch return error.OutOfMemory;
    }
    stream.endArray() catch return error.OutOfMemory;
    stream.endObject() catch return error.OutOfMemory;

    return wrapToolResult(allocator, &aw);
}

// -- analysis.complexity --

fn handleComplexity(allocator: std.mem.Allocator, gen: *GraphGeneration, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const g = &gen.graph;
    const top_n = getOptionalInt(args, "top_n", 10);
    const scope = getOptionalString(args, "scope");

    const result = complexity_mod.findComplex(allocator, g, .{
        .top_n = top_n,
        .scope = scope,
    }) catch return error.OutOfMemory;
    defer result.deinit(allocator);

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };

    stream.beginObject() catch return error.OutOfMemory;
    stream.objectField("nodes") catch return error.OutOfMemory;
    stream.beginArray() catch return error.OutOfMemory;
    for (result.nodes) |entry| {
        stream.beginObject() catch return error.OutOfMemory;
        stream.objectField("node_id") catch return error.OutOfMemory;
        try writeNodeIdHex(&stream, entry.node_id);
        stream.objectField("name") catch return error.OutOfMemory;
        stream.write(entry.name) catch return error.OutOfMemory;
        stream.objectField("file") catch return error.OutOfMemory;
        stream.write(relativePath(entry.file_path, g.project_root)) catch return error.OutOfMemory;
        stream.objectField("complexity") catch return error.OutOfMemory;
        stream.write(entry.complexity) catch return error.OutOfMemory;
        stream.endObject() catch return error.OutOfMemory;
    }
    stream.endArray() catch return error.OutOfMemory;
    stream.endObject() catch return error.OutOfMemory;

    return wrapToolResult(allocator, &aw);
}

// -- analysis.dead_code --

fn handleDeadCode(allocator: std.mem.Allocator, gen: *GraphGeneration, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const g = &gen.graph;
    const include_public = getOptionalBool(args, "include_public", false);
    const scope = getOptionalString(args, "scope");

    const result = dead_code_mod.findDeadCode(allocator, g, .{
        .include_public = include_public,
        .scope = scope,
    }) catch return error.OutOfMemory;
    defer result.deinit(allocator);

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };

    stream.beginObject() catch return error.OutOfMemory;
    stream.objectField("nodes") catch return error.OutOfMemory;
    stream.beginArray() catch return error.OutOfMemory;
    for (result.nodes) |entry| {
        stream.beginObject() catch return error.OutOfMemory;
        stream.objectField("node_id") catch return error.OutOfMemory;
        try writeNodeIdHex(&stream, entry.node_id);
        stream.objectField("name") catch return error.OutOfMemory;
        stream.write(entry.name) catch return error.OutOfMemory;
        stream.objectField("kind") catch return error.OutOfMemory;
        stream.write(@tagName(entry.kind)) catch return error.OutOfMemory;
        stream.objectField("file") catch return error.OutOfMemory;
        stream.write(relativePath(entry.file_path, g.project_root)) catch return error.OutOfMemory;
        stream.objectField("visibility") catch return error.OutOfMemory;
        stream.write(@tagName(entry.visibility)) catch return error.OutOfMemory;
        stream.endObject() catch return error.OutOfMemory;
    }
    stream.endArray() catch return error.OutOfMemory;
    stream.endObject() catch return error.OutOfMemory;

    return wrapToolResult(allocator, &aw);
}

// -- analysis.dependency_cycles --

fn handleDependencyCycles(allocator: std.mem.Allocator, gen: *GraphGeneration, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const g = &gen.graph;
    const max_cycle_length = getOptionalInt(args, "max_cycle_length", 20);

    const result = cycles_mod.findCycles(allocator, g, .{
        .max_cycle_length = max_cycle_length,
    }) catch return error.OutOfMemory;
    defer result.deinit(allocator);

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };

    stream.beginObject() catch return error.OutOfMemory;
    stream.objectField("cycles") catch return error.OutOfMemory;
    stream.beginArray() catch return error.OutOfMemory;
    for (result.cycles) |cycle| {
        stream.beginObject() catch return error.OutOfMemory;
        stream.objectField("nodes") catch return error.OutOfMemory;
        stream.beginArray() catch return error.OutOfMemory;
        for (cycle.nodes) |cn| {
            stream.beginObject() catch return error.OutOfMemory;
            stream.objectField("node_id") catch return error.OutOfMemory;
            try writeNodeIdHex(&stream, cn.node_id);
            stream.objectField("name") catch return error.OutOfMemory;
            stream.write(cn.name) catch return error.OutOfMemory;
            stream.objectField("file") catch return error.OutOfMemory;
            stream.write(relativePath(cn.file_path, g.project_root)) catch return error.OutOfMemory;
            stream.endObject() catch return error.OutOfMemory;
        }
        stream.endArray() catch return error.OutOfMemory;
        stream.endObject() catch return error.OutOfMemory;
    }
    stream.endArray() catch return error.OutOfMemory;
    stream.endObject() catch return error.OutOfMemory;

    return wrapToolResult(allocator, &aw);
}

// -- analysis.coupling --

fn handleCoupling(allocator: std.mem.Allocator, gen: *GraphGeneration, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const g = &gen.graph;
    const top_n = getOptionalInt(args, "top_n", 20);
    const min_coupling = getOptionalFloat(args, "min_coupling", 1.0);

    const result = coupling_mod.findCoupling(allocator, g, .{
        .min_coupling = min_coupling,
        .top_n = top_n,
    }) catch return error.OutOfMemory;
    defer result.deinit(allocator);

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };

    stream.beginObject() catch return error.OutOfMemory;
    stream.objectField("pairs") catch return error.OutOfMemory;
    stream.beginArray() catch return error.OutOfMemory;
    for (result.pairs) |pair| {
        stream.beginObject() catch return error.OutOfMemory;
        stream.objectField("module_a") catch return error.OutOfMemory;
        stream.write(relativePath(pair.module_a, g.project_root)) catch return error.OutOfMemory;
        stream.objectField("module_b") catch return error.OutOfMemory;
        stream.write(relativePath(pair.module_b, g.project_root)) catch return error.OutOfMemory;
        stream.objectField("shared_edges") catch return error.OutOfMemory;
        stream.write(pair.shared_edges) catch return error.OutOfMemory;
        stream.objectField("score") catch return error.OutOfMemory;
        stream.write(pair.score) catch return error.OutOfMemory;
        stream.endObject() catch return error.OutOfMemory;
    }
    stream.endArray() catch return error.OutOfMemory;
    stream.endObject() catch return error.OutOfMemory;

    return wrapToolResult(allocator, &aw);
}

// -- analysis.impact --

fn handleImpact(allocator: std.mem.Allocator, gen: *GraphGeneration, params: ?std.json.Value) HandlerError![]const u8 {
    const args = getArgs(params);
    const g = &gen.graph;
    const max_depth = getOptionalInt(args, "max_depth", 10);

    const node_ids = try collectNodeIds(allocator, args, "node_ids");
    defer if (node_ids.len > 0) allocator.free(node_ids);

    if (node_ids.len == 0) {
        return try errorResult(allocator, "node_ids is required");
    }

    const result = impact_mod.analyzeImpact(allocator, g, node_ids, .{
        .max_depth = max_depth,
    }) catch return error.OutOfMemory;
    defer result.deinit(allocator);

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };

    stream.beginObject() catch return error.OutOfMemory;
    stream.objectField("total_impacted") catch return error.OutOfMemory;
    stream.write(result.total_impacted) catch return error.OutOfMemory;
    stream.objectField("dependents") catch return error.OutOfMemory;
    stream.beginArray() catch return error.OutOfMemory;
    for (result.dependents) |dep| {
        stream.beginObject() catch return error.OutOfMemory;
        stream.objectField("node_id") catch return error.OutOfMemory;
        try writeNodeIdHex(&stream, dep.node_id);
        stream.objectField("name") catch return error.OutOfMemory;
        stream.write(dep.name) catch return error.OutOfMemory;
        stream.objectField("kind") catch return error.OutOfMemory;
        stream.write(@tagName(dep.kind)) catch return error.OutOfMemory;
        stream.objectField("file") catch return error.OutOfMemory;
        stream.write(relativePath(dep.file_path, g.project_root)) catch return error.OutOfMemory;
        stream.endObject() catch return error.OutOfMemory;
    }
    stream.endArray() catch return error.OutOfMemory;
    stream.endObject() catch return error.OutOfMemory;

    return wrapToolResult(allocator, &aw);
}

// -- Error result helper --

fn errorResult(allocator: std.mem.Allocator, message: []const u8) HandlerError![]const u8 {
    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    stream.beginObject() catch return error.OutOfMemory;
    stream.objectField("error") catch return error.OutOfMemory;
    stream.write(message) catch return error.OutOfMemory;
    stream.endObject() catch return error.OutOfMemory;
    return wrapToolResult(allocator, &aw);
}

// -- MCP result wrapper --

fn wrapToolResult(allocator: std.mem.Allocator, inner_aw: *std.io.Writer.Allocating) HandlerError![]const u8 {
    const inner_json = inner_aw.toOwnedSlice() catch return error.OutOfMemory;
    defer allocator.free(inner_json);

    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };

    stream.beginObject() catch return error.OutOfMemory;
    stream.objectField("content") catch return error.OutOfMemory;
    stream.beginArray() catch return error.OutOfMemory;
    stream.beginObject() catch return error.OutOfMemory;
    stream.objectField("type") catch return error.OutOfMemory;
    stream.write("text") catch return error.OutOfMemory;
    stream.objectField("text") catch return error.OutOfMemory;
    stream.write(inner_json) catch return error.OutOfMemory;
    stream.endObject() catch return error.OutOfMemory;
    stream.endArray() catch return error.OutOfMemory;
    stream.endObject() catch return error.OutOfMemory;

    return aw.toOwnedSlice() catch return error.OutOfMemory;
}
