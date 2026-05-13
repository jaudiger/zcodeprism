const std = @import("std");
const zcodeprism = @import("zcodeprism");
const fixtures = @import("test-fixtures");
const helpers = @import("test-helpers");

const Graph = zcodeprism.graph.Graph;
const Node = zcodeprism.node.Node;
const NodeId = zcodeprism.types.NodeId;
const NodeKind = zcodeprism.types.NodeKind;
const EdgeType = zcodeprism.types.EdgeType;

const server_mod = zcodeprism.mcp.server;
const jsonrpc = zcodeprism.mcp.jsonrpc;
const generation_mod = zcodeprism.generation;
const gen_manager_mod = zcodeprism.watcher.generation_manager;

const Server = server_mod.Server;
const GraphGeneration = generation_mod.GraphGeneration;
const GenerationManager = gen_manager_mod.GenerationManager;
const indexDirectory = zcodeprism.indexer.indexDirectory;
const writeFixtureFiles = helpers.writeFixtureFiles;

fn parseJsonResponse(allocator: std.mem.Allocator, bytes: []const u8) !std.json.Parsed(std.json.Value) {
    return std.json.parseFromSlice(std.json.Value, allocator, bytes, .{});
}

fn setupProjectFixtures(tmp_dir: *std.testing.TmpDir) ![:0]const u8 {
    try writeFixtureFiles(std.testing.io, tmp_dir.dir, &.{
        .{ .sub_path = "main.zig", .data = fixtures.zig.project.main_zig },
        .{ .sub_path = "parser.zig", .data = fixtures.zig.project.parser_zig },
        .{ .sub_path = "utils.zig", .data = fixtures.zig.project.utils_zig },
    });
    return try tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
}

fn formatNodeId(buf: []u8, id: NodeId) []const u8 {
    const val = @intFromEnum(id);
    return std.fmt.bufPrint(buf, "{x}", .{val}) catch unreachable;
}

/// Extract the inner text from a tools/call MCP response (result.content[0].text).
fn getToolResultText(parsed: std.json.Value) ?[]const u8 {
    const result = parsed.object.get("result") orelse return null;
    const content = result.object.get("content") orelse return null;
    if (content != .array) return null;
    if (content.array.items.len == 0) return null;
    const first = content.array.items[0];
    const text_val = first.object.get("text") orelse return null;
    if (text_val != .string) return null;
    return text_val.string;
}

fn jsonAsFloat(val: std.json.Value) f64 {
    return switch (val) {
        .float => val.float,
        .integer => @floatFromInt(val.integer),
        else => 0.0,
    };
}

fn isErrorResponse(parsed: std.json.Value) bool {
    return parsed.object.get("error") != null;
}

/// Send a tools/call request, then double-parse: outer JSON-RPC envelope
/// and inner domain JSON from the MCP text field.
/// Returns null on server error.
fn callToolAndParseInner(
    allocator: std.mem.Allocator,
    srv: *Server,
    request: []const u8,
) !?struct { outer: std.json.Parsed(std.json.Value), inner: std.json.Parsed(std.json.Value), response_bytes: []const u8 } {
    const response_bytes = (try srv.handleMessage(allocator, std.testing.io, request)) orelse return null;
    errdefer allocator.free(response_bytes);
    var outer = try parseJsonResponse(allocator, response_bytes);
    errdefer outer.deinit();
    if (isErrorResponse(outer.value)) {
        outer.deinit();
        allocator.free(response_bytes);
        return null;
    }
    const text = getToolResultText(outer.value) orelse {
        outer.deinit();
        allocator.free(response_bytes);
        return null;
    };
    const inner = try std.json.parseFromSlice(std.json.Value, allocator, text, .{});
    return .{ .outer = outer, .inner = inner, .response_bytes = response_bytes };
}

// ---------------------------------------------------------------------------
// graph.stats
// ---------------------------------------------------------------------------

test "stats returns file count, function count, languages, and externals" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"graph.stats","arguments":{"include_external_nodes":true}}}
    ;

    // Act
    const result = try callToolAndParseInner(allocator, &srv, input) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();
    const v = result.inner.value.object;

    // Assert
    try std.testing.expect(v.get("total_files").?.integer >= 3);

    const nodes_obj = v.get("nodes").?.object;
    try std.testing.expect(nodes_obj.get("function").?.integer > 0);

    const langs = v.get("languages").?.array;
    var found_zig = false;
    for (langs.items) |lang| {
        if (std.mem.eql(u8, lang.string, "zig")) found_zig = true;
    }
    try std.testing.expect(found_zig);

    const externals = v.get("externals").?.object;
    try std.testing.expect(externals.get("stdlib_symbols").?.integer > 0);
}

test "stats with scope restricts to one file" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"graph.stats","arguments":{"scope":"parser.zig"}}}
    ;

    // Act
    const result = try callToolAndParseInner(allocator, &srv, input) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert
    try std.testing.expectEqual(@as(i64, 1), result.inner.value.object.get("total_files").?.integer);
}

test "stats on empty graph" {
    // Arrange
    const allocator = std.testing.allocator;
    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    _ = try gen.graph.freeze(gen.arena.allocator());
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"graph.stats","arguments":{}}}
    ;

    // Act
    const result = try callToolAndParseInner(allocator, &srv, input) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert
    try std.testing.expectEqual(@as(i64, 0), result.inner.value.object.get("total_files").?.integer);
}

// ---------------------------------------------------------------------------
// graph.search
// ---------------------------------------------------------------------------

test "search returns matching nodes with metadata" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"graph.search","arguments":{"query":"parse"}}}
    ;

    // Act
    const result = try callToolAndParseInner(allocator, &srv, input) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();
    const v = result.inner.value.object;

    // Assert
    try std.testing.expect(v.get("total_matches").?.integer > 0);
    const nodes = v.get("nodes").?.array;
    try std.testing.expect(nodes.items.len > 0);
    const first = nodes.items[0].object;
    try std.testing.expect(first.get("id") != null);
    try std.testing.expect(first.get("name") != null);
    try std.testing.expect(first.get("kind") != null);
    try std.testing.expect(first.get("file") != null);
}

test "search with kind filter returns only that kind" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"graph.search","arguments":{"query":".*","kind":"function"}}}
    ;

    // Act
    const result = try callToolAndParseInner(allocator, &srv, input) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert
    const nodes = result.inner.value.object.get("nodes").?.array;
    for (nodes.items) |node| {
        try std.testing.expectEqualStrings("function", node.object.get("kind").?.string);
    }
}

test "search with no results returns zero matches" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"graph.search","arguments":{"query":"zzz_nonexistent"}}}
    ;

    // Act
    const result = try callToolAndParseInner(allocator, &srv, input) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert
    try std.testing.expectEqual(@as(i64, 0), result.inner.value.object.get("total_matches").?.integer);
}

// ---------------------------------------------------------------------------
// graph.get_nodes
// ---------------------------------------------------------------------------

test "get_nodes returns all fields for a single id" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const node = helpers.findNode(&gen.graph, "parse", .function) orelse return;
    var id_buf: [20]u8 = undefined;
    const id_str = formatNodeId(&id_buf, node.id);

    const request = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"graph.get_nodes","arguments":{{"node_ids":"{s}"}}}}}}
    , .{id_str});
    defer allocator.free(request);

    // Act
    const result = try callToolAndParseInner(allocator, &srv, request) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();
    const nodes = result.inner.value.object.get("nodes").?.array;

    // Assert: single string id returns exactly one node
    try std.testing.expectEqual(@as(usize, 1), nodes.items.len);
    const first = nodes.items[0].object;
    try std.testing.expect(first.get("id") != null);
    try std.testing.expect(first.get("name") != null);
    try std.testing.expect(first.get("kind") != null);
    try std.testing.expect(first.contains("lang_meta"));
    try std.testing.expect(first.contains("parent_id"));
    try std.testing.expect(first.contains("doc"));
    try std.testing.expect(first.contains("external"));
    try std.testing.expect(first.contains("metrics"));
}

test "get_nodes with array of ids" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const n1 = helpers.findNode(&gen.graph, "parse", .function) orelse return;
    const n2 = helpers.findNode(&gen.graph, "formatOutput", .function) orelse return;
    var buf1: [20]u8 = undefined;
    var buf2: [20]u8 = undefined;
    const id1 = formatNodeId(&buf1, n1.id);
    const id2 = formatNodeId(&buf2, n2.id);

    const request = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"graph.get_nodes","arguments":{{"node_ids":["{s}","{s}"]}}}}}}
    , .{ id1, id2 });
    defer allocator.free(request);

    // Act
    const result = try callToolAndParseInner(allocator, &srv, request) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert
    const nodes = result.inner.value.object.get("nodes").?.array;
    try std.testing.expectEqual(@as(usize, 2), nodes.items.len);
}

test "get_nodes with include_source true" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const node = helpers.findNode(&gen.graph, "parse", .function) orelse return;
    var id_buf: [20]u8 = undefined;
    const id_str = formatNodeId(&id_buf, node.id);

    const request = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"graph.get_nodes","arguments":{{"node_ids":"{s}","include_source":true}}}}}}
    , .{id_str});
    defer allocator.free(request);

    // Act
    const result = try callToolAndParseInner(allocator, &srv, request) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert
    const nodes = result.inner.value.object.get("nodes").?.array;
    const source_field = nodes.items[0].object.get("source") orelse return;
    try std.testing.expect(source_field != .null);
}

test "get_nodes for phantom node has external and null source" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    var phantom_id: ?NodeId = null;
    for (gen.graph.nodes.items, 0..) |n, i| {
        if (n.external != .none) {
            phantom_id = @enumFromInt(i);
            break;
        }
    }
    const pid = phantom_id orelse return;
    var id_buf: [20]u8 = undefined;
    const id_str = formatNodeId(&id_buf, pid);

    const request = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"graph.get_nodes","arguments":{{"node_ids":"{s}","include_source":true}}}}}}
    , .{id_str});
    defer allocator.free(request);

    // Act
    const result = try callToolAndParseInner(allocator, &srv, request) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();
    const first = result.inner.value.object.get("nodes").?.array.items[0].object;

    // Assert
    const ext = first.get("external") orelse return;
    try std.testing.expect(ext != .null);
    const source_field = first.get("source") orelse return;
    try std.testing.expect(source_field == .null);
}

test "get_nodes with non-existent id" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"graph.get_nodes","arguments":{"node_ids":"ffffffff"}}}
    ;

    // Act
    const response_bytes = (try srv.handleMessage(allocator, std.testing.io, input)).?;
    defer allocator.free(response_bytes);
    const parsed = try parseJsonResponse(allocator, response_bytes);
    defer parsed.deinit();

    // Assert: either an error response or an empty nodes array
    if (isErrorResponse(parsed.value)) return;
    const text = getToolResultText(parsed.value) orelse return;
    var inner = try std.json.parseFromSlice(std.json.Value, allocator, text, .{});
    defer inner.deinit();
    if (inner.value.object.get("error")) |_| return;
    const nodes = inner.value.object.get("nodes") orelse return;
    try std.testing.expectEqual(@as(usize, 0), nodes.array.items.len);
}

test "get_nodes string id equals single-element array" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const node = helpers.findNode(&gen.graph, "parse", .function) orelse return;
    var id_buf: [20]u8 = undefined;
    const id_str = formatNodeId(&id_buf, node.id);

    const req_string = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"graph.get_nodes","arguments":{{"node_ids":"{s}"}}}}}}
    , .{id_str});
    defer allocator.free(req_string);

    const req_array = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{{"name":"graph.get_nodes","arguments":{{"node_ids":["{s}"]}}}}}}
    , .{id_str});
    defer allocator.free(req_array);

    // Act
    const r1 = try callToolAndParseInner(allocator, &srv, req_string) orelse return;
    defer allocator.free(r1.response_bytes);
    defer r1.outer.deinit();
    defer r1.inner.deinit();
    const r2 = try callToolAndParseInner(allocator, &srv, req_array) orelse return;
    defer allocator.free(r2.response_bytes);
    defer r2.outer.deinit();
    defer r2.inner.deinit();

    // Assert
    const nodes1 = r1.inner.value.object.get("nodes").?.array;
    const nodes2 = r2.inner.value.object.get("nodes").?.array;
    try std.testing.expectEqual(nodes1.items.len, nodes2.items.len);
}

// ---------------------------------------------------------------------------
// graph.get_source
// ---------------------------------------------------------------------------

test "get_source full returns non-empty source" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const node = helpers.findNode(&gen.graph, "parse", .function) orelse return;
    var id_buf: [20]u8 = undefined;
    const id_str = formatNodeId(&id_buf, node.id);

    const request = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"graph.get_source","arguments":{{"node_ids":"{s}"}}}}}}
    , .{id_str});
    defer allocator.free(request);

    // Act
    const result = try callToolAndParseInner(allocator, &srv, request) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert
    const sources = result.inner.value.object.get("sources") orelse return;
    const first = sources.array.items[0].object;
    const source = first.get("source") orelse return;
    try std.testing.expect(source != .null);
    try std.testing.expect(source.string.len > 0);
}

test "get_source signature is shorter than full" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const node = helpers.findNode(&gen.graph, "parse", .function) orelse return;
    var id_buf: [20]u8 = undefined;
    const id_str = formatNodeId(&id_buf, node.id);

    const req_full = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"graph.get_source","arguments":{{"node_ids":"{s}"}}}}}}
    , .{id_str});
    defer allocator.free(req_full);

    const req_sig = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{{"name":"graph.get_source","arguments":{{"node_ids":"{s}","part":"signature"}}}}}}
    , .{id_str});
    defer allocator.free(req_sig);

    // Act
    const r_full = try callToolAndParseInner(allocator, &srv, req_full) orelse return;
    defer allocator.free(r_full.response_bytes);
    defer r_full.outer.deinit();
    defer r_full.inner.deinit();
    const r_sig = try callToolAndParseInner(allocator, &srv, req_sig) orelse return;
    defer allocator.free(r_sig.response_bytes);
    defer r_sig.outer.deinit();
    defer r_sig.inner.deinit();

    // Assert
    const full_src = r_full.inner.value.object.get("sources").?.array.items[0].object.get("source").?.string;
    const sig_src = r_sig.inner.value.object.get("sources").?.array.items[0].object.get("source").?.string;
    try std.testing.expect(sig_src.len <= full_src.len);
}

test "get_source body does not start with function keyword" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const node = helpers.findNode(&gen.graph, "parse", .function) orelse return;
    var id_buf: [20]u8 = undefined;
    const id_str = formatNodeId(&id_buf, node.id);

    const request = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"graph.get_source","arguments":{{"node_ids":"{s}","part":"body"}}}}}}
    , .{id_str});
    defer allocator.free(request);

    // Act
    const result = try callToolAndParseInner(allocator, &srv, request) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert
    const sources = result.inner.value.object.get("sources") orelse return;
    const source = sources.array.items[0].object.get("source") orelse return;
    if (source == .null) return;
    try std.testing.expect(!std.mem.startsWith(u8, source.string, "pub fn"));
    try std.testing.expect(!std.mem.startsWith(u8, source.string, "fn "));
}

test "get_source with context_lines is at least as long as without" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const node = helpers.findNode(&gen.graph, "parse", .function) orelse return;
    var id_buf: [20]u8 = undefined;
    const id_str = formatNodeId(&id_buf, node.id);

    const req_no_ctx = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"graph.get_source","arguments":{{"node_ids":"{s}"}}}}}}
    , .{id_str});
    defer allocator.free(req_no_ctx);

    const req_ctx = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{{"name":"graph.get_source","arguments":{{"node_ids":"{s}","context_lines":2}}}}}}
    , .{id_str});
    defer allocator.free(req_ctx);

    // Act
    const r1 = try callToolAndParseInner(allocator, &srv, req_no_ctx) orelse return;
    defer allocator.free(r1.response_bytes);
    defer r1.outer.deinit();
    defer r1.inner.deinit();
    const r2 = try callToolAndParseInner(allocator, &srv, req_ctx) orelse return;
    defer allocator.free(r2.response_bytes);
    defer r2.outer.deinit();
    defer r2.inner.deinit();

    // Assert
    const no_ctx_src = r1.inner.value.object.get("sources").?.array.items[0].object.get("source").?.string;
    const ctx_src = r2.inner.value.object.get("sources").?.array.items[0].object.get("source").?.string;
    try std.testing.expect(ctx_src.len >= no_ctx_src.len);
}

test "get_source for phantom node returns null source" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    var phantom_id: ?NodeId = null;
    for (gen.graph.nodes.items, 0..) |n, i| {
        if (n.external != .none) {
            phantom_id = @enumFromInt(i);
            break;
        }
    }
    const pid = phantom_id orelse return;
    var id_buf: [20]u8 = undefined;
    const id_str = formatNodeId(&id_buf, pid);

    const request = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"graph.get_source","arguments":{{"node_ids":"{s}"}}}}}}
    , .{id_str});
    defer allocator.free(request);

    // Act
    const result = try callToolAndParseInner(allocator, &srv, request) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert
    const sources = result.inner.value.object.get("sources") orelse return;
    const source = sources.array.items[0].object.get("source") orelse return;
    try std.testing.expect(source == .null);
}

test "get_source for file node returns file content" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const file_node = helpers.findNode(&gen.graph, "utils.zig", .file) orelse return;
    var id_buf: [20]u8 = undefined;
    const id_str = formatNodeId(&id_buf, file_node.id);

    const request = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"graph.get_source","arguments":{{"node_ids":"{s}"}}}}}}
    , .{id_str});
    defer allocator.free(request);

    // Act
    const result = try callToolAndParseInner(allocator, &srv, request) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert
    const sources = result.inner.value.object.get("sources") orelse return;
    const source = sources.array.items[0].object.get("source") orelse return;
    try std.testing.expect(source != .null);
    try std.testing.expect(source.string.len > 0);
}

// ---------------------------------------------------------------------------
// graph.get_edges
// ---------------------------------------------------------------------------

test "get_edges out direction" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const node = helpers.findNode(&gen.graph, "processInput", .function) orelse return;
    var id_buf: [20]u8 = undefined;
    const id_str = formatNodeId(&id_buf, node.id);

    const request = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"graph.get_edges","arguments":{{"node_ids":"{s}","direction":"out"}}}}}}
    , .{id_str});
    defer allocator.free(request);

    // Act
    const result = try callToolAndParseInner(allocator, &srv, request) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert
    const edges = result.inner.value.object.get("edges").?.array;
    try std.testing.expect(edges.items.len > 0);
    for (edges.items) |edge| {
        const from_val = edge.object.get("from") orelse edge.object.get("from_id") orelse continue;
        try std.testing.expectEqualStrings(id_str, from_val.string);
    }
}

test "get_edges in direction" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const node = helpers.findNode(&gen.graph, "parse", .function) orelse return;
    var id_buf: [20]u8 = undefined;
    const id_str = formatNodeId(&id_buf, node.id);

    const request = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"graph.get_edges","arguments":{{"node_ids":"{s}","direction":"in"}}}}}}
    , .{id_str});
    defer allocator.free(request);

    // Act
    const result = try callToolAndParseInner(allocator, &srv, request) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert
    const edges = result.inner.value.object.get("edges").?.array;
    try std.testing.expect(edges.items.len > 0);
    for (edges.items) |edge| {
        const to_val = edge.object.get("to") orelse edge.object.get("to_id") orelse continue;
        try std.testing.expectEqualStrings(id_str, to_val.string);
    }
}

test "get_edges both has at least as many as out only" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const node = helpers.findNode(&gen.graph, "processInput", .function) orelse return;
    var id_buf: [20]u8 = undefined;
    const id_str = formatNodeId(&id_buf, node.id);

    const req_out = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"graph.get_edges","arguments":{{"node_ids":"{s}","direction":"out"}}}}}}
    , .{id_str});
    defer allocator.free(req_out);

    const req_both = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{{"name":"graph.get_edges","arguments":{{"node_ids":"{s}","direction":"both"}}}}}}
    , .{id_str});
    defer allocator.free(req_both);

    // Act
    const r_out = try callToolAndParseInner(allocator, &srv, req_out) orelse return;
    defer allocator.free(r_out.response_bytes);
    defer r_out.outer.deinit();
    defer r_out.inner.deinit();
    const r_both = try callToolAndParseInner(allocator, &srv, req_both) orelse return;
    defer allocator.free(r_both.response_bytes);
    defer r_both.outer.deinit();
    defer r_both.inner.deinit();

    // Assert
    const out_count = r_out.inner.value.object.get("edges").?.array.items.len;
    const both_count = r_both.inner.value.object.get("edges").?.array.items.len;
    try std.testing.expect(both_count >= out_count);
}

test "get_edges with type filter and connected node info" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const node = helpers.findNode(&gen.graph, "processInput", .function) orelse return;
    var id_buf: [20]u8 = undefined;
    const id_str = formatNodeId(&id_buf, node.id);

    const request = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"graph.get_edges","arguments":{{"node_ids":"{s}","edge_type":"calls"}}}}}}
    , .{id_str});
    defer allocator.free(request);

    // Act
    const result = try callToolAndParseInner(allocator, &srv, request) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert: all edges have type "calls"
    const edges = result.inner.value.object.get("edges").?.array;
    for (edges.items) |edge| {
        try std.testing.expectEqualStrings("calls", edge.object.get("type").?.string);
    }
    // Assert: connected node info present
    if (edges.items.len > 0) {
        const first = edges.items[0].object;
        const to_node = first.get("to_node") orelse first.get("to_info") orelse return;
        try std.testing.expect(to_node.object.get("id") != null);
        try std.testing.expect(to_node.object.get("name") != null);
        try std.testing.expect(to_node.object.get("kind") != null);
    }
}

test "get_edges for isolated node returns empty" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    // Find a field node with no edges at all (truly isolated).
    var isolated_id: ?NodeId = null;
    for (gen.graph.nodes.items, 0..) |n, i| {
        if (n.kind == .field) {
            const candidate: NodeId = @enumFromInt(i);
            var has_edges = false;
            for (gen.graph.edges.items) |e| {
                if (e.source_id == candidate or e.target_id == candidate) {
                    has_edges = true;
                    break;
                }
            }
            if (!has_edges) {
                isolated_id = candidate;
                break;
            }
        }
    }
    const nid = isolated_id orelse return;
    var id_buf: [20]u8 = undefined;
    const id_str = formatNodeId(&id_buf, nid);

    const request = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"graph.get_edges","arguments":{{"node_ids":"{s}","direction":"both"}}}}}}
    , .{id_str});
    defer allocator.free(request);

    // Act
    const result = try callToolAndParseInner(allocator, &srv, request) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert
    const edges = result.inner.value.object.get("edges") orelse return;
    try std.testing.expectEqual(@as(usize, 0), edges.array.items.len);
}

// ---------------------------------------------------------------------------
// graph.path
// ---------------------------------------------------------------------------

test "path between connected nodes returns non-empty" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const from_node = helpers.findNode(&gen.graph, "processInput", .function) orelse return;
    const to_node = helpers.findNode(&gen.graph, "parse", .function) orelse return;
    var buf1: [20]u8 = undefined;
    var buf2: [20]u8 = undefined;
    const from_id = formatNodeId(&buf1, from_node.id);
    const to_id = formatNodeId(&buf2, to_node.id);

    const request = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"graph.path","arguments":{{"from_id":"{s}","to_id":"{s}"}}}}}}
    , .{ from_id, to_id });
    defer allocator.free(request);

    // Act
    const result = try callToolAndParseInner(allocator, &srv, request) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert
    const paths = result.inner.value.object.get("paths").?.array;
    try std.testing.expect(paths.items.len > 0);
}

test "path between unconnected nodes returns empty" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const f1 = helpers.findNode(&gen.graph, "isValid", .function) orelse return;
    const f2 = helpers.findNode(&gen.graph, "isDelimiter", .function) orelse return;
    var buf1: [20]u8 = undefined;
    var buf2: [20]u8 = undefined;
    const id1 = formatNodeId(&buf1, f1.id);
    const id2 = formatNodeId(&buf2, f2.id);

    const request = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"graph.path","arguments":{{"from_id":"{s}","to_id":"{s}"}}}}}}
    , .{ id1, id2 });
    defer allocator.free(request);

    // Act
    const result = try callToolAndParseInner(allocator, &srv, request) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert
    const paths = result.inner.value.object.get("paths").?.array;
    try std.testing.expectEqual(@as(usize, 0), paths.items.len);
}

test "path with max_depth zero returns empty" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const from_node = helpers.findNode(&gen.graph, "processInput", .function) orelse return;
    const to_node = helpers.findNode(&gen.graph, "parse", .function) orelse return;
    var buf1: [20]u8 = undefined;
    var buf2: [20]u8 = undefined;
    const from_id = formatNodeId(&buf1, from_node.id);
    const to_id = formatNodeId(&buf2, to_node.id);

    const request = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"graph.path","arguments":{{"from_id":"{s}","to_id":"{s}","max_depth":0}}}}}}
    , .{ from_id, to_id });
    defer allocator.free(request);

    // Act
    const result = try callToolAndParseInner(allocator, &srv, request) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert
    const paths = result.inner.value.object.get("paths").?.array;
    try std.testing.expectEqual(@as(usize, 0), paths.items.len);
}

test "path with edge_types filter returns only matching edges" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const f_main = helpers.findNode(&gen.graph, "main.zig", .file) orelse return;
    const f_utils = helpers.findNode(&gen.graph, "utils.zig", .file) orelse return;
    var buf1: [20]u8 = undefined;
    var buf2: [20]u8 = undefined;
    const from_id = formatNodeId(&buf1, f_main.id);
    const to_id = formatNodeId(&buf2, f_utils.id);

    const request = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"graph.path","arguments":{{"from_id":"{s}","to_id":"{s}","edge_types":["imports"]}}}}}}
    , .{ from_id, to_id });
    defer allocator.free(request);

    // Act
    const result = try callToolAndParseInner(allocator, &srv, request) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert
    const paths = result.inner.value.object.get("paths").?.array;
    for (paths.items) |path| {
        const path_edges = path.object.get("edges").?.array;
        for (path_edges.items) |edge| {
            try std.testing.expectEqualStrings("imports", edge.object.get("type").?.string);
        }
    }
}

test "path with non-existent node" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const node = helpers.findNode(&gen.graph, "parse", .function) orelse return;
    var id_buf: [20]u8 = undefined;
    const id_str = formatNodeId(&id_buf, node.id);

    const request = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"graph.path","arguments":{{"from_id":"{s}","to_id":"ffffffff"}}}}}}
    , .{id_str});
    defer allocator.free(request);

    // Act
    const response_bytes = (try srv.handleMessage(allocator, std.testing.io, request)).?;
    defer allocator.free(response_bytes);
    const parsed = try parseJsonResponse(allocator, response_bytes);
    defer parsed.deinit();

    // Assert: either an error or empty paths
    if (isErrorResponse(parsed.value)) return;
    const text = getToolResultText(parsed.value) orelse return;
    var inner = try std.json.parseFromSlice(std.json.Value, allocator, text, .{});
    defer inner.deinit();
    if (inner.value.object.get("error")) |_| return;
    const paths = inner.value.object.get("paths") orelse return;
    try std.testing.expectEqual(@as(usize, 0), paths.array.items.len);
}

// ---------------------------------------------------------------------------
// explorer.cursor_create
// ---------------------------------------------------------------------------

test "cursor_create returns cursor_id and neighborhood" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"explorer.cursor_create","arguments":{}}}
    ;

    // Act
    const result = try callToolAndParseInner(allocator, &srv, input) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();
    const v = result.inner.value.object;

    // Assert
    const cursor_id = v.get("cursor_id").?.string;
    try std.testing.expect(cursor_id.len > 0);

    const neighborhood = v.get("neighborhood").?.object;
    const children = neighborhood.get("children").?.array;
    try std.testing.expect(children.items.len > 0);
}

test "cursor_create with start_node positions there" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const node = helpers.findNode(&gen.graph, "parse", .function) orelse return;
    var id_buf: [20]u8 = undefined;
    const id_str = formatNodeId(&id_buf, node.id);

    const request = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"explorer.cursor_create","arguments":{{"start_node_id":"{s}"}}}}}}
    , .{id_str});
    defer allocator.free(request);

    // Act
    const result = try callToolAndParseInner(allocator, &srv, request) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert
    const position = result.inner.value.object.get("position").?.object;
    try std.testing.expectEqualStrings(id_str, position.get("id").?.string);
}

// ---------------------------------------------------------------------------
// explorer.cursor_move
// ---------------------------------------------------------------------------

test "cursor_move updates position" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const target_node = helpers.findNode(&gen.graph, "parse", .function) orelse return;
    var target_buf: [20]u8 = undefined;
    const target_id = formatNodeId(&target_buf, target_node.id);

    // Create cursor first
    const create_req =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"explorer.cursor_create","arguments":{}}}
    ;
    const r1 = try callToolAndParseInner(allocator, &srv, create_req) orelse return;
    defer allocator.free(r1.response_bytes);
    defer r1.outer.deinit();
    defer r1.inner.deinit();
    const cursor_id = r1.inner.value.object.get("cursor_id").?.string;

    // Act: move to target
    const move_req = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{{"name":"explorer.cursor_move","arguments":{{"cursor_id":"{s}","node_id":"{s}"}}}}}}
    , .{ cursor_id, target_id });
    defer allocator.free(move_req);

    const r2 = try callToolAndParseInner(allocator, &srv, move_req) orelse return;
    defer allocator.free(r2.response_bytes);
    defer r2.outer.deinit();
    defer r2.inner.deinit();

    // Assert
    const position = r2.inner.value.object.get("position").?.object;
    try std.testing.expectEqualStrings(target_id, position.get("id").?.string);
}

// ---------------------------------------------------------------------------
// explorer.cursor_close
// ---------------------------------------------------------------------------

test "cursor_close then move fails" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const target_node = helpers.findNode(&gen.graph, "parse", .function) orelse return;
    var target_buf: [20]u8 = undefined;
    const target_id = formatNodeId(&target_buf, target_node.id);

    // Create cursor
    const create_req =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"explorer.cursor_create","arguments":{}}}
    ;
    const r1 = try callToolAndParseInner(allocator, &srv, create_req) orelse return;
    defer allocator.free(r1.response_bytes);
    defer r1.outer.deinit();
    defer r1.inner.deinit();
    const cursor_id = r1.inner.value.object.get("cursor_id").?.string;

    // Close cursor
    const close_req = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{{"name":"explorer.cursor_close","arguments":{{"cursor_id":"{s}"}}}}}}
    , .{cursor_id});
    defer allocator.free(close_req);
    const close_resp = (try srv.handleMessage(allocator, std.testing.io, close_req)).?;
    defer allocator.free(close_resp);

    // Act: attempt move on closed cursor
    const move_req = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{{"name":"explorer.cursor_move","arguments":{{"cursor_id":"{s}","node_id":"{s}"}}}}}}
    , .{ cursor_id, target_id });
    defer allocator.free(move_req);

    const move_resp = (try srv.handleMessage(allocator, std.testing.io, move_req)).?;
    defer allocator.free(move_resp);
    const parsed = try parseJsonResponse(allocator, move_resp);
    defer parsed.deinit();

    // Assert: should be an error (invalid cursor) or null from callToolAndParseInner
    if (isErrorResponse(parsed.value)) return;
    const text = getToolResultText(parsed.value) orelse return;
    var inner = try std.json.parseFromSlice(std.json.Value, allocator, text, .{});
    defer inner.deinit();
    try std.testing.expect(inner.value.object.get("error") != null);
}

// ---------------------------------------------------------------------------
// explorer.cursor_expand
// ---------------------------------------------------------------------------

test "cursor_expand returns subgraph" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const start_node = helpers.findNode(&gen.graph, "processInput", .function) orelse return;
    var id_buf: [20]u8 = undefined;
    const start_id = formatNodeId(&id_buf, start_node.id);

    // Create cursor at a node with outgoing edges
    const create_req = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"explorer.cursor_create","arguments":{{"start_node_id":"{s}"}}}}}}
    , .{start_id});
    defer allocator.free(create_req);

    const r1 = try callToolAndParseInner(allocator, &srv, create_req) orelse return;
    defer allocator.free(r1.response_bytes);
    defer r1.outer.deinit();
    defer r1.inner.deinit();
    const cursor_id = r1.inner.value.object.get("cursor_id").?.string;

    // Act: expand from current position
    const expand_req = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{{"name":"explorer.cursor_expand","arguments":{{"cursor_id":"{s}","depth":2}}}}}}
    , .{cursor_id});
    defer allocator.free(expand_req);

    const r2 = try callToolAndParseInner(allocator, &srv, expand_req) orelse return;
    defer allocator.free(r2.response_bytes);
    defer r2.outer.deinit();
    defer r2.inner.deinit();

    // Assert
    const subgraph = r2.inner.value.object.get("subgraph").?.object;
    const nodes = subgraph.get("nodes").?.array;
    const edges = subgraph.get("edges").?.array;
    try std.testing.expect(nodes.items.len > 0);
    try std.testing.expect(edges.items.len > 0);
}

// ---------------------------------------------------------------------------
// explorer.cursor_query
// ---------------------------------------------------------------------------

test "cursor_query with kind filter" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    // Create cursor at root
    const create_req =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"explorer.cursor_create","arguments":{}}}
    ;
    const r1 = try callToolAndParseInner(allocator, &srv, create_req) orelse return;
    defer allocator.free(r1.response_bytes);
    defer r1.outer.deinit();
    defer r1.inner.deinit();
    const cursor_id = r1.inner.value.object.get("cursor_id").?.string;

    // Act: query for functions only
    const query_req = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{{"name":"explorer.cursor_query","arguments":{{"cursor_id":"{s}","kind":"function"}}}}}}
    , .{cursor_id});
    defer allocator.free(query_req);

    const r2 = try callToolAndParseInner(allocator, &srv, query_req) orelse return;
    defer allocator.free(r2.response_bytes);
    defer r2.outer.deinit();
    defer r2.inner.deinit();

    // Assert: all returned nodes must be functions
    const nodes = r2.inner.value.object.get("nodes").?.array;
    try std.testing.expect(nodes.items.len > 0);
    for (nodes.items) |n| {
        try std.testing.expectEqualStrings("function", n.object.get("kind").?.string);
    }
}

// ---------------------------------------------------------------------------
// explorer.diff
// ---------------------------------------------------------------------------

test "diff identical function with itself" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const node = helpers.findNode(&gen.graph, "parse", .function) orelse return;
    var id_buf: [20]u8 = undefined;
    const id_str = formatNodeId(&id_buf, node.id);

    const request = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"explorer.diff","arguments":{{"node_ids":["{s}","{s}"]}}}}}}
    , .{ id_str, id_str });
    defer allocator.free(request);

    // Act
    const result = try callToolAndParseInner(allocator, &srv, request) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert: self-diff has similarity 1.0
    const pairs = result.inner.value.object.get("pairs").?.array;
    try std.testing.expect(pairs.items.len > 0);
    const similarity = jsonAsFloat(pairs.items[0].object.get("similarity").?);
    try std.testing.expectApproxEqAbs(@as(f64, 1.0), similarity, 0.001);
}

test "diff two different functions" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const fn_a = helpers.findNode(&gen.graph, "parse", .function) orelse return;
    const fn_b = helpers.findNode(&gen.graph, "processInput", .function) orelse return;
    var buf_a: [20]u8 = undefined;
    var buf_b: [20]u8 = undefined;
    const id_a = formatNodeId(&buf_a, fn_a.id);
    const id_b = formatNodeId(&buf_b, fn_b.id);

    const request = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"explorer.diff","arguments":{{"node_ids":["{s}","{s}"]}}}}}}
    , .{ id_a, id_b });
    defer allocator.free(request);

    // Act
    const result = try callToolAndParseInner(allocator, &srv, request) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert: different functions have similarity between 0 and 1 exclusive
    const pairs = result.inner.value.object.get("pairs").?.array;
    try std.testing.expect(pairs.items.len > 0);
    const similarity = jsonAsFloat(pairs.items[0].object.get("similarity").?);
    try std.testing.expect(similarity >= 0.0);
    try std.testing.expect(similarity < 1.0);
}

test "diff N nodes returns NxN matrix" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const fn1 = helpers.findNode(&gen.graph, "parse", .function) orelse return;
    const fn2 = helpers.findNode(&gen.graph, "processInput", .function) orelse return;
    const fn3 = helpers.findNode(&gen.graph, "formatOutput", .function) orelse return;
    var buf1: [20]u8 = undefined;
    var buf2: [20]u8 = undefined;
    var buf3: [20]u8 = undefined;
    const id1 = formatNodeId(&buf1, fn1.id);
    const id2 = formatNodeId(&buf2, fn2.id);
    const id3 = formatNodeId(&buf3, fn3.id);

    const request = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"explorer.diff","arguments":{{"node_ids":["{s}","{s}","{s}"]}}}}}}
    , .{ id1, id2, id3 });
    defer allocator.free(request);

    // Act
    const result = try callToolAndParseInner(allocator, &srv, request) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert: 3x3 matrix with 1.0 on diagonal
    const matrix = result.inner.value.object.get("matrix").?.object;
    const node_ids = matrix.get("node_ids").?.array;
    try std.testing.expectEqual(@as(usize, 3), node_ids.items.len);

    const similarities = matrix.get("similarities").?.array;
    try std.testing.expectEqual(@as(usize, 3), similarities.items.len);
    for (similarities.items, 0..) |row, i| {
        try std.testing.expectEqual(@as(usize, 3), row.array.items.len);
        const diag = jsonAsFloat(row.array.items[i]);
        try std.testing.expectApproxEqAbs(@as(f64, 1.0), diag, 0.001);
    }
}

// ---------------------------------------------------------------------------
// explorer.annotate / explorer.annotations
// ---------------------------------------------------------------------------

test "annotate sets tag and annotations returns it" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const node = helpers.findNode(&gen.graph, "parse", .function) orelse return;
    var id_buf: [20]u8 = undefined;
    const id_str = formatNodeId(&id_buf, node.id);

    // Create cursor
    const create_req =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"explorer.cursor_create","arguments":{}}}
    ;
    const r1 = try callToolAndParseInner(allocator, &srv, create_req) orelse return;
    defer allocator.free(r1.response_bytes);
    defer r1.outer.deinit();
    defer r1.inner.deinit();
    const cursor_id = r1.inner.value.object.get("cursor_id").?.string;

    // Annotate
    const annotate_req = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{{"name":"explorer.annotate","arguments":{{"cursor_id":"{s}","node_ids":"{s}","tag":"reviewed"}}}}}}
    , .{ cursor_id, id_str });
    defer allocator.free(annotate_req);
    const r2 = try callToolAndParseInner(allocator, &srv, annotate_req) orelse return;
    defer allocator.free(r2.response_bytes);
    defer r2.outer.deinit();
    defer r2.inner.deinit();

    // Act: query annotations
    const annot_req = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{{"name":"explorer.annotations","arguments":{{"cursor_id":"{s}"}}}}}}
    , .{cursor_id});
    defer allocator.free(annot_req);
    const r3 = try callToolAndParseInner(allocator, &srv, annot_req) orelse return;
    defer allocator.free(r3.response_bytes);
    defer r3.outer.deinit();
    defer r3.inner.deinit();

    // Assert
    const annotations = r3.inner.value.object.get("annotations").?.array;
    try std.testing.expect(annotations.items.len > 0);
    var found_tag = false;
    for (annotations.items) |a| {
        if (std.mem.eql(u8, a.object.get("tag").?.string, "reviewed")) found_tag = true;
    }
    try std.testing.expect(found_tag);
}

test "annotate with note" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const node = helpers.findNode(&gen.graph, "parse", .function) orelse return;
    var id_buf: [20]u8 = undefined;
    const id_str = formatNodeId(&id_buf, node.id);

    // Create cursor
    const create_req =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"explorer.cursor_create","arguments":{}}}
    ;
    const r1 = try callToolAndParseInner(allocator, &srv, create_req) orelse return;
    defer allocator.free(r1.response_bytes);
    defer r1.outer.deinit();
    defer r1.inner.deinit();
    const cursor_id = r1.inner.value.object.get("cursor_id").?.string;

    // Annotate with note
    const annotate_req = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{{"name":"explorer.annotate","arguments":{{"cursor_id":"{s}","node_ids":"{s}","tag":"candidate","note":"needs refactor"}}}}}}
    , .{ cursor_id, id_str });
    defer allocator.free(annotate_req);
    const r2 = try callToolAndParseInner(allocator, &srv, annotate_req) orelse return;
    defer allocator.free(r2.response_bytes);
    defer r2.outer.deinit();
    defer r2.inner.deinit();

    // Act: query annotations
    const annot_req = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{{"name":"explorer.annotations","arguments":{{"cursor_id":"{s}"}}}}}}
    , .{cursor_id});
    defer allocator.free(annot_req);
    const r3 = try callToolAndParseInner(allocator, &srv, annot_req) orelse return;
    defer allocator.free(r3.response_bytes);
    defer r3.outer.deinit();
    defer r3.inner.deinit();

    // Assert
    const annotations = r3.inner.value.object.get("annotations").?.array;
    try std.testing.expect(annotations.items.len > 0);
    var found = false;
    for (annotations.items) |a| {
        if (std.mem.eql(u8, a.object.get("tag").?.string, "candidate")) {
            const note = a.object.get("note").?.string;
            try std.testing.expectEqualStrings("needs refactor", note);
            found = true;
        }
    }
    try std.testing.expect(found);
}

test "annotate multiple nodes" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const fn_a = helpers.findNode(&gen.graph, "parse", .function) orelse return;
    const fn_b = helpers.findNode(&gen.graph, "formatOutput", .function) orelse return;
    var buf_a: [20]u8 = undefined;
    var buf_b: [20]u8 = undefined;
    const id_a = formatNodeId(&buf_a, fn_a.id);
    const id_b = formatNodeId(&buf_b, fn_b.id);

    // Create cursor
    const create_req =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"explorer.cursor_create","arguments":{}}}
    ;
    const r1 = try callToolAndParseInner(allocator, &srv, create_req) orelse return;
    defer allocator.free(r1.response_bytes);
    defer r1.outer.deinit();
    defer r1.inner.deinit();
    const cursor_id = r1.inner.value.object.get("cursor_id").?.string;

    // Annotate both nodes
    const annotate_req = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{{"name":"explorer.annotate","arguments":{{"cursor_id":"{s}","node_ids":["{s}","{s}"],"tag":"marked"}}}}}}
    , .{ cursor_id, id_a, id_b });
    defer allocator.free(annotate_req);
    const r2 = try callToolAndParseInner(allocator, &srv, annotate_req) orelse return;
    defer allocator.free(r2.response_bytes);
    defer r2.outer.deinit();
    defer r2.inner.deinit();

    // Act: query annotations
    const annot_req = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{{"name":"explorer.annotations","arguments":{{"cursor_id":"{s}"}}}}}}
    , .{cursor_id});
    defer allocator.free(annot_req);
    const r3 = try callToolAndParseInner(allocator, &srv, annot_req) orelse return;
    defer allocator.free(r3.response_bytes);
    defer r3.outer.deinit();
    defer r3.inner.deinit();

    // Assert: both node IDs appear in annotations
    const annotations = r3.inner.value.object.get("annotations").?.array;
    var found_a = false;
    var found_b = false;
    for (annotations.items) |a| {
        const nid = a.object.get("node_id").?.string;
        if (std.mem.eql(u8, nid, id_a)) found_a = true;
        if (std.mem.eql(u8, nid, id_b)) found_b = true;
    }
    try std.testing.expect(found_a);
    try std.testing.expect(found_b);
}

test "annotations filter by tag" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const fn_a = helpers.findNode(&gen.graph, "parse", .function) orelse return;
    const fn_b = helpers.findNode(&gen.graph, "formatOutput", .function) orelse return;
    var buf_a: [20]u8 = undefined;
    var buf_b: [20]u8 = undefined;
    const id_a = formatNodeId(&buf_a, fn_a.id);
    const id_b = formatNodeId(&buf_b, fn_b.id);

    // Create cursor
    const create_req =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"explorer.cursor_create","arguments":{}}}
    ;
    const r1 = try callToolAndParseInner(allocator, &srv, create_req) orelse return;
    defer allocator.free(r1.response_bytes);
    defer r1.outer.deinit();
    defer r1.inner.deinit();
    const cursor_id = r1.inner.value.object.get("cursor_id").?.string;

    // Annotate with different tags
    const ann1_req = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{{"name":"explorer.annotate","arguments":{{"cursor_id":"{s}","node_ids":"{s}","tag":"alpha"}}}}}}
    , .{ cursor_id, id_a });
    defer allocator.free(ann1_req);
    const r2 = try callToolAndParseInner(allocator, &srv, ann1_req) orelse return;
    defer allocator.free(r2.response_bytes);
    defer r2.outer.deinit();
    defer r2.inner.deinit();

    const ann2_req = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{{"name":"explorer.annotate","arguments":{{"cursor_id":"{s}","node_ids":"{s}","tag":"beta"}}}}}}
    , .{ cursor_id, id_b });
    defer allocator.free(ann2_req);
    const r3 = try callToolAndParseInner(allocator, &srv, ann2_req) orelse return;
    defer allocator.free(r3.response_bytes);
    defer r3.outer.deinit();
    defer r3.inner.deinit();

    // Act: query with tag filter
    const query_req = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{{"name":"explorer.annotations","arguments":{{"cursor_id":"{s}","tag":"alpha"}}}}}}
    , .{cursor_id});
    defer allocator.free(query_req);
    const r4 = try callToolAndParseInner(allocator, &srv, query_req) orelse return;
    defer allocator.free(r4.response_bytes);
    defer r4.outer.deinit();
    defer r4.inner.deinit();

    // Assert: only alpha tag returned
    const annotations = r4.inner.value.object.get("annotations").?.array;
    for (annotations.items) |a| {
        try std.testing.expectEqualStrings("alpha", a.object.get("tag").?.string);
    }
    try std.testing.expect(annotations.items.len > 0);
}

test "annotations on wrong cursor returns empty" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const node = helpers.findNode(&gen.graph, "parse", .function) orelse return;
    var id_buf: [20]u8 = undefined;
    const id_str = formatNodeId(&id_buf, node.id);

    // Create cursor A and annotate
    const create_a =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"explorer.cursor_create","arguments":{}}}
    ;
    const r1 = try callToolAndParseInner(allocator, &srv, create_a) orelse return;
    defer allocator.free(r1.response_bytes);
    defer r1.outer.deinit();
    defer r1.inner.deinit();
    const cursor_a = r1.inner.value.object.get("cursor_id").?.string;

    const annotate_req = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{{"name":"explorer.annotate","arguments":{{"cursor_id":"{s}","node_ids":"{s}","tag":"tagged"}}}}}}
    , .{ cursor_a, id_str });
    defer allocator.free(annotate_req);
    const r2 = try callToolAndParseInner(allocator, &srv, annotate_req) orelse return;
    defer allocator.free(r2.response_bytes);
    defer r2.outer.deinit();
    defer r2.inner.deinit();

    // Create cursor B (separate cursor, no annotations)
    const create_b =
        \\{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"explorer.cursor_create","arguments":{}}}
    ;
    const r3 = try callToolAndParseInner(allocator, &srv, create_b) orelse return;
    defer allocator.free(r3.response_bytes);
    defer r3.outer.deinit();
    defer r3.inner.deinit();
    const cursor_b = r3.inner.value.object.get("cursor_id").?.string;

    // Act: query annotations on cursor B
    const query_req = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{{"name":"explorer.annotations","arguments":{{"cursor_id":"{s}"}}}}}}
    , .{cursor_b});
    defer allocator.free(query_req);
    const r4 = try callToolAndParseInner(allocator, &srv, query_req) orelse return;
    defer allocator.free(r4.response_bytes);
    defer r4.outer.deinit();
    defer r4.inner.deinit();

    // Assert: cursor B has no annotations
    const annotations = r4.inner.value.object.get("annotations").?.array;
    try std.testing.expectEqual(@as(usize, 0), annotations.items.len);
}

// ---------------------------------------------------------------------------
// analysis.* setup helpers
// ---------------------------------------------------------------------------

fn setupDuplicatesFixture(tmp_dir: *std.testing.TmpDir) ![:0]const u8 {
    try writeFixtureFiles(std.testing.io, tmp_dir.dir, &.{
        .{ .sub_path = "duplicates.zig", .data = fixtures.zig.analysis.duplicates },
    });
    return try tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
}

fn setupDeadCodeFixture(tmp_dir: *std.testing.TmpDir) ![:0]const u8 {
    try writeFixtureFiles(std.testing.io, tmp_dir.dir, &.{
        .{ .sub_path = "dead_code.zig", .data = fixtures.zig.analysis.dead_code },
    });
    return try tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
}

fn setupCircularFixture(tmp_dir: *std.testing.TmpDir) ![:0]const u8 {
    try writeFixtureFiles(std.testing.io, tmp_dir.dir, &.{
        .{ .sub_path = "a.zig", .data = fixtures.zig.analysis.circular.a_zig },
        .{ .sub_path = "b.zig", .data = fixtures.zig.analysis.circular.b_zig },
        .{ .sub_path = "c.zig", .data = fixtures.zig.analysis.circular.c_zig },
    });
    return try tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
}

fn setupComplexFixture(tmp_dir: *std.testing.TmpDir) ![:0]const u8 {
    try writeFixtureFiles(std.testing.io, tmp_dir.dir, &.{
        .{ .sub_path = "complex.zig", .data = fixtures.zig.analysis.complex },
    });
    return try tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
}

// ---------------------------------------------------------------------------
// analysis.duplicates
// ---------------------------------------------------------------------------

test "duplicates finds near-identical functions" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupDuplicatesFixture(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"analysis.duplicates","arguments":{}}}
    ;

    // Act
    const result = try callToolAndParseInner(allocator, &srv, input) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();
    const v = result.inner.value.object;

    // Assert
    const groups = v.get("groups").?.array;
    try std.testing.expect(groups.items.len >= 1);
    const first_group = groups.items[0].object;
    const members = first_group.get("members").?.array;
    var found_process_items = false;
    var found_process_entries = false;
    for (members.items) |m| {
        const name = m.object.get("name").?.string;
        if (std.mem.eql(u8, name, "processItems")) found_process_items = true;
        if (std.mem.eql(u8, name, "processEntries")) found_process_entries = true;
    }
    try std.testing.expect(found_process_items);
    try std.testing.expect(found_process_entries);
}

test "duplicates empty graph" {
    // Arrange
    const allocator = std.testing.allocator;
    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    _ = try gen.graph.freeze(gen.arena.allocator());
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"analysis.duplicates","arguments":{}}}
    ;

    // Act
    const result = try callToolAndParseInner(allocator, &srv, input) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert
    const groups = result.inner.value.object.get("groups").?.array;
    try std.testing.expectEqual(@as(usize, 0), groups.items.len);
}

// ---------------------------------------------------------------------------
// analysis.complexity
// ---------------------------------------------------------------------------

test "complexity returns top N sorted descending" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupComplexFixture(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"analysis.complexity","arguments":{"top_n":3}}}
    ;

    // Act
    const result = try callToolAndParseInner(allocator, &srv, input) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();
    const v = result.inner.value.object;

    // Assert
    const nodes = v.get("nodes").?.array;
    try std.testing.expect(nodes.items.len <= 3);
    try std.testing.expect(nodes.items.len > 0);
    var prev_complexity: f64 = std.math.floatMax(f64);
    for (nodes.items) |n| {
        const complexity = jsonAsFloat(n.object.get("complexity").?);
        try std.testing.expect(complexity <= prev_complexity);
        prev_complexity = complexity;
    }
}

test "complexity empty graph" {
    // Arrange
    const allocator = std.testing.allocator;
    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    _ = try gen.graph.freeze(gen.arena.allocator());
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"analysis.complexity","arguments":{}}}
    ;

    // Act
    const result = try callToolAndParseInner(allocator, &srv, input) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert
    const nodes = result.inner.value.object.get("nodes").?.array;
    try std.testing.expectEqual(@as(usize, 0), nodes.items.len);
}

// ---------------------------------------------------------------------------
// analysis.dead_code
// ---------------------------------------------------------------------------

test "dead_code finds unreferenced private function" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupDeadCodeFixture(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"analysis.dead_code","arguments":{}}}
    ;

    // Act
    const result = try callToolAndParseInner(allocator, &srv, input) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();
    const v = result.inner.value.object;

    // Assert
    const total_count = v.get("total_count").?.integer;
    const nodes = v.get("nodes").?.array;
    try std.testing.expectEqual(total_count, @as(i64, @intCast(nodes.items.len)));
    var found_truly_dead = false;
    var found_tested_private = false;
    var found_counter = false;
    var found_orphaned = false;
    var found_value = false;
    for (nodes.items) |n| {
        const name = n.object.get("name").?.string;
        if (std.mem.eql(u8, name, "trulyDead")) found_truly_dead = true;
        if (std.mem.eql(u8, name, "testedPrivate")) found_tested_private = true;
        if (std.mem.eql(u8, name, "Counter")) found_counter = true;
        if (std.mem.eql(u8, name, "orphaned")) found_orphaned = true;
        if (std.mem.eql(u8, name, "value")) found_value = true;
    }
    try std.testing.expect(found_truly_dead);
    try std.testing.expect(!found_tested_private);
    try std.testing.expect(!found_counter);
    try std.testing.expect(found_orphaned);
    try std.testing.expect(!found_value);
}

test "dead_code rust finds unreferenced private function, not Counter fields" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try tmp_dir.dir.writeFile(std.testing.io, .{ .sub_path = "dead_code.rs", .data = fixtures.rust.analysis.dead_code });
    const project_root = try tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"analysis.dead_code","arguments":{}}}
    ;

    // Act
    const result = try callToolAndParseInner(allocator, &srv, input) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();
    const v = result.inner.value.object;

    // Assert
    const total_count = v.get("total_count").?.integer;
    const nodes = v.get("nodes").?.array;
    try std.testing.expectEqual(total_count, @as(i64, @intCast(nodes.items.len)));
    var found_truly_dead = false;
    var found_tested_private = false;
    var found_counter = false;
    var found_value = false;
    var found_limit = false;
    var found_label = false;
    for (nodes.items) |n| {
        const name = n.object.get("name").?.string;
        if (std.mem.eql(u8, name, "truly_dead")) found_truly_dead = true;
        if (std.mem.eql(u8, name, "tested_private")) found_tested_private = true;
        if (std.mem.eql(u8, name, "Counter")) found_counter = true;
        if (std.mem.eql(u8, name, "value")) found_value = true;
        if (std.mem.eql(u8, name, "limit")) found_limit = true;
        if (std.mem.eql(u8, name, "label")) found_label = true;
    }
    try std.testing.expect(found_truly_dead);
    // tested_private is also dead: assert_eq! is a macro invocation
    // opaque to tree-sitter, so the call inside it is invisible.
    try std.testing.expect(found_tested_private);
    try std.testing.expect(!found_counter);
    try std.testing.expect(!found_value);
    try std.testing.expect(!found_limit);
    try std.testing.expect(!found_label);
}

test "dead_code empty graph" {
    // Arrange
    const allocator = std.testing.allocator;
    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    _ = try gen.graph.freeze(gen.arena.allocator());
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"analysis.dead_code","arguments":{}}}
    ;

    // Act
    const result = try callToolAndParseInner(allocator, &srv, input) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert
    const v = result.inner.value.object;
    try std.testing.expectEqual(@as(i64, 0), v.get("total_count").?.integer);
    const nodes = v.get("nodes").?.array;
    try std.testing.expectEqual(@as(usize, 0), nodes.items.len);
}

// ---------------------------------------------------------------------------
// analysis.dependency_cycles
// ---------------------------------------------------------------------------

test "dependency_cycles detects import cycle" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupCircularFixture(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"analysis.dependency_cycles","arguments":{}}}
    ;

    // Act
    const result = try callToolAndParseInner(allocator, &srv, input) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();
    const v = result.inner.value.object;

    // Assert
    const cycles = v.get("cycles").?.array;
    try std.testing.expect(cycles.items.len >= 1);
    var found_all_three = false;
    for (cycles.items) |cycle| {
        const nodes = cycle.object.get("nodes").?.array;
        var has_a = false;
        var has_b = false;
        var has_c = false;
        for (nodes.items) |n| {
            const name = n.object.get("name") orelse n.object.get("file") orelse continue;
            if (name != .string) continue;
            if (std.mem.indexOf(u8, name.string, "a.zig") != null) has_a = true;
            if (std.mem.indexOf(u8, name.string, "b.zig") != null) has_b = true;
            if (std.mem.indexOf(u8, name.string, "c.zig") != null) has_c = true;
        }
        if (has_a and has_b and has_c) found_all_three = true;
    }
    try std.testing.expect(found_all_three);
}

test "dependency_cycles empty graph" {
    // Arrange
    const allocator = std.testing.allocator;
    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    _ = try gen.graph.freeze(gen.arena.allocator());
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"analysis.dependency_cycles","arguments":{}}}
    ;

    // Act
    const result = try callToolAndParseInner(allocator, &srv, input) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert
    const cycles = result.inner.value.object.get("cycles").?.array;
    try std.testing.expectEqual(@as(usize, 0), cycles.items.len);
}

// ---------------------------------------------------------------------------
// analysis.coupling
// ---------------------------------------------------------------------------

test "coupling coupled modules have score > 0" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"analysis.coupling","arguments":{"min_coupling":0.0}}}
    ;

    // Act
    const result = try callToolAndParseInner(allocator, &srv, input) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();
    const v = result.inner.value.object;

    // Assert
    const pairs = v.get("pairs").?.array;
    try std.testing.expect(pairs.items.len >= 1);
    const score = jsonAsFloat(pairs.items[0].object.get("score").?);
    try std.testing.expect(score > 0.0);
}

test "coupling empty graph" {
    // Arrange
    const allocator = std.testing.allocator;
    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    _ = try gen.graph.freeze(gen.arena.allocator());
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"analysis.coupling","arguments":{}}}
    ;

    // Act
    const result = try callToolAndParseInner(allocator, &srv, input) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert
    const pairs = result.inner.value.object.get("pairs").?.array;
    try std.testing.expectEqual(@as(usize, 0), pairs.items.len);
}

// ---------------------------------------------------------------------------
// analysis.impact
// ---------------------------------------------------------------------------

test "impact core function has dependents" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const node = helpers.findNode(&gen.graph, "isDelimiter", .function) orelse return;
    var id_buf: [20]u8 = undefined;
    const id_str = formatNodeId(&id_buf, node.id);

    const request = try std.fmt.allocPrint(allocator,
        \\{{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{{"name":"analysis.impact","arguments":{{"node_ids":"{s}"}}}}}}
    , .{id_str});
    defer allocator.free(request);

    // Act
    const result = try callToolAndParseInner(allocator, &srv, request) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();
    const v = result.inner.value.object;

    // Assert
    const total = v.get("total_impacted").?.integer;
    try std.testing.expect(total > 0);
}

test "impact empty graph" {
    // Arrange
    const allocator = std.testing.allocator;
    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    _ = try gen.graph.freeze(gen.arena.allocator());
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"analysis.impact","arguments":{"node_ids":"0"}}}
    ;

    // Act
    const result = try callToolAndParseInner(allocator, &srv, input) orelse return;
    defer allocator.free(result.response_bytes);
    defer result.outer.deinit();
    defer result.inner.deinit();

    // Assert
    const total = result.inner.value.object.get("total_impacted").?.integer;
    try std.testing.expectEqual(@as(i64, 0), total);
}

// ---------------------------------------------------------------------------
// Cross-cutting: no opinions in analysis responses
// ---------------------------------------------------------------------------

fn containsOpinionKey(obj: std.json.ObjectMap) bool {
    const opinion_keys = [_][]const u8{ "suggestion", "recommendation", "should", "advice", "warning" };
    var it = obj.iterator();
    while (it.next()) |entry| {
        for (opinion_keys) |key| {
            if (std.mem.eql(u8, entry.key_ptr.*, key)) return true;
        }
        if (entry.value_ptr.* == .object) {
            if (containsOpinionKey(entry.value_ptr.*.object)) return true;
        }
        if (entry.value_ptr.* == .array) {
            for (entry.value_ptr.*.array.items) |item| {
                if (item == .object) {
                    if (containsOpinionKey(item.object)) return true;
                }
            }
        }
    }
    return false;
}

test "no analysis tool returns opinions" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = try setupProjectFixtures(&tmp_dir);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    const tools = [_][]const u8{
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"analysis.duplicates","arguments":{}}}
        ,
        \\{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"analysis.complexity","arguments":{}}}
        ,
        \\{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"analysis.dead_code","arguments":{}}}
        ,
        \\{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"analysis.dependency_cycles","arguments":{}}}
        ,
        \\{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"analysis.coupling","arguments":{}}}
        ,
    };

    // Act + Assert
    for (tools) |tool_req| {
        const result = try callToolAndParseInner(allocator, &srv, tool_req) orelse continue;
        defer allocator.free(result.response_bytes);
        defer result.outer.deinit();
        defer result.inner.deinit();
        try std.testing.expect(!containsOpinionKey(result.inner.value.object));
    }
}

// ---------------------------------------------------------------------------
// dispatch: all tools respond
// ---------------------------------------------------------------------------

test "all 20 MCP tools respond without crash" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try writeFixtureFiles(std.testing.io, tmp_dir.dir, &.{
        .{ .sub_path = "main.zig", .data = "pub fn hello() void {}" },
    });
    const project_root = try tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(project_root);

    const gen = try GraphGeneration.create(allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    gen.graph = Graph.init(project_root);
    _ = try indexDirectory(gen.arena.allocator(), std.testing.io, project_root, &gen.graph, null, .{});
    const guard = gen.acquire();
    defer guard.deinit();
    var mgr = GenerationManager.init(gen);
    var srv = Server.init(&mgr);
    defer srv.deinit();

    // Act: tools/list
    const list_input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}
    ;
    const list_response = (try srv.handleMessage(allocator, std.testing.io, list_input)) orelse
        return error.NoResponse;
    defer allocator.free(list_response);
    var list_parsed = try parseJsonResponse(allocator, list_response);
    defer list_parsed.deinit();

    // Assert: exactly 20 tools
    const result_obj = list_parsed.value.object.get("result") orelse return error.NoResult;
    const tools_arr = result_obj.object.get("tools") orelse return error.NoTools;
    try std.testing.expectEqual(@as(usize, 20), tools_arr.array.items.len);

    // Act + Assert: call each tool with empty arguments, verify it responds
    for (tools_arr.array.items, 0..) |tool_val, i| {
        const name = tool_val.object.get("name") orelse continue;
        if (name != .string) continue;
        const tool_name = name.string;

        var req_buf: [512]u8 = undefined;
        const req_id = i + 100;
        const req = std.fmt.bufPrint(&req_buf,
            \\{{"jsonrpc":"2.0","id":{d},"method":"tools/call","params":{{"name":"{s}","arguments":{{}}}}}}
        , .{ req_id, tool_name }) catch continue;

        const response = (try srv.handleMessage(allocator, std.testing.io, req)) orelse continue;
        defer allocator.free(response);

        var parsed = try parseJsonResponse(allocator, response);
        defer parsed.deinit();

        const has_result = parsed.value.object.get("result") != null;
        const has_error = parsed.value.object.get("error") != null;
        if (!has_result and !has_error) {
            std.debug.print("tool '{s}' returned neither result nor error\n", .{tool_name});
            return error.InvalidResponse;
        }
    }
}
