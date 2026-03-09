const std = @import("std");
const zcodeprism = @import("zcodeprism");

const jsonrpc = zcodeprism.mcp.jsonrpc;
const server_mod = zcodeprism.mcp.server;
const generation_mod = zcodeprism.generation;

const Server = server_mod.Server;
const GraphGeneration = generation_mod.GraphGeneration;

fn makeTestServer(gen: *GraphGeneration) Server {
    return Server.init(gen);
}

fn parseJsonResponse(allocator: std.mem.Allocator, bytes: []const u8) !std.json.Parsed(std.json.Value) {
    return std.json.parseFromSlice(std.json.Value, allocator, bytes, .{});
}

// ---------------------------------------------------------------
// JSON-RPC parsing
// ---------------------------------------------------------------

test "parses valid request" {
    // Arrange
    const allocator = std.testing.allocator;
    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{"foo":"bar"}}
    ;

    // Act
    var parsed = try jsonrpc.parseRequest(allocator, input);
    defer parsed.deinit();
    const req = parsed.value;

    // Assert
    try std.testing.expectEqual(jsonrpc.RequestId{ .integer = 1 }, req.id);
    try std.testing.expectEqualStrings("tools/list", req.method);
    try std.testing.expect(req.params != null);
}

test "parses request without params" {
    // Arrange
    const allocator = std.testing.allocator;
    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/list"}
    ;

    // Act
    var parsed = try jsonrpc.parseRequest(allocator, input);
    defer parsed.deinit();
    const req = parsed.value;

    // Assert
    try std.testing.expectEqual(jsonrpc.RequestId{ .integer = 1 }, req.id);
    try std.testing.expectEqualStrings("tools/list", req.method);
    try std.testing.expect(req.params == null);
}

test "rejects malformed JSON" {
    // Arrange
    const allocator = std.testing.allocator;
    var gen = GraphGeneration.init(allocator, 1, "abcdef123456".*);
    gen.acquire();
    defer gen.release();
    var srv = makeTestServer(&gen);
    defer srv.deinit();

    // Act
    const response_bytes = try srv.handleMessage(allocator, "{{{");
    defer if (response_bytes) |b| allocator.free(b);

    // Assert
    try std.testing.expect(response_bytes != null);
    const parsed = try parseJsonResponse(allocator, response_bytes.?);
    defer parsed.deinit();
    const err_obj = parsed.value.object.get("error").?;
    try std.testing.expectEqual(@as(i64, jsonrpc.parse_error), err_obj.object.get("code").?.integer);
}

test "rejects missing jsonrpc field" {
    // Arrange
    const allocator = std.testing.allocator;
    var gen = GraphGeneration.init(allocator, 1, "abcdef123456".*);
    gen.acquire();
    defer gen.release();
    var srv = makeTestServer(&gen);
    defer srv.deinit();
    const input =
        \\{"id":1,"method":"foo"}
    ;

    // Act
    const response_bytes = try srv.handleMessage(allocator, input);
    defer if (response_bytes) |b| allocator.free(b);

    // Assert
    try std.testing.expect(response_bytes != null);
    const parsed = try parseJsonResponse(allocator, response_bytes.?);
    defer parsed.deinit();
    const err_obj = parsed.value.object.get("error").?;
    try std.testing.expectEqual(@as(i64, jsonrpc.invalid_request), err_obj.object.get("code").?.integer);
}

test "rejects missing method" {
    // Arrange
    const allocator = std.testing.allocator;
    var gen = GraphGeneration.init(allocator, 1, "abcdef123456".*);
    gen.acquire();
    defer gen.release();
    var srv = makeTestServer(&gen);
    defer srv.deinit();
    const input =
        \\{"jsonrpc":"2.0","id":1}
    ;

    // Act
    const response_bytes = try srv.handleMessage(allocator, input);
    defer if (response_bytes) |b| allocator.free(b);

    // Assert
    try std.testing.expect(response_bytes != null);
    const parsed = try parseJsonResponse(allocator, response_bytes.?);
    defer parsed.deinit();
    const err_obj = parsed.value.object.get("error").?;
    try std.testing.expectEqual(@as(i64, jsonrpc.invalid_request), err_obj.object.get("code").?.integer);
}

test "rejects unknown method" {
    // Arrange
    const allocator = std.testing.allocator;
    var gen = GraphGeneration.init(allocator, 1, "abcdef123456".*);
    gen.acquire();
    defer gen.release();
    var srv = makeTestServer(&gen);
    defer srv.deinit();
    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"nonexistent"}
    ;

    // Act
    const response_bytes = try srv.handleMessage(allocator, input);
    defer if (response_bytes) |b| allocator.free(b);

    // Assert
    try std.testing.expect(response_bytes != null);
    const parsed = try parseJsonResponse(allocator, response_bytes.?);
    defer parsed.deinit();
    const err_obj = parsed.value.object.get("error").?;
    try std.testing.expectEqual(@as(i64, jsonrpc.method_not_found), err_obj.object.get("code").?.integer);
}

test "handles string id" {
    // Arrange
    const allocator = std.testing.allocator;
    var gen = GraphGeneration.init(allocator, 1, "abcdef123456".*);
    gen.acquire();
    defer gen.release();
    var srv = makeTestServer(&gen);
    defer srv.deinit();
    const input =
        \\{"jsonrpc":"2.0","id":"abc","method":"tools/list"}
    ;

    // Act
    const response_bytes = try srv.handleMessage(allocator, input);
    defer if (response_bytes) |b| allocator.free(b);

    // Assert
    try std.testing.expect(response_bytes != null);
    const parsed = try parseJsonResponse(allocator, response_bytes.?);
    defer parsed.deinit();
    const id_val = parsed.value.object.get("id").?;
    try std.testing.expectEqualStrings("abc", id_val.string);
}

test "handles integer id" {
    // Arrange
    const allocator = std.testing.allocator;
    var gen = GraphGeneration.init(allocator, 1, "abcdef123456".*);
    gen.acquire();
    defer gen.release();
    var srv = makeTestServer(&gen);
    defer srv.deinit();
    const input =
        \\{"jsonrpc":"2.0","id":42,"method":"tools/list"}
    ;

    // Act
    const response_bytes = try srv.handleMessage(allocator, input);
    defer if (response_bytes) |b| allocator.free(b);

    // Assert
    try std.testing.expect(response_bytes != null);
    const parsed = try parseJsonResponse(allocator, response_bytes.?);
    defer parsed.deinit();
    const id_val = parsed.value.object.get("id").?;
    try std.testing.expectEqual(@as(i64, 42), id_val.integer);
}

test "handles null id (notification)" {
    // Arrange
    const allocator = std.testing.allocator;
    var gen = GraphGeneration.init(allocator, 1, "abcdef123456".*);
    gen.acquire();
    defer gen.release();
    var srv = makeTestServer(&gen);
    defer srv.deinit();
    const input =
        \\{"jsonrpc":"2.0","method":"notifications/cancelled","params":{}}
    ;

    // Act
    const response_bytes = try srv.handleMessage(allocator, input);

    // Assert: notifications produce no response
    try std.testing.expect(response_bytes == null);
}

// ---------------------------------------------------------------
// tools/list
// ---------------------------------------------------------------

test "tools list returns exactly 20 tools" {
    // Arrange
    const allocator = std.testing.allocator;
    var gen = GraphGeneration.init(allocator, 1, "abcdef123456".*);
    gen.acquire();
    defer gen.release();
    var srv = makeTestServer(&gen);
    defer srv.deinit();
    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/list"}
    ;

    // Act
    const response_bytes = (try srv.handleMessage(allocator, input)).?;
    defer allocator.free(response_bytes);
    const parsed = try parseJsonResponse(allocator, response_bytes);
    defer parsed.deinit();

    // Assert
    const result = parsed.value.object.get("result").?;
    const tools = result.object.get("tools").?.array;
    try std.testing.expectEqual(@as(usize, 20), tools.items.len);
}

test "tools list has 6 graph tools" {
    // Arrange
    const allocator = std.testing.allocator;
    var gen = GraphGeneration.init(allocator, 1, "abcdef123456".*);
    gen.acquire();
    defer gen.release();
    var srv = makeTestServer(&gen);
    defer srv.deinit();
    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/list"}
    ;

    // Act
    const response_bytes = (try srv.handleMessage(allocator, input)).?;
    defer allocator.free(response_bytes);
    const parsed = try parseJsonResponse(allocator, response_bytes);
    defer parsed.deinit();

    // Assert
    const tools = parsed.value.object.get("result").?.object.get("tools").?.array;
    var count: usize = 0;
    for (tools.items) |tool| {
        const name = tool.object.get("name").?.string;
        if (std.mem.startsWith(u8, name, "graph.")) count += 1;
    }
    try std.testing.expectEqual(@as(usize, 6), count);
}

test "tools list has 8 explorer tools" {
    // Arrange
    const allocator = std.testing.allocator;
    var gen = GraphGeneration.init(allocator, 1, "abcdef123456".*);
    gen.acquire();
    defer gen.release();
    var srv = makeTestServer(&gen);
    defer srv.deinit();
    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/list"}
    ;

    // Act
    const response_bytes = (try srv.handleMessage(allocator, input)).?;
    defer allocator.free(response_bytes);
    const parsed = try parseJsonResponse(allocator, response_bytes);
    defer parsed.deinit();

    // Assert
    const tools = parsed.value.object.get("result").?.object.get("tools").?.array;
    var count: usize = 0;
    for (tools.items) |tool| {
        const name = tool.object.get("name").?.string;
        if (std.mem.startsWith(u8, name, "explorer.")) count += 1;
    }
    try std.testing.expectEqual(@as(usize, 8), count);
}

test "tools list has 6 analysis tools" {
    // Arrange
    const allocator = std.testing.allocator;
    var gen = GraphGeneration.init(allocator, 1, "abcdef123456".*);
    gen.acquire();
    defer gen.release();
    var srv = makeTestServer(&gen);
    defer srv.deinit();
    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/list"}
    ;

    // Act
    const response_bytes = (try srv.handleMessage(allocator, input)).?;
    defer allocator.free(response_bytes);
    const parsed = try parseJsonResponse(allocator, response_bytes);
    defer parsed.deinit();

    // Assert
    const tools = parsed.value.object.get("result").?.object.get("tools").?.array;
    var count: usize = 0;
    for (tools.items) |tool| {
        const name = tool.object.get("name").?.string;
        if (std.mem.startsWith(u8, name, "analysis.")) count += 1;
    }
    try std.testing.expectEqual(@as(usize, 6), count);
}

test "each tool has name and inputSchema" {
    // Arrange
    const allocator = std.testing.allocator;
    var gen = GraphGeneration.init(allocator, 1, "abcdef123456".*);
    gen.acquire();
    defer gen.release();
    var srv = makeTestServer(&gen);
    defer srv.deinit();
    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/list"}
    ;

    // Act
    const response_bytes = (try srv.handleMessage(allocator, input)).?;
    defer allocator.free(response_bytes);
    const parsed = try parseJsonResponse(allocator, response_bytes);
    defer parsed.deinit();

    // Assert
    const tools = parsed.value.object.get("result").?.object.get("tools").?.array;
    for (tools.items) |tool| {
        try std.testing.expect(tool.object.get("name") != null);
        try std.testing.expect(tool.object.get("inputSchema") != null);
    }
}

// ---------------------------------------------------------------
// initialize
// ---------------------------------------------------------------

test "initialize returns serverInfo" {
    // Arrange
    const allocator = std.testing.allocator;
    var gen = GraphGeneration.init(allocator, 1, "abcdef123456".*);
    gen.acquire();
    defer gen.release();
    var srv = makeTestServer(&gen);
    defer srv.deinit();
    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test","version":"0.1.0"}}}
    ;

    // Act
    const response_bytes = (try srv.handleMessage(allocator, input)).?;
    defer allocator.free(response_bytes);
    const parsed = try parseJsonResponse(allocator, response_bytes);
    defer parsed.deinit();

    // Assert
    const result = parsed.value.object.get("result").?;
    const info = result.object.get("serverInfo").?;
    try std.testing.expect(info.object.get("name") != null);
    try std.testing.expect(info.object.get("version") != null);
}

test "initialize returns capabilities" {
    // Arrange
    const allocator = std.testing.allocator;
    var gen = GraphGeneration.init(allocator, 1, "abcdef123456".*);
    gen.acquire();
    defer gen.release();
    var srv = makeTestServer(&gen);
    defer srv.deinit();
    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test","version":"0.1.0"}}}
    ;

    // Act
    const response_bytes = (try srv.handleMessage(allocator, input)).?;
    defer allocator.free(response_bytes);
    const parsed = try parseJsonResponse(allocator, response_bytes);
    defer parsed.deinit();

    // Assert
    const result = parsed.value.object.get("result").?;
    try std.testing.expect(result.object.get("capabilities") != null);
}

// ---------------------------------------------------------------
// Server lifecycle
// ---------------------------------------------------------------

test "server acquires generation on request" {
    // Arrange
    const allocator = std.testing.allocator;
    var gen = GraphGeneration.init(allocator, 1, "abcdef123456".*);
    gen.acquire();
    defer gen.release();
    var srv = makeTestServer(&gen);
    defer srv.deinit();
    const before = gen.ref_count.load(.monotonic);
    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/list"}
    ;

    // Act
    const response_bytes = try srv.handleMessage(allocator, input);
    defer if (response_bytes) |b| allocator.free(b);

    // Assert: ref_count unchanged means acquire+release paired correctly
    const after = gen.ref_count.load(.monotonic);
    try std.testing.expectEqual(before, after);
}

test "server releases generation after response" {
    // Arrange
    const allocator = std.testing.allocator;
    var gen = GraphGeneration.init(allocator, 1, "abcdef123456".*);
    gen.acquire();
    defer gen.release();
    var srv = makeTestServer(&gen);
    defer srv.deinit();
    const baseline = gen.ref_count.load(.monotonic);

    // Act: send multiple requests
    const input =
        \\{"jsonrpc":"2.0","id":1,"method":"tools/list"}
    ;
    const r1 = try srv.handleMessage(allocator, input);
    defer if (r1) |b| allocator.free(b);
    const r2 = try srv.handleMessage(allocator, input);
    defer if (r2) |b| allocator.free(b);
    const r3 = try srv.handleMessage(allocator, input);
    defer if (r3) |b| allocator.free(b);

    // Assert: ref_count stable after all requests
    const after = gen.ref_count.load(.monotonic);
    try std.testing.expectEqual(baseline, after);
}
