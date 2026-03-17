//! LSP client: manages child process lifecycle, message framing, and
//! JSON-RPC request/response exchange with an LSP server.

const std = @import("std");
const protocol = @import("protocol.zig");
const logging = @import("../logging.zig");

const Logger = logging.Logger;
const Field = logging.Field;
const Value = std.json.Value;
const ObjectMap = std.json.ObjectMap;

pub const HeaderError = error{
    InvalidHeader,
};

/// Counters and timing from a completed warmup drain pass.
pub const WarmupResult = struct {
    elapsed_ms: u64,
    notifications_drained: usize,
    /// True when a readiness notification arrived before the timeout expired.
    ready_signal: bool,
};

/// An LSP client that communicates with a language server over stdio.
pub const LspClient = struct {
    next_request_id: i64 = 1,
    child: ?std.process.Child = null,
    logger: Logger,

    pub fn init(logger: Logger) LspClient {
        return .{ .logger = logger };
    }

    pub fn deinit(self: *LspClient) void {
        if (self.child != null) self.stop();
    }

    /// Spawn the LSP server as a child process with piped stdio.
    pub fn start(self: *LspClient, allocator: std.mem.Allocator, server_command: []const u8) !void {
        var child = std.process.Child.init(&.{server_command}, allocator);
        child.stdin_behavior = .Pipe;
        child.stdout_behavior = .Pipe;
        child.stderr_behavior = .Ignore;
        try child.spawn();
        self.child = child;
    }

    /// Shut down the LSP server gracefully, then reap the process.
    pub fn stop(self: *LspClient) void {
        var child = self.child orelse return;
        self.child = null;

        if (child.stdin) |stdin| {
            const shutdown = "{\"jsonrpc\":\"2.0\",\"id\":999999,\"method\":\"shutdown\",\"params\":null}";
            const s_hdr = std.fmt.comptimePrint("Content-Length: {d}\r\n\r\n", .{shutdown.len});
            stdin.writeAll(s_hdr) catch {};
            stdin.writeAll(shutdown) catch {};

            const exit_msg = "{\"jsonrpc\":\"2.0\",\"method\":\"exit\",\"params\":null}";
            const e_hdr = std.fmt.comptimePrint("Content-Length: {d}\r\n\r\n", .{exit_msg.len});
            stdin.writeAll(e_hdr) catch {};
            stdin.writeAll(exit_msg) catch {};
        }

        _ = child.wait() catch {};
    }

    /// Write a Content-Length framed message to the server stdin.
    fn sendFramed(self: *LspClient, data: []const u8) !void {
        const child = &(self.child orelse return error.BrokenPipe);
        const stdin = child.stdin orelse return error.BrokenPipe;
        var hdr_buf: [64]u8 = undefined;
        const hdr = std.fmt.bufPrint(&hdr_buf, "Content-Length: {d}\r\n\r\n", .{data.len}) catch
            return error.BrokenPipe;
        try stdin.writeAll(hdr);
        try stdin.writeAll(data);
    }

    /// Read one Content-Length framed message body from stdout.
    fn readFramedBody(self: *LspClient, allocator: std.mem.Allocator) ![]const u8 {
        const child = &(self.child orelse return error.BrokenPipe);
        const stdout = child.stdout orelse return error.BrokenPipe;

        var hdr_buf: [512]u8 = undefined;
        var hdr_len: usize = 0;
        while (hdr_len < hdr_buf.len) {
            const n = try stdout.read(hdr_buf[hdr_len .. hdr_len + 1]);
            if (n == 0) return error.EndOfStream;
            hdr_len += 1;
            if (hdr_len >= 4 and std.mem.eql(u8, hdr_buf[hdr_len - 4 .. hdr_len], "\r\n\r\n")) break;
        }

        const content_length = try parseContentLength(hdr_buf[0..hdr_len]);
        const body = try allocator.alloc(u8, content_length);
        errdefer allocator.free(body);

        var total: usize = 0;
        while (total < content_length) {
            const n = try stdout.read(body[total..]);
            if (n == 0) return error.EndOfStream;
            total += n;
        }
        return body;
    }

    /// Scan a header block for the Content-Length value.
    pub fn parseContentLength(header: []const u8) HeaderError!usize {
        const needle = "Content-Length: ";
        var pos: usize = 0;
        while (pos + needle.len <= header.len) {
            if (std.mem.startsWith(u8, header[pos..], needle)) {
                const digits_start = pos + needle.len;
                const line_end = std.mem.indexOfPos(u8, header, digits_start, "\r\n") orelse
                    return HeaderError.InvalidHeader;
                return std.fmt.parseInt(usize, header[digits_start..line_end], 10) catch
                    return HeaderError.InvalidHeader;
            }
            if (std.mem.indexOfPos(u8, header, pos, "\r\n")) |eol| {
                pos = eol + 2;
            } else break;
        }
        return HeaderError.InvalidHeader;
    }

    /// Read responses until one with a non-null id arrives, skipping
    /// server-initiated notifications.
    fn readResponse(self: *LspClient, allocator: std.mem.Allocator) !protocol.Response {
        var skipped: usize = 0;
        while (skipped < 64) : (skipped += 1) {
            const body = try self.readFramedBody(allocator);
            defer allocator.free(body);

            const resp = protocol.parseResponse(allocator, body) catch continue;
            if (resp.id != null) return resp;

            resp.deinit(allocator);
        }
        return error.BrokenPipe;
    }

    /// Drain server-initiated notifications until a readiness signal arrives or
    /// the timeout expires. Polls stdout before each read to avoid blocking.
    /// Recognizes `$/progress` with `params.value.kind == "end"` (WorkDoneProgressEnd)
    /// and `textDocument/publishDiagnostics` as readiness signals.
    pub fn drainNotifications(
        self: *LspClient,
        allocator: std.mem.Allocator,
        timeout_ms: u32,
        logger: Logger,
    ) WarmupResult {
        const child = &(self.child orelse return .{
            .elapsed_ms = 0,
            .notifications_drained = 0,
            .ready_signal = false,
        });
        const stdout = child.stdout orelse return .{
            .elapsed_ms = 0,
            .notifications_drained = 0,
            .ready_signal = false,
        };

        var timer = std.time.Timer.start() catch return .{
            .elapsed_ms = 0,
            .notifications_drained = 0,
            .ready_signal = false,
        };
        var drained: usize = 0;

        while (true) {
            const elapsed_ms = timer.read() / std.time.ns_per_ms;
            if (elapsed_ms >= timeout_ms) break;

            const remaining: i32 = @intCast(timeout_ms - elapsed_ms);
            if (!pollAvailable(stdout.handle, remaining)) break;

            const body = self.readFramedBody(allocator) catch break;
            defer allocator.free(body);

            drained += 1;
            logger.trace("LSP warmup: received notification", &.{
                Field.uint("n", @as(u64, drained)),
            });

            if (isReadyNotification(body)) {
                return .{
                    .elapsed_ms = timer.read() / std.time.ns_per_ms,
                    .notifications_drained = drained,
                    .ready_signal = true,
                };
            }
        }

        return .{
            .elapsed_ms = timer.read() / std.time.ns_per_ms,
            .notifications_drained = drained,
            .ready_signal = false,
        };
    }

    fn nextId(self: *LspClient) i64 {
        const id = self.next_request_id;
        self.next_request_id += 1;
        return id;
    }

    /// Serialize a std.json.Value to an allocator-owned byte slice.
    fn serializeValue(allocator: std.mem.Allocator, value: Value) error{OutOfMemory}![]const u8 {
        var aw: std.io.Writer.Allocating = .init(allocator);
        errdefer aw.deinit();
        var stream: std.json.Stringify = .{ .writer = &aw.writer };
        stream.write(value) catch return error.OutOfMemory;
        return aw.toOwnedSlice() catch return error.OutOfMemory;
    }

    /// Build a JSON-RPC request envelope and serialize it.
    fn buildRequest(self: *LspClient, allocator: std.mem.Allocator, arena: std.mem.Allocator, method: []const u8, params: Value) error{OutOfMemory}![]const u8 {
        var obj = ObjectMap.init(arena);
        obj.put("jsonrpc", .{ .string = "2.0" }) catch return error.OutOfMemory;
        obj.put("id", .{ .integer = self.nextId() }) catch return error.OutOfMemory;
        obj.put("method", .{ .string = method }) catch return error.OutOfMemory;
        obj.put("params", params) catch return error.OutOfMemory;
        return serializeValue(allocator, .{ .object = obj });
    }

    /// Build a JSON-RPC notification envelope and serialize it.
    fn buildNotification(allocator: std.mem.Allocator, arena: std.mem.Allocator, method: []const u8, params: Value) error{OutOfMemory}![]const u8 {
        var obj = ObjectMap.init(arena);
        obj.put("jsonrpc", .{ .string = "2.0" }) catch return error.OutOfMemory;
        obj.put("method", .{ .string = method }) catch return error.OutOfMemory;
        obj.put("params", params) catch return error.OutOfMemory;
        return serializeValue(allocator, .{ .object = obj });
    }

    /// Run the initialize/initialized handshake.
    pub fn initialize(self: *LspClient, allocator: std.mem.Allocator, project_root: []const u8) !void {
        const root_uri = try pathToUri(allocator, project_root);
        defer allocator.free(root_uri);

        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        const a = arena.allocator();

        const caps = ObjectMap.init(a);
        var params = ObjectMap.init(a);
        try params.put("capabilities", .{ .object = caps });
        try params.put("rootUri", .{ .string = root_uri });
        try params.put("processId", .null);

        const req = try self.buildRequest(allocator, a, "initialize", .{ .object = params });
        defer allocator.free(req);
        try self.sendFramed(req);

        const resp = try self.readResponse(allocator);
        resp.deinit(allocator);

        const empty = ObjectMap.init(a);
        const notif = try buildNotification(allocator, a, "initialized", .{ .object = empty });
        defer allocator.free(notif);
        self.sendFramed(notif) catch {};
    }

    /// Notify the server that a document was opened.
    pub fn textDocumentDidOpen(self: *LspClient, allocator: std.mem.Allocator, uri: []const u8, text: []const u8, language_id: []const u8) !void {
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        const a = arena.allocator();

        var td = ObjectMap.init(a);
        try td.put("uri", .{ .string = uri });
        try td.put("languageId", .{ .string = language_id });
        try td.put("version", .{ .integer = 1 });
        try td.put("text", .{ .string = text });

        var params = ObjectMap.init(a);
        try params.put("textDocument", .{ .object = td });

        const notif = try buildNotification(allocator, a, "textDocument/didOpen", .{ .object = params });
        defer allocator.free(notif);
        self.sendFramed(notif) catch {};
    }

    /// Query hover information at a position.
    pub fn textDocumentHover(self: *LspClient, allocator: std.mem.Allocator, uri: []const u8, line: u32, character: u32) !?protocol.Hover {
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();

        const params = try buildPositionParams(arena.allocator(), uri, line, character);

        const req = try self.buildRequest(allocator, arena.allocator(), "textDocument/hover", .{ .object = params });
        defer allocator.free(req);
        try self.sendFramed(req);

        const resp = try self.readResponse(allocator);
        defer resp.deinit(allocator);

        if (resp.@"error" != null) return null;
        const raw = resp.result_raw orelse return null;

        return protocol.parseHover(allocator, raw) catch null;
    }

    /// Query definition locations at a position.
    /// Caller owns the returned slice; free with protocol.freeLocationArray.
    pub fn textDocumentDefinition(self: *LspClient, allocator: std.mem.Allocator, uri: []const u8, line: u32, character: u32) !?[]protocol.Location {
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();

        const params = try buildPositionParams(arena.allocator(), uri, line, character);

        const req = try self.buildRequest(allocator, arena.allocator(), "textDocument/definition", .{ .object = params });
        defer allocator.free(req);
        try self.sendFramed(req);

        const resp = try self.readResponse(allocator);
        defer resp.deinit(allocator);

        if (resp.@"error" != null) return null;
        const raw = resp.result_raw orelse return null;

        if (protocol.parseLocationArray(allocator, raw)) |locs| return locs else |_| {}
        if (protocol.parseLocation(allocator, raw)) |loc| {
            const locs = try allocator.alloc(protocol.Location, 1);
            locs[0] = loc;
            return locs;
        } else |_| {}

        return null;
    }

    /// Query type definition locations at a position.
    /// Servers that do not support this method return null rather than an error.
    /// Caller owns the returned slice; free with protocol.freeLocationArray.
    pub fn textDocumentTypeDefinition(self: *LspClient, allocator: std.mem.Allocator, uri: []const u8, line: u32, character: u32) !?[]protocol.Location {
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();

        const params = try buildPositionParams(arena.allocator(), uri, line, character);

        const req = try self.buildRequest(allocator, arena.allocator(), "textDocument/typeDefinition", .{ .object = params });
        defer allocator.free(req);
        try self.sendFramed(req);

        const resp = try self.readResponse(allocator);
        defer resp.deinit(allocator);

        if (resp.@"error" != null) return null;
        const raw = resp.result_raw orelse return null;

        if (protocol.parseLocationArray(allocator, raw)) |locs| return locs else |_| {}
        if (protocol.parseLocation(allocator, raw)) |loc| {
            const locs = try allocator.alloc(protocol.Location, 1);
            locs[0] = loc;
            return locs;
        } else |_| {}

        return null;
    }

    /// Find all references to the symbol at a position.
    /// `include_declaration` controls whether the declaration site is included.
    /// Caller owns the returned slice; free with protocol.freeLocationArray.
    pub fn textDocumentReferences(self: *LspClient, allocator: std.mem.Allocator, uri: []const u8, line: u32, character: u32, include_declaration: bool) !?[]protocol.Location {
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        const a = arena.allocator();

        var params = try buildPositionParams(a, uri, line, character);

        var ctx = ObjectMap.init(a);
        try ctx.put("includeDeclaration", .{ .bool = include_declaration });
        try params.put("context", .{ .object = ctx });

        const req = try self.buildRequest(allocator, a, "textDocument/references", .{ .object = params });
        defer allocator.free(req);
        try self.sendFramed(req);

        const resp = try self.readResponse(allocator);
        defer resp.deinit(allocator);

        if (resp.@"error" != null) return null;
        const raw = resp.result_raw orelse return null;

        return protocol.parseLocationArray(allocator, raw) catch null;
    }

    /// Build TextDocumentPositionParams as an ObjectMap.
    fn buildPositionParams(arena: std.mem.Allocator, uri: []const u8, line: u32, character: u32) !ObjectMap {
        var td = ObjectMap.init(arena);
        try td.put("uri", .{ .string = uri });

        var pos = ObjectMap.init(arena);
        try pos.put("line", .{ .integer = @intCast(line) });
        try pos.put("character", .{ .integer = @intCast(character) });

        var params = ObjectMap.init(arena);
        try params.put("textDocument", .{ .object = td });
        try params.put("position", .{ .object = pos });

        return params;
    }
};

/// Convert a filesystem path to a file:// URI.
pub fn pathToUri(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    const prefix = "file://";
    const buf = try allocator.alloc(u8, prefix.len + path.len);
    @memcpy(buf[0..prefix.len], prefix);
    @memcpy(buf[prefix.len..], path);
    return buf;
}

/// Extract the filesystem path from a file:// URI (slice into the URI).
pub fn uriToPath(uri: []const u8) ?[]const u8 {
    const prefix = "file://";
    if (std.mem.startsWith(u8, uri, prefix)) return uri[prefix.len..];
    return null;
}

/// True if the fd has data available to read within timeout_ms milliseconds.
fn pollAvailable(fd: std.posix.fd_t, timeout_ms: i32) bool {
    var fds = [1]std.posix.pollfd{.{
        .fd = fd,
        .events = std.posix.POLL.IN,
        .revents = 0,
    }};
    const n = std.posix.poll(&fds, timeout_ms) catch return false;
    return n > 0 and fds[0].revents & std.posix.POLL.IN != 0;
}

/// True if body is a `$/progress` WorkDoneProgressEnd or `textDocument/publishDiagnostics` notification.
fn isReadyNotification(body: []const u8) bool {
    if (std.mem.indexOf(u8, body, "\"$/progress\"") != null and
        std.mem.indexOf(u8, body, "\"end\"") != null)
        return true;
    return std.mem.indexOf(u8, body, "\"textDocument/publishDiagnostics\"") != null;
}

test "buildRequest serializes correct JSON-RPC envelope" {
    // Arrange
    var client = LspClient.init(Logger.noop);
    defer client.deinit();
    const allocator = std.testing.allocator;

    // Act: build two sequential requests with empty params
    var arena1 = std.heap.ArenaAllocator.init(allocator);
    defer arena1.deinit();
    const empty1 = ObjectMap.init(arena1.allocator());
    const req1 = try client.buildRequest(allocator, arena1.allocator(), "textDocument/definition", .{ .object = empty1 });
    defer allocator.free(req1);

    var arena2 = std.heap.ArenaAllocator.init(allocator);
    defer arena2.deinit();
    const empty2 = ObjectMap.init(arena2.allocator());
    const req2 = try client.buildRequest(allocator, arena2.allocator(), "textDocument/references", .{ .object = empty2 });
    defer allocator.free(req2);

    // Assert
    try std.testing.expect(std.mem.indexOf(u8, req1, "\"2.0\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, req1, "textDocument/definition") != null);
    try std.testing.expect(std.mem.indexOf(u8, req2, "textDocument/references") != null);
    try std.testing.expect(std.mem.indexOf(u8, req1, "\"id\":1") != null);
    try std.testing.expect(std.mem.indexOf(u8, req2, "\"id\":2") != null);
}

test "parseContentLength parses headers" {
    // Single-line header
    const len = try LspClient.parseContentLength("Content-Length: 52\r\n\r\n");
    try std.testing.expectEqual(@as(usize, 52), len);

    // Zero length
    const zero = try LspClient.parseContentLength("Content-Length: 0\r\n\r\n");
    try std.testing.expectEqual(@as(usize, 0), zero);

    // Multi-line header with Content-Type first
    const multi = try LspClient.parseContentLength("Content-Type: application/vscode-jsonrpc; charset=utf-8\r\nContent-Length: 128\r\n\r\n");
    try std.testing.expectEqual(@as(usize, 128), multi);

    // Missing Content-Length
    try std.testing.expectError(HeaderError.InvalidHeader, LspClient.parseContentLength("Bad-Header: 52\r\n\r\n"));
}

test "request method names for typeDefinition" {
    // Arrange
    var client = LspClient.init(Logger.noop);
    defer client.deinit();
    const allocator = std.testing.allocator;

    var arena_td = std.heap.ArenaAllocator.init(allocator);
    defer arena_td.deinit();
    const empty_td = ObjectMap.init(arena_td.allocator());

    // Act
    const req_td = try client.buildRequest(allocator, arena_td.allocator(), "textDocument/typeDefinition", .{ .object = empty_td });
    defer allocator.free(req_td);

    // Assert
    try std.testing.expect(std.mem.indexOf(u8, req_td, "textDocument/typeDefinition") != null);
}

test "references request includes context with includeDeclaration" {
    // Arrange
    var client = LspClient.init(Logger.noop);
    defer client.deinit();
    const allocator = std.testing.allocator;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();

    // Construct the same params that textDocumentReferences would build.
    var td = ObjectMap.init(a);
    try td.put("uri", .{ .string = "file:///src/main.zig" });
    var pos = ObjectMap.init(a);
    try pos.put("line", .{ .integer = 10 });
    try pos.put("character", .{ .integer = 4 });
    var params = ObjectMap.init(a);
    try params.put("textDocument", .{ .object = td });
    try params.put("position", .{ .object = pos });
    var ctx = ObjectMap.init(a);
    try ctx.put("includeDeclaration", .{ .bool = false });
    try params.put("context", .{ .object = ctx });

    // Act
    const req = try client.buildRequest(allocator, a, "textDocument/references", .{ .object = params });
    defer allocator.free(req);

    // Assert
    try std.testing.expect(std.mem.indexOf(u8, req, "textDocument/references") != null);
    try std.testing.expect(std.mem.indexOf(u8, req, "includeDeclaration") != null);
    try std.testing.expect(std.mem.indexOf(u8, req, "false") != null);
}
