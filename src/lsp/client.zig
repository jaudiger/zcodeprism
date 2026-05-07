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

/// How long stop() waits for the server to close stdout before killing it.
const shutdown_timeout_ms: u32 = 2000;

/// An LSP client that communicates with a language server over stdio.
pub const LspClient = struct {
    allocator: std.mem.Allocator,
    next_request_id: i64 = 1,
    child: ?std.process.Child = null,
    logger: Logger,
    /// Scratch arena for per-request params, envelopes, and serialized bytes.
    /// Reset at the start of each public request method.
    request_arena: std.heap.ArenaAllocator,

    pub fn init(allocator: std.mem.Allocator, logger: Logger) LspClient {
        return .{
            .allocator = allocator,
            .logger = logger,
            .request_arena = std.heap.ArenaAllocator.init(allocator),
        };
    }

    pub fn deinit(self: *LspClient, io: std.Io) void {
        if (self.child != null) self.stop(io);
        self.request_arena.deinit();
    }

    /// Spawn the LSP server as a child process with piped stdio.
    pub fn start(self: *LspClient, io: std.Io, server_command: []const u8) !void {
        const child = try std.process.spawn(io, .{
            .argv = &.{server_command},
            .stdin = .pipe,
            .stdout = .pipe,
            .stderr = .ignore,
        });
        self.child = child;
    }

    /// Shut down the LSP server gracefully, then reap the process. Force-kills
    /// if the server does not close stdout within shutdown_timeout_ms.
    pub fn stop(self: *LspClient, io: std.Io) void {
        var child = self.child orelse return;
        self.child = null;

        sendShutdownExit(io, &child);

        if (child.stdin) |stdin| {
            stdin.close(io);
            child.stdin = null;
        }

        if (drainUntilEof(io, &child, shutdown_timeout_ms)) {
            _ = child.wait(io) catch {};
        } else {
            child.kill(io);
        }
    }

    /// Write a Content-Length framed message to the server stdin.
    fn sendFramed(self: *LspClient, io: std.Io, data: []const u8) !void {
        const child = &(self.child orelse return error.BrokenPipe);
        const stdin = child.stdin orelse return error.BrokenPipe;
        var hdr_buf: [64]u8 = undefined;
        const hdr = std.fmt.bufPrint(&hdr_buf, "Content-Length: {d}\r\n\r\n", .{data.len}) catch
            return error.BrokenPipe;
        try stdin.writeStreamingAll(io, hdr);
        try stdin.writeStreamingAll(io, data);
    }

    /// Read one Content-Length framed message body from stdout.
    fn readFramedBody(self: *LspClient, allocator: std.mem.Allocator, io: std.Io) ![]const u8 {
        const child = &(self.child orelse return error.BrokenPipe);
        const stdout = child.stdout orelse return error.BrokenPipe;

        var hdr_buf: [512]u8 = undefined;
        var hdr_len: usize = 0;
        while (hdr_len < hdr_buf.len) {
            const n = try stdout.readStreaming(io, &.{hdr_buf[hdr_len .. hdr_len + 1]});
            if (n == 0) return error.EndOfStream;
            hdr_len += 1;
            if (hdr_len >= 4 and std.mem.eql(u8, hdr_buf[hdr_len - 4 .. hdr_len], "\r\n\r\n")) break;
        }

        const content_length = try parseContentLength(hdr_buf[0..hdr_len]);
        const body = try allocator.alloc(u8, content_length);
        errdefer allocator.free(body);

        var total: usize = 0;
        while (total < content_length) {
            const n = try stdout.readStreaming(io, &.{body[total..]});
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

    /// Read responses until one matching expected_id arrives, discarding
    /// server-initiated notifications and responses for other ids.
    fn readResponseFor(self: *LspClient, allocator: std.mem.Allocator, io: std.Io, expected_id: i64) !protocol.Response {
        var skipped: usize = 0;
        while (skipped < 64) : (skipped += 1) {
            const body = try self.readFramedBody(allocator, io);
            defer allocator.free(body);

            var resp = protocol.parseResponse(allocator, body) catch continue;
            errdefer resp.deinit(allocator);

            if (resp.id == expected_id) return resp;
            resp.deinit(allocator);
        }
        return error.BrokenPipe;
    }

    /// Drain server-initiated notifications until a readiness signal arrives or
    /// the timeout expires. Recognizes `$/progress` WorkDoneProgressEnd and
    /// `textDocument/publishDiagnostics` as readiness signals.
    pub fn drainNotifications(
        self: *LspClient,
        allocator: std.mem.Allocator,
        io: std.Io,
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

        const start_ts = std.Io.Timestamp.now(io, .awake);
        var drained: usize = 0;

        while (true) {
            const now_ts = std.Io.Timestamp.now(io, .awake);
            const elapsed_ms: u64 = @intCast(@max(0, @divTrunc(now_ts.nanoseconds - start_ts.nanoseconds, std.time.ns_per_ms)));
            if (elapsed_ms >= timeout_ms) break;

            const remaining: i32 = @intCast(timeout_ms - elapsed_ms);
            if (!pollAvailable(stdout.handle, remaining)) break;

            const body = self.readFramedBody(allocator, io) catch break;
            defer allocator.free(body);

            drained += 1;
            logger.trace("LSP warmup: received notification", &.{
                Field.uint("n", @as(u64, drained)),
            });

            if (isReadyNotification(body)) {
                const end_ts = std.Io.Timestamp.now(io, .awake);
                return .{
                    .elapsed_ms = @intCast(@max(0, @divTrunc(end_ts.nanoseconds - start_ts.nanoseconds, std.time.ns_per_ms))),
                    .notifications_drained = drained,
                    .ready_signal = true,
                };
            }
        }

        const end_ts = std.Io.Timestamp.now(io, .awake);
        return .{
            .elapsed_ms = @intCast(@max(0, @divTrunc(end_ts.nanoseconds - start_ts.nanoseconds, std.time.ns_per_ms))),
            .notifications_drained = drained,
            .ready_signal = false,
        };
    }

    fn nextId(self: *LspClient) i64 {
        const id = self.next_request_id;
        self.next_request_id += 1;
        return id;
    }

    /// Serialize a std.json.Value to an owned slice.
    fn serializeValue(arena: std.mem.Allocator, value: Value) error{OutOfMemory}![]const u8 {
        var aw: std.Io.Writer.Allocating = .init(arena);
        var stream: std.json.Stringify = .{ .writer = &aw.writer };
        stream.write(value) catch return error.OutOfMemory;
        return aw.toOwnedSlice() catch return error.OutOfMemory;
    }

    /// Build a JSON-RPC request envelope.
    fn buildRequest(self: *LspClient, id: i64, method: []const u8, params: Value) error{OutOfMemory}![]const u8 {
        const a = self.request_arena.allocator();
        var obj: ObjectMap = .empty;
        try obj.put(a, "jsonrpc", .{ .string = "2.0" });
        try obj.put(a, "id", .{ .integer = id });
        try obj.put(a, "method", .{ .string = method });
        try obj.put(a, "params", params);
        return serializeValue(a, .{ .object = obj });
    }

    /// Build a JSON-RPC notification envelope.
    fn buildNotification(self: *LspClient, method: []const u8, params: Value) error{OutOfMemory}![]const u8 {
        const a = self.request_arena.allocator();
        var obj: ObjectMap = .empty;
        try obj.put(a, "jsonrpc", .{ .string = "2.0" });
        try obj.put(a, "method", .{ .string = method });
        try obj.put(a, "params", params);
        return serializeValue(a, .{ .object = obj });
    }

    /// Run the initialize/initialized handshake.
    pub fn initialize(self: *LspClient, allocator: std.mem.Allocator, io: std.Io, project_root: []const u8, init_options: ?[]const u8) !void {
        _ = self.request_arena.reset(.retain_capacity);
        const a = self.request_arena.allocator();

        const root_uri = try pathToUri(allocator, project_root);
        defer allocator.free(root_uri);

        const caps: ObjectMap = .empty;
        var params: ObjectMap = .empty;
        try params.put(a, "capabilities", .{ .object = caps });
        try params.put(a, "rootUri", .{ .string = root_uri });
        try params.put(a, "processId", .null);

        if (init_options) |opts_json| {
            if (std.json.parseFromSlice(Value, a, opts_json, .{})) |parsed| {
                try params.put(a, "initializationOptions", parsed.value);
            } else |_| {}
        }

        const id = self.nextId();
        const req = try self.buildRequest(id, "initialize", .{ .object = params });
        try self.sendFramed(io, req);

        const resp = try self.readResponseFor(allocator, io, id);
        resp.deinit(allocator);

        const empty: ObjectMap = .empty;
        const notif = try self.buildNotification("initialized", .{ .object = empty });
        self.sendFramed(io, notif) catch {};
    }

    /// Notify the server that a document was opened.
    pub fn textDocumentDidOpen(self: *LspClient, allocator: std.mem.Allocator, io: std.Io, uri: []const u8, text: []const u8, language_id: []const u8) !void {
        _ = self.request_arena.reset(.retain_capacity);
        const a = self.request_arena.allocator();

        var td: ObjectMap = .empty;
        try td.put(a, "uri", .{ .string = uri });
        try td.put(a, "languageId", .{ .string = language_id });
        try td.put(a, "version", .{ .integer = 1 });
        try td.put(a, "text", .{ .string = text });

        var params: ObjectMap = .empty;
        try params.put(a, "textDocument", .{ .object = td });

        const notif = try self.buildNotification("textDocument/didOpen", .{ .object = params });
        self.sendFramed(io, notif) catch {};
        _ = allocator;
    }

    /// Notify the server that a document's content changed (full replacement).
    pub fn textDocumentDidChange(self: *LspClient, allocator: std.mem.Allocator, io: std.Io, uri: []const u8, version: i32, new_text: []const u8) !void {
        _ = self.request_arena.reset(.retain_capacity);
        const a = self.request_arena.allocator();

        var td: ObjectMap = .empty;
        try td.put(a, "uri", .{ .string = uri });
        try td.put(a, "version", .{ .integer = @as(i64, version) });

        var change: ObjectMap = .empty;
        try change.put(a, "text", .{ .string = new_text });

        var changes = std.json.Array.init(a);
        try changes.append(.{ .object = change });

        var params: ObjectMap = .empty;
        try params.put(a, "textDocument", .{ .object = td });
        try params.put(a, "contentChanges", .{ .array = changes });

        const notif = try self.buildNotification("textDocument/didChange", .{ .object = params });
        self.sendFramed(io, notif) catch {};
        _ = allocator;
    }

    /// Notify the server that a document was closed.
    pub fn textDocumentDidClose(self: *LspClient, allocator: std.mem.Allocator, io: std.Io, uri: []const u8) !void {
        _ = self.request_arena.reset(.retain_capacity);
        const a = self.request_arena.allocator();

        var td: ObjectMap = .empty;
        try td.put(a, "uri", .{ .string = uri });

        var params: ObjectMap = .empty;
        try params.put(a, "textDocument", .{ .object = td });

        const notif = try self.buildNotification("textDocument/didClose", .{ .object = params });
        self.sendFramed(io, notif) catch {};
        _ = allocator;
    }

    /// Query hover information at a position.
    pub fn textDocumentHover(self: *LspClient, allocator: std.mem.Allocator, io: std.Io, uri: []const u8, line: u32, character: u32) !?protocol.Hover {
        _ = self.request_arena.reset(.retain_capacity);
        const a = self.request_arena.allocator();

        const params = try buildPositionParams(a, uri, line, character);

        const id = self.nextId();
        const req = try self.buildRequest(id, "textDocument/hover", .{ .object = params });
        try self.sendFramed(io, req);

        const resp = try self.readResponseFor(allocator, io, id);
        defer resp.deinit(allocator);

        if (resp.@"error" != null) return null;
        const raw = resp.result_raw orelse return null;

        return protocol.parseHover(allocator, raw) catch null;
    }

    /// Query definition locations at a position.
    /// Caller owns the returned slice; free with protocol.freeLocationArray.
    pub fn textDocumentDefinition(self: *LspClient, allocator: std.mem.Allocator, io: std.Io, uri: []const u8, line: u32, character: u32) !?[]protocol.Location {
        _ = self.request_arena.reset(.retain_capacity);
        const a = self.request_arena.allocator();

        const params = try buildPositionParams(a, uri, line, character);

        const id = self.nextId();
        const req = try self.buildRequest(id, "textDocument/definition", .{ .object = params });
        try self.sendFramed(io, req);

        const resp = try self.readResponseFor(allocator, io, id);
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
    pub fn textDocumentTypeDefinition(self: *LspClient, allocator: std.mem.Allocator, io: std.Io, uri: []const u8, line: u32, character: u32) !?[]protocol.Location {
        _ = self.request_arena.reset(.retain_capacity);
        const a = self.request_arena.allocator();

        const params = try buildPositionParams(a, uri, line, character);

        const id = self.nextId();
        const req = try self.buildRequest(id, "textDocument/typeDefinition", .{ .object = params });
        try self.sendFramed(io, req);

        const resp = try self.readResponseFor(allocator, io, id);
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
    pub fn textDocumentReferences(self: *LspClient, allocator: std.mem.Allocator, io: std.Io, uri: []const u8, line: u32, character: u32, include_declaration: bool) !?[]protocol.Location {
        _ = self.request_arena.reset(.retain_capacity);
        const a = self.request_arena.allocator();

        var params = try buildPositionParams(a, uri, line, character);

        var ctx: ObjectMap = .empty;
        try ctx.put(a, "includeDeclaration", .{ .bool = include_declaration });
        try params.put(a, "context", .{ .object = ctx });

        const id = self.nextId();
        const req = try self.buildRequest(id, "textDocument/references", .{ .object = params });
        try self.sendFramed(io, req);

        const resp = try self.readResponseFor(allocator, io, id);
        defer resp.deinit(allocator);

        if (resp.@"error" != null) return null;
        const raw = resp.result_raw orelse return null;

        return protocol.parseLocationArray(allocator, raw) catch null;
    }

    /// Build TextDocumentPositionParams as an ObjectMap.
    fn buildPositionParams(arena: std.mem.Allocator, uri: []const u8, line: u32, character: u32) !ObjectMap {
        var td: ObjectMap = .empty;
        try td.put(arena, "uri", .{ .string = uri });

        var pos: ObjectMap = .empty;
        try pos.put(arena, "line", .{ .integer = @intCast(line) });
        try pos.put(arena, "character", .{ .integer = @intCast(character) });

        var params: ObjectMap = .empty;
        try params.put(arena, "textDocument", .{ .object = td });
        try params.put(arena, "position", .{ .object = pos });

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

/// Send the LSP shutdown request and exit notification; best effort only.
fn sendShutdownExit(io: std.Io, child: *std.process.Child) void {
    const stdin = child.stdin orelse return;
    const shutdown = "{\"jsonrpc\":\"2.0\",\"id\":999999,\"method\":\"shutdown\",\"params\":null}";
    const s_hdr = std.fmt.comptimePrint("Content-Length: {d}\r\n\r\n", .{shutdown.len});
    stdin.writeStreamingAll(io, s_hdr) catch {};
    stdin.writeStreamingAll(io, shutdown) catch {};

    const exit_msg = "{\"jsonrpc\":\"2.0\",\"method\":\"exit\",\"params\":null}";
    const e_hdr = std.fmt.comptimePrint("Content-Length: {d}\r\n\r\n", .{exit_msg.len});
    stdin.writeStreamingAll(io, e_hdr) catch {};
    stdin.writeStreamingAll(io, exit_msg) catch {};
}

/// Read and discard stdout until the server closes it (returns true) or
/// timeout_ms elapses (returns false).
fn drainUntilEof(io: std.Io, child: *std.process.Child, timeout_ms: u32) bool {
    const stdout = child.stdout orelse return true;
    const start_ts = std.Io.Timestamp.now(io, .awake);
    var buf: [4096]u8 = undefined;
    while (true) {
        const now_ts = std.Io.Timestamp.now(io, .awake);
        const elapsed_ms: u64 = @intCast(@max(0, @divTrunc(now_ts.nanoseconds - start_ts.nanoseconds, std.time.ns_per_ms)));
        if (elapsed_ms >= timeout_ms) return false;

        const remaining: i32 = @intCast(timeout_ms - elapsed_ms);
        const poll_ms: i32 = @min(remaining, 50);
        if (!pollAvailable(stdout.handle, poll_ms)) continue;

        const n = stdout.readStreaming(io, &.{&buf}) catch return true;
        if (n == 0) return true;
    }
}

test "buildRequest serializes correct JSON-RPC envelope" {
    // Arrange
    const allocator = std.testing.allocator;
    var client = LspClient.init(allocator, Logger.noop);
    defer client.deinit(std.testing.io);

    // Act: build two sequential requests with empty params
    const empty1: ObjectMap = .empty;
    const id1 = client.nextId();
    const req1 = try client.buildRequest(id1, "textDocument/definition", .{ .object = empty1 });
    const owned1 = try allocator.dupe(u8, req1);
    defer allocator.free(owned1);

    const empty2: ObjectMap = .empty;
    const id2 = client.nextId();
    const req2 = try client.buildRequest(id2, "textDocument/references", .{ .object = empty2 });
    const owned2 = try allocator.dupe(u8, req2);
    defer allocator.free(owned2);

    // Assert
    try std.testing.expect(std.mem.indexOf(u8, owned1, "\"2.0\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, owned1, "textDocument/definition") != null);
    try std.testing.expect(std.mem.indexOf(u8, owned2, "textDocument/references") != null);
    try std.testing.expect(std.mem.indexOf(u8, owned1, "\"id\":1") != null);
    try std.testing.expect(std.mem.indexOf(u8, owned2, "\"id\":2") != null);
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
    const allocator = std.testing.allocator;
    var client = LspClient.init(allocator, Logger.noop);
    defer client.deinit(std.testing.io);

    const empty_td: ObjectMap = .empty;

    // Act
    const id = client.nextId();
    const req_td = try client.buildRequest(id, "textDocument/typeDefinition", .{ .object = empty_td });
    const owned = try allocator.dupe(u8, req_td);
    defer allocator.free(owned);

    // Assert
    try std.testing.expect(std.mem.indexOf(u8, owned, "textDocument/typeDefinition") != null);
}

test "references request includes context with includeDeclaration" {
    // Arrange
    const allocator = std.testing.allocator;
    var client = LspClient.init(allocator, Logger.noop);
    defer client.deinit(std.testing.io);

    const a = client.request_arena.allocator();

    // Construct the same params that textDocumentReferences would build.
    var td: ObjectMap = .empty;
    try td.put(a, "uri", .{ .string = "file:///src/main.zig" });
    var pos: ObjectMap = .empty;
    try pos.put(a, "line", .{ .integer = 10 });
    try pos.put(a, "character", .{ .integer = 4 });
    var params: ObjectMap = .empty;
    try params.put(a, "textDocument", .{ .object = td });
    try params.put(a, "position", .{ .object = pos });
    var ctx: ObjectMap = .empty;
    try ctx.put(a, "includeDeclaration", .{ .bool = false });
    try params.put(a, "context", .{ .object = ctx });

    // Act
    const id = client.nextId();
    const req = try client.buildRequest(id, "textDocument/references", .{ .object = params });
    const owned = try allocator.dupe(u8, req);
    defer allocator.free(owned);

    // Assert
    try std.testing.expect(std.mem.indexOf(u8, owned, "textDocument/references") != null);
    try std.testing.expect(std.mem.indexOf(u8, owned, "includeDeclaration") != null);
    try std.testing.expect(std.mem.indexOf(u8, owned, "false") != null);
}

test "textDocumentDidChange builds correct notification" {
    // Arrange
    const allocator = std.testing.allocator;
    var client = LspClient.init(allocator, Logger.noop);
    defer client.deinit(std.testing.io);

    const a = client.request_arena.allocator();

    // Build the same params that textDocumentDidChange would build.
    var td: ObjectMap = .empty;
    try td.put(a, "uri", .{ .string = "file:///src/main.zig" });
    try td.put(a, "version", .{ .integer = 2 });

    var change: ObjectMap = .empty;
    try change.put(a, "text", .{ .string = "const x = 42;\n" });

    var changes = std.json.Array.init(a);
    try changes.append(.{ .object = change });

    var params: ObjectMap = .empty;
    try params.put(a, "textDocument", .{ .object = td });
    try params.put(a, "contentChanges", .{ .array = changes });

    // Act
    const notif = try client.buildNotification("textDocument/didChange", .{ .object = params });
    const owned = try allocator.dupe(u8, notif);
    defer allocator.free(owned);

    // Assert
    try std.testing.expect(std.mem.indexOf(u8, owned, "textDocument/didChange") != null);
    try std.testing.expect(std.mem.indexOf(u8, owned, "\"version\":2") != null);
    try std.testing.expect(std.mem.indexOf(u8, owned, "contentChanges") != null);
    try std.testing.expect(std.mem.indexOf(u8, owned, "const x = 42;") != null);
}

test "textDocumentDidClose builds correct notification" {
    // Arrange
    const allocator = std.testing.allocator;
    var client = LspClient.init(allocator, Logger.noop);
    defer client.deinit(std.testing.io);

    const a = client.request_arena.allocator();

    var td: ObjectMap = .empty;
    try td.put(a, "uri", .{ .string = "file:///src/lib.zig" });

    var params: ObjectMap = .empty;
    try params.put(a, "textDocument", .{ .object = td });

    // Act
    const notif = try client.buildNotification("textDocument/didClose", .{ .object = params });
    const owned = try allocator.dupe(u8, notif);
    defer allocator.free(owned);

    // Assert
    try std.testing.expect(std.mem.indexOf(u8, owned, "textDocument/didClose") != null);
    try std.testing.expect(std.mem.indexOf(u8, owned, "file:///src/lib.zig") != null);
}
