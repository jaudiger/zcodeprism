const std = @import("std");
const generation_mod = @import("../core/generation.zig");
const cursor_manager_mod = @import("../explorer/cursor_manager.zig");
const dispatcher_mod = @import("dispatcher.zig");
const handlers = @import("handlers.zig");
const jsonrpc = @import("jsonrpc.zig");
const protocol = @import("protocol.zig");

const GraphGeneration = generation_mod.GraphGeneration;
const CursorManager = cursor_manager_mod.CursorManager;
const Dispatcher = dispatcher_mod.Dispatcher;

/// Errors that the MCP server can surface to callers.
pub const ServerError = error{
    OutOfMemory,
};

/// MCP server that processes JSON-RPC messages against a graph generation.
pub const Server = struct {
    generation: *GraphGeneration,
    dispatcher: Dispatcher,
    cursor_manager: CursorManager,

    /// Create a server bound to a graph generation.
    pub fn init(gen: *GraphGeneration) Server {
        return .{
            .generation = gen,
            .dispatcher = Dispatcher.init(),
            .cursor_manager = CursorManager.init(),
        };
    }

    /// Release server resources.
    pub fn deinit(self: *Server) void {
        self.dispatcher.deinit();
        self.cursor_manager.deinit();
    }

    /// Process a single JSON-RPC message. Returns response bytes,
    /// or null for notifications. Caller owns returned slice.
    pub fn handleMessage(self: *Server, allocator: std.mem.Allocator, input: []const u8) ServerError!?[]const u8 {
        var parsed = jsonrpc.parseRequest(allocator, input) catch |err| switch (err) {
            error.InvalidJson => return try buildErrorResponse(allocator, .none, jsonrpc.parse_error, "Parse error"),
            error.InvalidRequest => return try buildErrorResponse(allocator, .none, jsonrpc.invalid_request, "Invalid Request"),
        };
        defer parsed.deinit();
        const req = parsed.value;

        if (req.id == .none) return null;

        self.generation.acquire();
        defer self.generation.release();

        if (std.mem.eql(u8, req.method, "initialize")) {
            return try buildSuccessResponse(allocator, req.id, protocol.InitializeResult{});
        } else if (std.mem.eql(u8, req.method, "tools/list")) {
            return try buildSuccessResponse(allocator, req.id, protocol.ToolsListResult{
                .tools = self.dispatcher.listTools(),
            });
        } else if (std.mem.eql(u8, req.method, "tools/call")) {
            return try self.handleToolCall(allocator, req);
        } else {
            return try buildErrorResponse(allocator, req.id, jsonrpc.method_not_found, "Method not found");
        }
    }

    fn handleToolCall(self: *Server, allocator: std.mem.Allocator, req: jsonrpc.Request) ServerError![]const u8 {
        const params_obj = if (req.params) |p| (if (p == .object) p.object else null) else null;
        const tool_name = if (params_obj) |obj| (if (obj.get("name")) |v| (if (v == .string) v.string else null) else null) else null;

        if (tool_name == null) {
            return try buildErrorResponse(allocator, req.id, jsonrpc.invalid_params, "Missing tool name");
        }

        const result_json = handlers.handleToolCall(allocator, self.generation, &self.cursor_manager, tool_name.?, req.params) catch
            return try buildErrorResponse(allocator, req.id, jsonrpc.internal_error, "Handler error");

        if (result_json) |json| {
            defer allocator.free(json);
            // Parse the result JSON and embed it in the response
            var result_parsed = std.json.parseFromSlice(std.json.Value, allocator, json, .{}) catch
                return try buildErrorResponse(allocator, req.id, jsonrpc.internal_error, "JSON parse error");
            defer result_parsed.deinit();
            return try buildSuccessResponse(allocator, req.id, result_parsed.value);
        }

        return try buildErrorResponse(allocator, req.id, jsonrpc.method_not_found, "Unknown tool");
    }

    fn writeId(stream: *std.json.Stringify, id: jsonrpc.RequestId) ServerError!void {
        switch (id) {
            .integer => |n| stream.write(n) catch return error.OutOfMemory,
            .string => |s| stream.write(s) catch return error.OutOfMemory,
            .none => stream.write(null) catch return error.OutOfMemory,
        }
    }

    fn buildSuccessResponse(allocator: std.mem.Allocator, id: jsonrpc.RequestId, result: anytype) ServerError![]const u8 {
        var aw: std.io.Writer.Allocating = .init(allocator);
        errdefer aw.deinit();

        var stream: std.json.Stringify = .{ .writer = &aw.writer };
        stream.beginObject() catch return error.OutOfMemory;
        stream.objectField("jsonrpc") catch return error.OutOfMemory;
        stream.write(protocol.jsonrpc_version) catch return error.OutOfMemory;
        stream.objectField("id") catch return error.OutOfMemory;
        try writeId(&stream, id);
        stream.objectField("result") catch return error.OutOfMemory;
        stream.write(result) catch return error.OutOfMemory;
        stream.endObject() catch return error.OutOfMemory;

        return aw.toOwnedSlice() catch return error.OutOfMemory;
    }

    fn buildErrorResponse(allocator: std.mem.Allocator, id: jsonrpc.RequestId, code: i32, message: []const u8) ServerError![]const u8 {
        var aw: std.io.Writer.Allocating = .init(allocator);
        errdefer aw.deinit();

        var stream: std.json.Stringify = .{ .writer = &aw.writer };
        stream.beginObject() catch return error.OutOfMemory;
        stream.objectField("jsonrpc") catch return error.OutOfMemory;
        stream.write(protocol.jsonrpc_version) catch return error.OutOfMemory;
        stream.objectField("id") catch return error.OutOfMemory;
        try writeId(&stream, id);
        stream.objectField("error") catch return error.OutOfMemory;
        stream.beginObject() catch return error.OutOfMemory;
        stream.objectField("code") catch return error.OutOfMemory;
        stream.write(code) catch return error.OutOfMemory;
        stream.objectField("message") catch return error.OutOfMemory;
        stream.write(message) catch return error.OutOfMemory;
        stream.endObject() catch return error.OutOfMemory;
        stream.endObject() catch return error.OutOfMemory;

        return aw.toOwnedSlice() catch return error.OutOfMemory;
    }
};
