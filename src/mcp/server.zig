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
        } else if (std.mem.eql(u8, req.method, "ping")) {
            return try buildSuccessResponse(allocator, req.id, struct {}{});
        } else if (std.mem.eql(u8, req.method, "tools/list")) {
            return try buildToolsListResponse(allocator, req.id, self.dispatcher.listTools());
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

        const content_json = handlers.handleToolCall(allocator, self.generation, &self.cursor_manager, tool_name.?, req.params) catch
            return try buildErrorResponse(allocator, req.id, jsonrpc.internal_error, "Handler error");

        if (content_json) |json| {
            defer allocator.free(json);
            return try buildToolCallResponse(allocator, req.id, json);
        }

        return try buildErrorResponse(allocator, req.id, jsonrpc.method_not_found, "Unknown tool");
    }

    /// Wraps raw handler JSON into the full JSON-RPC + MCP content envelope.
    fn buildToolCallResponse(allocator: std.mem.Allocator, id: jsonrpc.RequestId, raw_content_json: []const u8) ServerError![]const u8 {
        var aw: std.io.Writer.Allocating = .init(allocator);
        errdefer aw.deinit();
        var stream: std.json.Stringify = .{ .writer = &aw.writer };

        stream.beginObject() catch return error.OutOfMemory;
        stream.objectField("jsonrpc") catch return error.OutOfMemory;
        stream.write(protocol.jsonrpc_version) catch return error.OutOfMemory;
        stream.objectField("id") catch return error.OutOfMemory;
        try writeId(&stream, id);
        stream.objectField("result") catch return error.OutOfMemory;
        stream.beginObject() catch return error.OutOfMemory;
        stream.objectField("content") catch return error.OutOfMemory;
        stream.beginArray() catch return error.OutOfMemory;
        stream.beginObject() catch return error.OutOfMemory;
        stream.objectField("type") catch return error.OutOfMemory;
        stream.write("text") catch return error.OutOfMemory;
        stream.objectField("text") catch return error.OutOfMemory;
        stream.write(raw_content_json) catch return error.OutOfMemory;
        stream.endObject() catch return error.OutOfMemory;
        stream.endArray() catch return error.OutOfMemory;
        stream.endObject() catch return error.OutOfMemory;
        stream.endObject() catch return error.OutOfMemory;

        return aw.toOwnedSlice() catch return error.OutOfMemory;
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

    /// Build the tools/list response with full JSON Schema for each tool.
    fn buildToolsListResponse(allocator: std.mem.Allocator, id: jsonrpc.RequestId, tools: []const protocol.Tool) ServerError![]const u8 {
        var aw: std.io.Writer.Allocating = .init(allocator);
        errdefer aw.deinit();

        var s: std.json.Stringify = .{ .writer = &aw.writer };
        s.beginObject() catch return error.OutOfMemory;
        s.objectField("jsonrpc") catch return error.OutOfMemory;
        s.write(protocol.jsonrpc_version) catch return error.OutOfMemory;
        s.objectField("id") catch return error.OutOfMemory;
        try writeId(&s, id);
        s.objectField("result") catch return error.OutOfMemory;
        s.beginObject() catch return error.OutOfMemory;
        s.objectField("tools") catch return error.OutOfMemory;
        s.beginArray() catch return error.OutOfMemory;

        for (tools) |tool| {
            s.beginObject() catch return error.OutOfMemory;

            s.objectField("name") catch return error.OutOfMemory;
            s.write(tool.name) catch return error.OutOfMemory;

            s.objectField("title") catch return error.OutOfMemory;
            s.write(tool.title) catch return error.OutOfMemory;

            s.objectField("description") catch return error.OutOfMemory;
            s.write(tool.description) catch return error.OutOfMemory;

            // inputSchema
            s.objectField("inputSchema") catch return error.OutOfMemory;
            try writeSchemaObject(&s, tool.properties, tool.required);

            // outputSchema (only if output_properties is non-empty)
            if (tool.output_properties.len > 0) {
                s.objectField("outputSchema") catch return error.OutOfMemory;
                try writeSchemaObject(&s, tool.output_properties, tool.output_required);
            }

            s.endObject() catch return error.OutOfMemory;
        }

        s.endArray() catch return error.OutOfMemory;
        s.endObject() catch return error.OutOfMemory;
        s.endObject() catch return error.OutOfMemory;

        return aw.toOwnedSlice() catch return error.OutOfMemory;
    }

    /// Writes a JSON Schema object with properties and required fields.
    fn writeSchemaObject(s: *std.json.Stringify, properties: []const protocol.SchemaProperty, required: []const []const u8) ServerError!void {
        s.beginObject() catch return error.OutOfMemory;
        s.objectField("type") catch return error.OutOfMemory;
        s.write("object") catch return error.OutOfMemory;

        s.objectField("properties") catch return error.OutOfMemory;
        s.beginObject() catch return error.OutOfMemory;

        for (properties) |prop| {
            s.objectField(prop.name) catch return error.OutOfMemory;
            try writePropertySchema(s, prop);
        }

        s.endObject() catch return error.OutOfMemory;

        if (required.len > 0) {
            s.objectField("required") catch return error.OutOfMemory;
            s.beginArray() catch return error.OutOfMemory;
            for (required) |r| {
                s.write(r) catch return error.OutOfMemory;
            }
            s.endArray() catch return error.OutOfMemory;
        }

        s.endObject() catch return error.OutOfMemory;
    }

    /// Writes the JSON Schema for a single property.
    fn writePropertySchema(s: *std.json.Stringify, prop: protocol.SchemaProperty) ServerError!void {
        s.beginObject() catch return error.OutOfMemory;

        if (prop.one_of_string_or_array) {
            s.objectField("oneOf") catch return error.OutOfMemory;
            s.beginArray() catch return error.OutOfMemory;
            // { "type": "string" }
            s.beginObject() catch return error.OutOfMemory;
            s.objectField("type") catch return error.OutOfMemory;
            s.write("string") catch return error.OutOfMemory;
            s.endObject() catch return error.OutOfMemory;
            // { "type": "array", "items": { "type": "string" }, ... }
            s.beginObject() catch return error.OutOfMemory;
            s.objectField("type") catch return error.OutOfMemory;
            s.write("array") catch return error.OutOfMemory;
            s.objectField("items") catch return error.OutOfMemory;
            s.beginObject() catch return error.OutOfMemory;
            s.objectField("type") catch return error.OutOfMemory;
            s.write("string") catch return error.OutOfMemory;
            s.endObject() catch return error.OutOfMemory;
            if (prop.max_items) |mi| {
                s.objectField("maxItems") catch return error.OutOfMemory;
                s.write(mi) catch return error.OutOfMemory;
            }
            if (prop.min_items) |mi| {
                s.objectField("minItems") catch return error.OutOfMemory;
                s.write(mi) catch return error.OutOfMemory;
            }
            s.endObject() catch return error.OutOfMemory;
            s.endArray() catch return error.OutOfMemory;
        } else {
            s.objectField("type") catch return error.OutOfMemory;
            s.write(prop.type) catch return error.OutOfMemory;

            // Array items
            if (std.mem.eql(u8, prop.type, "array")) {
                s.objectField("items") catch return error.OutOfMemory;
                s.beginObject() catch return error.OutOfMemory;
                s.objectField("type") catch return error.OutOfMemory;
                s.write(prop.items_type orelse "string") catch return error.OutOfMemory;
                if (prop.items_enum) |ie| {
                    s.objectField("enum") catch return error.OutOfMemory;
                    s.beginArray() catch return error.OutOfMemory;
                    for (ie) |v| {
                        s.write(v) catch return error.OutOfMemory;
                    }
                    s.endArray() catch return error.OutOfMemory;
                }
                s.endObject() catch return error.OutOfMemory;
            }
        }

        if (prop.description) |d| {
            s.objectField("description") catch return error.OutOfMemory;
            s.write(d) catch return error.OutOfMemory;
        }

        if (prop.enum_values) |ev| {
            s.objectField("enum") catch return error.OutOfMemory;
            s.beginArray() catch return error.OutOfMemory;
            for (ev) |v| {
                s.write(v) catch return error.OutOfMemory;
            }
            s.endArray() catch return error.OutOfMemory;
        }

        if (prop.default_bool) |db| {
            s.objectField("default") catch return error.OutOfMemory;
            s.write(db) catch return error.OutOfMemory;
        }
        if (prop.default_int) |di| {
            s.objectField("default") catch return error.OutOfMemory;
            s.write(di) catch return error.OutOfMemory;
        }
        if (prop.default_float) |df| {
            s.objectField("default") catch return error.OutOfMemory;
            s.write(df) catch return error.OutOfMemory;
        }
        if (prop.default_str) |ds| {
            s.objectField("default") catch return error.OutOfMemory;
            s.write(ds) catch return error.OutOfMemory;
        }
        if (prop.default_str_array) |dsa| {
            s.objectField("default") catch return error.OutOfMemory;
            s.beginArray() catch return error.OutOfMemory;
            for (dsa) |v| {
                s.write(v) catch return error.OutOfMemory;
            }
            s.endArray() catch return error.OutOfMemory;
        }
        if (prop.minimum) |m| {
            s.objectField("minimum") catch return error.OutOfMemory;
            s.write(m) catch return error.OutOfMemory;
        }
        if (prop.maximum) |m| {
            s.objectField("maximum") catch return error.OutOfMemory;
            s.write(m) catch return error.OutOfMemory;
        }
        if (prop.max_items) |mi| {
            if (!prop.one_of_string_or_array) {
                s.objectField("maxItems") catch return error.OutOfMemory;
                s.write(mi) catch return error.OutOfMemory;
            }
        }
        if (prop.min_items) |mi| {
            if (!prop.one_of_string_or_array) {
                s.objectField("minItems") catch return error.OutOfMemory;
                s.write(mi) catch return error.OutOfMemory;
            }
        }

        s.endObject() catch return error.OutOfMemory;
    }
};
