const std = @import("std");
const types = @import("../core/types.zig");
const generation_mod = @import("../core/generation.zig");
const gen_manager_mod = @import("../watcher/generation_manager.zig");
const cursor_manager_mod = @import("../explorer/cursor_manager.zig");
const dispatcher_mod = @import("dispatcher.zig");
const handlers = @import("handlers.zig");
const jsonrpc = @import("jsonrpc.zig");
const protocol = @import("protocol.zig");
const json_writer_mod = @import("json_writer.zig");

const GraphGeneration = generation_mod.GraphGeneration;
const GenerationManager = gen_manager_mod.GenerationManager;
const CursorManager = cursor_manager_mod.CursorManager;
const Dispatcher = dispatcher_mod.Dispatcher;
const JsonWriter = json_writer_mod.JsonWriter;

/// Errors that the MCP server can surface to callers.
pub const ServerError = error{
    OutOfMemory,
};

/// MCP server that processes JSON-RPC messages against a graph generation.
pub const Server = struct {
    gen_manager: *GenerationManager,
    dispatcher: Dispatcher,
    cursor_manager: CursorManager,

    pub fn init(mgr: *GenerationManager) Server {
        return .{
            .gen_manager = mgr,
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
    pub fn handleMessage(self: *Server, allocator: std.mem.Allocator, io: std.Io, input: []const u8) ServerError!?[]const u8 {
        var parsed = jsonrpc.parseRequest(allocator, input) catch |err| switch (err) {
            error.InvalidJson => return try buildErrorResponse(allocator, .none, jsonrpc.parse_error, "Parse error"),
            error.InvalidRequest => return try buildErrorResponse(allocator, .none, jsonrpc.invalid_request, "Invalid Request"),
        };
        defer parsed.deinit();
        const req = parsed.value;

        if (req.id == .none) return null;

        const guard = self.gen_manager.acquireCurrent(io);
        defer guard.deinit();

        if (std.mem.eql(u8, req.method, "initialize")) {
            return try buildSuccessResponse(allocator, req.id, protocol.InitializeResult{});
        } else if (std.mem.eql(u8, req.method, "ping")) {
            return try buildSuccessResponse(allocator, req.id, struct {}{});
        } else if (std.mem.eql(u8, req.method, "tools/list")) {
            return try buildToolsListResponse(allocator, req.id, self.dispatcher.listTools());
        } else if (std.mem.eql(u8, req.method, "tools/call")) {
            return try self.handleToolCall(allocator, io, req, guard.gen);
        } else {
            return try buildErrorResponse(allocator, req.id, jsonrpc.method_not_found, "Method not found");
        }
    }

    fn handleToolCall(self: *Server, allocator: std.mem.Allocator, io: std.Io, req: jsonrpc.Request, gen: *GraphGeneration) ServerError![]const u8 {
        const params_obj = if (req.params) |p| (if (p == .object) p.object else null) else null;
        const tool_name = if (params_obj) |obj| (if (obj.get("name")) |v| (if (v == .string) v.string else null) else null) else null;

        if (tool_name == null) {
            return try buildErrorResponse(allocator, req.id, jsonrpc.invalid_params, "Missing tool name");
        }

        const content_json = handlers.handleToolCall(allocator, io, gen, &self.cursor_manager, tool_name.?, req.params) catch
            return try buildErrorResponse(allocator, req.id, jsonrpc.internal_error, "Handler error");

        if (content_json) |json| {
            defer allocator.free(json);
            return try buildToolCallResponse(allocator, req.id, json);
        }

        return try buildErrorResponse(allocator, req.id, jsonrpc.method_not_found, "Unknown tool");
    }

    /// Wraps raw handler JSON into the full JSON-RPC + MCP content envelope.
    fn buildToolCallResponse(allocator: std.mem.Allocator, id: jsonrpc.RequestId, raw_content_json: []const u8) ServerError![]const u8 {
        var aw: std.Io.Writer.Allocating = .init(allocator);
        errdefer aw.deinit();
        var s: std.json.Stringify = .{ .writer = &aw.writer };
        const w: JsonWriter = .{ .s = &s };

        try w.beginObject();
        try w.fieldValue("jsonrpc", protocol.jsonrpc_version);
        try w.field("id");
        try writeId(w, id);
        try w.field("result");
        try w.beginObject();
        try w.field("content");
        try w.beginArray();
        try w.beginObject();
        try w.fieldValue("type", "text");
        try w.fieldValue("text", raw_content_json);
        try w.endObject();
        try w.endArray();
        try w.endObject();
        try w.endObject();

        return aw.toOwnedSlice() catch return error.OutOfMemory;
    }

    fn writeId(w: JsonWriter, id: jsonrpc.RequestId) error{OutOfMemory}!void {
        switch (id) {
            .integer => |n| try w.write(n),
            .string => |str| try w.write(str),
            .none => try w.write(null),
        }
    }

    fn buildSuccessResponse(allocator: std.mem.Allocator, id: jsonrpc.RequestId, result: anytype) ServerError![]const u8 {
        var aw: std.Io.Writer.Allocating = .init(allocator);
        errdefer aw.deinit();
        var s: std.json.Stringify = .{ .writer = &aw.writer };
        const w: JsonWriter = .{ .s = &s };

        try w.beginObject();
        try w.fieldValue("jsonrpc", protocol.jsonrpc_version);
        try w.field("id");
        try writeId(w, id);
        try w.fieldValue("result", result);
        try w.endObject();

        return aw.toOwnedSlice() catch return error.OutOfMemory;
    }

    fn buildErrorResponse(allocator: std.mem.Allocator, id: jsonrpc.RequestId, code: i32, message: []const u8) ServerError![]const u8 {
        var aw: std.Io.Writer.Allocating = .init(allocator);
        errdefer aw.deinit();
        var s: std.json.Stringify = .{ .writer = &aw.writer };
        const w: JsonWriter = .{ .s = &s };

        try w.beginObject();
        try w.fieldValue("jsonrpc", protocol.jsonrpc_version);
        try w.field("id");
        try writeId(w, id);
        try w.field("error");
        try w.beginObject();
        try w.fieldValue("code", code);
        try w.fieldValue("message", message);
        try w.endObject();
        try w.endObject();

        return aw.toOwnedSlice() catch return error.OutOfMemory;
    }

    /// Build the tools/list response with full JSON Schema for each tool.
    fn buildToolsListResponse(allocator: std.mem.Allocator, id: jsonrpc.RequestId, tools: []const protocol.Tool) ServerError![]const u8 {
        var aw: std.Io.Writer.Allocating = .init(allocator);
        errdefer aw.deinit();
        var s: std.json.Stringify = .{ .writer = &aw.writer };
        const w: JsonWriter = .{ .s = &s };

        try w.beginObject();
        try w.fieldValue("jsonrpc", protocol.jsonrpc_version);
        try w.field("id");
        try writeId(w, id);
        try w.field("result");
        try w.beginObject();
        try w.field("tools");
        try w.beginArray();

        for (tools) |tool| {
            try w.beginObject();
            try w.fieldValue("name", tool.name);
            try w.fieldValue("title", tool.title);
            try w.fieldValue("description", tool.description);

            // inputSchema
            try w.field("inputSchema");
            writeSchemaObject(w.s, tool.properties, tool.required) catch return error.OutOfMemory;

            // outputSchema (only if output_properties is non-empty)
            if (tool.output_properties.len > 0) {
                try w.field("outputSchema");
                writeSchemaObject(w.s, tool.output_properties, tool.output_required) catch return error.OutOfMemory;
            }

            try w.endObject();
        }

        try w.endArray();
        try w.endObject();
        try w.endObject();

        return aw.toOwnedSlice() catch return error.OutOfMemory;
    }

    /// Writes a JSON Schema object with properties and required fields.
    fn writeSchemaObject(s: *std.json.Stringify, properties: []const protocol.SchemaProperty, required: []const []const u8) !void {
        try s.beginObject();
        try s.objectField("type");
        try s.write("object");

        try s.objectField("properties");
        try s.beginObject();

        for (properties) |prop| {
            try s.objectField(prop.name);
            try writePropertySchema(s, prop);
        }

        try s.endObject();

        if (required.len > 0) {
            try s.objectField("required");
            try s.beginArray();
            for (required) |r| {
                try s.write(r);
            }
            try s.endArray();
        }

        try s.endObject();
    }

    /// Build a JSON-RPC notification (no id field). Caller owns returned slice.
    pub fn buildNotification(allocator: std.mem.Allocator, method: []const u8, generation_id: u64, source_hash: types.ContentHash) ServerError![]const u8 {
        var aw: std.Io.Writer.Allocating = .init(allocator);
        errdefer aw.deinit();
        var s: std.json.Stringify = .{ .writer = &aw.writer };
        const w: JsonWriter = .{ .s = &s };

        try w.beginObject();
        try w.fieldValue("jsonrpc", protocol.jsonrpc_version);
        try w.fieldValue("method", method);
        try w.field("params");
        try w.beginObject();
        try w.fieldValue("generation_id", generation_id);
        try w.field("source_hash");
        try w.hashHex(source_hash);
        try w.endObject();
        try w.endObject();

        return aw.toOwnedSlice() catch return error.OutOfMemory;
    }

    /// Writes the JSON Schema for a single property.
    fn writePropertySchema(s: *std.json.Stringify, prop: protocol.SchemaProperty) !void {
        try s.beginObject();

        if (prop.one_of_string_or_array) {
            try s.objectField("oneOf");
            try s.beginArray();
            // { "type": "string" }
            try s.beginObject();
            try s.objectField("type");
            try s.write("string");
            try s.endObject();
            // { "type": "array", "items": { "type": "string" }, ... }
            try s.beginObject();
            try s.objectField("type");
            try s.write("array");
            try s.objectField("items");
            try s.beginObject();
            try s.objectField("type");
            try s.write("string");
            try s.endObject();
            if (prop.max_items) |mi| {
                try s.objectField("maxItems");
                try s.write(mi);
            }
            if (prop.min_items) |mi| {
                try s.objectField("minItems");
                try s.write(mi);
            }
            try s.endObject();
            try s.endArray();
        } else {
            try s.objectField("type");
            try s.write(prop.type);

            // Array items
            if (std.mem.eql(u8, prop.type, "array")) {
                try s.objectField("items");
                try s.beginObject();
                try s.objectField("type");
                try s.write(prop.items_type orelse "string");
                if (prop.items_enum) |ie| {
                    try s.objectField("enum");
                    try s.beginArray();
                    for (ie) |v| {
                        try s.write(v);
                    }
                    try s.endArray();
                }
                try s.endObject();
            }
        }

        if (prop.description) |d| {
            try s.objectField("description");
            try s.write(d);
        }

        if (prop.enum_values) |ev| {
            try s.objectField("enum");
            try s.beginArray();
            for (ev) |v| {
                try s.write(v);
            }
            try s.endArray();
        }

        if (prop.default) |default| {
            try s.objectField("default");
            switch (default) {
                .bool => |v| try s.write(v),
                .int => |v| try s.write(v),
                .float => |v| try s.write(v),
                .str => |v| try s.write(v),
                .str_array => |arr| {
                    try s.beginArray();
                    for (arr) |v| {
                        try s.write(v);
                    }
                    try s.endArray();
                },
            }
        }

        if (prop.minimum) |m| {
            try s.objectField("minimum");
            try s.write(m);
        }
        if (prop.maximum) |m| {
            try s.objectField("maximum");
            try s.write(m);
        }
        if (prop.max_items) |mi| {
            if (!prop.one_of_string_or_array) {
                try s.objectField("maxItems");
                try s.write(mi);
            }
        }
        if (prop.min_items) |mi| {
            if (!prop.one_of_string_or_array) {
                try s.objectField("minItems");
                try s.write(mi);
            }
        }

        try s.endObject();
    }
};
