const std = @import("std");
const protocol = @import("protocol.zig");

/// JSON-RPC 2.0 standard error codes.
pub const parse_error: i32 = -32700;
pub const invalid_request: i32 = -32600;
pub const method_not_found: i32 = -32601;
pub const invalid_params: i32 = -32602;
pub const internal_error: i32 = -32603;

/// JSON-RPC request id: integer, string, or absent (notification).
pub const RequestId = union(enum) {
    integer: i64,
    string: []const u8,
    none,
};

/// A parsed JSON-RPC 2.0 request.
pub const Request = struct {
    id: RequestId,
    method: []const u8,
    params: ?std.json.Value,
};

/// A parsed JSON-RPC 2.0 request with its backing storage.
pub const ParsedRequest = struct {
    value: Request,
    parsed: std.json.Parsed(std.json.Value),

    pub fn deinit(self: *ParsedRequest) void {
        self.parsed.deinit();
    }
};

/// A structured JSON-RPC error object.
pub const ErrorObject = struct {
    code: i32,
    message: []const u8,
    data: ?std.json.Value,
};

/// A JSON-RPC 2.0 response (either result or error).
pub const Response = struct {
    id: RequestId,
    result: ?std.json.Value,
    @"error": ?ErrorObject,
};

/// Errors returned when parsing a JSON-RPC 2.0 request fails.
pub const ParseError = error{
    InvalidJson,
    InvalidRequest,
};

/// Parse a raw JSON-RPC 2.0 request from bytes.
pub fn parseRequest(allocator: std.mem.Allocator, input: []const u8) ParseError!ParsedRequest {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, input, .{}) catch
        return ParseError.InvalidJson;

    const root = parsed.value;
    if (root != .object) {
        parsed.deinit();
        return ParseError.InvalidRequest;
    }
    const obj = root.object;

    // Validate jsonrpc field.
    const jsonrpc_val = obj.get("jsonrpc") orelse {
        parsed.deinit();
        return ParseError.InvalidRequest;
    };
    if (jsonrpc_val != .string) {
        parsed.deinit();
        return ParseError.InvalidRequest;
    }
    if (!std.mem.eql(u8, jsonrpc_val.string, protocol.jsonrpc_version)) {
        parsed.deinit();
        return ParseError.InvalidRequest;
    }

    // Extract method.
    const method_val = obj.get("method") orelse {
        parsed.deinit();
        return ParseError.InvalidRequest;
    };
    if (method_val != .string) {
        parsed.deinit();
        return ParseError.InvalidRequest;
    }

    // Extract id.
    const id: RequestId = if (obj.get("id")) |id_val| switch (id_val) {
        .integer => |n| .{ .integer = n },
        .string => |s| .{ .string = s },
        else => .none,
    } else .none;

    const params: ?std.json.Value = obj.get("params");

    return .{
        .value = .{
            .id = id,
            .method = method_val.string,
            .params = params,
        },
        .parsed = parsed,
    };
}

/// Serialize a Response to JSON bytes. Caller owns returned slice.
pub fn serializeResponse(allocator: std.mem.Allocator, response: Response) error{OutOfMemory}![]const u8 {
    var aw: std.io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();

    var stream: std.json.Stringify = .{ .writer = &aw.writer };

    stream.beginObject() catch return error.OutOfMemory;
    stream.objectField("jsonrpc") catch return error.OutOfMemory;
    stream.write(protocol.jsonrpc_version) catch return error.OutOfMemory;

    stream.objectField("id") catch return error.OutOfMemory;
    switch (response.id) {
        .integer => |n| stream.write(n) catch return error.OutOfMemory,
        .string => |s| stream.write(s) catch return error.OutOfMemory,
        .none => stream.write(null) catch return error.OutOfMemory,
    }

    if (response.result) |result| {
        stream.objectField("result") catch return error.OutOfMemory;
        stream.write(result) catch return error.OutOfMemory;
    }

    if (response.@"error") |err| {
        stream.objectField("error") catch return error.OutOfMemory;
        stream.beginObject() catch return error.OutOfMemory;
        stream.objectField("code") catch return error.OutOfMemory;
        stream.write(err.code) catch return error.OutOfMemory;
        stream.objectField("message") catch return error.OutOfMemory;
        stream.write(err.message) catch return error.OutOfMemory;
        stream.endObject() catch return error.OutOfMemory;
    }

    stream.endObject() catch return error.OutOfMemory;

    return aw.toOwnedSlice() catch return error.OutOfMemory;
}
