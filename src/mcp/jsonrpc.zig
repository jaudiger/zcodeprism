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

