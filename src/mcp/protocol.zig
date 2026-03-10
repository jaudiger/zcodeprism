const std = @import("std");

/// JSON-RPC 2.0 version identifier, required in every request and response.
pub const jsonrpc_version = "2.0";

/// MCP protocol version this server implements (from the MCP specification).
pub const mcp_protocol_version = "2025-11-25";

/// Project identity constants.
pub const server_name = "zcodeprism";
pub const server_version = "0.1.0";

/// Describes a single property in a JSON Schema object.
pub const SchemaProperty = struct {
    name: []const u8,
    type: []const u8 = "string",
    description: ?[]const u8 = null,
    enum_values: ?[]const []const u8 = null,
    default_bool: ?bool = null,
    default_int: ?i32 = null,
    default_float: ?f64 = null,
    default_str: ?[]const u8 = null,
    minimum: ?f64 = null,
    maximum: ?f64 = null,
    /// Emits a oneOf [string, array-of-strings] schema instead of a plain type.
    one_of_string_or_array: bool = false,
    max_items: ?u32 = null,
    min_items: ?u32 = null,
    /// For array properties, the type of each item.
    items_type: ?[]const u8 = null,
    /// For array items with enum constraints.
    items_enum: ?[]const []const u8 = null,
    /// Default array of strings (serialized as JSON array).
    default_str_array: ?[]const []const u8 = null,
};

/// An MCP tool descriptor with name, description, and structured schema info.
pub const Tool = struct {
    name: []const u8,
    title: []const u8,
    description: []const u8,
    properties: []const SchemaProperty,
    required: []const []const u8 = &.{},
    output_properties: []const SchemaProperty = &.{},
    output_required: []const []const u8 = &.{},
};

/// Server identity returned in the initialize handshake.
pub const ServerInfo = struct {
    name: []const u8 = server_name,
    version: []const u8 = server_version,
};

/// Server capabilities advertised during initialize.
pub const Capabilities = struct {
    pub const ToolsCapability = struct {
        listChanged: bool = false,
    };
    tools: ToolsCapability = .{},
};

/// Result payload for the initialize method.
pub const InitializeResult = struct {
    protocolVersion: []const u8 = mcp_protocol_version,
    serverInfo: ServerInfo = .{},
    capabilities: Capabilities = .{},
};
