const std = @import("std");

/// JSON-RPC 2.0 version identifier, required in every request and response.
pub const jsonrpc_version = "2.0";

/// MCP protocol version this server implements (from the MCP specification).
pub const mcp_protocol_version = "2024-11-05";

/// Project identity constants.
pub const server_name = "zcodeprism";
pub const server_version = "0.1.0";

/// JSON Schema stub for tool input.
pub const InputSchema = struct {
    type: []const u8 = "object",
    properties: struct {} = .{},
};

/// An MCP tool descriptor with name, description, and JSON Schema input.
pub const Tool = struct {
    name: []const u8,
    description: []const u8,
    inputSchema: InputSchema = .{},
};

/// Server identity returned in the initialize handshake.
pub const ServerInfo = struct {
    name: []const u8 = server_name,
    version: []const u8 = server_version,
};

/// Server capabilities advertised during initialize.
pub const Capabilities = struct {
    tools: bool = true,
};

/// Result payload for the initialize method.
pub const InitializeResult = struct {
    protocolVersion: []const u8 = mcp_protocol_version,
    serverInfo: ServerInfo = .{},
    capabilities: Capabilities = .{},
};

/// Result payload for the tools/list method.
pub const ToolsListResult = struct {
    tools: []const Tool,
};
