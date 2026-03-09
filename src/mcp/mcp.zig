//! MCP (Model Context Protocol) module root.
//! Re-exports sub-modules for JSON-RPC transport, protocol types,
//! server lifecycle, and tool dispatch.

pub const jsonrpc = @import("jsonrpc.zig");
pub const protocol = @import("protocol.zig");
pub const server = @import("server.zig");
pub const dispatcher = @import("dispatcher.zig");
pub const handlers = @import("handlers.zig");
pub const cursor_manager = @import("../explorer/cursor_manager.zig");

test {
    _ = jsonrpc;
    _ = protocol;
    _ = server;
    _ = dispatcher;
    _ = handlers;
    _ = cursor_manager;
}
