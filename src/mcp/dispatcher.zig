const std = @import("std");
const protocol = @import("protocol.zig");

const Tool = protocol.Tool;

const tool_defs = [_]Tool{
    .{ .name = "analysis.complexity", .description = "Compute complexity metrics for nodes." },
    .{ .name = "analysis.coupling", .description = "Measure coupling between modules." },
    .{ .name = "analysis.dead_code", .description = "Find unreferenced private declarations." },
    .{ .name = "analysis.dependency_cycles", .description = "Detect circular dependency chains." },
    .{ .name = "analysis.duplicates", .description = "Find near-duplicate code regions." },
    .{ .name = "analysis.impact", .description = "Estimate change impact from a node." },
    .{ .name = "explorer.annotate", .description = "Attach an annotation to a node." },
    .{ .name = "explorer.annotations", .description = "List annotations on a node." },
    .{ .name = "explorer.cursor_close", .description = "Close an exploration cursor." },
    .{ .name = "explorer.cursor_create", .description = "Create a new exploration cursor." },
    .{ .name = "explorer.cursor_expand", .description = "Expand cursor to show children." },
    .{ .name = "explorer.cursor_move", .description = "Move cursor to a different node." },
    .{ .name = "explorer.cursor_query", .description = "Query the graph from cursor position." },
    .{ .name = "explorer.diff", .description = "Compare two graph generations." },
    .{ .name = "graph.get_edges", .description = "Retrieve edges for a node." },
    .{ .name = "graph.get_nodes", .description = "Retrieve node details by id or scope." },
    .{ .name = "graph.get_source", .description = "Retrieve source text for a node." },
    .{ .name = "graph.path", .description = "Find shortest path between two nodes." },
    .{ .name = "graph.search", .description = "Search nodes by name, kind, or attributes." },
    .{ .name = "graph.stats", .description = "Returns global codebase statistics." },
};

/// Tool registry holding MCP tool descriptors.
pub const Dispatcher = struct {
    tools: []const Tool = &tool_defs,

    /// Create a dispatcher with the default tool definitions.
    pub fn init() Dispatcher {
        return .{};
    }

    /// Release dispatcher resources (currently a no-op).
    pub fn deinit(self: *Dispatcher) void {
        _ = self;
    }

    /// Return the full list of registered tools.
    pub fn listTools(self: *const Dispatcher) []const Tool {
        return self.tools;
    }
};
