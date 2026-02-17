const std = @import("std");
const graph_mod = @import("graph.zig");
const types = @import("types.zig");
const node_mod = @import("node.zig");
const lang = @import("../languages/language.zig");

const Graph = graph_mod.Graph;
const Node = node_mod.Node;
const NodeId = types.NodeId;
const NodeKind = types.NodeKind;

/// Manages phantom nodes, i.e. nodes representing external symbols (stdlib, dependencies)
/// that are referenced but not defined in the project.
/// Phantom nodes are deduplicated: multiple references to the same qualified name
/// share a single node.
pub const PhantomManager = struct {
    graph: *Graph,
    allocator: std.mem.Allocator,
    lookup: std.StringHashMapUnmanaged(NodeId),

    pub fn init(allocator: std.mem.Allocator, graph: *Graph) PhantomManager {
        return .{
            .graph = graph,
            .allocator = allocator,
            .lookup = .{},
        };
    }

    /// Get the phantom node for a qualified name, creating it if it doesn't exist.
    /// For example, "std.mem.Allocator" creates a chain: std → std.mem → std.mem.Allocator.
    pub fn getOrCreate(self: *PhantomManager, qualified_name: []const u8, kind: NodeKind) !NodeId {
        // Fast path: already created.
        if (self.lookup.get(qualified_name)) |id| return id;

        // Build parent chain by splitting on '.'.
        var it = std.mem.splitScalar(u8, qualified_name, '.');
        var parent_id: ?NodeId = null;
        var prefix_len: usize = 0;

        while (it.next()) |segment| {
            // Advance prefix_len to include separator if not the first segment.
            if (prefix_len > 0) prefix_len += 1; // account for '.'
            prefix_len += segment.len;

            const prefix = qualified_name[0..prefix_len];

            if (self.lookup.get(prefix)) |existing_id| {
                parent_id = existing_id;
                continue;
            }

            // Determine kind: .module for intermediate segments, caller-specified for leaf.
            const node_kind: NodeKind = if (prefix_len == qualified_name.len) kind else .module;

            // Dupe segment name for the node, owned by graph.
            const duped_name = try self.allocator.dupe(u8, segment);
            try self.graph.addOwnedBuffer(duped_name);

            const node_id = try self.graph.addNode(Node{
                .id = .root,
                .name = duped_name,
                .kind = node_kind,
                .language = .zig,
                .parent_id = parent_id,
                .external = .{ .stdlib = {} },
            });

            // Dupe prefix for the lookup key, owned by PhantomManager, freed in deinit.
            const duped_prefix = try self.allocator.dupe(u8, prefix);
            try self.lookup.put(self.allocator, duped_prefix, node_id);

            parent_id = node_id;
        }

        return self.lookup.get(qualified_name).?;
    }

    pub fn deinit(self: *PhantomManager) void {
        var it = self.lookup.iterator();
        while (it.next()) |entry| {
            self.allocator.free(@constCast(entry.key_ptr.*));
        }
        self.lookup.deinit(self.allocator);
    }
};
