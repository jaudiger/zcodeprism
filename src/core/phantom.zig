const std = @import("std");
const graph_mod = @import("graph.zig");
const types = @import("types.zig");
const node_mod = @import("node.zig");
const lang = @import("../languages/language.zig");

const Graph = graph_mod.Graph;
const Node = node_mod.Node;
const NodeId = types.NodeId;
const NodeKind = types.NodeKind;
const Language = types.Language;
const ExternalInfo = lang.ExternalInfo;

/// Deduplicated store for phantom nodes -- external symbols (stdlib, dependencies)
/// referenced but not defined in the project.
///
/// Multiple references to the same qualified name share a single node.
/// Intermediate segments in a dotted path (e.g. "std.mem" in "std.mem.Allocator")
/// are created automatically as `.module` phantom nodes.
pub const PhantomManager = struct {
    graph: *Graph,
    allocator: std.mem.Allocator,
    lookup: std.StringHashMapUnmanaged(NodeId),

    /// Creates a new PhantomManager backed by the given graph.
    ///
    /// `allocator` -- used for the internal lookup table keys; must outlive the manager.
    /// `graph` -- the graph into which phantom nodes are inserted.
    pub fn init(allocator: std.mem.Allocator, graph: *Graph) PhantomManager {
        return .{
            .graph = graph,
            .allocator = allocator,
            .lookup = .{},
        };
    }

    /// Returns the phantom NodeId for `qualified_name`, creating it (and any
    /// missing intermediate segments) on first encounter.
    ///
    /// Dotted names are split on '.'; intermediate segments become `.module`
    /// nodes while the leaf receives the caller-supplied `kind`.
    /// For example, "std.mem.Allocator" produces the chain:
    ///   std (.module) -> std.mem (.module) -> std.mem.Allocator (`kind`).
    ///
    /// `language` and `external` are forwarded to every newly created node.
    /// Returns `error.OutOfMemory` if the graph or lookup allocation fails.
    pub fn getOrCreate(self: *PhantomManager, qualified_name: []const u8, kind: NodeKind, language: Language, external: ExternalInfo) !NodeId {
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
            const duped_name = blk: {
                const d = try self.allocator.dupe(u8, segment);
                errdefer self.allocator.free(d);
                try self.graph.addOwnedBuffer(d);
                break :blk d;
            };

            const node_id = try self.graph.addNode(Node{
                .id = .root,
                .name = duped_name,
                .kind = node_kind,
                .language = language,
                .parent_id = parent_id,
                .external = external,
            });

            // Dupe prefix for the lookup key, owned by PhantomManager, freed in deinit.
            {
                const duped_prefix = try self.allocator.dupe(u8, prefix);
                errdefer self.allocator.free(duped_prefix);
                try self.lookup.put(self.allocator, duped_prefix, node_id);
            }

            parent_id = node_id;
        }

        return self.lookup.get(qualified_name).?;
    }

    /// Frees all lookup-key memory owned by this manager.
    ///
    /// Does not free the phantom nodes themselves; those are owned by the graph.
    pub fn deinit(self: *PhantomManager) void {
        var it = self.lookup.iterator();
        while (it.next()) |entry| {
            self.allocator.free(@constCast(entry.key_ptr.*));
        }
        self.lookup.deinit(self.allocator);
    }
};
