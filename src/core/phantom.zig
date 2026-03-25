const std = @import("std");
const graph_mod = @import("graph.zig");
const types = @import("types.zig");
const node_mod = @import("node.zig");
const lang_meta_mod = @import("lang_meta.zig");
const worklist_mod = @import("../lsp/worklist.zig");

const Graph = graph_mod.Graph;
const Node = node_mod.Node;
const NodeId = types.NodeId;
const NodeKind = types.NodeKind;
const Language = types.Language;
const ExternalInfo = lang_meta_mod.ExternalInfo;
const UsageSite = worklist_mod.UsageSite;

/// Deduplicated store for phantom nodes -- external symbols (stdlib, dependencies)
/// referenced but not defined in the project.
///
/// Multiple references to the same qualified name share a single node.
/// The kind of each segment is derived from its role and name: intermediate
/// segments are always `.module`, and leaf segments use the naming convention
/// (PascalCase = `.type_def`, otherwise `.module`).
pub const PhantomManager = struct {
    graph: *Graph,
    lookup: std.StringHashMapUnmanaged(NodeId),
    usage_sites: std.AutoHashMapUnmanaged(NodeId, UsageSite),

    /// Creates a new PhantomManager backed by the given graph.
    pub fn init(graph: *Graph) PhantomManager {
        return .{
            .graph = graph,
            .lookup = .{},
            .usage_sites = .{},
        };
    }

    /// Returns the phantom NodeId for `qualified_name`, creating it (and any
    /// missing intermediate segments) on first encounter.
    ///
    /// Dotted names are split on '.'. Each segment's kind is determined by
    /// the PhantomManager: intermediate segments are `.module`, and the leaf
    /// segment is `.type_def` if its first character is uppercase (PascalCase),
    /// `.module` otherwise. Intermediate segments become `.module` nodes;
    ///   the leaf becomes `.type_def` if PascalCase, `.module` otherwise.
    ///
    /// `language` and `external` are forwarded to every newly created node.
    pub fn getOrCreate(self: *PhantomManager, allocator: std.mem.Allocator, qualified_name: []const u8, language: Language, external: ExternalInfo) !NodeId {
        // Fast path: already created.
        if (self.lookup.get(qualified_name)) |id| return id;

        // Build parent chain by splitting on '.'.
        var it = std.mem.splitScalar(u8, qualified_name, '.');
        var parent_id: ?NodeId = null;
        var prefix_len: usize = 0;

        while (it.next()) |segment| {
            if (prefix_len > 0) prefix_len += 1; // account for '.'
            prefix_len += segment.len;

            const prefix = qualified_name[0..prefix_len];

            if (self.lookup.get(prefix)) |existing_id| {
                parent_id = existing_id;
                continue;
            }

            const node_kind = inferKind(segment, prefix_len == qualified_name.len);

            // Dupe segment name for the node, owned by graph.
            const duped_name = blk: {
                const d = try allocator.dupe(u8, segment);
                errdefer allocator.free(d);
                try self.graph.addOwnedBuffer(allocator, d);
                break :blk d;
            };

            const node_id = try self.graph.addNode(allocator, Node{
                .id = .root,
                .name = duped_name,
                .kind = node_kind,
                .language = language,
                .parent_id = parent_id,
                .external = external,
            });

            // Dupe prefix for the lookup key, owned by PhantomManager, freed in deinit.
            {
                const duped_prefix = try allocator.dupe(u8, prefix);
                errdefer allocator.free(duped_prefix);
                try self.lookup.put(allocator, duped_prefix, node_id);
            }

            parent_id = node_id;
        }

        return self.lookup.get(qualified_name).?;
    }

    /// Records the first usage site seen for the given phantom node.
    /// Subsequent calls for the same `id` are ignored.
    pub fn recordUsageSite(
        self: *PhantomManager,
        allocator: std.mem.Allocator,
        id: NodeId,
        site: UsageSite,
    ) error{OutOfMemory}!void {
        const gop = try self.usage_sites.getOrPut(allocator, id);
        if (!gop.found_existing) gop.value_ptr.* = site;
    }

    /// Find a phantom node by its leaf segment (the name after the last '.').
    /// Returns null if no match exists or if multiple phantoms share the same leaf.
    pub fn findByShortName(self: *const PhantomManager, name: []const u8) ?NodeId {
        var match: ?NodeId = null;
        var count: usize = 0;
        var it = self.lookup.iterator();
        while (it.next()) |entry| {
            const key = entry.key_ptr.*;
            const leaf = if (std.mem.lastIndexOfScalar(u8, key, '.')) |pos| key[pos + 1 ..] else key;
            if (std.mem.eql(u8, leaf, name)) {
                match = entry.value_ptr.*;
                count += 1;
                if (count > 1) return null;
            }
        }
        return match;
    }

    /// Frees all memory owned by this manager.
    ///
    /// Does not free the phantom nodes themselves.
    /// `allocator` must be the same allocator used for all prior calls.
    pub fn deinit(self: *PhantomManager, allocator: std.mem.Allocator) void {
        var it = self.lookup.iterator();
        while (it.next()) |entry| {
            allocator.free(@constCast(entry.key_ptr.*));
        }
        self.lookup.deinit(allocator);
        self.usage_sites.deinit(allocator);
    }

    /// Intermediate segments are always modules. Leaf segments use naming
    /// convention: PascalCase (first char uppercase) means type_def,
    /// everything else means module.
    fn inferKind(segment: []const u8, is_leaf: bool) NodeKind {
        if (!is_leaf) return .module;
        if (segment.len > 0 and segment[0] >= 'A' and segment[0] <= 'Z') return .type_def;
        return .module;
    }
};
