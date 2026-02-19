const std = @import("std");
const graph_mod = @import("../../core/graph.zig");
const logging = @import("../../logging.zig");
const types = @import("../../core/types.zig");
const ts = @import("tree-sitter");

const Field = logging.Field;
const Logger = logging.Logger;

const Graph = graph_mod.Graph;
const NodeId = types.NodeId;
const EdgeType = types.EdgeType;

// =========================================================================
// KindIds: u16 tree-sitter node kind IDs for fast dispatch
// =========================================================================

/// Pre-resolved tree-sitter node kind IDs. Built once per parse() call from
/// `Language.idForNodeKind()`. Replaces all `std.mem.eql(u8, child.kind(), "...")`
/// patterns with `child.kindId() == k.identifier`, using integer compare
/// instead of string compare.
pub const KindIds = struct {
    identifier: u16,
    field_identifier: u16,
    property_identifier: u16,
    field_expression: u16,
    call_expression: u16,
    function_declaration: u16,
    variable_declaration: u16,
    test_declaration: u16,
    container_field: u16,
    block: u16,
    comment: u16,
    string: u16,
    string_content: u16,
    builtin_function: u16,
    builtin_identifier: u16,
    builtin_type: u16,
    parameters: u16,
    parameter: u16,
    return_expression: u16,
    expression_statement: u16,
    struct_declaration: u16,
    union_declaration: u16,
    enum_declaration: u16,
    error_set_declaration: u16,
    try_expression: u16,
    if_statement: u16,
    if_expression: u16,
    for_statement: u16,
    while_statement: u16,
    defer_statement: u16,
    pointer_type: u16,
    nullable_type: u16,
    optional_type: u16,
    error_union_type: u16,
    payload: u16,
    payload_identifier: u16,
    pub_kw: u16,
    colon: u16,

    pub fn init(lang: *const ts.Language) KindIds {
        return .{
            .identifier = lang.idForNodeKind("identifier", true),
            .field_identifier = lang.idForNodeKind("field_identifier", true),
            .property_identifier = lang.idForNodeKind("property_identifier", true),
            .field_expression = lang.idForNodeKind("field_expression", true),
            .call_expression = lang.idForNodeKind("call_expression", true),
            .function_declaration = lang.idForNodeKind("function_declaration", true),
            .variable_declaration = lang.idForNodeKind("variable_declaration", true),
            .test_declaration = lang.idForNodeKind("test_declaration", true),
            .container_field = lang.idForNodeKind("container_field", true),
            .block = lang.idForNodeKind("block", true),
            .comment = lang.idForNodeKind("comment", true),
            .string = lang.idForNodeKind("string", true),
            .string_content = lang.idForNodeKind("string_content", true),
            .builtin_function = lang.idForNodeKind("builtin_function", true),
            .builtin_identifier = lang.idForNodeKind("builtin_identifier", true),
            .builtin_type = lang.idForNodeKind("builtin_type", true),
            .parameters = lang.idForNodeKind("parameters", true),
            .parameter = lang.idForNodeKind("parameter", true),
            .return_expression = lang.idForNodeKind("return_expression", true),
            .expression_statement = lang.idForNodeKind("expression_statement", true),
            .struct_declaration = lang.idForNodeKind("struct_declaration", true),
            .union_declaration = lang.idForNodeKind("union_declaration", true),
            .enum_declaration = lang.idForNodeKind("enum_declaration", true),
            .error_set_declaration = lang.idForNodeKind("error_set_declaration", true),
            .try_expression = lang.idForNodeKind("try_expression", true),
            .if_statement = lang.idForNodeKind("if_statement", true),
            .if_expression = lang.idForNodeKind("if_expression", true),
            .for_statement = lang.idForNodeKind("for_statement", true),
            .while_statement = lang.idForNodeKind("while_statement", true),
            .defer_statement = lang.idForNodeKind("defer_statement", true),
            .pointer_type = lang.idForNodeKind("pointer_type", true),
            .nullable_type = lang.idForNodeKind("nullable_type", true),
            .optional_type = lang.idForNodeKind("optional_type", true),
            .error_union_type = lang.idForNodeKind("error_union_type", true),
            .payload = lang.idForNodeKind("payload", true),
            .payload_identifier = lang.idForNodeKind("payload_identifier", true),
            .pub_kw = lang.idForNodeKind("pub", false),
            .colon = lang.idForNodeKind(":", false),
        };
    }
};

// =========================================================================
// EdgeSet: hash dedup for candidate edge insertion
// =========================================================================

const EdgeKey = struct {
    source_id: NodeId,
    target_id: NodeId,
    edge_type: EdgeType,
};

/// Hash-based dedup set for edges. Replaces the linear scan in addEdgeIfNew
/// with a hash lookup. Accumulates seen (source, target, type) triples during
/// a single parse() call.
pub const EdgeSet = struct {
    map: std.AutoHashMapUnmanaged(EdgeKey, void) = .{},

    /// Insert edge into the dedup set. If new, also adds it to the graph.
    pub fn insertAndAdd(self: *EdgeSet, allocator: std.mem.Allocator, g: *Graph, source_id: NodeId, target_id: NodeId, edge_type: EdgeType, log: Logger) !void {
        if (source_id == target_id) return;
        const key = EdgeKey{ .source_id = source_id, .target_id = target_id, .edge_type = edge_type };
        const gop = try self.map.getOrPut(allocator, key);
        if (gop.found_existing) {
            log.trace("duplicate edge skipped", &.{
                Field.uint("source", @intFromEnum(source_id)),
                Field.uint("target", @intFromEnum(target_id)),
            });
            return;
        }
        _ = try g.addEdge(.{
            .source_id = source_id,
            .target_id = target_id,
            .edge_type = edge_type,
            .source = .tree_sitter,
        });
    }

    pub fn deinit(self: *EdgeSet, allocator: std.mem.Allocator) void {
        self.map.deinit(allocator);
    }
};

// =========================================================================
// ScopeIndex: parent NodeId to children slice (MAF pattern)
// =========================================================================

const Node = @import("../../core/node.zig").Node;

/// Pre-built index mapping parent node IDs to their direct children.
/// Replaces full scans of `g.nodes.items` that filter by `parent_id`
/// with a hash lookup and small slice iteration.
pub const ScopeIndex = struct {
    map: std.AutoHashMapUnmanaged(u64, Range) = .{},
    storage: []u64 = &.{},

    const Range = struct { start: u32, len: u32 };

    /// Return the child node indices for a given parent.
    pub fn childrenOf(self: *const ScopeIndex, parent_id: NodeId) []const u64 {
        const key = @intFromEnum(parent_id);
        const range = self.map.get(key) orelse return &.{};
        return self.storage[range.start .. range.start + range.len];
    }

    /// Build the scope index from a node array.
    /// `offset` is the index of the first node to include (typically 0 for all nodes,
    /// or the scope_start for file-scoped indices).
    pub fn build(allocator: std.mem.Allocator, nodes: []const Node, offset: usize) !ScopeIndex {
        // --- Measure ---
        var child_counts = std.AutoHashMapUnmanaged(u64, u32){};
        defer child_counts.deinit(allocator);
        var total_children: usize = 0;
        for (nodes[offset..]) |n| {
            if (n.parent_id) |pid| {
                const key = @intFromEnum(pid);
                const gop = try child_counts.getOrPut(allocator, key);
                if (!gop.found_existing) gop.value_ptr.* = 0;
                gop.value_ptr.* += 1;
                total_children += 1;
            }
        }
        if (total_children == 0) return .{};

        // --- Allocate ---
        const storage = try allocator.alloc(u64, total_children);
        errdefer allocator.free(storage);

        // Compute offsets: each parent gets a contiguous slice.
        var offsets = std.AutoHashMapUnmanaged(u64, u32){};
        defer offsets.deinit(allocator);
        {
            var running: u32 = 0;
            var it = child_counts.iterator();
            while (it.next()) |entry| {
                try offsets.put(allocator, entry.key_ptr.*, running);
                running += entry.value_ptr.*;
            }
        }

        // --- Fill ---
        var write_pos = std.AutoHashMapUnmanaged(u64, u32){};
        defer write_pos.deinit(allocator);
        {
            var it = offsets.iterator();
            while (it.next()) |entry| {
                try write_pos.put(allocator, entry.key_ptr.*, entry.value_ptr.*);
            }
        }
        for (nodes[offset..], offset..) |n, i| {
            if (n.parent_id) |pid| {
                const key = @intFromEnum(pid);
                if (write_pos.getPtr(key)) |pos| {
                    storage[pos.*] = i;
                    pos.* += 1;
                }
            }
        }

        // Build the final map with Range values.
        var map = std.AutoHashMapUnmanaged(u64, Range){};
        errdefer map.deinit(allocator);
        {
            var it = offsets.iterator();
            while (it.next()) |entry| {
                const parent_key = entry.key_ptr.*;
                const start = entry.value_ptr.*;
                const count = child_counts.get(parent_key).?;
                try map.put(allocator, parent_key, .{ .start = start, .len = count });
            }
        }

        return .{ .map = map, .storage = storage };
    }

    pub fn deinit(self: *ScopeIndex, allocator: std.mem.Allocator) void {
        self.map.deinit(allocator);
        if (self.storage.len > 0) allocator.free(self.storage);
    }
};

// =========================================================================
// FileIndex: file basename to NodeId for fast file lookup
// =========================================================================

/// Maps file node names to their NodeId. Built once per parse,
/// replaces full scans in `findImportTargetFile`.
pub const FileIndex = struct {
    map: std.StringHashMapUnmanaged(NodeId) = .{},

    /// Build the file index from a node array.
    pub fn build(allocator: std.mem.Allocator, nodes: []const Node) !FileIndex {
        var fi = FileIndex{};
        for (nodes, 0..) |n, i| {
            if (n.kind == .file) {
                try fi.map.put(allocator, n.name, @enumFromInt(i));
            }
        }
        return fi;
    }

    pub fn findByName(self: *const FileIndex, name: []const u8) ?NodeId {
        return self.map.get(name);
    }

    pub fn deinit(self: *FileIndex, allocator: std.mem.Allocator) void {
        self.map.deinit(allocator);
    }
};

// =========================================================================
// Shared utility functions
// =========================================================================

pub fn isIdentChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_';
}

pub fn isWhitespace(c: u8) bool {
    return c == ' ' or c == '\t' or c == '\n' or c == '\r';
}
