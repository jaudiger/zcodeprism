//! Transient worklist of unresolved references and phantom hover sites
//! collected during the AST edge-builder and phantom-resolution passes.
//! Not serialized, not visible to graph queries.

const std = @import("std");
const types = @import("../core/types.zig");

const NodeId = types.NodeId;

/// File-relative path and 0-based LSP position of a single phantom usage site.
/// Slice fields are not owned; they must outlive this struct.
pub const UsageSite = struct {
    file_path: []const u8,
    line: u32,
    col: u32,
    hint_name: ?[]const u8 = null,
};

/// LSP method to invoke for a worklist entry.
pub const QueryKind = enum {
    /// textDocument/definition: resolve an unknown call or reference target.
    definition,
    /// textDocument/typeDefinition: resolve the type of an inferred variable.
    type_definition,
    /// textDocument/hover: extract error set or signature information.
    hover,
    /// textDocument/references: find all reference sites for a symbol.
    references,
};

/// A single unresolved reference recorded during the AST pass.
/// All slice fields borrow from the graph or source text and must
/// outlive the worklist.
pub const WorklistEntry = struct {
    /// Graph node containing the unresolved reference (the caller).
    source_node_id: NodeId,
    /// Relative file path within the project.
    file_path: []const u8,
    /// 0-based line position from the tree-sitter AST node.
    line: u32,
    /// 0-based column position from the tree-sitter AST node.
    col: u32,
    /// LSP method to use when resolving this entry.
    query_kind: QueryKind,
    /// Identifier the AST walker saw but could not resolve.
    hint_name: ?[]const u8 = null,
};

/// Two-list worklist produced by the indexer pipeline. `entries` holds
/// unresolved AST references consumed by `dispatchWorklist`. `phantom_hovers`
/// holds external-symbol hover sites consumed by `enrichPhantoms`.
pub const LspWorklist = struct {
    /// Unresolved AST references: definition, type_definition, hover for
    /// local functions, and references queries.
    entries: std.ArrayList(WorklistEntry) = .{},
    /// Hover sites for phantom (external) nodes, one entry per phantom NodeId.
    phantom_hovers: std.ArrayList(WorklistEntry) = .{},

    pub fn deinit(self: *LspWorklist, allocator: std.mem.Allocator) void {
        self.entries.deinit(allocator);
        self.phantom_hovers.deinit(allocator);
    }

    /// Append an unresolved AST reference to the entries list.
    pub fn append(self: *LspWorklist, allocator: std.mem.Allocator, entry: WorklistEntry) !void {
        try self.entries.append(allocator, entry);
    }

    /// Append a phantom hover site to the phantom_hovers list.
    pub fn appendPhantomHover(self: *LspWorklist, allocator: std.mem.Allocator, entry: WorklistEntry) !void {
        try self.phantom_hovers.append(allocator, entry);
    }

    /// Read-only slice of unresolved AST reference entries.
    pub fn items(self: *const LspWorklist) []const WorklistEntry {
        return self.entries.items;
    }

    /// Read-only slice of phantom hover entries.
    pub fn phantomHovers(self: *const LspWorklist) []const WorklistEntry {
        return self.phantom_hovers.items;
    }

    /// Number of entries. Phantom hover entries are not counted.
    pub fn count(self: *const LspWorklist) usize {
        return self.entries.items.len;
    }
};

test "LspWorklist append and read back" {
    // Arrange
    const allocator = std.testing.allocator;
    var wl = LspWorklist{};
    defer wl.deinit(allocator);

    // Act
    try wl.append(allocator, .{
        .source_node_id = @enumFromInt(5),
        .file_path = "src/main.zig",
        .line = 10,
        .col = 4,
        .query_kind = .definition,
        .hint_name = "doStuff",
    });
    try wl.append(allocator, .{
        .source_node_id = @enumFromInt(7),
        .file_path = "src/main.zig",
        .line = 20,
        .col = 8,
        .query_kind = .type_definition,
    });

    // Assert
    try std.testing.expectEqual(@as(usize, 2), wl.count());
    try std.testing.expectEqual(QueryKind.definition, wl.items()[0].query_kind);
    try std.testing.expectEqualStrings("doStuff", wl.items()[0].hint_name.?);
    try std.testing.expectEqual(QueryKind.type_definition, wl.items()[1].query_kind);
    try std.testing.expect(wl.items()[1].hint_name == null);
}

test "LspWorklist empty has zero count and empty slices" {
    // Arrange
    const allocator = std.testing.allocator;
    var wl = LspWorklist{};
    defer wl.deinit(allocator);

    // Assert
    try std.testing.expectEqual(@as(usize, 0), wl.count());
    try std.testing.expectEqual(@as(usize, 0), wl.items().len);
    try std.testing.expectEqual(@as(usize, 0), wl.phantomHovers().len);
}

test "LspWorklist phantom_hovers are separate from entries" {
    // Arrange
    const allocator = std.testing.allocator;
    var wl = LspWorklist{};
    defer wl.deinit(allocator);

    // Act
    try wl.append(allocator, .{
        .source_node_id = @enumFromInt(1),
        .file_path = "src/a.zig",
        .line = 0,
        .col = 0,
        .query_kind = .definition,
    });
    try wl.appendPhantomHover(allocator, .{
        .source_node_id = @enumFromInt(42),
        .file_path = "src/b.zig",
        .line = 5,
        .col = 2,
        .query_kind = .hover,
        .hint_name = "std",
    });

    // Assert: phantom hover appears in phantomHovers, not in items or count
    try std.testing.expectEqual(@as(usize, 1), wl.count());
    try std.testing.expectEqual(@as(usize, 1), wl.items().len);
    try std.testing.expectEqual(@as(usize, 1), wl.phantomHovers().len);
    try std.testing.expectEqual(QueryKind.hover, wl.phantomHovers()[0].query_kind);
    try std.testing.expectEqualStrings("std", wl.phantomHovers()[0].hint_name.?);
}

test "QueryKind has exactly 4 variants" {
    comptime {
        std.debug.assert(@typeInfo(QueryKind).@"enum".fields.len == 4);
    }
}
