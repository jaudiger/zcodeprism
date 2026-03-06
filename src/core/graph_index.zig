const std = @import("std");
const types = @import("types.zig");
const edge_mod = @import("edge.zig");
const scope_index_mod = @import("scope_index.zig");
const name_index_mod = @import("name_index.zig");
const file_index_mod = @import("file_index.zig");
const kind_index_mod = @import("kind_index.zig");
const import_index_mod = @import("import_index.zig");
const node_mod = @import("node.zig");

const Node = node_mod.Node;
const Edge = edge_mod.Edge;

pub const ScopeIndex = scope_index_mod.ScopeIndex;
pub const NameIndex = name_index_mod.NameIndex;
pub const FileIndex = file_index_mod.FileIndex;
pub const KindIndex = kind_index_mod.KindIndex;
pub const ImportIndex = import_index_mod.ImportIndex;

/// Composite index built once from the complete graph after parsing.
/// Packages scope, name, file, kind, and import indexes so every
/// language hook gets uniform access without redundant scans.
pub const GraphIndex = struct {
    scope: ScopeIndex,
    names: NameIndex,
    files: FileIndex,
    kinds: KindIndex,
    imports: ImportIndex = .{},

    /// Build the four node-based indexes from the node array.
    /// Call `buildImportTargets` separately after edge building.
    pub fn build(allocator: std.mem.Allocator, nodes: []const Node) !GraphIndex {
        var scope = try ScopeIndex.build(allocator, nodes, 0);
        errdefer scope.deinit(allocator);
        var names = try NameIndex.build(allocator, nodes, 0);
        errdefer names.deinit(allocator);
        var files = try FileIndex.build(allocator, nodes);
        errdefer files.deinit(allocator);
        const kinds = try KindIndex.build(allocator, nodes);
        errdefer comptime unreachable;
        return .{
            .scope = scope,
            .names = names,
            .files = files,
            .kinds = kinds,
        };
    }

    /// Build the import index from the edge array. Called after edge
    /// building so phantom resolution can look up per-file import targets.
    pub fn buildImportTargets(self: *GraphIndex, allocator: std.mem.Allocator, edges: []const Edge) !void {
        self.imports = try ImportIndex.build(allocator, edges);
    }

    /// Release all five sub-indexes.
    pub fn deinit(self: *GraphIndex, allocator: std.mem.Allocator) void {
        self.imports.deinit(allocator);
        self.scope.deinit(allocator);
        self.names.deinit(allocator);
        self.files.deinit(allocator);
        self.kinds.deinit(allocator);
    }
};

// -- Tests --

test "build on empty nodes succeeds" {
    // Arrange
    const nodes: []const Node = &.{};

    // Act
    var idx = try GraphIndex.build(std.testing.allocator, nodes);
    defer idx.deinit(std.testing.allocator);

    // Assert: all sub-indexes are functional but empty
    try std.testing.expectEqual(@as(usize, 0), idx.scope.childrenOf(.root).len);
    try std.testing.expectEqual(@as(usize, 0), idx.names.findByName("anything").len);
    try std.testing.expectEqual(@as(?types.NodeId, null), idx.files.findByName("any.zig"));
    try std.testing.expectEqual(@as(usize, 0), idx.kinds.findByKind(.function).len);
}

test "build populates all four sub-indexes" {
    // Arrange
    const file_id: types.NodeId = @enumFromInt(0);
    const nodes: []const Node = &.{
        .{ .id = @enumFromInt(0), .name = "src/main.zig", .kind = .file, .language = .zig, .file_path = "src/main.zig" },
        .{ .id = @enumFromInt(1), .name = "main", .kind = .function, .language = .zig, .parent_id = file_id },
        .{ .id = @enumFromInt(2), .name = "Config", .kind = .type_def, .language = .zig, .parent_id = file_id },
    };

    // Act
    var idx = try GraphIndex.build(std.testing.allocator, nodes);
    defer idx.deinit(std.testing.allocator);

    // Assert: scope has 2 children under file
    try std.testing.expectEqual(@as(usize, 2), idx.scope.childrenOf(file_id).len);
    // Assert: name lookup works
    try std.testing.expectEqual(@as(usize, 1), idx.names.findByName("main").len);
    // Assert: file lookup works
    try std.testing.expect(idx.files.findByName("src/main.zig") != null);
    // Assert: kind lookup works
    try std.testing.expectEqual(@as(usize, 1), idx.kinds.findByKind(.function).len);
    try std.testing.expectEqual(@as(usize, 1), idx.kinds.findByKind(.type_def).len);
    try std.testing.expectEqual(@as(usize, 1), idx.kinds.findByKind(.file).len);
}
