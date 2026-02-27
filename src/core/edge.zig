const std = @import("std");
const types = @import("types.zig");

const NodeId = types.NodeId;
const EdgeType = types.EdgeType;
const EdgeSource = types.EdgeSource;

/// Deduplication key for edges: the (source_id, target_id, edge_type) triple.
///
/// Two edges with the same key are considered duplicates regardless of their
/// discovery source. Used as the key type in edge sets to prevent inserting
/// the same semantic relationship more than once.
pub const EdgeKey = struct {
    /// Node where the relationship originates.
    source_id: NodeId,
    /// Node where the relationship points to.
    target_id: NodeId,
    /// Semantic kind of the relationship.
    edge_type: EdgeType,
};

/// A directed edge in the code graph, connecting two nodes with a typed
/// semantic relationship (e.g. calls, imports, uses_type).
///
/// Each edge also records how it was discovered via the `source` field,
/// but the discovery source is not part of the deduplication key.
pub const Edge = struct {
    /// Node where the relationship originates.
    source_id: NodeId,
    /// Node where the relationship points to.
    target_id: NodeId,
    /// Semantic kind of the relationship.
    edge_type: EdgeType,
    /// How this edge was discovered. Defaults to `.tree_sitter` because most
    /// edges are created during AST-based parsing.
    source: EdgeSource = .tree_sitter,

    /// Returns the deduplication key for this edge, stripping the discovery source.
    pub fn key(self: Edge) EdgeKey {
        return .{ .source_id = self.source_id, .target_id = self.target_id, .edge_type = self.edge_type };
    }
};

test "edge types are correctly stored" {
    // Arrange
    const edge_types = [_]EdgeType{ .calls, .imports, .uses_type, .similar_to, .exports, .implements };

    // Act / Assert
    for (edge_types) |et| {
        const e = Edge{
            .source_id = .root,
            .target_id = @enumFromInt(1),
            .edge_type = et,
        };
        try std.testing.expectEqual(et, e.edge_type);
    }
}

test "edge sources are correctly stored" {
    // Arrange
    const sources = [_]EdgeSource{ .tree_sitter, .lsp, .phantom, .workspace };

    // Act / Assert
    for (sources) |src| {
        const e = Edge{
            .source_id = .root,
            .target_id = @enumFromInt(1),
            .edge_type = .calls,
            .source = src,
        };
        try std.testing.expectEqual(src, e.source);
    }
}

test "edge default source is tree_sitter" {
    // Arrange
    const e = Edge{
        .source_id = .root,
        .target_id = @enumFromInt(1),
        .edge_type = .calls,
    };

    // Assert
    try std.testing.expectEqual(EdgeSource.tree_sitter, e.source);
}
