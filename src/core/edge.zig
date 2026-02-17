const std = @import("std");
const types = @import("types.zig");

const NodeId = types.NodeId;
const EdgeType = types.EdgeType;
const EdgeSource = types.EdgeSource;

/// An edge in the code graph representing a relationship between two nodes.
pub const Edge = struct {
    source_id: NodeId,
    target_id: NodeId,
    edge_type: EdgeType,
    source: EdgeSource = .tree_sitter,
};

// --- Tests ---

test "edge with calls type" {
    // Arrange
    const e = Edge{
        .source_id = .root,
        .target_id = @enumFromInt(1),
        .edge_type = .calls,
    };

    // Assert
    try std.testing.expectEqual(EdgeType.calls, e.edge_type);
}

test "edge with imports type" {
    // Arrange
    const e = Edge{
        .source_id = .root,
        .target_id = @enumFromInt(1),
        .edge_type = .imports,
    };

    // Assert
    try std.testing.expectEqual(EdgeType.imports, e.edge_type);
}

test "edge with uses_type type" {
    // Arrange
    const e = Edge{
        .source_id = .root,
        .target_id = @enumFromInt(1),
        .edge_type = .uses_type,
    };

    // Assert
    try std.testing.expectEqual(EdgeType.uses_type, e.edge_type);
}

test "edge with similar_to type" {
    // Arrange
    const e = Edge{
        .source_id = .root,
        .target_id = @enumFromInt(1),
        .edge_type = .similar_to,
    };

    // Assert
    try std.testing.expectEqual(EdgeType.similar_to, e.edge_type);
}

test "edge with exports type" {
    // Arrange
    const e = Edge{
        .source_id = .root,
        .target_id = @enumFromInt(1),
        .edge_type = .exports,
    };

    // Assert
    try std.testing.expectEqual(EdgeType.exports, e.edge_type);
}

test "edge with implements type" {
    // Arrange
    const e = Edge{
        .source_id = .root,
        .target_id = @enumFromInt(1),
        .edge_type = .implements,
    };

    // Assert
    try std.testing.expectEqual(EdgeType.implements, e.edge_type);
}

test "edge with tree_sitter source" {
    // Arrange
    const e = Edge{
        .source_id = .root,
        .target_id = @enumFromInt(1),
        .edge_type = .calls,
        .source = .tree_sitter,
    };

    // Assert
    try std.testing.expectEqual(EdgeSource.tree_sitter, e.source);
}

test "edge with lsp source" {
    // Arrange
    const e = Edge{
        .source_id = .root,
        .target_id = @enumFromInt(1),
        .edge_type = .calls,
        .source = .lsp,
    };

    // Assert
    try std.testing.expectEqual(EdgeSource.lsp, e.source);
}

test "edge with phantom source" {
    // Arrange
    const e = Edge{
        .source_id = .root,
        .target_id = @enumFromInt(1),
        .edge_type = .uses_type,
        .source = .phantom,
    };

    // Assert
    try std.testing.expectEqual(EdgeSource.phantom, e.source);
}

test "edge with workspace source" {
    // Arrange
    const e = Edge{
        .source_id = .root,
        .target_id = @enumFromInt(1),
        .edge_type = .imports,
        .source = .workspace,
    };

    // Assert
    try std.testing.expectEqual(EdgeSource.workspace, e.source);
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
