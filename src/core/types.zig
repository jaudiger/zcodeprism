const std = @import("std");

/// Strongly-typed index that identifies a node in the code graph.
/// Uses the newtype-index pattern over u64; the sentinel `root` (0)
/// always refers to the synthetic root node of the graph.
pub const NodeId = enum(u64) {
    /// The synthetic root node that parents every top-level file node.
    root = 0,
    _,
};

/// Strongly-typed index that identifies an edge in the code graph.
/// Uses the newtype-index pattern over u64.
pub const EdgeId = enum(u64) {
    _,
};

/// The semantic kind of a node in the code graph.
/// Each variant maps to a first-class declaration concept in the
/// supported languages.
pub const NodeKind = enum {
    /// A source file (one node per parsed file).
    file,
    /// A namespace or sub-module within a file.
    module,
    /// A function or method declaration.
    function,
    /// A struct or opaque type definition (container that can hold fields/methods).
    type_def,
    /// An enum type definition.
    enum_def,
    /// A struct/union/enum field.
    field,
    /// A compile-time or runtime constant (`const` declaration).
    constant,
    /// A `test` block declaration.
    test_def,
    /// An error value or error-set definition.
    error_def,
    /// An `@import` declaration that brings in another module.
    import_decl,
    /// A tagged or bare union type definition.
    union_def,
    /// A filesystem directory containing source files.
    directory,

    /// Returns true for type containers that can hold children:
    /// struct, enum, union.
    pub fn isTypeContainer(self: NodeKind) bool {
        return switch (self) {
            .type_def, .enum_def, .union_def => true,
            else => false,
        };
    }
};

/// The semantic relationship between two nodes in the code graph.
/// An edge always points from a source node to a target node.
pub const EdgeType = enum {
    /// Source node reads a field on the target field node.
    accesses_field,
    /// Source node invokes or calls the target node (function call).
    calls,
    /// Source node imports the target module or file.
    imports,
    /// Source node references the target as a type (parameter, return, field type).
    uses_type,
    /// Source and target nodes have structurally similar signatures or bodies.
    similar_to,
    /// Source file/module exports the target declaration publicly.
    exports,
    /// Source node implements the target interface or trait.
    implements,
    /// Directory or module contains the target file or sub-directory.
    contains,
};

/// How an edge was discovered during indexing.
/// Packed as u3 to minimize per-edge storage overhead.
pub const EdgeSource = enum(u3) {
    /// Discovered by static analysis of the tree-sitter AST.
    tree_sitter,
    /// Discovered via LSP queries.
    lsp,
    /// Inferred edge to a phantom node representing an external symbol.
    phantom,
    /// Discovered by workspace-level cross-file resolution.
    workspace,
};

/// Visibility of a declaration as seen from outside its parent scope.
pub const Visibility = enum {
    /// Exported with `pub`; accessible from other modules.
    public,
    /// Not exported; accessible only within the declaring scope.
    private,
};

/// Traversal direction for neighbor queries on the code graph.
pub const Direction = enum {
    /// Follow outgoing edges (source -> target).
    out,
    /// Follow incoming edges (target <- source).
    in,
    /// Follow edges in both directions.
    both,
};

/// Programming languages supported by the indexer and visitor pipeline.
/// Each variant corresponds to a language-specific visitor implementation.
pub const Language = enum {
    /// Rust language support.
    rust,
    /// Zig language support.
    zig,
};

/// Parse a string into any enum type by matching against field names.
pub fn parseEnum(comptime E: type, name: []const u8) ?E {
    inline for (@typeInfo(E).@"enum".fields) |f| {
        if (std.mem.eql(u8, name, f.name)) return @enumFromInt(f.value);
    }
    return null;
}

/// Parse a NodeId from a string in the given radix.
pub fn parseNodeId(s: []const u8, radix: u8) ?NodeId {
    const v = std.fmt.parseInt(u64, s, radix) catch return null;
    return @enumFromInt(v);
}

test "NodeId is 8 bytes" {
    comptime {
        std.debug.assert(@sizeOf(NodeId) == 8);
    }
}

test "EdgeId is 8 bytes" {
    comptime {
        std.debug.assert(@sizeOf(EdgeId) == 8);
    }
}

test "NodeKind has exactly 12 variants" {
    comptime {
        const fields = @typeInfo(NodeKind).@"enum".fields;
        std.debug.assert(fields.len == 12);
    }
}

test "isTypeContainer returns true for type_def, enum_def, union_def" {
    try std.testing.expect(NodeKind.type_def.isTypeContainer());
    try std.testing.expect(NodeKind.enum_def.isTypeContainer());
    try std.testing.expect(NodeKind.union_def.isTypeContainer());
    try std.testing.expect(!NodeKind.function.isTypeContainer());
    try std.testing.expect(!NodeKind.field.isTypeContainer());
    try std.testing.expect(!NodeKind.constant.isTypeContainer());
    try std.testing.expect(!NodeKind.file.isTypeContainer());
    try std.testing.expect(!NodeKind.module.isTypeContainer());
    try std.testing.expect(!NodeKind.test_def.isTypeContainer());
    try std.testing.expect(!NodeKind.error_def.isTypeContainer());
    try std.testing.expect(!NodeKind.import_decl.isTypeContainer());
    try std.testing.expect(!NodeKind.directory.isTypeContainer());
}

test "EdgeType has exactly 8 variants" {
    comptime {
        const fields = @typeInfo(EdgeType).@"enum".fields;
        std.debug.assert(fields.len == 8);
    }
}

test "EdgeSource is 1 byte or less" {
    comptime {
        std.debug.assert(@sizeOf(EdgeSource) <= 1);
    }
}

test "Visibility has exactly 2 variants" {
    comptime {
        const fields = @typeInfo(Visibility).@"enum".fields;
        std.debug.assert(fields.len == 2);
    }
}
