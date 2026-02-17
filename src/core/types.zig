const std = @import("std");

/// Newtype index for graph nodes. Root node always has id 0.
pub const NodeId = enum(u64) {
    root = 0,
    _,
};

/// Newtype index for graph edges.
pub const EdgeId = enum(u64) {
    _,
};

/// The semantic kind of a node in the code graph.
pub const NodeKind = enum {
    file,
    module,
    function,
    type_def,
    enum_def,
    field,
    constant,
    test_def,
    error_def,
    import_decl,
};

/// The semantic relationship between two nodes.
pub const EdgeType = enum {
    calls,
    imports,
    uses_type,
    similar_to,
    exports,
    implements,
};

/// How an edge was discovered.
pub const EdgeSource = enum(u3) {
    tree_sitter,
    lsp,
    phantom,
    workspace,
};

/// Visibility of a declaration.
pub const Visibility = enum {
    public,
    private,
};

/// Supported programming languages.
pub const Language = enum {
    zig,
    rust,
};

// --- Tests ---

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

test "NodeKind has exactly 10 variants" {
    comptime {
        const fields = @typeInfo(NodeKind).@"enum".fields;
        std.debug.assert(fields.len == 10);
    }
}

test "EdgeType has exactly 6 variants" {
    comptime {
        const fields = @typeInfo(EdgeType).@"enum".fields;
        std.debug.assert(fields.len == 6);
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
