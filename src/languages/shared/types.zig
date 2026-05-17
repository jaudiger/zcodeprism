const std = @import("std");
const types = @import("../../core/types.zig");
const NodeId = types.NodeId;
const EdgeType = types.EdgeType;

/// Maximum depth for field_expression or scoped_identifier chains.
pub const max_chain_depth: usize = 16;

/// Maximum AST depth for scan functions that recurse into the tree.
pub const max_ast_scan_depth: u32 = 256;

/// Shallow search bound for probing a small AST neighborhood.
pub const max_shallow_search_depth: u32 = 8;

/// A symbol origin: identifies a node within a target file by file id and access chain.
/// An empty chain refers to the module itself; a non-empty chain contains the
/// identifier segments extracted from post-import field accesses or use declarations.
pub const SymbolOrigin = struct {
    file_id: NodeId,
    chain: []const []const u8,
};

/// A single import binding: the local name, resolved target file, and
/// any post-import extraction chain.
pub const ImportEntry = struct {
    name: []const u8,
    file_id: NodeId,
    chain: [max_chain_depth][]const u8 = undefined,
    chain_len: usize = 0,
    /// True when the binding comes from a public re-export declaration.
    is_reexport: bool = false,
};

/// Build a SymbolOrigin by scanning a slice of name-keyed records.
/// Works for any record with `name: []const u8`, `file_id: NodeId`,
/// `chain: [N][]const u8`, and `chain_len: usize` (ImportEntry, ParamOrigin, ...).
/// Returns null if no entry matches `name`.
pub fn findOriginByName(items: anytype, name: []const u8) ?SymbolOrigin {
    for (items) |*entry| {
        if (std.mem.eql(u8, entry.name, name)) {
            return .{
                .file_id = entry.file_id,
                .chain = entry.chain[0..entry.chain_len],
            };
        }
    }
    return null;
}

/// A glob import target paired with its visibility at the import site.
pub const GlobTarget = struct {
    target: NodeId,
    is_public: bool,
};

/// Context for cross-file edge creation during a single file parse.
/// Holds the file's node scope range and a dynamically-sized table of import
/// bindings mapped to their resolved target file NodeIds and extraction chains.
/// Languages that do not use glob imports leave glob_targets empty.
pub const EdgeContext = struct {
    scope_start: usize,
    scope_end: usize,
    imports: std.ArrayListUnmanaged(ImportEntry) = .empty,
    glob_targets: std.ArrayListUnmanaged(GlobTarget) = .empty,

    /// Release the imports and glob_targets lists.
    pub fn deinit(self: *EdgeContext, allocator: std.mem.Allocator) void {
        self.imports.deinit(allocator);
        self.glob_targets.deinit(allocator);
    }

    /// Look up the target file NodeId for an import binding by name.
    /// Returns the file_id only, ignoring any extraction chain.
    pub fn findImportTarget(self: *const EdgeContext, name: []const u8) ?NodeId {
        const origin = findOriginByName(self.imports.items, name) orelse return null;
        return origin.file_id;
    }

    /// Look up the full SymbolOrigin (file id + extraction chain) for an import binding by name.
    pub fn findImportOrigin(self: *const EdgeContext, name: []const u8) ?SymbolOrigin {
        return findOriginByName(self.imports.items, name);
    }
};

/// A resolved edge target from qualified chain resolution.
pub const ResolvedEdge = struct {
    target_id: NodeId,
    edge_type: EdgeType,
};
