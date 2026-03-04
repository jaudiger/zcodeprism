const std = @import("std");
const types = @import("../../core/types.zig");
const NodeId = types.NodeId;
const EdgeType = types.EdgeType;

/// Maximum depth for field_expression or scoped_identifier chains.
pub const max_chain_depth: usize = 16;

/// Maximum AST depth for scan functions that recurse into the tree.
pub const max_ast_scan_depth: u32 = 256;

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
    target: NodeId,
    chain: [max_chain_depth][]const u8 = undefined,
    chain_len: usize = 0,
};

/// Context for cross-file edge creation during a single file parse.
/// Holds the file's node scope range and a dynamically-sized table of import
/// bindings mapped to their resolved target file NodeIds and extraction chains.
/// The glob_targets list is Rust-specific; Zig code never populates it.
pub const EdgeContext = struct {
    scope_start: usize,
    scope_end: usize,
    imports: std.ArrayListUnmanaged(ImportEntry) = .empty,
    glob_targets: std.ArrayListUnmanaged(NodeId) = .empty,

    pub fn deinit(self: *EdgeContext, allocator: std.mem.Allocator) void {
        self.imports.deinit(allocator);
        self.glob_targets.deinit(allocator);
    }

    /// Look up the target file NodeId for an import binding by name.
    /// Returns the file_id only, ignoring any extraction chain.
    pub fn findImportTarget(self: *const EdgeContext, name: []const u8) ?NodeId {
        for (self.imports.items) |entry| {
            if (std.mem.eql(u8, entry.name, name)) return entry.target;
        }
        return null;
    }

    /// Look up the full SymbolOrigin (file id + extraction chain) for an import binding by name.
    pub fn findImportOrigin(self: *const EdgeContext, name: []const u8) ?SymbolOrigin {
        for (self.imports.items) |*entry| {
            if (std.mem.eql(u8, entry.name, name)) {
                return .{
                    .file_id = entry.target,
                    .chain = entry.chain[0..entry.chain_len],
                };
            }
        }
        return null;
    }
};

/// A single variable-to-file binding.
pub const VarBinding = struct {
    name: []const u8,
    target: NodeId,
};

/// Tracks variable-to-file bindings within a function scope.
/// Stores mappings from local variable names to the target file NodeId they
/// were assigned from via import-qualified expressions, so that later method
/// calls can be resolved to the correct cross-file target.
pub const VarTracker = struct {
    bindings: std.ArrayListUnmanaged(VarBinding) = .empty,

    pub fn deinit(self: *VarTracker, allocator: std.mem.Allocator) void {
        self.bindings.deinit(allocator);
    }

    /// Record a variable-to-file binding.
    pub fn addBinding(self: *VarTracker, allocator: std.mem.Allocator, name: []const u8, target_file: NodeId) !void {
        try self.bindings.append(allocator, .{ .name = name, .target = target_file });
    }

    /// Return the target file NodeId associated with a variable name, or null if not tracked.
    pub fn findTarget(self: *const VarTracker, name: []const u8) ?NodeId {
        for (self.bindings.items) |b| {
            if (std.mem.eql(u8, b.name, name)) return b.target;
        }
        return null;
    }
};

/// A resolved edge target from qualified chain resolution.
pub const ResolvedEdge = struct {
    target_id: NodeId,
    edge_type: EdgeType,
};

/// A local variable bound to a type name inferred from its initializer.
pub const TypeBinding = struct {
    var_name: []const u8,
    type_name: []const u8,
};

/// Tracks local variable bindings from initializers to their inferred struct types.
/// Populated during prescan, queried during call resolution.
pub const LocalTypeTracker = struct {
    bindings: std.ArrayListUnmanaged(TypeBinding) = .empty,

    pub fn deinit(self: *LocalTypeTracker, allocator: std.mem.Allocator) void {
        self.bindings.deinit(allocator);
    }

    /// Record that local variable `name` was initialized from type `type_name`.
    pub fn addBinding(self: *LocalTypeTracker, allocator: std.mem.Allocator, name: []const u8, type_name: []const u8) !void {
        try self.bindings.append(allocator, .{ .var_name = name, .type_name = type_name });
    }

    /// Return the type name bound to `name`, or null if not tracked.
    pub fn findTypeName(self: *const LocalTypeTracker, name: []const u8) ?[]const u8 {
        for (self.bindings.items) |b| {
            if (std.mem.eql(u8, b.var_name, name)) return b.type_name;
        }
        return null;
    }
};
