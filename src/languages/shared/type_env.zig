const std = @import("std");
const types_mod = @import("../../core/types.zig");
const shared_types = @import("types.zig");

const NodeId = types_mod.NodeId;
const SymbolOrigin = shared_types.SymbolOrigin;
const max_chain_depth = shared_types.max_chain_depth;

/// A cross-file parameter binding: param name -> origin file + member chain.
pub const ParamOrigin = struct {
    name: []const u8,
    file_id: NodeId,
    chain: [max_chain_depth][]const u8 = undefined,
    chain_len: usize = 0,
};

/// Type environment for a single function body.
///
/// local         - local name to resolved type NodeId bindings.
/// cross_file    - local name to cross-file target file NodeId bindings.
/// param_origins - cross-file parameter origin bindings.
pub const TypeEnv = struct {
    local: std.StringHashMapUnmanaged(NodeId) = .{},
    cross_file: std.StringHashMapUnmanaged(NodeId) = .{},
    param_origins: std.ArrayListUnmanaged(ParamOrigin) = .empty,

    pub fn deinit(self: *TypeEnv, allocator: std.mem.Allocator) void {
        self.local.deinit(allocator);
        self.cross_file.deinit(allocator);
        self.param_origins.deinit(allocator);
    }

    pub fn bindLocal(self: *TypeEnv, allocator: std.mem.Allocator, name: []const u8, type_id: NodeId) !void {
        try self.local.put(allocator, name, type_id);
    }

    pub fn bindCrossFile(self: *TypeEnv, allocator: std.mem.Allocator, name: []const u8, file_id: NodeId) !void {
        try self.cross_file.put(allocator, name, file_id);
    }

    pub fn addParamOrigin(self: *TypeEnv, allocator: std.mem.Allocator, name: []const u8, file_id: NodeId, chain: []const []const u8) !void {
        if (chain.len == 0) return;
        var entry = ParamOrigin{ .name = name, .file_id = file_id };
        const copy_len = @min(chain.len, max_chain_depth);
        for (chain[0..copy_len], 0..) |seg, i| {
            entry.chain[i] = seg;
        }
        entry.chain_len = copy_len;
        try self.param_origins.append(allocator, entry);
    }

    pub fn findParamOrigin(self: *const TypeEnv, name: []const u8) ?SymbolOrigin {
        for (self.param_origins.items) |*b| {
            if (std.mem.eql(u8, b.name, name)) {
                return .{ .file_id = b.file_id, .chain = b.chain[0..b.chain_len] };
            }
        }
        return null;
    }
};
