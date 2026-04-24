const std = @import("std");
const types = @import("../core/types.zig");

const NodeId = types.NodeId;

/// A user-attached note on a graph node within an exploration session.
pub const Annotation = struct {
    node_id: NodeId,
    tag: []const u8,
    note: ?[]const u8,
};

/// Per-session cursor tracking position and user annotations.
pub const Cursor = struct {
    position: NodeId,
    annotations: std.ArrayList(Annotation),
    scope: ?[]const u8,
    include_tests: bool,
    include_external_nodes: bool,

    /// Create a cursor at the given node position.
    pub fn init(position: NodeId) Cursor {
        return .{
            .position = position,
            .annotations = .empty,
            .scope = null,
            .include_tests = false,
            .include_external_nodes = false,
        };
    }

    /// Append an annotation for `node_id` to this cursor's session.
    pub fn addAnnotation(self: *Cursor, allocator: std.mem.Allocator, node_id: NodeId, tag: []const u8, note: ?[]const u8) !void {
        try self.annotations.append(allocator, .{
            .node_id = node_id,
            .tag = tag,
            .note = note,
        });
    }

    /// Return all annotations recorded on this cursor.
    pub fn getAnnotations(self: *const Cursor) []const Annotation {
        return self.annotations.items;
    }
};
