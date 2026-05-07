const std = @import("std");
const types = @import("../core/types.zig");
const cursor_mod = @import("cursor.zig");

const NodeId = types.NodeId;
const Cursor = cursor_mod.Cursor;

pub const CursorOptions = struct {
    scope: ?[]const u8 = null,
    include_tests: bool = false,
    include_external_nodes: bool = false,
};

/// Manages exploration cursors with arena-backed storage.
pub const CursorManager = struct {
    arena: std.heap.ArenaAllocator,
    cursors: std.StringHashMapUnmanaged(Cursor),
    next_id: u64,

    /// Create an empty cursor manager backed by a page allocator arena.
    pub fn init() CursorManager {
        return .{
            .arena = std.heap.ArenaAllocator.init(std.heap.page_allocator),
            .cursors = .{},
            .next_id = 1,
        };
    }

    /// Release all arena memory and invalidate the manager.
    pub fn deinit(self: *CursorManager) void {
        self.arena.deinit();
        self.* = undefined;
    }

    /// Create a cursor at the given position. Returns the cursor ID string.
    pub fn createCursor(self: *CursorManager, position: NodeId, options: CursorOptions) ![]const u8 {
        const alloc = self.arena.allocator();
        const id_num = self.next_id;
        self.next_id += 1;

        const id_str = try std.fmt.allocPrint(alloc, "cur_{x}", .{id_num});

        var cursor = Cursor.init(alloc, position);
        cursor.scope = if (options.scope) |s| try alloc.dupe(u8, s) else null;
        cursor.include_tests = options.include_tests;
        cursor.include_external_nodes = options.include_external_nodes;
        try self.cursors.put(alloc, id_str, cursor);
        return id_str;
    }

    /// Look up a cursor by its ID string. Returns null if not found.
    pub fn getCursor(self: *CursorManager, id: []const u8) ?*Cursor {
        return self.cursors.getPtr(id);
    }

    /// Remove a cursor by ID. Returns true if it existed.
    pub fn closeCursor(self: *CursorManager, id: []const u8) bool {
        return self.cursors.remove(id);
    }
};
