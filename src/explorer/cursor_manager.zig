const std = @import("std");
const types = @import("../core/types.zig");
const cursor_mod = @import("cursor.zig");

const NodeId = types.NodeId;
const Cursor = cursor_mod.Cursor;

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
    pub fn createCursor(self: *CursorManager, position: NodeId) ![]const u8 {
        const alloc = self.arena.allocator();
        const id_num = self.next_id;
        self.next_id += 1;

        var buf: [20]u8 = undefined;
        const hex = std.fmt.bufPrint(&buf, "cur_{x}", .{id_num}) catch unreachable;
        const id_str = try alloc.dupe(u8, hex);

        const cursor = Cursor.init(position);
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
