const std = @import("std");

/// A local resource type with init and deinit.
pub const Resource = struct {
    data: u64,

    pub fn init(d: u64) Resource {
        return .{ .data = d };
    }

    pub fn deinit(self: *Resource) void {
        self.data = 0;
    }
};

/// Calls ArenaAllocator.init — "init" collides with Resource.init.
/// Should not create a calls edge to Resource.init.
pub fn externalInit(child_allocator: std.mem.Allocator) std.heap.ArenaAllocator {
    return std.heap.ArenaAllocator.init(child_allocator);
}

/// Calls arena.deinit() — "deinit" collides with Resource.deinit.
/// Should not create a calls edge to Resource.deinit.
pub fn externalDeinit(arena: *std.heap.ArenaAllocator) void {
    arena.deinit();
}

/// Explicitly calls the local Resource.init via qualified path.
/// Should create a calls edge to Resource.init.
pub fn createLocalResource() Resource {
    return Resource.init(42);
}
