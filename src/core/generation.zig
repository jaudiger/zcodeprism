const std = @import("std");
const graph_mod = @import("graph.zig");

const Graph = graph_mod.Graph;

/// A versioned snapshot of the code graph with reference counting.
/// When the last reference is released, the arena is freed.
pub const GraphGeneration = struct {
    graph: Graph,
    arena: std.heap.ArenaAllocator,
    ref_count: std.atomic.Value(u32),
    source_hash: [12]u8,
    generation_id: u64,

    /// Create a new generation with the given id and source hash.
    /// Allocates an internal arena from `backing_allocator` for the graph.
    pub fn init(backing_allocator: std.mem.Allocator, generation_id: u64, source_hash: [12]u8) GraphGeneration {
        var arena = std.heap.ArenaAllocator.init(backing_allocator);
        return .{
            .graph = Graph.init(arena.allocator(), ""),
            .arena = arena,
            .ref_count = std.atomic.Value(u32).init(0),
            .source_hash = source_hash,
            .generation_id = generation_id,
        };
    }

    /// Increment the reference count (a reader has acquired this generation).
    pub fn acquire(self: *GraphGeneration) void {
        _ = self.ref_count.fetchAdd(1, .monotonic);
    }

    /// Decrement the reference count. When it reaches zero, frees the arena.
    pub fn release(self: *GraphGeneration) void {
        const prev = self.ref_count.fetchSub(1, .monotonic);
        if (prev == 1) {
            // Last reference released, free the arena
            self.graph.deinit();
            self.arena.deinit();
        }
    }
};

test "acquire increments refcount" {
    // Arrange
    var gen = GraphGeneration.init(std.testing.allocator, 1, "abcdef123456".*);

    // Act
    gen.acquire();

    // Assert
    try std.testing.expectEqual(@as(u32, 1), gen.ref_count.load(.monotonic));

    // Cleanup
    gen.release();
}

test "release decrements refcount" {
    // Arrange
    var gen = GraphGeneration.init(std.testing.allocator, 1, "abcdef123456".*);
    gen.acquire();
    gen.acquire();

    // Act
    gen.release();

    // Assert
    try std.testing.expectEqual(@as(u32, 1), gen.ref_count.load(.monotonic));

    // Cleanup
    gen.release();
}

test "release on last ref frees arena" {
    // Arrange
    var gen = GraphGeneration.init(std.testing.allocator, 1, "abcdef123456".*);
    gen.acquire();

    // Act: release the last reference, arena should be freed
    gen.release();

    // Assert: ref_count is 0 (arena freed, but the struct itself is on the stack)
    try std.testing.expectEqual(@as(u32, 0), gen.ref_count.load(.monotonic));
    // No leak detected by std.testing.allocator confirms arena was freed
}

test "multiple acquires" {
    // Arrange
    var gen = GraphGeneration.init(std.testing.allocator, 1, "abcdef123456".*);

    // Act: acquire 3 times
    gen.acquire();
    gen.acquire();
    gen.acquire();

    // Assert
    try std.testing.expectEqual(@as(u32, 3), gen.ref_count.load(.monotonic));

    // Release all 3
    gen.release();
    try std.testing.expectEqual(@as(u32, 2), gen.ref_count.load(.monotonic));
    gen.release();
    try std.testing.expectEqual(@as(u32, 1), gen.ref_count.load(.monotonic));
    gen.release();
    try std.testing.expectEqual(@as(u32, 0), gen.ref_count.load(.monotonic));
}

test "generation_id is set" {
    // Arrange
    var gen = GraphGeneration.init(std.testing.allocator, 42, "abcdef123456".*);
    gen.acquire();
    defer gen.release();

    // Assert
    try std.testing.expectEqual(@as(u64, 42), gen.generation_id);
}
