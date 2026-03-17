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
    indexed_at: i128,

    /// Create a new generation with the given id and source hash.
    /// Allocates an internal arena from `backing_allocator` for the graph.
    pub fn init(backing_allocator: std.mem.Allocator, generation_id: u64, source_hash: [12]u8) GraphGeneration {
        const arena = std.heap.ArenaAllocator.init(backing_allocator);
        return .{
            .graph = Graph.init(""),
            .arena = arena,
            .ref_count = std.atomic.Value(u32).init(0),
            .source_hash = source_hash,
            .generation_id = generation_id,
            .indexed_at = std.time.nanoTimestamp(),
        };
    }

    /// RAII guard that decrements the reference count on deinit.
    pub const Guard = struct {
        gen: *GraphGeneration,

        /// Decrement the reference count. When it reaches zero, frees the arena.
        pub fn deinit(self: Guard) void {
            const prev = self.gen.ref_count.fetchSub(1, .monotonic);
            if (prev == 1) {
                self.gen.graph.deinit(self.gen.arena.allocator());
                self.gen.arena.deinit();
            }
        }
    };

    /// Increment the reference count and return a guard that will release it.
    pub fn acquire(self: *GraphGeneration) Guard {
        const prev = self.ref_count.fetchAdd(1, .monotonic);
        std.debug.assert(prev != std.math.maxInt(u32));
        return .{ .gen = self };
    }
};

test "acquire increments refcount" {
    // Arrange
    var gen = GraphGeneration.init(std.testing.allocator, 1, "abcdef123456".*);

    // Act
    const guard = gen.acquire();

    // Assert
    try std.testing.expectEqual(@as(u32, 1), gen.ref_count.load(.monotonic));

    // Cleanup
    guard.deinit();
}

test "guard deinit decrements refcount" {
    // Arrange
    var gen = GraphGeneration.init(std.testing.allocator, 1, "abcdef123456".*);
    const g1 = gen.acquire();
    const g2 = gen.acquire();

    // Act
    g2.deinit();

    // Assert
    try std.testing.expectEqual(@as(u32, 1), gen.ref_count.load(.monotonic));

    // Cleanup
    g1.deinit();
}

test "last guard deinit frees arena" {
    // Arrange
    var gen = GraphGeneration.init(std.testing.allocator, 1, "abcdef123456".*);
    const guard = gen.acquire();

    // Act
    guard.deinit();

    // Assert
    try std.testing.expectEqual(@as(u32, 0), gen.ref_count.load(.monotonic));
}

test "multiple acquires and releases" {
    // Arrange
    var gen = GraphGeneration.init(std.testing.allocator, 1, "abcdef123456".*);

    // Act
    const g1 = gen.acquire();
    const g2 = gen.acquire();
    const g3 = gen.acquire();

    // Assert
    try std.testing.expectEqual(@as(u32, 3), gen.ref_count.load(.monotonic));

    g3.deinit();
    try std.testing.expectEqual(@as(u32, 2), gen.ref_count.load(.monotonic));
    g2.deinit();
    try std.testing.expectEqual(@as(u32, 1), gen.ref_count.load(.monotonic));
    g1.deinit();
    try std.testing.expectEqual(@as(u32, 0), gen.ref_count.load(.monotonic));
}

test "generation_id is set" {
    // Arrange
    var gen = GraphGeneration.init(std.testing.allocator, 42, "abcdef123456".*);
    const guard = gen.acquire();
    defer guard.deinit();

    // Assert
    try std.testing.expectEqual(@as(u64, 42), gen.generation_id);
}
