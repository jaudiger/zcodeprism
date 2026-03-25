const std = @import("std");
const graph_mod = @import("graph.zig");

const Graph = graph_mod.Graph;

/// A versioned snapshot of the code graph with reference counting.
/// Heap-allocated via `create`. The caller is responsible for calling
/// `destroy` once all guards have been released (ref_count == 0).
pub const GraphGeneration = struct {
    graph: Graph,
    arena: std.heap.ArenaAllocator,
    ref_count: std.atomic.Value(u32),
    source_hash: [12]u8,
    generation_id: u64,
    indexed_at: i128,

    /// Heap-allocate a new generation.
    pub fn create(allocator: std.mem.Allocator, generation_id: u64, source_hash: [12]u8) !*GraphGeneration {
        const gen = try allocator.create(GraphGeneration);
        const arena = std.heap.ArenaAllocator.init(allocator);
        gen.* = .{
            .graph = Graph.init(""),
            .arena = arena,
            .ref_count = std.atomic.Value(u32).init(0),
            .source_hash = source_hash,
            .generation_id = generation_id,
            .indexed_at = std.time.nanoTimestamp(),
        };
        return gen;
    }

    /// Free the generation's graph, arena, and struct. Only safe when
    /// ref_count is 0 (all guards have been released).
    pub fn destroy(self: *GraphGeneration, allocator: std.mem.Allocator) void {
        self.graph.deinit(self.arena.allocator());
        self.arena.deinit();
        allocator.destroy(self);
    }

    /// RAII guard that decrements the reference count on deinit.
    pub const Guard = struct {
        gen: *GraphGeneration,

        pub fn deinit(self: Guard) void {
            const prev = self.gen.ref_count.fetchSub(1, .monotonic);
            std.debug.assert(prev > 0);
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
    const gen = try GraphGeneration.create(std.testing.allocator, 1, "abcdef123456".*);
    defer gen.destroy(std.testing.allocator);

    // Act
    const guard = gen.acquire();

    // Assert
    try std.testing.expectEqual(@as(u32, 1), gen.ref_count.load(.monotonic));

    // Cleanup
    guard.deinit();
}

test "guard deinit decrements refcount" {
    // Arrange
    const gen = try GraphGeneration.create(std.testing.allocator, 1, "abcdef123456".*);
    defer gen.destroy(std.testing.allocator);
    const g1 = gen.acquire();
    const g2 = gen.acquire();

    // Act
    g2.deinit();

    // Assert
    try std.testing.expectEqual(@as(u32, 1), gen.ref_count.load(.monotonic));

    // Cleanup
    g1.deinit();
}

test "multiple acquires and releases" {
    // Arrange
    const gen = try GraphGeneration.create(std.testing.allocator, 1, "abcdef123456".*);
    defer gen.destroy(std.testing.allocator);

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
    const gen = try GraphGeneration.create(std.testing.allocator, 42, "abcdef123456".*);
    defer gen.destroy(std.testing.allocator);
    const guard = gen.acquire();
    defer guard.deinit();

    // Assert
    try std.testing.expectEqual(@as(u64, 42), gen.generation_id);
}
