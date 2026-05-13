const std = @import("std");
const graph_mod = @import("graph.zig");
const types = @import("types.zig");
const refcount_mod = @import("refcount.zig");

const Graph = graph_mod.Graph;
const RefCount = refcount_mod.RefCount;

/// A versioned snapshot of the code graph with reference counting.
/// Heap-allocated via `create` with one outstanding reference. Destroyed
/// when the last reference is dropped via `release` or `Guard.deinit`.
pub const GraphGeneration = struct {
    graph: Graph,
    arena: std.heap.ArenaAllocator,
    ref_count: RefCount,
    source_hash: types.ContentHash,
    generation_id: u64,
    indexed_at: i128,
    allocator: std.mem.Allocator,

    /// Heap-allocate a new generation with an initial reference count of 1.
    pub fn create(allocator: std.mem.Allocator, io: std.Io, generation_id: u64, source_hash: types.ContentHash) !*GraphGeneration {
        const gen = try allocator.create(GraphGeneration);
        gen.* = .{
            .graph = Graph.init(""),
            .arena = std.heap.ArenaAllocator.init(allocator),
            .ref_count = RefCount.init(1),
            .source_hash = source_hash,
            .generation_id = generation_id,
            .indexed_at = std.Io.Timestamp.now(io, .real).nanoseconds,
            .allocator = allocator,
        };
        return gen;
    }

    /// Drop one reference; destroys the generation on the last release.
    pub fn release(self: *GraphGeneration) void {
        if (self.ref_count.release()) self.destroyInternal();
    }

    fn destroyInternal(self: *GraphGeneration) void {
        const allocator = self.allocator;
        self.graph.deinit(self.arena.allocator());
        self.arena.deinit();
        allocator.destroy(self);
    }

    /// RAII guard that releases one reference on deinit.
    pub const Guard = struct {
        gen: *GraphGeneration,

        pub fn deinit(self: Guard) void {
            self.gen.release();
        }
    };

    /// Increment the reference count and return a guard that will release it.
    pub fn acquire(self: *GraphGeneration) Guard {
        self.ref_count.acquire();
        return .{ .gen = self };
    }
};

test "acquire increments refcount" {
    // Arrange
    const gen = try GraphGeneration.create(std.testing.allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();

    // Act
    const guard = gen.acquire();

    // Assert
    try std.testing.expectEqual(@as(u32, 2), gen.ref_count.count());

    // Cleanup
    guard.deinit();
}

test "guard deinit decrements refcount" {
    // Arrange
    const gen = try GraphGeneration.create(std.testing.allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();
    const g1 = gen.acquire();
    const g2 = gen.acquire();

    // Act
    g2.deinit();

    // Assert
    try std.testing.expectEqual(@as(u32, 2), gen.ref_count.count());

    // Cleanup
    g1.deinit();
}

test "multiple acquires and releases" {
    // Arrange
    const gen = try GraphGeneration.create(std.testing.allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.release();

    // Act
    const g1 = gen.acquire();
    const g2 = gen.acquire();
    const g3 = gen.acquire();

    // Assert
    try std.testing.expectEqual(@as(u32, 4), gen.ref_count.count());

    g3.deinit();
    try std.testing.expectEqual(@as(u32, 3), gen.ref_count.count());
    g2.deinit();
    try std.testing.expectEqual(@as(u32, 2), gen.ref_count.count());
    g1.deinit();
    try std.testing.expectEqual(@as(u32, 1), gen.ref_count.count());
}

test "generation_id is set" {
    // Arrange
    const gen = try GraphGeneration.create(std.testing.allocator, std.testing.io, 42, "abcdef1234567890".*);
    defer gen.release();
    const guard = gen.acquire();
    defer guard.deinit();

    // Assert
    try std.testing.expectEqual(@as(u64, 42), gen.generation_id);
}
