const std = @import("std");
const generation_mod = @import("../core/generation.zig");

const GraphGeneration = generation_mod.GraphGeneration;

/// Thread-safe manager for the current GraphGeneration pointer.
/// Uses a mutex to prevent the acquire-before-swap race where a reader
/// could load the pointer without also incrementing the ref count.
pub const GenerationManager = struct {
    current: *GraphGeneration,
    mutex: std.Io.Mutex,

    pub fn init(initial: *GraphGeneration) GenerationManager {
        return .{
            .current = initial,
            .mutex = .init,
        };
    }

    /// Acquire the current generation under the mutex. Increments ref count
    /// before releasing the mutex, so the generation cannot be freed between
    /// pointer load and ref count increment.
    pub fn acquireCurrent(self: *GenerationManager, io: std.Io) GraphGeneration.Guard {
        self.mutex.lockUncancelable(io);
        defer self.mutex.unlock(io);
        return self.current.acquire();
    }

    /// Swap in a new generation. Returns the old generation pointer.
    /// The old generation's ref count is not touched here; existing guards
    /// will release it when they deinit.
    pub fn swap(self: *GenerationManager, io: std.Io, new_gen: *GraphGeneration) *GraphGeneration {
        self.mutex.lockUncancelable(io);
        defer self.mutex.unlock(io);
        const old = self.current;
        self.current = new_gen;
        return old;
    }
};

test "init stores initial generation" {
    // Arrange
    const gen = try GraphGeneration.create(std.testing.allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.destroy(std.testing.allocator);

    // Act
    const mgr = GenerationManager.init(gen);

    // Assert
    try std.testing.expectEqual(gen, mgr.current);
}

test "acquireCurrent increments ref count" {
    // Arrange
    const gen = try GraphGeneration.create(std.testing.allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen.destroy(std.testing.allocator);
    var mgr = GenerationManager.init(gen);

    // Act
    const guard = mgr.acquireCurrent(std.testing.io);

    // Assert
    try std.testing.expectEqual(@as(u32, 1), gen.ref_count.load(.monotonic));

    // Cleanup
    guard.deinit();
}

test "swap returns old generation and installs new" {
    // Arrange
    const gen1 = try GraphGeneration.create(std.testing.allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen1.destroy(std.testing.allocator);
    const gen2 = try GraphGeneration.create(std.testing.allocator, std.testing.io, 2, "654321fedcba7890".*);
    defer gen2.destroy(std.testing.allocator);
    var mgr = GenerationManager.init(gen1);

    // Act
    const old = mgr.swap(std.testing.io, gen2);

    // Assert
    try std.testing.expectEqual(gen1, old);
    try std.testing.expectEqual(gen2, mgr.current);
}

test "acquireCurrent after swap returns new generation" {
    // Arrange
    const gen1 = try GraphGeneration.create(std.testing.allocator, std.testing.io, 1, "abcdef1234567890".*);
    defer gen1.destroy(std.testing.allocator);
    const gen2 = try GraphGeneration.create(std.testing.allocator, std.testing.io, 2, "654321fedcba7890".*);
    defer gen2.destroy(std.testing.allocator);
    var mgr = GenerationManager.init(gen1);

    // Act
    _ = mgr.swap(std.testing.io, gen2);
    const guard = mgr.acquireCurrent(std.testing.io);

    // Assert
    try std.testing.expectEqual(@as(u64, 2), guard.gen.generation_id);

    // Cleanup
    guard.deinit();
}
