const std = @import("std");
const generation_mod = @import("../core/generation.zig");

const GraphGeneration = generation_mod.GraphGeneration;

/// Thread-safe manager for the current GraphGeneration pointer. Owns one
/// reference on its current generation.
pub const GenerationManager = struct {
    current: *GraphGeneration,
    mutex: std.Io.Mutex,

    /// Take ownership of `initial`'s reference.
    pub fn init(initial: *GraphGeneration) GenerationManager {
        return .{
            .current = initial,
            .mutex = .init,
        };
    }

    /// Release the manager's reference on the current generation. Must be
    /// called after all `acquireCurrent` and `swap` calls have stopped.
    pub fn deinit(self: *GenerationManager) void {
        self.current.release();
    }

    /// Acquire the current generation under the mutex.
    pub fn acquireCurrent(self: *GenerationManager, io: std.Io) GraphGeneration.Guard {
        self.mutex.lockUncancelable(io);
        defer self.mutex.unlock(io);
        return self.current.acquire();
    }

    /// Install `new_gen` as the current generation and release the
    /// manager's reference on the previous one. The caller transfers
    /// ownership of `new_gen`'s reference to the manager.
    pub fn swap(self: *GenerationManager, io: std.Io, new_gen: *GraphGeneration) void {
        self.mutex.lockUncancelable(io);
        const old = self.current;
        self.current = new_gen;
        self.mutex.unlock(io);
        old.release();
    }
};

test "init stores initial generation" {
    // Arrange
    const gen = try GraphGeneration.create(std.testing.allocator, std.testing.io, 1, "abcdef1234567890".*);
    var mgr = GenerationManager.init(gen);
    defer mgr.deinit();

    // Assert
    try std.testing.expectEqual(gen, mgr.current);
}

test "acquireCurrent increments ref count" {
    // Arrange
    const gen = try GraphGeneration.create(std.testing.allocator, std.testing.io, 1, "abcdef1234567890".*);
    var mgr = GenerationManager.init(gen);
    defer mgr.deinit();

    // Act
    const guard = mgr.acquireCurrent(std.testing.io);

    // Assert
    try std.testing.expectEqual(@as(u32, 2), gen.ref_count.count());

    // Cleanup
    guard.deinit();
}

test "swap installs new generation and reclaims old" {
    // Arrange
    const gen1 = try GraphGeneration.create(std.testing.allocator, std.testing.io, 1, "abcdef1234567890".*);
    const gen2 = try GraphGeneration.create(std.testing.allocator, std.testing.io, 2, "654321fedcba7890".*);
    var mgr = GenerationManager.init(gen1);
    defer mgr.deinit();

    // Act
    mgr.swap(std.testing.io, gen2);

    // Assert
    try std.testing.expectEqual(gen2, mgr.current);
}

test "acquireCurrent after swap returns new generation" {
    // Arrange
    const gen1 = try GraphGeneration.create(std.testing.allocator, std.testing.io, 1, "abcdef1234567890".*);
    const gen2 = try GraphGeneration.create(std.testing.allocator, std.testing.io, 2, "654321fedcba7890".*);
    var mgr = GenerationManager.init(gen1);
    defer mgr.deinit();

    // Act
    mgr.swap(std.testing.io, gen2);
    const guard = mgr.acquireCurrent(std.testing.io);

    // Assert
    try std.testing.expectEqual(@as(u64, 2), guard.gen.generation_id);

    // Cleanup
    guard.deinit();
}
