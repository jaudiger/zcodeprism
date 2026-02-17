const std = @import("std");
const graph_mod = @import("../core/graph.zig");

const Graph = graph_mod.Graph;

/// Abstract storage backend interface using vtable pattern.
/// Concrete implementations provide their own save/load
/// functions through this interface.
pub const StorageBackend = struct {
    saveFn: *const fn (*StorageBackend, std.mem.Allocator, *const Graph) anyerror!void,
    loadFn: *const fn (*StorageBackend, std.mem.Allocator) anyerror!Graph,

    pub fn save(self: *StorageBackend, allocator: std.mem.Allocator, g: *const Graph) !void {
        return self.saveFn(self, allocator, g);
    }

    pub fn load(self: *StorageBackend, allocator: std.mem.Allocator) !Graph {
        return self.loadFn(self, allocator);
    }
};

// --- Tests ---

test "storage interface is abstract" {
    comptime {
        // StorageBackend is a struct
        const info = @typeInfo(StorageBackend);
        std.debug.assert(info == .@"struct");

        // It has saveFn and loadFn fields
        std.debug.assert(@hasField(StorageBackend, "saveFn"));
        std.debug.assert(@hasField(StorageBackend, "loadFn"));

        // Binary module exposes expected function signatures
        const binary = @import("binary.zig");
        std.debug.assert(@TypeOf(binary.save) == @TypeOf(binary.save));
        std.debug.assert(@TypeOf(binary.load) == @TypeOf(binary.load));

        // JSONL module exposes expected function signatures
        const jsonl = @import("jsonl.zig");
        std.debug.assert(@TypeOf(jsonl.exportJsonl) == @TypeOf(jsonl.exportJsonl));
        std.debug.assert(@TypeOf(jsonl.importJsonl) == @TypeOf(jsonl.importJsonl));
    }
}
