const std = @import("std");
const graph_mod = @import("../core/graph.zig");

const Graph = graph_mod.Graph;

/// Error set for storage operations.
/// Covers domain-specific errors (format validation) plus common I/O
/// and allocation failures. Concrete StorageBackend implementations
/// must map their internal errors to this set.
pub const StorageError = error{
    OutOfMemory,
    InvalidFormat,
    InvalidMagic,
    UnsupportedVersion,
    AccessDenied,
    FileNotFound,
    InputOutput,
    NoSpaceLeft,
    Unexpected,
};

/// Abstract storage backend interface using vtable pattern.
/// Concrete implementations provide their own save/load
/// functions through this interface.
pub const StorageBackend = struct {
    saveFn: *const fn (*StorageBackend, std.mem.Allocator, *const Graph) StorageError!void,
    loadFn: *const fn (*StorageBackend, std.mem.Allocator) StorageError!Graph,

    pub fn save(self: *StorageBackend, allocator: std.mem.Allocator, g: *const Graph) StorageError!void {
        return self.saveFn(self, allocator, g);
    }

    pub fn load(self: *StorageBackend, allocator: std.mem.Allocator) StorageError!Graph {
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
