const std = @import("std");
const graph_mod = @import("../core/graph.zig");

/// Crash-safe atomic file write with parent-directory fsync.
pub const atomic_file = @import("atomic_file.zig");

/// Snapshot save/load and source-hash computation.
pub const snapshot = @import("snapshot.zig");

/// Binary storage backend (compact, not human-readable).
pub const binary = @import("binary.zig");

/// JSONL storage backend (one JSON object per line, human-readable).
pub const jsonl = @import("jsonl.zig");

/// Workspace assembly loader.
pub const workspace_loader = @import("workspace_loader.zig");

/// Default project data directory.
pub const data_dir = ".zcodeprism";

/// Default path for the indexed graph in binary format.
pub const graph_binary_path = ".zcodeprism/graph.bin";

/// Default path for the indexed graph in JSONL format.
pub const graph_jsonl_path = ".zcodeprism/graph.jsonl";

/// Filenames inside a project data directory.
const graph_binary_name = "graph.bin";
const graph_jsonl_name = "graph.jsonl";

/// Resolved filesystem paths derived from a configured data directory.
/// The caller owns the backing buffers (allocated by `Layout.init`) and
/// must call `Layout.deinit` to release them.
pub const Layout = struct {
    allocator: std.mem.Allocator,
    data_dir: []const u8,
    graph_binary: []const u8,
    graph_jsonl: []const u8,

    /// Allocate path strings for a data directory. Trailing slashes on
    /// the base are stripped before joining.
    pub fn init(allocator: std.mem.Allocator, base: []const u8) !Layout {
        const trimmed = std.mem.trimEnd(u8, base, "/");
        const root = if (trimmed.len == 0) "." else trimmed;
        const bin = try std.fmt.allocPrint(allocator, "{s}/" ++ graph_binary_name, .{root});
        errdefer allocator.free(bin);
        const jsonl_path = try std.fmt.allocPrint(allocator, "{s}/" ++ graph_jsonl_name, .{root});
        return .{
            .allocator = allocator,
            .data_dir = base,
            .graph_binary = bin,
            .graph_jsonl = jsonl_path,
        };
    }

    pub fn deinit(self: *Layout) void {
        self.allocator.free(self.graph_binary);
        self.allocator.free(self.graph_jsonl);
        self.* = undefined;
    }
};

/// Load the saved graph from `layout`, picking binary or JSONL based on
/// whichever file exists. Binary is preferred when both are present.
pub fn loadGraph(allocator: std.mem.Allocator, io: std.Io, layout: Layout) !graph_mod.Graph {
    if (fileExists(io, layout.graph_binary)) {
        return binary.load(allocator, io, layout.graph_binary);
    }
    if (fileExists(io, layout.graph_jsonl)) {
        return loadJsonl(allocator, io, layout.graph_jsonl);
    }
    return error.FileNotFound;
}

fn loadJsonl(allocator: std.mem.Allocator, io: std.Io, path: []const u8) !graph_mod.Graph {
    const file = try std.Io.Dir.cwd().openFile(io, path, .{});
    defer file.close(io);
    var rbuf: [4096]u8 = undefined;
    var fr = file.reader(io, &rbuf);
    const data = try fr.interface.allocRemaining(allocator, .limited(1024 * 1024 * 1024));
    defer allocator.free(data);
    return jsonl.importJsonl(allocator, data);
}

fn fileExists(io: std.Io, path: []const u8) bool {
    std.Io.Dir.cwd().access(io, path, .{}) catch return false;
    return true;
}

/// Supported persistent storage formats for graph serialization.
///
/// Callers select a format and call the corresponding module's functions
/// directly. There is no vtable dispatch -- the caller is the adapter.
pub const Format = enum {
    /// Compact binary format with a string table and 8-byte aligned tables.
    binary_v1,
    /// One JSON object per line (nodes first, then edges).
    jsonl,
};

/// Domain-specific errors that storage backends may return.
///
/// These cover format validation failures distinct from generic I/O and
/// allocation errors. Concrete backends return them directly in their
/// inferred error sets; this definition lets callers catch storage-specific
/// failures without matching on `anyerror`.
pub const StorageError = error{
    /// Data does not conform to the expected layout.
    InvalidFormat,
    /// File does not start with the expected magic bytes.
    InvalidMagic,
    /// File version is newer than what this build supports.
    UnsupportedVersion,
};

test "Format enum has exactly two variants" {
    comptime {
        const fields = @typeInfo(Format).@"enum".fields;
        std.debug.assert(fields.len == 2);
    }
}

test "StorageError contains domain-specific errors" {
    comptime {
        // Verify each domain error is in the set by attempting a catch.
        // If any were missing, this would be a compile error.
        const errors = [_]StorageError{
            error.InvalidFormat,
            error.InvalidMagic,
            error.UnsupportedVersion,
        };
        std.debug.assert(errors.len == 3);
    }
}

test "binary module exposes save and load" {
    comptime {
        std.debug.assert(@hasDecl(binary, "save"));
        std.debug.assert(@hasDecl(binary, "load"));
        std.debug.assert(@hasDecl(binary, "append"));
    }
}

test "jsonl module exposes exportJsonl and importJsonl" {
    comptime {
        std.debug.assert(@hasDecl(jsonl, "exportJsonl"));
        std.debug.assert(@hasDecl(jsonl, "importJsonl"));
    }
}
