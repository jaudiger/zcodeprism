const std = @import("std");

/// Binary storage backend (compact, not human-readable).
pub const binary = @import("binary.zig");

/// JSONL storage backend (one JSON object per line, human-readable).
pub const jsonl = @import("jsonl.zig");

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
