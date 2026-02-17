//! Compile-time access to test fixture files.
//! Organized by language. Each language gets its own namespace struct.
//! This module is rooted in `tests/`, so @embedFile can reach `tests/fixtures/`.

pub const zig = struct {
    pub const deeply_nested = @embedFile("fixtures/zig/edge_cases/deeply_nested.zig");
    pub const empty = @embedFile("fixtures/zig/edge_cases/empty.zig");
    pub const file_struct = @embedFile("fixtures/zig/file_struct.zig");
    pub const generic_type = @embedFile("fixtures/zig/generic_type.zig");
    pub const many_params = @embedFile("fixtures/zig/edge_cases/many_params.zig");
    pub const no_pub = @embedFile("fixtures/zig/edge_cases/no_pub.zig");
    pub const only_comments = @embedFile("fixtures/zig/edge_cases/only_comments.zig");
    pub const simple = @embedFile("fixtures/zig/simple.zig");
    pub const unicode_names = @embedFile("fixtures/zig/edge_cases/unicode_names.zig");
};
