//! High-level CLI command workflows. Each module exposes a `run`
//! function returning an error union.

pub const init = @import("init.zig");
pub const index = @import("index.zig");
pub const @"export" = @import("export.zig");
pub const snapshot = @import("snapshot.zig");
pub const diff = @import("diff.zig");
pub const serve = @import("serve.zig");
pub const status = @import("status.zig");
pub const source_hash = @import("source_hash.zig");

test {
    _ = init;
    _ = index;
    _ = @"export";
    _ = snapshot;
    _ = diff;
    _ = serve;
    _ = status;
    _ = source_hash;
}
