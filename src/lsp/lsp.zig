//! LSP module root.
//! Re-exports sub-modules for protocol types, client lifecycle,
//! enrichment orchestration, the resolution worklist, and the
//! connection pool.

pub const protocol = @import("protocol.zig");
pub const client = @import("client.zig");
pub const enricher = @import("enricher.zig");
pub const worklist = @import("worklist.zig");
pub const pool = @import("pool.zig");

test {
    _ = protocol;
    _ = client;
    _ = enricher;
    _ = worklist;
    _ = pool;
}
