//! LSP module root.
//! Re-exports sub-modules for protocol types, client lifecycle,
//! enrichment orchestration, and the resolution worklist.

pub const protocol = @import("protocol.zig");
pub const client = @import("client.zig");
pub const enricher = @import("enricher.zig");
pub const worklist = @import("worklist.zig");

test {
    _ = protocol;
    _ = client;
    _ = enricher;
    _ = worklist;
}
