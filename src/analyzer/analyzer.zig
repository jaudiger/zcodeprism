//! Analysis module root.
//! Re-exports sub-modules for code analysis: complexity, dead code,
//! duplicates, impact, coupling, and dependency cycles.

pub const complexity = @import("complexity.zig");
pub const dead_code = @import("dead_code.zig");
pub const duplicates = @import("duplicates.zig");
pub const filter = @import("filter.zig");
pub const impact = @import("impact.zig");
pub const coupling = @import("coupling.zig");
pub const cycles = @import("cycles.zig");
pub const pagination = @import("pagination.zig");

test {
    _ = complexity;
    _ = dead_code;
    _ = duplicates;
    _ = filter;
    _ = impact;
    _ = coupling;
    _ = cycles;
    _ = pagination;
}
