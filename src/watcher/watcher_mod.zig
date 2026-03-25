//! Watcher module root.
//! Re-exports sub-modules for file watching, debouncing, and
//! generation management.

pub const debouncer = @import("debouncer.zig");
pub const generation_manager = @import("generation_manager.zig");
pub const watcher = @import("watcher.zig");

test {
    _ = debouncer;
    _ = generation_manager;
    _ = watcher;
}
