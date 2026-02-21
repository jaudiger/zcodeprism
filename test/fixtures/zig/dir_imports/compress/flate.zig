const inner = @import("flate/inner.zig");

pub fn compress() void {
    inner.process();
}
