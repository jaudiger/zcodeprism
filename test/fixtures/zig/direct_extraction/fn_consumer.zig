const standalone = @import("provider.zig").standalone;

pub fn callStandalone() void {
    standalone();
}
