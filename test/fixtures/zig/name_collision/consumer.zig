const alpha = @import("alpha.zig");
const beta = @import("beta.zig");

pub fn useAlpha() void {
    var a = alpha.Self.init(42);
    a.deinit();
}

pub fn useBeta() void {
    var b = beta.Self.init("hello");
    b.deinit();
}
