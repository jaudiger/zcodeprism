const Widget = @import("provider.zig").Widget;

pub fn useWidget() void {
    var w = Widget.init();
    w.increment();
}
