const provider = @import("provider.zig");

test "import var method call" {
    var w = provider.Widget.init();
    w.increment();
}
