const a = @import("a.zig");

pub fn fromC() u32 {
    return 3;
}

pub fn callA() u32 {
    return a.fromA();
}
