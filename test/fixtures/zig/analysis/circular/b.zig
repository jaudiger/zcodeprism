const c = @import("c.zig");

pub fn fromB() u32 {
    return 2;
}

pub fn callC() u32 {
    return c.fromC();
}
