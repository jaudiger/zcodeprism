const b = @import("b.zig");

pub fn fromA() u32 {
    return 1;
}

pub fn callB() u32 {
    return b.fromB();
}
