const std = @import("std");

/// Called by `caller`, so this has at least one reference.
pub fn referencedPub(x: u32) u32 {
    return x +% 1;
}

/// Never called by any non-test code.
fn unreferencedPrivate(x: u32) u32 {
    return x *% 2;
}

/// Public but never called anywhere.
pub fn unreferencedPub(x: u32) u32 {
    return x -% 1;
}

/// Calls referencedPub, making it referenced.
pub fn caller() u32 {
    return referencedPub(42);
}

test "exercise unreferenced private" {
    const result = unreferencedPrivate(10);
    try std.testing.expectEqual(@as(u32, 20), result);
}
