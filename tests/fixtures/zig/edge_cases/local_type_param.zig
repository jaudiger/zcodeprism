/// Fixture for testing local-type parameter method calls.
/// When a function takes a parameter of a locally-defined type (e.g., fn process(p: Point)),
/// calls via that parameter (p.method()) should create calls edges.
const std = @import("std");

pub const Point = struct {
    x: i32,
    y: i32,

    pub fn manhattan(self: Point) i32 {
        return if (self.x < 0) -self.x else self.x +
            if (self.y < 0) -self.y else self.y;
    }

    pub fn scale(self: Point, factor: i32) Point {
        _ = factor;
        return self;
    }
};

/// Takes a Point parameter by value and calls its method.
/// p.manhattan() should create a calls edge to Point.manhattan.
pub fn processPoint(p: Point) i32 {
    return p.manhattan();
}

/// Takes an external type parameter — should NOT create a local calls edge.
pub fn externalParam(allocator: std.mem.Allocator) void {
    _ = allocator;
}

/// Takes a pointer-to-Point parameter — pointer types are not handled
/// by the simple bare-identifier check, so no local calls edge expected.
pub fn pointerParam(p: *Point) void {
    p.scale(2);
}

/// Multiple parameters: one local type, one primitive.
/// Only the local-type parameter method call should create a calls edge.
pub fn multiParam(p: Point, n: i32) i32 {
    _ = n;
    return p.manhattan();
}
