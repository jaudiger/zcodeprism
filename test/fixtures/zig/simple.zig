//! Simple fixture for ZCodePrism visitor tests.
//! Contains structs, enums, functions, and edge cases.
const std = @import("std");
const ZigMeta = @import("zig/meta.zig").ZigMeta;
const max_iterations = 1000;

/// A simple point type.
pub const Point = struct {
    x: i32,
    y: i32,

    /// Compute the Manhattan distance from origin.
    pub fn manhattan(self: Point) i32 {
        return abs(self.x) + abs(self.y);
    }

    /// Check whether this point is within a given radius (Manhattan).
    pub fn isWithinRadius(self: Point, radius: i32) bool {
        return self.manhattan() <= radius;
    }
};

/// Direction with three variants.
pub const Direction = enum {
    north,
    south,
    east,
};

/// An error set for parsing.
pub const ParseError = error{
    InvalidToken,
    UnexpectedEof,
    BadEncoding,
};

/// A compile-time buffer size.
pub const buffer_size: usize = 1024;

/// Create a default point at the origin.
pub fn defaultPoint() Point {
    return .{ .x = 0, .y = 0 };
}

fn abs(val: i32) i32 {
    if (val < 0) return -val;
    return val;
}

/// Represents a geometric shape.
pub const Shape = union(enum) {
    circle: f64,
    rect: struct { w: f64, h: f64 },
};

pub const Color = enum {
    red,
    green,
    blue,

    pub fn isWarm(self: Color) bool {
        return self == .red;
    }
};

const RawValue = union {
    int: i64,
    float: f64,
};

test "point manhattan distance" {
    const p = Point{ .x = 3, .y = -4 };
    const dist = p.manhattan();
    _ = dist;
}
