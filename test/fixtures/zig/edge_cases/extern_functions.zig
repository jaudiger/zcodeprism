//! Fixture for extern and inline function qualifier detection.

/// A C-calling-convention extern function.
pub extern "c" fn c_write(fd: c_int, buf: [*]const u8, count: usize) isize;

/// An extern function without calling convention.
pub extern fn bare_extern() void;

/// A private extern function.
extern fn private_extern(x: i32) i32;

/// A public inline function.
pub inline fn fast_add(a: i32, b: i32) i32 {
    return a + b;
}

/// A private inline function.
inline fn internal_helper() void {}

/// A regular function (neither extern nor inline).
pub fn normal_function() void {}

/// A private regular function.
fn also_normal() void {}
