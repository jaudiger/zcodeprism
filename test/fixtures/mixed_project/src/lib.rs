//! Mixed-language FFI fixture: Rust side.
//! Exports C-ABI functions callable from Zig.

/// Addition over C ABI, callable from Zig.
#[no_mangle]
pub extern "C" fn rust_add(a: i32, b: i32) -> i32 {
    a + b
}

/// Multiplication over C ABI, callable from Zig.
#[no_mangle]
pub extern "C" fn rust_multiply(a: i32, b: i32) -> i32 {
    a * b
}

/// A simple addition helper (structurally similar to Zig add_values).
pub fn add_numbers(a: i32, b: i32) -> i32 {
    a + b
}

/// A standalone helper that does not use FFI.
pub fn triple(n: i32) -> i32 {
    n * 3
}
