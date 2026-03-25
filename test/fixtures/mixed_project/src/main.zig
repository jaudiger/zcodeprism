//! Mixed-language FFI fixture: Zig side.
//! Declares C-ABI prototypes implemented in Rust and calls them.

// C-ABI function prototypes implemented in Rust (no body = declaration only).
extern "c" fn rust_add(a: i32, b: i32) i32;
extern "c" fn rust_multiply(a: i32, b: i32) i32;

/// Calls both FFI functions and combines results.
pub fn compute(x: i32, y: i32) i32 {
    return rust_add(x, y) + rust_multiply(x, y);
}

/// A simple addition helper (structurally similar to Rust add_numbers).
pub fn add_values(a: i32, b: i32) i32 {
    return a + b;
}

/// A standalone helper that does not use FFI.
pub fn double(n: i32) i32 {
    return n * 2;
}
