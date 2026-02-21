//! Fixture for testing mutability detection (var vs const).

const std = @import("std");

/// A public immutable constant.
pub const max_size: usize = 1024;

/// A public mutable variable.
pub var counter: usize = 0;

/// A private mutable variable.
var internal_state: bool = false;

/// A private constant (no type annotation).
const default_name = "unnamed";

/// A public mutable variable without type annotation.
pub var verbose = false;

/// A struct with methods.
pub const Config = struct {
    name: []const u8,
    value: u32,

    pub fn init(name: []const u8) Config {
        return .{ .name = name, .value = 0 };
    }
};
