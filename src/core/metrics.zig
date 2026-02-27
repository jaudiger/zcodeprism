const std = @import("std");

/// Code metrics for a node (function, type, etc.).
/// All fields default to 0 for newly created nodes.
pub const Metrics = struct {
    complexity: u16 = 0,
    lines: u32 = 0,
    fan_in: u16 = 0,
    fan_out: u16 = 0,
    branches: u16 = 0,
    loops: u16 = 0,
    error_paths: u16 = 0,
    nesting_depth_max: u8 = 0,
    structural_hash: u32 = 0,
};

test "metrics default values are all zero" {
    // Arrange
    const m = Metrics{};

    // Assert
    try std.testing.expectEqual(@as(u16, 0), m.complexity);
    try std.testing.expectEqual(@as(u32, 0), m.lines);
    try std.testing.expectEqual(@as(u16, 0), m.fan_in);
    try std.testing.expectEqual(@as(u16, 0), m.fan_out);
    try std.testing.expectEqual(@as(u16, 0), m.branches);
    try std.testing.expectEqual(@as(u16, 0), m.loops);
    try std.testing.expectEqual(@as(u16, 0), m.error_paths);
    try std.testing.expectEqual(@as(u8, 0), m.nesting_depth_max);
    try std.testing.expectEqual(@as(u32, 0), m.structural_hash);
}

test "metrics stores values" {
    // Arrange
    const m = Metrics{
        .complexity = 5,
        .lines = 100,
        .fan_in = 3,
        .fan_out = 7,
        .branches = 4,
        .loops = 2,
        .error_paths = 1,
        .nesting_depth_max = 6,
        .structural_hash = 0xDEADBEEF,
    };

    // Assert
    try std.testing.expectEqual(@as(u16, 5), m.complexity);
    try std.testing.expectEqual(@as(u32, 100), m.lines);
    try std.testing.expectEqual(@as(u16, 3), m.fan_in);
    try std.testing.expectEqual(@as(u16, 7), m.fan_out);
    try std.testing.expectEqual(@as(u16, 4), m.branches);
    try std.testing.expectEqual(@as(u16, 2), m.loops);
    try std.testing.expectEqual(@as(u16, 1), m.error_paths);
    try std.testing.expectEqual(@as(u8, 6), m.nesting_depth_max);
    try std.testing.expectEqual(@as(u32, 0xDEADBEEF), m.structural_hash);
}
