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
    structural_hash: u64 = 0,

    /// Binary record size: matches METRICS_RECORD_SIZE in binary.zig.
    pub const BINARY_SIZE: usize = 28;

    /// Write this metrics value as a JSON object to `stream`.
    pub fn writeJson(self: Metrics, stream: *std.json.Stringify) !void {
        try stream.beginObject();
        try stream.objectField("complexity");
        try stream.write(self.complexity);
        try stream.objectField("lines");
        try stream.write(self.lines);
        try stream.objectField("fan_in");
        try stream.write(self.fan_in);
        try stream.objectField("fan_out");
        try stream.write(self.fan_out);
        try stream.objectField("branches");
        try stream.write(self.branches);
        try stream.objectField("loops");
        try stream.write(self.loops);
        try stream.objectField("error_paths");
        try stream.write(self.error_paths);
        try stream.objectField("nesting_depth_max");
        try stream.write(self.nesting_depth_max);
        try stream.objectField("structural_hash");
        var hex_buf: [16]u8 = undefined;
        const hex = std.fmt.bufPrint(&hex_buf, "{x:0>16}", .{self.structural_hash}) catch unreachable;
        try stream.write(hex);
        try stream.endObject();
    }

    /// Parse a Metrics from a JSON Value. Returns null for non-object or null values.
    pub fn parseJson(val: std.json.Value) ?Metrics {
        switch (val) {
            .null => return null,
            .object => |obj| {
                return .{
                    .complexity = jsonField(u16, obj, "complexity"),
                    .lines = jsonField(u32, obj, "lines"),
                    .fan_in = jsonField(u16, obj, "fan_in"),
                    .fan_out = jsonField(u16, obj, "fan_out"),
                    .branches = jsonField(u16, obj, "branches"),
                    .loops = jsonField(u16, obj, "loops"),
                    .error_paths = jsonField(u16, obj, "error_paths"),
                    .nesting_depth_max = jsonField(u8, obj, "nesting_depth_max"),
                    .structural_hash = jsonHashField(obj, "structural_hash"),
                };
            },
            else => return null,
        }
    }

    /// Encode into a fixed-size 28-byte binary record (little-endian).
    pub fn encodeBinary(self: Metrics, buf: *[BINARY_SIZE]u8) void {
        std.mem.writeInt(u16, buf[0..2], self.complexity, .little);
        std.mem.writeInt(u32, buf[2..6], self.lines, .little);
        std.mem.writeInt(u16, buf[6..8], self.fan_in, .little);
        std.mem.writeInt(u16, buf[8..10], self.fan_out, .little);
        std.mem.writeInt(u16, buf[10..12], self.branches, .little);
        std.mem.writeInt(u16, buf[12..14], self.loops, .little);
        std.mem.writeInt(u16, buf[14..16], self.error_paths, .little);
        buf[16] = self.nesting_depth_max;
        buf[17] = 0; // padding
        std.mem.writeInt(u64, buf[18..26], self.structural_hash, .little);
        buf[26] = 0; // padding
        buf[27] = 0;
    }

    /// Decode from a fixed-size 28-byte binary record (little-endian).
    pub fn decodeBinary(buf: *const [BINARY_SIZE]u8) Metrics {
        return .{
            .complexity = std.mem.readInt(u16, buf[0..2], .little),
            .lines = std.mem.readInt(u32, buf[2..6], .little),
            .fan_in = std.mem.readInt(u16, buf[6..8], .little),
            .fan_out = std.mem.readInt(u16, buf[8..10], .little),
            .branches = std.mem.readInt(u16, buf[10..12], .little),
            .loops = std.mem.readInt(u16, buf[12..14], .little),
            .error_paths = std.mem.readInt(u16, buf[14..16], .little),
            .nesting_depth_max = buf[16],
            .structural_hash = std.mem.readInt(u64, buf[18..26], .little),
        };
    }
};

fn jsonField(comptime T: type, obj: std.json.ObjectMap, key: []const u8) T {
    const val = obj.get(key) orelse return 0;
    return switch (val) {
        .integer => |i| @intCast(i),
        else => 0,
    };
}

/// Parse structural_hash from either a hex string or an integer.
fn jsonHashField(obj: std.json.ObjectMap, key: []const u8) u64 {
    const val = obj.get(key) orelse return 0;
    return switch (val) {
        .string => |s| std.fmt.parseInt(u64, s, 16) catch 0,
        .integer => |i| @intCast(i),
        else => 0,
    };
}

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
    try std.testing.expectEqual(@as(u64, 0), m.structural_hash);
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
        .structural_hash = 0xDEADBEEFCAFEBABE,
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
    try std.testing.expectEqual(@as(u64, 0xDEADBEEFCAFEBABE), m.structural_hash);
}
