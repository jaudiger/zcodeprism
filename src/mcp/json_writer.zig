const std = @import("std");
const types = @import("../core/types.zig");

const NodeId = types.NodeId;

/// Thin wrapper around std.json.Stringify that narrows all errors to
/// error{OutOfMemory}. Every method maps directly to one or two Stringify
/// calls with the catch-return boilerplate done once here.
pub const JsonWriter = struct {
    s: *std.json.Stringify,

    const OomError = error{OutOfMemory};

    pub fn beginObject(self: JsonWriter) OomError!void {
        self.s.beginObject() catch return error.OutOfMemory;
    }

    pub fn endObject(self: JsonWriter) OomError!void {
        self.s.endObject() catch return error.OutOfMemory;
    }

    pub fn beginArray(self: JsonWriter) OomError!void {
        self.s.beginArray() catch return error.OutOfMemory;
    }

    pub fn endArray(self: JsonWriter) OomError!void {
        self.s.endArray() catch return error.OutOfMemory;
    }

    pub fn write(self: JsonWriter, value: anytype) OomError!void {
        self.s.write(value) catch return error.OutOfMemory;
    }

    pub fn field(self: JsonWriter, name: []const u8) OomError!void {
        self.s.objectField(name) catch return error.OutOfMemory;
    }

    /// objectField + write combined.
    pub fn fieldValue(self: JsonWriter, name: []const u8, value: anytype) OomError!void {
        self.s.objectField(name) catch return error.OutOfMemory;
        self.s.write(value) catch return error.OutOfMemory;
    }

    /// Write @tagName(value) as a string.
    pub fn tagValue(self: JsonWriter, value: anytype) OomError!void {
        self.s.write(@tagName(value)) catch return error.OutOfMemory;
    }

    /// objectField + @tagName(enum_value).
    pub fn tagFieldValue(self: JsonWriter, name: []const u8, value: anytype) OomError!void {
        self.s.objectField(name) catch return error.OutOfMemory;
        try self.tagValue(value);
    }

    /// Write a NodeId as a hex string value.
    pub fn nodeIdHex(self: JsonWriter, id: NodeId) OomError!void {
        var buf: [20]u8 = undefined;
        const hex = std.fmt.bufPrint(&buf, "{x}", .{@intFromEnum(id)}) catch unreachable;
        self.s.write(hex) catch return error.OutOfMemory;
    }

    /// objectField + NodeId as hex string.
    pub fn fieldNodeIdHex(self: JsonWriter, name: []const u8, id: NodeId) OomError!void {
        self.s.objectField(name) catch return error.OutOfMemory;
        try self.nodeIdHex(id);
    }

    /// objectField + u64 as a zero-padded 16-char hex string.
    pub fn fieldHashHex(self: JsonWriter, name: []const u8, value: u64) OomError!void {
        self.s.objectField(name) catch return error.OutOfMemory;
        var buf: [16]u8 = undefined;
        const hex = std.fmt.bufPrint(&buf, "{x:0>16}", .{value}) catch unreachable;
        self.s.write(hex) catch return error.OutOfMemory;
    }

    /// Write a ContentHash as a hex string value.
    pub fn hashHex(self: JsonWriter, hash: types.ContentHash) OomError!void {
        const hex_buf = types.formatHash(hash);
        self.s.write(@as([]const u8, &hex_buf)) catch return error.OutOfMemory;
    }

    /// objectField + writeInner(self, v) for the unwrapped value, or null.
    pub fn optionalField(
        self: JsonWriter,
        name: []const u8,
        value: anytype,
        comptime writeInner: anytype,
    ) OomError!void {
        self.s.objectField(name) catch return error.OutOfMemory;
        if (value) |v| {
            try writeInner(self, v);
        } else {
            self.s.write(null) catch return error.OutOfMemory;
        }
    }
};
