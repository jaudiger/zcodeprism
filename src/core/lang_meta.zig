const std = @import("std");
const RustMeta = @import("../languages/rust/meta.zig").RustMeta;
const RustSubKind = @import("../languages/rust/meta.zig").RustSubKind;
const ZigMeta = @import("../languages/zig/meta.zig").ZigMeta;

/// Language-specific metadata union carried by each graph node.
/// `.rust` holds Rust-specific flags and sub-kind.
/// `.zig` holds Zig-specific flags and calling convention.
/// `.none` is used for nodes that need no language-specific information.
pub const LangMeta = union(enum) {
    /// Rust-specific metadata (unsafe, async, const, sub-kind, etc.).
    rust: RustMeta,
    /// Zig-specific metadata (comptime, inline, extern, packed, etc.).
    zig: ZigMeta,
    /// No language-specific metadata for this node.
    none: void,

    /// FFI convention string for extern functions, or null if not FFI.
    pub fn ffiConvention(self: LangMeta) ?[]const u8 {
        return switch (self) {
            .zig => |zm| if (zm.is_extern) zm.calling_convention else null,
            .rust => |rm| if (rm.is_extern) rm.abi else null,
            .none => null,
        };
    }

    // Binary serialization

    /// Fixed header: tag + flags + sub_kind + abi_len + vs_len + derives_len + attrs_len + inner_attrs_len.
    const rust_header_size = 8;
    /// Fixed header size for Zig binary encoding: tag + flags.
    const zig_header_size = 2;

    /// Return an upper-bound byte count for binary encoding of this value.
    /// `.none` returns 0.
    pub fn binarySize(self: LangMeta) usize {
        return switch (self) {
            .rust => |rm| rust_header_size + (if (rm.abi) |a| a.len else 0) + (if (rm.visibility_scope) |vs| vs.len else 0) + (if (rm.derives) |d| d.len else 0) + (if (rm.attributes) |at| at.len else 0) + (if (rm.inner_attributes) |ia| ia.len else 0),
            .zig => |zm| zig_header_size + (if (zm.calling_convention) |cc| cc.len else 0),
            .none => 0,
        };
    }

    /// Encode this value into a caller-provided byte buffer.
    /// Returns the number of bytes written.
    /// `.none` writes nothing. `.zig` writes `[tag=1][flags_byte][calling_convention?]`.
    /// `buf` must be at least `binarySize()` bytes long.
    pub fn encodeBinary(self: LangMeta, buf: []u8) usize {
        switch (self) {
            .none => return 0,
            .rust => |rm| {
                buf[0] = 2; // tag = rust
                var flags: u8 = 0;
                if (rm.is_unsafe) flags |= 0x01;
                if (rm.is_async) flags |= 0x02;
                if (rm.is_const) flags |= 0x04;
                if (rm.is_extern) flags |= 0x08;
                if (rm.is_default) flags |= 0x10;
                buf[1] = flags;
                buf[2] = @intFromEnum(rm.sub_kind);
                const abi_len: u8 = if (rm.abi) |a| @intCast(a.len) else 0;
                buf[3] = abi_len;
                const vs_len: u8 = if (rm.visibility_scope) |vs| @intCast(vs.len) else 0;
                buf[4] = vs_len;
                const derives_len: u8 = if (rm.derives) |d| @intCast(d.len) else 0;
                buf[5] = derives_len;
                const attrs_len: u8 = if (rm.attributes) |at| @intCast(at.len) else 0;
                buf[6] = attrs_len;
                const inner_attrs_len: u8 = if (rm.inner_attributes) |ia| @intCast(ia.len) else 0;
                buf[7] = inner_attrs_len;
                var pos: usize = rust_header_size;
                if (rm.abi) |a| {
                    @memcpy(buf[pos..][0..a.len], a);
                    pos += a.len;
                }
                if (rm.visibility_scope) |vs| {
                    @memcpy(buf[pos..][0..vs.len], vs);
                    pos += vs.len;
                }
                if (rm.derives) |d| {
                    @memcpy(buf[pos..][0..d.len], d);
                    pos += d.len;
                }
                if (rm.attributes) |at| {
                    @memcpy(buf[pos..][0..at.len], at);
                    pos += at.len;
                }
                if (rm.inner_attributes) |ia| {
                    @memcpy(buf[pos..][0..ia.len], ia);
                    pos += ia.len;
                }
                return pos;
            },
            .zig => |zm| {
                buf[0] = 1; // tag = zig
                var flags: u8 = 0;
                if (zm.is_comptime) flags |= 0x01;
                if (zm.is_inline) flags |= 0x02;
                if (zm.is_extern) flags |= 0x04;
                if (zm.comptime_conditional) flags |= 0x08;
                if (zm.is_mutable) flags |= 0x10;
                if (zm.is_packed) flags |= 0x20;
                buf[1] = flags;
                var len: usize = zig_header_size;
                if (zm.calling_convention) |cc| {
                    if (cc.len <= buf.len - zig_header_size) {
                        @memcpy(buf[zig_header_size..][0..cc.len], cc);
                        len += cc.len;
                    }
                }
                return len;
            },
        }
    }

    /// Decode a LangMeta from a binary slice produced by `encodeBinary`.
    /// Empty slice returns `.none`. Unknown tag returns `.none` (forward compatibility).
    pub fn decodeBinary(data: []const u8) LangMeta {
        if (data.len == 0) return .{ .none = {} };
        if (data[0] == 2 and data.len >= rust_header_size) {
            const flags = data[1];
            const abi_len: usize = data[3];
            const vs_len: usize = data[4];
            const derives_len: usize = data[5];
            const attrs_len: usize = data[6];
            const inner_attrs_len: usize = data[7];
            const abi_end = rust_header_size + abi_len;
            const vs_end = abi_end + vs_len;
            const derives_end = vs_end + derives_len;
            const attrs_end = derives_end + attrs_len;
            return .{ .rust = .{
                .is_unsafe = flags & 0x01 != 0,
                .is_async = flags & 0x02 != 0,
                .is_const = flags & 0x04 != 0,
                .is_extern = flags & 0x08 != 0,
                .is_default = flags & 0x10 != 0,
                .sub_kind = @enumFromInt(data[2]),
                .abi = if (abi_len > 0) data[rust_header_size..abi_end] else null,
                .visibility_scope = if (vs_len > 0) data[abi_end..vs_end] else null,
                .derives = if (derives_len > 0) data[vs_end..derives_end] else null,
                .attributes = if (attrs_len > 0) data[derives_end..attrs_end] else null,
                .inner_attributes = if (inner_attrs_len > 0) data[attrs_end..][0..inner_attrs_len] else null,
            } };
        }
        if (data[0] == 1 and data.len >= zig_header_size) {
            const flags = data[1];
            return .{ .zig = .{
                .is_comptime = flags & 0x01 != 0,
                .is_inline = flags & 0x02 != 0,
                .is_extern = flags & 0x04 != 0,
                .comptime_conditional = flags & 0x08 != 0,
                .is_mutable = flags & 0x10 != 0,
                .is_packed = flags & 0x20 != 0,
                .calling_convention = if (data.len > zig_header_size) data[zig_header_size..] else null,
            } };
        }
        return .{ .none = {} };
    }

    // JSON serialization

    /// Write a JSON representation of this value to `stream`.
    /// `.none` writes the literal `null`. `.zig` writes `{"type":"zig",...}`.
    pub fn writeJson(self: LangMeta, stream: *std.json.Stringify) !void {
        switch (self) {
            .none => try stream.write(null),
            .rust => |rm| {
                try stream.beginObject();
                try stream.objectField("type");
                try stream.write("rust");
                try stream.objectField("is_unsafe");
                try stream.write(rm.is_unsafe);
                try stream.objectField("is_async");
                try stream.write(rm.is_async);
                try stream.objectField("is_const");
                try stream.write(rm.is_const);
                try stream.objectField("is_extern");
                try stream.write(rm.is_extern);
                try stream.objectField("is_default");
                try stream.write(rm.is_default);
                try stream.objectField("sub_kind");
                try stream.write(@tagName(rm.sub_kind));
                try stream.objectField("abi");
                try stream.write(rm.abi);
                try stream.objectField("derives");
                try stream.write(rm.derives);
                try stream.objectField("attributes");
                try stream.write(rm.attributes);
                try stream.objectField("inner_attributes");
                try stream.write(rm.inner_attributes);
                try stream.objectField("visibility_scope");
                try stream.write(rm.visibility_scope);
                try stream.endObject();
            },
            .zig => |zm| {
                try stream.beginObject();
                try stream.objectField("type");
                try stream.write("zig");
                try stream.objectField("is_comptime");
                try stream.write(zm.is_comptime);
                try stream.objectField("is_mutable");
                try stream.write(zm.is_mutable);
                try stream.objectField("is_inline");
                try stream.write(zm.is_inline);
                try stream.objectField("is_extern");
                try stream.write(zm.is_extern);
                try stream.objectField("is_packed");
                try stream.write(zm.is_packed);
                try stream.objectField("comptime_conditional");
                try stream.write(zm.comptime_conditional);
                try stream.objectField("calling_convention");
                try stream.write(zm.calling_convention);
                try stream.endObject();
            },
        }
    }

    /// Parse a LangMeta from a `std.json.Value`, duplicating string data
    /// so the result is independent of the JSON parser's lifetime.
    /// `null` or unknown type returns `.none`.
    /// Caller owns the duplicated `calling_convention` slice (if any) and
    /// must free it via the same allocator.
    pub fn parseJson(allocator: std.mem.Allocator, val: std.json.Value) !LangMeta {
        switch (val) {
            .null => return .{ .none = {} },
            .object => |obj| {
                const type_val = obj.get("type") orelse return .{ .none = {} };
                if (type_val != .string) return .{ .none = {} };
                if (std.mem.eql(u8, type_val.string, "rust")) {
                    const sub_kind_val: RustSubKind = blk: {
                        const sk = obj.get("sub_kind") orelse break :blk .none;
                        if (sk != .string) break :blk .none;
                        inline for (@typeInfo(RustSubKind).@"enum".fields) |f| {
                            if (std.mem.eql(u8, sk.string, f.name)) break :blk @enumFromInt(f.value);
                        }
                        break :blk .none;
                    };
                    const abi: ?[]const u8 = if (obj.get("abi")) |v| switch (v) {
                        .string => |s| try allocator.dupe(u8, s),
                        else => null,
                    } else null;
                    errdefer if (abi) |a| allocator.free(a);
                    const derives: ?[]const u8 = if (obj.get("derives")) |v| switch (v) {
                        .string => |s| try allocator.dupe(u8, s),
                        else => null,
                    } else null;
                    errdefer if (derives) |d| allocator.free(d);
                    const attributes: ?[]const u8 = if (obj.get("attributes")) |v| switch (v) {
                        .string => |s| try allocator.dupe(u8, s),
                        else => null,
                    } else null;
                    errdefer if (attributes) |at| allocator.free(at);
                    const inner_attributes: ?[]const u8 = if (obj.get("inner_attributes")) |v| switch (v) {
                        .string => |s| try allocator.dupe(u8, s),
                        else => null,
                    } else null;
                    errdefer if (inner_attributes) |ia| allocator.free(ia);
                    const visibility_scope: ?[]const u8 = if (obj.get("visibility_scope")) |v| switch (v) {
                        .string => |s| try allocator.dupe(u8, s),
                        else => null,
                    } else null;
                    errdefer comptime unreachable;
                    return .{ .rust = .{
                        .is_unsafe = if (obj.get("is_unsafe")) |v| (v == .bool and v.bool) else false,
                        .is_async = if (obj.get("is_async")) |v| (v == .bool and v.bool) else false,
                        .is_const = if (obj.get("is_const")) |v| (v == .bool and v.bool) else false,
                        .is_extern = if (obj.get("is_extern")) |v| (v == .bool and v.bool) else false,
                        .is_default = if (obj.get("is_default")) |v| (v == .bool and v.bool) else false,
                        .sub_kind = sub_kind_val,
                        .abi = abi,
                        .derives = derives,
                        .attributes = attributes,
                        .inner_attributes = inner_attributes,
                        .visibility_scope = visibility_scope,
                    } };
                }
                if (std.mem.eql(u8, type_val.string, "zig")) {
                    return .{ .zig = .{
                        .is_comptime = if (obj.get("is_comptime")) |v| (v == .bool and v.bool) else false,
                        .is_mutable = if (obj.get("is_mutable")) |v| (v == .bool and v.bool) else false,
                        .is_inline = if (obj.get("is_inline")) |v| (v == .bool and v.bool) else false,
                        .is_extern = if (obj.get("is_extern")) |v| (v == .bool and v.bool) else false,
                        .is_packed = if (obj.get("is_packed")) |v| (v == .bool and v.bool) else false,
                        .comptime_conditional = if (obj.get("comptime_conditional")) |v| (v == .bool and v.bool) else false,
                        .calling_convention = if (obj.get("calling_convention")) |v| switch (v) {
                            .string => |s| try allocator.dupe(u8, s),
                            else => null,
                        } else null,
                    } };
                }
                return .{ .none = {} };
            },
            else => return .{ .none = {} },
        }
    }

    // Debug output

    /// Write human-readable flag annotations to `writer` for debug tools.
    /// `.none` writes nothing. `.zig` writes bracketed flags like `[comptime]`, `[packed]`.
    pub fn writeDebug(self: LangMeta, writer: *std.Io.Writer) !void {
        switch (self) {
            .none => {},
            .rust => |rm| {
                if (rm.is_unsafe) try writer.print("  [unsafe]", .{});
                if (rm.is_async) try writer.print("  [async]", .{});
                if (rm.is_const) try writer.print("  [const]", .{});
                if (rm.is_extern) try writer.print("  [extern]", .{});
                if (rm.is_default) try writer.print("  [default]", .{});
                if (rm.sub_kind != .none) try writer.print("  [{s}]", .{@tagName(rm.sub_kind)});
                if (rm.abi) |a| try writer.print("  [abi={s}]", .{a});
                if (rm.derives) |d| try writer.print("  [derives={s}]", .{d});
                if (rm.attributes) |at| try writer.print("  [attrs={s}]", .{at});
                if (rm.inner_attributes) |ia| try writer.print("  [inner_attrs={s}]", .{ia});
                if (rm.visibility_scope) |vs| try writer.print("  [vis={s}]", .{vs});
            },
            .zig => |zm| {
                if (zm.is_comptime) try writer.print("  [comptime]", .{});
                if (zm.is_mutable) try writer.print("  [mutable]", .{});
                if (zm.is_extern) try writer.print("  [extern]", .{});
                if (zm.is_packed) try writer.print("  [packed]", .{});
                if (zm.is_inline) try writer.print("  [inline]", .{});
                if (zm.comptime_conditional) try writer.print("  [comptime_conditional]", .{});
                if (zm.calling_convention) |cc| try writer.print("  [callconv={s}]", .{cc});
            },
        }
    }
};

/// Provenance tag for external (phantom) nodes that reference code outside
/// the indexed project: stdlib or third-party dependencies.
pub const ExternalInfo = union(enum) {
    /// The node is project-internal (not external).
    none: void,
    /// The node references a standard library symbol.
    stdlib: void,
    /// The node references a third-party dependency.
    dependency: struct {
        /// Semver version string, or null if the version is unknown.
        version: ?[]const u8,
    },
};

// --- LangMeta generic tests ---

test "node stores LangMeta.none" {
    // Arrange
    const meta = LangMeta{ .none = {} };

    // Assert
    try std.testing.expectEqual(LangMeta.none, meta);
}

test "node with external equals none" {
    // Arrange
    const ext = ExternalInfo{ .none = {} };

    // Assert
    try std.testing.expectEqual(ExternalInfo.none, ext);
}

test "node with external equals stdlib" {
    // Arrange
    const ext = ExternalInfo{ .stdlib = {} };

    // Assert
    try std.testing.expectEqual(ExternalInfo.stdlib, ext);
}

test "node with external equals dependency with version" {
    // Arrange
    const version_str = "1.2.3";
    const ext = ExternalInfo{ .dependency = .{ .version = version_str } };

    // Assert
    switch (ext) {
        .dependency => |dep| {
            try std.testing.expect(dep.version != null);
            try std.testing.expectEqualStrings("1.2.3", dep.version.?);
        },
        else => return error.UnexpectedVariant,
    }
}

test "LangMeta.encodeBinary none returns 0 bytes" {
    // Arrange
    const meta = LangMeta{ .none = {} };
    var buf: [256]u8 = undefined;

    // Act
    const len = meta.encodeBinary(&buf);

    // Assert
    try std.testing.expectEqual(@as(usize, 0), len);
}

test "LangMeta.decodeBinary empty slice returns none" {
    // Act
    const meta = LangMeta.decodeBinary("");

    // Assert
    try std.testing.expectEqual(LangMeta.none, meta);
}

test "LangMeta.decodeBinary unknown tag returns none" {
    // Arrange
    const data = [_]u8{ 99, 0 };

    // Act
    const meta = LangMeta.decodeBinary(&data);

    // Assert
    try std.testing.expectEqual(LangMeta.none, meta);
}

test "LangMeta.binarySize none returns 0" {
    // Arrange
    const meta = LangMeta{ .none = {} };

    // Assert
    try std.testing.expectEqual(@as(usize, 0), meta.binarySize());
}

test "LangMeta.writeJson none writes null" {
    // Arrange
    const meta = LangMeta{ .none = {} };
    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    try meta.writeJson(&stream);
    try aw.writer.flush();

    // Assert
    try std.testing.expectEqualStrings("null", aw.written());
}

test "LangMeta.parseJson null returns none" {
    // Act
    const meta = try LangMeta.parseJson(std.testing.allocator, .null);

    // Assert
    try std.testing.expectEqual(LangMeta.none, meta);
}

test "LangMeta.writeDebug none writes nothing" {
    // Arrange
    const meta = LangMeta{ .none = {} };
    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    try meta.writeDebug(&aw.writer);
    try aw.writer.flush();

    // Assert
    try std.testing.expectEqual(@as(usize, 0), aw.written().len);
}

// --- LangMeta Rust tests ---

test "node stores RustMeta" {
    // Arrange
    const meta = LangMeta{ .rust = .{ .is_unsafe = true, .sub_kind = .trait_ } };

    // Assert
    try std.testing.expect(meta.rust.is_unsafe);
    try std.testing.expectEqual(RustSubKind.trait_, meta.rust.sub_kind);
    try std.testing.expect(!meta.rust.is_async);
}

test "LangMeta.encodeBinary rust returns tag 2" {
    // Arrange
    const meta = LangMeta{ .rust = .{ .is_unsafe = true } };
    var buf: [256]u8 = undefined;

    // Act
    const len = meta.encodeBinary(&buf);

    // Assert
    try std.testing.expect(len >= LangMeta.rust_header_size);
    try std.testing.expectEqual(@as(u8, 2), buf[0]);
}

test "LangMeta binary encode/decode round-trip for rust" {
    // Arrange
    const original = LangMeta{ .rust = .{
        .is_unsafe = true,
        .is_async = true,
        .is_const = false,
        .is_extern = true,
        .is_default = false,
        .sub_kind = .impl_block,
        .abi = "C",
        .derives = "Debug,Clone",
        .attributes = "#[cfg(test)]",
        .inner_attributes = "#![no_std]",
        .visibility_scope = "crate",
    } };
    var buf: [256]u8 = undefined;

    // Act
    const len = original.encodeBinary(&buf);
    const decoded = LangMeta.decodeBinary(buf[0..len]);

    // Assert
    try std.testing.expect(decoded.rust.is_unsafe);
    try std.testing.expect(decoded.rust.is_async);
    try std.testing.expect(!decoded.rust.is_const);
    try std.testing.expect(decoded.rust.is_extern);
    try std.testing.expect(!decoded.rust.is_default);
    try std.testing.expectEqual(RustSubKind.impl_block, decoded.rust.sub_kind);
    try std.testing.expectEqualStrings("C", decoded.rust.abi.?);
    try std.testing.expectEqualStrings("crate", decoded.rust.visibility_scope.?);
    try std.testing.expectEqualStrings("Debug,Clone", decoded.rust.derives.?);
    try std.testing.expectEqualStrings("#[cfg(test)]", decoded.rust.attributes.?);
    try std.testing.expectEqualStrings("#![no_std]", decoded.rust.inner_attributes.?);
}

test "LangMeta.binarySize rust matches actual encoded length" {
    // Arrange
    const meta = LangMeta{ .rust = .{ .is_unsafe = true, .abi = "C", .derives = "Debug", .attributes = "#[inline]", .inner_attributes = "#![no_std]", .visibility_scope = "super" } };
    var buf: [256]u8 = undefined;

    // Act
    const actual_len = meta.encodeBinary(&buf);

    // Assert
    try std.testing.expectEqual(meta.binarySize(), actual_len);
}

test "LangMeta.writeJson rust produces valid JSON with type rust" {
    // Arrange
    const meta = LangMeta{ .rust = .{ .is_unsafe = true, .sub_kind = .trait_ } };
    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    try meta.writeJson(&stream);
    try aw.writer.flush();

    // Assert
    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, aw.written(), .{});
    defer parsed.deinit();
    try std.testing.expect(parsed.value == .object);
    const type_val = parsed.value.object.get("type").?;
    try std.testing.expectEqualStrings("rust", type_val.string);
}

test "LangMeta JSON writeJson/parseJson round-trip for rust" {
    // Arrange
    const original = LangMeta{ .rust = .{
        .is_unsafe = true,
        .is_async = true,
        .sub_kind = .macro_rules,
        .abi = "C",
        .derives = "Debug,Clone",
        .attributes = "#[inline]\n#[must_use]",
        .inner_attributes = "#![no_std]\n#![forbid(unsafe_code)]",
        .visibility_scope = "crate",
    } };
    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    try original.writeJson(&stream);
    try aw.writer.flush();
    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, aw.written(), .{});
    defer parsed.deinit();
    const decoded = try LangMeta.parseJson(std.testing.allocator, parsed.value);
    defer {
        if (decoded.rust.abi) |a| std.testing.allocator.free(a);
        if (decoded.rust.derives) |d| std.testing.allocator.free(d);
        if (decoded.rust.attributes) |at| std.testing.allocator.free(at);
        if (decoded.rust.inner_attributes) |ia| std.testing.allocator.free(ia);
        if (decoded.rust.visibility_scope) |vs| std.testing.allocator.free(vs);
    }

    // Assert
    try std.testing.expect(decoded.rust.is_unsafe);
    try std.testing.expect(decoded.rust.is_async);
    try std.testing.expect(!decoded.rust.is_const);
    try std.testing.expectEqual(RustSubKind.macro_rules, decoded.rust.sub_kind);
    try std.testing.expectEqualStrings("C", decoded.rust.abi.?);
    try std.testing.expectEqualStrings("Debug,Clone", decoded.rust.derives.?);
    try std.testing.expectEqualStrings("#[inline]\n#[must_use]", decoded.rust.attributes.?);
    try std.testing.expectEqualStrings("#![no_std]\n#![forbid(unsafe_code)]", decoded.rust.inner_attributes.?);
    try std.testing.expectEqualStrings("crate", decoded.rust.visibility_scope.?);
}

test "LangMeta.writeDebug rust writes expected flags" {
    // Arrange
    const meta = LangMeta{ .rust = .{ .is_unsafe = true, .is_async = true, .sub_kind = .trait_ } };
    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    try meta.writeDebug(&aw.writer);
    try aw.writer.flush();

    // Assert
    const output = aw.written();
    try std.testing.expect(std.mem.indexOf(u8, output, "[unsafe]") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "[async]") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "[trait_]") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "[const]") == null);
}

// --- LangMeta Zig tests ---

test "node stores ZigMeta with is_comptime true" {
    // Arrange
    const meta = LangMeta{ .zig = .{ .is_comptime = true } };

    // Assert
    try std.testing.expect(meta.zig.is_comptime);
}

test "ZigMeta default values are all false or null" {
    // Arrange
    const meta = ZigMeta{};

    // Assert
    try std.testing.expect(!meta.is_comptime);
    try std.testing.expect(!meta.is_inline);
    try std.testing.expect(!meta.is_extern);
    try std.testing.expect(!meta.is_packed);
    try std.testing.expectEqual(@as(?[]const []const u8, null), meta.error_set_names);
    try std.testing.expectEqual(@as(?[]const []const u8, null), meta.inferred_errors);
    try std.testing.expect(!meta.comptime_conditional);
}

test "LangMeta.encodeBinary zig returns tag and flags" {
    // Arrange
    const meta = LangMeta{ .zig = .{ .is_comptime = true, .is_packed = true } };
    var buf: [256]u8 = undefined;

    // Act
    const len = meta.encodeBinary(&buf);

    // Assert
    try std.testing.expectEqual(@as(usize, LangMeta.zig_header_size), len);
    try std.testing.expectEqual(@as(u8, 1), buf[0]); // tag = zig
    try std.testing.expect(buf[1] & 0x01 != 0); // is_comptime
    try std.testing.expect(buf[1] & 0x20 != 0); // is_packed
}

test "LangMeta binary encode/decode round-trip for zig" {
    // Arrange
    const original = LangMeta{ .zig = .{
        .is_comptime = true,
        .is_inline = true,
        .is_extern = false,
        .comptime_conditional = true,
        .is_mutable = false,
        .is_packed = true,
    } };
    var buf: [256]u8 = undefined;

    // Act
    const len = original.encodeBinary(&buf);
    const decoded = LangMeta.decodeBinary(buf[0..len]);

    // Assert
    try std.testing.expect(decoded.zig.is_comptime);
    try std.testing.expect(decoded.zig.is_inline);
    try std.testing.expect(!decoded.zig.is_extern);
    try std.testing.expect(decoded.zig.comptime_conditional);
    try std.testing.expect(!decoded.zig.is_mutable);
    try std.testing.expect(decoded.zig.is_packed);
}

test "LangMeta.binarySize zig without calling_convention returns 2" {
    // Arrange
    const meta = LangMeta{ .zig = .{ .is_comptime = true } };

    // Assert
    try std.testing.expectEqual(@as(usize, LangMeta.zig_header_size), meta.binarySize());
}

test "LangMeta.binarySize zig matches actual encoded length" {
    // Arrange
    const meta = LangMeta{ .zig = .{ .is_comptime = true } };
    var buf: [256]u8 = undefined;

    // Act
    const actual_len = meta.encodeBinary(&buf);

    // Assert
    try std.testing.expectEqual(meta.binarySize(), actual_len);
}

test "LangMeta.writeJson zig produces valid JSON" {
    // Arrange
    const meta = LangMeta{ .zig = .{ .is_comptime = true } };
    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    try meta.writeJson(&stream);
    try aw.writer.flush();

    // Assert: must parse as JSON
    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, aw.written(), .{});
    defer parsed.deinit();
    try std.testing.expect(parsed.value == .object);
    const type_val = parsed.value.object.get("type").?;
    try std.testing.expectEqualStrings("zig", type_val.string);
}

test "LangMeta JSON writeJson/parseJson round-trip for zig" {
    // Arrange
    const original = LangMeta{ .zig = .{
        .is_comptime = true,
        .is_packed = true,
        .comptime_conditional = true,
    } };
    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    try original.writeJson(&stream);
    try aw.writer.flush();
    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, aw.written(), .{});
    defer parsed.deinit();
    const decoded = try LangMeta.parseJson(std.testing.allocator, parsed.value);

    // Assert
    try std.testing.expect(decoded.zig.is_comptime);
    try std.testing.expect(decoded.zig.is_packed);
    try std.testing.expect(decoded.zig.comptime_conditional);
    try std.testing.expect(!decoded.zig.is_inline);
}

test "LangMeta.writeDebug zig writes expected flags" {
    // Arrange
    const meta = LangMeta{ .zig = .{ .is_comptime = true, .is_packed = true } };
    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    try meta.writeDebug(&aw.writer);
    try aw.writer.flush();

    // Assert
    const output = aw.written();
    try std.testing.expect(std.mem.indexOf(u8, output, "[comptime]") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "[packed]") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "[inline]") == null);
}
