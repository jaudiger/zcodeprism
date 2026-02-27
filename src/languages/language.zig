const std = @import("std");
const logging = @import("../logging.zig");
const ZigMeta = @import("zig/meta.zig").ZigMeta;

/// Language-specific metadata union carried by each graph node.
/// `.zig` holds Zig-specific flags and calling convention.
/// `.none` is used for nodes that need no language-specific information.
pub const LangMeta = union(enum) {
    /// Zig-specific metadata (comptime, inline, extern, packed, etc.).
    zig: ZigMeta,
    /// No language-specific metadata for this node.
    none: void,

    // Binary serialization

    /// Return an upper-bound byte count for binary encoding of this value.
    /// `.none` returns 0; `.zig` returns 2 plus the calling convention length.
    pub fn binarySize(self: LangMeta) usize {
        return switch (self) {
            .zig => |zm| 2 + (if (zm.calling_convention) |cc| cc.len else 0),
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
                var len: usize = 2;
                if (zm.calling_convention) |cc| {
                    if (cc.len <= buf.len - 2) {
                        @memcpy(buf[2..][0..cc.len], cc);
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
        if (data[0] == 1 and data.len >= 2) {
            const flags = data[1];
            return .{ .zig = .{
                .is_comptime = flags & 0x01 != 0,
                .is_inline = flags & 0x02 != 0,
                .is_extern = flags & 0x04 != 0,
                .comptime_conditional = flags & 0x08 != 0,
                .is_mutable = flags & 0x10 != 0,
                .is_packed = flags & 0x20 != 0,
                .calling_convention = if (data.len > 2) data[2..] else null,
            } };
        }
        return .{ .none = {} };
    }

    // JSON serialization

    /// Write a JSON representation of this value to `writer`.
    /// `.none` writes the literal `null`. `.zig` writes `{"type":"zig",...}`.
    pub fn writeJson(self: LangMeta, writer: *std.Io.Writer) !void {
        switch (self) {
            .none => try writer.writeAll("null"),
            .zig => |zm| {
                try writer.writeAll("{\"type\":\"zig\"");
                try writer.print(",\"is_comptime\":{s}", .{if (zm.is_comptime) "true" else "false"});
                try writer.print(",\"is_mutable\":{s}", .{if (zm.is_mutable) "true" else "false"});
                try writer.print(",\"is_inline\":{s}", .{if (zm.is_inline) "true" else "false"});
                try writer.print(",\"is_extern\":{s}", .{if (zm.is_extern) "true" else "false"});
                try writer.print(",\"is_packed\":{s}", .{if (zm.is_packed) "true" else "false"});
                try writer.print(",\"comptime_conditional\":{s}", .{if (zm.comptime_conditional) "true" else "false"});
                try writer.writeAll(",\"calling_convention\":");
                if (zm.calling_convention) |cc| {
                    try writer.writeByte('"');
                    try writer.writeAll(cc);
                    try writer.writeByte('"');
                } else {
                    try writer.writeAll("null");
                }
                try writer.writeByte('}');
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

/// Provenance tag for external (phantom) nodes, i.e. nodes referencing
/// code outside the indexed project such as stdlib or third-party dependencies.
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

/// Classification of an import statement for topo-sort ordering and
/// phantom node resolution.
pub const ImportKind = enum {
    /// Project-internal file resolved via a relative path.
    project_file,
    /// Standard library module (e.g. `@import("std")`).
    stdlib,
    /// External dependency declared in the build manifest.
    dependency,
    /// Cannot be classified without additional context.
    unknown,
};

/// A single import statement extracted from source text.
pub const ImportEntry = struct {
    /// Import path string -- a zero-copy slice into the source text.
    path: []const u8,
    /// Classification of this import (project file, stdlib, dependency, or unknown).
    kind: ImportKind,
};

/// Describes whether a language's import system resolves to individual
/// files or to entire packages (directories).
pub const ImportGranularity = enum {
    /// Each import resolves to a single source file.
    file,
    /// Each import resolves to a directory containing all package files.
    package,
};

/// Language-specific build configuration extracted from a project manifest
/// (e.g. `build.zig.zon` for Zig). All slice data is allocator-owned;
/// call `deinit` to release it.
pub const BuildConfig = struct {
    /// Names of declared external dependencies, or null if none were found.
    /// Each name and the outer slice are individually allocator-owned.
    dependency_names: ?[][]u8 = null,

    /// Free all allocator-owned memory held by this BuildConfig.
    pub fn deinit(self: BuildConfig, allocator: std.mem.Allocator) void {
        if (self.dependency_names) |names| {
            for (names) |name| allocator.free(name);
            allocator.free(names);
        }
    }
};

/// Callback that extracts import entries from raw source text.
/// Writes up to `out.len` entries into the caller-provided buffer and
/// returns the number of entries actually written.
pub const ExtractImportsFn = *const fn (source: []const u8, out: []ImportEntry) usize;

/// Callback that resolves an import path relative to the importing file.
/// Returns a slice into `buf` with the resolved absolute path, or null if
/// `candidate_idx` is out of range or resolution fails. Languages with
/// multiple resolution candidates (e.g. `foo.zig` vs `foo/mod.zig`)
/// increment `candidate_idx` across calls; single-candidate languages
/// return null for any `candidate_idx` > 0.
pub const ResolveImportPathFn = *const fn (buf: []u8, importer_path: []const u8, import_path: []const u8, candidate_idx: usize) ?[]const u8;

/// Callback that parses a build manifest file at the given project root
/// and returns extracted dependency names in a `BuildConfig`.
/// The returned config is allocator-owned; caller must call `deinit`.
pub const ParseBuildConfigFn = *const fn (allocator: std.mem.Allocator, project_root: []const u8, logger: logging.Logger) anyerror!BuildConfig;

test "node stores ZigMeta with is_comptime true" {
    // Arrange
    const meta = LangMeta{ .zig = .{ .is_comptime = true } };

    // Assert
    try std.testing.expect(meta.zig.is_comptime);
}

test "node stores LangMeta.none" {
    // Arrange
    const meta = LangMeta{ .none = {} };

    // Assert
    try std.testing.expectEqual(LangMeta.none, meta);
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

test "LangMeta.encodeBinary zig returns tag and flags" {
    // Arrange
    const meta = LangMeta{ .zig = .{ .is_comptime = true, .is_packed = true } };
    var buf: [256]u8 = undefined;

    // Act
    const len = meta.encodeBinary(&buf);

    // Assert
    try std.testing.expectEqual(@as(usize, 2), len);
    try std.testing.expectEqual(@as(u8, 1), buf[0]); // tag = zig
    try std.testing.expect(buf[1] & 0x01 != 0); // is_comptime
    try std.testing.expect(buf[1] & 0x20 != 0); // is_packed
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

test "LangMeta binary encode/decode round-trip" {
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

test "LangMeta.binarySize none returns 0" {
    // Arrange
    const meta = LangMeta{ .none = {} };

    // Assert
    try std.testing.expectEqual(@as(usize, 0), meta.binarySize());
}

test "LangMeta.binarySize zig without calling_convention returns 2" {
    // Arrange
    const meta = LangMeta{ .zig = .{ .is_comptime = true } };

    // Assert
    try std.testing.expectEqual(@as(usize, 2), meta.binarySize());
}

test "LangMeta.binarySize matches actual encoded length" {
    // Arrange
    const meta = LangMeta{ .zig = .{ .is_comptime = true } };
    var buf: [256]u8 = undefined;

    // Act
    const actual_len = meta.encodeBinary(&buf);

    // Assert
    try std.testing.expectEqual(meta.binarySize(), actual_len);
}

test "LangMeta.writeJson none writes null" {
    // Arrange
    const meta = LangMeta{ .none = {} };
    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    try meta.writeJson(&aw.writer);
    try aw.writer.flush();

    // Assert
    try std.testing.expectEqualStrings("null", aw.written());
}

test "LangMeta.writeJson zig produces valid JSON" {
    // Arrange
    const meta = LangMeta{ .zig = .{ .is_comptime = true } };
    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    try meta.writeJson(&aw.writer);
    try aw.writer.flush();

    // Assert: must parse as JSON
    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, aw.written(), .{});
    defer parsed.deinit();
    try std.testing.expect(parsed.value == .object);
    const type_val = parsed.value.object.get("type").?;
    try std.testing.expectEqualStrings("zig", type_val.string);
}

test "LangMeta.parseJson null returns none" {
    // Act
    const meta = try LangMeta.parseJson(std.testing.allocator, .null);

    // Assert
    try std.testing.expectEqual(LangMeta.none, meta);
}

test "LangMeta JSON writeJson/parseJson round-trip" {
    // Arrange
    const original = LangMeta{ .zig = .{
        .is_comptime = true,
        .is_packed = true,
        .comptime_conditional = true,
    } };
    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    try original.writeJson(&aw.writer);
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
