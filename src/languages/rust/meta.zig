const std = @import("std");
const graph_mod = @import("../../core/graph.zig");
const node_mod = @import("../../core/node.zig");

const Graph = graph_mod.Graph;
const Node = node_mod.Node;

/// Sub-kind discriminator for Rust-specific node classification.
pub const RustSubKind = enum(u4) {
    /// Regular struct, function, enum, union (no special sub-kind).
    none = 0,
    /// Trait definition (NodeKind = type_def).
    trait_ = 1,
    /// Impl block (NodeKind = type_def).
    impl_block = 2,
    /// macro_rules! definition (NodeKind = function).
    macro_rules = 3,
    /// Type alias (NodeKind = type_def).
    type_alias = 4,
    /// Static variable (NodeKind = constant).
    static_item = 5,
    /// Function signature in trait body (NodeKind = function).
    fn_signature = 6,
    /// Associated type declaration in a trait body (NodeKind = type_def).
    associated_type = 7,
};

pub const RustMeta = struct {
    is_unsafe: bool = false,
    is_async: bool = false,
    is_const: bool = false,
    is_extern: bool = false,
    is_default: bool = false,
    sub_kind: RustSubKind = .none,
    /// ABI string for extern functions.
    abi: ?[]const u8 = null,
    /// Comma-separated derive trait names.
    derives: ?[]const u8 = null,
    /// Raw source text of outer attributes besides derive and test.
    attributes: ?[]const u8 = null,
    /// Raw source text of inner attributes (#![...]) on file or inline module nodes.
    inner_attributes: ?[]const u8 = null,
    /// Restriction scope for pub(...) visibility. Null for bare pub or private.
    visibility_scope: ?[]const u8 = null,
};

/// Tag byte that identifies a Rust payload in the binary encoding.
pub const binary_tag: u8 = 2;

/// Fixed header: tag + flags + sub_kind + abi_len + vs_len + derives_len + attrs_len + inner_attrs_len.
const header_size = 8;

/// Returns a typed read-only view of the Rust metadata attached to `node`,
/// or null if the node has no Rust metadata.
pub fn metaOf(node: *const Node) ?*const RustMeta {
    if (node.language != .rust) return null;
    const ptr = node.lang_meta orelse return null;
    return @ptrCast(@alignCast(ptr));
}

/// Returns a typed mutable view of the Rust metadata attached to `node`.
pub fn metaOfMut(node: *Node) ?*RustMeta {
    if (node.language != .rust) return null;
    const ptr = node.lang_meta orelse return null;
    const const_ptr: *const RustMeta = @ptrCast(@alignCast(ptr));
    return @constCast(const_ptr);
}

/// Allocate `m` on the graph's owned buffers and return a const pointer
/// suitable for assigning to `Node.lang_meta`.
pub fn allocAndAttach(allocator: std.mem.Allocator, graph: *Graph, m: RustMeta) !*const RustMeta {
    const slice = try allocator.alloc(RustMeta, 1);
    errdefer allocator.free(slice);
    slice[0] = m;
    try graph.addOwnedSlice(allocator, RustMeta, slice);
    return &slice[0];
}

/// Upper bound on the byte count produced by `encodeBinary`.
pub fn binarySize(meta: RustMeta) usize {
    return header_size +
        (if (meta.abi) |a| a.len else 0) +
        (if (meta.visibility_scope) |vs| vs.len else 0) +
        (if (meta.derives) |d| d.len else 0) +
        (if (meta.attributes) |at| at.len else 0) +
        (if (meta.inner_attributes) |ia| ia.len else 0);
}

/// Encode `meta` into a caller-provided buffer and return bytes written.
/// `buf` must be at least `binarySize(meta)` bytes long.
pub fn encodeBinary(meta: RustMeta, buf: []u8) usize {
    buf[0] = binary_tag;
    var flags: u8 = 0;
    if (meta.is_unsafe) flags |= 0x01;
    if (meta.is_async) flags |= 0x02;
    if (meta.is_const) flags |= 0x04;
    if (meta.is_extern) flags |= 0x08;
    if (meta.is_default) flags |= 0x10;
    buf[1] = flags;
    buf[2] = @intFromEnum(meta.sub_kind);
    const abi_len: u8 = if (meta.abi) |a| @intCast(a.len) else 0;
    buf[3] = abi_len;
    const vs_len: u8 = if (meta.visibility_scope) |vs| @intCast(vs.len) else 0;
    buf[4] = vs_len;
    const derives_len: u8 = if (meta.derives) |d| @intCast(d.len) else 0;
    buf[5] = derives_len;
    const attrs_len: u8 = if (meta.attributes) |at| @intCast(at.len) else 0;
    buf[6] = attrs_len;
    const inner_attrs_len: u8 = if (meta.inner_attributes) |ia| @intCast(ia.len) else 0;
    buf[7] = inner_attrs_len;
    var pos: usize = header_size;
    if (meta.abi) |a| {
        @memcpy(buf[pos..][0..a.len], a);
        pos += a.len;
    }
    if (meta.visibility_scope) |vs| {
        @memcpy(buf[pos..][0..vs.len], vs);
        pos += vs.len;
    }
    if (meta.derives) |d| {
        @memcpy(buf[pos..][0..d.len], d);
        pos += d.len;
    }
    if (meta.attributes) |at| {
        @memcpy(buf[pos..][0..at.len], at);
        pos += at.len;
    }
    if (meta.inner_attributes) |ia| {
        @memcpy(buf[pos..][0..ia.len], ia);
        pos += ia.len;
    }
    return pos;
}

/// Decode `bytes` (whose first byte is the Rust tag), allocate the result on
/// `graph.owned_buffers`, and return a const pointer. The string fields of
/// the decoded struct borrow into `bytes`, which must outlive the returned
/// pointer.
pub fn decodeBinaryAndAttach(allocator: std.mem.Allocator, graph: *Graph, bytes: []const u8) !*const RustMeta {
    std.debug.assert(bytes.len >= header_size);
    std.debug.assert(bytes[0] == binary_tag);
    const flags = bytes[1];
    const abi_len: usize = bytes[3];
    const vs_len: usize = bytes[4];
    const derives_len: usize = bytes[5];
    const attrs_len: usize = bytes[6];
    const inner_attrs_len: usize = bytes[7];
    const abi_end = header_size + abi_len;
    const vs_end = abi_end + vs_len;
    const derives_end = vs_end + derives_len;
    const attrs_end = derives_end + attrs_len;
    const meta = RustMeta{
        .is_unsafe = flags & 0x01 != 0,
        .is_async = flags & 0x02 != 0,
        .is_const = flags & 0x04 != 0,
        .is_extern = flags & 0x08 != 0,
        .is_default = flags & 0x10 != 0,
        .sub_kind = @enumFromInt(bytes[2]),
        .abi = if (abi_len > 0) bytes[header_size..abi_end] else null,
        .visibility_scope = if (vs_len > 0) bytes[abi_end..vs_end] else null,
        .derives = if (derives_len > 0) bytes[vs_end..derives_end] else null,
        .attributes = if (attrs_len > 0) bytes[derives_end..attrs_end] else null,
        .inner_attributes = if (inner_attrs_len > 0) bytes[attrs_end..][0..inner_attrs_len] else null,
    };
    return try allocAndAttach(allocator, graph, meta);
}

pub fn writeJson(meta: RustMeta, stream: *std.json.Stringify) !void {
    try stream.beginObject();
    try stream.objectField("type");
    try stream.write("rust");
    try stream.objectField("is_unsafe");
    try stream.write(meta.is_unsafe);
    try stream.objectField("is_async");
    try stream.write(meta.is_async);
    try stream.objectField("is_const");
    try stream.write(meta.is_const);
    try stream.objectField("is_extern");
    try stream.write(meta.is_extern);
    try stream.objectField("is_default");
    try stream.write(meta.is_default);
    try stream.objectField("sub_kind");
    try stream.write(@tagName(meta.sub_kind));
    try stream.objectField("abi");
    try stream.write(meta.abi);
    try stream.objectField("derives");
    try stream.write(meta.derives);
    try stream.objectField("attributes");
    try stream.write(meta.attributes);
    try stream.objectField("inner_attributes");
    try stream.write(meta.inner_attributes);
    try stream.objectField("visibility_scope");
    try stream.write(meta.visibility_scope);
    try stream.endObject();
}

fn dupeOptString(allocator: std.mem.Allocator, graph: *Graph, obj: std.json.ObjectMap, key: []const u8) !?[]const u8 {
    const v = obj.get(key) orelse return null;
    return switch (v) {
        .string => |s| try graph.dupeAndOwn(allocator, s),
        else => null,
    };
}

fn getBoolOr(obj: std.json.ObjectMap, key: []const u8, default: bool) bool {
    const v = obj.get(key) orelse return default;
    return v == .bool and v.bool;
}

fn parseEnumString(comptime E: type, obj: std.json.ObjectMap, key: []const u8, default: E) E {
    const v = obj.get(key) orelse return default;
    if (v != .string) return default;
    inline for (@typeInfo(E).@"enum".fields) |f| {
        if (std.mem.eql(u8, v.string, f.name)) return @enumFromInt(f.value);
    }
    return default;
}

/// Parse a Rust-typed lang_meta JSON object and attach the result to `graph`.
/// String fields are duped into `graph.owned_buffers`.
pub fn parseJsonAndAttach(allocator: std.mem.Allocator, graph: *Graph, obj: std.json.ObjectMap) !*const RustMeta {
    const meta = RustMeta{
        .is_unsafe = getBoolOr(obj, "is_unsafe", false),
        .is_async = getBoolOr(obj, "is_async", false),
        .is_const = getBoolOr(obj, "is_const", false),
        .is_extern = getBoolOr(obj, "is_extern", false),
        .is_default = getBoolOr(obj, "is_default", false),
        .sub_kind = parseEnumString(RustSubKind, obj, "sub_kind", .none),
        .abi = try dupeOptString(allocator, graph, obj, "abi"),
        .derives = try dupeOptString(allocator, graph, obj, "derives"),
        .attributes = try dupeOptString(allocator, graph, obj, "attributes"),
        .inner_attributes = try dupeOptString(allocator, graph, obj, "inner_attributes"),
        .visibility_scope = try dupeOptString(allocator, graph, obj, "visibility_scope"),
    };
    return try allocAndAttach(allocator, graph, meta);
}

pub fn writeDebug(meta: RustMeta, writer: *std.Io.Writer) !void {
    if (meta.is_unsafe) try writer.print("  [unsafe]", .{});
    if (meta.is_async) try writer.print("  [async]", .{});
    if (meta.is_const) try writer.print("  [const]", .{});
    if (meta.is_extern) try writer.print("  [extern]", .{});
    if (meta.is_default) try writer.print("  [default]", .{});
    if (meta.sub_kind != .none) try writer.print("  [{s}]", .{@tagName(meta.sub_kind)});
    if (meta.abi) |a| try writer.print("  [abi={s}]", .{a});
    if (meta.derives) |d| try writer.print("  [derives={s}]", .{d});
    if (meta.attributes) |at| try writer.print("  [attrs={s}]", .{at});
    if (meta.inner_attributes) |ia| try writer.print("  [inner_attrs={s}]", .{ia});
    if (meta.visibility_scope) |vs| try writer.print("  [vis={s}]", .{vs});
}

/// FFI calling convention string for extern functions, or null.
pub fn ffiConvention(meta: RustMeta) ?[]const u8 {
    return if (meta.is_extern) meta.abi else null;
}

test "RustSubKind has exactly 8 variants" {
    comptime {
        const fields = @typeInfo(RustSubKind).@"enum".fields;
        std.debug.assert(fields.len == 8);
    }
}

test "RustMeta default values" {
    // Arrange
    const meta = RustMeta{};

    // Assert
    try std.testing.expect(!meta.is_unsafe);
    try std.testing.expect(!meta.is_async);
    try std.testing.expect(!meta.is_const);
    try std.testing.expect(!meta.is_extern);
    try std.testing.expect(!meta.is_default);
    try std.testing.expectEqual(RustSubKind.none, meta.sub_kind);
    try std.testing.expectEqual(@as(?[]const u8, null), meta.abi);
    try std.testing.expectEqual(@as(?[]const u8, null), meta.derives);
    try std.testing.expectEqual(@as(?[]const u8, null), meta.attributes);
    try std.testing.expectEqual(@as(?[]const u8, null), meta.inner_attributes);
    try std.testing.expectEqual(@as(?[]const u8, null), meta.visibility_scope);
}

test "encodeBinary returns tag 2" {
    // Arrange
    const meta = RustMeta{ .is_unsafe = true };
    var buf: [256]u8 = undefined;

    // Act
    const len = encodeBinary(meta, &buf);

    // Assert
    try std.testing.expect(len >= header_size);
    try std.testing.expectEqual(@as(u8, binary_tag), buf[0]);
}

test "binary encode/decode round-trip" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = Graph.init("/tmp/test");
    defer g.deinit(allocator);
    const original = RustMeta{
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
    };
    var buf: [256]u8 = undefined;

    // Act
    const len = encodeBinary(original, &buf);
    const bytes_copy = try g.dupeAndOwn(allocator, buf[0..len]);
    const decoded = try decodeBinaryAndAttach(allocator, &g, bytes_copy);

    // Assert
    try std.testing.expect(decoded.is_unsafe);
    try std.testing.expect(decoded.is_async);
    try std.testing.expect(!decoded.is_const);
    try std.testing.expect(decoded.is_extern);
    try std.testing.expect(!decoded.is_default);
    try std.testing.expectEqual(RustSubKind.impl_block, decoded.sub_kind);
    try std.testing.expectEqualStrings("C", decoded.abi.?);
    try std.testing.expectEqualStrings("crate", decoded.visibility_scope.?);
    try std.testing.expectEqualStrings("Debug,Clone", decoded.derives.?);
    try std.testing.expectEqualStrings("#[cfg(test)]", decoded.attributes.?);
    try std.testing.expectEqualStrings("#![no_std]", decoded.inner_attributes.?);
}

test "binarySize matches actual encoded length" {
    // Arrange
    const meta = RustMeta{ .is_unsafe = true, .abi = "C", .derives = "Debug", .attributes = "#[inline]", .inner_attributes = "#![no_std]", .visibility_scope = "super" };
    var buf: [256]u8 = undefined;

    // Act
    const actual_len = encodeBinary(meta, &buf);

    // Assert
    try std.testing.expectEqual(binarySize(meta), actual_len);
}

test "writeJson produces valid JSON with type rust" {
    // Arrange
    const meta = RustMeta{ .is_unsafe = true, .sub_kind = .trait_ };
    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    try writeJson(meta, &stream);
    try aw.writer.flush();

    // Assert
    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, aw.written(), .{});
    defer parsed.deinit();
    try std.testing.expect(parsed.value == .object);
    const type_val = parsed.value.object.get("type").?;
    try std.testing.expectEqualStrings("rust", type_val.string);
}

test "writeDebug writes expected flags" {
    // Arrange
    const meta = RustMeta{ .is_unsafe = true, .is_async = true, .sub_kind = .trait_ };
    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    try writeDebug(meta, &aw.writer);
    try aw.writer.flush();

    // Assert
    const output = aw.written();
    try std.testing.expect(std.mem.indexOf(u8, output, "[unsafe]") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "[async]") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "[trait_]") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "[const]") == null);
}

test "parseJson round-trip via writeJson" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = Graph.init("/tmp/test");
    defer g.deinit(allocator);
    const original = RustMeta{
        .is_unsafe = true,
        .is_async = false,
        .is_const = true,
        .is_extern = false,
        .is_default = false,
        .sub_kind = .impl_block,
        .abi = "C",
        .derives = "Debug",
        .attributes = "#[cfg(test)]",
        .inner_attributes = null,
        .visibility_scope = "crate",
    };
    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    try writeJson(original, &stream);
    try aw.writer.flush();

    // Act
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, aw.written(), .{});
    defer parsed.deinit();
    const decoded = try parseJsonAndAttach(allocator, &g, parsed.value.object);

    // Assert
    try std.testing.expect(decoded.is_unsafe);
    try std.testing.expect(!decoded.is_async);
    try std.testing.expect(decoded.is_const);
    try std.testing.expectEqual(RustSubKind.impl_block, decoded.sub_kind);
    try std.testing.expectEqualStrings("C", decoded.abi.?);
    try std.testing.expectEqualStrings("Debug", decoded.derives.?);
    try std.testing.expectEqualStrings("#[cfg(test)]", decoded.attributes.?);
    try std.testing.expectEqualStrings("crate", decoded.visibility_scope.?);
    try std.testing.expectEqual(@as(?[]const u8, null), decoded.inner_attributes);
}
