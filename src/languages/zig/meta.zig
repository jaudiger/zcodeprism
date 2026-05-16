const std = @import("std");
const graph_mod = @import("../../core/graph.zig");
const node_mod = @import("../../core/node.zig");

const Graph = graph_mod.Graph;
const Node = node_mod.Node;

pub const ZigMeta = struct {
    is_comptime: bool = false,
    is_mutable: bool = false,
    is_inline: bool = false,
    is_extern: bool = false,
    is_packed: bool = false,
    calling_convention: ?[]const u8 = null,
    error_set_names: ?[]const []const u8 = null,
    inferred_errors: ?[]const []const u8 = null,
    comptime_conditional: bool = false,
};

/// Tag byte that identifies a Zig payload in the binary encoding.
pub const binary_tag: u8 = 1;

/// Fixed header: tag + flags.
const header_size = 2;

/// Returns a typed read-only view of the Zig metadata attached to `node`,
/// or null if the node has no Zig metadata.
pub fn metaOf(node: *const Node) ?*const ZigMeta {
    if (node.language != .zig) return null;
    const ptr = node.lang_meta orelse return null;
    return @ptrCast(@alignCast(ptr));
}

/// Returns a typed mutable view of the Zig metadata attached to `node`.
pub fn metaOfMut(node: *Node) ?*ZigMeta {
    if (node.language != .zig) return null;
    const ptr = node.lang_meta orelse return null;
    const const_ptr: *const ZigMeta = @ptrCast(@alignCast(ptr));
    return @constCast(const_ptr);
}

/// Allocate `m` on the graph's owned buffers and return a const pointer
/// suitable for assigning to `Node.lang_meta`.
pub fn allocAndAttach(allocator: std.mem.Allocator, graph: *Graph, m: ZigMeta) !*const ZigMeta {
    const slice = try allocator.alloc(ZigMeta, 1);
    errdefer allocator.free(slice);
    slice[0] = m;
    try graph.addOwnedSlice(allocator, ZigMeta, slice);
    return &slice[0];
}

/// Upper bound on the byte count produced by `encodeBinary`.
pub fn binarySize(meta: ZigMeta) usize {
    return header_size + (if (meta.calling_convention) |cc| cc.len else 0);
}

/// Encode `meta` into a caller-provided buffer and return bytes written.
/// `buf` must be at least `binarySize(meta)` bytes long.
pub fn encodeBinary(meta: ZigMeta, buf: []u8) usize {
    buf[0] = binary_tag;
    var flags: u8 = 0;
    if (meta.is_comptime) flags |= 0x01;
    if (meta.is_inline) flags |= 0x02;
    if (meta.is_extern) flags |= 0x04;
    if (meta.comptime_conditional) flags |= 0x08;
    if (meta.is_mutable) flags |= 0x10;
    if (meta.is_packed) flags |= 0x20;
    buf[1] = flags;
    var len: usize = header_size;
    if (meta.calling_convention) |cc| {
        if (cc.len <= buf.len - header_size) {
            @memcpy(buf[header_size..][0..cc.len], cc);
            len += cc.len;
        }
    }
    return len;
}

/// Decode `bytes` (whose first byte is the Zig tag), allocate the result on
/// `graph.owned_buffers`, and return a const pointer. The `calling_convention`
/// field borrows into `bytes`, which must outlive the returned pointer.
pub fn decodeBinaryAndAttach(allocator: std.mem.Allocator, graph: *Graph, bytes: []const u8) !*const ZigMeta {
    std.debug.assert(bytes.len >= header_size);
    std.debug.assert(bytes[0] == binary_tag);
    const flags = bytes[1];
    const meta = ZigMeta{
        .is_comptime = flags & 0x01 != 0,
        .is_inline = flags & 0x02 != 0,
        .is_extern = flags & 0x04 != 0,
        .comptime_conditional = flags & 0x08 != 0,
        .is_mutable = flags & 0x10 != 0,
        .is_packed = flags & 0x20 != 0,
        .calling_convention = if (bytes.len > header_size) bytes[header_size..] else null,
    };
    return try allocAndAttach(allocator, graph, meta);
}

pub fn writeJson(meta: ZigMeta, stream: *std.json.Stringify) !void {
    try stream.beginObject();
    try stream.objectField("type");
    try stream.write("zig");
    try stream.objectField("is_comptime");
    try stream.write(meta.is_comptime);
    try stream.objectField("is_mutable");
    try stream.write(meta.is_mutable);
    try stream.objectField("is_inline");
    try stream.write(meta.is_inline);
    try stream.objectField("is_extern");
    try stream.write(meta.is_extern);
    try stream.objectField("is_packed");
    try stream.write(meta.is_packed);
    try stream.objectField("comptime_conditional");
    try stream.write(meta.comptime_conditional);
    try stream.objectField("calling_convention");
    try stream.write(meta.calling_convention);
    try stream.endObject();
}

/// Parse a Zig-typed lang_meta JSON object and attach the result to `graph`.
pub fn parseJsonAndAttach(allocator: std.mem.Allocator, graph: *Graph, obj: std.json.ObjectMap) !*const ZigMeta {
    const calling_convention: ?[]const u8 = if (obj.get("calling_convention")) |v| switch (v) {
        .string => |s| try graph.dupeAndOwn(allocator, s),
        else => null,
    } else null;
    const meta = ZigMeta{
        .is_comptime = if (obj.get("is_comptime")) |v| (v == .bool and v.bool) else false,
        .is_mutable = if (obj.get("is_mutable")) |v| (v == .bool and v.bool) else false,
        .is_inline = if (obj.get("is_inline")) |v| (v == .bool and v.bool) else false,
        .is_extern = if (obj.get("is_extern")) |v| (v == .bool and v.bool) else false,
        .is_packed = if (obj.get("is_packed")) |v| (v == .bool and v.bool) else false,
        .comptime_conditional = if (obj.get("comptime_conditional")) |v| (v == .bool and v.bool) else false,
        .calling_convention = calling_convention,
    };
    return try allocAndAttach(allocator, graph, meta);
}

pub fn writeDebug(meta: ZigMeta, writer: *std.Io.Writer) !void {
    if (meta.is_comptime) try writer.print("  [comptime]", .{});
    if (meta.is_mutable) try writer.print("  [mutable]", .{});
    if (meta.is_extern) try writer.print("  [extern]", .{});
    if (meta.is_packed) try writer.print("  [packed]", .{});
    if (meta.is_inline) try writer.print("  [inline]", .{});
    if (meta.comptime_conditional) try writer.print("  [comptime_conditional]", .{});
    if (meta.calling_convention) |cc| try writer.print("  [callconv={s}]", .{cc});
}

/// FFI calling convention string for extern functions, or null.
pub fn ffiConvention(meta: ZigMeta) ?[]const u8 {
    return if (meta.is_extern) meta.calling_convention else null;
}

test "node stores ZigMeta with is_comptime true" {
    // Arrange
    const meta = ZigMeta{ .is_comptime = true };

    // Assert
    try std.testing.expect(meta.is_comptime);
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

test "encodeBinary returns tag and flags" {
    // Arrange
    const meta = ZigMeta{ .is_comptime = true, .is_packed = true };
    var buf: [256]u8 = undefined;

    // Act
    const len = encodeBinary(meta, &buf);

    // Assert
    try std.testing.expectEqual(@as(usize, header_size), len);
    try std.testing.expectEqual(@as(u8, binary_tag), buf[0]);
    try std.testing.expect(buf[1] & 0x01 != 0);
    try std.testing.expect(buf[1] & 0x20 != 0);
}

test "binary encode/decode round-trip" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = Graph.init("/tmp/test");
    defer g.deinit(allocator);
    const original = ZigMeta{
        .is_comptime = true,
        .is_inline = true,
        .is_extern = false,
        .comptime_conditional = true,
        .is_mutable = false,
        .is_packed = true,
    };
    var buf: [256]u8 = undefined;

    // Act
    const len = encodeBinary(original, &buf);
    const bytes_copy = try g.dupeAndOwn(allocator, buf[0..len]);
    const decoded = try decodeBinaryAndAttach(allocator, &g, bytes_copy);

    // Assert
    try std.testing.expect(decoded.is_comptime);
    try std.testing.expect(decoded.is_inline);
    try std.testing.expect(!decoded.is_extern);
    try std.testing.expect(decoded.comptime_conditional);
    try std.testing.expect(!decoded.is_mutable);
    try std.testing.expect(decoded.is_packed);
}

test "binarySize without calling_convention returns 2" {
    // Arrange
    const meta = ZigMeta{ .is_comptime = true };

    // Assert
    try std.testing.expectEqual(@as(usize, header_size), binarySize(meta));
}

test "binarySize matches actual encoded length" {
    // Arrange
    const meta = ZigMeta{ .is_comptime = true };
    var buf: [256]u8 = undefined;

    // Act
    const actual_len = encodeBinary(meta, &buf);

    // Assert
    try std.testing.expectEqual(binarySize(meta), actual_len);
}

test "writeJson produces valid JSON" {
    // Arrange
    const meta = ZigMeta{ .is_comptime = true };
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
    try std.testing.expectEqualStrings("zig", type_val.string);
}

test "writeDebug writes expected flags" {
    // Arrange
    const meta = ZigMeta{ .is_comptime = true, .is_packed = true };
    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    try writeDebug(meta, &aw.writer);
    try aw.writer.flush();

    // Assert
    const output = aw.written();
    try std.testing.expect(std.mem.indexOf(u8, output, "[comptime]") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "[packed]") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "[inline]") == null);
}

test "parseJson round-trip via writeJson" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = Graph.init("/tmp/test");
    defer g.deinit(allocator);
    const original = ZigMeta{
        .is_comptime = true,
        .is_inline = false,
        .is_extern = true,
        .is_packed = false,
        .is_mutable = false,
        .comptime_conditional = false,
        .calling_convention = "C",
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
    try std.testing.expect(decoded.is_comptime);
    try std.testing.expect(!decoded.is_inline);
    try std.testing.expect(decoded.is_extern);
    try std.testing.expectEqualStrings("C", decoded.calling_convention.?);
}
