const std = @import("std");
const types = @import("../core/types.zig");
const graph_mod = @import("../core/graph.zig");
const node_mod = @import("../core/node.zig");
const rust_meta = @import("rust/meta.zig");
const zig_meta = @import("zig/meta.zig");

const Language = types.Language;
const Graph = graph_mod.Graph;
const Node = node_mod.Node;

/// Byte count needed to encode `node.lang_meta` for binary storage.
/// Returns 0 when the node has no language metadata.
pub fn binarySize(node: Node) usize {
    const lang = node.language orelse return 0;
    if (node.lang_meta == null) return 0;
    return switch (lang) {
        .rust => rust_meta.binarySize(rust_meta.metaOf(&node).?.*),
        .zig => zig_meta.binarySize(zig_meta.metaOf(&node).?.*),
    };
}

/// Encode `node.lang_meta` into `buf` and return the byte count written.
/// Returns 0 when the node has no language metadata.
pub fn encodeBinary(node: Node, buf: []u8) usize {
    const lang = node.language orelse return 0;
    if (node.lang_meta == null) return 0;
    return switch (lang) {
        .rust => rust_meta.encodeBinary(rust_meta.metaOf(&node).?.*, buf),
        .zig => zig_meta.encodeBinary(zig_meta.metaOf(&node).?.*, buf),
    };
}

/// Decode `bytes` produced by `encodeBinary` and attach the resulting struct
/// to `graph`. Returns null when `bytes` is empty.
pub fn decodeBinaryAndAttach(allocator: std.mem.Allocator, graph: *Graph, language: Language, bytes: []const u8) !?*const anyopaque {
    if (bytes.len == 0) return null;
    return switch (language) {
        .rust => @ptrCast(try rust_meta.decodeBinaryAndAttach(allocator, graph, bytes)),
        .zig => @ptrCast(try zig_meta.decodeBinaryAndAttach(allocator, graph, bytes)),
    };
}

/// Write `node.lang_meta` as JSON to `stream`. Emits the literal `null` when
/// the node has no language metadata.
pub fn writeJson(node: Node, stream: *std.json.Stringify) !void {
    const lang = node.language orelse {
        try stream.write(null);
        return;
    };
    if (node.lang_meta == null) {
        try stream.write(null);
        return;
    }
    switch (lang) {
        .rust => try rust_meta.writeJson(rust_meta.metaOf(&node).?.*, stream),
        .zig => try zig_meta.writeJson(zig_meta.metaOf(&node).?.*, stream),
    }
}

/// Parse a JSON value into a typed lang_meta struct and attach it to `graph`.
/// Returns null when `val` is `null`, when `language` is unset, when the JSON
/// is not an object, or when the object's `type` field does not match.
pub fn parseJsonAndAttach(allocator: std.mem.Allocator, graph: *Graph, language: ?Language, val: std.json.Value) !?*const anyopaque {
    if (val == .null) return null;
    if (val != .object) return null;
    const lang = language orelse return null;
    const obj = val.object;
    const type_val = obj.get("type") orelse return null;
    if (type_val != .string) return null;
    return switch (lang) {
        .rust => blk: {
            if (!std.mem.eql(u8, type_val.string, "rust")) break :blk null;
            break :blk @ptrCast(try rust_meta.parseJsonAndAttach(allocator, graph, obj));
        },
        .zig => blk: {
            if (!std.mem.eql(u8, type_val.string, "zig")) break :blk null;
            break :blk @ptrCast(try zig_meta.parseJsonAndAttach(allocator, graph, obj));
        },
    };
}

/// Write human-readable flag annotations to `writer` for debug tools.
pub fn writeDebug(node: Node, writer: *std.Io.Writer) !void {
    const lang = node.language orelse return;
    if (node.lang_meta == null) return;
    switch (lang) {
        .rust => try rust_meta.writeDebug(rust_meta.metaOf(&node).?.*, writer),
        .zig => try zig_meta.writeDebug(zig_meta.metaOf(&node).?.*, writer),
    }
}

/// FFI calling convention string for extern functions, or null.
pub fn ffiConvention(node: Node) ?[]const u8 {
    const lang = node.language orelse return null;
    if (node.lang_meta == null) return null;
    return switch (lang) {
        .rust => rust_meta.ffiConvention(rust_meta.metaOf(&node).?.*),
        .zig => zig_meta.ffiConvention(zig_meta.metaOf(&node).?.*),
    };
}

test "dispatch covers every Language variant" {
    comptime {
        const variants = @typeInfo(Language).@"enum".fields.len;
        std.debug.assert(variants == 2);
    }
}

test "writeJson emits null when node has no language" {
    // Arrange
    const node = Node{ .id = .root, .name = "x", .kind = .module };
    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    try writeJson(node, &stream);
    try aw.writer.flush();

    // Assert
    try std.testing.expectEqualStrings("null", aw.written());
}

test "writeJson emits null when lang_meta is null" {
    // Arrange
    const node = Node{ .id = .root, .name = "x", .kind = .module, .language = .rust };
    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    try writeJson(node, &stream);
    try aw.writer.flush();

    // Assert
    try std.testing.expectEqualStrings("null", aw.written());
}

test "binarySize returns 0 when lang_meta is null" {
    // Arrange
    const node = Node{ .id = .root, .name = "x", .kind = .module, .language = .rust };

    // Assert
    try std.testing.expectEqual(@as(usize, 0), binarySize(node));
}

test "encodeBinary writes 0 bytes when lang_meta is null" {
    // Arrange
    const node = Node{ .id = .root, .name = "x", .kind = .module, .language = .rust };
    var buf: [16]u8 = undefined;

    // Act
    const len = encodeBinary(node, &buf);

    // Assert
    try std.testing.expectEqual(@as(usize, 0), len);
}

test "decodeBinaryAndAttach returns null for empty bytes" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = Graph.init("/tmp/test");
    defer g.deinit(allocator);

    // Act
    const result = try decodeBinaryAndAttach(allocator, &g, .rust, "");

    // Assert
    try std.testing.expectEqual(@as(?*const anyopaque, null), result);
}

test "parseJsonAndAttach returns null for JSON null" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = Graph.init("/tmp/test");
    defer g.deinit(allocator);

    // Act
    const result = try parseJsonAndAttach(allocator, &g, .rust, .null);

    // Assert
    try std.testing.expectEqual(@as(?*const anyopaque, null), result);
}

test "writeDebug writes nothing when lang_meta is null" {
    // Arrange
    const node = Node{ .id = .root, .name = "x", .kind = .module, .language = .rust };
    var aw = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer aw.deinit();

    // Act
    try writeDebug(node, &aw.writer);
    try aw.writer.flush();

    // Assert
    try std.testing.expectEqual(@as(usize, 0), aw.written().len);
}
