const std = @import("std");
const types = @import("types.zig");
const lang = @import("../languages/language.zig");
const metrics_mod = @import("metrics.zig");

const NodeId = types.NodeId;
const NodeKind = types.NodeKind;
const Visibility = types.Visibility;
const Language = types.Language;
const LangMeta = lang.LangMeta;
const ExternalInfo = lang.ExternalInfo;

/// A node in the code graph representing a semantic code element.
pub const Node = struct {
    id: NodeId,
    name: []const u8,
    kind: NodeKind,
    language: Language,
    file_path: ?[]const u8 = null,
    line_start: ?u32 = null,
    line_end: ?u32 = null,
    parent_id: ?NodeId = null,
    visibility: Visibility = .private,
    doc: ?[]const u8 = null,
    signature: ?[]const u8 = null,
    content_hash: ?[12]u8 = null,
    metrics: ?metrics_mod.Metrics = null,
    lang_meta: LangMeta = .{ .none = {} },
    external: ExternalInfo = .{ .none = {} },
};

// --- Tests ---

test "node stores and returns doc comment" {
    // Arrange
    const n = Node{
        .id = .root,
        .name = "myFunc",
        .kind = .function,
        .language = .zig,
        .doc = "/// This is a doc comment",
    };

    // Assert
    try std.testing.expect(n.doc != null);
    try std.testing.expectEqualStrings("/// This is a doc comment", n.doc.?);
}

test "node stores and returns signature" {
    // Arrange
    const n = Node{
        .id = .root,
        .name = "myFunc",
        .kind = .function,
        .language = .zig,
        .signature = "pub fn myFunc(a: u32) void",
    };

    // Assert
    try std.testing.expect(n.signature != null);
    try std.testing.expectEqualStrings("pub fn myFunc(a: u32) void", n.signature.?);
}

test "node with null doc" {
    // Arrange
    const n = Node{
        .id = .root,
        .name = "myFunc",
        .kind = .function,
        .language = .zig,
    };

    // Assert
    try std.testing.expectEqual(@as(?[]const u8, null), n.doc);
}

test "node with null signature" {
    // Arrange
    const n = Node{
        .id = .root,
        .name = "myFunc",
        .kind = .function,
        .language = .zig,
    };

    // Assert
    try std.testing.expectEqual(@as(?[]const u8, null), n.signature);
}

test "node with null content_hash" {
    // Arrange
    const n = Node{
        .id = .root,
        .name = "myFunc",
        .kind = .function,
        .language = .zig,
    };

    // Assert
    try std.testing.expectEqual(@as(?[12]u8, null), n.content_hash);
}

test "node with null parent_id" {
    // Arrange
    const n = Node{
        .id = .root,
        .name = "main.zig",
        .kind = .file,
        .language = .zig,
    };

    // Assert
    try std.testing.expectEqual(@as(?NodeId, null), n.parent_id);
}
