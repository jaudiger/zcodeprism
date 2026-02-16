const std = @import("std");
const ts = @import("tree-sitter");

// Zig grammar provided by tree-sitter-zig C library linked in build.zig
extern fn tree_sitter_zig() callconv(.c) *const ts.Language;

// --- Step 1 smoke tests ---

test "parse simple declaration produces source_file with variable_declaration child" {
    // Arrange
    const parser = ts.Parser.create();
    defer parser.destroy();

    try parser.setLanguage(tree_sitter_zig());

    // Act
    const tree = parser.parseString("const x = 42;", null) orelse
        return error.ParseFailed;
    defer tree.destroy();

    const root = tree.rootNode();

    // Assert
    try std.testing.expectEqualStrings("source_file", root.kind());
    try std.testing.expect(root.childCount() > 0);

    const first_child = root.child(0) orelse return error.NoChild;
    try std.testing.expectEqualStrings("variable_declaration", first_child.kind());
}

test "parse empty string produces source_file with zero named children" {
    // Arrange
    const parser = ts.Parser.create();
    defer parser.destroy();

    try parser.setLanguage(tree_sitter_zig());

    // Act
    const tree = parser.parseString("", null) orelse
        return error.ParseFailed;
    defer tree.destroy();

    const root = tree.rootNode();

    // Assert
    try std.testing.expectEqualStrings("source_file", root.kind());
    try std.testing.expectEqual(@as(u32, 0), root.namedChildCount());
}

test "parse invalid syntax returns tree with error nodes" {
    // Arrange
    const parser = ts.Parser.create();
    defer parser.destroy();

    try parser.setLanguage(tree_sitter_zig());

    // Act
    const tree = parser.parseString("fn {{{", null) orelse
        return error.ParseFailed;
    defer tree.destroy();

    const root = tree.rootNode();

    // Assert — tree-sitter always returns a tree, even for invalid syntax
    try std.testing.expectEqualStrings("source_file", root.kind());

    // Walk direct children to find at least one ERROR node
    var found_error = false;
    var i: u32 = 0;
    const child_count = root.childCount();
    while (i < child_count) : (i += 1) {
        if (root.child(i)) |child_node| {
            if (child_node.isError()) {
                found_error = true;
                break;
            }
        }
    }
    try std.testing.expect(found_error);
}

test "tree-sitter types are accessible" {
    comptime {
        // Verify that the tree-sitter binding types exist and are usable
        _ = ts.Node;
        _ = ts.Parser;
        _ = ts.Language;
    }
}
