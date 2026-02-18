const std = @import("std");
const ts = @import("tree-sitter");

// --- Public module exports ---
pub const binary_storage = @import("storage/binary.zig");
pub const ctg = @import("render/ctg.zig");
pub const edge = @import("core/edge.zig");
pub const generation = @import("core/generation.zig");
pub const graph = @import("core/graph.zig");
pub const indexer = @import("parser/indexer.zig");
pub const jsonl_storage = @import("storage/jsonl.zig");
pub const language = @import("languages/language.zig");
pub const mermaid = @import("render/mermaid.zig");
pub const metrics_mod = @import("core/metrics.zig");
pub const node = @import("core/node.zig");
pub const phantom = @import("core/phantom.zig");
pub const registry = @import("languages/registry.zig");
pub const render_common = @import("render/common.zig");
pub const source_map = @import("parser/source_map.zig");
pub const storage = @import("storage/storage.zig");
pub const tree_sitter_api = @import("parser/tree_sitter_api.zig");
pub const types = @import("core/types.zig");
pub const visitor = @import("languages/zig/visitor.zig");
pub const zig_ast_analysis = @import("languages/zig/ast_analysis.zig");
pub const zig_cross_file = @import("languages/zig/cross_file.zig");
pub const zig_edge_builder = @import("languages/zig/edge_builder.zig");
pub const zig_meta = @import("languages/zig/meta.zig");

// Convenience re-exports
pub const Graph = graph.Graph;
pub const Node = node.Node;
pub const NodeKind = types.NodeKind;
pub const EdgeType = types.EdgeType;
pub const Visibility = types.Visibility;

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

    // Assert: tree-sitter always returns a tree, even for invalid syntax
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

// --- Test discovery for sub-modules ---
test {
    _ = @import("core/edge.zig");
    _ = @import("core/generation.zig");
    _ = @import("core/graph.zig");
    _ = @import("core/metrics.zig");
    _ = @import("core/node.zig");
    _ = @import("core/phantom.zig");
    _ = @import("core/types.zig");
    _ = @import("languages/language.zig");
    _ = @import("languages/registry.zig");
    _ = @import("languages/zig/ast_analysis.zig");
    _ = @import("languages/zig/cross_file.zig");
    _ = @import("languages/zig/edge_builder.zig");
    _ = @import("languages/zig/meta.zig");
    _ = @import("languages/zig/visitor.zig");
    _ = @import("parser/indexer.zig");
    _ = @import("parser/source_map.zig");
    _ = @import("render/common.zig");
    _ = @import("render/ctg.zig");
    _ = @import("render/ctg_sections.zig");
    _ = @import("render/mermaid.zig");
    _ = @import("render/mermaid_sections.zig");
    _ = @import("parser/tree_sitter_api.zig");
    _ = @import("storage/binary.zig");
    _ = @import("storage/jsonl.zig");
    _ = @import("storage/storage.zig");
}
