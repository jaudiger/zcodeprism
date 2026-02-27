//! ZCodePrism -- a Zig 0.15 library that ingests a local codebase, builds a
//! semantic code graph, and exposes it for exploration by LLMs and humans.
//!
//! This root module re-exports every public sub-module so that downstream
//! consumers (CLI, MCP server, tests) can import a single `@import("lib.zig")`
//! and reach any component.

const std = @import("std");
const ts = @import("tree-sitter");

/// Adjacency-list index for fast edge lookups by node.
pub const adjacency = @import("core/adjacency.zig");
/// Edge definition and helpers.
pub const edge = @import("core/edge.zig");
/// Generation-swap support for atomic graph replacement.
pub const generation = @import("core/generation.zig");
/// The central Graph container (nodes + edges + edge index).
pub const graph = @import("core/graph.zig");
/// Node metrics (complexity, fan-in/out, etc.).
pub const metrics_mod = @import("core/metrics.zig");
/// Node definition and helpers.
pub const node = @import("core/node.zig");
/// Phantom node creation for external/stdlib references.
pub const phantom = @import("core/phantom.zig");
/// Shared type definitions: NodeId, NodeKind, EdgeType, Visibility, etc.
pub const types = @import("core/types.zig");

/// Language-agnostic metadata and external-info types.
pub const language = @import("languages/language.zig");
/// Language feature detection and capability queries.
pub const language_support = @import("languages/language_support.zig");
/// Language registry mapping file extensions to parsers.
pub const registry = @import("languages/registry.zig");

/// Zig-specific AST analysis helpers.
pub const zig_ast_analysis = @import("languages/zig/ast_analysis.zig");
/// Zig cross-file edge resolution.
pub const zig_cross_file = @import("languages/zig/cross_file.zig");
/// Zig intra-file edge builder (calls, uses_type, etc.).
pub const zig_edge_builder = @import("languages/zig/edge_builder.zig");
/// Zig indexer hooks for multi-file processing.
pub const zig_indexer_hooks = @import("languages/zig/indexer_hooks.zig");
/// Zig language-specific metadata (comptime, inline, packed, etc.).
pub const zig_meta = @import("languages/zig/meta.zig");
/// Zig tree-sitter visitor producing nodes and edges from a single file.
pub const visitor = @import("languages/zig/visitor.zig");

/// Multi-file indexer orchestrating per-file visitors and cross-file linking.
pub const indexer = @import("parser/indexer.zig");
/// Source map for file-path to content-hash tracking.
pub const source_map = @import("parser/source_map.zig");
/// Low-level tree-sitter API wrappers.
pub const tree_sitter_api = @import("parser/tree_sitter_api.zig");

/// Compact Text Graph (CTG) renderer.
pub const ctg = @import("render/ctg.zig");
/// Mermaid flowchart renderer.
pub const mermaid = @import("render/mermaid.zig");
/// Shared rendering utilities (filtering, sorting, label helpers).
pub const render_common = @import("render/common.zig");

/// Persistent storage backends (binary and JSONL).
pub const storage = @import("storage/storage.zig");

/// Vtable-based structured logging (noop by default, TextStderrLogger for CLI).
pub const logging = @import("logging.zig");

/// The central graph container.
pub const Graph = graph.Graph;
/// A single node in the code graph.
pub const Node = node.Node;
/// Semantic kind of a node (function, type_def, module, etc.).
pub const NodeKind = types.NodeKind;
/// Semantic kind of an edge (calls, uses_type, exports, etc.).
pub const EdgeType = types.EdgeType;
/// Vtable-based logger interface.
pub const Logger = logging.Logger;
/// Visibility of a declaration (public, private, etc.).
pub const Visibility = types.Visibility;

// Zig grammar provided by tree-sitter-zig C library linked in build.zig
extern fn tree_sitter_zig() callconv(.c) *const ts.Language;

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

test {
    _ = @import("core/adjacency.zig");
    _ = @import("core/edge.zig");
    _ = @import("core/generation.zig");
    _ = @import("core/graph.zig");
    _ = @import("core/metrics.zig");
    _ = @import("core/node.zig");
    _ = @import("core/phantom.zig");
    _ = @import("core/types.zig");
    _ = @import("languages/language.zig");
    _ = @import("languages/language_support.zig");
    _ = @import("languages/registry.zig");
    _ = @import("languages/zig/ast_analysis.zig");
    _ = @import("languages/zig/indexer_hooks.zig");
    _ = @import("languages/zig/meta.zig");
    _ = @import("languages/zig/visitor.zig");
    _ = @import("logging.zig");
    _ = @import("parser/source_map.zig");
    _ = @import("render/common.zig");
    _ = @import("render/ctg.zig");
    _ = @import("render/ctg_sections.zig");
    _ = @import("render/mermaid.zig");
    _ = @import("render/mermaid_sections.zig");
    _ = @import("parser/tree_sitter_api.zig");
    _ = @import("storage/storage.zig");
}
