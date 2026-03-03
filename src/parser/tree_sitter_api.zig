//! Tree-sitter API wrapper for language visitors.
//! Provides shared parsing utilities used by all language-specific visitors.

const ts = @import("tree-sitter");

// Grammar C libraries linked in build.zig.
pub extern fn tree_sitter_rust() callconv(.c) *const ts.Language;
pub extern fn tree_sitter_zig() callconv(.c) *const ts.Language;

/// Parse source code using the given language, returning an AST tree.
/// The caller must call `tree.destroy()` when done.
pub fn parseSource(language: *const ts.Language, source: []const u8) ?*ts.Tree {
    const parser = ts.Parser.create();
    defer parser.destroy();
    parser.setLanguage(language) catch return null;
    return parser.parseString(source, null);
}

/// Extract the source text for a given tree-sitter node.
pub fn nodeText(source: []const u8, node: ts.Node) []const u8 {
    return source[node.startByte()..node.endByte()];
}

/// Count the number of lines in source text.
/// A trailing newline does not count as an extra line.
pub fn countLines(source: []const u8) u32 {
    if (source.len == 0) return 0;
    var count: u32 = 1;
    for (source) |c| {
        if (c == '\n') count += 1;
    }
    // A trailing newline terminates the last line, not a new empty line.
    if (source[source.len - 1] == '\n') count -= 1;
    return count;
}
