//! Tree-sitter API wrapper for language visitors.
//! Provides shared parsing utilities used by all language-specific visitors.

const std = @import("std");
const ts = @import("tree-sitter");

// Zig grammar provided by tree-sitter-zig C library linked in build.zig.
extern fn tree_sitter_zig() callconv(.c) *const ts.Language;

/// Returns the tree-sitter Language for Zig.
pub fn zigLanguage() *const ts.Language {
    return tree_sitter_zig();
}

/// Look up the tree-sitter Language for a file extension.
/// Returns null if the extension is not supported.
pub fn getLanguageByExtension(ext: []const u8) ?*const ts.Language {
    if (std.mem.eql(u8, ext, ".zig")) return zigLanguage();
    // TODO: if (std.mem.eql(u8, ext, ".rs")) return rustLanguage();
    return null;
}

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
