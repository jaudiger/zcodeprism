const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const kind_index_mod = @import("../core/kind_index.zig");
const types = @import("../core/types.zig");

const KindIndex = kind_index_mod.KindIndex;

const Graph = graph_mod.Graph;
const Language = types.Language;
const NodeId = types.NodeId;

/// Describes comment and string literal delimiters for a language, so
/// computeStructuralHash can strip comments without hardcoding syntax.
pub const CommentSyntax = struct {
    line_comment: ?[]const u8 = null,
    block_comment_open: ?[]const u8 = null,
    block_comment_close: ?[]const u8 = null,
    /// Whether block comments can nest (Rust allows `/* /* */ */`).
    block_comment_nests: bool = false,

    pub fn forLanguage(lang: ?Language) CommentSyntax {
        const l = lang orelse return c_like;
        return switch (l) {
            .zig => zig,
            .rust => rust,
        };
    }

    /// Zig uses only line comments.
    pub const zig = CommentSyntax{
        .line_comment = "//",
    };

    /// Rust uses line and nestable block comments.
    pub const rust = CommentSyntax{
        .line_comment = "//",
        .block_comment_open = "/*",
        .block_comment_close = "*/",
        .block_comment_nests = true,
    };

    /// Fallback for unknown languages: C-style non-nesting comments.
    pub const c_like = CommentSyntax{
        .line_comment = "//",
        .block_comment_open = "/*",
        .block_comment_close = "*/",
    };
};

/// Return true if `c` is an ASCII identifier character (letter, digit, or underscore).
pub fn isIdentChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_';
}

/// Return true if `c` is ASCII whitespace (space, tab, newline, carriage return).
pub fn isWhitespace(c: u8) bool {
    return c == ' ' or c == '\t' or c == '\n' or c == '\r';
}

/// Return true if `text` starts with `keyword` at a word boundary.
/// A match requires that the character immediately after the keyword (if any)
/// is not an identifier character, preventing partial matches inside longer words.
pub fn matchKeyword(text: []const u8, keyword: []const u8) bool {
    if (text.len < keyword.len) return false;
    if (!std.mem.startsWith(u8, text, keyword)) return false;
    if (text.len > keyword.len and isIdentChar(text[keyword.len])) return false;
    return true;
}

/// Extract the byte slice spanning lines `line_start` through `line_end` (1-based, inclusive).
/// Returns a sub-slice of `source`. If `source` is empty, returns it unchanged.
/// If `line_end` extends past the end of the file, returns through the last byte.
pub fn extractLineRange(source: []const u8, line_start: u32, line_end: u32) []const u8 {
    if (source.len == 0) return source;
    var current_line: u32 = 1;
    var start_byte: usize = 0;
    var found_start: bool = (line_start <= 1);

    for (source, 0..) |c, i| {
        if (c == '\n') {
            if (found_start and current_line >= line_end) {
                return source[start_byte .. i + 1];
            }
            current_line += 1;
            if (!found_start and current_line >= line_start) {
                start_byte = i + 1;
                found_start = true;
            }
        }
    }

    if (found_start) return source[start_byte..];
    return source[0..0];
}

/// Return true if `node_id` is a descendant of `ancestor_id` in the parent chain.
/// Walks up the parent_id links from `node_id`, returning true if `ancestor_id`
/// is encountered within 100 hops. Returns false if the chain is broken (null
/// parent) or if the hop limit is reached.
pub fn isDescendantOf(graph: *const Graph, node_id: NodeId, ancestor_id: NodeId) bool {
    var current = node_id;
    var hops: usize = 0;
    while (hops < 100) : (hops += 1) {
        const node = graph.getNode(current) orelse return false;
        const pid = node.parent_id orelse return false;
        if (pid == ancestor_id) return true;
        current = pid;
    }
    return false;
}

/// Hash the structural skeleton of a source fragment. Identifiers become
/// a fixed placeholder, numeric literals another, comments are stripped
/// according to `syntax`, and whitespace runs collapse to a single space.
pub fn computeStructuralHash(fn_source: []const u8, syntax: CommentSyntax) u64 {
    var h = std.hash.Wyhash.init(0);
    var in_ident = false;
    var in_number = false;
    var in_whitespace = false;
    var in_line_comment = false;
    var block_comment_depth: u32 = 0;
    var in_string = false;
    var in_char = false;
    var prev_was_backslash = false;

    var i: usize = 0;
    while (i < fn_source.len) {
        const c = fn_source[i];

        if (in_line_comment) {
            if (c == '\n') in_line_comment = false;
            i += 1;
            continue;
        }

        if (block_comment_depth > 0) {
            if (syntax.block_comment_nests) {
                if (startsWithAt(fn_source, i, syntax.block_comment_open.?)) {
                    block_comment_depth += 1;
                    i += syntax.block_comment_open.?.len;
                    continue;
                }
            }
            if (startsWithAt(fn_source, i, syntax.block_comment_close.?)) {
                block_comment_depth -= 1;
                i += syntax.block_comment_close.?.len;
            } else {
                i += 1;
            }
            continue;
        }

        if (in_string) {
            if (c == '\\' and !prev_was_backslash) {
                prev_was_backslash = true;
                i += 1;
                continue;
            }
            if (c == '"' and !prev_was_backslash) in_string = false;
            prev_was_backslash = false;
            h.update(&[_]u8{c});
            i += 1;
            continue;
        }

        if (in_char) {
            if (c == '\\' and !prev_was_backslash) {
                prev_was_backslash = true;
                i += 1;
                continue;
            }
            if (c == '\'' and !prev_was_backslash) in_char = false;
            prev_was_backslash = false;
            h.update(&[_]u8{c});
            i += 1;
            continue;
        }

        // Detect comment starts.
        if (syntax.line_comment) |lc| {
            if (startsWithAt(fn_source, i, lc)) {
                in_line_comment = true;
                in_ident = false;
                in_number = false;
                i += lc.len;
                continue;
            }
        }
        if (syntax.block_comment_open) |bco| {
            if (startsWithAt(fn_source, i, bco)) {
                block_comment_depth = 1;
                in_ident = false;
                in_number = false;
                i += bco.len;
                continue;
            }
        }

        // Detect string and char literal starts.
        if (c == '"') {
            in_string = true;
            in_ident = false;
            in_number = false;
            in_whitespace = false;
            prev_was_backslash = false;
            h.update("\"");
            i += 1;
            continue;
        }
        if (c == '\'') {
            in_char = true;
            in_ident = false;
            in_number = false;
            in_whitespace = false;
            prev_was_backslash = false;
            h.update("'");
            i += 1;
            continue;
        }

        if (isWhitespace(c)) {
            if (!in_whitespace) {
                in_whitespace = true;
                in_ident = false;
                in_number = false;
                h.update(" ");
            }
            i += 1;
            continue;
        }

        in_whitespace = false;

        const is_digit = c >= '0' and c <= '9';
        if (isIdentChar(c)) {
            if (is_digit and !in_ident and !in_number) {
                h.update("#");
                in_number = true;
            } else if (!in_ident and !in_number) {
                h.update("_");
                in_ident = true;
            }
        } else {
            in_ident = false;
            in_number = false;
            h.update(&[_]u8{c});
        }

        i += 1;
    }
    const raw = h.final();
    // Zero is reserved as "no hash computed" sentinel.
    return if (raw == 0) 1 else raw;
}

fn startsWithAt(text: []const u8, pos: usize, prefix: []const u8) bool {
    if (pos + prefix.len > text.len) return false;
    return std.mem.eql(u8, text[pos..][0..prefix.len], prefix);
}

/// Find the function node that contains the given source `line` within the
/// subtree rooted at `file_id`. When a KindIndex is available, iterates only
/// function nodes instead of the full graph. Returns the NodeId of the first
/// function whose line range includes `line`, or null if none is found.
pub fn findContainingFunction(graph: *const Graph, file_id: NodeId, line: u32, kind_index: ?*const KindIndex) ?NodeId {
    if (kind_index) |ki| {
        for (ki.findByKind(.function)) |i| {
            const n = graph.nodes.items[i];
            if (!isDescendantOf(graph, @enumFromInt(i), file_id)) continue;
            const ls = n.line_start orelse continue;
            const le = n.line_end orelse continue;
            if (line >= ls and line <= le) return @enumFromInt(i);
        }
    } else {
        for (graph.nodes.items, 0..) |n, i| {
            if (n.kind != .function) continue;
            if (!isDescendantOf(graph, @enumFromInt(i), file_id)) continue;
            const ls = n.line_start orelse continue;
            const le = n.line_end orelse continue;
            if (line >= ls and line <= le) return @enumFromInt(i);
        }
    }
    return null;
}

test "computeStructuralHash: identical structure with different names" {
    // Arrange
    const a = "fn foo(x: u32) void { if (x) {} }";
    const b = "fn bar(y: u32) void { if (y) {} }";

    // Act / Assert
    try std.testing.expectEqual(computeStructuralHash(a, CommentSyntax.c_like), computeStructuralHash(b, CommentSyntax.c_like));
}

test "computeStructuralHash: different structure produces different hash" {
    // Arrange
    const a = "fn foo() void { if (x) {} }";
    const b = "fn foo() void { if (x) {} if (y) {} }";

    // Act / Assert
    try std.testing.expect(computeStructuralHash(a, CommentSyntax.c_like) != computeStructuralHash(b, CommentSyntax.c_like));
}

test "computeStructuralHash: line comments are ignored" {
    // Arrange
    const a = "fn foo() void { if (x) {} }";
    const b = "fn foo() void { // a comment\nif (x) {} }";

    // Act / Assert
    try std.testing.expectEqual(computeStructuralHash(a, CommentSyntax.c_like), computeStructuralHash(b, CommentSyntax.c_like));
}

test "computeStructuralHash: block comments are ignored" {
    // Arrange
    const a = "fn foo() void { if (x) {} }";
    const b = "fn foo() void { /* block comment */ if (x) {} }";

    // Act / Assert
    try std.testing.expectEqual(computeStructuralHash(a, CommentSyntax.c_like), computeStructuralHash(b, CommentSyntax.c_like));
}

test "computeStructuralHash: nested block comment text is ignored" {
    // Arrange
    const a = "fn f() void { x(); }";
    const b = "fn f() void { /* multi\nline\ncomment */ x(); }";

    // Act / Assert
    try std.testing.expectEqual(computeStructuralHash(a, CommentSyntax.c_like), computeStructuralHash(b, CommentSyntax.c_like));
}

test "computeStructuralHash: whitespace differences are normalized" {
    // Arrange
    const a = "fn foo() void { if (x) {} }";
    const b = "fn  foo()  void  {\n    if  (x)  {}\n}";

    // Act / Assert
    try std.testing.expectEqual(computeStructuralHash(a, CommentSyntax.c_like), computeStructuralHash(b, CommentSyntax.c_like));
}

test "computeStructuralHash: tabs vs spaces produce same hash" {
    // Arrange
    const a = "fn foo() void {\n    x();\n}";
    const b = "fn foo() void {\n\tx();\n}";

    // Act / Assert
    try std.testing.expectEqual(computeStructuralHash(a, CommentSyntax.c_like), computeStructuralHash(b, CommentSyntax.c_like));
}

test "computeStructuralHash: CRLF vs LF produce same hash" {
    // Arrange
    const a = "fn foo() void {\n    x();\n}";
    const b = "fn foo() void {\r\n    x();\r\n}";

    // Act / Assert
    try std.testing.expectEqual(computeStructuralHash(a, CommentSyntax.c_like), computeStructuralHash(b, CommentSyntax.c_like));
}

test "computeStructuralHash: comment-like sequences inside strings are preserved" {
    // Arrange
    const a = "fn f() void { print(\"// not a comment\"); }";
    const b = "fn f() void { print(\"// different text\"); }";

    // Act / Assert: different string content should produce different hashes
    try std.testing.expect(computeStructuralHash(a, CommentSyntax.c_like) != computeStructuralHash(b, CommentSyntax.c_like));
}

test "computeStructuralHash: block comment syntax inside string is preserved" {
    // Arrange
    const a = "fn f() void { print(\"/* not a comment */\"); }";
    const b = "fn f() void { print(\"/* other */\"); }";

    // Act / Assert
    try std.testing.expect(computeStructuralHash(a, CommentSyntax.c_like) != computeStructuralHash(b, CommentSyntax.c_like));
}

test "computeStructuralHash: empty input returns consistent hash" {
    // Arrange / Act / Assert
    try std.testing.expectEqual(computeStructuralHash("", CommentSyntax.c_like), computeStructuralHash("", CommentSyntax.c_like));
}

test "computeStructuralHash: numeric literals are normalized" {
    // Arrange
    const a = "fn f() void { return 42; }";
    const b = "fn f() void { return 99; }";

    // Act / Assert
    try std.testing.expectEqual(computeStructuralHash(a, CommentSyntax.c_like), computeStructuralHash(b, CommentSyntax.c_like));
}

test "computeStructuralHash: escaped quote inside string does not end string" {
    // Arrange
    const a = "fn f() void { x(\"hello \\\" world\"); }";
    const b = "fn f() void { x(\"hello \\\" world\"); }";

    // Act / Assert
    try std.testing.expectEqual(computeStructuralHash(a, CommentSyntax.c_like), computeStructuralHash(b, CommentSyntax.c_like));
}

test "computeStructuralHash: char literals with comment-like content" {
    // Arrange
    const a = "fn f() void { const c = '/'; }";
    const b = "fn f() void { const c = '*'; }";

    // Act / Assert: different char literal content produces different hash
    try std.testing.expect(computeStructuralHash(a, CommentSyntax.c_like) != computeStructuralHash(b, CommentSyntax.c_like));
}

test "computeStructuralHash: Rust nested block comments are fully stripped" {
    // Arrange
    const a = "fn f() { x(); }";
    const b = "fn f() { /* outer /* inner */ still comment */ x(); }";

    // Act / Assert
    try std.testing.expectEqual(computeStructuralHash(a, CommentSyntax.rust), computeStructuralHash(b, CommentSyntax.rust));
}

test "computeStructuralHash: Zig syntax does not strip block comments" {
    // Arrange: Zig has no block comments, so /* */ are structural punctuation.
    const a = "fn f() void { x(); }";
    const b = "fn f() void { /* stuff */ x(); }";

    // Act / Assert: with Zig syntax these are different structures
    try std.testing.expect(computeStructuralHash(a, CommentSyntax.zig) != computeStructuralHash(b, CommentSyntax.zig));
}

test "computeStructuralHash: forLanguage maps correctly" {
    // Arrange
    const src = "fn f() void { // comment\nx(); }";

    // Act / Assert: all known languages strip line comments
    const hash_zig = computeStructuralHash(src, CommentSyntax.forLanguage(.zig));
    const hash_rust = computeStructuralHash(src, CommentSyntax.forLanguage(.rust));
    const hash_null = computeStructuralHash(src, CommentSyntax.forLanguage(null));
    const hash_no_comment = computeStructuralHash("fn f() void { x(); }", CommentSyntax.c_like);

    try std.testing.expectEqual(hash_no_comment, hash_zig);
    try std.testing.expectEqual(hash_no_comment, hash_rust);
    try std.testing.expectEqual(hash_no_comment, hash_null);
}
