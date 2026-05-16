const std = @import("std");

/// Scope restricts query results to a subtree of the code graph.
/// Supports path prefixes and glob patterns.
pub const Scope = struct {
    raw: []const u8,

    /// Parse a raw scope string into a Scope. An empty string matches everything.
    pub fn parse(raw: []const u8) Scope {
        return .{ .raw = raw };
    }

    /// Return true if `path` falls within this scope.
    /// An empty scope matches everything.
    pub fn matches(self: Scope, path: []const u8) bool {
        if (self.raw.len == 0) return true;
        if (std.mem.indexOf(u8, self.raw, "*") == null) {
            return std.mem.startsWith(u8, path, self.raw);
        }
        return globMatch(self.raw, path);
    }
};

fn globMatch(pattern: []const u8, text: []const u8) bool {
    return globMatchInner(pattern, 0, text, 0);
}

fn globMatchInner(pattern: []const u8, pi_arg: usize, text: []const u8, ti_arg: usize) bool {
    var pi = pi_arg;
    var ti = ti_arg;

    while (pi < pattern.len) {
        if (pattern[pi] == '*') {
            if (pi + 1 < pattern.len and pattern[pi + 1] == '*') {
                // ** matches across path separators
                pi += 2;
                if (pi < pattern.len and pattern[pi] == '/') pi += 1;
                var pos = ti;
                while (pos <= text.len) {
                    if (globMatchInner(pattern, pi, text, pos)) return true;
                    if (pos >= text.len) break;
                    pos += 1;
                }
                return false;
            } else {
                // * matches within a single path segment
                pi += 1;
                var pos = ti;
                while (pos <= text.len) {
                    if (globMatchInner(pattern, pi, text, pos)) return true;
                    if (pos >= text.len or text[pos] == '/') break;
                    pos += 1;
                }
                return false;
            }
        } else {
            if (ti >= text.len or pattern[pi] != text[ti]) return false;
            pi += 1;
            ti += 1;
        }
    }
    return ti == text.len;
}

test "empty scope matches everything" {
    // Arrange
    const scope = Scope.parse("");

    // Act / Assert
    try std.testing.expect(scope.matches("src/parser/tokenizer.zig"));
    try std.testing.expect(scope.matches("lib/utils.zig"));
    try std.testing.expect(scope.matches(""));
}

test "scope by path prefix matches files under that prefix" {
    // Arrange
    const scope = Scope.parse("src/parser/");

    // Act / Assert
    try std.testing.expect(scope.matches("src/parser/tokenizer.zig"));
    try std.testing.expect(scope.matches("src/parser/ast.zig"));
    try std.testing.expect(!scope.matches("src/core/graph.zig"));
    try std.testing.expect(!scope.matches("test/parser_test.zig"));
}

test "scope by glob matches nested files" {
    // Arrange
    const scope = Scope.parse("src/**/*.zig");

    // Act / Assert
    try std.testing.expect(scope.matches("src/core/graph.zig"));
    try std.testing.expect(scope.matches("src/parser/visitor.zig"));
    try std.testing.expect(!scope.matches("test/test_parsing.zig"));
}

test "scope no matches returns false for all paths" {
    // Arrange
    const scope = Scope.parse("nonexistent/");

    // Act / Assert
    try std.testing.expect(!scope.matches("src/core/graph.zig"));
    try std.testing.expect(!scope.matches("nonexistent_other/file.zig"));
}
