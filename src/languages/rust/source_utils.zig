const std = @import("std");

/// Result of extracting a path from a use declaration signature.
pub const PathSpan = struct {
    path: []const u8,
    end: usize,
};

/// Walk an identifier path (ident + :: sequences) starting at `start`.
pub fn scanIdentPath(text: []const u8, start: usize) usize {
    var pos = start;
    while (pos < text.len) {
        if (isIdentChar(text[pos])) {
            pos += 1;
        } else if (pos + 1 < text.len and text[pos] == ':' and text[pos + 1] == ':') {
            pos += 2;
        } else {
            break;
        }
    }
    return pos;
}

/// Extract the path portion from a use declaration signature.
pub fn extractUsePath(sig: []const u8) ?PathSpan {
    const use_idx = std.mem.indexOf(u8, sig, "use ") orelse return null;
    var start = use_idx + 4;
    while (start < sig.len and sig[start] == ' ') start += 1;
    const end = scanIdentPath(sig, start);
    if (end == start) return null;
    return .{ .path = sig[start..end], .end = end };
}

/// Extract an "as Alias" suffix starting at `offset` in `text`.
pub fn extractAlias(text: []const u8, offset: usize) ?[]const u8 {
    var pos = offset;
    while (pos < text.len and text[pos] == ' ') pos += 1;
    if (pos + 3 <= text.len and std.mem.eql(u8, text[pos..][0..3], "as ")) {
        pos += 3;
        while (pos < text.len and text[pos] == ' ') pos += 1;
        const alias_start = pos;
        while (pos < text.len and isIdentChar(text[pos])) pos += 1;
        if (pos > alias_start) return text[alias_start..pos];
    }
    return null;
}

/// Find the position of the closing brace that matches the opening brace at `open_pos`,
/// handling nested brace pairs. Returns null if no match is found.
pub fn findMatchingBrace(text: []const u8, open_pos: usize) ?usize {
    if (open_pos >= text.len or text[open_pos] != '{') return null;
    var depth: usize = 0;
    var pos = open_pos;
    while (pos < text.len) : (pos += 1) {
        if (text[pos] == '{') {
            depth += 1;
        } else if (text[pos] == '}') {
            depth -= 1;
            if (depth == 0) return pos;
        }
    }
    return null;
}

fn isIdentChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_';
}
