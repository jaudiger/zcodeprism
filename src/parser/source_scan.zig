const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const kind_index_mod = @import("../core/kind_index.zig");
const types = @import("../core/types.zig");

const KindIndex = kind_index_mod.KindIndex;

const Graph = graph_mod.Graph;
const NodeId = types.NodeId;

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
    return source;
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

/// Hash the structural skeleton of a function: identifiers and numeric
/// literals are replaced by fixed placeholders so that renaming variables
/// or changing magic numbers does not change the result.
pub fn computeStructuralHash(fn_source: []const u8) u32 {
    var h = std.hash.Wyhash.init(0);
    var in_ident = false;
    var in_number = false;
    for (fn_source) |c| {
        const is_digit = c >= '0' and c <= '9';
        if (isIdentChar(c)) {
            if (is_digit and !in_ident and !in_number) {
                // Bare numeric literal starting with a digit.
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
    }
    return @truncate(h.final());
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
