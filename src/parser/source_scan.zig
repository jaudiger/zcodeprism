const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const kind_index_mod = @import("../core/kind_index.zig");
const logging = @import("../logging.zig");
const types = @import("../core/types.zig");
const phantom_mod = @import("../core/phantom.zig");
const metrics_mod = @import("../core/metrics.zig");
const lang = @import("../languages/language.zig");

const KindIndex = kind_index_mod.KindIndex;

const Field = logging.Field;
const Logger = logging.Logger;
const Graph = graph_mod.Graph;
const NodeId = types.NodeId;
const Language = types.Language;
const ExternalInfo = lang.ExternalInfo;

/// A function's node ID and its inclusive line range within a source file.
/// Used internally to map source positions back to the enclosing function node.
const FnRange = struct { id: NodeId, line_start: u32, line_end: u32 };
const EdgeType = types.EdgeType;
const EdgeSource = types.EdgeSource;
const PhantomManager = phantom_mod.PhantomManager;
const Metrics = metrics_mod.Metrics;

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

/// Check whether position `pos` in `source` falls inside a line comment.
/// Scans backwards from `pos` to the start of the current line looking for `//`.
fn isInsideLineComment(source: []const u8, pos: usize) bool {
    var i: usize = pos;
    while (i > 0) {
        i -= 1;
        if (source[i] == '\n') break;
        if (i > 0 and source[i - 1] == '/' and source[i] == '/') return true;
    }
    // Check if the line starts with "//"
    if (i == 0 and source.len > 1 and source[0] == '/' and source[1] == '/') return true;
    return false;
}

/// Scan `source` for references to the standard library (prefixed by `std_name`)
/// and create phantom nodes for each unique qualified chain.
/// For PascalCase leaf segments (type references), a `uses_type` edge is added
/// from the enclosing function to the phantom node.
///
/// `file_idx` and `file_end_idx` delimit the node range belonging to this file
/// in `graph`. `phantom` manages deduplication of phantom nodes. `external`
/// provides the external-origin metadata attached to each phantom.
pub fn resolveStdPhantoms(
    allocator: std.mem.Allocator,
    graph: *Graph,
    source: []const u8,
    file_idx: usize,
    file_end_idx: usize,
    phantom: *PhantomManager,
    std_name: []const u8,
    language: Language,
    external: ExternalInfo,
    log: Logger,
) !void {
    // Pre-build function line index from file-scoped nodes.
    var fn_ranges: [256]FnRange = undefined;
    var fn_count: usize = 0;
    var fn_cap_warned = false;
    const end = @min(file_end_idx, graph.nodes.items.len);
    for (graph.nodes.items[file_idx..end], file_idx..) |n, i| {
        if (n.kind != .function) continue;
        const ls = n.line_start orelse continue;
        const le = n.line_end orelse continue;
        if (fn_count < fn_ranges.len) {
            fn_ranges[fn_count] = .{ .id = @enumFromInt(i), .line_start = ls, .line_end = le };
            fn_count += 1;
        } else if (!fn_cap_warned) {
            log.warn("function range tracker at capacity, some phantom uses_type edges will be missing", &.{
                Field.uint("capacity", fn_ranges.len),
            });
            fn_cap_warned = true;
        }
    }

    // Running line counter: maintained incrementally as pos advances.
    var current_line: u32 = 1;
    var last_counted_pos: usize = 0;

    var pos: usize = 0;
    while (pos < source.len) {
        // Look for std_name followed by '.'
        if (pos + std_name.len < source.len and
            std.mem.startsWith(u8, source[pos..], std_name) and
            source[pos + std_name.len] == '.')
        {
            // Check word boundary before.
            if (pos > 0 and isIdentChar(source[pos - 1])) {
                pos += 1;
                continue;
            }

            // Skip matches inside line comments (//, ///, //!).
            if (isInsideLineComment(source, pos)) {
                pos += 1;
                continue;
            }

            // Collect the full chain: std.X.Y.Z
            const chain_start = pos;
            pos += std_name.len + 1; // skip "std."
            while (pos < source.len and (isIdentChar(source[pos]) or source[pos] == '.')) {
                pos += 1;
            }
            // Remove trailing dot if any.
            var chain_end = pos;
            if (chain_end > chain_start and source[chain_end - 1] == '.') {
                chain_end -= 1;
            }

            const chain = source[chain_start..chain_end];

            // Skip if just "std" with no further segments.
            if (chain.len <= std_name.len + 1) continue;

            // Determine whether the leaf is a type reference (PascalCase).
            const last_dot = std.mem.lastIndexOfScalar(u8, chain, '.') orelse continue;
            const leaf = chain[last_dot + 1 ..];
            if (leaf.len == 0) continue;

            const is_type = leaf[0] >= 'A' and leaf[0] <= 'Z';

            const phantom_id = try phantom.getOrCreate(allocator, chain, language, external);

            // For type references (PascalCase leaf), create uses_type edge from
            // the containing function using the running line counter.
            if (is_type) {
                // Advance running line counter from last_counted_pos to chain_start.
                for (source[last_counted_pos..chain_start]) |c| {
                    if (c == '\n') current_line += 1;
                }
                last_counted_pos = chain_start;

                // Find containing function from pre-built index.
                const fn_id = findFnByLine(fn_ranges[0..fn_count], current_line);
                if (fn_id) |fid| {
                    _ = try graph.addEdgeIfNew(allocator, .{ .source_id = fid, .target_id = phantom_id, .edge_type = .uses_type, .source = .phantom });
                }
            }
        } else {
            pos += 1;
        }
    }
}

/// Find the function whose line range contains the given line.
fn findFnByLine(ranges: []const FnRange, line: u32) ?NodeId {
    for (ranges) |r| {
        if (line >= r.line_start and line <= r.line_end) return r.id;
    }
    return null;
}
