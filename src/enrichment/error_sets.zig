//! Error set extraction and propagation for Zig code. Extracts declared
//! error names from error_def signatures and propagates them along call
//! edges to a fixed point.

const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const types = @import("../core/types.zig");
const lang = @import("../languages/language.zig");
const logging = @import("../logging.zig");

const Graph = graph_mod.Graph;
const NodeId = types.NodeId;
const EdgeType = types.EdgeType;
const LangMeta = lang.LangMeta;
const Logger = logging.Logger;
const Field = logging.Field;

/// Parse error_def signatures ("error{A,B,C}") and populate
/// ZigMeta.error_set_names. Only processes nodes with kind == .error_def
/// and language == .zig whose signature contains "error{...}".
pub fn extractErrorSets(allocator: std.mem.Allocator, graph: *Graph) !void {
    for (graph.nodes.items) |*n| {
        if (n.kind != .error_def) continue;
        if (n.language != .zig) continue;
        if (n.lang_meta != .zig) continue;
        if (n.lang_meta.zig.error_set_names != null) continue;

        const sig = n.signature orelse continue;
        const names = try parseErrorSetSignature(allocator, sig) orelse continue;
        // Register backing buffers so they are freed on graph.deinit().
        try graph.addOwnedBuffer(allocator, names.flat_buf);
        try graph.addOwnedBuffer(allocator, std.mem.sliceAsBytes(names.slices));

        n.lang_meta.zig.error_set_names = names.slices;
    }
}

/// Pair of a flat backing buffer and an array of slices pointing into it.
/// Both must be registered in graph.owned_buffers for lifetime management.
const ParsedNames = struct {
    slices: []const []const u8,
    flat_buf: []const u8,
};

/// Parse "error{ Name1, Name2, ... }" into individual name slices.
/// Returns null if the signature does not contain a valid error set body.
fn parseErrorSetSignature(allocator: std.mem.Allocator, sig: []const u8) !?ParsedNames {
    const open = std.mem.indexOfScalar(u8, sig, '{') orelse return null;
    const close = std.mem.lastIndexOfScalar(u8, sig, '}') orelse return null;
    if (close <= open + 1) return null;

    const body = sig[open + 1 .. close];

    // Count names first.
    var count: usize = 0;
    var flat_len: usize = 0;
    var iter = std.mem.splitScalar(u8, body, ',');
    while (iter.next()) |segment| {
        const trimmed = std.mem.trim(u8, segment, " \t\n\r");
        if (trimmed.len == 0) continue;
        count += 1;
        flat_len += trimmed.len;
    }
    if (count == 0) return null;

    // Allocate flat buffer + slice array.
    const flat_buf = try allocator.alloc(u8, flat_len);
    errdefer allocator.free(flat_buf);
    const slices = try allocator.alloc([]const u8, count);
    errdefer allocator.free(slices);

    // Fill.
    var pos: usize = 0;
    var si: usize = 0;
    iter = std.mem.splitScalar(u8, body, ',');
    while (iter.next()) |segment| {
        const trimmed = std.mem.trim(u8, segment, " \t\n\r");
        if (trimmed.len == 0) continue;
        @memcpy(flat_buf[pos..][0..trimmed.len], trimmed);
        slices[si] = flat_buf[pos..][0..trimmed.len];
        pos += trimmed.len;
        si += 1;
    }
    std.debug.assert(pos == flat_len);
    std.debug.assert(si == count);

    return .{ .slices = slices, .flat_buf = flat_buf };
}

/// Propagate error sets along call edges to a fixed point. For each
/// function that calls another, the callee's error_set_names merge into
/// the caller's inferred_errors. Iterates until no new errors are added
/// or max_rounds is reached.
pub fn propagateErrorSets(allocator: std.mem.Allocator, graph: *Graph, logger: Logger) !void {
    // Build a map: for each error_def node, find its parent function (if any)
    // and associate the function with the error set names.
    var fn_errors = std.AutoHashMapUnmanaged(NodeId, []const []const u8){};
    defer fn_errors.deinit(allocator);

    // Collect direct error sets from error_def nodes.
    for (graph.nodes.items, 0..) |n, i| {
        if (n.kind != .error_def) continue;
        if (n.lang_meta != .zig) continue;
        const names = n.lang_meta.zig.error_set_names orelse continue;

        // The error_def node itself is a constant; functions that return it
        // are connected via uses_type edges. Walk uses_type edges where this
        // error_def is the target to find the functions.
        for (graph.edges.items) |e| {
            if (e.edge_type != .uses_type) continue;
            if (e.target_id != @as(NodeId, @enumFromInt(i))) continue;
            const src_idx = @intFromEnum(e.source_id);
            if (src_idx >= graph.nodes.items.len) continue;
            if (graph.nodes.items[src_idx].kind != .function) continue;
            try fn_errors.put(allocator, e.source_id, names);
        }

        // Also check if any function in the same scope returns this error type
        // by name (heuristic: function's signature mentions the error_def name).
        const err_name = n.name;
        for (graph.nodes.items, 0..) |fn_node, fi| {
            if (fn_node.kind != .function) continue;
            if (fn_node.language != .zig) continue;
            const fn_sig = fn_node.signature orelse continue;
            if (std.mem.indexOf(u8, fn_sig, err_name) != null) {
                const fid: NodeId = @enumFromInt(fi);
                if (!fn_errors.contains(fid)) {
                    try fn_errors.put(allocator, fid, names);
                }
            }
        }
    }

    if (fn_errors.count() == 0) {
        logger.debug("no error sets to propagate", &.{});
        return;
    }

    // Propagate along call edges to fixed point.
    const max_rounds: u32 = 100;
    var round: u32 = 0;
    while (round < max_rounds) : (round += 1) {
        var changed = false;

        for (graph.edges.items) |e| {
            if (e.edge_type != .calls) continue;
            // If callee has errors, propagate to caller.
            const callee_errors = fn_errors.get(e.target_id) orelse continue;
            const gop = try fn_errors.getOrPut(allocator, e.source_id);
            if (!gop.found_existing) {
                gop.value_ptr.* = callee_errors;
                changed = true;
            } else {
                // Merge: if callee has names not in caller's set, we need
                // to union. For simplicity with the current data model
                // (slices into flat buffers), we track whether the callee's
                // errors are a superset; if not, create a merged set.
                const existing = gop.value_ptr.*;
                if (!isSubset(callee_errors, existing)) {
                    const merged = try mergeErrorSets(allocator, existing, callee_errors);
                    try graph.addOwnedBuffer(allocator, merged.flat_buf);
                    try graph.addOwnedBuffer(allocator, std.mem.sliceAsBytes(merged.slices));
                    gop.value_ptr.* = merged.slices;
                    changed = true;
                }
            }
        }

        if (!changed) break;
    }

    logger.debug("error set propagation converged", &.{Field.uint("rounds", round + 1)});

    // Write inferred_errors back to nodes.
    var it = fn_errors.iterator();
    while (it.next()) |entry| {
        const idx = @intFromEnum(entry.key_ptr.*);
        if (idx >= graph.nodes.items.len) continue;
        var n = &graph.nodes.items[idx];
        if (n.lang_meta != .zig) continue;
        n.lang_meta.zig.inferred_errors = entry.value_ptr.*;
    }
}

/// True if every name in needle appears in haystack.
fn isSubset(needle: []const []const u8, haystack: []const []const u8) bool {
    for (needle) |n| {
        var found = false;
        for (haystack) |h| {
            if (std.mem.eql(u8, n, h)) {
                found = true;
                break;
            }
        }
        if (!found) return false;
    }
    return true;
}

/// Union two error name sets into a new ParsedNames. Caller must register
/// the returned buffers in graph.owned_buffers.
fn mergeErrorSets(allocator: std.mem.Allocator, a: []const []const u8, b: []const []const u8) !ParsedNames {
    // Count unique names.
    var count: usize = a.len;
    var extra_len: usize = 0;
    for (b) |name| {
        var dup = false;
        for (a) |existing| {
            if (std.mem.eql(u8, name, existing)) {
                dup = true;
                break;
            }
        }
        if (!dup) {
            count += 1;
            extra_len += name.len;
        }
    }

    if (count == a.len) {
        // No new names; should not happen given isSubset check, but be safe.
        return .{ .slices = a, .flat_buf = &.{} };
    }

    // Compute total flat size.
    var flat_len: usize = extra_len;
    for (a) |name| flat_len += name.len;

    const flat_buf = try allocator.alloc(u8, flat_len);
    errdefer allocator.free(flat_buf);
    const slices = try allocator.alloc([]const u8, count);
    errdefer allocator.free(slices);

    // Fill: first all of a, then new names from b.
    var pos: usize = 0;
    var si: usize = 0;
    for (a) |name| {
        @memcpy(flat_buf[pos..][0..name.len], name);
        slices[si] = flat_buf[pos..][0..name.len];
        pos += name.len;
        si += 1;
    }
    for (b) |name| {
        var dup = false;
        for (a) |existing| {
            if (std.mem.eql(u8, name, existing)) {
                dup = true;
                break;
            }
        }
        if (!dup) {
            @memcpy(flat_buf[pos..][0..name.len], name);
            slices[si] = flat_buf[pos..][0..name.len];
            pos += name.len;
            si += 1;
        }
    }
    std.debug.assert(pos == flat_len);
    std.debug.assert(si == count);

    return .{ .slices = slices, .flat_buf = flat_buf };
}

test "parseErrorSetSignature extracts names" {
    // Arrange
    const sig = "error{\n    InvalidToken,\n    UnexpectedEof,\n    BadEncoding,\n}";

    // Act
    const result = (try parseErrorSetSignature(std.testing.allocator, sig)).?;
    defer std.testing.allocator.free(result.flat_buf);
    defer std.testing.allocator.free(result.slices);

    // Assert
    try std.testing.expectEqual(@as(usize, 3), result.slices.len);
    try std.testing.expectEqualStrings("InvalidToken", result.slices[0]);
    try std.testing.expectEqualStrings("UnexpectedEof", result.slices[1]);
    try std.testing.expectEqualStrings("BadEncoding", result.slices[2]);
}

test "parseErrorSetSignature handles single-line" {
    // Arrange
    const sig = "error{Overflow}";

    // Act
    const result = (try parseErrorSetSignature(std.testing.allocator, sig)).?;
    defer std.testing.allocator.free(result.flat_buf);
    defer std.testing.allocator.free(result.slices);

    // Assert
    try std.testing.expectEqual(@as(usize, 1), result.slices.len);
    try std.testing.expectEqualStrings("Overflow", result.slices[0]);
}

test "parseErrorSetSignature returns null for empty set" {
    // Arrange / Act
    const result = try parseErrorSetSignature(std.testing.allocator, "error{}");

    // Assert
    try std.testing.expectEqual(@as(?ParsedNames, null), result);
}

test "parseErrorSetSignature returns null for no braces" {
    // Arrange / Act
    const result = try parseErrorSetSignature(std.testing.allocator, "void");

    // Assert
    try std.testing.expectEqual(@as(?ParsedNames, null), result);
}

test "isSubset returns true when all names present" {
    // Arrange
    const a: []const []const u8 = &.{"Foo"};
    const b: []const []const u8 = &.{ "Foo", "Bar" };

    // Assert
    try std.testing.expect(isSubset(a, b));
}

test "isSubset returns false when name missing" {
    // Arrange
    const a: []const []const u8 = &.{ "Foo", "Baz" };
    const b: []const []const u8 = &.{"Foo"};

    // Assert
    try std.testing.expect(!isSubset(a, b));
}
