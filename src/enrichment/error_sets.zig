//! Error set propagation for Zig code. Propagates declared error names
//! along call edges to a fixed point.

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

/// Propagate error sets along call edges to a fixed point. For each
/// function that calls another, the callee's error_set_names merge into
/// the caller's inferred_errors. Iterates until no new errors are added
/// or max_rounds is reached.
pub fn propagateErrorSets(allocator: std.mem.Allocator, graph: *Graph, logger: Logger) !void {
    // Build a map from function NodeId to its error set names, seeded
    // by uses_type edges pointing at error_def nodes.
    var fn_errors = std.AutoHashMapUnmanaged(NodeId, []const []const u8){};
    defer fn_errors.deinit(allocator);

    for (graph.nodes.items, 0..) |n, i| {
        if (n.kind != .error_def) continue;
        if (n.lang_meta != .zig) continue;
        const names = n.lang_meta.zig.error_set_names orelse continue;

        // Walk uses_type edges where this error_def is the target.
        for (graph.edges.items) |e| {
            if (e.edge_type != .uses_type) continue;
            if (e.target_id != @as(NodeId, @enumFromInt(i))) continue;
            const src_idx = @intFromEnum(e.source_id);
            if (src_idx >= graph.nodes.items.len) continue;
            if (graph.nodes.items[src_idx].kind != .function) continue;
            try fn_errors.put(allocator, e.source_id, names);
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
            const callee_errors = fn_errors.get(e.target_id) orelse continue;
            const gop = try fn_errors.getOrPut(allocator, e.source_id);
            if (!gop.found_existing) {
                gop.value_ptr.* = callee_errors;
                changed = true;
            } else {
                const existing = gop.value_ptr.*;
                if (!isSubset(callee_errors, existing)) {
                    const merged = try mergeErrorSets(allocator, existing, callee_errors);
                    try graph.addOwnedBuffer(allocator, merged.flat_buf);
                    errdefer allocator.free(merged.slices);
                    try graph.addOwnedSlice(allocator, []const u8, merged.slices);
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

/// Pair of a flat backing buffer and an array of slices pointing into it.
const ParsedNames = struct {
    slices: []const []const u8,
    flat_buf: []const u8,
};

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
        return .{ .slices = a, .flat_buf = &.{} };
    }

    var flat_len: usize = extra_len;
    for (a) |name| flat_len += name.len;

    const flat_buf = try allocator.alloc(u8, flat_len);
    errdefer allocator.free(flat_buf);
    const slices = try allocator.alloc([]const u8, count);
    errdefer allocator.free(slices);

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
