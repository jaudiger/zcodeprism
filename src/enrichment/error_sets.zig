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

test "propagation: direct, multi-hop, union, and no-call boundary" {
    // Arrange: build a graph with error_def nodes, functions, and edges.
    //   err_x (error_def, error_set_names=["X"])
    //   err_y (error_def, error_set_names=["Y"])
    //   fn_a (function, uses_type -> err_x)      => declares {X}
    //   fn_b (function, calls -> fn_a)            => inferred {X} via direct call
    //   fn_c (function, calls -> fn_b)            => inferred {X} via 2-hop
    //   fn_d (function, calls -> fn_c)            => inferred {X} via 3-hop
    //   fn_e (function, uses_type -> err_y, calls -> fn_a) => inferred {X, Y} (union)
    //   fn_f (function, uses_type -> fn_a)        => no call edge, no propagation
    //   fn_g (function, returns void, no edges)   => inferred_errors stays null
    const allocator = std.testing.allocator;
    var g = Graph.init("/tmp/test");
    defer g.deinit(allocator);

    const err_x = try g.addNode(allocator, .{
        .id = .root,
        .name = "ErrorX",
        .kind = .error_def,
        .language = .zig,
        .lang_meta = .{ .zig = .{ .error_set_names = &.{"X"} } },
    });
    const err_y = try g.addNode(allocator, .{
        .id = .root,
        .name = "ErrorY",
        .kind = .error_def,
        .language = .zig,
        .lang_meta = .{ .zig = .{ .error_set_names = &.{"Y"} } },
    });
    const fn_a = try g.addNode(allocator, .{ .id = .root, .name = "fnA", .kind = .function, .language = .zig, .lang_meta = .{ .zig = .{} } });
    const fn_b = try g.addNode(allocator, .{ .id = .root, .name = "fnB", .kind = .function, .language = .zig, .lang_meta = .{ .zig = .{} } });
    const fn_c = try g.addNode(allocator, .{ .id = .root, .name = "fnC", .kind = .function, .language = .zig, .lang_meta = .{ .zig = .{} } });
    const fn_d = try g.addNode(allocator, .{ .id = .root, .name = "fnD", .kind = .function, .language = .zig, .lang_meta = .{ .zig = .{} } });
    const fn_e = try g.addNode(allocator, .{ .id = .root, .name = "fnE", .kind = .function, .language = .zig, .lang_meta = .{ .zig = .{} } });
    const fn_f = try g.addNode(allocator, .{ .id = .root, .name = "fnF", .kind = .function, .language = .zig, .lang_meta = .{ .zig = .{} } });
    const fn_g = try g.addNode(allocator, .{ .id = .root, .name = "fnG", .kind = .function, .language = .zig, .lang_meta = .{ .zig = .{} } });

    // fn_a uses_type err_x
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = fn_a, .target_id = err_x, .edge_type = .uses_type });
    // fn_b calls fn_a (direct propagation)
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = fn_b, .target_id = fn_a, .edge_type = .calls });
    // fn_c calls fn_b (2-hop)
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = fn_c, .target_id = fn_b, .edge_type = .calls });
    // fn_d calls fn_c (3-hop)
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = fn_d, .target_id = fn_c, .edge_type = .calls });
    // fn_e uses_type err_y AND calls fn_a (union of {Y} and {X})
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = fn_e, .target_id = err_y, .edge_type = .uses_type });
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = fn_e, .target_id = fn_a, .edge_type = .calls });
    // fn_f uses_type fn_a but does NOT call it
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = fn_f, .target_id = fn_a, .edge_type = .uses_type });

    // Act
    try propagateErrorSets(allocator, &g, Logger.noop);

    // Assert: fn_a has inferred {X} (direct uses_type)
    const a_errors = g.nodes.items[@intFromEnum(fn_a)].lang_meta.zig.inferred_errors.?;
    try std.testing.expectEqual(@as(usize, 1), a_errors.len);
    try std.testing.expectEqualStrings("X", a_errors[0]);

    // Assert: fn_b has inferred {X} (1-hop call)
    const b_errors = g.nodes.items[@intFromEnum(fn_b)].lang_meta.zig.inferred_errors.?;
    try std.testing.expectEqual(@as(usize, 1), b_errors.len);
    try std.testing.expectEqualStrings("X", b_errors[0]);

    // Assert: fn_c has inferred {X} (2-hop)
    const c_errors = g.nodes.items[@intFromEnum(fn_c)].lang_meta.zig.inferred_errors.?;
    try std.testing.expectEqual(@as(usize, 1), c_errors.len);

    // Assert: fn_d has inferred {X} (3-hop)
    const d_errors = g.nodes.items[@intFromEnum(fn_d)].lang_meta.zig.inferred_errors.?;
    try std.testing.expectEqual(@as(usize, 1), d_errors.len);

    // Assert: fn_e has inferred {X, Y} (union from two sources)
    const e_errors = g.nodes.items[@intFromEnum(fn_e)].lang_meta.zig.inferred_errors.?;
    try std.testing.expectEqual(@as(usize, 2), e_errors.len);

    // Assert: fn_f has no inferred_errors (uses_type, not calls)
    try std.testing.expectEqual(
        @as(?[]const []const u8, null),
        g.nodes.items[@intFromEnum(fn_f)].lang_meta.zig.inferred_errors,
    );

    // Assert: fn_g has no inferred_errors (void, no edges)
    try std.testing.expectEqual(
        @as(?[]const []const u8, null),
        g.nodes.items[@intFromEnum(fn_g)].lang_meta.zig.inferred_errors,
    );
}
