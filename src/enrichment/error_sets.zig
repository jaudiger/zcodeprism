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

/// Per-function accumulator maintained during propagation.
/// `slices` is the ordered array registered in graph.owned_buffers.
/// `members` keys are slices into graph-owned storage and must not be freed here.
const ErrorSetEntry = struct {
    slices: []const []const u8,
    members: std.StringHashMapUnmanaged(void),

    fn deinit(self: *ErrorSetEntry, allocator: std.mem.Allocator) void {
        self.members.deinit(allocator);
    }
};

/// Propagate error sets along call edges to a fixed point. For each
/// function that calls another, the callee's error_set_names merge into
/// the caller's inferred_errors. Iterates until no new errors are added
/// or max_rounds is reached.
pub fn propagateErrorSets(allocator: std.mem.Allocator, graph: *Graph, logger: Logger) !void {
    var fn_errors = std.AutoHashMapUnmanaged(NodeId, ErrorSetEntry){};
    defer {
        var it = fn_errors.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.deinit(allocator);
        }
        fn_errors.deinit(allocator);
    }

    // Seed fn_errors from error_def nodes and their uses_type callers.
    for (graph.nodes.items, 0..) |n, i| {
        if (n.kind != .error_def) continue;
        if (n.lang_meta != .zig) continue;
        const names = n.lang_meta.zig.error_set_names orelse continue;

        for (graph.edges.items) |e| {
            if (e.edge_type != .uses_type) continue;
            if (e.target_id != @as(NodeId, @enumFromInt(i))) continue;
            const src_idx = @intFromEnum(e.source_id);
            if (src_idx >= graph.nodes.items.len) continue;
            if (graph.nodes.items[src_idx].kind != .function) continue;

            const gop = try fn_errors.getOrPut(allocator, e.source_id);
            if (!gop.found_existing) {
                var members = std.StringHashMapUnmanaged(void){};
                errdefer members.deinit(allocator);
                for (names) |name| {
                    try members.put(allocator, name, {});
                }
                gop.value_ptr.* = .{ .slices = names, .members = members };
            } else {
                // Union additional names from a second error_def on the
                // same function.
                try mergeIntoEntry(allocator, graph, gop.value_ptr, names);
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
            // Capture before getOrPut may rehash fn_errors.
            const callee_slices = (fn_errors.getPtr(e.target_id) orelse continue).slices;
            const gop = try fn_errors.getOrPut(allocator, e.source_id);
            if (!gop.found_existing) {
                // First time caller is encountered: seed from callee.
                var members = std.StringHashMapUnmanaged(void){};
                errdefer members.deinit(allocator);
                for (callee_slices) |name| {
                    try members.put(allocator, name, {});
                }
                gop.value_ptr.* = .{ .slices = callee_slices, .members = members };
                changed = true;
            } else {
                // Try to merge callee names into caller.
                const prev_len = gop.value_ptr.slices.len;
                try mergeIntoEntry(allocator, graph, gop.value_ptr, callee_slices);
                if (gop.value_ptr.slices.len != prev_len) changed = true;
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
        n.lang_meta.zig.inferred_errors = entry.value_ptr.slices;
    }
}

/// Pair of a flat backing buffer and an array of slices pointing into it.
const ParsedNames = struct {
    slices: [][]const u8,
    flat_buf: []u8,
};

/// Extend `entry` with names from `new_names` that are not already members.
/// Allocates a new merged ParsedNames registered on the graph if any new
/// names are found.
fn mergeIntoEntry(allocator: std.mem.Allocator, graph: *Graph, entry: *ErrorSetEntry, new_names: []const []const u8) !void {
    var additions: std.ArrayList([]const u8) = .empty;
    defer additions.deinit(allocator);

    for (new_names) |name| {
        if (!entry.members.contains(name)) {
            try additions.append(allocator, name);
        }
    }
    if (additions.items.len == 0) return;

    const merged = try buildMergedNames(allocator, entry.slices, additions.items);
    try graph.addOwnedBuffer(allocator, merged.flat_buf);
    errdefer allocator.free(merged.slices);
    try graph.addOwnedSlice(allocator, []const u8, merged.slices);

    // Update members with keys from the new buffer's tail slice.
    const old_len = entry.slices.len;
    for (merged.slices[old_len..]) |s| {
        try entry.members.put(allocator, s, {});
    }
    entry.slices = merged.slices;
}

/// MAF-build a new ParsedNames by appending `additions` after `base`.
/// The returned flat_buf and slices must be registered on the graph by
/// the caller.
fn buildMergedNames(allocator: std.mem.Allocator, base: []const []const u8, additions: []const []const u8) !ParsedNames {
    // Measure
    var flat_len: usize = 0;
    for (base) |name| flat_len += name.len;
    for (additions) |name| flat_len += name.len;

    const count = base.len + additions.len;

    // Allocate
    const flat_buf = try allocator.alloc(u8, flat_len);
    errdefer allocator.free(flat_buf);
    const slices = try allocator.alloc([]const u8, count);
    errdefer allocator.free(slices);

    // Fill
    var pos: usize = 0;
    var si: usize = 0;
    for (base) |name| {
        @memcpy(flat_buf[pos..][0..name.len], name);
        slices[si] = flat_buf[pos..][0..name.len];
        pos += name.len;
        si += 1;
    }
    for (additions) |name| {
        @memcpy(flat_buf[pos..][0..name.len], name);
        slices[si] = flat_buf[pos..][0..name.len];
        pos += name.len;
        si += 1;
    }
    std.debug.assert(pos == flat_len);
    std.debug.assert(si == count);

    return .{ .slices = slices, .flat_buf = flat_buf };
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

test "propagation: union of two error_def seeds on the same function" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = Graph.init("/tmp/test");
    defer g.deinit(allocator);

    const err_a = try g.addNode(allocator, .{
        .id = .root,
        .name = "ErrA",
        .kind = .error_def,
        .language = .zig,
        .lang_meta = .{ .zig = .{ .error_set_names = &.{"A"} } },
    });
    const err_b = try g.addNode(allocator, .{
        .id = .root,
        .name = "ErrB",
        .kind = .error_def,
        .language = .zig,
        .lang_meta = .{ .zig = .{ .error_set_names = &.{"B"} } },
    });
    const fn_x = try g.addNode(allocator, .{ .id = .root, .name = "fnX", .kind = .function, .language = .zig, .lang_meta = .{ .zig = .{} } });
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = fn_x, .target_id = err_a, .edge_type = .uses_type });
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = fn_x, .target_id = err_b, .edge_type = .uses_type });

    // Act
    try propagateErrorSets(allocator, &g, Logger.noop);

    // Assert
    const errors = g.nodes.items[@intFromEnum(fn_x)].lang_meta.zig.inferred_errors.?;
    try std.testing.expectEqual(@as(usize, 2), errors.len);
}

test "propagation: caller already has all callee errors makes no copy" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = Graph.init("/tmp/test");
    defer g.deinit(allocator);

    const err_xy = try g.addNode(allocator, .{
        .id = .root,
        .name = "ErrXY",
        .kind = .error_def,
        .language = .zig,
        .lang_meta = .{ .zig = .{ .error_set_names = &.{ "X", "Y" } } },
    });
    const err_x = try g.addNode(allocator, .{
        .id = .root,
        .name = "ErrX",
        .kind = .error_def,
        .language = .zig,
        .lang_meta = .{ .zig = .{ .error_set_names = &.{"X"} } },
    });
    const fn_a = try g.addNode(allocator, .{ .id = .root, .name = "fnA", .kind = .function, .language = .zig, .lang_meta = .{ .zig = .{} } });
    const fn_b = try g.addNode(allocator, .{ .id = .root, .name = "fnB", .kind = .function, .language = .zig, .lang_meta = .{ .zig = .{} } });
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = fn_a, .target_id = err_x, .edge_type = .uses_type });
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = fn_b, .target_id = err_xy, .edge_type = .uses_type });
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = fn_b, .target_id = fn_a, .edge_type = .calls });

    // Act
    try propagateErrorSets(allocator, &g, Logger.noop);

    // Assert: fn_b already had X from its own seed; calling fn_a adds nothing
    const b_errors = g.nodes.items[@intFromEnum(fn_b)].lang_meta.zig.inferred_errors.?;
    try std.testing.expectEqual(@as(usize, 2), b_errors.len);
}

test "propagation: deduplicates names across multiple call hops" {
    // Arrange: fn_a declares {X}, fn_b calls fn_a AND has uses_type -> err_x
    // (same X). fn_b should end up with exactly one X, not two.
    const allocator = std.testing.allocator;
    var g = Graph.init("/tmp/test");
    defer g.deinit(allocator);

    const err_x = try g.addNode(allocator, .{
        .id = .root,
        .name = "ErrX",
        .kind = .error_def,
        .language = .zig,
        .lang_meta = .{ .zig = .{ .error_set_names = &.{"X"} } },
    });
    const fn_a = try g.addNode(allocator, .{ .id = .root, .name = "fnA", .kind = .function, .language = .zig, .lang_meta = .{ .zig = .{} } });
    const fn_b = try g.addNode(allocator, .{ .id = .root, .name = "fnB", .kind = .function, .language = .zig, .lang_meta = .{ .zig = .{} } });
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = fn_a, .target_id = err_x, .edge_type = .uses_type });
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = fn_b, .target_id = err_x, .edge_type = .uses_type });
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = fn_b, .target_id = fn_a, .edge_type = .calls });

    // Act
    try propagateErrorSets(allocator, &g, Logger.noop);

    // Assert
    const b_errors = g.nodes.items[@intFromEnum(fn_b)].lang_meta.zig.inferred_errors.?;
    try std.testing.expectEqual(@as(usize, 1), b_errors.len);
}

test "propagation: large fan-in does not duplicate" {
    // Arrange: 30 callers all call fn_a which has {E1..E10}
    const allocator = std.testing.allocator;
    var g = Graph.init("/tmp/test");
    defer g.deinit(allocator);

    const err_def = try g.addNode(allocator, .{
        .id = .root,
        .name = "BigErr",
        .kind = .error_def,
        .language = .zig,
        .lang_meta = .{ .zig = .{ .error_set_names = &.{ "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10" } } },
    });
    const fn_a = try g.addNode(allocator, .{ .id = .root, .name = "fnA", .kind = .function, .language = .zig, .lang_meta = .{ .zig = .{} } });
    _ = try g.addEdgeIfNew(allocator, .{ .source_id = fn_a, .target_id = err_def, .edge_type = .uses_type });

    var callers: [30]NodeId = undefined;
    for (&callers, 0..) |*c, i| {
        var name_buf: [8]u8 = undefined;
        const name = std.fmt.bufPrint(&name_buf, "caller{d}", .{i}) catch unreachable;
        const owned_name = try allocator.dupe(u8, name);
        defer allocator.free(owned_name);
        c.* = try g.addNode(allocator, .{ .id = .root, .name = owned_name, .kind = .function, .language = .zig, .lang_meta = .{ .zig = .{} } });
        _ = try g.addEdgeIfNew(allocator, .{ .source_id = c.*, .target_id = fn_a, .edge_type = .calls });
    }

    // Act
    try propagateErrorSets(allocator, &g, Logger.noop);

    // Assert: every caller has exactly 10 errors, no duplicates
    for (callers) |c| {
        const errors = g.nodes.items[@intFromEnum(c)].lang_meta.zig.inferred_errors.?;
        try std.testing.expectEqual(@as(usize, 10), errors.len);
    }
}
