//! Source-level metrics computed from function bodies. Replaces the old
//! source_scan.computeMetricsForNodes with a single-pass scan that
//! populates all Metrics fields from source text.

const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const metrics_mod = @import("../core/metrics.zig");
const source_scan = @import("../parser/source_scan.zig");

const Graph = graph_mod.Graph;
const Metrics = metrics_mod.Metrics;

/// All source-level counters produced from a single scan of function text.
pub const SourceMetricsResult = struct {
    complexity: u16,
    branches: u16,
    loops: u16,
    error_paths: u16,
    nesting_depth_max: u8,
};

/// Compute all source-level metrics for function nodes in a file range.
/// Iterates `[file_idx, file_end_idx)`, skips non-function nodes and
/// nodes without line info. Writes directly to `node.metrics`.
pub fn computeAllSourceMetrics(graph: *Graph, source: []const u8, file_idx: usize, file_end_idx: usize) void {
    const end = @min(file_end_idx, graph.nodes.items.len);
    for (graph.nodes.items[file_idx..end]) |*n| {
        if (n.kind != .function) continue;
        const ls = n.line_start orelse continue;
        const le = n.line_end orelse continue;

        const lines: u32 = le - ls + 1;
        const fn_source = source_scan.extractLineRange(source, ls, le);
        const sm = computeSourceMetrics(fn_source);
        const sh = source_scan.computeStructuralHash(fn_source);

        n.metrics = Metrics{
            .complexity = sm.complexity,
            .lines = lines,
            .structural_hash = sh,
            .branches = sm.branches,
            .loops = sm.loops,
            .error_paths = sm.error_paths,
            .nesting_depth_max = sm.nesting_depth_max,
        };
    }
}

/// Single-pass scan over function source text producing all counters.
/// Matches keywords at word boundaries and tracks brace nesting depth.
fn computeSourceMetrics(fn_source: []const u8) SourceMetricsResult {
    var complexity: u16 = 1;
    var branches: u16 = 0;
    var loops: u16 = 0;
    var error_paths: u16 = 0;
    var depth: u16 = 0;
    var max_depth: u16 = 0;

    var pos: usize = 0;
    while (pos < fn_source.len) : (pos += 1) {
        const c = fn_source[pos];

        if (c == '{') {
            depth += 1;
            if (depth > max_depth) max_depth = depth;
            continue;
        }
        if (c == '}') {
            depth -|= 1;
            continue;
        }

        // Only match keywords at word boundaries.
        if (pos > 0 and source_scan.isIdentChar(fn_source[pos - 1])) continue;

        switch (c) {
            'i' => {
                if (source_scan.matchKeyword(fn_source[pos..], "if")) {
                    complexity += 1;
                    branches += 1;
                }
            },
            'f' => {
                if (source_scan.matchKeyword(fn_source[pos..], "for")) {
                    complexity += 1;
                    loops += 1;
                }
            },
            'w' => {
                if (source_scan.matchKeyword(fn_source[pos..], "while")) {
                    complexity += 1;
                    loops += 1;
                }
            },
            's' => {
                if (source_scan.matchKeyword(fn_source[pos..], "switch")) {
                    complexity += 1;
                    branches += 1;
                }
            },
            'c' => {
                if (source_scan.matchKeyword(fn_source[pos..], "catch")) {
                    complexity += 1;
                    error_paths += 1;
                }
            },
            'o' => {
                if (source_scan.matchKeyword(fn_source[pos..], "orelse")) {
                    complexity += 1;
                    branches += 1;
                }
            },
            't' => {
                if (source_scan.matchKeyword(fn_source[pos..], "try")) {
                    error_paths += 1;
                }
            },
            'e' => {
                if (source_scan.matchKeyword(fn_source[pos..], "errdefer")) {
                    error_paths += 1;
                }
            },
            else => {},
        }
    }

    return .{
        .complexity = complexity,
        .branches = branches,
        .loops = loops,
        .error_paths = error_paths,
        .nesting_depth_max = if (max_depth > std.math.maxInt(u8)) std.math.maxInt(u8) else @intCast(max_depth),
    };
}

test "single-pass metrics counts all keyword categories" {
    // Arrange
    const src = "fn foo() !void { if (x) { for (items) |i| { try bar(); } } }";

    // Act
    const m = computeSourceMetrics(src);

    // Assert
    try std.testing.expectEqual(@as(u16, 3), m.complexity); // 1 base + if + for
    try std.testing.expectEqual(@as(u16, 1), m.branches); // if
    try std.testing.expectEqual(@as(u16, 1), m.loops); // for
    try std.testing.expectEqual(@as(u16, 1), m.error_paths); // try
    try std.testing.expectEqual(@as(u8, 3), m.nesting_depth_max); // 3 levels of braces
}

test "empty function body has base complexity only" {
    // Arrange
    const src = "fn noop() void {}";

    // Act
    const m = computeSourceMetrics(src);

    // Assert
    try std.testing.expectEqual(@as(u16, 1), m.complexity);
    try std.testing.expectEqual(@as(u16, 0), m.branches);
    try std.testing.expectEqual(@as(u16, 0), m.loops);
    try std.testing.expectEqual(@as(u16, 0), m.error_paths);
    try std.testing.expectEqual(@as(u8, 1), m.nesting_depth_max);
}

test "orelse and errdefer are counted" {
    // Arrange
    const src = "fn f() void { const x = opt orelse 0; errdefer cleanup(); }";

    // Act
    const m = computeSourceMetrics(src);

    // Assert
    try std.testing.expectEqual(@as(u16, 2), m.complexity); // 1 base + orelse
    try std.testing.expectEqual(@as(u16, 1), m.branches); // orelse
    try std.testing.expectEqual(@as(u16, 1), m.error_paths); // errdefer
}

test "switch and catch are counted" {
    // Arrange
    const src = "fn f() void { switch (x) { 0 => {}, else => {} } const y = foo() catch 0; }";

    // Act
    const m = computeSourceMetrics(src);

    // Assert
    try std.testing.expectEqual(@as(u16, 3), m.complexity); // 1 base + switch + catch
    try std.testing.expectEqual(@as(u16, 1), m.branches); // switch
    try std.testing.expectEqual(@as(u16, 1), m.error_paths); // catch
}

test "while is counted as loop" {
    // Arrange
    const src = "fn f() void { while (cond) { } }";

    // Act
    const m = computeSourceMetrics(src);

    // Assert
    try std.testing.expectEqual(@as(u16, 1), m.loops);
}

test "no false positives from identifier substrings" {
    // Arrange: "notify" contains "if" but is not a keyword
    const src = "fn f() void { notify();iformatted(); }";

    // Act
    const m = computeSourceMetrics(src);

    // Assert
    try std.testing.expectEqual(@as(u16, 1), m.complexity);
    try std.testing.expectEqual(@as(u16, 0), m.branches);
}
