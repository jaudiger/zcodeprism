//! Enrichment pipeline: two-phase computation split around freeze().
//! Pre-freeze computes source-level metrics (structural_hash, lines).
//! Post-freeze computes fan metrics (fan_in, fan_out) and propagates
//! error sets.

const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const logging = @import("../logging.zig");

pub const source_metrics = @import("source_metrics.zig");
pub const fan_metrics = @import("fan_metrics.zig");
pub const error_sets = @import("error_sets.zig");

const Graph = graph_mod.Graph;
const Logger = logging.Logger;
const Field = logging.Field;

/// Per-file source info for source-level metric computation.
pub const FileSource = struct {
    /// Index of the file node in graph.nodes.
    node_idx: usize,
    /// One past the last node index belonging to this file.
    scope_end: usize,
    /// Full source text of the file.
    source: []const u8,
};

/// Options controlling the enrichment pipeline.
pub const EnrichmentOptions = struct {
    logger: Logger = Logger.noop,
};

/// Pre-freeze pass: source-level metrics only.
/// Idempotent. Call after all nodes exist, before freeze().
pub fn enrichPreFreeze(
    allocator: std.mem.Allocator,
    io: std.Io,
    graph: *Graph,
    file_sources: []const FileSource,
    options: EnrichmentOptions,
) !void {
    _ = allocator;
    const log = options.logger;

    for (file_sources) |fs| {
        source_metrics.computeAllSourceMetrics(graph, fs.source, fs.node_idx, fs.scope_end);
    }
    log.debug(io, "source metrics computed", &.{Field.uint("files", file_sources.len)});
}

/// Post-freeze pass: fan_in, fan_out, and error set propagation.
/// Idempotent.
pub fn enrichPostFreeze(
    allocator: std.mem.Allocator,
    io: std.Io,
    graph: *Graph,
    options: EnrichmentOptions,
) !void {
    const log = options.logger;

    fan_metrics.computeFanOut(graph);
    fan_metrics.computeFanIn(graph);
    log.debug(io, "fan metrics computed", &.{});

    try error_sets.propagateErrorSets(allocator, io, graph, log);
}

test {
    _ = source_metrics;
    _ = fan_metrics;
    _ = error_sets;
}
