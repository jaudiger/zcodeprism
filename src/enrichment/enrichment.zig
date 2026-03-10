//! Enrichment pipeline: the sole authority for all node metrics and
//! error set population. Designed as a two-phase pipeline (pre-freeze
//! and post-freeze) so that LSP integration later only needs to re-run
//! the post-freeze pass after adding new edges.

const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const logging = @import("../logging.zig");

pub const source_metrics = @import("source_metrics.zig");
pub const fan_metrics = @import("fan_metrics.zig");
pub const error_sets = @import("error_sets.zig");

const Graph = graph_mod.Graph;
const Logger = logging.Logger;
const Field = logging.Field;

/// Per-file source info needed for source-level metrics.
/// Mirrors the relevant fields of indexer.FileInfo without coupling
/// the enrichment module to the indexer.
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

/// Pre-freeze pass: source metrics + fan-out + error set extraction.
/// Idempotent. Call after all nodes and edges exist, before freeze().
pub fn enrichPreFreeze(
    allocator: std.mem.Allocator,
    graph: *Graph,
    file_sources: []const FileSource,
    options: EnrichmentOptions,
) !void {
    const log = options.logger;

    // Source-level metrics: complexity, lines, structural_hash,
    // branches, loops, error_paths, nesting_depth_max.
    for (file_sources) |fs| {
        source_metrics.computeAllSourceMetrics(graph, fs.source, fs.node_idx, fs.scope_end);
    }
    log.debug("source metrics computed", &.{Field.uint("files", file_sources.len)});

    // Fan-out from edges (calls + uses_type).
    fan_metrics.computeFanOut(graph);
    log.debug("fan-out computed", &.{});

    // Error set name extraction from error_def signatures.
    try error_sets.extractErrorSets(allocator, graph);
    log.debug("error sets extracted", &.{});
}

/// Post-freeze pass: fan-in + error set propagation.
/// Idempotent. Call after freeze(). Re-run after LSP adds edges
/// and re-freezes.
pub fn enrichPostFreeze(
    allocator: std.mem.Allocator,
    graph: *Graph,
    options: EnrichmentOptions,
) !void {
    const log = options.logger;

    // Fan-in from edges (calls + uses_type).
    fan_metrics.computeFanIn(graph);
    log.debug("fan-in computed", &.{});

    // Error set propagation along call edges.
    try error_sets.propagateErrorSets(allocator, graph, log);
}

test {
    _ = source_metrics;
    _ = fan_metrics;
    _ = error_sets;
}
