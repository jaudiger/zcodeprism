const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const logging = @import("../logging.zig");
const node_mod = @import("../core/node.zig");
const edge_mod = @import("../core/edge.zig");
const types = @import("../core/types.zig");
const lang = @import("../languages/language.zig");
const lang_support = @import("../languages/language_support.zig");
const registry_mod = @import("../languages/registry.zig");
const phantom_mod = @import("../core/phantom.zig");
const metrics_mod = @import("../core/metrics.zig");
const source_scan = @import("source_scan.zig");

const Field = logging.Field;
const Logger = logging.Logger;

const Graph = graph_mod.Graph;
const Node = node_mod.Node;
const Edge = edge_mod.Edge;
const NodeId = types.NodeId;
const NodeKind = types.NodeKind;
const EdgeType = types.EdgeType;
const EdgeSource = types.EdgeSource;
const Visibility = types.Visibility;
const Language = types.Language;
const ExternalInfo = lang.ExternalInfo;
const Registry = registry_mod.Registry;
const PhantomManager = phantom_mod.PhantomManager;
const Metrics = metrics_mod.Metrics;

/// Configuration for `indexDirectory`.
///
/// - `exclude_paths`: relative paths or basenames to skip during discovery.
/// - `incremental`: when true, files whose content hash matches an existing
///   file node in the graph are skipped.
/// - `logger`: structured logger for diagnostics (defaults to noop).
pub const IndexOptions = struct {
    exclude_paths: []const []const u8 = &.{},
    incremental: bool = false,
    logger: Logger = Logger.noop,
};

/// Summary counters returned by `indexDirectory` after a complete run.
///
/// - `files_indexed`: files successfully parsed and added to the graph.
/// - `files_skipped`: files skipped in incremental mode (unchanged hash).
/// - `files_errored`: files that could not be read or parsed.
pub const IndexResult = struct {
    files_indexed: usize = 0,
    files_skipped: usize = 0,
    files_errored: usize = 0,
};

/// Internal file entry collected during discovery.
const FileEntry = struct {
    rel_path: []const u8,
    basename: []const u8,
    content: []const u8,
    content_hash: [12]u8,
    lang_support: *const lang_support.LanguageSupport,
    import_count: usize = 0,
    /// Whether ownership of content has been transferred to the consumer.
    content_consumed: bool = false,
    /// Whether ownership of rel_path has been transferred to the consumer.
    path_consumed: bool = false,
};

/// Tracks a parsed file's node index and source for post-processing.
const FileInfo = struct {
    idx: usize,
    scope_end: usize,
    source: []const u8,
    lang_support: *const lang_support.LanguageSupport,
};

/// Discover and index all supported source files under `project_root`, populating
/// `graph` with file nodes, declaration nodes, intra- and inter-file edges
/// (imports, calls, uses_type), phantom nodes for external references, and
/// per-function metrics (line count, cyclomatic complexity).
///
/// Files are parsed in topological order so that imported files are visited
/// before their importers. After all files are processed, the graph is frozen.
///
/// Returns an `IndexResult` with per-file counters. The caller owns `graph`;
/// the indexer transfers ownership of file content and path buffers into
/// `graph.owned_buffers`.
pub fn indexDirectory(
    allocator: std.mem.Allocator,
    project_root: []const u8,
    graph: *Graph,
    options: IndexOptions,
) !IndexResult {
    var result = IndexResult{};
    const log = options.logger.withScope("indexer");

    // Discover source files in the project directory.
    var dir = try std.fs.openDirAbsolute(project_root, .{ .iterate = true });
    defer dir.close();

    var file_entries = std.ArrayListUnmanaged(FileEntry){};
    defer file_entries.deinit(allocator);

    {
        var walker = try dir.walk(allocator);
        defer walker.deinit();

        while (try walker.next()) |entry| {
            if (entry.kind != .file) continue;
            const ext = std.fs.path.extension(entry.path);
            const file_lang = Registry.getByExtension(ext) orelse continue;
            if (isExcluded(entry.path, options.exclude_paths)) continue;

            const file = dir.openFile(entry.path, .{}) catch {
                log.warn("file read error", &.{Field.string("path", entry.path)});
                result.files_errored += 1;
                continue;
            };
            defer file.close();
            const content = file.readToEndAlloc(allocator, 10 * 1024 * 1024) catch {
                result.files_errored += 1;
                continue;
            };

            errdefer allocator.free(content);

            const hash = computeContentHash(content);
            const rel_path = try allocator.dupe(u8, entry.path);
            errdefer allocator.free(rel_path);
            const basename = std.fs.path.basename(rel_path);

            try file_entries.append(allocator, .{
                .rel_path = rel_path,
                .basename = basename,
                .content = content,
                .content_hash = hash,
                .lang_support = file_lang,
            });
        }
    }

    // Cleanup non-consumed entries (skipped or error). This defer runs before
    // the deinit defers above (reverse declaration order), so items are valid.
    defer {
        for (file_entries.items) |fe| {
            if (!fe.content_consumed) allocator.free(fe.content);
            if (!fe.path_consumed) allocator.free(fe.rel_path);
        }
    }

    log.info("discovered files", &.{
        Field.uint("count", file_entries.items.len),
        Field.string("root", project_root),
    });

    if (file_entries.items.len == 0) {
        log.debug("no files to index", &.{});
        try graph.freeze();
        return result;
    }

    // Pre-allocate graph capacity: rough estimate of ~30 nodes and ~20 edges per file.
    // Each file contributes 2 owned buffers (content + rel_path).
    {
        const file_count = file_entries.items.len;
        const est_nodes: u32 = @intCast(file_count * 30);
        const est_edges: u32 = @intCast(file_count * 20);
        const est_bufs: u32 = @intCast(graph.owned_buffers.items.len + file_count * 2);
        try graph.nodes.ensureTotalCapacity(graph.allocator, est_nodes);
        try graph.edges.ensureTotalCapacity(graph.allocator, est_edges);
        try graph.edge_index.ensureTotalCapacity(graph.allocator, est_edges);
        try graph.owned_buffers.ensureTotalCapacity(graph.allocator, est_bufs);
    }

    // Topological sort: imported files are parsed before their importers.
    try topoSortFiles(allocator, file_entries.items);

    // Load build config (if the language supports it).
    var build_config: ?lang.BuildConfig = null;
    defer if (build_config) |*bc| bc.deinit(allocator);
    if (file_entries.items.len > 0) {
        if (file_entries.items[0].lang_support.parseBuildConfigFn) |parse_config_fn| {
            build_config = parse_config_fn(allocator, project_root, log) catch null;
        }
    }

    // Parse each file through the visitor in dependency order.
    var file_infos = std.ArrayListUnmanaged(FileInfo){};
    defer file_infos.deinit(allocator);

    for (file_entries.items, 0..) |fe, entry_idx| {
        // Incremental check: skip if existing file node has matching hash.
        if (options.incremental) {
            if (findExistingFileNode(graph, fe.rel_path)) |existing_idx| {
                const existing = graph.nodes.items[existing_idx];
                if (existing.content_hash) |old_hash| {
                    if (std.mem.eql(u8, &old_hash, &fe.content_hash)) {
                        log.debug("skipping unchanged file", &.{Field.string("path", fe.rel_path)});
                        result.files_skipped += 1;
                        continue;
                    }
                }
            }
        }

        const parse_fn = fe.lang_support.parseFn orelse {
            result.files_errored += 1;
            continue;
        };

        const before_count = graph.nodeCount();

        // Graph owns the backing memory for all node slices (name, doc, signature).
        // Ownership must be established before parse_fn, which creates those slices.
        try graph.addOwnedBuffer(fe.content);
        file_entries.items[entry_idx].content_consumed = true;
        try graph.addOwnedBuffer(fe.rel_path);
        file_entries.items[entry_idx].path_consumed = true;

        // Call visitor via language support. Creates file node + children + edges.
        log.debug("parsing file", &.{Field.string("path", fe.rel_path)});
        parse_fn(fe.content, graph, fe.rel_path, log) catch {
            log.warn("file parse error", &.{Field.string("path", fe.rel_path)});
            result.files_errored += 1;
            continue;
        };

        // Patch the file node (visitor creates it at before_count with name="").
        if (before_count < graph.nodeCount()) {
            var file_node = &graph.nodes.items[before_count];
            file_node.name = fe.basename;
            file_node.file_path = fe.rel_path;
            file_node.content_hash = fe.content_hash;

            if (graph.nodeCount() == before_count + 1) {
                log.trace("file produced no nodes", &.{Field.string("path", fe.rel_path)});
            }

            try file_infos.append(allocator, .{
                .idx = before_count,
                .scope_end = graph.nodeCount(),
                .source = fe.content,
                .lang_support = fe.lang_support,
            });
        }

        result.files_indexed += 1;
    }

    // Compute metrics for function nodes in each file.
    for (file_infos.items) |fi| {
        source_scan.computeMetricsForNodes(graph, fi.source, fi.idx, fi.scope_end);
    }

    // Resolve inter-file imports and external (phantom) references.
    // Build rel_path-to-FileInfo-index map for cross-file lookup.
    log.debug("resolving cross-file edges", &.{Field.uint("file_count", file_infos.items.len)});
    var relpath_map = std.StringHashMapUnmanaged(usize){};
    defer relpath_map.deinit(allocator);
    try relpath_map.ensureTotalCapacity(allocator, @intCast(file_infos.items.len));
    for (file_infos.items, 0..) |fi, i| {
        const file_node = graph.nodes.items[fi.idx];
        const key = file_node.file_path orelse file_node.name;
        relpath_map.putAssumeCapacity(key, i);
    }

    // Import edges between project files.
    for (file_infos.items) |fi| {
        const file_id: NodeId = @enumFromInt(fi.idx);
        const file_node = graph.nodes.items[fi.idx];
        const importer_path = file_node.file_path;

        // Only scan import_decl nodes within this file's node range.
        for (graph.nodes.items[fi.idx..fi.scope_end]) |n| {
            if (n.kind != .import_decl) continue;
            if (n.parent_id == null or n.parent_id.? != file_id) continue;

            const import_path = n.signature orelse continue;

            // Resolve import path relative to the importing file's directory.
            const target_idx = blk: {
                if (importer_path) |ip| {
                    if (fi.lang_support.resolveImportPathFn) |resolve_fn| {
                        var buf: [512]u8 = undefined;
                        var ci: usize = 0;
                        while (ci < 8) : (ci += 1) {
                            const resolved = resolve_fn(&buf, ip, import_path, ci) orelse break;
                            if (relpath_map.get(resolved)) |idx| break :blk idx;
                        }
                    }
                }
                // Fallback: direct lookup.
                break :blk relpath_map.get(import_path);
            };

            if (target_idx) |tidx| {
                _ = try graph.addEdgeIfNew(.{ .source_id = file_id, .target_id = @enumFromInt(file_infos.items[tidx].idx), .edge_type = .imports });
            }
        }
    }

    // Phantom nodes for external references.
    log.debug("resolving phantom nodes", &.{Field.uint("file_count", file_infos.items.len)});
    var phantom = PhantomManager.init(allocator, graph);
    defer phantom.deinit();

    for (file_infos.items) |fi| {
        if (fi.lang_support.resolvePhantomsFn) |resolve_phantoms| {
            const bc_ptr: ?*const lang.BuildConfig = if (build_config) |*bc| bc else null;
            try resolve_phantoms(graph, fi.source, fi.idx, fi.scope_end, &phantom, bc_ptr, log);
        }
    }

    log.info("indexing complete", &.{
        Field.uint("files_indexed", result.files_indexed),
        Field.uint("files_skipped", result.files_skipped),
        Field.uint("files_errored", result.files_errored),
        Field.uint("nodes", graph.nodeCount()),
        Field.uint("edges", graph.edgeCount()),
    });

    try graph.freeze();
    return result;
}

fn computeContentHash(content: []const u8) [12]u8 {
    const h1 = std.hash.XxHash3.hash(0, content);
    const h2 = std.hash.XxHash3.hash(1, content);
    var result: [12]u8 = undefined;
    std.mem.writeInt(u64, result[0..8], h1, .little);
    std.mem.writeInt(u32, result[8..12], @truncate(h2), .little);
    return result;
}

fn isExcluded(rel_path: []const u8, exclude_paths: []const []const u8) bool {
    const bn = std.fs.path.basename(rel_path);
    for (exclude_paths) |exc| {
        if (std.mem.eql(u8, rel_path, exc)) return true;
        if (std.mem.eql(u8, bn, exc)) return true;
        if (pathHasComponent(rel_path, exc)) return true;
    }
    // Exclude directories declared by all registered languages.
    for (Registry.allLanguages()) |ls| {
        for (ls.excluded_dirs) |dir| {
            if (pathHasComponent(rel_path, dir)) return true;
        }
    }
    return false;
}

/// Returns true if any directory component of `path` equals `component`.
fn pathHasComponent(path: []const u8, component: []const u8) bool {
    var it = std.mem.splitScalar(u8, path, std.fs.path.sep);
    while (it.next()) |seg| {
        if (std.mem.eql(u8, seg, component)) return true;
    }
    return false;
}

/// Topological sort of file entries using Kahn's algorithm.
/// Files with no imports come first; files that import others come after
/// their dependencies. Handles cycles gracefully by appending remaining files.
fn topoSortFiles(allocator: std.mem.Allocator, entries: []FileEntry) !void {
    const n = entries.len;
    if (n <= 1) return;

    // Build rel_path-to-index map for directory-aware resolution.
    var relpath_map = std.StringHashMapUnmanaged(usize){};
    defer relpath_map.deinit(allocator);
    try relpath_map.ensureTotalCapacity(allocator, @intCast(n));
    for (entries, 0..) |fe, i| {
        relpath_map.putAssumeCapacity(fe.rel_path, i);
    }

    // Compute in-degrees and adjacency (importer -> importee).
    var in_degree = try allocator.alloc(usize, n);
    defer allocator.free(in_degree);
    @memset(in_degree, 0);

    // Adjacency: for each file, which files import it (reverse edges for Kahn's).
    var adj = try allocator.alloc(std.ArrayListUnmanaged(usize), n);
    defer {
        for (adj) |*a| a.deinit(allocator);
        allocator.free(adj);
    }
    for (adj) |*a| a.* = .{};

    var import_buf: [256]lang.ImportEntry = undefined;
    for (entries, 0..) |fe, i| {
        const extract_fn = fe.lang_support.extractImportsFn orelse continue;
        const count = extract_fn(fe.content, &import_buf);
        for (import_buf[0..count]) |ie| {
            if (ie.kind != .project_file) continue; // only project files affect topo order
            // Resolve import path relative to the importing file's directory.
            const dep_idx = blk: {
                if (fe.lang_support.resolveImportPathFn) |resolve_fn| {
                    var buf: [512]u8 = undefined;
                    var ci: usize = 0;
                    while (ci < 8) : (ci += 1) {
                        const resolved = resolve_fn(&buf, fe.rel_path, ie.path, ci) orelse break;
                        if (relpath_map.get(resolved)) |idx| break :blk idx;
                    }
                }
                // Fallback: direct lookup (handles same-directory and basename-only).
                break :blk relpath_map.get(ie.path);
            };

            if (dep_idx) |didx| {
                if (didx != i) {
                    // i imports didx, so didx must come before i.
                    // Edge: didx -> i in Kahn's sense.
                    try adj[didx].append(allocator, i);
                    in_degree[i] += 1;
                }
            }
        }
    }

    // BFS queue: start with all files that have no dependencies.
    var queue = try allocator.alloc(usize, n);
    defer allocator.free(queue);
    var q_head: usize = 0;
    var q_tail: usize = 0;
    for (in_degree, 0..) |deg, i| {
        if (deg == 0) {
            queue[q_tail] = i;
            q_tail += 1;
        }
    }

    // Kahn's BFS produces topological order.
    var order = try allocator.alloc(usize, n);
    defer allocator.free(order);
    var order_len: usize = 0;
    while (q_head < q_tail) {
        const u = queue[q_head];
        q_head += 1;
        order[order_len] = u;
        order_len += 1;
        for (adj[u].items) |v| {
            in_degree[v] -= 1;
            if (in_degree[v] == 0) {
                queue[q_tail] = v;
                q_tail += 1;
            }
        }
    }

    // Append any remaining files (cycles) in original order.
    if (order_len < n) {
        for (0..n) |i| {
            if (in_degree[i] > 0) {
                order[order_len] = i;
                order_len += 1;
            }
        }
    }

    // Reorder entries in-place using the computed order.
    var tmp = try allocator.alloc(FileEntry, n);
    defer allocator.free(tmp);
    for (order[0..n], 0..) |src_idx, dst| {
        tmp[dst] = entries[src_idx];
    }
    @memcpy(entries, tmp);
}

fn findExistingFileNode(graph: *const Graph, rel_path: []const u8) ?usize {
    for (graph.nodes.items, 0..) |n, i| {
        if (n.kind != .file) continue;
        // Match by file_path (rel_path) first, fall back to name (basename).
        const key = n.file_path orelse n.name;
        if (std.mem.eql(u8, key, rel_path)) return i;
    }
    return null;
}
