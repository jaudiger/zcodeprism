const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const logging = @import("../logging.zig");
const node_mod = @import("../core/node.zig");
const edge_mod = @import("../core/edge.zig");
const types = @import("../core/types.zig");
const graph_index_mod = @import("../core/graph_index.zig");
const lang = @import("../languages/language.zig");
const lang_support = @import("../languages/language_support.zig");
const registry_mod = @import("../languages/registry.zig");
const phantom_mod = @import("../core/phantom.zig");
const metrics_mod = @import("../core/metrics.zig");
const source_scan = @import("source_scan.zig");
const enrichment = @import("../enrichment/enrichment.zig");

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
const GraphIndex = graph_index_mod.GraphIndex;
const KindIndex = @import("../core/kind_index.zig").KindIndex;
const Metrics = metrics_mod.Metrics;

/// Maximum source file size the indexer will read into memory.
/// Files that exceed this limit are skipped and counted as errors.
pub const max_source_bytes: usize = 10 * 1024 * 1024;

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
};

/// Tracks a parsed file's node index and source for post-processing.
const FileInfo = struct {
    /// Index of the file node in graph.nodes.
    node_idx: usize,
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

    var file_entries = std.ArrayList(FileEntry){};
    defer file_entries.deinit(allocator);

    try discoverFiles(allocator, project_root, &file_entries, options, &result, log);

    log.info("discovered files", &.{
        Field.uint("count", file_entries.items.len),
        Field.string("root", project_root),
    });

    if (file_entries.items.len == 0) {
        log.debug("no files to index", &.{});
        try graph.freeze(allocator);
        return result;
    }

    // Pre-allocate graph capacity: rough estimate of ~30 nodes and ~20 edges per file.
    // Each file contributes 2 owned buffers (content + rel_path).
    {
        const file_count = file_entries.items.len;
        const est_nodes: u32 = @intCast(file_count * 30);
        const est_edges: u32 = @intCast(file_count * 20);
        const est_bufs: u32 = @intCast(graph.owned_buffers.items.len + file_count * 2);
        try graph.nodes.ensureTotalCapacity(allocator, est_nodes);
        try graph.edges.ensureTotalCapacity(allocator, est_edges);
        try graph.edge_index.ensureTotalCapacity(allocator, est_edges);
        try graph.owned_buffers.ensureTotalCapacity(allocator, est_bufs);
    }

    // Topological sort: imported files are parsed before their importers.
    try topoSortFiles(allocator, file_entries.items);

    // Load build config from the first registered language that provides one.
    var build_config: ?lang.BuildConfig = null;
    var build_config_lang: Language = undefined;
    defer if (build_config) |*bc| bc.deinit(allocator);
    for (registry_mod.Registry.allLanguages()) |ls| {
        if (ls.parseBuildConfigFn) |parse_config_fn| {
            build_config = parse_config_fn(allocator, project_root, log) catch null;
            if (build_config != null) {
                build_config_lang = ls.language;
                break;
            }
        }
    }

    // Transfer all paths and content to graph ownership in one batch.
    // After this loop the graph is the sole owner; no separate cleanup
    // defer is needed and no per-entry ownership flag is required.
    for (file_entries.items) |fe| {
        try graph.addOwnedBuffer(allocator, fe.rel_path);
        try graph.addOwnedBuffer(allocator, fe.content);
    }

    var dir_map = std.StringHashMapUnmanaged(NodeId){};
    defer dir_map.deinit(allocator);
    const root_dir_id = try buildDirectoryNodes(allocator, graph, file_entries.items, options.incremental, &dir_map);

    var module_file_map = std.StringHashMapUnmanaged(NodeId){};
    defer module_file_map.deinit(allocator);
    if (build_config) |bc| {
        try buildModuleNodes(allocator, graph, root_dir_id, bc, &module_file_map);
    }

    var file_infos = std.ArrayList(FileInfo){};
    defer file_infos.deinit(allocator);

    try parseFiles(allocator, graph, file_entries.items, &dir_map, root_dir_id, options.incremental, &file_infos, &result, log);

    // Build graph indexes once for the complete graph.
    var graph_index = try GraphIndex.build(allocator, graph.nodes.items);
    defer graph_index.deinit(allocator);

    // Build rel_path-to-FileInfo-index map for cross-file lookup.
    var relpath_map = std.StringHashMapUnmanaged(usize){};
    defer relpath_map.deinit(allocator);
    try relpath_map.ensureTotalCapacity(allocator, @intCast(file_infos.items.len));
    for (file_infos.items, 0..) |fi, i| {
        const file_node = graph.nodes.items[fi.node_idx];
        const key = file_node.file_path orelse file_node.name;
        relpath_map.putAssumeCapacity(key, i);
    }

    log.debug("resolving cross-file edges", &.{Field.uint("file_count", file_infos.items.len)});
    try resolveImportEdges(allocator, graph, file_infos.items, &relpath_map);

    log.debug("resolving phantom nodes", &.{Field.uint("file_count", file_infos.items.len)});
    var phantom = PhantomManager.init(graph);
    defer phantom.deinit(allocator);

    if (build_config) |bc| {
        try buildPhantomDependencies(allocator, graph, bc, build_config_lang, &phantom);
    }

    try graph_index.buildImportTargets(allocator, graph.edges.items);

    try resolvePhantomNodes(allocator, graph, file_infos.items, &phantom, &graph_index, build_config, log);

    log.debug("building edges", &.{Field.uint("file_count", file_infos.items.len)});
    buildCrossFileEdges(allocator, graph, file_infos.items, &graph_index, &phantom, log);

    if (module_file_map.count() > 0) {
        try buildModuleContainsEdges(allocator, graph, file_infos.items, &module_file_map);
    }

    // Build FileSource slice for the enrichment pipeline.
    const file_sources = try buildFileSources(allocator, file_infos.items);
    defer allocator.free(file_sources);

    try enrichment.enrichPreFreeze(allocator, graph, file_sources, .{ .logger = log });

    log.info("indexing complete", &.{
        Field.uint("files_indexed", result.files_indexed),
        Field.uint("files_skipped", result.files_skipped),
        Field.uint("files_errored", result.files_errored),
        Field.uint("nodes", graph.nodeCount()),
        Field.uint("edges", graph.edgeCount()),
    });

    try graph.freeze(allocator);

    try enrichment.enrichPostFreeze(allocator, graph, .{ .logger = log });

    return result;
}

/// Walk `project_root`, read every supported source file, and append a
/// `FileEntry` per file to `entries`. Updates `result` error counters.
fn discoverFiles(
    allocator: std.mem.Allocator,
    project_root: []const u8,
    entries: *std.ArrayList(FileEntry),
    options: IndexOptions,
    result: *IndexResult,
    log: Logger,
) !void {
    var dir = try std.fs.openDirAbsolute(project_root, .{ .iterate = true });
    defer dir.close();

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
        const content = file.readToEndAlloc(allocator, max_source_bytes) catch |err| {
            const reason = if (err == error.StreamTooLong) "exceeds 10 MiB read limit" else @errorName(err);
            log.warn("skipping file", &.{
                Field.string("path", entry.path),
                Field.string("reason", reason),
            });
            result.files_errored += 1;
            continue;
        };
        errdefer allocator.free(content);

        const hash = computeContentHash(content);
        const rel_path = try allocator.dupe(u8, entry.path);
        errdefer allocator.free(rel_path);
        const basename = std.fs.path.basename(rel_path);

        try entries.append(allocator, .{
            .rel_path = rel_path,
            .basename = basename,
            .content = content,
            .content_hash = hash,
            .lang_support = file_lang,
        });
    }
}

/// Create directory nodes for every unique directory component of the
/// discovered files, returning the root directory NodeId. Populates
/// `dir_map` with rel_path -> NodeId entries for all subdirectories.
fn buildDirectoryNodes(
    allocator: std.mem.Allocator,
    graph: *Graph,
    entries: []const FileEntry,
    incremental: bool,
    dir_map: *std.StringHashMapUnmanaged(NodeId),
) !NodeId {
    var dir_set = std.StringHashMapUnmanaged(void){};
    defer dir_set.deinit(allocator);

    for (entries) |fe| {
        var path: []const u8 = fe.rel_path;
        while (std.fs.path.dirname(path)) |parent_dir| {
            const gop = try dir_set.getOrPut(allocator, parent_dir);
            if (gop.found_existing) break;
            path = parent_dir;
        }
    }

    // Sort unique paths so parents are created before children.
    var sorted_dirs = std.ArrayList([]const u8){};
    defer sorted_dirs.deinit(allocator);
    try sorted_dirs.ensureTotalCapacity(allocator, @intCast(dir_set.count()));
    {
        var it = dir_set.keyIterator();
        while (it.next()) |key| {
            sorted_dirs.appendAssumeCapacity(key.*);
        }
    }
    std.mem.sort([]const u8, sorted_dirs.items, {}, struct {
        fn lessThan(_: void, a: []const u8, b: []const u8) bool {
            return std.mem.order(u8, a, b) == .lt;
        }
    }.lessThan);

    // For incremental runs, reuse existing directory nodes.
    var root_dir_id: NodeId = undefined;
    var root_found = false;
    if (incremental) {
        var kind_idx = try KindIndex.build(allocator, graph.nodes.items);
        defer kind_idx.deinit(allocator);
        for (kind_idx.findByKind(.directory)) |i| {
            const n = graph.nodes.items[i];
            const nid: NodeId = @enumFromInt(i);
            if (n.file_path) |fp| {
                try dir_map.put(allocator, fp, nid);
            } else {
                root_dir_id = nid;
                root_found = true;
            }
        }
    }

    if (!root_found) {
        root_dir_id = try graph.addNode(allocator, .{
            .id = .root,
            .name = "",
            .kind = .directory,
            .visibility = .public,
        });
    }

    try dir_map.ensureTotalCapacity(allocator, dir_map.count() + @as(u32, @intCast(sorted_dirs.items.len)));

    for (sorted_dirs.items) |dir_path| {
        if (dir_map.contains(dir_path)) continue;
        const parent_dir = std.fs.path.dirname(dir_path);
        const parent_id = if (parent_dir) |pd|
            dir_map.get(pd) orelse root_dir_id
        else
            root_dir_id;
        const dir_id = try graph.addNode(allocator, .{
            .id = .root,
            .name = std.fs.path.basename(dir_path),
            .kind = .directory,
            .file_path = dir_path,
            .visibility = .public,
            .parent_id = parent_id,
        });
        dir_map.putAssumeCapacity(dir_path, dir_id);
    }

    return root_dir_id;
}

/// Create module nodes from a build config and populate `module_file_map`
/// with root_source_file -> module NodeId for later contains-edge creation.
fn buildModuleNodes(
    allocator: std.mem.Allocator,
    graph: *Graph,
    root_dir_id: NodeId,
    bc: lang.BuildConfig,
    module_file_map: *std.StringHashMapUnmanaged(NodeId),
) !void {
    const modules = bc.build_modules orelse return;
    for (modules) |m| {
        const mod_name = try allocator.dupe(u8, m.name);
        // Transfer ownership to graph immediately.
        try graph.addOwnedBuffer(allocator, mod_name);

        const mod_id = try graph.addNode(allocator, .{
            .id = .root,
            .name = mod_name,
            .kind = .module,
            .visibility = .public,
            .parent_id = root_dir_id,
        });
        if (m.root_source_file) |rsf| {
            try module_file_map.put(allocator, rsf, mod_id);
        }
    }
}

/// Parse each file through the visitor in dependency order. For each
/// successfully parsed file, appends a FileInfo to `infos` and increments
/// result counters. Content and path ownership has already been transferred
/// to the graph before this function is called.
fn parseFiles(
    allocator: std.mem.Allocator,
    graph: *Graph,
    entries: []const FileEntry,
    dir_map: *const std.StringHashMapUnmanaged(NodeId),
    root_dir_id: NodeId,
    incremental: bool,
    infos: *std.ArrayList(FileInfo),
    result: *IndexResult,
    log: Logger,
) !void {
    for (entries) |fe| {
        if (incremental) {
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

        const before_count = graph.nodeCount();

        log.debug("parsing file", &.{Field.string("path", fe.rel_path)});
        fe.lang_support.parseFn(allocator, fe.content, graph, fe.rel_path, log) catch {
            log.warn("file parse error", &.{Field.string("path", fe.rel_path)});
            result.files_errored += 1;
            continue;
        };

        if (before_count < graph.nodeCount()) {
            var file_node = &graph.nodes.items[before_count];
            file_node.name = fe.basename;
            file_node.file_path = fe.rel_path;
            file_node.content_hash = fe.content_hash;
            const file_dir = std.fs.path.dirname(fe.rel_path);
            file_node.parent_id = if (file_dir) |fd| dir_map.get(fd) orelse root_dir_id else root_dir_id;

            // Propagate file_path to all child nodes the visitor created.
            for (graph.nodes.items[before_count + 1 .. graph.nodeCount()]) |*child| {
                if (child.file_path == null) child.file_path = fe.rel_path;
            }

            if (graph.nodeCount() == before_count + 1) {
                log.trace("file produced no nodes", &.{Field.string("path", fe.rel_path)});
            }

            try infos.append(allocator, .{
                .node_idx = before_count,
                .scope_end = graph.nodeCount(),
                .source = fe.content,
                .lang_support = fe.lang_support,
            });
        }

        result.files_indexed += 1;
    }
}

/// Add import edges between project files using the relpath_map for lookup.
fn resolveImportEdges(
    allocator: std.mem.Allocator,
    graph: *Graph,
    infos: []const FileInfo,
    relpath_map: *const std.StringHashMapUnmanaged(usize),
) !void {
    for (infos) |fi| {
        const file_id: NodeId = @enumFromInt(fi.node_idx);
        const file_node = graph.nodes.items[fi.node_idx];
        const importer_path = file_node.file_path;

        for (graph.nodes.items[fi.node_idx..fi.scope_end]) |n| {
            if (n.kind != .import_decl) continue;
            if (n.parent_id == null or n.parent_id.? != file_id) continue;

            const import_path = n.signature orelse continue;
            const target_idx = resolveToEntryIdx(relpath_map, importer_path, import_path, fi.lang_support.resolveImportPathFn);

            if (target_idx) |tidx| {
                _ = try graph.addEdgeIfNew(allocator, .{
                    .source_id = file_id,
                    .target_id = @enumFromInt(infos[tidx].node_idx),
                    .edge_type = .imports,
                });
            }
        }
    }
}

/// Create phantom module nodes for build dependencies. Duplicate the
/// version string into graph ownership so the phantom node keeps a valid
/// reference after BuildConfig.deinit frees its copy.
fn buildPhantomDependencies(
    allocator: std.mem.Allocator,
    graph: *Graph,
    bc: lang.BuildConfig,
    bc_lang: Language,
    phantom: *PhantomManager,
) !void {
    const deps = bc.build_dependencies orelse return;
    for (deps) |dep| {
        const owned_version: ?[]const u8 = if (dep.version) |v| blk: {
            const dup = try allocator.dupe(u8, v);
            try graph.addOwnedBuffer(allocator, dup);
            break :blk dup;
        } else null;
        _ = try phantom.getOrCreate(allocator, dep.name, bc_lang, .{ .dependency = .{ .version = owned_version } });
    }
}

/// Call each language's resolvePhantomsFn for every file.
fn resolvePhantomNodes(
    allocator: std.mem.Allocator,
    graph: *Graph,
    infos: []const FileInfo,
    phantom: *PhantomManager,
    graph_index: *GraphIndex,
    build_config: ?lang.BuildConfig,
    log: Logger,
) !void {
    for (infos) |fi| {
        const resolve_fn = fi.lang_support.resolvePhantomsFn orelse continue;
        const bc_ptr: ?*const lang.BuildConfig = if (build_config) |*bc| bc else null;
        try resolve_fn(allocator, graph, fi.source, fi.node_idx, fi.scope_end, phantom, graph_index, bc_ptr, log);
    }
}

/// Call each language's buildEdgesFn for every file.
fn buildCrossFileEdges(
    allocator: std.mem.Allocator,
    graph: *Graph,
    infos: []const FileInfo,
    graph_index: *GraphIndex,
    phantom: *PhantomManager,
    log: Logger,
) void {
    for (infos) |fi| {
        const build_edges = fi.lang_support.buildEdgesFn orelse continue;
        const file_node = graph.nodes.items[fi.node_idx];
        build_edges(allocator, fi.source, graph, fi.node_idx, fi.scope_end, file_node.file_path, graph_index, phantom, log) catch |err| {
            log.warn("edge building failed", &.{
                Field.string("path", file_node.file_path orelse "?"),
                Field.string("error", @errorName(err)),
            });
        };
    }
}

/// Create contains edges from module nodes to their root source files.
fn buildModuleContainsEdges(
    allocator: std.mem.Allocator,
    graph: *Graph,
    infos: []const FileInfo,
    module_file_map: *const std.StringHashMapUnmanaged(NodeId),
) !void {
    for (infos) |fi| {
        const file_node = graph.nodes.items[fi.node_idx];
        const fp = file_node.file_path orelse continue;
        const mod_id = module_file_map.get(fp) orelse continue;
        const file_id: NodeId = @enumFromInt(fi.node_idx);
        _ = try graph.addEdgeIfNew(allocator, .{
            .source_id = mod_id,
            .target_id = file_id,
            .edge_type = .contains,
            .source = .workspace,
        });
    }
}

/// Map FileInfo slice to enrichment FileSource slice for the enrichment pipeline.
fn buildFileSources(allocator: std.mem.Allocator, infos: []const FileInfo) ![]const enrichment.FileSource {
    const result = try allocator.alloc(enrichment.FileSource, infos.len);
    for (infos, 0..) |fi, i| {
        result[i] = .{ .node_idx = fi.node_idx, .scope_end = fi.scope_end, .source = fi.source };
    }
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

/// Resolves an import path to an entry index using the language-specific path
/// resolver and, if that fails, a direct map lookup.
fn resolveToEntryIdx(
    relpath_map: *const std.StringHashMapUnmanaged(usize),
    importer_path: ?[]const u8,
    import_path: []const u8,
    resolve_fn: ?lang.ResolveImportPathFn,
) ?usize {
    if (importer_path) |ip| {
        if (resolve_fn) |rfn| {
            var buf: [std.fs.max_path_bytes]u8 = undefined;
            var ci: usize = 0;
            while (ci < 8) : (ci += 1) {
                const resolved = rfn(&buf, ip, import_path, ci) orelse break;
                if (relpath_map.get(resolved)) |idx| return idx;
            }
        }
    }
    return relpath_map.get(import_path);
}

/// Populates in_degree[] and adj[] from the import relationships in entries.
/// adj[dep_idx] holds all entry indices that import entry dep_idx, so Kahn's
/// BFS can process importees before their importers.
fn buildDepGraph(
    allocator: std.mem.Allocator,
    entries: []const FileEntry,
    relpath_map: *const std.StringHashMapUnmanaged(usize),
    in_degree: []usize,
    adj: []std.ArrayList(usize),
) !void {
    var import_buf: [256]lang.ImportEntry = undefined;
    for (entries, 0..) |fe, i| {
        const extract_fn = fe.lang_support.extractImportsFn orelse continue;
        const count = extract_fn(fe.content, &import_buf);
        for (import_buf[0..count]) |ie| {
            if (ie.kind != .project_file) continue;
            const dep_idx = resolveToEntryIdx(relpath_map, fe.rel_path, ie.path, fe.lang_support.resolveImportPathFn) orelse continue;
            if (dep_idx == i) continue;
            try adj[dep_idx].append(allocator, i);
            in_degree[i] += 1;
        }
    }
}

/// Kahn's BFS topological sort. Returns the number of entries written to order[].
/// Cycle nodes are appended in original index order, so the return value always equals n.
fn kahnTopologicalOrder(
    n: usize,
    in_degree: []usize,
    adj: []const std.ArrayList(usize),
    queue: []usize,
    order: []usize,
) usize {
    var q_head: usize = 0;
    var q_tail: usize = 0;
    for (in_degree, 0..) |deg, i| {
        if (deg == 0) {
            queue[q_tail] = i;
            q_tail += 1;
        }
    }
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
    if (order_len < n) {
        for (0..n) |i| {
            if (in_degree[i] > 0) {
                order[order_len] = i;
                order_len += 1;
            }
        }
    }
    return order_len;
}

/// Topological sort of file entries using Kahn's algorithm.
/// Files with no imports come first; files that import others come after
/// their dependencies. Handles cycles gracefully by appending remaining files.
fn topoSortFiles(allocator: std.mem.Allocator, entries: []FileEntry) !void {
    const n = entries.len;
    if (n <= 1) return;

    var relpath_map = std.StringHashMapUnmanaged(usize){};
    defer relpath_map.deinit(allocator);
    try relpath_map.ensureTotalCapacity(allocator, @intCast(n));
    for (entries, 0..) |fe, i| relpath_map.putAssumeCapacity(fe.rel_path, i);

    const in_degree = try allocator.alloc(usize, n);
    defer allocator.free(in_degree);
    @memset(in_degree, 0);

    const adj = try allocator.alloc(std.ArrayList(usize), n);
    defer {
        for (adj) |*a| a.deinit(allocator);
        allocator.free(adj);
    }
    for (adj) |*a| a.* = .{};

    try buildDepGraph(allocator, entries, &relpath_map, in_degree, adj);

    const queue = try allocator.alloc(usize, n);
    defer allocator.free(queue);
    var order = try allocator.alloc(usize, n);
    defer allocator.free(order);
    _ = kahnTopologicalOrder(n, in_degree, adj, queue, order);

    var tmp = try allocator.alloc(FileEntry, n);
    defer allocator.free(tmp);
    for (order[0..n], 0..) |src_idx, dst| tmp[dst] = entries[src_idx];
    @memcpy(entries, tmp);
}

fn findExistingFileNode(graph: *const Graph, rel_path: []const u8) ?usize {
    for (graph.nodes.items, 0..) |n, i| {
        if (n.kind != .file) continue;
        const key = n.file_path orelse n.name;
        if (std.mem.eql(u8, key, rel_path)) return i;
    }
    return null;
}
