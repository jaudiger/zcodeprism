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
const worklist_mod = @import("../lsp/worklist.zig");
const LspWorklist = worklist_mod.LspWorklist;
const WorklistEntry = worklist_mod.WorklistEntry;

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
    budget_bytes: ?u64 = null,
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
    content_hash: types.ContentHash,
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
pub fn indexDirectory(
    allocator: std.mem.Allocator,
    io: std.Io,
    project_root: []const u8,
    graph: *Graph,
    out_worklist: ?*LspWorklist,
    options: IndexOptions,
) !IndexResult {
    var result = IndexResult{};
    const log = options.logger.withScope("indexer");

    var file_entries: std.ArrayList(FileEntry) = .empty;
    defer file_entries.deinit(allocator);

    try discoverFiles(allocator, io, project_root, &file_entries, options, &result, log);

    log.info("discovered files", &.{
        Field.uint("count", file_entries.items.len),
        Field.string("root", project_root),
    });

    if (file_entries.items.len == 0) {
        log.debug("no files to index", &.{});
        _ = try graph.freeze(allocator);
        return result;
    }

    // Pre-allocate graph capacity: rough estimate of ~30 nodes and ~20 edges per file.
    // Each file contributes 2 owned buffers (content + rel_path).
    {
        const file_count = file_entries.items.len;
        const est_nodes: u32 = @intCast(file_count * 30);
        const est_edges: u32 = @intCast(file_count * 20);
        const est_bufs: u32 = @intCast(graph.owned_buffers.items.len + file_count * 2);
        try graph.ensureCapacity(allocator, est_nodes, est_edges, est_bufs);
    }

    // Topological sort: imported files are parsed before their importers.
    try topoSortFiles(allocator, file_entries.items);

    // Load build configs for each language whose build files exist on disk.
    const all_langs = registry_mod.Registry.allLanguages();
    var build_configs: [registry_mod.language_count]?lang.BuildConfig = .{null} ** registry_mod.language_count;
    defer for (&build_configs) |*slot| {
        if (slot.*) |*bc| bc.deinit(allocator);
    };
    for (all_langs, 0..) |ls, lang_idx| {
        const parse_config_fn = ls.parseBuildConfigFn orelse continue;
        if (!hasBuildFile(io, project_root, ls.build_files)) continue;
        build_configs[lang_idx] = try parse_config_fn(allocator, io, project_root, log);
    }

    // Transfer paths and content to graph ownership. owned_buffers capacity
    // was reserved by ensureCapacity above; registration is infallible.
    for (file_entries.items) |fe| {
        graph.owned_buffers.appendAssumeCapacity(graph_mod.OwnedBuffer.fromSlice(u8, fe.rel_path));
        graph.owned_buffers.appendAssumeCapacity(graph_mod.OwnedBuffer.fromSlice(u8, fe.content));
    }

    var dir_map = std.StringHashMapUnmanaged(NodeId){};
    defer dir_map.deinit(allocator);
    const root_dir_id = try buildDirectoryNodes(allocator, graph, file_entries.items, options.incremental, &dir_map);

    var module_file_map = std.StringHashMapUnmanaged(NodeId){};
    defer module_file_map.deinit(allocator);
    for (build_configs) |maybe_bc| {
        const bc = maybe_bc orelse continue;
        try buildModuleNodes(allocator, graph, root_dir_id, bc, &module_file_map);
    }

    var file_infos = std.ArrayList(FileInfo).empty;
    defer file_infos.deinit(allocator);

    try parseFiles(allocator, io, graph, file_entries.items, &dir_map, root_dir_id, options.incremental, &file_infos, &result, log);

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

    for (all_langs, 0..) |ls, lang_idx| {
        const bc = build_configs[lang_idx] orelse continue;
        try buildPhantomDependencies(allocator, graph, bc, ls.language, &phantom);
    }

    try graph_index.buildImportTargets(allocator, graph.edges.items);

    var local_wl = LspWorklist{};
    defer local_wl.deinit(allocator);
    const wl: *LspWorklist = out_worklist orelse &local_wl;

    try resolvePhantomNodes(allocator, graph, file_infos.items, &phantom, &graph_index, all_langs, &build_configs, log);

    // Transfer phantom usage sites into the phantom_hovers list, one entry per phantom NodeId.
    {
        var sit = phantom.usage_sites.iterator();
        while (sit.next()) |kv| {
            try wl.appendPhantomHover(allocator, WorklistEntry{
                .source_node_id = kv.key_ptr.*,
                .file_path = kv.value_ptr.file_path,
                .line = kv.value_ptr.line,
                .col = kv.value_ptr.col,
                .query_kind = .hover,
                .hint_name = kv.value_ptr.hint_name,
            });
        }
    }

    log.debug("building edges", &.{Field.uint("file_count", file_infos.items.len)});
    buildCrossFileEdges(allocator, io, graph, file_infos.items, &graph_index, &phantom, wl, log);

    try resolveCrossLanguageEdges(allocator, graph, log);

    if (wl.count() > 0) {
        log.info("worklist entries collected", &.{Field.uint("count", wl.count())});
    }

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

    _ = try graph.freeze(allocator);

    try enrichment.enrichPostFreeze(allocator, graph, .{ .logger = log });

    // Append hover entries for Zig function nodes that still need error set inference.
    // Runs after enrichPreFreeze and enrichPostFreeze so AST-extracted error sets are
    // already present; functions with inferred_errors are skipped.
    for (graph.nodes.items) |n| {
        if (n.kind != .function) continue;
        if (n.language != .zig) continue;
        if (n.lang_meta == .zig and n.lang_meta.zig.inferred_errors != null) continue;
        const file_path = n.file_path orelse continue;
        const line_start = n.line_start orelse continue;
        const lsp_line: u32 = if (line_start > 0) line_start - 1 else 0;
        try wl.append(allocator, WorklistEntry{
            .source_node_id = n.id,
            .file_path = file_path,
            .line = lsp_line,
            .col = n.col_start orelse 0,
            .query_kind = .hover,
            .hint_name = n.name,
        });
    }

    return result;
}

/// Walk `project_root`, read every supported source file, and append a
/// `FileEntry` per file to `entries`. Updates `result` error counters.
fn discoverFiles(
    allocator: std.mem.Allocator,
    io: std.Io,
    project_root: []const u8,
    entries: *std.ArrayList(FileEntry),
    options: IndexOptions,
    result: *IndexResult,
    log: Logger,
) !void {
    var dir = try std.Io.Dir.openDirAbsolute(io, project_root, .{ .iterate = true });
    defer dir.close(io);

    var walker = try dir.walk(allocator);
    defer walker.deinit();

    var cumulative_bytes: u64 = 0;

    while (try walker.next(io)) |entry| {
        if (entry.kind != .file) continue;
        const ext = std.fs.path.extension(entry.path);
        const file_lang = Registry.getByExtension(ext) orelse continue;
        if (isExcluded(entry.path, options.exclude_paths)) continue;

        const file = dir.openFile(io, entry.path, .{}) catch {
            log.warn("file read error", &.{Field.string("path", entry.path)});
            result.files_errored += 1;
            continue;
        };
        defer file.close(io);
        var rbuf: [4096]u8 = undefined;
        var freader = file.reader(io, &rbuf);
        const content = freader.interface.allocRemaining(allocator, .limited(max_source_bytes)) catch |err| {
            const reason = if (err == error.StreamTooLong) "exceeds 10 MiB read limit" else @errorName(err);
            log.warn("skipping file", &.{
                Field.string("path", entry.path),
                Field.string("reason", reason),
            });
            result.files_errored += 1;
            continue;
        };
        errdefer allocator.free(content);

        cumulative_bytes += content.len;
        if (options.budget_bytes) |budget| {
            if (cumulative_bytes > budget) {
                log.warn("memory budget exceeded, stopping discovery", &.{});
                allocator.free(content);
                break;
            }
        }

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
    var sorted_dirs = std.ArrayList([]const u8).empty;
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
    io: std.Io,
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
        fe.lang_support.parseFn(allocator, io, fe.content, graph, fe.rel_path, log) catch {
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

/// Call each language's resolvePhantomsFn for every file, passing
/// the build config that matches the file's language.
fn resolvePhantomNodes(
    allocator: std.mem.Allocator,
    graph: *Graph,
    infos: []const FileInfo,
    phantom: *PhantomManager,
    graph_index: *GraphIndex,
    all_langs: []const *const lang_support.LanguageSupport,
    build_configs: []const ?lang.BuildConfig,
    log: Logger,
) !void {
    for (infos) |fi| {
        const resolve_fn = fi.lang_support.resolvePhantomsFn orelse continue;
        const bc_ptr: ?*const lang.BuildConfig = for (all_langs, 0..) |ls, i| {
            if (ls == fi.lang_support) {
                if (build_configs[i]) |*bc| break bc;
                break null;
            }
        } else null;
        try resolve_fn(allocator, graph, fi.source, fi.node_idx, fi.scope_end, phantom, graph_index, bc_ptr, log);
    }
}

/// Call each language's buildEdgesFn for every file. Unresolved references
/// are appended to `wl`.
fn buildCrossFileEdges(
    allocator: std.mem.Allocator,
    io: std.Io,
    graph: *Graph,
    infos: []const FileInfo,
    graph_index: *GraphIndex,
    phantom: *PhantomManager,
    wl: *LspWorklist,
    log: Logger,
) void {
    var node_type_map = lang_support.NodeTypeMap{};
    defer node_type_map.deinit(allocator);

    for (infos) |fi| {
        const build_edges = fi.lang_support.buildEdgesFn orelse continue;
        const file_node = graph.nodes.items[fi.node_idx];
        build_edges(allocator, io, fi.source, graph, fi.node_idx, fi.scope_end, file_node.file_path, graph_index, phantom, &node_type_map, wl, log) catch |err| {
            log.warn("edge building failed", &.{
                Field.string("path", file_node.file_path orelse "?"),
                Field.string("error", @errorName(err)),
            });
        };
    }
}

/// Match FFI prototypes to definitions across languages by convention and symbol name.
fn resolveCrossLanguageEdges(
    allocator: std.mem.Allocator,
    graph: *Graph,
    log: Logger,
) !void {
    const nodes = graph.nodes.items;

    // Index: (name, convention) -> list of definition node indices.
    const FfiKey = struct { name: []const u8, convention: []const u8 };
    var defn_map = std.ArrayHashMapUnmanaged(FfiKey, std.ArrayList(usize), struct {
        pub fn hash(_: @This(), key: FfiKey) u32 {
            var h = std.hash.Fnv1a_32.init();
            h.update(key.name);
            for (key.convention) |c| h.update(&.{std.ascii.toLower(c)});
            return h.final();
        }
        pub fn eql(_: @This(), a: FfiKey, b: FfiKey, _: usize) bool {
            return std.mem.eql(u8, a.name, b.name) and std.ascii.eqlIgnoreCase(a.convention, b.convention);
        }
    }, true){};
    defer {
        for (defn_map.values()) |*list| list.deinit(allocator);
        defn_map.deinit(allocator);
    }

    // Collect all FFI definitions (functions with a body).
    for (nodes, 0..) |n, idx| {
        if (n.kind != .function) continue;
        if (n.language == null) continue;
        const conv = n.lang_meta.ffiConvention() orelse continue;
        if (!isFfiDefinition(n)) continue;
        const key = FfiKey{ .name = n.name, .convention = conv };
        const gop = try defn_map.getOrPut(allocator, key);
        if (!gop.found_existing) gop.value_ptr.* = .empty;
        try gop.value_ptr.append(allocator, idx);
    }

    // Match prototypes against definitions in other languages.
    var edges_added: usize = 0;
    for (nodes) |proto| {
        if (proto.kind != .function) continue;
        const proto_lang = proto.language orelse continue;
        const conv = proto.lang_meta.ffiConvention() orelse continue;
        if (!isFfiPrototype(proto)) continue;

        const key = FfiKey{ .name = proto.name, .convention = conv };
        const defns = defn_map.get(key) orelse continue;
        for (defns.items) |didx| {
            const defn = nodes[didx];
            if (defn.language.? == proto_lang) continue;
            _ = try graph.addEdgeIfNew(allocator, .{
                .source_id = proto.id,
                .target_id = defn.id,
                .edge_type = .calls,
                .source = .workspace,
            });
            edges_added += 1;
        }
    }

    if (edges_added > 0) {
        log.info("cross-language FFI edges", &.{Field.uint("count", edges_added)});
    }
}

fn isFfiPrototype(n: Node) bool {
    const s = n.line_start orelse return false;
    const e = n.line_end orelse return false;
    return s == e;
}

fn isFfiDefinition(n: Node) bool {
    const s = n.line_start orelse return false;
    const e = n.line_end orelse return false;
    return s < e;
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

fn computeContentHash(content: []const u8) types.ContentHash {
    var hasher = std.crypto.hash.Blake3.init(.{});
    hasher.update(content);
    var result: types.ContentHash = undefined;
    hasher.final(&result);
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
        const count = extract_fn(fe.content, fe.lang_support.grammarFn(), &import_buf);
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
    for (adj) |*a| a.* = .empty;

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

/// Returns true when at least one of the given build file names exists
/// directly under `project_root`.
fn hasBuildFile(io: std.Io, project_root: []const u8, build_files: []const []const u8) bool {
    const dir = std.Io.Dir.openDirAbsolute(io, project_root, .{}) catch return false;
    // dir is a copy of the handle struct, not a pointer; closing is fine.
    var d = dir;
    defer d.close(io);
    for (build_files) |name| {
        d.access(io, name, .{}) catch continue;
        return true;
    }
    return false;
}

fn findExistingFileNode(graph: *const Graph, rel_path: []const u8) ?usize {
    for (graph.nodes.items, 0..) |n, i| {
        if (n.kind != .file) continue;
        const key = n.file_path orelse n.name;
        if (std.mem.eql(u8, key, rel_path)) return i;
    }
    return null;
}
