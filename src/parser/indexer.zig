const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const logging = @import("../logging.zig");
const node_mod = @import("../core/node.zig");
const edge_mod = @import("../core/edge.zig");
const types = @import("../core/types.zig");
const lang = @import("../languages/language.zig");
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

/// Options controlling directory indexation.
pub const IndexOptions = struct {
    exclude_paths: []const []const u8 = &.{},
    incremental: bool = false,
    logger: Logger = Logger.noop,
};

/// Result of indexing a directory.
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
    import_count: usize = 0,
    consumed: bool = false,
};

/// Tracks a parsed file's node index and source for post-processing.
const FileInfo = struct {
    idx: usize,
    scope_end: usize,
    source: []const u8,
};

/// Index all Zig source files in a directory, populating the graph with
/// file nodes, declaration nodes, edges (imports, calls, uses_type),
/// phantom nodes for external references, and computed metrics.
pub fn indexDirectory(
    allocator: std.mem.Allocator,
    project_root: []const u8,
    graph: *Graph,
    options: IndexOptions,
) !IndexResult {
    var result = IndexResult{};
    const log = options.logger.withScope("indexer");

    // Discover .zig files in the project directory.
    var dir = try std.fs.openDirAbsolute(project_root, .{ .iterate = true });
    defer dir.close();

    var file_entries = std.ArrayListUnmanaged(FileEntry){};
    defer file_entries.deinit(allocator);

    {
        var walker = try dir.walk(allocator);
        defer walker.deinit();

        while (try walker.next()) |entry| {
            if (entry.kind != .file) continue;
            if (!std.mem.endsWith(u8, entry.path, ".zig")) continue;
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
            });
        }
    }

    // Cleanup non-consumed entries (skipped or error). This defer runs before
    // the deinit defers above (reverse declaration order), so items are valid.
    defer {
        for (file_entries.items) |fe| {
            if (!fe.consumed) {
                allocator.free(fe.content);
                allocator.free(fe.rel_path);
            }
        }
    }

    log.info("discovered files", &.{
        Field.uint("count", file_entries.items.len),
        Field.string("root", project_root),
    });

    if (file_entries.items.len == 0) {
        log.debug("no files to index", &.{});
        return result;
    }

    // Pre-allocate graph capacity: rough estimate of ~30 nodes and ~20 edges per file.
    {
        const est_nodes: u32 = @intCast(file_entries.items.len * 30);
        const est_edges: u32 = @intCast(file_entries.items.len * 20);
        try graph.nodes.ensureTotalCapacity(graph.allocator, est_nodes);
        try graph.edges.ensureTotalCapacity(graph.allocator, est_edges);
        try graph.edge_index.ensureTotalCapacity(graph.allocator, est_edges);
    }

    // Topological sort: imported files are parsed before their importers.
    try topoSortFiles(allocator, file_entries.items);

    // Parse each file through the visitor in dependency order.
    const lang_support = Registry.getByExtension(".zig") orelse return error.UnsupportedLanguage;
    const parse_fn = lang_support.parseFn orelse return error.NoParseFn;

    var file_infos = std.ArrayListUnmanaged(FileInfo){};
    defer file_infos.deinit(allocator);

    for (file_entries.items, 0..) |fe, entry_idx| {
        // Incremental check: skip if existing file node has matching hash.
        if (options.incremental) {
            if (findExistingFileNode(graph, fe.basename)) |existing_idx| {
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

        // Call visitor via registry. Creates file node + children + edges.
        log.debug("parsing file", &.{Field.string("path", fe.rel_path)});
        parse_fn(fe.content, @ptrCast(graph), log) catch {
            log.warn("file parse error", &.{Field.string("path", fe.rel_path)});
            result.files_errored += 1;
            continue;
        };

        // Transfer ownership of content and path to graph.
        try graph.addOwnedBuffer(fe.content);
        try graph.addOwnedBuffer(fe.rel_path);
        file_entries.items[entry_idx].consumed = true;

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
            });
        }

        result.files_indexed += 1;
    }

    // Compute metrics for function nodes in each file.
    for (file_infos.items) |fi| {
        source_scan.computeMetricsForNodes(graph, fi.source, fi.idx, fi.scope_end);
    }

    // Resolve inter-file imports and external (phantom) references.
    // Build basename-to-FileInfo-index map for cross-file lookup.
    log.debug("resolving cross-file edges", &.{Field.uint("file_count", file_infos.items.len)});
    var basename_map = std.StringHashMapUnmanaged(usize){};
    defer basename_map.deinit(allocator);
    try basename_map.ensureTotalCapacity(allocator, @intCast(file_infos.items.len));
    for (file_infos.items, 0..) |fi, i| {
        const name = graph.nodes.items[fi.idx].name;
        basename_map.putAssumeCapacity(name, i);
    }

    // Import edges between project files.
    for (file_infos.items) |fi| {
        const file_id: NodeId = @enumFromInt(fi.idx);

        // Only scan import_decl nodes within this file's node range.
        for (graph.nodes.items[fi.idx..fi.scope_end]) |n| {
            if (n.kind != .import_decl) continue;
            if (n.parent_id == null or n.parent_id.? != file_id) continue;

            const import_path = n.signature orelse continue;

            if (basename_map.get(import_path)) |target_idx| {
                try source_scan.addEdgeIfNew(graph, file_id, @enumFromInt(file_infos.items[target_idx].idx), .imports, .tree_sitter);
            }
        }
    }

    // Phantom nodes for std references.
    log.debug("resolving phantom nodes", &.{Field.uint("file_count", file_infos.items.len)});
    var phantom = PhantomManager.init(allocator, graph);
    defer phantom.deinit();

    for (file_infos.items) |fi| {
        const file_id: NodeId = @enumFromInt(fi.idx);

        // Find the std import variable name in this file (usually "std").
        var std_import_name: ?[]const u8 = null;
        for (graph.nodes.items[fi.idx..fi.scope_end]) |n| {
            if (n.kind != .import_decl) continue;
            if (n.parent_id == null or n.parent_id.? != file_id) continue;
            const import_path = n.signature orelse continue;
            if (std.mem.eql(u8, import_path, "std")) {
                std_import_name = n.name;
                const std_id = try phantom.getOrCreate("std", .module);
                try source_scan.addEdgeIfNew(graph, file_id, std_id, .imports, .phantom);
                break;
            }
        }

        if (std_import_name) |sname| {
            try source_scan.resolveStdPhantoms(graph, fi.source, fi.idx, fi.scope_end, &phantom, sname);
        }
    }

    log.info("indexing complete", &.{
        Field.uint("files_indexed", result.files_indexed),
        Field.uint("files_skipped", result.files_skipped),
        Field.uint("files_errored", result.files_errored),
        Field.uint("nodes", graph.nodeCount()),
        Field.uint("edges", graph.edgeCount()),
    });

    return result;
}

// ---------------------------------------------------------------------------
// Indexer helpers
// ---------------------------------------------------------------------------

fn computeContentHash(content: []const u8) [12]u8 {
    const h1 = std.hash.XxHash3.hash(0, content);
    const h2 = std.hash.XxHash3.hash(1, content);
    var result: [12]u8 = undefined;
    std.mem.writeInt(u64, result[0..8], h1, .little);
    std.mem.writeInt(u32, result[8..12], @truncate(h2), .little);
    return result;
}

/// Directories unconditionally excluded from indexation to not pollute the graph.
const hardcoded_excludes = [_][]const u8{ ".zig-cache", "zig-out" };

fn isExcluded(rel_path: []const u8, exclude_paths: []const []const u8) bool {
    const bn = std.fs.path.basename(rel_path);
    for (exclude_paths) |exc| {
        if (std.mem.eql(u8, rel_path, exc)) return true;
        if (std.mem.eql(u8, bn, exc)) return true;
        if (pathHasComponent(rel_path, exc)) return true;
    }
    for (&hardcoded_excludes) |dir| {
        if (pathHasComponent(rel_path, dir)) return true;
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

/// Extract all @import("...") basenames from source content as zero-copy
/// slices into `content`. Returns the number of basenames found.
fn extractImportBasenames(content: []const u8, out: [][]const u8) usize {
    const pattern = "@import(\"";
    var pos: usize = 0;
    var count: usize = 0;
    while (pos + pattern.len <= content.len) {
        const idx = std.mem.indexOf(u8, content[pos..], pattern) orelse break;
        const abs_idx = pos + idx;
        const path_start = abs_idx + pattern.len;
        if (path_start >= content.len) break;
        const end = std.mem.indexOfScalar(u8, content[path_start..], '"') orelse break;
        if (count < out.len) {
            out[count] = content[path_start .. path_start + end];
            count += 1;
        }
        pos = path_start + end + 1;
    }
    return count;
}

/// Topological sort of file entries using Kahn's algorithm.
/// Files with no imports come first; files that import others come after
/// their dependencies. Handles cycles gracefully by appending remaining files.
fn topoSortFiles(allocator: std.mem.Allocator, entries: []FileEntry) !void {
    const n = entries.len;
    if (n <= 1) return;

    // Build basename-to-index map.
    var name_map = std.StringHashMapUnmanaged(usize){};
    defer name_map.deinit(allocator);
    try name_map.ensureTotalCapacity(allocator, @intCast(n));
    for (entries, 0..) |fe, i| {
        name_map.putAssumeCapacity(fe.basename, i);
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

    var import_buf: [256][]const u8 = undefined;
    for (entries, 0..) |fe, i| {
        const count = extractImportBasenames(fe.content, &import_buf);
        for (import_buf[0..count]) |imp| {
            if (name_map.get(imp)) |dep_idx| {
                if (dep_idx != i) {
                    // i imports dep_idx, so dep_idx must come before i.
                    // Edge: dep_idx -> i in Kahn's sense.
                    try adj[dep_idx].append(allocator, i);
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

fn findExistingFileNode(graph: *const Graph, basename: []const u8) ?usize {
    for (graph.nodes.items, 0..) |n, i| {
        if (n.kind == .file and std.mem.eql(u8, n.name, basename)) {
            return i;
        }
    }
    return null;
}
