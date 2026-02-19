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


// ---------------------------------------------------------------------------
// Test helpers
// ---------------------------------------------------------------------------

const fixtures = @import("test-fixtures");

/// Write project fixture files into a temporary directory and return it.
fn setupProjectFixtures(tmp_dir: *std.testing.TmpDir) ![]const u8 {
    // Write main.zig
    try tmp_dir.dir.writeFile(.{
        .sub_path = "main.zig",
        .data = fixtures.zig.project.main_zig,
    });
    // Write parser.zig
    try tmp_dir.dir.writeFile(.{
        .sub_path = "parser.zig",
        .data = fixtures.zig.project.parser_zig,
    });
    // Write utils.zig
    try tmp_dir.dir.writeFile(.{
        .sub_path = "utils.zig",
        .data = fixtures.zig.project.utils_zig,
    });
    // Return the real path of the tmp dir for use as project_root.
    return try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
}

/// Index the project fixtures using the default options.
fn indexProjectFixtures(graph: *Graph, tmp_dir: *std.testing.TmpDir) !IndexResult {
    const project_root = try setupProjectFixtures(tmp_dir);
    defer std.testing.allocator.free(project_root);
    return indexDirectory(std.testing.allocator, project_root, graph, .{});
}

/// Count nodes of a given kind in the graph.
fn countNodesByKind(graph: *const Graph, kind: NodeKind) usize {
    var count: usize = 0;
    for (graph.nodes.items) |n| {
        if (n.kind == kind) count += 1;
    }
    return count;
}

/// Find the first node matching a name and kind.
fn findNode(graph: *const Graph, name: []const u8, kind: NodeKind) ?*const Node {
    for (graph.nodes.items) |*n| {
        if (n.kind == kind and std.mem.eql(u8, n.name, name)) return n;
    }
    return null;
}

/// Check if an edge exists between two node ids with the given type.
fn hasEdge(graph: *const Graph, source: NodeId, target: NodeId, edge_type: EdgeType) bool {
    for (graph.edges.items) |e| {
        if (e.source_id == source and e.target_id == target and e.edge_type == edge_type) return true;
    }
    return false;
}

/// Check if any edge with the given source has edge_source == .phantom.
fn hasPhantomEdge(graph: *const Graph, source: NodeId) bool {
    for (graph.edges.items) |e| {
        if (e.source_id == source and e.source == .phantom) return true;
    }
    return false;
}

// ===========================================================================
// Indexer: nominal
// ===========================================================================

test "indexes all zig files" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = indexProjectFixtures(&g, &tmp_dir) catch |err| return err;

    // Assert: 3 file nodes (main.zig, parser.zig, utils.zig)
    try std.testing.expectEqual(@as(usize, 3), countNodesByKind(&g, .file));
}

test "creates import edges between files" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = indexProjectFixtures(&g, &tmp_dir) catch |err| return err;

    // Assert: main.zig imports parser.zig and utils.zig
    const main_file = findNode(&g, "main.zig", .file) orelse return error.TestExpectedEqual;
    const parser_file = findNode(&g, "parser.zig", .file) orelse return error.TestExpectedEqual;
    const utils_file = findNode(&g, "utils.zig", .file) orelse return error.TestExpectedEqual;
    try std.testing.expect(hasEdge(&g, main_file.id, parser_file.id, .imports));
    try std.testing.expect(hasEdge(&g, main_file.id, utils_file.id, .imports));
}

test "creates cross-file calls edges" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = indexProjectFixtures(&g, &tmp_dir) catch |err| return err;

    // Assert: processInput calls Parser.parse (cross-file)
    const process_fn = findNode(&g, "processInput", .function) orelse return error.TestExpectedEqual;
    const parse_fn = findNode(&g, "parse", .function) orelse return error.TestExpectedEqual;
    try std.testing.expect(hasEdge(&g, process_fn.id, parse_fn.id, .calls));
}

test "creates cross-file uses_type edges" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = indexProjectFixtures(&g, &tmp_dir) catch |err| return err;

    // Assert: processInput uses Parser type
    const process_fn = findNode(&g, "processInput", .function) orelse return error.TestExpectedEqual;
    const parser_type = findNode(&g, "Parser", .type_def) orelse return error.TestExpectedEqual;
    try std.testing.expect(hasEdge(&g, process_fn.id, parser_type.id, .uses_type));
}

test "creates phantom nodes for std" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = indexProjectFixtures(&g, &tmp_dir) catch |err| return err;

    // Assert: at least one node with external=.stdlib exists
    var found_stdlib = false;
    for (g.nodes.items) |n| {
        switch (n.external) {
            .stdlib => {
                found_stdlib = true;
                break;
            },
            else => {},
        }
    }
    try std.testing.expect(found_stdlib);
}

test "phantom nodes have no file_path" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = indexProjectFixtures(&g, &tmp_dir) catch |err| return err;

    // Assert: every phantom (stdlib) node has file_path == null
    for (g.nodes.items) |n| {
        switch (n.external) {
            .stdlib => try std.testing.expectEqual(@as(?[]const u8, null), n.file_path),
            else => {},
        }
    }
}

test "phantom nodes have no line numbers" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = indexProjectFixtures(&g, &tmp_dir) catch |err| return err;

    // Assert: every phantom (stdlib) node has line_start == null
    for (g.nodes.items) |n| {
        switch (n.external) {
            .stdlib => {
                try std.testing.expectEqual(@as(?u32, null), n.line_start);
                try std.testing.expectEqual(@as(?u32, null), n.line_end);
            },
            else => {},
        }
    }
}

test "phantom edges have source phantom" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = indexProjectFixtures(&g, &tmp_dir) catch |err| return err;

    // Assert: edges targeting phantom nodes have source=.phantom
    for (g.edges.items) |e| {
        const target = g.getNode(e.target_id) orelse continue;
        switch (target.external) {
            .stdlib => try std.testing.expectEqual(EdgeSource.phantom, e.source),
            else => {},
        }
    }
}

test "file nodes have content_hash" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = indexProjectFixtures(&g, &tmp_dir) catch |err| return err;

    // Assert: each file node has a non-null content_hash
    for (g.nodes.items) |n| {
        if (n.kind == .file) {
            try std.testing.expect(n.content_hash != null);
        }
    }
}

test "all nodes have language zig" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = indexProjectFixtures(&g, &tmp_dir) catch |err| return err;

    // Assert: every non-phantom node has language=.zig
    for (g.nodes.items) |n| {
        switch (n.external) {
            .none => try std.testing.expectEqual(Language.zig, n.language),
            else => {},
        }
    }
}

test "parent_id chain is consistent" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = indexProjectFixtures(&g, &tmp_dir) catch |err| return err;

    // Assert: for every node with a parent_id, getNode(parent_id) != null
    for (g.nodes.items) |n| {
        if (n.parent_id) |pid| {
            try std.testing.expect(g.getNode(pid) != null);
        }
    }
}

test "no parent_id cycles" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = indexProjectFixtures(&g, &tmp_dir) catch |err| return err;

    // Assert: parent_id chain terminates within 100 hops for every node
    for (g.nodes.items) |n| {
        var current_id: ?NodeId = n.parent_id;
        var hops: usize = 0;
        while (current_id) |cid| {
            hops += 1;
            if (hops > 100) return error.TestExpectedEqual; // cycle detected
            const parent = g.getNode(cid) orelse break;
            current_id = parent.parent_id;
        }
    }
}

test "metrics computed" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = indexProjectFixtures(&g, &tmp_dir) catch |err| return err;

    // Assert: at least one function node has metrics.complexity > 0
    var found = false;
    for (g.nodes.items) |n| {
        if (n.kind == .function) {
            if (n.metrics) |m| {
                if (m.complexity > 0) {
                    found = true;
                    break;
                }
            }
        }
    }
    try std.testing.expect(found);
}

test "metrics lines counted" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = indexProjectFixtures(&g, &tmp_dir) catch |err| return err;

    // Assert: for functions with metrics and line info, lines == line_end - line_start + 1
    for (g.nodes.items) |n| {
        if (n.kind == .function) {
            if (n.metrics) |m| {
                if (n.line_start != null and n.line_end != null and m.lines > 0) {
                    const expected = n.line_end.? - n.line_start.? + 1;
                    try std.testing.expectEqual(expected, m.lines);
                }
            }
        }
    }
}

// ===========================================================================
// Indexer: incremental
// ===========================================================================

test "incremental skips unchanged files" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = setupProjectFixtures(&tmp_dir) catch return error.SkipZigTest;
    defer std.testing.allocator.free(project_root);

    // Act: index twice with incremental=true
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{ .incremental = true }) catch |err| return err;
    const result2 = indexDirectory(std.testing.allocator, project_root, &g, .{ .incremental = true }) catch |err| return err;

    // Assert: second pass skips files
    try std.testing.expect(result2.files_skipped > 0);
}

test "incremental detects changed file" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = setupProjectFixtures(&tmp_dir) catch return error.SkipZigTest;
    defer std.testing.allocator.free(project_root);

    // First index
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{ .incremental = true }) catch |err| return err;

    // Modify utils.zig
    try tmp_dir.dir.writeFile(.{
        .sub_path = "utils.zig",
        .data = "pub fn changed() void {}\n",
    });

    // Act: re-index
    const result2 = indexDirectory(std.testing.allocator, project_root, &g, .{ .incremental = true }) catch |err| return err;

    // Assert: at least one file was re-indexed
    try std.testing.expect(result2.files_indexed > 0);
}

test "content_hash changes when file changes" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = setupProjectFixtures(&tmp_dir) catch return error.SkipZigTest;
    defer std.testing.allocator.free(project_root);

    // First index
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Capture original hash for utils.zig
    const utils_node = findNode(&g, "utils.zig", .file) orelse return error.TestExpectedEqual;
    const old_hash = utils_node.content_hash orelse return error.TestExpectedEqual;

    // Modify utils.zig
    try tmp_dir.dir.writeFile(.{
        .sub_path = "utils.zig",
        .data = "pub fn changed() void {}\n",
    });

    // Re-index into a fresh graph
    var g2 = Graph.init(std.testing.allocator, "/tmp/project");
    defer g2.deinit();
    _ = indexDirectory(std.testing.allocator, project_root, &g2, .{}) catch |err| return err;

    // Assert: hash differs
    const utils_node2 = findNode(&g2, "utils.zig", .file) orelse return error.TestExpectedEqual;
    const new_hash = utils_node2.content_hash orelse return error.TestExpectedEqual;
    try std.testing.expect(!std.mem.eql(u8, &old_hash, &new_hash));
}

// ===========================================================================
// Indexer: edge cases
// ===========================================================================

test "single file project" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(.{
        .sub_path = "single.zig",
        .data = fixtures.zig.edge_cases.project_single_file,
    });
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: 1 file node + its children
    try std.testing.expectEqual(@as(usize, 1), countNodesByKind(&g, .file));
    try std.testing.expect(g.nodeCount() > 1); // file + at least one child
}

test "directory with no zig files" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(.{
        .sub_path = "readme.txt",
        .data = "no zig here",
    });
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: 0 file nodes, no crash
    try std.testing.expectEqual(@as(usize, 0), countNodesByKind(&g, .file));
    try std.testing.expectEqual(@as(usize, 0), g.nodeCount());
}

test "respects exclude_paths" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = setupProjectFixtures(&tmp_dir) catch return error.SkipZigTest;
    defer std.testing.allocator.free(project_root);

    // Act: exclude parser.zig
    const exclude = [_][]const u8{"parser.zig"};
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{
        .exclude_paths = &exclude,
    }) catch |err| return err;

    // Assert: parser.zig not indexed
    try std.testing.expectEqual(@as(?*const Node, null), findNode(&g, "parser.zig", .file));
    // But main.zig and utils.zig are
    try std.testing.expect(findNode(&g, "main.zig", .file) != null);
    try std.testing.expect(findNode(&g, "utils.zig", .file) != null);
}

test "file with no imports" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    _ = indexProjectFixtures(&g, &tmp_dir) catch |err| return err;

    // Assert: utils.zig has no project import edges (only imports std, which is phantom)
    const utils_file = findNode(&g, "utils.zig", .file) orelse return error.TestExpectedEqual;
    for (g.edges.items) |e| {
        if (e.source_id == utils_file.id and e.edge_type == .imports) {
            // Target must be a phantom node, not a project file
            const target = g.getNode(e.target_id) orelse continue;
            switch (target.external) {
                .none => return error.TestExpectedEqual, // project import found, fail
                else => {},
            }
        }
    }
}

// ===========================================================================
// Phantom nodes
// ===========================================================================

test "phantom node for std.mem.Allocator" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    _ = indexProjectFixtures(&g, &tmp_dir) catch |err| return err;

    // Assert: a phantom type_def node with name containing "Allocator" exists
    var found = false;
    for (g.nodes.items) |n| {
        switch (n.external) {
            .stdlib => {
                if (n.kind == .type_def and std.mem.indexOf(u8, n.name, "Allocator") != null) {
                    found = true;
                    break;
                }
            },
            else => {},
        }
    }
    try std.testing.expect(found);
}

test "phantom nodes are deduplicated" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    _ = indexProjectFixtures(&g, &tmp_dir) catch |err| return err;

    // Assert: exactly 1 phantom node with name containing "Allocator"
    // (both main.zig and parser.zig reference std.mem.Allocator)
    var count: usize = 0;
    for (g.nodes.items) |n| {
        switch (n.external) {
            .stdlib => {
                if (std.mem.indexOf(u8, n.name, "Allocator") != null) {
                    count += 1;
                }
            },
            else => {},
        }
    }
    try std.testing.expectEqual(@as(usize, 1), count);
}

test "phantom node parent chain" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    _ = indexProjectFixtures(&g, &tmp_dir) catch |err| return err;

    // Assert: Allocator phantom has parent chain: Allocator → mem → std
    // Find Allocator phantom
    var allocator_node: ?*const Node = null;
    for (g.nodes.items) |*n| {
        switch (n.external) {
            .stdlib => {
                if (n.kind == .type_def and std.mem.indexOf(u8, n.name, "Allocator") != null) {
                    allocator_node = n;
                    break;
                }
            },
            else => {},
        }
    }
    const alloc_n = allocator_node orelse return error.TestExpectedEqual;

    // Allocator → mem (module)
    try std.testing.expect(alloc_n.parent_id != null);
    const mem_node = g.getNode(alloc_n.parent_id.?) orelse return error.TestExpectedEqual;
    try std.testing.expect(std.mem.indexOf(u8, mem_node.name, "mem") != null);

    // mem → std (module)
    try std.testing.expect(mem_node.parent_id != null);
    const std_node = g.getNode(mem_node.parent_id.?) orelse return error.TestExpectedEqual;
    try std.testing.expect(std.mem.indexOf(u8, std_node.name, "std") != null);
}

// ===========================================================================
// Test helpers: name collision fixtures
// ===========================================================================

/// Write name collision fixture files into a temporary directory.
fn writeCollisionFixtures(dir: std.fs.Dir) !void {
    try dir.writeFile(.{
        .sub_path = "alpha.zig",
        .data = fixtures.zig.name_collision.alpha_zig,
    });
    try dir.writeFile(.{
        .sub_path = "beta.zig",
        .data = fixtures.zig.name_collision.beta_zig,
    });
    try dir.writeFile(.{
        .sub_path = "consumer.zig",
        .data = fixtures.zig.name_collision.consumer_zig,
    });
}

/// Find a node matching name and kind that is a descendant of a given ancestor node.
/// Used to distinguish same-named nodes across different files.
fn findNodeInFile(graph: *const Graph, name: []const u8, kind: NodeKind, ancestor_id: NodeId) ?NodeId {
    for (graph.nodes.items, 0..) |n, i| {
        const nid: NodeId = @enumFromInt(i);
        if (n.kind == kind and std.mem.eql(u8, n.name, name) and source_scan.isDescendantOf(graph, nid, ancestor_id)) return nid;
    }
    return null;
}

// ===========================================================================
// Cross-file name collision: calls resolution
// ===========================================================================

test "cross-file init call resolves to correct struct" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/collision");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeCollisionFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: useAlpha has a calls edge to alpha's init, not beta's
    const alpha_file = findNode(&g, "alpha.zig", .file) orelse return error.TestExpectedEqual;
    const consumer_file = findNode(&g, "consumer.zig", .file) orelse return error.TestExpectedEqual;
    const alpha_init = findNodeInFile(&g, "init", .function, alpha_file.id) orelse return error.TestExpectedEqual;
    const use_alpha = findNodeInFile(&g, "useAlpha", .function, consumer_file.id) orelse return error.TestExpectedEqual;
    try std.testing.expect(hasEdge(&g, use_alpha, alpha_init, .calls));
}

test "cross-file deinit call resolves to correct struct" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/collision");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeCollisionFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: useAlpha has a calls edge to alpha's deinit, not beta's
    const alpha_file = findNode(&g, "alpha.zig", .file) orelse return error.TestExpectedEqual;
    const consumer_file = findNode(&g, "consumer.zig", .file) orelse return error.TestExpectedEqual;
    const alpha_deinit = findNodeInFile(&g, "deinit", .function, alpha_file.id) orelse return error.TestExpectedEqual;
    const use_alpha = findNodeInFile(&g, "useAlpha", .function, consumer_file.id) orelse return error.TestExpectedEqual;
    try std.testing.expect(hasEdge(&g, use_alpha, alpha_deinit, .calls));
}

test "cross-file init call to second file resolves correctly" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/collision");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeCollisionFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: useBeta has a calls edge to beta's init, not alpha's
    const beta_file = findNode(&g, "beta.zig", .file) orelse return error.TestExpectedEqual;
    const consumer_file = findNode(&g, "consumer.zig", .file) orelse return error.TestExpectedEqual;
    const beta_init = findNodeInFile(&g, "init", .function, beta_file.id) orelse return error.TestExpectedEqual;
    const use_beta = findNodeInFile(&g, "useBeta", .function, consumer_file.id) orelse return error.TestExpectedEqual;
    try std.testing.expect(hasEdge(&g, use_beta, beta_init, .calls));
}

// ===========================================================================
// Cross-file name collision: Self / uses_type resolution
// ===========================================================================

test "Self constants are filtered in cross-file indexation" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/collision");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeCollisionFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: @This() aliases are filtered — no Self constant nodes exist
    const alpha_file = findNode(&g, "alpha.zig", .file) orelse return error.TestExpectedEqual;
    const beta_file = findNode(&g, "beta.zig", .file) orelse return error.TestExpectedEqual;
    try std.testing.expectEqual(@as(?NodeId, null), findNodeInFile(&g, "Self", .constant, alpha_file.id));
    try std.testing.expectEqual(@as(?NodeId, null), findNodeInFile(&g, "Self", .constant, beta_file.id));

    // Assert: init functions still exist in both files
    try std.testing.expect(findNodeInFile(&g, "init", .function, alpha_file.id) != null);
    try std.testing.expect(findNodeInFile(&g, "init", .function, beta_file.id) != null);
}

// ===========================================================================
// Cross-file name collision: negative tests (wrong edges must not exist)
// ===========================================================================

test "useAlpha does not call beta init" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/collision");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeCollisionFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: useAlpha must not have a calls edge to beta's init
    const beta_file = findNode(&g, "beta.zig", .file) orelse return error.TestExpectedEqual;
    const consumer_file = findNode(&g, "consumer.zig", .file) orelse return error.TestExpectedEqual;
    const beta_init = findNodeInFile(&g, "init", .function, beta_file.id) orelse return error.TestExpectedEqual;
    const use_alpha = findNodeInFile(&g, "useAlpha", .function, consumer_file.id) orelse return error.TestExpectedEqual;
    try std.testing.expect(!hasEdge(&g, use_alpha, beta_init, .calls));
}

test "useBeta does not call alpha init" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/collision");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeCollisionFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: useBeta must not have a calls edge to alpha's init
    const alpha_file = findNode(&g, "alpha.zig", .file) orelse return error.TestExpectedEqual;
    const consumer_file = findNode(&g, "consumer.zig", .file) orelse return error.TestExpectedEqual;
    const alpha_init = findNodeInFile(&g, "init", .function, alpha_file.id) orelse return error.TestExpectedEqual;
    const use_beta = findNodeInFile(&g, "useBeta", .function, consumer_file.id) orelse return error.TestExpectedEqual;
    try std.testing.expect(!hasEdge(&g, use_beta, alpha_init, .calls));
}

// ===========================================================================
// Test block: import-assigned variable method call resolution
// ===========================================================================

test "test block resolves method call on import-assigned variable" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(.{
        .sub_path = "provider.zig",
        .data = fixtures.zig.test_import_call.provider_zig,
    });
    try tmp_dir.dir.writeFile(.{
        .sub_path = "consumer.zig",
        .data = fixtures.zig.test_import_call.consumer_zig,
    });
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: the test node has a calls edge to the increment method
    const provider_file = findNode(&g, "provider.zig", .file) orelse return error.TestExpectedEqual;
    const increment_id = findNodeInFile(&g, "increment", .function, provider_file.id) orelse return error.TestExpectedEqual;

    var test_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .test_def and std.mem.eql(u8, n.name, "import var method call")) {
            test_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(test_id != null);

    try std.testing.expect(hasEdge(&g, test_id.?, increment_id, .calls));
}

// ===========================================================================
// Parameter method call: fixture helpers
// ===========================================================================

/// Write the basic param_method_call fixtures (service.zig + consumer.zig) into
/// a temporary directory. Returns the real project root path (caller must free).
fn writeBasicParamFixtures(dir: std.fs.Dir) !void {
    try dir.writeFile(.{ .sub_path = "service.zig", .data = fixtures.zig.param_method_call.service_zig });
    try dir.writeFile(.{ .sub_path = "consumer.zig", .data = fixtures.zig.param_method_call.consumer_zig });
}

/// Write service.zig + pointer_param.zig fixtures.
fn writePointerParamFixtures(dir: std.fs.Dir) !void {
    try dir.writeFile(.{ .sub_path = "service.zig", .data = fixtures.zig.param_method_call.service_zig });
    try dir.writeFile(.{ .sub_path = "pointer_param.zig", .data = fixtures.zig.param_method_call.pointer_param_zig });
}

/// Write service.zig + optional_param.zig fixtures.
fn writeOptionalParamFixtures(dir: std.fs.Dir) !void {
    try dir.writeFile(.{ .sub_path = "service.zig", .data = fixtures.zig.param_method_call.service_zig });
    try dir.writeFile(.{ .sub_path = "optional_param.zig", .data = fixtures.zig.param_method_call.optional_param_zig });
}

/// Write service.zig + client.zig + service_with_client.zig + chained.zig fixtures.
fn writeChainedParamFixtures(dir: std.fs.Dir) !void {
    try dir.writeFile(.{ .sub_path = "client.zig", .data = fixtures.zig.param_method_call.client_zig });
    try dir.writeFile(.{ .sub_path = "service_with_client.zig", .data = fixtures.zig.param_method_call.service_with_client_zig });
    try dir.writeFile(.{ .sub_path = "chained.zig", .data = fixtures.zig.param_method_call.chained_zig });
}

/// Write service.zig + client.zig + multi_param.zig fixtures.
fn writeMultiParamFixtures(dir: std.fs.Dir) !void {
    try dir.writeFile(.{ .sub_path = "service.zig", .data = fixtures.zig.param_method_call.service_zig });
    try dir.writeFile(.{ .sub_path = "client.zig", .data = fixtures.zig.param_method_call.client_zig });
    try dir.writeFile(.{ .sub_path = "multi_param.zig", .data = fixtures.zig.param_method_call.multi_param_zig });
}

/// Write service.zig + self_calls_param.zig fixtures.
fn writeSelfCallsParamFixtures(dir: std.fs.Dir) !void {
    try dir.writeFile(.{ .sub_path = "service.zig", .data = fixtures.zig.param_method_call.service_zig });
    try dir.writeFile(.{ .sub_path = "self_calls_param.zig", .data = fixtures.zig.param_method_call.self_calls_param_zig });
}

/// Write service.zig + factory.zig + return_value.zig fixtures.
fn writeReturnValueFixtures(dir: std.fs.Dir) !void {
    try dir.writeFile(.{ .sub_path = "service.zig", .data = fixtures.zig.param_method_call.service_zig });
    try dir.writeFile(.{ .sub_path = "factory.zig", .data = fixtures.zig.param_method_call.factory_zig });
    try dir.writeFile(.{ .sub_path = "return_value.zig", .data = fixtures.zig.param_method_call.return_value_zig });
}

/// Write service.zig + no_calls.zig fixtures (negative test).
fn writeNoCallsFixtures(dir: std.fs.Dir) !void {
    try dir.writeFile(.{ .sub_path = "service.zig", .data = fixtures.zig.param_method_call.service_zig });
    try dir.writeFile(.{ .sub_path = "no_calls.zig", .data = fixtures.zig.param_method_call.no_calls_zig });
}

// ===========================================================================
// Parameter method call: basic parameter
// ===========================================================================

test "parameter method call creates cross-file calls edge" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeBasicParamFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: handle() has a calls edge to Service.process()
    const service_file = findNode(&g, "service.zig", .file) orelse return error.TestExpectedEqual;
    const consumer_file = findNode(&g, "consumer.zig", .file) orelse return error.TestExpectedEqual;
    const process_fn = findNodeInFile(&g, "process", .function, service_file.id) orelse return error.TestExpectedEqual;
    const handle_fn = findNodeInFile(&g, "handle", .function, consumer_file.id) orelse return error.TestExpectedEqual;
    try std.testing.expect(hasEdge(&g, handle_fn, process_fn, .calls));
}

test "parameter method call edge is calls not uses_type" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeBasicParamFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: the edge from handle to process is .calls, not .uses_type
    const service_file = findNode(&g, "service.zig", .file) orelse return error.TestExpectedEqual;
    const consumer_file = findNode(&g, "consumer.zig", .file) orelse return error.TestExpectedEqual;
    const process_fn = findNodeInFile(&g, "process", .function, service_file.id) orelse return error.TestExpectedEqual;
    const handle_fn = findNodeInFile(&g, "handle", .function, consumer_file.id) orelse return error.TestExpectedEqual;
    try std.testing.expect(!hasEdge(&g, handle_fn, process_fn, .uses_type));
}

test "parameter method call coexists with uses_type edge to struct" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeBasicParamFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: handle() also has a uses_type edge to Service (the struct, not the method)
    const service_file = findNode(&g, "service.zig", .file) orelse return error.TestExpectedEqual;
    const consumer_file = findNode(&g, "consumer.zig", .file) orelse return error.TestExpectedEqual;
    const service_type = findNodeInFile(&g, "Service", .type_def, service_file.id) orelse return error.TestExpectedEqual;
    const handle_fn = findNodeInFile(&g, "handle", .function, consumer_file.id) orelse return error.TestExpectedEqual;
    try std.testing.expect(hasEdge(&g, handle_fn, service_type, .uses_type));
}

test "parameter method call source is calling function" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeBasicParamFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: the calls edge direction is handle → process (not reverse)
    const service_file = findNode(&g, "service.zig", .file) orelse return error.TestExpectedEqual;
    const consumer_file = findNode(&g, "consumer.zig", .file) orelse return error.TestExpectedEqual;
    const process_fn = findNodeInFile(&g, "process", .function, service_file.id) orelse return error.TestExpectedEqual;
    const handle_fn = findNodeInFile(&g, "handle", .function, consumer_file.id) orelse return error.TestExpectedEqual;
    try std.testing.expect(!hasEdge(&g, process_fn, handle_fn, .calls));
}

// ===========================================================================
// Parameter method call: pointer parameter
// ===========================================================================

test "pointer parameter method call resolves through indirection" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writePointerParamFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: handlePtr() has a calls edge to Service.reset()
    const service_file = findNode(&g, "service.zig", .file) orelse return error.TestExpectedEqual;
    const param_file = findNode(&g, "pointer_param.zig", .file) orelse return error.TestExpectedEqual;
    const reset_fn = findNodeInFile(&g, "reset", .function, service_file.id) orelse return error.TestExpectedEqual;
    const handle_ptr_fn = findNodeInFile(&g, "handlePtr", .function, param_file.id) orelse return error.TestExpectedEqual;
    try std.testing.expect(hasEdge(&g, handle_ptr_fn, reset_fn, .calls));
}

test "pointer parameter method call has uses_type edge to struct" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writePointerParamFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: handlePtr() has a uses_type edge to Service
    const service_file = findNode(&g, "service.zig", .file) orelse return error.TestExpectedEqual;
    const param_file = findNode(&g, "pointer_param.zig", .file) orelse return error.TestExpectedEqual;
    const service_type = findNodeInFile(&g, "Service", .type_def, service_file.id) orelse return error.TestExpectedEqual;
    const handle_ptr_fn = findNodeInFile(&g, "handlePtr", .function, param_file.id) orelse return error.TestExpectedEqual;
    try std.testing.expect(hasEdge(&g, handle_ptr_fn, service_type, .uses_type));
}

// ===========================================================================
// Parameter method call: optional parameter
// ===========================================================================

test "optional parameter method call resolves through unwrap" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeOptionalParamFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: handleOpt() has a calls edge to Service.process()
    const service_file = findNode(&g, "service.zig", .file) orelse return error.TestExpectedEqual;
    const param_file = findNode(&g, "optional_param.zig", .file) orelse return error.TestExpectedEqual;
    const process_fn = findNodeInFile(&g, "process", .function, service_file.id) orelse return error.TestExpectedEqual;
    const handle_opt_fn = findNodeInFile(&g, "handleOpt", .function, param_file.id) orelse return error.TestExpectedEqual;
    try std.testing.expect(hasEdge(&g, handle_opt_fn, process_fn, .calls));
}

// ===========================================================================
// Parameter method call: chained cross-file call
// ===========================================================================

test "chained cross-file method call creates calls edge to first method" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeChainedParamFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: handleChained() has a calls edge to ServiceWithClient.getClient()
    const swc_file = findNode(&g, "service_with_client.zig", .file) orelse return error.TestExpectedEqual;
    const chained_file = findNode(&g, "chained.zig", .file) orelse return error.TestExpectedEqual;
    const get_client_fn = findNodeInFile(&g, "getClient", .function, swc_file.id) orelse return error.TestExpectedEqual;
    const handle_chained_fn = findNodeInFile(&g, "handleChained", .function, chained_file.id) orelse return error.TestExpectedEqual;
    try std.testing.expect(hasEdge(&g, handle_chained_fn, get_client_fn, .calls));
}

test "chained cross-file method call creates transitive calls edge" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeChainedParamFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: handleChained() has a calls edge to Client.send() (transitive through getClient())
    const client_file = findNode(&g, "client.zig", .file) orelse return error.TestExpectedEqual;
    const chained_file = findNode(&g, "chained.zig", .file) orelse return error.TestExpectedEqual;
    const send_fn = findNodeInFile(&g, "send", .function, client_file.id) orelse return error.TestExpectedEqual;
    const handle_chained_fn = findNodeInFile(&g, "handleChained", .function, chained_file.id) orelse return error.TestExpectedEqual;
    try std.testing.expect(hasEdge(&g, handle_chained_fn, send_fn, .calls));
}

// ===========================================================================
// Parameter method call: multiple parameters from different files
// ===========================================================================

test "multiple cross-file parameters each get calls edges" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeMultiParamFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: handleMulti() has calls edges to both Service.process() and Client.send()
    const service_file = findNode(&g, "service.zig", .file) orelse return error.TestExpectedEqual;
    const client_file = findNode(&g, "client.zig", .file) orelse return error.TestExpectedEqual;
    const multi_file = findNode(&g, "multi_param.zig", .file) orelse return error.TestExpectedEqual;
    const process_fn = findNodeInFile(&g, "process", .function, service_file.id) orelse return error.TestExpectedEqual;
    const send_fn = findNodeInFile(&g, "send", .function, client_file.id) orelse return error.TestExpectedEqual;
    const handle_multi_fn = findNodeInFile(&g, "handleMulti", .function, multi_file.id) orelse return error.TestExpectedEqual;
    try std.testing.expect(hasEdge(&g, handle_multi_fn, process_fn, .calls));
    try std.testing.expect(hasEdge(&g, handle_multi_fn, send_fn, .calls));
}

test "multiple cross-file parameters each get uses_type edges" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeMultiParamFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: handleMulti() has uses_type edges to both Service and Client
    const service_file = findNode(&g, "service.zig", .file) orelse return error.TestExpectedEqual;
    const client_file = findNode(&g, "client.zig", .file) orelse return error.TestExpectedEqual;
    const multi_file = findNode(&g, "multi_param.zig", .file) orelse return error.TestExpectedEqual;
    const service_type = findNodeInFile(&g, "Service", .type_def, service_file.id) orelse return error.TestExpectedEqual;
    const client_type = findNodeInFile(&g, "Client", .type_def, client_file.id) orelse return error.TestExpectedEqual;
    const handle_multi_fn = findNodeInFile(&g, "handleMulti", .function, multi_file.id) orelse return error.TestExpectedEqual;
    try std.testing.expect(hasEdge(&g, handle_multi_fn, service_type, .uses_type));
    try std.testing.expect(hasEdge(&g, handle_multi_fn, client_type, .uses_type));
}

test "multiple cross-file parameters do not create spurious calls edges" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeMultiParamFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: handleMulti() does NOT have calls edges to uncalled methods
    const client_file = findNode(&g, "client.zig", .file) orelse return error.TestExpectedEqual;
    const multi_file = findNode(&g, "multi_param.zig", .file) orelse return error.TestExpectedEqual;
    const disconnect_fn = findNodeInFile(&g, "disconnect", .function, client_file.id) orelse return error.TestExpectedEqual;
    const handle_multi_fn = findNodeInFile(&g, "handleMulti", .function, multi_file.id) orelse return error.TestExpectedEqual;
    try std.testing.expect(!hasEdge(&g, handle_multi_fn, disconnect_fn, .calls));
}

// ===========================================================================
// Parameter method call: self method calling parameter method
// ===========================================================================

test "struct method resolves parameter method call across files" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeSelfCallsParamFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: Handler.execute() has a calls edge to Service.process()
    const service_file = findNode(&g, "service.zig", .file) orelse return error.TestExpectedEqual;
    const self_file = findNode(&g, "self_calls_param.zig", .file) orelse return error.TestExpectedEqual;
    const process_fn = findNodeInFile(&g, "process", .function, service_file.id) orelse return error.TestExpectedEqual;
    const execute_fn = findNodeInFile(&g, "execute", .function, self_file.id) orelse return error.TestExpectedEqual;
    try std.testing.expect(hasEdge(&g, execute_fn, process_fn, .calls));
}

test "struct method has uses_type edge to parameter type from another file" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeSelfCallsParamFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: Handler.execute() has a uses_type edge to Service
    const service_file = findNode(&g, "service.zig", .file) orelse return error.TestExpectedEqual;
    const self_file = findNode(&g, "self_calls_param.zig", .file) orelse return error.TestExpectedEqual;
    const service_type = findNodeInFile(&g, "Service", .type_def, service_file.id) orelse return error.TestExpectedEqual;
    const execute_fn = findNodeInFile(&g, "execute", .function, self_file.id) orelse return error.TestExpectedEqual;
    try std.testing.expect(hasEdge(&g, execute_fn, service_type, .uses_type));
}

// ===========================================================================
// Parameter method call: return value from imported function
// ===========================================================================

test "return value from imported function gets calls edge for method" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeReturnValueFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: useFactory() has a calls edge to factory.create()
    const factory_file = findNode(&g, "factory.zig", .file) orelse return error.TestExpectedEqual;
    const rv_file = findNode(&g, "return_value.zig", .file) orelse return error.TestExpectedEqual;
    const create_fn = findNodeInFile(&g, "create", .function, factory_file.id) orelse return error.TestExpectedEqual;
    const use_factory_fn = findNodeInFile(&g, "useFactory", .function, rv_file.id) orelse return error.TestExpectedEqual;
    try std.testing.expect(hasEdge(&g, use_factory_fn, create_fn, .calls));
}

test "return value from imported function resolves method on result" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeReturnValueFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: useFactory() has a calls edge to Service.process() (via return type of create())
    const service_file = findNode(&g, "service.zig", .file) orelse return error.TestExpectedEqual;
    const rv_file = findNode(&g, "return_value.zig", .file) orelse return error.TestExpectedEqual;
    const process_fn = findNodeInFile(&g, "process", .function, service_file.id) orelse return error.TestExpectedEqual;
    const use_factory_fn = findNodeInFile(&g, "useFactory", .function, rv_file.id) orelse return error.TestExpectedEqual;
    try std.testing.expect(hasEdge(&g, use_factory_fn, process_fn, .calls));
}

// ===========================================================================
// Parameter method call: negative tests (no false positives)
// ===========================================================================

test "no calls edge when parameter method is not called" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeNoCallsFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: noMethodCalls() has NO calls edge to Service.process()
    const service_file = findNode(&g, "service.zig", .file) orelse return error.TestExpectedEqual;
    const no_calls_file = findNode(&g, "no_calls.zig", .file) orelse return error.TestExpectedEqual;
    const process_fn = findNodeInFile(&g, "process", .function, service_file.id) orelse return error.TestExpectedEqual;
    const no_calls_fn = findNodeInFile(&g, "noMethodCalls", .function, no_calls_file.id) orelse return error.TestExpectedEqual;
    try std.testing.expect(!hasEdge(&g, no_calls_fn, process_fn, .calls));
}

test "no calls edge when parameter method is not called but uses_type exists" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeNoCallsFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: noMethodCalls() still has a uses_type edge to Service even without calls
    const service_file = findNode(&g, "service.zig", .file) orelse return error.TestExpectedEqual;
    const no_calls_file = findNode(&g, "no_calls.zig", .file) orelse return error.TestExpectedEqual;
    const service_type = findNodeInFile(&g, "Service", .type_def, service_file.id) orelse return error.TestExpectedEqual;
    const no_calls_fn = findNodeInFile(&g, "noMethodCalls", .function, no_calls_file.id) orelse return error.TestExpectedEqual;
    try std.testing.expect(hasEdge(&g, no_calls_fn, service_type, .uses_type));
}

test "uncalled method in provider does not get spurious calls edge" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/param");
    defer g.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeBasicParamFixtures(tmp_dir.dir);
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: handle() does NOT have a calls edge to Service.reset() (only process() is called)
    const service_file = findNode(&g, "service.zig", .file) orelse return error.TestExpectedEqual;
    const consumer_file = findNode(&g, "consumer.zig", .file) orelse return error.TestExpectedEqual;
    const reset_fn = findNodeInFile(&g, "reset", .function, service_file.id) orelse return error.TestExpectedEqual;
    const handle_fn = findNodeInFile(&g, "handle", .function, consumer_file.id) orelse return error.TestExpectedEqual;
    try std.testing.expect(!hasEdge(&g, handle_fn, reset_fn, .calls));
}
