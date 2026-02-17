const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const node_mod = @import("../core/node.zig");
const edge_mod = @import("../core/edge.zig");
const types = @import("../core/types.zig");
const lang = @import("../languages/language.zig");
const registry_mod = @import("../languages/registry.zig");
const phantom_mod = @import("../core/phantom.zig");
const metrics_mod = @import("../core/metrics.zig");

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
};

/// Tracks a parsed file's node index and source for post-processing.
const FileInfo = struct {
    idx: usize,
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

    // Discover .zig files in the project directory.
    var dir = try std.fs.openDirAbsolute(project_root, .{ .iterate = true });
    defer dir.close();

    var file_entries = std.ArrayListUnmanaged(FileEntry){};
    defer file_entries.deinit(allocator);

    var basenames_list = std.ArrayListUnmanaged([]const u8){};
    defer basenames_list.deinit(allocator);

    var consumed = std.ArrayListUnmanaged(bool){};
    defer consumed.deinit(allocator);

    {
        var walker = try dir.walk(allocator);
        defer walker.deinit();

        while (try walker.next()) |entry| {
            if (entry.kind != .file) continue;
            if (!std.mem.endsWith(u8, entry.path, ".zig")) continue;
            if (isExcluded(entry.path, options.exclude_paths)) continue;

            const file = dir.openFile(entry.path, .{}) catch {
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
            try basenames_list.append(allocator, basename);
            try consumed.append(allocator, false);
        }
    }

    // Cleanup non-consumed entries (skipped or error). This defer runs before
    // the deinit defers above (reverse declaration order), so items are valid.
    defer {
        for (file_entries.items, 0..) |fe, i| {
            if (i < consumed.items.len and !consumed.items[i]) {
                allocator.free(fe.content);
                allocator.free(fe.rel_path);
            }
        }
    }

    if (file_entries.items.len == 0) return result;

    // Sort files so that imported files are parsed before their importers.
    for (file_entries.items) |*fe| {
        fe.import_count = countProjectImports(fe.content, basenames_list.items);
    }

    std.sort.block(FileEntry, file_entries.items, {}, struct {
        fn lessThan(_: void, a: FileEntry, b: FileEntry) bool {
            if (a.import_count != b.import_count) return a.import_count < b.import_count;
            return std.mem.order(u8, a.basename, b.basename) == .lt;
        }
    }.lessThan);

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
                        result.files_skipped += 1;
                        continue;
                    }
                }
            }
        }

        const before_count = graph.nodeCount();

        // Call visitor via registry. Creates file node + children + edges.
        parse_fn(fe.content, @ptrCast(graph)) catch {
            result.files_errored += 1;
            continue;
        };

        // Transfer ownership of content and path to graph.
        try graph.addOwnedBuffer(fe.content);
        try graph.addOwnedBuffer(fe.rel_path);
        consumed.items[entry_idx] = true;

        // Patch the file node (visitor creates it at before_count with name="").
        if (before_count < graph.nodeCount()) {
            var file_node = &graph.nodes.items[before_count];
            file_node.name = fe.basename;
            file_node.file_path = fe.rel_path;
            file_node.content_hash = fe.content_hash;

            try file_infos.append(allocator, .{ .idx = before_count, .source = fe.content });
        }

        result.files_indexed += 1;
    }

    // Compute metrics for function nodes in each file.
    for (file_infos.items) |fi| {
        computeMetricsForNodes(graph, fi.source, fi.idx);
    }

    // Resolve inter-file imports and external (phantom) references.

    // Import edges between project files.
    for (file_infos.items) |fi| {
        const file_id: NodeId = @enumFromInt(fi.idx);

        for (graph.nodes.items) |n| {
            if (n.kind != .import_decl) continue;
            if (n.parent_id == null or n.parent_id.? != file_id) continue;

            const import_path = n.signature orelse continue;

            // Find target file node by matching import path to file basenames.
            for (file_infos.items) |target_fi| {
                const target_file = graph.nodes.items[target_fi.idx];
                if (std.mem.eql(u8, target_file.name, import_path)) {
                    try addEdgeIfNew(graph, file_id, @enumFromInt(target_fi.idx), .imports, .tree_sitter);
                    break;
                }
            }
        }
    }

    // Phantom nodes for std references.
    var phantom = PhantomManager.init(allocator, graph);
    defer phantom.deinit();

    for (file_infos.items) |fi| {
        const file_id: NodeId = @enumFromInt(fi.idx);

        // Find the std import variable name in this file (usually "std").
        var std_import_name: ?[]const u8 = null;
        for (graph.nodes.items) |n| {
            if (n.kind != .import_decl) continue;
            if (n.parent_id == null or n.parent_id.? != file_id) continue;
            const import_path = n.signature orelse continue;
            if (std.mem.eql(u8, import_path, "std")) {
                std_import_name = n.name;
                const std_id = try phantom.getOrCreate("std", .module);
                try addEdgeIfNew(graph, file_id, std_id, .imports, .phantom);
                break;
            }
        }

        if (std_import_name) |sname| {
            try resolveStdPhantoms(graph, fi.source, fi.idx, &phantom, sname);
        }
    }

    return result;
}

// ---------------------------------------------------------------------------
// Indexer helpers
// ---------------------------------------------------------------------------

fn computeContentHash(content: []const u8) [12]u8 {
    var full_hash: [32]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(content, &full_hash, .{});
    return full_hash[0..12].*;
}

fn isExcluded(rel_path: []const u8, exclude_paths: []const []const u8) bool {
    const bn = std.fs.path.basename(rel_path);
    for (exclude_paths) |exc| {
        if (std.mem.eql(u8, rel_path, exc)) return true;
        if (std.mem.eql(u8, bn, exc)) return true;
    }
    return false;
}

fn countProjectImports(content: []const u8, basenames: []const []const u8) usize {
    var count: usize = 0;
    const pattern = "@import(\"";
    var search_from: usize = 0;
    while (search_from + pattern.len <= content.len) {
        const idx = std.mem.indexOf(u8, content[search_from..], pattern) orelse break;
        const abs_idx = search_from + idx;
        const path_start = abs_idx + pattern.len;
        if (path_start >= content.len) break;
        const path_end_rel = std.mem.indexOfScalar(u8, content[path_start..], '"') orelse break;
        const import_path = content[path_start .. path_start + path_end_rel];
        for (basenames) |bn| {
            if (std.mem.eql(u8, import_path, bn)) {
                count += 1;
                break;
            }
        }
        search_from = path_start + path_end_rel + 1;
    }
    return count;
}

fn findExistingFileNode(graph: *const Graph, basename: []const u8) ?usize {
    for (graph.nodes.items, 0..) |n, i| {
        if (n.kind == .file and std.mem.eql(u8, n.name, basename)) {
            return i;
        }
    }
    return null;
}

fn addEdgeIfNew(graph: *Graph, source_id: NodeId, target_id: NodeId, edge_type: EdgeType, edge_source: EdgeSource) !void {
    for (graph.edges.items) |e| {
        if (e.source_id == source_id and e.target_id == target_id and e.edge_type == edge_type) return;
    }
    _ = try graph.addEdge(.{
        .source_id = source_id,
        .target_id = target_id,
        .edge_type = edge_type,
        .source = edge_source,
    });
}

fn isDescendantOf(graph: *const Graph, node_id: NodeId, ancestor_id: NodeId) bool {
    var current = node_id;
    var hops: usize = 0;
    while (hops < 100) : (hops += 1) {
        const node = graph.getNode(current) orelse return false;
        const pid = node.parent_id orelse return false;
        if (pid == ancestor_id) return true;
        current = pid;
    }
    return false;
}

fn isIdentChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_';
}

fn matchKeyword(text: []const u8, keyword: []const u8) bool {
    if (text.len < keyword.len) return false;
    if (!std.mem.startsWith(u8, text, keyword)) return false;
    if (text.len > keyword.len and isIdentChar(text[keyword.len])) return false;
    return true;
}

fn extractLineRange(source: []const u8, line_start: u32, line_end: u32) []const u8 {
    if (source.len == 0) return source;
    var current_line: u32 = 1;
    var start_byte: usize = 0;
    var found_start: bool = (line_start <= 1);

    for (source, 0..) |c, i| {
        if (c == '\n') {
            if (found_start and current_line >= line_end) {
                return source[start_byte .. i + 1];
            }
            current_line += 1;
            if (!found_start and current_line >= line_start) {
                start_byte = i + 1;
                found_start = true;
            }
        }
    }

    if (found_start) return source[start_byte..];
    return source;
}

fn computeMetricsForNodes(graph: *Graph, source: []const u8, file_idx: usize) void {
    const file_id: NodeId = @enumFromInt(file_idx);
    var idx: usize = file_idx;
    while (idx < graph.nodes.items.len) : (idx += 1) {
        var n = &graph.nodes.items[idx];
        if (n.kind != .function) continue;
        if (!isDescendantOf(graph, @enumFromInt(idx), file_id)) continue;
        const ls = n.line_start orelse continue;
        const le = n.line_end orelse continue;

        const lines: u32 = le - ls + 1;
        const fn_source = extractLineRange(source, ls, le);

        var complexity: u16 = 1; // base complexity
        var pos: usize = 0;
        while (pos < fn_source.len) : (pos += 1) {
            if (pos > 0 and isIdentChar(fn_source[pos - 1])) continue;
            const remaining = fn_source[pos..];
            if (matchKeyword(remaining, "if") or
                matchKeyword(remaining, "for") or
                matchKeyword(remaining, "while") or
                matchKeyword(remaining, "switch") or
                matchKeyword(remaining, "catch"))
            {
                complexity += 1;
            }
        }

        n.metrics = Metrics{ .complexity = complexity, .lines = lines };
    }
}

fn countLinesUpTo(source: []const u8, byte_pos: usize) u32 {
    var line: u32 = 1;
    const end = @min(byte_pos, source.len);
    for (source[0..end]) |c| {
        if (c == '\n') line += 1;
    }
    return line;
}

fn findContainingFunction(graph: *const Graph, file_id: NodeId, line: u32) ?NodeId {
    for (graph.nodes.items, 0..) |n, i| {
        if (n.kind != .function) continue;
        if (!isDescendantOf(graph, @enumFromInt(i), file_id)) continue;
        const ls = n.line_start orelse continue;
        const le = n.line_end orelse continue;
        if (line >= ls and line <= le) return @enumFromInt(i);
    }
    return null;
}

fn resolveStdPhantoms(
    graph: *Graph,
    source: []const u8,
    file_idx: usize,
    phantom: *PhantomManager,
    std_name: []const u8,
) !void {
    const file_id: NodeId = @enumFromInt(file_idx);

    var pos: usize = 0;
    while (pos < source.len) {
        // Look for std_name followed by '.'
        if (pos + std_name.len < source.len and
            std.mem.startsWith(u8, source[pos..], std_name) and
            source[pos + std_name.len] == '.')
        {
            // Check word boundary before.
            if (pos > 0 and isIdentChar(source[pos - 1])) {
                pos += 1;
                continue;
            }

            // Collect the full chain: std.X.Y.Z
            const chain_start = pos;
            pos += std_name.len + 1; // skip "std."
            while (pos < source.len and (isIdentChar(source[pos]) or source[pos] == '.')) {
                pos += 1;
            }
            // Remove trailing dot if any.
            var chain_end = pos;
            if (chain_end > chain_start and source[chain_end - 1] == '.') {
                chain_end -= 1;
            }

            const chain = source[chain_start..chain_end];

            // Skip if just "std" with no further segments.
            if (chain.len <= std_name.len + 1) continue;

            // Determine the kind of the leaf segment.
            const last_dot = std.mem.lastIndexOfScalar(u8, chain, '.') orelse continue;
            const leaf = chain[last_dot + 1 ..];
            if (leaf.len == 0) continue;

            const kind: NodeKind = if (leaf[0] >= 'A' and leaf[0] <= 'Z') .type_def else .module;

            const phantom_id = try phantom.getOrCreate(chain, kind);

            // For type references (PascalCase leaf), create uses_type edge from
            // the containing function.
            if (kind == .type_def) {
                const ref_line = countLinesUpTo(source, chain_start);
                if (findContainingFunction(graph, file_id, ref_line)) |fn_id| {
                    try addEdgeIfNew(graph, fn_id, phantom_id, .uses_type, .phantom);
                }
            }
        } else {
            pos += 1;
        }
    }
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
        if (n.kind == kind and std.mem.eql(u8, n.name, name) and isDescendantOf(graph, nid, ancestor_id)) return nid;
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

test "Self resolves to own struct not first struct in graph" {
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

    // Assert: beta's init should have a uses_type edge to beta's Self, not alpha's
    const alpha_file = findNode(&g, "alpha.zig", .file) orelse return error.TestExpectedEqual;
    const beta_file = findNode(&g, "beta.zig", .file) orelse return error.TestExpectedEqual;
    const alpha_self = findNodeInFile(&g, "Self", .constant, alpha_file.id);
    const beta_self = findNodeInFile(&g, "Self", .constant, beta_file.id);
    const beta_init = findNodeInFile(&g, "init", .function, beta_file.id) orelse return error.TestExpectedEqual;

    // beta's init must not have a uses_type edge to alpha's Self
    if (alpha_self) |a_self| {
        try std.testing.expect(!hasEdge(&g, beta_init, a_self, .uses_type));
    }

    // If beta's init has any uses_type edge to a Self, it must be beta's Self
    if (beta_self) |b_self| {
        // Check if there is any uses_type edge from beta_init to any Self node
        var has_self_edge = false;
        for (g.edges.items) |e| {
            if (e.source_id == beta_init and e.edge_type == .uses_type) {
                const target = g.getNode(e.target_id) orelse continue;
                if (std.mem.eql(u8, target.name, "Self")) {
                    has_self_edge = true;
                    // It must point to beta's Self
                    try std.testing.expectEqual(b_self, e.target_id);
                }
            }
        }
        // There should be at least one Self reference from init
        try std.testing.expect(has_self_edge);
    }
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
