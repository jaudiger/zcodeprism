const std = @import("std");
const graph_mod = @import("../../core/graph.zig");
const phantom_mod = @import("../../core/phantom.zig");
const source_scan = @import("../../parser/source_scan.zig");
const parse_context = @import("parse_context.zig");
const logging = @import("../../logging.zig");
const types = @import("../../core/types.zig");
const lang = @import("../language.zig");

const Graph = graph_mod.Graph;
const PhantomManager = phantom_mod.PhantomManager;
const NodeId = types.NodeId;
const NodeKind = types.NodeKind;
const Language = types.Language;
const ImportEntry = lang.ImportEntry;
const ImportKind = lang.ImportKind;
const ExternalInfo = lang.ExternalInfo;
const BuildConfig = lang.BuildConfig;
const Logger = logging.Logger;

/// Scan source text for `@import("...")` patterns and classify each import.
///
/// Writes results into the caller-provided `out` buffer and returns the
/// number of entries written. Entries beyond `out.len` are silently dropped.
pub fn extractImports(source: []const u8, out: []ImportEntry) usize {
    const pattern = "@import(\"";
    var pos: usize = 0;
    var count: usize = 0;
    while (pos + pattern.len <= source.len) {
        const idx = std.mem.indexOf(u8, source[pos..], pattern) orelse break;
        const abs_idx = pos + idx;
        const path_start = abs_idx + pattern.len;
        if (path_start >= source.len) break;
        const end = std.mem.indexOfScalar(u8, source[path_start..], '"') orelse break;
        if (count < out.len) {
            const imp = source[path_start .. path_start + end];
            out[count] = .{
                .path = imp,
                .kind = classifyImport(imp),
            };
            count += 1;
        }
        pos = path_start + end + 1;
    }
    return count;
}

fn classifyImport(path: []const u8) ImportKind {
    if (std.mem.eql(u8, path, "std")) return .stdlib;
    if (std.mem.eql(u8, path, "builtin")) return .stdlib;
    if (std.mem.endsWith(u8, path, ".zig")) return .project_file;
    return .unknown;
}

/// Resolve an import path relative to the importing file's directory.
///
/// Zig imports have a single candidate, so this returns `null` when
/// `candidate_idx > 0`. The resolved path is written into `buf`.
pub fn resolveImportPath(buf: []u8, importer_path: []const u8, import_path: []const u8, candidate_idx: usize) ?[]const u8 {
    if (candidate_idx > 0) return null;
    return parse_context.resolveImportPath(buf, importer_path, import_path);
}

/// Parse `build.zig.zon` from `project_root` and extract dependency names.
///
/// Returns an empty `BuildConfig` if the file is missing or unreadable.
/// The caller owns the returned slice memory via `allocator`.
pub fn parseBuildConfig(allocator: std.mem.Allocator, project_root: []const u8, log: Logger) anyerror!BuildConfig {
    _ = log;
    var dir = std.fs.openDirAbsolute(project_root, .{}) catch return .{};
    defer dir.close();
    const file = dir.openFile("build.zig.zon", .{}) catch return .{};
    defer file.close();
    const content = file.readToEndAlloc(allocator, 1 * 1024 * 1024) catch return .{};
    defer allocator.free(content);
    return extractDependencyNames(allocator, content);
}

fn extractDependencyNames(allocator: std.mem.Allocator, content: []const u8) !BuildConfig {
    const deps_start = std.mem.indexOf(u8, content, ".dependencies") orelse return .{};
    var pos = deps_start + ".dependencies".len;

    // Find opening brace of the dependencies block.
    while (pos < content.len and content[pos] != '{') : (pos += 1) {}
    if (pos >= content.len) return .{};
    pos += 1;

    // Collect field names at depth 1 inside the dependencies block.
    var names = std.ArrayListUnmanaged([]u8){};
    errdefer {
        for (names.items) |n| allocator.free(n);
        names.deinit(allocator);
    }

    var depth: usize = 1;
    while (pos < content.len and depth > 0) : (pos += 1) {
        switch (content[pos]) {
            '{' => depth += 1,
            '}' => depth -= 1,
            '.' => if (depth == 1) {
                if (extractFieldName(content, pos + 1)) |name| {
                    try names.append(allocator, try allocator.dupe(u8, name));
                }
            },
            else => {},
        }
    }

    if (names.items.len == 0) {
        names.deinit(allocator);
        return .{};
    }

    const owned = try names.toOwnedSlice(allocator);
    return .{ .dependency_names = owned };
}

fn extractFieldName(content: []const u8, pos: usize) ?[]const u8 {
    if (pos >= content.len) return null;
    if (content[pos] == '@' and pos + 1 < content.len and content[pos + 1] == '"') {
        // .@"name" pattern
        const start = pos + 2;
        const end = std.mem.indexOfScalar(u8, content[start..], '"') orelse return null;
        return content[start .. start + end];
    }
    // .identifier pattern
    if (!isIdentStart(content[pos])) return null;
    var end = pos;
    while (end < content.len and isIdentChar(content[end])) : (end += 1) {}
    return content[pos..end];
}

fn isIdentStart(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

fn isIdentChar(c: u8) bool {
    return isIdentStart(c) or (c >= '0' and c <= '9');
}

/// Create phantom nodes and edges for external references in a single file.
///
/// Walks the graph slice `[file_idx..scope_end)` looking for `@import("std")`
/// declarations, creates the corresponding phantom module node and import
/// edge, then delegates to `source_scan.resolveStdPhantoms` for member-level
/// phantom resolution.
pub fn resolvePhantoms(
    graph: *Graph,
    source: []const u8,
    file_idx: usize,
    scope_end: usize,
    phantom: *PhantomManager,
    build_config: ?*const BuildConfig,
    log: Logger,
) anyerror!void {
    _ = build_config; // future: classify dependency phantoms

    const file_id: NodeId = @enumFromInt(file_idx);

    // Find the std import variable name in this file (usually "std").
    var std_import_name: ?[]const u8 = null;
    for (graph.nodes.items[file_idx..scope_end]) |n| {
        if (n.kind != .import_decl) continue;
        if (n.parent_id == null or n.parent_id.? != file_id) continue;
        const import_path = n.signature orelse continue;
        if (std.mem.eql(u8, import_path, "std")) {
            std_import_name = n.name;
            const std_id = try phantom.getOrCreate("std", .module, .zig, .{ .stdlib = {} });
            _ = try graph.addEdgeIfNew(.{ .source_id = file_id, .target_id = std_id, .edge_type = .imports, .source = .phantom });
            break;
        }
    }

    if (std_import_name) |sname| {
        try source_scan.resolveStdPhantoms(graph, source, file_idx, scope_end, phantom, sname, .zig, .{ .stdlib = {} }, log);
    }
}
