const std = @import("std");
const graph_mod = @import("../../core/graph.zig");
const phantom_mod = @import("../../core/phantom.zig");
const source_scan = @import("../../parser/source_scan.zig");
const parse_context = @import("parse_context.zig");
const build_parser = @import("build_parser.zig");
const logging = @import("../../logging.zig");
const types = @import("../../core/types.zig");
const lang = @import("../language.zig");

const Graph = graph_mod.Graph;
const PhantomManager = phantom_mod.PhantomManager;
const GraphIndex = @import("../../core/graph_index.zig").GraphIndex;
const NodeId = types.NodeId;
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

/// Parse build.zig and build.zig.zon from `project_root` and extract
/// module declarations and dependency information.
///
/// Returns an empty `BuildConfig` if the files are missing or unreadable.
/// The caller owns the returned data via `allocator`.
pub fn parseBuildConfig(allocator: std.mem.Allocator, project_root: []const u8, log: Logger) error{OutOfMemory}!BuildConfig {
    const info = build_parser.parseBuildFiles(allocator, project_root, log) catch return .{};
    defer {
        // Free fields we do not transfer to BuildConfig.
        if (info.targets) |targets| {
            for (targets) |t| {
                allocator.free(t.name);
                if (t.root_module_var) |rmv| allocator.free(rmv);
            }
            allocator.free(targets);
        }
        if (info.dependencies) |dependencies| {
            for (dependencies) |d| {
                allocator.free(d.name);
                if (d.var_name) |vn| allocator.free(vn);
            }
            allocator.free(dependencies);
        }
    }

    // Convert modules: transfer name and root_source_file ownership.
    // Import names are not used in BuildConfig, so free them here.
    var build_modules: ?[]BuildConfig.BuildModule = null;
    errdefer if (build_modules) |bm| {
        for (bm) |m| {
            allocator.free(m.name);
            if (m.root_source_file) |rsf| allocator.free(rsf);
        }
        allocator.free(bm);
    };
    if (info.modules) |modules| {
        const mods = try allocator.alloc(BuildConfig.BuildModule, modules.len);
        for (modules, 0..) |m, i| {
            mods[i] = .{
                .name = m.name,
                .root_source_file = m.root_source_file,
            };
            // Free import_names (not transferred to BuildConfig).
            if (m.import_names) |imports| {
                for (imports) |imp| allocator.free(imp);
                allocator.free(imports);
            }
        }
        allocator.free(modules);
        build_modules = mods;
    }

    // Convert dependency URLs to BuildDep.
    var build_deps: ?[]BuildConfig.BuildDep = null;
    errdefer if (build_deps) |bd| {
        for (bd) |d| {
            allocator.free(d.name);
            if (d.version) |v| allocator.free(v);
        }
        allocator.free(bd);
    };
    if (info.dependency_urls) |urls| {
        const deps = try allocator.alloc(BuildConfig.BuildDep, urls.len);
        for (urls, 0..) |du, i| {
            deps[i] = .{
                .name = du.name,
                .version = du.url,
            };
        }
        allocator.free(urls);
        build_deps = deps;
    }

    return .{
        .build_modules = build_modules,
        .build_dependencies = build_deps,
    };
}

/// Create phantom nodes and edges for external references in a single file.
///
/// Scans import_decl nodes for all non-.zig external imports, creates phantom
/// module nodes, and adds import edges from both the file node and the
/// import_decl node. Calls resolveStdPhantoms for member-level resolution.
pub fn resolvePhantoms(
    allocator: std.mem.Allocator,
    graph: *Graph,
    source: []const u8,
    file_idx: usize,
    scope_end: usize,
    phantom: *PhantomManager,
    _: *const GraphIndex,
    build_config: ?*const BuildConfig,
    log: Logger,
) error{OutOfMemory}!void {
    const file_id: NodeId = @enumFromInt(file_idx);
    const clamped_end = @min(scope_end, graph.nodes.items.len);

    // Collect: scan import_decl nodes for all external imports (read-only).
    const ImportCollect = struct {
        import_name: []const u8,
        import_decl_id: NodeId,
        import_path: []const u8,
        external: ExternalInfo,
    };
    var collected: [32]ImportCollect = undefined;
    var collect_count: usize = 0;

    for (graph.nodes.items[file_idx..clamped_end], file_idx..) |n, node_idx| {
        if (n.kind != .import_decl) continue;
        if (n.parent_id == null or n.parent_id.? != file_id) continue;
        const import_path = n.signature orelse continue;

        // Project file imports are handled via the file index, not phantoms.
        if (std.mem.endsWith(u8, import_path, ".zig")) continue;

        const external: ExternalInfo = if (std.mem.eql(u8, import_path, "std") or
            std.mem.eql(u8, import_path, "builtin"))
            .{ .stdlib = {} }
        else blk: {
            if (build_config) |bc| {
                if (bc.build_dependencies) |deps| {
                    for (deps) |dep| {
                        if (std.mem.eql(u8, import_path, dep.name)) {
                            break :blk .{ .dependency = .{ .version = dep.version } };
                        }
                    }
                }
            }
            break :blk .{ .dependency = .{ .version = null } };
        };

        if (collect_count < collected.len) {
            collected[collect_count] = .{
                .import_name = n.name,
                .import_decl_id = @enumFromInt(node_idx),
                .import_path = import_path,
                .external = external,
            };
            collect_count += 1;
        }
    }

    // Act: create phantom nodes and edges outside the scan loop.
    for (collected[0..collect_count]) |entry| {
        const phantom_id = try phantom.getOrCreate(allocator, entry.import_path, .zig, entry.external);
        _ = try graph.addEdgeIfNew(allocator, .{ .source_id = file_id, .target_id = phantom_id, .edge_type = .imports, .source = .phantom });
        _ = try graph.addEdgeIfNew(allocator, .{ .source_id = entry.import_decl_id, .target_id = phantom_id, .edge_type = .imports, .source = .phantom });
        try source_scan.resolveStdPhantoms(allocator, graph, source, file_idx, clamped_end, phantom, entry.import_name, .zig, entry.external, log);
    }
}
