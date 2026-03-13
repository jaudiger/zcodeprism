const std = @import("std");
const ts = @import("tree-sitter");
const graph_mod = @import("../../core/graph.zig");
const phantom_mod = @import("../../core/phantom.zig");
const ts_api = @import("../../parser/tree_sitter_api.zig");
const parse_context = @import("parse_context.zig");
const build_parser = @import("build_parser.zig");
const logging = @import("../../logging.zig");
const types = @import("../../core/types.zig");
const lang = @import("../language.zig");
const lang_support = @import("../language_support.zig");
const lsp_client = @import("../../lsp/client.zig");

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
const LspClient = lsp_client.LspClient;
const protocol = @import("../../lsp/protocol.zig");
const worklist_mod = @import("../../lsp/worklist.zig");
const LspWorklist = worklist_mod.LspWorklist;
const WorklistEntry = worklist_mod.WorklistEntry;
const UsageSite = worklist_mod.UsageSite;
const EnrichResult = lang_support.EnrichResult;

/// A function node's ID and its inclusive 1-based line range.
const FnRange = struct { id: NodeId, line_start: u32, line_end: u32 };

/// Parse source with tree-sitter and extract all @import paths from the AST.
///
/// Writes results into `out` and returns the count of entries written.
pub fn extractImports(source: []const u8, ts_language: *const ts.Language, out: []ImportEntry) usize {
    const tree = ts_api.parseSource(ts_language, source) orelse return 0;
    defer tree.destroy();

    const k = parse_context.KindIds.init(ts_language);
    const root = tree.rootNode();
    var count: usize = 0;

    var i: u32 = 0;
    while (i < root.namedChildCount()) : (i += 1) {
        if (count >= out.len) break;
        const child = root.namedChild(i) orelse continue;
        const imp = findImportInNode(source, child, k) orelse continue;
        out[count] = .{
            .path = imp,
            .kind = classifyImport(imp),
        };
        count += 1;
    }
    return count;
}

/// Recursively search a subtree for a builtin_function that is @import,
/// returning the string argument path.
fn findImportInNode(source: []const u8, node: ts.Node, k: parse_context.KindIds) ?[]const u8 {
    if (node.kindId() == k.builtin_function) {
        return extractImportPath(source, node, k);
    }
    var ci: u32 = 0;
    while (ci < node.namedChildCount()) : (ci += 1) {
        const c = node.namedChild(ci) orelse continue;
        if (findImportInNode(source, c, k)) |path| return path;
    }
    return null;
}

/// Extract the string literal argument from a builtin_function node
/// whose builtin_identifier is @import.
fn extractImportPath(source: []const u8, builtin_node: ts.Node, k: parse_context.KindIds) ?[]const u8 {
    var ci: u32 = 0;
    var is_import = false;
    while (ci < builtin_node.childCount()) : (ci += 1) {
        const c = builtin_node.child(ci) orelse continue;
        if (c.kindId() == k.builtin_identifier) {
            is_import = std.mem.eql(u8, ts_api.nodeText(source, c), "@import");
            break;
        }
    }
    if (!is_import) return null;
    return findStringContent(source, builtin_node, k);
}

/// Recursively find the first string_content node under the given parent.
fn findStringContent(source: []const u8, node: ts.Node, k: parse_context.KindIds) ?[]const u8 {
    var ci: u32 = 0;
    while (ci < node.childCount()) : (ci += 1) {
        const c = node.child(ci) orelse continue;
        if (c.kindId() == k.string_content) return ts_api.nodeText(source, c);
        if (findStringContent(source, c, k)) |s| return s;
    }
    return null;
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
/// import_decl node. Records a usage site for each module-level phantom from
/// the import_decl's stored position. Calls resolveStdPhantoms for member-level resolution.
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
    const file_path: []const u8 = graph.nodes.items[file_idx].file_path orelse "";

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

        // Record this import_decl's position as the canonical usage site for the module phantom.
        // graph nodes store 1-based line numbers; LSP positions are 0-based.
        const decl_node = graph.nodes.items[@intFromEnum(entry.import_decl_id)];
        if (decl_node.line_start) |ls| {
            try phantom.recordUsageSite(allocator, phantom_id, .{
                .file_path = file_path,
                .line = if (ls > 0) ls - 1 else 0,
                .col = decl_node.col_start orelse 0,
                .hint_name = entry.import_path,
            });
        }

        try resolveStdPhantoms(allocator, graph, source, file_idx, clamped_end, phantom, entry.import_name, .zig, entry.external, file_path, log);
    }
}

/// Dispatch worklist entries to LSP queries, confirm dead-code candidates
/// with a targeted references pass, and fill in signatures and docs for
/// phantom nodes by querying hover at their usage sites.
pub fn enrichWithLsp(
    allocator: std.mem.Allocator,
    graph: *Graph,
    client: *LspClient,
    wl: *const LspWorklist,
    logger: Logger,
) error{OutOfMemory}!EnrichResult {
    var result = EnrichResult{};
    result.worklist_total = wl.count();

    var file_map = try buildFileNodeMap(allocator, graph);
    defer file_map.deinit(allocator);

    try dispatchWorklist(allocator, graph, client, wl.items(), &file_map, &result, logger);
    try runDeadCodeReferencesPass(allocator, graph, client, &file_map, &result, logger);
    try enrichPhantoms(allocator, graph, client, wl.phantomHovers(), &result, logger);

    return result;
}

/// Send each worklist entry to the appropriate LSP method and integrate
/// the result as a new graph edge or node metadata.
fn dispatchWorklist(
    allocator: std.mem.Allocator,
    graph: *Graph,
    client: *LspClient,
    worklist: []const WorklistEntry,
    file_map: *const std.StringHashMapUnmanaged(NodeId),
    result: *EnrichResult,
    logger: Logger,
) error{OutOfMemory}!void {
    for (worklist) |entry| {
        const abs_path = std.fs.path.join(allocator, &.{ graph.project_root, entry.file_path }) catch continue;
        defer allocator.free(abs_path);
        const uri = lsp_client.pathToUri(allocator, abs_path) catch continue;
        defer allocator.free(uri);

        switch (entry.query_kind) {
            .definition => {
                result.definition_queries += 1;
                const locs = (client.textDocumentDefinition(allocator, uri, entry.line, entry.col) catch continue) orelse continue;
                defer protocol.freeLocationArray(allocator, locs);
                for (locs) |loc| {
                    const rel = resolveDefinitionToRelPath(loc.uri, graph.project_root) orelse continue;
                    const file_node_id = file_map.get(rel) orelse continue;
                    const target_id = findDeclarationAtLine(graph, file_node_id, loc.range.start.line);
                    if (target_id == file_node_id) continue;
                    const added = graph.addEdgeIfNew(allocator, .{
                        .source_id = entry.source_node_id,
                        .target_id = target_id,
                        .edge_type = .calls,
                        .source = .lsp,
                    }) catch return error.OutOfMemory;
                    if (added) {
                        result.edges_promoted += 1;
                        result.definition_successes += 1;
                        result.worklist_resolved += 1;
                        logger.debug("promoted call edge via definition", &.{
                            logging.Field.string("hint", entry.hint_name orelse "?"),
                            logging.Field.string("target", graph.nodes.items[@intFromEnum(target_id)].name),
                        });
                    }
                    break;
                }
            },
            .type_definition => {
                result.type_definition_queries += 1;
                const locs = (client.textDocumentTypeDefinition(allocator, uri, entry.line, entry.col) catch continue) orelse continue;
                defer protocol.freeLocationArray(allocator, locs);
                for (locs) |loc| {
                    const rel = resolveDefinitionToRelPath(loc.uri, graph.project_root) orelse continue;
                    const file_node_id = file_map.get(rel) orelse continue;
                    const target_id = findDeclarationAtLine(graph, file_node_id, loc.range.start.line);
                    if (target_id == file_node_id) continue;
                    const added = graph.addEdgeIfNew(allocator, .{
                        .source_id = entry.source_node_id,
                        .target_id = target_id,
                        .edge_type = .uses_type,
                        .source = .lsp,
                    }) catch return error.OutOfMemory;
                    if (added) {
                        result.edges_added += 1;
                        result.type_definition_successes += 1;
                        result.worklist_resolved += 1;
                    }
                    break;
                }
            },
            .hover => {
                const src_idx = @intFromEnum(entry.source_node_id);
                if (src_idx >= graph.nodes.items.len) continue;
                result.hover_queries += 1;
                const hover = (client.textDocumentHover(allocator, uri, entry.line, entry.col) catch {
                    logger.debug("hover query failed", &.{logging.Field.string("hint", entry.hint_name orelse "?")});
                    continue;
                }) orelse continue;
                defer protocol.freeHover(allocator, hover);
                const hover_text = switch (hover.contents) {
                    .markup => |m| m.value,
                    .plain_string => |s| s,
                };
                const parsed = parseErrorsFromHover(allocator, hover_text) catch continue;
                const names = parsed orelse continue;
                try graph.addOwnedBuffer(allocator, names.flat_buf);
                try graph.addOwnedSlice(allocator, []const u8, names.slices);
                var zm = if (graph.nodes.items[src_idx].lang_meta == .zig) graph.nodes.items[src_idx].lang_meta.zig else @import("meta.zig").ZigMeta{};
                zm.inferred_errors = names.slices;
                graph.nodes.items[src_idx].lang_meta = .{ .zig = zm };
                result.errors_inferred += 1;
                result.hover_successes += 1;
                result.worklist_resolved += 1;
            },
            .references => {
                result.reference_queries += 1;
                const locs = (client.textDocumentReferences(allocator, uri, entry.line, entry.col, false) catch continue) orelse continue;
                defer protocol.freeLocationArray(allocator, locs);
                var resolved_any = false;
                for (locs) |loc| {
                    const rel = resolveDefinitionToRelPath(loc.uri, graph.project_root) orelse continue;
                    const file_node_id = file_map.get(rel) orelse continue;
                    const ref_id = findDeclarationAtLine(graph, file_node_id, loc.range.start.line);
                    if (ref_id == file_node_id) continue;
                    const added = graph.addEdgeIfNew(allocator, .{
                        .source_id = ref_id,
                        .target_id = entry.source_node_id,
                        .edge_type = .calls,
                        .source = .lsp,
                    }) catch return error.OutOfMemory;
                    if (added) {
                        result.edges_added += 1;
                        resolved_any = true;
                    }
                }
                if (resolved_any) {
                    result.reference_successes += 1;
                    result.worklist_resolved += 1;
                }
            },
        }
    }
}

/// Query textDocument/references for every private function, constant, and
/// type node with no inbound fan edges. Adds a calls or uses_type edge for
/// each reference site that maps to a known graph node. Counts queries and
/// successes in result.
fn runDeadCodeReferencesPass(
    allocator: std.mem.Allocator,
    graph: *Graph,
    client: *LspClient,
    file_map: *const std.StringHashMapUnmanaged(NodeId),
    result: *EnrichResult,
    logger: Logger,
) error{OutOfMemory}!void {
    // Build inbound fan-edge counts from the full edge list.
    var inbound: std.AutoHashMapUnmanaged(NodeId, u32) = .{};
    defer inbound.deinit(allocator);
    for (graph.edges.items) |e| {
        if (e.edge_type != .calls and e.edge_type != .uses_type) continue;
        const gop = try inbound.getOrPut(allocator, e.target_id);
        if (!gop.found_existing) gop.value_ptr.* = 0;
        gop.value_ptr.* += 1;
    }

    for (graph.nodes.items) |node| {
        switch (node.kind) {
            .function, .constant, .type_def, .enum_def, .union_def => {},
            else => continue,
        }
        if (node.visibility != .private) continue;
        if (node.external != .none) continue;
        const line_start = node.line_start orelse continue;
        const col_start = node.col_start orelse 0;
        const file_path = node.file_path orelse continue;
        if ((inbound.get(node.id) orelse 0) > 0) continue;

        const abs_path = std.fs.path.join(allocator, &.{ graph.project_root, file_path }) catch continue;
        defer allocator.free(abs_path);
        const uri = lsp_client.pathToUri(allocator, abs_path) catch continue;
        defer allocator.free(uri);

        result.reference_queries += 1;
        // line_start is 1-based; LSP positions are 0-based.
        const locs = (client.textDocumentReferences(allocator, uri, line_start - 1, col_start, false) catch continue) orelse continue;
        defer protocol.freeLocationArray(allocator, locs);

        // Functions are called; constants and types are referenced as types.
        const edge_type: types.EdgeType = if (node.kind == .function) .calls else .uses_type;

        var resolved_any = false;
        for (locs) |loc| {
            const rel = resolveDefinitionToRelPath(loc.uri, graph.project_root) orelse continue;
            const file_node_id = file_map.get(rel) orelse continue;
            const ref_id = findDeclarationAtLine(graph, file_node_id, loc.range.start.line);
            if (ref_id == file_node_id) continue;
            if (ref_id == node.id) continue;
            const added = graph.addEdgeIfNew(allocator, .{
                .source_id = ref_id,
                .target_id = node.id,
                .edge_type = edge_type,
                .source = .lsp,
            }) catch return error.OutOfMemory;
            if (added) {
                result.edges_added += 1;
                resolved_any = true;
            }
        }
        if (resolved_any) {
            result.reference_successes += 1;
            logger.debug("confirmed live node via references", &.{
                logging.Field.string("name", node.name),
            });
        }
    }
}

/// Build a map from relative file paths to file node IDs.
fn buildFileNodeMap(allocator: std.mem.Allocator, graph: *const Graph) error{OutOfMemory}!std.StringHashMapUnmanaged(NodeId) {
    var map: std.StringHashMapUnmanaged(NodeId) = .{};
    errdefer map.deinit(allocator);
    for (graph.nodes.items, 0..) |n, i| {
        if (n.kind != .file) continue;
        const fp = n.file_path orelse continue;
        try map.put(allocator, fp, @enumFromInt(i));
    }
    return map;
}

/// Resolve a definition URI to a relative path within the project, or null
/// if the definition points outside the project.
fn resolveDefinitionToRelPath(uri: []const u8, project_root: []const u8) ?[]const u8 {
    const abs = lsp_client.uriToPath(uri) orelse return null;
    if (!std.mem.startsWith(u8, abs, project_root)) return null;
    var rel = abs[project_root.len..];
    if (rel.len > 0 and rel[0] == '/') rel = rel[1..];
    return rel;
}

/// Find the most specific (narrowest line span) declaration node within the
/// file at `file_node_id` that contains `def_line` (0-based, from LSP).
/// Returns the file node itself when no declaration spans the line.
fn findDeclarationAtLine(graph: *const Graph, file_node_id: NodeId, def_line: u32) NodeId {
    const graph_line: u32 = def_line + 1; // convert 0-based LSP to 1-based graph
    const file_idx = @intFromEnum(file_node_id);
    const file_path = graph.nodes.items[file_idx].file_path orelse return file_node_id;

    var best: NodeId = file_node_id;
    var best_span: u32 = std.math.maxInt(u32);

    for (graph.nodes.items[file_idx + 1 ..]) |candidate| {
        const fp = candidate.file_path orelse break;
        if (fp.ptr != file_path.ptr or fp.len != file_path.len) break;
        const ls = candidate.line_start orelse continue;
        const le = candidate.line_end orelse continue;
        if (le < ls) continue;
        if (ls <= graph_line and graph_line <= le) {
            const span = le - ls;
            if (span < best_span) {
                best_span = span;
                best = candidate.id;
            }
        }
    }

    return best;
}

/// Walk up from a node to the topmost field_expression ancestor,
/// stopping at non-field_expression boundaries.
fn topmostFieldExpr(node: ts.Node, k: *const parse_context.KindIds) ?ts.Node {
    var current = node.parent() orelse return null;
    if (current.kindId() != k.field_expression) return null;
    while (true) {
        const parent = current.parent() orelse return current;
        if (parent.kindId() == k.field_expression) {
            current = parent;
        } else {
            return current;
        }
    }
}

/// Find the function whose line range contains the given 1-based line.
fn findFnByLine(ranges: []const FnRange, line: u32) ?NodeId {
    for (ranges) |r| {
        if (line >= r.line_start and line <= r.line_end) return r.id;
    }
    return null;
}

/// Recursive AST walk that creates phantom nodes for field_expression chains
/// rooted at `std_name` and records the first usage site for each phantom.
fn walkForStdRefs(
    allocator: std.mem.Allocator,
    graph: *Graph,
    source: []const u8,
    node: ts.Node,
    k: *const parse_context.KindIds,
    std_name: []const u8,
    language: Language,
    external: ExternalInfo,
    phantom: *PhantomManager,
    fn_ranges: []const FnRange,
    file_path: []const u8,
) !void {
    const kid = node.kindId();

    if (kid == k.comment or kid == k.string) return;

    if (kid == k.identifier) {
        const text = ts_api.nodeText(source, node);
        if (std.mem.eql(u8, text, std_name)) {
            const top = topmostFieldExpr(node, k) orelse return;
            const chain_text = ts_api.nodeText(source, top);

            if (chain_text.len <= std_name.len + 1) return;

            const last_dot = std.mem.lastIndexOfScalar(u8, chain_text, '.') orelse return;
            const leaf = chain_text[last_dot + 1 ..];
            if (leaf.len == 0) return;

            const is_type = leaf[0] >= 'A' and leaf[0] <= 'Z';
            const phantom_id = try phantom.getOrCreate(allocator, chain_text, language, external);

            try phantom.recordUsageSite(allocator, phantom_id, .{
                .file_path = file_path,
                .line = node.startPoint().row,
                .col = node.startPoint().column,
                .hint_name = chain_text,
            });

            if (is_type) {
                const line: u32 = node.startPoint().row + 1;
                if (findFnByLine(fn_ranges, line)) |fn_id| {
                    _ = try graph.addEdgeIfNew(allocator, .{
                        .source_id = fn_id,
                        .target_id = phantom_id,
                        .edge_type = .uses_type,
                        .source = .phantom,
                    });
                }
            }
        }
        return;
    }

    var ci: u32 = 0;
    while (ci < node.childCount()) : (ci += 1) {
        const child = node.child(ci) orelse continue;
        try walkForStdRefs(allocator, graph, source, child, k, std_name, language, external, phantom, fn_ranges, file_path);
    }
}

/// Scan source AST for field_expression chains starting with `std_name`
/// and create phantom nodes for each unique qualified reference. PascalCase
/// leaf segments get a uses_type edge from the enclosing function.
fn resolveStdPhantoms(
    allocator: std.mem.Allocator,
    graph: *Graph,
    source: []const u8,
    file_idx: usize,
    file_end_idx: usize,
    phantom: *PhantomManager,
    std_name: []const u8,
    language: Language,
    external: ExternalInfo,
    file_path: []const u8,
    _: Logger,
) !void {
    const ts_language = ts_api.tree_sitter_zig();
    const tree = ts_api.parseSource(ts_language, source) orelse return;
    defer tree.destroy();

    const k = parse_context.KindIds.init(ts_language);

    const end = @min(file_end_idx, graph.nodes.items.len);
    var fn_ranges = std.ArrayList(FnRange){};
    defer fn_ranges.deinit(allocator);
    for (graph.nodes.items[file_idx..end], file_idx..) |n, idx| {
        if (n.kind != .function) continue;
        const ls = n.line_start orelse continue;
        const le = n.line_end orelse continue;
        try fn_ranges.append(allocator, .{ .id = @enumFromInt(idx), .line_start = ls, .line_end = le });
    }

    try walkForStdRefs(allocator, graph, source, tree.rootNode(), &k, std_name, language, external, phantom, fn_ranges.items, file_path);
}

/// Flat-buffer + slice-array pair, same layout as error_sets.ParsedNames.
const ParsedNames = struct {
    slices: []const []const u8,
    flat_buf: []const u8,
};

/// Extract error names from LSP hover text using tree-sitter. Parses the
/// signature from the hover markdown, then walks the AST for
/// error_set_declaration nodes. Falls back to parsing a bare !ErrorName
/// when no error set is found.
fn parseErrorsFromHover(allocator: std.mem.Allocator, text: []const u8) !?ParsedNames {
    const contents = parseHoverContents(text);
    const sig = contents.signature orelse return null;

    const ts_language = ts_api.tree_sitter_zig();
    const tree = ts_api.parseSource(ts_language, sig) orelse return null;
    defer tree.destroy();
    const k = parse_context.KindIds.init(ts_language);

    if (try findErrorSetInTree(allocator, sig, tree.rootNode(), k)) |result| {
        return result;
    }

    // ZLS sometimes emits !ErrorName without a full error set declaration.
    if (std.mem.indexOf(u8, sig, "!")) |bang| {
        const after = sig[bang + 1 ..];
        if (after.len == 0 or !std.ascii.isAlphabetic(after[0])) return null;
        var name_end: usize = 0;
        while (name_end < after.len and (std.ascii.isAlphanumeric(after[name_end]) or after[name_end] == '_')) : (name_end += 1) {}
        if (name_end == 0) return null;

        const flat_buf = try allocator.alloc(u8, name_end);
        errdefer allocator.free(flat_buf);
        @memcpy(flat_buf, after[0..name_end]);

        const slices = try allocator.alloc([]const u8, 1);
        slices[0] = flat_buf;
        return .{ .slices = slices, .flat_buf = flat_buf };
    }

    return null;
}

/// Recursive AST walk that returns the first error_set_declaration's
/// identifier children as a ParsedNames pair.
fn findErrorSetInTree(
    allocator: std.mem.Allocator,
    source: []const u8,
    node: ts.Node,
    k: parse_context.KindIds,
) !?ParsedNames {
    if (node.kindId() == k.error_set_declaration) {
        return try parseNamesFromErrorSetNode(allocator, source, node, k);
    }
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        if (try findErrorSetInTree(allocator, source, child, k)) |result| {
            return result;
        }
    }
    return null;
}

/// Extract identifier children from an error_set_declaration node into
/// a flat-buffer pair using MAF.
fn parseNamesFromErrorSetNode(
    allocator: std.mem.Allocator,
    source: []const u8,
    node: ts.Node,
    k: parse_context.KindIds,
) !?ParsedNames {
    // Measure: count identifier children and total text length.
    var count: usize = 0;
    var flat_len: usize = 0;
    var ci: u32 = 0;
    while (ci < node.namedChildCount()) : (ci += 1) {
        const child = node.namedChild(ci) orelse continue;
        if (child.kindId() != k.identifier) continue;
        const text = ts_api.nodeText(source, child);
        count += 1;
        flat_len += text.len;
    }
    if (count == 0) return null;

    // Allocate.
    const flat_buf = try allocator.alloc(u8, flat_len);
    errdefer allocator.free(flat_buf);
    const slices = try allocator.alloc([]const u8, count);
    errdefer allocator.free(slices);

    // Fill.
    var pos: usize = 0;
    var si: usize = 0;
    ci = 0;
    while (ci < node.namedChildCount()) : (ci += 1) {
        const child = node.namedChild(ci) orelse continue;
        if (child.kindId() != k.identifier) continue;
        const text = ts_api.nodeText(source, child);
        @memcpy(flat_buf[pos..][0..text.len], text);
        slices[si] = flat_buf[pos..][0..text.len];
        pos += text.len;
        si += 1;
    }
    std.debug.assert(pos == flat_len);
    std.debug.assert(si == count);

    return .{ .slices = slices, .flat_buf = flat_buf };
}

/// Hover text split into signature and doc sub-slices of the input.
/// Both fields borrow from the input text and carry no owned memory.
const HoverContents = struct {
    signature: ?[]const u8,
    doc: ?[]const u8,
};

/// Extract signature and doc from markdown hover text. Signature is the
/// content of the first ``` code fence; doc is the text between the
/// closing fence and any subsequent fence. Without a code fence the
/// trimmed full text becomes signature and doc is null.
fn parseHoverContents(text: []const u8) HoverContents {
    const fence = "```";

    // No code fence at all: the whole trimmed text is the signature.
    const open = std.mem.indexOf(u8, text, fence) orelse {
        const sig = std.mem.trim(u8, text, " \t\n\r");
        return .{ .signature = if (sig.len > 0) sig else null, .doc = null };
    };

    // Skip the language hint on the opening fence line.
    const after_fence = open + fence.len;
    const line_end = std.mem.indexOfScalarPos(u8, text, after_fence, '\n') orelse text.len;
    const code_start = if (line_end < text.len) line_end + 1 else line_end;

    // Missing closing fence: treat remaining text as signature.
    const close = std.mem.indexOfPos(u8, text, code_start, fence) orelse {
        const sig = std.mem.trim(u8, text[code_start..], " \t\n\r");
        return .{ .signature = if (sig.len > 0) sig else null, .doc = null };
    };

    const sig_raw = std.mem.trim(u8, text[code_start..close], " \t\n\r");
    const after_close = close + fence.len;

    // Trim doc at the next fence to avoid contamination from subsequent code blocks.
    const doc_region = if (after_close < text.len) text[after_close..] else "";
    const doc_trimmed = if (std.mem.indexOf(u8, doc_region, fence)) |next_fence|
        std.mem.trim(u8, doc_region[0..next_fence], " \t\n\r")
    else
        std.mem.trim(u8, doc_region, " \t\n\r");

    return .{
        .signature = if (sig_raw.len > 0) sig_raw else null,
        .doc = if (doc_trimmed.len > 0) doc_trimmed else null,
    };
}

/// Query hover for each phantom node using the pre-recorded usage site
/// from the phantom_hovers slice. Each entry carries the exact phantom
/// NodeId and its AST-derived file position. Stores extracted signature
/// and doc on the corresponding graph node.
fn enrichPhantoms(
    allocator: std.mem.Allocator,
    graph: *Graph,
    client: *LspClient,
    phantom_hovers: []const WorklistEntry,
    result: *EnrichResult,
    logger: Logger,
) error{OutOfMemory}!void {
    for (phantom_hovers) |entry| {
        const src_idx = @intFromEnum(entry.source_node_id);
        if (src_idx >= graph.nodes.items.len) continue;
        if (graph.nodes.items[src_idx].signature != null) continue;

        const abs_path = std.fs.path.join(
            allocator,
            &.{ graph.project_root, entry.file_path },
        ) catch continue;
        defer allocator.free(abs_path);
        const uri = lsp_client.pathToUri(allocator, abs_path) catch continue;
        defer allocator.free(uri);

        result.hover_queries += 1;
        const hover = (client.textDocumentHover(
            allocator,
            uri,
            entry.line,
            entry.col,
        ) catch {
            logger.debug("phantom hover failed", &.{
                logging.Field.string("hint", entry.hint_name orelse "?"),
            });
            result.phantoms_remaining += 1;
            continue;
        }) orelse {
            result.phantoms_remaining += 1;
            continue;
        };
        defer protocol.freeHover(allocator, hover);

        const hover_text = switch (hover.contents) {
            .markup => |m| m.value,
            .plain_string => |s| s,
        };
        const extracted = parseHoverContents(hover_text);
        var enriched = false;

        if (extracted.signature) |sig| {
            const d = try allocator.dupe(u8, sig);
            errdefer allocator.free(d);
            try graph.addOwnedBuffer(allocator, d);
            graph.nodes.items[src_idx].signature = d;
            enriched = true;
        }
        if (extracted.doc) |doc_text| {
            const d = try allocator.dupe(u8, doc_text);
            errdefer allocator.free(d);
            try graph.addOwnedBuffer(allocator, d);
            graph.nodes.items[src_idx].doc = d;
            enriched = true;
        }

        if (enriched) {
            result.phantoms_enriched += 1;
            result.hover_successes += 1;
            logger.debug("enriched phantom", &.{
                logging.Field.string("hint", entry.hint_name orelse "?"),
            });
        } else {
            result.phantoms_remaining += 1;
        }
    }
}
