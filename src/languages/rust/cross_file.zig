const std = @import("std");
const graph_mod = @import("../../core/graph.zig");
const logging = @import("../../logging.zig");
const types = @import("../../core/types.zig");
const ts = @import("tree-sitter");
const ts_api = @import("../../parser/tree_sitter_api.zig");
const source_scan = @import("../../parser/source_scan.zig");
const source_utils = @import("source_utils.zig");
const pc = @import("parse_context.zig");
const ih = @import("indexer_hooks.zig");
const impl_resolve = @import("impl_resolve.zig");
const shared_types = @import("../shared/types.zig");
const shared_resolve = @import("../shared/resolve.zig");

const Field = logging.Field;
const Logger = logging.Logger;

const Graph = graph_mod.Graph;
const NodeId = types.NodeId;
const NodeKind = types.NodeKind;
const KindIds = pc.KindIds;
const FileIndex = @import("../../core/file_index.zig").FileIndex;
const graph_index_mod = @import("../../core/graph_index.zig");
const GraphIndex = graph_index_mod.GraphIndex;
const ScopeIndex = graph_index_mod.ScopeIndex;

pub const SymbolOrigin = shared_types.SymbolOrigin;
pub const max_chain_depth = shared_types.max_chain_depth;
pub const max_ast_scan_depth = shared_types.max_ast_scan_depth;
pub const ImportEntry = shared_types.ImportEntry;
pub const EdgeContext = shared_types.EdgeContext;
pub const ResolvedEdge = shared_types.ResolvedEdge;
pub const resolveQualifiedCall = shared_resolve.resolveQualifiedCall;

/// Maximum depth for transitive re-export resolution.
const max_reexport_depth: usize = 8;

/// Result of resolving a re-export: the defining file and the chain of
/// identifier segments needed to locate the symbol within that file.
const ReExportResult = struct {
    file_id: NodeId,
    chain: [max_chain_depth][]const u8 = undefined,
    chain_len: usize = 0,
};

/// Populate the import map in ctx by scanning mod declarations from graph nodes
/// and use declarations from the tree-sitter AST. Traversing the AST directly
/// handles all Rust use-path forms including nested brace groups, glob imports,
/// and aliases within groups.
pub fn buildImportMap(
    allocator: std.mem.Allocator,
    io: std.Io,
    g: *const Graph,
    source: []const u8,
    root: ts.Node,
    ctx: *EdgeContext,
    graph_index: *const GraphIndex,
    importer_path: ?[]const u8,
    k: *const KindIds,
    log: Logger,
) !void {
    const file_index = &graph_index.files;
    const file_id: NodeId = @enumFromInt(ctx.scope_start);
    const clamped_end = @min(ctx.scope_end, g.nodes.items.len);

    // Resolve mod declarations from graph nodes (these are simple name bindings).
    for (g.nodes.items[ctx.scope_start..clamped_end]) |n| {
        if (n.kind != .import_decl) continue;
        if (n.parent_id == null or n.parent_id.? != file_id) continue;
        const sig = n.signature orelse continue;

        if (std.mem.eql(u8, n.name, sig) and !std.mem.startsWith(u8, sig, "use ") and !std.mem.startsWith(u8, sig, "pub use ")) {
            if (resolveModTarget(file_index, importer_path, n.name)) |target_id| {
                try ctx.imports.append(allocator, .{ .name = n.name, .target = target_id });
                log.trace("import map: mod resolved", &.{
                    Field.string("name", n.name),
                });
            } else {
                log.trace("import map: mod target not found", &.{
                    Field.string("name", n.name),
                });
            }
        }
    }

    // Resolve use declarations by traversing the tree-sitter AST.
    var i: u32 = 0;
    while (i < root.childCount()) : (i += 1) {
        const child = root.child(i) orelse continue;
        if (child.kindId() != k.use_declaration) continue;

        // The use_declaration's payload is its second named child (after optional visibility).
        var is_public = false;
        var ci: u32 = 0;
        while (ci < child.namedChildCount()) : (ci += 1) {
            const payload = child.namedChild(ci) orelse continue;
            const kid = payload.kindId();
            if (kid == k.visibility_modifier) {
                is_public = true;
                continue;
            }
            try resolveUseNode(allocator, io, g, source, payload, &.{}, is_public, ctx, graph_index, importer_path, k, log, 0);
            break;
        }
    }

    // Bind "super" to the parent module file for inline super:: qualified calls.
    if (resolveParentFile(file_index, importer_path)) |parent_id| {
        try ctx.imports.append(allocator, .{ .name = "super", .target = parent_id });
        log.trace("import map: super resolved", &.{});
    }

    // Resolve deferred glob imports. All explicit imports were processed above,
    // so skipping names already in the map gives explicit imports priority over
    // glob-sourced ones (matching Rust shadowing semantics).
    const scope_index = &graph_index.scope;
    for (ctx.glob_targets.items) |glob| {
        for (scope_index.childrenOf(glob.target)) |child_idx| {
            const n = g.nodes.items[child_idx];
            if (n.visibility != .public) continue;
            if (n.kind == .import_decl) continue;
            if (n.name.len == 0) continue;
            if (ctx.findImportTarget(n.name) != null) continue;
            var entry = ImportEntry{ .name = n.name, .target = glob.target, .is_reexport = glob.is_public };
            entry.chain[0] = n.name;
            entry.chain_len = 1;
            try ctx.imports.append(allocator, entry);
        }
    }
    if (ctx.glob_targets.items.len > 0) {
        log.trace("import map: resolved glob imports", &.{
            Field.uint("count", ctx.glob_targets.items.len),
        });
    }
}

/// Recursively resolve a use-path AST node into import map entries. The `prefix`
/// accumulates path segments from outer scoped_use_list nodes as the recursion
/// descends into nested groups.
fn resolveUseNode(
    allocator: std.mem.Allocator,
    io: std.Io,
    g: *const Graph,
    source: []const u8,
    node: ts.Node,
    prefix: []const []const u8,
    is_public: bool,
    ctx: *EdgeContext,
    graph_index: *const GraphIndex,
    importer_path: ?[]const u8,
    k: *const KindIds,
    log: Logger,
    depth: u32,
) !void {
    if (depth >= max_ast_scan_depth) return;
    const kid = node.kindId();

    if (kid == k.scoped_identifier or kid == k.identifier or kid == k.type_identifier) {
        var segments: [max_chain_depth][]const u8 = undefined;
        var seg_count = copyPrefix(prefix, &segments);
        collectScopedSegments(source, node, &segments, &seg_count, k, 0);
        if (seg_count == 0) return;
        try resolveAndAddEntry(allocator, io, g, ctx, graph_index, importer_path, segments[0..seg_count], segments[seg_count - 1], is_public, log);
    } else if (kid == k.scoped_use_list) {
        var new_prefix: [max_chain_depth][]const u8 = undefined;
        var pcount = copyPrefix(prefix, &new_prefix);

        // Collect prefix segments from non-use_list children, recurse into use_list.
        var ci: u32 = 0;
        while (ci < node.namedChildCount()) : (ci += 1) {
            const child = node.namedChild(ci) orelse continue;
            if (child.kindId() == k.use_list) {
                var mi: u32 = 0;
                while (mi < child.namedChildCount()) : (mi += 1) {
                    const member = child.namedChild(mi) orelse continue;
                    try resolveUseNode(allocator, io, g, source, member, new_prefix[0..pcount], is_public, ctx, graph_index, importer_path, k, log, depth + 1);
                }
            } else {
                collectScopedSegments(source, child, &new_prefix, &pcount, k, 0);
            }
        }
    } else if (kid == k.use_as_clause) {
        // AST shape: namedChild(0) = path, namedChild(1) = alias identifier.
        var segments: [max_chain_depth][]const u8 = undefined;
        var seg_count = copyPrefix(prefix, &segments);
        if (node.namedChild(0)) |path_node| {
            collectScopedSegments(source, path_node, &segments, &seg_count, k, 0);
        }
        if (seg_count == 0) return;
        const alias: ?[]const u8 = if (node.namedChild(1)) |alias_node| ts_api.nodeText(source, alias_node) else null;
        const binding = alias orelse segments[seg_count - 1];
        try resolveAndAddEntry(allocator, io, g, ctx, graph_index, importer_path, segments[0..seg_count], binding, is_public, log);
    } else if (kid == k.use_wildcard) {
        // The use_wildcard node contains the path prefix in its named children.
        // Combine with the recursion prefix to get the full module path.
        var full_prefix: [max_chain_depth][]const u8 = undefined;
        var pcount = copyPrefix(prefix, &full_prefix);
        var ci: u32 = 0;
        while (ci < node.namedChildCount()) : (ci += 1) {
            const wchild = node.namedChild(ci) orelse continue;
            collectScopedSegments(source, wchild, &full_prefix, &pcount, k, 0);
        }

        if (pcount == 0) return;

        var work: []const []const u8 = full_prefix[0..pcount];
        if (work.len > 0 and (std.mem.eql(u8, work[0], "crate") or std.mem.eql(u8, work[0], "self"))) {
            work = work[1..];
        }

        // Resolve the root target: super goes to parent file, anything else
        // looks up a module by name.
        const root_id: NodeId = if (work.len > 0 and std.mem.eql(u8, work[0], "super")) blk: {
            work = work[1..];
            break :blk resolveParentFile(&graph_index.files, importer_path) orelse {
                log.trace("import map: glob parent not found", &.{});
                return;
            };
        } else if (work.len > 0) blk: {
            const module_name = work[0];
            work = work[1..];
            break :blk ctx.findImportTarget(module_name) orelse
                graph_index.files.findByName(module_name) orelse
                resolveModuleByConvention(&graph_index.files, module_name) orelse {
                log.trace("import map: glob module not found", &.{
                    Field.string("module", module_name),
                });
                return;
            };
        } else {
            log.trace("import map: skipping bare glob import", &.{});
            return;
        };

        const target = walkScopePath(g, &graph_index.scope, root_id, work) orelse {
            log.trace("import map: glob inner scope not found", &.{});
            return;
        };
        try ctx.glob_targets.append(allocator, .{ .target = target, .is_public = is_public });
    }
}

/// Copy prefix segments into a fixed buffer. Returns the count written.
fn copyPrefix(prefix: []const []const u8, dst: *[max_chain_depth][]const u8) usize {
    const n = @min(prefix.len, max_chain_depth);
    for (prefix[0..n], 0..n) |seg, i| dst[i] = seg;
    return n;
}

/// Walk a chain of name segments through scope children starting from root_id.
/// Returns the final scope node if every segment matched, null if any step failed.
/// An empty path returns root_id unchanged.
fn walkScopePath(g: *const Graph, scope_index: *const ScopeIndex, root_id: NodeId, path: []const []const u8) ?NodeId {
    var scope_id = root_id;
    for (path) |seg| {
        var found: ?NodeId = null;
        for (scope_index.childrenOf(scope_id)) |child_idx| {
            const n = g.nodes.items[child_idx];
            if (std.mem.eql(u8, n.name, seg)) {
                found = @enumFromInt(child_idx);
                break;
            }
        }
        scope_id = found orelse return null;
    }
    return scope_id;
}

/// Normalize a resolved segment array and add it to the import map. Handles
/// crate::, self::, and super:: prefixes, then resolves the first module
/// segment to a target file.
fn resolveAndAddEntry(
    allocator: std.mem.Allocator,
    io: std.Io,
    g: *const Graph,
    ctx: *EdgeContext,
    graph_index: *const GraphIndex,
    importer_path: ?[]const u8,
    segments: []const []const u8,
    binding_name: []const u8,
    is_public: bool,
    log: Logger,
) !void {
    const file_index = &graph_index.files;
    if (segments.len == 0) return;

    var work = segments;
    const is_super = std.mem.eql(u8, work[0], "super");
    const is_crate = std.mem.eql(u8, work[0], "crate");
    const is_self = std.mem.eql(u8, work[0], "self");

    if (is_crate or is_self) {
        work = work[1..];
        if (work.len == 0) return;
    }

    if (is_super) {
        work = work[1..];
        if (work.len == 0) return;

        const parent_id = resolveParentFile(file_index, importer_path) orelse {
            log.trace("use super: parent file not found", &.{});
            return;
        };

        var entry = ImportEntry{ .name = binding_name, .target = parent_id, .is_reexport = is_public };
        const chain_len = @min(work.len, max_chain_depth);
        for (work[0..chain_len], 0..) |seg, ci| {
            entry.chain[ci] = seg;
        }
        entry.chain_len = chain_len;
        try ctx.imports.append(allocator, entry);
        log.trace("import map: use super resolved", &.{
            Field.string("binding", binding_name),
        });
        return;
    }

    if (work.len < 2) return;

    const module_name = work[0];
    const target_file_id = ctx.findImportTarget(module_name) orelse
        file_index.findByName(module_name) orelse
        resolveModuleByConvention(file_index, module_name) orelse {
        log.trace("use: module not found", &.{
            Field.string("module", module_name),
        });
        return;
    };

    // Try re-export resolution for the symbol.
    const symbol_name = work[work.len - 1];
    const reexport = resolveReExport(io, g, target_file_id, symbol_name, graph_index, log, 0);

    if (reexport) |re| {
        var entry = ImportEntry{ .name = binding_name, .target = re.file_id, .is_reexport = is_public };
        const copy_len = @min(re.chain_len, max_chain_depth);
        for (re.chain[0..copy_len], 0..) |seg, ci| {
            entry.chain[ci] = seg;
        }
        entry.chain_len = copy_len;
        try ctx.imports.append(allocator, entry);
        log.trace("import map: use resolved via re-export", &.{
            Field.string("binding", binding_name),
        });
    } else {
        var entry = ImportEntry{ .name = binding_name, .target = target_file_id, .is_reexport = is_public };
        const chain_start = work[1..];
        const chain_len = @min(chain_start.len, max_chain_depth);
        for (chain_start[0..chain_len], 0..) |seg, ci| {
            entry.chain[ci] = seg;
        }
        entry.chain_len = chain_len;
        try ctx.imports.append(allocator, entry);
        log.trace("import map: use resolved", &.{
            Field.string("binding", binding_name),
        });
    }
}

/// Resolve a mod declaration to a target file using the FileIndex.
fn resolveModTarget(file_index: *const FileIndex, importer_path: ?[]const u8, mod_name: []const u8) ?NodeId {
    if (importer_path) |ip| {
        var buf: [std.fs.max_path_bytes]u8 = undefined;
        var ci: usize = 0;
        while (ci < 2) : (ci += 1) {
            const resolved = ih.resolveImportPath(&buf, ip, mod_name, ci) orelse break;
            if (file_index.findByName(resolved)) |id| return id;
        }
    }
    return null;
}

/// Find the parent file for super:: resolution. For mod.rs files the parent
/// is one directory up; for regular files the parent is in the same directory.
/// Tries mod.rs, lib.rs, main.rs, and the named-file pattern (<dir>.rs).
fn resolveParentFile(file_index: *const FileIndex, importer_path: ?[]const u8) ?NodeId {
    const ip = importer_path orelse return null;

    const last_slash = std.mem.lastIndexOfScalar(u8, ip, '/');
    const dir = if (last_slash) |s| ip[0..s] else "";
    const basename = if (last_slash) |s| ip[s + 1 ..] else ip;

    // For mod.rs the parent module lives one directory up.
    const search_dir: []const u8 = if (std.mem.eql(u8, basename, "mod.rs"))
        if (last_slash != null)
            dir[0..(std.mem.lastIndexOfScalar(u8, dir, '/') orelse return null)]
        else
            return null
    else
        dir;

    var buf: [std.fs.max_path_bytes]u8 = undefined;

    // Root-level files have empty search_dir; try bare names directly.
    if (search_dir.len == 0) {
        const bare_candidates = [_][]const u8{ "mod.rs", "lib.rs", "main.rs" };
        for (bare_candidates) |name| {
            if (file_index.findByName(name)) |id| return id;
        }
        return null;
    }

    const candidates = [_][]const u8{ "/mod.rs", "/lib.rs", "/main.rs" };
    for (candidates) |suffix| {
        const needed = search_dir.len + suffix.len;
        if (needed > buf.len) continue;
        @memcpy(buf[0..search_dir.len], search_dir);
        @memcpy(buf[search_dir.len..][0..suffix.len], suffix);
        if (file_index.findByName(buf[0..needed])) |id| return id;
    }

    // Named-file pattern: <search_dir>.rs instead of <search_dir>/mod.rs.
    const named_len = search_dir.len + 3;
    if (named_len <= buf.len) {
        @memcpy(buf[0..search_dir.len], search_dir);
        @memcpy(buf[search_dir.len..][0..3], ".rs");
        if (file_index.findByName(buf[0..named_len])) |id| return id;
    }

    return null;
}

/// Try standard Rust module file conventions for a module name that wasn't found
/// via local mod declarations. Handles crate-root-relative imports where the
/// importing file doesn't declare the module itself.
fn resolveModuleByConvention(file_index: *const FileIndex, module_name: []const u8) ?NodeId {
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const suffixes = [_][]const u8{ "/mod.rs", ".rs", "/lib.rs" };
    for (suffixes) |suffix| {
        const needed = module_name.len + suffix.len;
        if (needed > buf.len) continue;
        @memcpy(buf[0..module_name.len], module_name);
        @memcpy(buf[module_name.len..][0..suffix.len], suffix);
        if (file_index.findByName(buf[0..needed])) |id| return id;
    }
    return null;
}

/// Emit exports edges for pub use declarations that resolved to in-project nodes.
/// Iterates tagged import entries so that all use-path forms are handled uniformly,
/// including brace groups, aliases, and nested groups.
pub fn buildExportEdges(
    allocator: std.mem.Allocator,
    g: *Graph,
    ctx: *const EdgeContext,
    graph_index: *const GraphIndex,
    log: Logger,
) !void {
    const file_id: NodeId = @enumFromInt(ctx.scope_start);
    const scope_index = &graph_index.scope;

    for (ctx.imports.items) |*entry| {
        if (!entry.is_reexport) continue;
        if (entry.chain_len == 0) continue;
        const target = walkScopePath(g, scope_index, entry.target, entry.chain[0..entry.chain_len]) orelse continue;
        const target_node = g.getNode(target) orelse continue;
        if (target_node.kind == .file or target_node.kind == .directory or target_node.kind == .module) continue;
        _ = try g.addEdgeIfNew(allocator, .{
            .source_id = file_id,
            .target_id = target,
            .edge_type = .exports,
        });
        log.trace("export edge emitted", &.{Field.string("binding", entry.name)});
    }
}

/// Check if a module file re-exports a symbol via pub use. Scans the file's
/// import_decl children for re-export declarations and follows transitive
/// chains up to max_reexport_depth hops. Returns the defining file and the
/// remaining chain segments needed to locate the symbol within that file.
fn resolveReExport(
    io: std.Io,
    g: *const Graph,
    module_file_id: NodeId,
    symbol_name: []const u8,
    graph_index: *const GraphIndex,
    log: Logger,
    depth: usize,
) ?ReExportResult {
    if (depth >= max_reexport_depth) {
        log.warn("re-export chain exceeded max depth", &.{
            Field.string("symbol", symbol_name),
            Field.uint("depth", depth),
        });
        return null;
    }

    const scope_index = &graph_index.scope;
    const file_index = &graph_index.files;
    const file_idx = @intFromEnum(module_file_id);
    if (file_idx >= g.nodes.items.len) return null;
    const file_node = g.nodes.items[file_idx];
    const file_path = file_node.file_path orelse return null;

    for (scope_index.childrenOf(module_file_id)) |child_idx| {
        const n = g.nodes.items[child_idx];
        if (n.kind != .import_decl) continue;
        if (n.visibility != .public) continue;
        const sig = n.signature orelse continue;

        const span = source_utils.extractUsePath(sig) orelse continue;
        const reexport_path = span.path;
        const pe = span.end;

        // Brace-delimited group re-export: use balanced matching for nested groups.
        if (pe < sig.len and sig[pe] == '{') {
            const brace_end = source_utils.findMatchingBrace(sig, pe) orelse continue;
            if (impl_resolve.findTraitInGroup(sig[pe + 1 .. brace_end], symbol_name)) {
                const submod = if (std.mem.endsWith(u8, reexport_path, "::"))
                    reexport_path[0 .. reexport_path.len - 2]
                else
                    reexport_path;
                if (resolveModTarget(file_index, file_path, submod)) |target| {
                    if (resolveReExport(io, g, target, symbol_name, graph_index, log, depth + 1)) |deeper| {
                        return deeper;
                    }
                    var result = ReExportResult{ .file_id = target };
                    result.chain[0] = symbol_name;
                    result.chain_len = 1;
                    return result;
                }
            }
            continue;
        }

        // Simple single-symbol re-export.
        var reex_segs: [max_chain_depth][]const u8 = undefined;
        var reex_count: usize = 0;
        var seg_iter = std.mem.splitSequence(u8, reexport_path, "::");
        while (seg_iter.next()) |seg| {
            if (reex_count >= max_chain_depth) break;
            if (seg.len == 0) continue;
            reex_segs[reex_count] = seg;
            reex_count += 1;
        }
        if (reex_count < 2) continue;

        const alias = source_utils.extractAlias(sig, pe);
        const exported_name = alias orelse reex_segs[reex_count - 1];

        if (!std.mem.eql(u8, exported_name, symbol_name)) continue;

        if (resolveModTarget(file_index, file_path, reex_segs[0])) |target| {
            if (resolveReExport(io, g, target, symbol_name, graph_index, log, depth + 1)) |deeper| {
                return deeper;
            }
            // Return current level with remaining chain (segments after the module name).
            var result = ReExportResult{ .file_id = target };
            const remaining = reex_segs[1..reex_count];
            const copy_len = @min(remaining.len, max_chain_depth);
            for (remaining[0..copy_len], 0..) |seg, ci| {
                result.chain[ci] = seg;
            }
            result.chain_len = copy_len;
            return result;
        }
    }

    return null;
}

/// Check whether a let_declaration's initializer is rooted in a module-qualified expression.
/// Scans the declaration's children for scoped_identifier or field_expression whose root
/// matches a known import name in ctx.
/// Returns the target file NodeId if found, null otherwise.
pub fn findImportQualifiedRoot(
    source: []const u8,
    let_node: ts.Node,
    ctx: *const EdgeContext,
    k: *const KindIds,
) ?NodeId {
    var i: u32 = 0;
    while (i < let_node.childCount()) : (i += 1) {
        const child = let_node.child(i) orelse continue;
        if (extractExpressionImportRoot(source, child, ctx, k, 0)) |target| return target;
    }
    return null;
}

/// Recursively extract the root import target from an expression node.
/// Checks scoped_identifier and call_expression for module-qualified roots.
fn extractExpressionImportRoot(
    source: []const u8,
    node: ts.Node,
    ctx: *const EdgeContext,
    k: *const KindIds,
    depth: u32,
) ?NodeId {
    if (depth >= max_ast_scan_depth) return null;
    const kid = node.kindId();

    if (kid == k.scoped_identifier) {
        var segments: [max_chain_depth][]const u8 = undefined;
        var seg_count: usize = 0;
        collectScopedSegments(source, node, &segments, &seg_count, k, 0);
        if (seg_count > 0) {
            return ctx.findImportTarget(segments[0]);
        }
    }
    if (kid == k.call_expression) {
        if (node.child(0)) |fn_ref| {
            return extractExpressionImportRoot(source, fn_ref, ctx, k, depth + 1);
        }
    }
    if (kid == k.field_expression) {
        if (node.namedChild(0)) |obj| {
            return extractExpressionImportRoot(source, obj, ctx, k, depth + 1);
        }
    }

    var i: u32 = 0;
    while (i < node.namedChildCount()) : (i += 1) {
        const child = node.namedChild(i) orelse continue;
        if (extractExpressionImportRoot(source, child, ctx, k, depth + 1)) |target| return target;
    }
    return null;
}

/// Recursively collect identifier segments from a scoped_identifier or plain
/// identifier AST node. Handles all Rust path keywords: super, crate, self.
pub fn collectScopedSegments(source: []const u8, node: ts.Node, segments: *[max_chain_depth][]const u8, count: *usize, k: *const KindIds, depth: u32) void {
    if (depth >= max_chain_depth) return;
    const nkid = node.kindId();
    if (nkid == k.identifier or nkid == k.type_identifier) {
        if (count.* < max_chain_depth) {
            segments[count.*] = ts_api.nodeText(source, node);
            count.* += 1;
        }
        return;
    }
    if (nkid == k.kw_super or nkid == k.kw_crate or nkid == k.self_expr) {
        if (count.* < max_chain_depth) {
            segments[count.*] = if (nkid == k.kw_super) "super" else if (nkid == k.kw_crate) "crate" else "self";
            count.* += 1;
        }
        return;
    }
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        const kid = child.kindId();

        if (kid == k.scoped_identifier) {
            collectScopedSegments(source, child, segments, count, k, depth + 1);
        } else if (kid == k.identifier or kid == k.type_identifier) {
            if (count.* < max_chain_depth) {
                segments[count.*] = ts_api.nodeText(source, child);
                count.* += 1;
            }
        } else if (kid == k.kw_super) {
            if (count.* < max_chain_depth) {
                segments[count.*] = "super";
                count.* += 1;
            }
        } else if (kid == k.kw_crate) {
            if (count.* < max_chain_depth) {
                segments[count.*] = "crate";
                count.* += 1;
            }
        } else if (kid == k.self_expr) {
            if (count.* < max_chain_depth) {
                segments[count.*] = "self";
                count.* += 1;
            }
        }
    }
}

/// Resolve a function's return type to a type node in the graph.
/// Parses the return type from the function's stored signature text by
/// finding the `->` token, stripping Result/Option wrappers, references,
/// and pointer markers. Handles module-qualified types by resolving the
/// import in the containing file.
/// Returns the type's NodeId for use as scope in further chain resolution.
pub fn resolveReturnTypeScope(g: *const Graph, fn_id: NodeId, graph_index: *const GraphIndex) ?NodeId {
    const scope_index = &graph_index.scope;
    const fn_node = g.getNode(fn_id) orelse return null;
    const sig = fn_node.signature orelse return null;

    // Extract return type: text after "->" in the signature.
    const arrow_pos = std.mem.indexOf(u8, sig, "->") orelse return null;
    if (arrow_pos + 2 >= sig.len) return null;
    var return_text = std.mem.trim(u8, sig[arrow_pos + 2 ..], " \t\n\r");
    if (return_text.len == 0) return null;

    // Strip reference markers (&, &mut).
    while (return_text.len > 0 and return_text[0] == '&') {
        return_text = return_text[1..];
        return_text = std.mem.trimStart(u8, return_text, " \t\n\r");
        if (std.mem.startsWith(u8, return_text, "mut ")) {
            return_text = return_text[4..];
            return_text = std.mem.trimStart(u8, return_text, " \t\n\r");
        }
    }

    // Strip Result<T, E> or Option<T> wrappers to extract the inner type.
    return_text = unwrapGenericWrapper(return_text);

    // Strip pointer markers (* const, * mut).
    while (return_text.len > 0 and return_text[0] == '*') {
        return_text = return_text[1..];
        return_text = std.mem.trimStart(u8, return_text, " \t\n\r");
        if (std.mem.startsWith(u8, return_text, "const ")) {
            return_text = return_text[6..];
        } else if (std.mem.startsWith(u8, return_text, "mut ")) {
            return_text = return_text[4..];
        }
        return_text = std.mem.trimStart(u8, return_text, " \t\n\r");
    }

    return_text = std.mem.trim(u8, return_text, " \t\n\r");
    if (return_text.len == 0) return null;

    // Split on "::" to get segments.
    var segments: [max_chain_depth][]const u8 = undefined;
    var seg_count: usize = 0;
    var iter = std.mem.splitSequence(u8, return_text, "::");
    while (iter.next()) |seg| {
        if (seg_count >= max_chain_depth) break;
        var s = std.mem.trim(u8, seg, " \t\n\r");
        // Trim angle bracket suffixes from generic types.
        if (std.mem.indexOfScalar(u8, s, '<')) |lt| {
            s = s[0..lt];
        }
        var end: usize = 0;
        while (end < s.len and source_scan.isIdentChar(s[end])) : (end += 1) {}
        if (end == 0) continue;
        segments[seg_count] = s[0..end];
        seg_count += 1;
    }
    if (seg_count == 0) return null;

    // Module-qualified return type: resolve the import prefix then the type name.
    if (seg_count >= 2) {
        const fn_file_id = g.findContainingFile(fn_id) orelse return null;
        const target_file_id = findImportInFile(g, fn_file_id, segments[0], graph_index) orelse return null;
        return g.findTypeAmongChildren(scope_index.childrenOf(target_file_id), segments[seg_count - 1]);
    }

    // Bare type name: look among siblings in the same scope.
    const fn_parent = fn_node.parent_id orelse return null;
    return g.findTypeAmongChildren(scope_index.childrenOf(fn_parent), segments[0]);
}

/// Strip Result<T, E> or Option<T> wrappers, returning the inner type text.
fn unwrapGenericWrapper(text: []const u8) []const u8 {
    const wrappers = [_][]const u8{ "Result", "Option", "Box", "Vec", "Arc", "Rc" };
    for (wrappers) |wrapper| {
        if (!std.mem.startsWith(u8, text, wrapper)) continue;
        if (wrapper.len >= text.len) continue;
        if (text[wrapper.len] != '<') continue;
        // Find matching '>'.
        var depth: usize = 0;
        var pos: usize = wrapper.len;
        while (pos < text.len) : (pos += 1) {
            if (text[pos] == '<') {
                depth += 1;
            } else if (text[pos] == '>') {
                depth -= 1;
                if (depth == 0) break;
            }
        }
        // Extract the first type parameter.
        const inner_start = wrapper.len + 1;
        const inner_end = pos;
        if (inner_start >= inner_end) continue;
        const inner = text[inner_start..inner_end];
        // For Result<T, E>, take only T (before the comma).
        if (std.mem.indexOfScalar(u8, inner, ',')) |comma| {
            return std.mem.trim(u8, inner[0..comma], " \t\n\r");
        }
        return std.mem.trim(u8, inner, " \t\n\r");
    }
    return text;
}

/// Find an import_decl child of a file node that matches the given name,
/// and return the target file's NodeId by resolving the module path.
fn findImportInFile(g: *const Graph, file_id: NodeId, import_name: []const u8, graph_index: *const GraphIndex) ?NodeId {
    const scope_index = &graph_index.scope;
    const file_index = &graph_index.files;
    const file_node = g.getNode(file_id) orelse return null;
    const importer_path = file_node.file_path orelse return null;

    for (scope_index.childrenOf(file_id)) |child_idx| {
        const n = g.nodes.items[child_idx];
        if (n.kind != .import_decl) continue;
        if (!std.mem.eql(u8, n.name, import_name)) continue;
        if (resolveModTarget(file_index, importer_path, n.name)) |target_id| {
            return target_id;
        }
    }
    return null;
}

/// Resolve a variable's target file through the return type of its initializer.
/// For a variable assigned from a module-qualified function call, extracts the
/// full chain, walks it in the target file to locate the called function, then
/// resolves that function's return type to find the file containing the result type.
pub fn resolveVarTargetThroughReturnType(
    g: *const Graph,
    source: []const u8,
    let_node: ts.Node,
    ctx: *const EdgeContext,
    k: *const KindIds,
    graph_index: *const GraphIndex,
    log: Logger,
) ?NodeId {
    const scope_index = &graph_index.scope;
    // Extract the full chain from the assignment expression.
    var chain: [max_chain_depth][]const u8 = undefined;
    var chain_len: usize = 0;

    var i: u32 = 0;
    while (i < let_node.childCount()) : (i += 1) {
        const c = let_node.child(i) orelse continue;
        const ck = c.kindId();
        if (ck == k.call_expression) {
            // Unwrap call_expression to reach the function reference.
            if (c.child(0)) |fn_ref| {
                const fk = fn_ref.kindId();
                if (fk == k.scoped_identifier) {
                    collectScopedSegments(source, fn_ref, &chain, &chain_len, k, 0);
                } else if (fk == k.field_expression) {
                    collectFieldChainForVar(source, fn_ref, &chain, &chain_len, k, 0);
                }
            }
            break;
        }
        if (ck == k.scoped_identifier) {
            collectScopedSegments(source, c, &chain, &chain_len, k, 0);
            break;
        }
        if (ck == k.field_expression) {
            collectFieldChainForVar(source, c, &chain, &chain_len, k, 0);
            break;
        }
    }

    if (chain_len == 0) {
        log.trace("var target: chain extraction failed", &.{});
        return null;
    }

    // Look up the origin for the root name (includes extraction chain).
    const origin = ctx.findImportOrigin(chain[0]) orelse return null;

    // Build effective chain: origin.chain ++ chain[1..chain_len].
    var effective: [max_chain_depth][]const u8 = undefined;
    var eff_len: usize = 0;
    for (origin.chain) |seg| {
        if (eff_len >= max_chain_depth) break;
        effective[eff_len] = seg;
        eff_len += 1;
    }
    for (chain[1..chain_len]) |seg| {
        if (eff_len >= max_chain_depth) break;
        effective[eff_len] = seg;
        eff_len += 1;
    }

    if (eff_len == 0) return null;

    // Walk effective chain to find the function node in the target file.
    var scope_id = origin.file_id;
    var last_fn_id: ?NodeId = null;

    for (effective[0..eff_len]) |segment| {
        var matched: ?NodeId = null;
        for (scope_index.childrenOf(scope_id)) |child_idx| {
            const n = g.nodes.items[child_idx];
            if (!std.mem.eql(u8, n.name, segment)) continue;
            matched = @enumFromInt(child_idx);
            break;
        }
        if (matched == null) {
            if (std.mem.eql(u8, segment, "Self")) continue;
            return null;
        }
        const node = g.getNode(matched.?) orelse return null;
        if (node.kind == .function) {
            last_fn_id = matched;
        }
        if (node.kind.isTypeContainer()) {
            scope_id = matched.?;
        } else {
            scope_id = node.parent_id orelse return null;
        }
    }

    const fn_id = last_fn_id orelse return null;

    // Resolve the function's return type to a type node.
    const return_type_id = resolveReturnTypeScope(g, fn_id, graph_index) orelse return null;

    // Return the file containing the return type.
    return g.findContainingFile(return_type_id);
}

/// Collect identifier segments from a Rust field_expression chain for variable resolution.
/// Handles nested field_expression and call_expression wrappers.
pub fn collectFieldChainForVar(source: []const u8, node: ts.Node, chain: *[max_chain_depth][]const u8, count: *usize, k: *const KindIds, depth: u32) void {
    if (depth >= max_chain_depth) return;
    const kid = node.kindId();
    if (kid == k.identifier or kid == k.type_identifier) {
        if (count.* < max_chain_depth) {
            chain[count.*] = ts_api.nodeText(source, node);
            count.* += 1;
        }
        return;
    }
    if (kid == k.scoped_identifier) {
        collectScopedSegments(source, node, chain, count, k, 0);
        return;
    }
    if (kid == k.field_expression) {
        if (node.namedChild(0)) |obj| {
            collectFieldChainForVar(source, obj, chain, count, k, depth + 1);
        }
        const nc = node.namedChildCount();
        if (nc >= 2) {
            if (node.namedChild(nc - 1)) |field| {
                collectFieldChainForVar(source, field, chain, count, k, depth + 1);
            }
        }
        return;
    }
    if (kid == k.call_expression) {
        if (node.child(0)) |fn_ref| {
            collectFieldChainForVar(source, fn_ref, chain, count, k, depth + 1);
        }
        return;
    }
}
