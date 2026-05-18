const std = @import("std");
const ts = @import("tree-sitter");
const graph_mod = @import("../../core/graph.zig");
const phantom_mod = @import("../../core/phantom.zig");
const graph_index_mod = @import("../../core/graph_index.zig");
const source_scan = @import("../../parser/source_scan.zig");
const ts_api = @import("../../parser/tree_sitter_api.zig");
const source_utils = @import("source_utils.zig");
const impl_resolve = @import("impl_resolve.zig");
const logging = @import("../../logging.zig");
const types = @import("../../core/types.zig");
const lang = @import("../language.zig");
const lang_support = @import("../language_support.zig");
const rust_ctx = @import("parse_context.zig");
const node_mod = @import("../../core/node.zig");
const worklist_mod = @import("../../lsp/worklist.zig");
const enrich_helpers = @import("../../lsp/enrich_helpers.zig");
const lsp_client = @import("../../lsp/client.zig");
const protocol = @import("../../lsp/protocol.zig");

const Graph = graph_mod.Graph;
const PhantomManager = phantom_mod.PhantomManager;
const GraphIndex = graph_index_mod.GraphIndex;
const FileIndex = graph_index_mod.FileIndex;
const NodeId = types.NodeId;
const EdgeType = types.EdgeType;
const Language = types.Language;
const Logger = logging.Logger;
const Field = logging.Field;
const ImportEntry = lang.ImportEntry;
const ImportKind = lang.ImportKind;
const ExternalInfo = @import("../../core/external.zig").ExternalInfo;
const BuildConfig = lang.BuildConfig;
const Node = node_mod.Node;
const UsageSite = phantom_mod.UsageSite;
const LspClient = lsp_client.LspClient;
const LspWorklist = worklist_mod.LspWorklist;
const EnrichResult = lang_support.EnrichResult;

/// Parse source with tree-sitter and extract external mod declarations from the AST.
///
/// Writes results into `out` and returns the count of entries written.
pub fn extractImports(source: []const u8, ts_language: *const ts.Language, out: []ImportEntry) usize {
    const tree = ts_api.parseSource(ts_language, source) orelse return 0;
    defer tree.destroy();

    const k = rust_ctx.KindIds.init(ts_language);
    const root = tree.rootNode();
    var count: usize = 0;

    var i: u32 = 0;
    while (i < root.namedChildCount()) : (i += 1) {
        if (count >= out.len) break;
        const child = root.namedChild(i) orelse continue;
        if (child.kindId() != k.mod_item) continue;
        // Skip inline modules (they have a declaration_list body).
        if (hasChildOfKind(child, k.declaration_list)) continue;
        const name = extractIdentifier(source, child, k) orelse continue;
        out[count] = .{
            .path = name,
            .kind = .project_file,
        };
        count += 1;
    }
    return count;
}

fn hasChildOfKind(node: ts.Node, kind_id: u16) bool {
    var ci: u32 = 0;
    while (ci < node.namedChildCount()) : (ci += 1) {
        const c = node.namedChild(ci) orelse continue;
        if (c.kindId() == kind_id) return true;
    }
    return false;
}

/// Return the text of the first identifier child.
fn extractIdentifier(source: []const u8, node: ts.Node, k: rust_ctx.KindIds) ?[]const u8 {
    var ci: u32 = 0;
    while (ci < node.namedChildCount()) : (ci += 1) {
        const c = node.namedChild(ci) orelse continue;
        if (c.kindId() == k.identifier) return ts_api.nodeText(source, c);
    }
    return null;
}

/// Resolve a Rust module import path relative to the importing file.
///
/// Module directory rules:
///   If the importer is lib.rs, main.rs, or mod.rs, the module directory
///   is the importer's directory.
///   Otherwise, the module directory is a subdirectory named after the file stem.
///
/// Resolution candidates:
///   candidate 0: <module_dir>/<mod_name>.rs
///   candidate 1: <module_dir>/<mod_name>/mod.rs
/// Returns null for candidate_idx > 1.
pub fn resolveImportPath(buf: []u8, importer_path: []const u8, import_path: []const u8, candidate_idx: usize) ?[]const u8 {
    if (candidate_idx > 1) return null;

    // Determine the directory of the importing file.
    const dir = if (std.mem.lastIndexOfScalar(u8, importer_path, '/')) |slash| importer_path[0 .. slash + 1] else "";

    // Determine if this is a root module file (lib.rs, main.rs, mod.rs).
    const basename = if (std.mem.lastIndexOfScalar(u8, importer_path, '/')) |slash| importer_path[slash + 1 ..] else importer_path;

    const is_root = std.mem.eql(u8, basename, "lib.rs") or
        std.mem.eql(u8, basename, "main.rs") or
        std.mem.eql(u8, basename, "mod.rs");

    if (candidate_idx == 0) {
        // candidate 0: <module_dir>/<mod_name>.rs
        if (is_root) {
            // module_dir = importer's directory
            const needed = dir.len + import_path.len + 3; // ".rs"
            if (needed > buf.len) return null;
            var pos: usize = 0;
            @memcpy(buf[pos .. pos + dir.len], dir);
            pos += dir.len;
            @memcpy(buf[pos .. pos + import_path.len], import_path);
            pos += import_path.len;
            @memcpy(buf[pos .. pos + 3], ".rs");
            pos += 3;
            return buf[0..pos];
        } else {
            // module_dir = dir + stem + /
            const stem = if (std.mem.endsWith(u8, basename, ".rs"))
                basename[0 .. basename.len - 3]
            else
                basename;
            const needed = dir.len + stem.len + 1 + import_path.len + 3;
            if (needed > buf.len) return null;
            var pos: usize = 0;
            @memcpy(buf[pos .. pos + dir.len], dir);
            pos += dir.len;
            @memcpy(buf[pos .. pos + stem.len], stem);
            pos += stem.len;
            buf[pos] = '/';
            pos += 1;
            @memcpy(buf[pos .. pos + import_path.len], import_path);
            pos += import_path.len;
            @memcpy(buf[pos .. pos + 3], ".rs");
            pos += 3;
            return buf[0..pos];
        }
    } else {
        // candidate 1: <module_dir>/<mod_name>/mod.rs
        if (is_root) {
            const needed = dir.len + import_path.len + 7; // "/mod.rs"
            if (needed > buf.len) return null;
            var pos: usize = 0;
            @memcpy(buf[pos .. pos + dir.len], dir);
            pos += dir.len;
            @memcpy(buf[pos .. pos + import_path.len], import_path);
            pos += import_path.len;
            @memcpy(buf[pos .. pos + 7], "/mod.rs");
            pos += 7;
            return buf[0..pos];
        } else {
            const stem = if (std.mem.endsWith(u8, basename, ".rs"))
                basename[0 .. basename.len - 3]
            else
                basename;
            const needed = dir.len + stem.len + 1 + import_path.len + 7;
            if (needed > buf.len) return null;
            var pos: usize = 0;
            @memcpy(buf[pos .. pos + dir.len], dir);
            pos += dir.len;
            @memcpy(buf[pos .. pos + stem.len], stem);
            pos += stem.len;
            buf[pos] = '/';
            pos += 1;
            @memcpy(buf[pos .. pos + import_path.len], import_path);
            pos += import_path.len;
            @memcpy(buf[pos .. pos + 7], "/mod.rs");
            pos += 7;
            return buf[0..pos];
        }
    }
}

/// Returns true when the root segment of a use-path is local: either a Rust
/// path keyword (self, crate, super) or a module that resolves to an
/// in-project file. Phantoms should only be created for external crates.
fn isLocalCrate(crate_name: []const u8, importer_path: ?[]const u8, file_index: *const FileIndex) bool {
    if (std.mem.eql(u8, crate_name, "self") or
        std.mem.eql(u8, crate_name, "crate") or
        std.mem.eql(u8, crate_name, "super"))
        return true;

    var buf: [std.fs.max_path_bytes]u8 = undefined;
    if (importer_path) |ip| {
        var ci: usize = 0;
        while (ci < 2) : (ci += 1) {
            const resolved = resolveImportPath(&buf, ip, crate_name, ci) orelse break;
            if (file_index.findByName(resolved) != null) return true;
        }
    }
    const suffixes = [_][]const u8{ ".rs", "/mod.rs", "/lib.rs" };
    for (suffixes) |suffix| {
        const needed = crate_name.len + suffix.len;
        if (needed > buf.len) continue;
        @memcpy(buf[0..crate_name.len], crate_name);
        @memcpy(buf[crate_name.len..][0..suffix.len], suffix);
        if (file_index.findByName(buf[0..needed]) != null) return true;
    }
    return false;
}

/// Dispatch worklist entries to LSP queries, confirm dead-code candidates
/// with a targeted references pass, and fill in signatures and docs for
/// phantom nodes by querying hover at their usage sites.
pub fn enrichWithLsp(
    allocator: std.mem.Allocator,
    io: std.Io,
    graph: *Graph,
    client: *LspClient,
    wl: *const LspWorklist,
    logger: Logger,
) error{OutOfMemory}!EnrichResult {
    var result = EnrichResult{};
    result.worklist_total = wl.count();

    var file_map = try enrich_helpers.buildFileNodeMap(allocator, graph);
    defer file_map.deinit(allocator);

    try enrich_helpers.dispatchWorklist(allocator, io, graph, client, wl.items(), &file_map, &result, &handleRustHover, logger);
    try enrich_helpers.runDeadCodeReferencesPass(allocator, io, graph, client, &file_map, &result, logger);
    try enrich_helpers.enrichPhantoms(allocator, io, graph, client, wl.phantomHovers(), &result, logger);

    return result;
}

/// Rust-specific hover handler: extracts and stores the signature on the
/// source node. No error set inference for Rust.
fn handleRustHover(allocator: std.mem.Allocator, graph: *Graph, src_idx: usize, hover: protocol.Hover, result: *EnrichResult) error{OutOfMemory}!void {
    const hover_text = switch (hover.contents) {
        .markup => |m| m.value,
        .plain_string => |s| s,
    };
    const extracted = enrich_helpers.parseHoverContents(hover_text);
    if (extracted.signature) |sig| {
        const d = try allocator.dupe(u8, sig);
        errdefer allocator.free(d);
        try graph.addOwnedBuffer(allocator, d);
        graph.nodes.items[src_idx].signature = d;
        result.hover_successes += 1;
        result.worklist_resolved += 1;
    }
}

/// Create phantom nodes and edges for all `use` declarations in a single Rust file.
/// Stdlib paths map to the `.stdlib` external variant; all other crates map
/// to `.dependency`. Records a usage site for each phantom so the worklist
/// transfer loop in indexer.zig can build phantom hover entries.
pub fn resolvePhantoms(
    allocator: std.mem.Allocator,
    graph: *Graph,
    source: []const u8,
    file_idx: usize,
    scope_end: usize,
    phantom: *PhantomManager,
    graph_index: *const GraphIndex,
    build_config: ?*const BuildConfig,
    log: Logger,
) error{OutOfMemory}!void {
    _ = build_config;
    _ = source;

    const file_id: NodeId = @enumFromInt(file_idx);
    const file_path = graph.nodes.items[file_idx].file_path;
    const clamped_end = @min(scope_end, graph.nodes.items.len);

    // Track root crate phantoms created this call to avoid duplicate file edges.
    const max_crates = 32;
    var seen_crates: [max_crates]struct { name: []const u8, id: NodeId } = undefined;
    var crate_count: usize = 0;

    for (graph.nodes.items[file_idx..clamped_end], file_idx..) |n, node_idx| {
        if (n.kind != .import_decl) continue;
        if (n.parent_id == null or n.parent_id.? != file_id) continue;
        const sig = n.signature orelse continue;

        const span = source_utils.extractUsePath(sig) orelse continue;
        const path = span.path;

        const sep_pos = std.mem.indexOf(u8, path, "::") orelse continue;
        const crate = path[0..sep_pos];
        if (isLocalCrate(crate, file_path, &graph_index.files)) continue;

        const external: ExternalInfo = if (std.mem.eql(u8, crate, "std"))
            .{ .stdlib = {} }
        else
            .{ .dependency = .{ .version = null } };

        // 0-based LSP position derived from the import_decl node's 1-based line.
        const site = usageSiteFromNode(n, file_path, crate);

        // Ensure the root crate phantom exists and has a file-level edge.
        const import_decl_id: NodeId = @enumFromInt(node_idx);
        {
            var found = false;
            for (seen_crates[0..crate_count]) |entry| {
                if (std.mem.eql(u8, entry.name, crate)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                const crate_id = try phantom.getOrCreate(allocator, crate, .rust, external);
                _ = try graph.addEdgeIfNew(allocator, .{ .source_id = file_id, .target_id = crate_id, .edge_type = .imports, .source = .phantom });
                if (site) |s| try phantom.recordUsageSite(allocator, crate_id, s);
                if (crate_count < max_crates) {
                    seen_crates[crate_count] = .{ .name = crate, .id = crate_id };
                    crate_count += 1;
                }
            }
        }

        // Handle brace group or simple path.
        if (span.end < sig.len and sig[span.end] == '{') {
            const brace_end = source_utils.findMatchingBrace(sig, span.end) orelse continue;
            const group = sig[span.end + 1 .. brace_end];
            const common = if (std.mem.endsWith(u8, path, "::"))
                path[0 .. path.len - 2]
            else
                path;
            try expandGroupPhantoms(allocator, graph, file_id, import_decl_id, phantom, common, group, crate, external, site, log);
        } else {
            try createExternalPhantom(allocator, graph, file_id, import_decl_id, phantom, path, crate, external, site, log);
        }
    }

    try resolveScopedFieldPhantoms(allocator, graph, file_idx, clamped_end, phantom, graph_index, log);

    try impl_resolve.resolveImplementsEdges(allocator, graph, file_idx, clamped_end, phantom, graph_index);
}

/// Build a UsageSite from an import_decl node's stored position.
/// Returns null when the node has no line_start.
fn usageSiteFromNode(n: Node, file_path: ?[]const u8, hint_name: ?[]const u8) ?UsageSite {
    const ls = n.line_start orelse return null;
    return .{
        .file_path = file_path orelse return null,
        .line = if (ls > 0) ls - 1 else 0,
        .col = n.col_start orelse 0,
        .hint_name = hint_name,
    };
}

/// Expand a brace group and create phantom nodes for each member. Handles
/// nested groups recursively, plain identifiers, and "X as Y" aliases.
fn expandGroupPhantoms(
    allocator: std.mem.Allocator,
    graph: *Graph,
    file_id: NodeId,
    import_decl_id: NodeId,
    phantom: *PhantomManager,
    common_prefix: []const u8,
    group: []const u8,
    crate: []const u8,
    external: ExternalInfo,
    site: ?UsageSite,
    log: Logger,
) error{OutOfMemory}!void {
    var pos: usize = 0;
    while (pos < group.len) {
        while (pos < group.len and (group[pos] == ' ' or group[pos] == ',' or group[pos] == '\t' or group[pos] == '\n' or group[pos] == '\r')) pos += 1;
        if (pos >= group.len) break;

        if (group[pos] == '{') {
            if (source_utils.findMatchingBrace(group, pos)) |close| {
                pos = close + 1;
            } else break;
            continue;
        }

        // Read an identifier.
        const ident_start = pos;
        while (pos < group.len and source_scan.isIdentChar(group[pos])) pos += 1;
        if (pos == ident_start) {
            pos += 1;
            continue;
        }
        const ident = group[ident_start..pos];

        // Handle nested group: "ident::{...}".
        if (pos + 1 < group.len and group[pos] == ':' and group[pos + 1] == ':') {
            pos += 2;
            if (pos < group.len and group[pos] == '{') {
                if (source_utils.findMatchingBrace(group, pos)) |close| {
                    var nested_buf: [256]u8 = undefined;
                    const needed = common_prefix.len + 2 + ident.len;
                    if (needed <= nested_buf.len) {
                        @memcpy(nested_buf[0..common_prefix.len], common_prefix);
                        nested_buf[common_prefix.len] = ':';
                        nested_buf[common_prefix.len + 1] = ':';
                        @memcpy(nested_buf[common_prefix.len + 2 ..][0..ident.len], ident);
                        try expandGroupPhantoms(allocator, graph, file_id, import_decl_id, phantom, nested_buf[0..needed], group[pos + 1 .. close], crate, external, site, log);
                    }
                    pos = close + 1;
                } else break;
            }
            continue;
        }

        // Skip "as Alias" suffix (the phantom uses the original name).
        var skip_pos = pos;
        while (skip_pos < group.len and group[skip_pos] == ' ') skip_pos += 1;
        if (skip_pos + 3 <= group.len and std.mem.eql(u8, group[skip_pos..][0..3], "as ")) {
            skip_pos += 3;
            while (skip_pos < group.len and source_scan.isIdentChar(group[skip_pos])) skip_pos += 1;
            pos = skip_pos;
        }

        // Build the full path: common_prefix::ident.
        var full_buf: [256]u8 = undefined;
        const needed = common_prefix.len + 2 + ident.len;
        if (needed <= full_buf.len) {
            @memcpy(full_buf[0..common_prefix.len], common_prefix);
            full_buf[common_prefix.len] = ':';
            full_buf[common_prefix.len + 1] = ':';
            @memcpy(full_buf[common_prefix.len + 2 ..][0..ident.len], ident);
            try createExternalPhantom(allocator, graph, file_id, import_decl_id, phantom, full_buf[0..needed], crate, external, site, log);
        }
    }
}

/// Create a phantom node for a single Rust `::` path. Converts the path to
/// dot-separated form, infers the edge type from the leaf segment's case,
/// attaches edges from both the file node and the import_decl node, and
/// records the import_decl's position as the phantom's usage site.
fn createExternalPhantom(
    allocator: std.mem.Allocator,
    graph: *Graph,
    file_id: NodeId,
    import_decl_id: NodeId,
    phantom: *PhantomManager,
    path: []const u8,
    crate: []const u8,
    external: ExternalInfo,
    site: ?UsageSite,
    log: Logger,
) error{OutOfMemory}!void {
    _ = log;

    var qname_buf: [256]u8 = undefined;
    const qname = impl_resolve.rustPathToDot(path, &qname_buf) orelse return;
    if (qname.len <= crate.len) return;

    const last_dot = std.mem.lastIndexOfScalar(u8, qname, '.') orelse return;
    const leaf = qname[last_dot + 1 ..];
    if (leaf.len == 0) return;
    const is_type = leaf[0] >= 'A' and leaf[0] <= 'Z';
    const edge_type: EdgeType = if (is_type) .uses_type else .imports;

    const leaf_id = try phantom.getOrCreate(allocator, qname, .rust, external);
    if (site) |s| try phantom.recordUsageSite(allocator, leaf_id, s);

    _ = try graph.addEdgeIfNew(allocator, .{
        .source_id = file_id,
        .target_id = leaf_id,
        .edge_type = edge_type,
        .source = .phantom,
    });
    _ = try graph.addEdgeIfNew(allocator, .{
        .source_id = import_decl_id,
        .target_id = leaf_id,
        .edge_type = edge_type,
        .source = .phantom,
    });
}

/// Parse Cargo.toml at the project root and return a BuildConfig.
/// Returns an empty config if no Cargo.toml is found or parsing fails.
pub fn parseBuildConfig(
    allocator: std.mem.Allocator,
    io: std.Io,
    project_root: []const u8,
    log: Logger,
) error{OutOfMemory}!BuildConfig {
    const cargo_parser = @import("cargo_parser.zig");
    const info = cargo_parser.parseCargoManifest(allocator, io, project_root, log) catch return .{};
    defer {
        // Free fields not transferred to BuildConfig.
        if (info.dev_dependencies) |deps| {
            for (deps) |d| {
                allocator.free(d.name);
                if (d.version) |v| allocator.free(v);
            }
            allocator.free(deps);
        }
        if (info.bin_targets) |targets| {
            for (targets) |t| {
                allocator.free(t.name);
                if (t.path) |p| allocator.free(p);
            }
            allocator.free(targets);
        }
        if (info.workspace_members) |members| {
            for (members) |m| allocator.free(m);
            allocator.free(members);
        }
    }

    // Convert dependencies to BuildDeps, transferring ownership.
    var build_deps: ?[]BuildConfig.BuildDep = null;
    errdefer if (build_deps) |bd| {
        for (bd) |d| {
            allocator.free(d.name);
            if (d.version) |v| allocator.free(v);
        }
        allocator.free(bd);
    };
    if (info.dependencies) |deps| {
        const bd = try allocator.alloc(BuildConfig.BuildDep, deps.len);
        for (deps, 0..) |d, i| {
            bd[i] = .{ .name = d.name, .version = d.version };
        }
        allocator.free(deps);
        build_deps = bd;
    }

    // Convert package name to a single BuildModule, transferring ownership.
    var build_modules: ?[]BuildConfig.BuildModule = null;
    errdefer if (build_modules) |bm| {
        for (bm) |m| {
            allocator.free(m.name);
            if (m.root_source_file) |rsf| allocator.free(rsf);
        }
        allocator.free(bm);
    };
    if (info.package_name) |pkg_name| {
        const mods = try allocator.alloc(BuildConfig.BuildModule, 1);
        // Pick root_source_file from lib target or first bin target.
        var rsf: ?[]u8 = null;
        if (info.lib_target) |lib| {
            rsf = lib.path;
            if (lib.name) |n| allocator.free(n);
        } else if (info.bin_targets) |bins| {
            if (bins.len > 0 and bins[0].path != null) {
                rsf = bins[0].path;
            }
        }
        mods[0] = .{ .name = pkg_name, .root_source_file = rsf };
        build_modules = mods;
    } else {
        // No package name: free lib_target strings if present.
        if (info.lib_target) |lib| {
            if (lib.name) |n| allocator.free(n);
            if (lib.path) |p| allocator.free(p);
        }
    }
    if (info.package_version) |v| allocator.free(v);

    return .{
        .build_modules = build_modules,
        .build_dependencies = build_deps,
    };
}

const PrefixEntry = struct { name: []const u8, path: []const u8, external: ExternalInfo };

const MAX_SCOPED_PREFIXES: usize = 64;

fn collectModulePrefixImports(
    graph: *const Graph,
    file_idx: usize,
    clamped_end: usize,
    graph_index: *const GraphIndex,
    importer_path: ?[]const u8,
    prefix_buf: []PrefixEntry,
) usize {
    const file_id: NodeId = @enumFromInt(file_idx);
    var count: usize = 0;

    for (graph.nodes.items[file_idx..clamped_end]) |n| {
        if (n.kind != .import_decl) continue;
        if (n.parent_id == null or n.parent_id.? != file_id) continue;
        const sig = n.signature orelse continue;

        const span = source_utils.extractUsePath(sig) orelse continue;
        const path = span.path;

        const sep_pos = std.mem.indexOf(u8, path, "::") orelse continue;
        const crate = path[0..sep_pos];
        if (isLocalCrate(crate, importer_path, &graph_index.files)) continue;

        const last_sep = std.mem.lastIndexOf(u8, path, "::") orelse continue;
        const terminal = path[last_sep + 2 ..];
        if (terminal.len == 0) continue;
        if (terminal[0] >= 'A' and terminal[0] <= 'Z') continue;

        const external: ExternalInfo = if (std.mem.eql(u8, crate, "std"))
            .{ .stdlib = {} }
        else
            .{ .dependency = .{ .version = null } };

        if (count < prefix_buf.len) {
            prefix_buf[count] = .{ .name = terminal, .path = path, .external = external };
            count += 1;
        }
    }
    return count;
}

fn walkToOwningType(graph: *const Graph, start_id: NodeId) NodeId {
    var owner_id = start_id;
    var hops: usize = 0;
    while (hops < 10) : (hops += 1) {
        const owner = graph.getNode(owner_id) orelse break;
        if (owner.kind == .type_def or owner.kind == .enum_def or owner.kind == .union_def) break;
        owner_id = owner.parent_id orelse break;
    }
    return owner_id;
}

fn resolveOneScopedField(
    allocator: std.mem.Allocator,
    graph: *Graph,
    phantom: *PhantomManager,
    field_node: node_mod.Node,
    prefixes: []const PrefixEntry,
    importer_path: ?[]const u8,
) error{OutOfMemory}!void {
    const sig = field_node.signature orelse return;

    const colon_pos = std.mem.indexOf(u8, sig, "::") orelse return;
    if (colon_pos == 0) return;
    const field_prefix = sig[0..colon_pos];

    const remainder = sig[colon_pos + 2 ..];
    if (remainder.len == 0) return;
    var name_end: usize = 0;
    while (name_end < remainder.len and source_scan.isIdentChar(remainder[name_end])) : (name_end += 1) {}
    if (name_end == 0) return;
    const type_name = remainder[0..name_end];
    if (type_name[0] < 'A' or type_name[0] > 'Z') return;

    for (prefixes) |entry| {
        if (!std.mem.eql(u8, field_prefix, entry.name)) continue;

        var full_buf: [256]u8 = undefined;
        const needed = entry.path.len + 2 + type_name.len;
        if (needed > full_buf.len) continue;
        @memcpy(full_buf[0..entry.path.len], entry.path);
        full_buf[entry.path.len] = ':';
        full_buf[entry.path.len + 1] = ':';
        @memcpy(full_buf[entry.path.len + 2 ..][0..type_name.len], type_name);

        var qname_buf: [256]u8 = undefined;
        const qname = impl_resolve.rustPathToDot(full_buf[0..needed], &qname_buf) orelse continue;

        const leaf_id = try phantom.getOrCreate(allocator, qname, .rust, entry.external);

        if (usageSiteFromNode(field_node, importer_path, type_name)) |s| {
            try phantom.recordUsageSite(allocator, leaf_id, s);
        }

        const start_id = field_node.parent_id orelse return;
        const owner_id = walkToOwningType(graph, start_id);

        _ = try graph.addEdgeIfNew(allocator, .{
            .source_id = owner_id,
            .target_id = leaf_id,
            .edge_type = .uses_type,
            .source = .phantom,
        });
        return;
    }
}

/// Scan struct/enum field signatures for module-qualified type references
/// and create phantom child nodes with uses_type edges from the owning type.
/// Handles any crate prefix, not just std.
fn resolveScopedFieldPhantoms(
    allocator: std.mem.Allocator,
    graph: *Graph,
    file_idx: usize,
    file_end_idx: usize,
    phantom: *PhantomManager,
    graph_index: *const GraphIndex,
    log: Logger,
) error{OutOfMemory}!void {
    _ = log;

    const clamped_end = @min(file_end_idx, graph.nodes.items.len);
    const importer_path = graph.nodes.items[file_idx].file_path;

    var prefix_buf: [MAX_SCOPED_PREFIXES]PrefixEntry = undefined;
    const prefix_count = collectModulePrefixImports(graph, file_idx, clamped_end, graph_index, importer_path, &prefix_buf);
    if (prefix_count == 0) return;
    const prefixes = prefix_buf[0..prefix_count];

    for (graph.nodes.items[file_idx..clamped_end]) |n| {
        if (n.kind != .field) continue;
        try resolveOneScopedField(allocator, graph, phantom, n, prefixes, importer_path);
    }
}
