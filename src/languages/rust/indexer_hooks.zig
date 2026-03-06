const std = @import("std");
const graph_mod = @import("../../core/graph.zig");
const phantom_mod = @import("../../core/phantom.zig");
const graph_index_mod = @import("../../core/graph_index.zig");
const source_scan = @import("../../parser/source_scan.zig");
const source_utils = @import("source_utils.zig");
const impl_resolve = @import("impl_resolve.zig");
const logging = @import("../../logging.zig");
const types = @import("../../core/types.zig");
const lang = @import("../language.zig");

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
const ExternalInfo = lang.ExternalInfo;
const BuildConfig = lang.BuildConfig;

/// Scan Rust source text for `mod <name>;` patterns (external module declarations).
/// Returns the number of entries written to `out`.
pub fn extractImports(source: []const u8, out: []ImportEntry) usize {
    var pos: usize = 0;
    var count: usize = 0;

    while (pos < source.len) {
        // Find next "mod " at a word boundary.
        const idx = std.mem.indexOf(u8, source[pos..], "mod ") orelse break;
        const abs_idx = pos + idx;

        // Ensure word boundary before "mod".
        if (abs_idx > 0 and source_scan.isIdentChar(source[abs_idx - 1])) {
            pos = abs_idx + 4;
            continue;
        }

        const name_start = abs_idx + 4;
        if (name_start >= source.len) break;

        // Skip whitespace after "mod ".
        var ns = name_start;
        while (ns < source.len and (source[ns] == ' ' or source[ns] == '\t')) ns += 1;
        if (ns >= source.len) break;

        // Read identifier.
        var ne = ns;
        while (ne < source.len and source_scan.isIdentChar(source[ne])) ne += 1;
        if (ne == ns) {
            pos = ne;
            continue;
        }

        const name = source[ns..ne];

        // Skip whitespace after name.
        var after = ne;
        while (after < source.len and (source[after] == ' ' or source[after] == '\t')) after += 1;

        // Check for semicolon (external module) vs '{' (inline module).
        if (after < source.len and source[after] == ';') {
            if (count < out.len) {
                out[count] = .{
                    .path = name,
                    .kind = .project_file,
                };
                count += 1;
            }
        }

        pos = if (after < source.len) after + 1 else source.len;
    }
    return count;
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

/// Create phantom nodes and edges for all `use` declarations in a single Rust file.
/// Stdlib paths map to the `.stdlib` external variant; all other crates map
/// to `.dependency`.
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
        if (isLocalCrate(crate, graph.nodes.items[file_idx].file_path, &graph_index.files)) continue;

        const external: ExternalInfo = if (std.mem.eql(u8, crate, "std"))
            .{ .stdlib = {} }
        else
            .{ .dependency = .{ .version = null } };

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
            try expandGroupPhantoms(allocator, graph, file_id, import_decl_id, phantom, common, group, crate, external, log);
        } else {
            try createExternalPhantom(allocator, graph, file_id, import_decl_id, phantom, path, crate, external, log);
        }
    }

    try resolveScopedFieldPhantoms(allocator, graph, file_idx, clamped_end, phantom, graph_index, log);

    try impl_resolve.resolveImplementsEdges(allocator, graph, file_idx, clamped_end, phantom, graph_index);
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
                        try expandGroupPhantoms(allocator, graph, file_id, import_decl_id, phantom, nested_buf[0..needed], group[pos + 1 .. close], crate, external, log);
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
            try createExternalPhantom(allocator, graph, file_id, import_decl_id, phantom, full_buf[0..needed], crate, external, log);
        }
    }
}

/// Create a phantom node for a single Rust `::` path. Converts the path to
/// dot-separated form, infers the edge type from the leaf segment's case,
/// and attaches edges from both the file node and the import_decl node.
fn createExternalPhantom(
    allocator: std.mem.Allocator,
    graph: *Graph,
    file_id: NodeId,
    import_decl_id: NodeId,
    phantom: *PhantomManager,
    path: []const u8,
    crate: []const u8,
    external: ExternalInfo,
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

    const file_id: NodeId = @enumFromInt(file_idx);
    const clamped_end = @min(file_end_idx, graph.nodes.items.len);

    // Collect module-prefix imports: for `use crate::module`, map "module" -> "crate::module".
    const max_prefixes = 64;
    const PrefixEntry = struct { name: []const u8, path: []const u8, external: ExternalInfo };
    var prefix_buf: [max_prefixes]PrefixEntry = undefined;
    var prefix_count: usize = 0;

    const importer_path = graph.nodes.items[file_idx].file_path;

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
        // Only module imports (lowercase first char).
        if (terminal[0] >= 'A' and terminal[0] <= 'Z') continue;

        const external: ExternalInfo = if (std.mem.eql(u8, crate, "std"))
            .{ .stdlib = {} }
        else
            .{ .dependency = .{ .version = null } };

        if (prefix_count < max_prefixes) {
            prefix_buf[prefix_count] = .{ .name = terminal, .path = path, .external = external };
            prefix_count += 1;
        }
    }
    if (prefix_count == 0) return;

    // Scan field nodes for scoped type references matching a module-prefix import.
    for (graph.nodes.items[file_idx..clamped_end]) |n| {
        if (n.kind != .field) continue;
        const sig = n.signature orelse continue;

        const colon_pos = std.mem.indexOf(u8, sig, "::") orelse continue;
        if (colon_pos == 0) continue;
        const field_prefix = sig[0..colon_pos];

        const remainder = sig[colon_pos + 2 ..];
        if (remainder.len == 0) continue;
        var name_end: usize = 0;
        while (name_end < remainder.len and source_scan.isIdentChar(remainder[name_end])) : (name_end += 1) {}
        if (name_end == 0) continue;
        const type_name = remainder[0..name_end];
        // Only PascalCase type names.
        if (type_name[0] < 'A' or type_name[0] > 'Z') continue;

        for (prefix_buf[0..prefix_count]) |entry| {
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

            // Walk up from the field to find the owning struct/enum.
            var owner_id = n.parent_id orelse continue;
            var hops: usize = 0;
            while (hops < 10) : (hops += 1) {
                const owner = graph.getNode(owner_id) orelse break;
                if (owner.kind == .type_def or owner.kind == .enum_def or owner.kind == .union_def) break;
                owner_id = owner.parent_id orelse break;
            }

            _ = try graph.addEdgeIfNew(allocator, .{
                .source_id = owner_id,
                .target_id = leaf_id,
                .edge_type = .uses_type,
                .source = .phantom,
            });
            break;
        }
    }
}
