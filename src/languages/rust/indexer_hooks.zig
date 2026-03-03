const std = @import("std");
const graph_mod = @import("../../core/graph.zig");
const phantom_mod = @import("../../core/phantom.zig");
const graph_index_mod = @import("../../core/graph_index.zig");
const source_scan = @import("../../parser/source_scan.zig");
const impl_resolve = @import("impl_resolve.zig");
const logging = @import("../../logging.zig");
const types = @import("../../core/types.zig");
const lang = @import("../language.zig");

const Graph = graph_mod.Graph;
const PhantomManager = phantom_mod.PhantomManager;
const GraphIndex = graph_index_mod.GraphIndex;
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

/// Create phantom nodes and edges for external references in a single Rust file.
/// Scans for `use std::...` patterns and creates phantom nodes for the stdlib.
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

    const file_id: NodeId = @enumFromInt(file_idx);
    const clamped_end = @min(scope_end, graph.nodes.items.len);

    // Collect: scan import_decl nodes for std usage (read-only).
    var found_std = false;
    for (graph.nodes.items[file_idx..clamped_end]) |n| {
        if (n.kind != .import_decl) continue;
        if (n.parent_id == null or n.parent_id.? != file_id) continue;
        const sig = n.signature orelse continue;
        if (std.mem.indexOf(u8, sig, "std::") != null) {
            found_std = true;
            break;
        }
    }

    // Act: create phantom nodes outside the scan loop.
    if (found_std) {
        const std_id = try phantom.getOrCreate(allocator, "std", .rust, .{ .stdlib = {} });
        _ = try graph.addEdgeIfNew(allocator, .{ .source_id = file_id, .target_id = std_id, .edge_type = .imports, .source = .phantom });
        try resolveStdUsePhantoms(allocator, graph, source, file_idx, clamped_end, phantom, "std", log);
        try resolveScopedFieldPhantoms(allocator, graph, file_idx, clamped_end, phantom, log);
    }

    // Resolve implements edges for trait impl blocks.
    // During single-file parsing, implements edges are only created when
    // both the trait and type are defined in the same file. This phase
    // handles cross-file traits and external traits (std, deps) by
    // searching the full graph and creating phantom nodes as needed.
    try impl_resolve.resolveImplementsEdges(allocator, graph, file_idx, clamped_end, phantom, graph_index);
}

/// Create phantom nodes for std use declarations by iterating the file's
/// import_decl graph nodes and parsing their signatures. Handles simple paths,
/// brace groups, and nested groups via balanced brace matching.
fn resolveStdUsePhantoms(
    allocator: std.mem.Allocator,
    graph: *Graph,
    source: []const u8,
    file_idx: usize,
    file_end_idx: usize,
    phantom: *PhantomManager,
    prefix: []const u8,
    log: Logger,
) error{OutOfMemory}!void {
    _ = source;

    const file_id: NodeId = @enumFromInt(file_idx);
    const clamped_end = @min(file_end_idx, graph.nodes.items.len);

    for (graph.nodes.items[file_idx..clamped_end]) |n| {
        if (n.kind != .import_decl) continue;
        if (n.parent_id == null or n.parent_id.? != file_id) continue;
        const sig = n.signature orelse continue;

        const span = source_scan.extractUsePath(sig) orelse continue;
        const path = span.path;

        // Skip paths that don't start with the target prefix.
        if (path.len < prefix.len + 2) continue;
        if (!std.mem.startsWith(u8, path, prefix)) continue;
        if (path[prefix.len] != ':' or path[prefix.len + 1] != ':') continue;

        // Handle brace group: use std::collections::{HashMap, BTreeMap}.
        if (span.end < sig.len and sig[span.end] == '{') {
            const brace_end = source_scan.findMatchingBrace(sig, span.end) orelse continue;
            const group = sig[span.end + 1 .. brace_end];

            // The prefix path (up to ::) is the common prefix for all members.
            const common = if (std.mem.endsWith(u8, path, "::"))
                path[0 .. path.len - 2]
            else
                path;

            try expandGroupPhantoms(allocator, graph, file_id, phantom, common, group, prefix, log);
            continue;
        }

        // Simple path: use std::collections::HashMap.
        try createStdPhantom(allocator, graph, file_id, phantom, path, prefix, log);
    }
}

/// Expand a brace group and create phantom nodes for each member. Handles
/// nested groups recursively, plain identifiers, and "X as Y" aliases.
fn expandGroupPhantoms(
    allocator: std.mem.Allocator,
    graph: *Graph,
    file_id: NodeId,
    phantom: *PhantomManager,
    common_prefix: []const u8,
    group: []const u8,
    std_prefix: []const u8,
    log: Logger,
) error{OutOfMemory}!void {
    var pos: usize = 0;
    while (pos < group.len) {
        while (pos < group.len and (group[pos] == ' ' or group[pos] == ',' or group[pos] == '\t' or group[pos] == '\n' or group[pos] == '\r')) pos += 1;
        if (pos >= group.len) break;

        if (group[pos] == '{') {
            if (source_scan.findMatchingBrace(group, pos)) |close| {
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
                if (source_scan.findMatchingBrace(group, pos)) |close| {
                    // Build nested prefix and recurse.
                    var nested_buf: [256]u8 = undefined;
                    const needed = common_prefix.len + 2 + ident.len;
                    if (needed <= nested_buf.len) {
                        @memcpy(nested_buf[0..common_prefix.len], common_prefix);
                        nested_buf[common_prefix.len] = ':';
                        nested_buf[common_prefix.len + 1] = ':';
                        @memcpy(nested_buf[common_prefix.len + 2 ..][0..ident.len], ident);
                        try expandGroupPhantoms(allocator, graph, file_id, phantom, nested_buf[0..needed], group[pos + 1 .. close], std_prefix, log);
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
            try createStdPhantom(allocator, graph, file_id, phantom, full_buf[0..needed], std_prefix, log);
        }
    }
}

/// Create a single std phantom node from a full Rust :: path. Converts the path
/// to dot-separated form and picks the edge type based on the leaf segment's case.
fn createStdPhantom(
    allocator: std.mem.Allocator,
    graph: *Graph,
    file_id: NodeId,
    phantom: *PhantomManager,
    path: []const u8,
    std_prefix: []const u8,
    log: Logger,
) error{OutOfMemory}!void {
    _ = log;

    var qname_buf: [256]u8 = undefined;
    const qname = impl_resolve.rustPathToDot(path, &qname_buf) orelse return;
    if (qname.len <= std_prefix.len) return;

    const last_dot = std.mem.lastIndexOfScalar(u8, qname, '.') orelse return;
    const leaf = qname[last_dot + 1 ..];
    if (leaf.len == 0) return;
    const is_type = leaf[0] >= 'A' and leaf[0] <= 'Z';
    const edge_type: EdgeType = if (is_type) .uses_type else .imports;

    const leaf_id = try phantom.getOrCreate(allocator, qname, .rust, .{ .stdlib = {} });
    _ = try graph.addEdgeIfNew(allocator, .{
        .source_id = file_id,
        .target_id = leaf_id,
        .edge_type = edge_type,
        .source = .phantom,
    });
}

/// Scan struct/enum field signatures for module-qualified type references
/// and create phantom child nodes with uses_type edges from the owning type.
fn resolveScopedFieldPhantoms(
    allocator: std.mem.Allocator,
    graph: *Graph,
    file_idx: usize,
    file_end_idx: usize,
    phantom: *PhantomManager,
    log: Logger,
) error{OutOfMemory}!void {
    _ = log;

    const file_id: NodeId = @enumFromInt(file_idx);
    const clamped_end = @min(file_end_idx, graph.nodes.items.len);

    // Collect module-prefix imports: for `use std::io`, map "io" to "std::io".
    const max_prefixes = 64;
    const PrefixEntry = struct { name: []const u8, path: []const u8 };
    var prefix_buf: [max_prefixes]PrefixEntry = undefined;
    var prefix_count: usize = 0;

    for (graph.nodes.items[file_idx..clamped_end]) |n| {
        if (n.kind != .import_decl) continue;
        if (n.parent_id == null or n.parent_id.? != file_id) continue;
        const sig = n.signature orelse continue;

        const span = source_scan.extractUsePath(sig) orelse continue;
        const path = span.path;
        if (!std.mem.startsWith(u8, path, "std::")) continue;

        const last_sep = std.mem.lastIndexOf(u8, path, "::") orelse continue;
        const terminal = path[last_sep + 2 ..];
        if (terminal.len == 0) continue;
        // Only module imports (lowercase first char).
        if (terminal[0] >= 'A' and terminal[0] <= 'Z') continue;

        if (prefix_count < max_prefixes) {
            prefix_buf[prefix_count] = .{ .name = terminal, .path = path };
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

            // Build full std path and convert to dot-separated form.
            var full_buf: [256]u8 = undefined;
            const needed = entry.path.len + 2 + type_name.len;
            if (needed > full_buf.len) continue;
            @memcpy(full_buf[0..entry.path.len], entry.path);
            full_buf[entry.path.len] = ':';
            full_buf[entry.path.len + 1] = ':';
            @memcpy(full_buf[entry.path.len + 2 ..][0..type_name.len], type_name);

            var qname_buf: [256]u8 = undefined;
            const qname = impl_resolve.rustPathToDot(full_buf[0..needed], &qname_buf) orelse continue;

            const leaf_id = try phantom.getOrCreate(allocator, qname, .rust, .{ .stdlib = {} });

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
