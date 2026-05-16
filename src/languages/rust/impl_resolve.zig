const std = @import("std");
const graph_mod = @import("../../core/graph.zig");
const phantom_mod = @import("../../core/phantom.zig");
const source_scan = @import("../../parser/source_scan.zig");
const source_utils = @import("source_utils.zig");
const types = @import("../../core/types.zig");
const node_utils = @import("node_utils.zig");
const rust_meta = @import("meta.zig");

const Graph = graph_mod.Graph;
const PhantomManager = phantom_mod.PhantomManager;
const GraphIndex = @import("../../core/graph_index.zig").GraphIndex;
const NameIndex = @import("../../core/name_index.zig").NameIndex;
const NodeId = types.NodeId;

/// Scan impl_block nodes for trait implementations and create implements edges.
/// For each trait impl block, extracts the trait name from the full declaration
/// signature, then searches the graph with scoped precedence: same-file,
/// imported files, full graph. If the trait is not in the project, consult use
/// declarations to build a qualified phantom name. Same-file edges already
/// created by the edge_builder are deduplicated by addEdgeIfNew.
pub fn resolveImplementsEdges(
    allocator: std.mem.Allocator,
    graph: *Graph,
    file_idx: usize,
    scope_end: usize,
    phantom: *PhantomManager,
    graph_index: *const GraphIndex,
) error{OutOfMemory}!void {
    const name_index = &graph_index.names;
    const ImplInfo = struct {
        idx: usize,
        trait_name: []const u8,
        type_name: []const u8,
    };

    // Collect: scan for trait impl blocks (read-only, no graph mutation).
    var buf: [128]ImplInfo = undefined;
    var count: usize = 0;
    {
        const items = graph.nodes.items;
        const clamped_end = @min(scope_end, items.len);
        for (items[file_idx..clamped_end], file_idx..) |n, idx| {
            if (n.kind != .type_def) continue;
            const m = rust_meta.metaOf(&n) orelse continue;
            if (m.sub_kind != .impl_block) continue;
            const sig = n.signature orelse continue;
            const trait_name = extractTraitFromImplSig(sig) orelse continue;
            if (count >= buf.len) break;
            buf[count] = .{ .idx = idx, .trait_name = trait_name, .type_name = n.name };
            count += 1;
        }
    }

    // Act
    // so phantom.getOrCreate adding nodes to the graph is harmless.
    const clamped_end = @min(scope_end, graph.nodes.items.len);
    for (buf[0..count]) |info| {
        const impl_id: NodeId = @enumFromInt(info.idx);

        const trait_id = findTraitInGraph(graph, info.trait_name, file_idx, clamped_end, graph_index) orelse blk: {
            var qbuf: [256]u8 = undefined;
            const phantom_name = resolveTraitViaUseDecls(graph, info.trait_name, file_idx, clamped_end, &qbuf) orelse info.trait_name;
            break :blk try phantom.getOrCreate(allocator, phantom_name, .rust, .{ .stdlib = {} });
        };

        const type_id = findTypeInGraph(graph, info.type_name, file_idx, clamped_end, name_index);
        const source_id = type_id orelse impl_id;

        _ = try graph.addEdgeIfNew(allocator, .{
            .source_id = source_id,
            .target_id = trait_id,
            .edge_type = .implements,
            .source = .phantom,
        });
    }
}

/// Search the graph for a trait node with the given name, preferring local scope.
/// Checks same-file first, then imported files via scope_index, then the full graph.
/// Strips any :: scope prefix before matching since graph nodes store bare names.
fn findTraitInGraph(graph: *const Graph, name: []const u8, file_idx: usize, scope_end: usize, graph_index: *const GraphIndex) ?NodeId {
    const scope_index = &graph_index.scope;
    const name_index = &graph_index.names;
    const items = graph.nodes.items;
    const clamped_end = @min(scope_end, items.len);

    const bare = if (std.mem.lastIndexOf(u8, name, "::")) |pos| name[pos + 2 ..] else name;

    // Same-file scope.
    for (items[file_idx..clamped_end], file_idx..) |n, idx| {
        if (node_utils.isTraitNode(n) and std.mem.eql(u8, n.name, bare)) {
            return @enumFromInt(idx);
        }
    }

    // Imported files: search direct children of each import target via scope_index.
    const file_id: NodeId = @enumFromInt(file_idx);
    {
        var import_match: ?NodeId = null;
        var import_match_count: usize = 0;
        for (graph_index.imports.targetsOf(file_id)) |target_id| {
            for (scope_index.childrenOf(target_id)) |child_idx| {
                const n = items[child_idx];
                if (!node_utils.isTraitNode(n)) continue;
                if (!std.mem.eql(u8, n.name, bare)) continue;
                import_match = @enumFromInt(child_idx);
                import_match_count += 1;
                if (import_match_count > 1) return null;
            }
        }
        if (import_match_count == 1) return import_match;
    }

    // Full graph lookup via name index, return only if unambiguous.
    var match: ?NodeId = null;
    var match_count: usize = 0;
    for (name_index.findByName(bare)) |idx| {
        const n = items[idx];
        if (!node_utils.isTraitNode(n)) continue;
        match = @enumFromInt(idx);
        match_count += 1;
        if (match_count > 1) return null;
    }
    return match;
}

/// Search the graph for a type definition (struct, enum, union, type alias)
/// with the given name, excluding impl blocks, traits, and associated types.
/// Prefers same-file scope, then imported files via scope_index.
fn findTypeInGraph(graph: *const Graph, name: []const u8, file_idx: usize, scope_end: usize, name_index: *const NameIndex) ?NodeId {
    const items = graph.nodes.items;
    const clamped_end = @min(scope_end, items.len);

    // Same-file scope.
    for (items[file_idx..clamped_end], file_idx..) |n, idx| {
        if (node_utils.isTypeOrAliasNode(n) and std.mem.eql(u8, n.name, name)) {
            return @enumFromInt(idx);
        }
    }

    // Full graph lookup via name index, return only if unambiguous.
    var match: ?NodeId = null;
    var match_count: usize = 0;
    for (name_index.findByName(name)) |idx| {
        const n = items[idx];
        if (!node_utils.isTypeOrAliasNode(n)) continue;
        match = @enumFromInt(idx);
        match_count += 1;
        if (match_count > 1) return null;
    }
    return match;
}

/// Resolve a trait name (bare or scoped) to a qualified dot-separated name
/// by consulting the file's use declarations.
fn resolveTraitViaUseDecls(graph: *const Graph, trait_name: []const u8, file_idx: usize, scope_end: usize, buf: *[256]u8) ?[]const u8 {
    const file_id: NodeId = @enumFromInt(file_idx);
    const items = graph.nodes.items;
    const clamped_end = @min(scope_end, items.len);

    const has_scope = std.mem.indexOf(u8, trait_name, "::");
    const bare_name = if (std.mem.lastIndexOf(u8, trait_name, "::")) |pos| trait_name[pos + 2 ..] else trait_name;
    const first_seg: ?[]const u8 = if (has_scope) |pos| trait_name[0..pos] else null;
    const rest: ?[]const u8 = if (has_scope) |pos| trait_name[pos + 2 ..] else null;

    for (items[file_idx..clamped_end]) |n| {
        if (n.kind != .import_decl) continue;
        if (n.parent_id == null or n.parent_id.? != file_id) continue;
        const sig = n.signature orelse continue;

        const span = source_utils.extractUsePath(sig) orelse continue;
        const path = span.path;
        const path_last_sep = std.mem.lastIndexOf(u8, path, "::");
        const path_last_seg = if (path_last_sep) |s| path[s + 2 ..] else path;

        // Handle brace-delimited group: use foo::{A, B as C}.
        if (span.end < sig.len and sig[span.end] == '{') {
            const brace_end = source_utils.findMatchingBrace(sig, span.end) orelse continue;
            const group = sig[span.end + 1 .. brace_end];
            if (findTraitInGroup(group, bare_name)) {
                // Build the full qualified path: prefix + bare_name.
                const prefix = if (std.mem.endsWith(u8, path, "::"))
                    path[0 .. path.len - 2]
                else
                    path;
                const total = prefix.len + 2 + bare_name.len;
                if (total <= buf.len) {
                    var assembled: [256]u8 = undefined;
                    @memcpy(assembled[0..prefix.len], prefix);
                    assembled[prefix.len] = ':';
                    assembled[prefix.len + 1] = ':';
                    @memcpy(assembled[prefix.len + 2 ..][0..bare_name.len], bare_name);
                    return rustPathToDot(assembled[0..total], buf);
                }
            }
            continue;
        }

        const alias = source_utils.extractAlias(sig, span.end);

        if (alias) |a| {
            if (path_last_sep != null and std.mem.eql(u8, a, bare_name)) {
                return rustPathToDot(path, buf);
            }
        }

        if (path_last_sep != null and std.mem.eql(u8, path_last_seg, bare_name)) {
            return rustPathToDot(path, buf);
        }

        if (first_seg) |fs| {
            if (std.mem.eql(u8, path_last_seg, fs)) {
                if (rest) |r| {
                    const total = path.len + 2 + r.len;
                    if (total > buf.len) continue;
                    var assembled: [256]u8 = undefined;
                    @memcpy(assembled[0..path.len], path);
                    assembled[path.len] = ':';
                    assembled[path.len + 1] = ':';
                    @memcpy(assembled[path.len + 2 ..][0..r.len], r);
                    return rustPathToDot(assembled[0..total], buf);
                }
            }
        }
    }

    if (has_scope != null) {
        return rustPathToDot(trait_name, buf);
    }

    return null;
}

/// Extract the trait name from a full impl declaration signature.
fn extractTraitFromImplSig(sig: []const u8) ?[]const u8 {
    const for_idx = std.mem.indexOf(u8, sig, " for ") orelse return null;

    var start: usize = 0;
    if (std.mem.startsWith(u8, sig, "unsafe ")) start = 7;
    if (start + 4 <= sig.len and std.mem.eql(u8, sig[start..][0..4], "impl")) start += 4;
    while (start < for_idx and sig[start] == ' ') start += 1;

    // Skip balanced angle brackets for impl-level type parameters.
    if (start < for_idx and sig[start] == '<') {
        var depth: usize = 0;
        while (start < for_idx) : (start += 1) {
            if (sig[start] == '<') {
                depth += 1;
            } else if (sig[start] == '>') {
                depth -= 1;
                if (depth == 0) {
                    start += 1;
                    break;
                }
            }
        }
        while (start < for_idx and sig[start] == ' ') start += 1;
    }

    if (start >= for_idx) return null;
    const trait_name = std.mem.trimEnd(u8, sig[start..for_idx], " ");
    if (trait_name.len == 0) return null;
    return trait_name;
}

/// Search a brace group for a symbol name, recognizing both plain identifiers
/// and "X as Y" aliases. Skips nested groups and sub-paths.
pub fn findTraitInGroup(group: []const u8, trait_name: []const u8) bool {
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

        const ident_start = pos;
        while (pos < group.len and source_scan.isIdentChar(group[pos])) pos += 1;
        if (pos == ident_start) {
            pos += 1;
            continue;
        }
        const ident = group[ident_start..pos];

        // Skip nested scoped_use_list: "ident::{...}".
        if (pos + 1 < group.len and group[pos] == ':' and group[pos + 1] == ':') {
            pos += 2;
            if (pos < group.len and group[pos] == '{') {
                if (source_utils.findMatchingBrace(group, pos)) |close| {
                    pos = close + 1;
                } else break;
            }
            continue;
        }

        // Check for "as Alias".
        var check_pos = pos;
        while (check_pos < group.len and group[check_pos] == ' ') check_pos += 1;
        if (check_pos + 3 <= group.len and std.mem.eql(u8, group[check_pos..][0..3], "as ")) {
            check_pos += 3;
            while (check_pos < group.len and group[check_pos] == ' ') check_pos += 1;
            const alias_start = check_pos;
            while (check_pos < group.len and source_scan.isIdentChar(group[check_pos])) check_pos += 1;
            if (check_pos > alias_start and std.mem.eql(u8, group[alias_start..check_pos], trait_name)) return true;
            pos = check_pos;
            continue;
        }

        if (std.mem.eql(u8, ident, trait_name)) return true;
    }
    return false;
}

/// Convert a Rust :: path to dot-separated form in a caller-provided buffer.
pub fn rustPathToDot(path: []const u8, buf: *[256]u8) ?[]const u8 {
    var wpos: usize = 0;
    var rpos: usize = 0;
    while (rpos < path.len) {
        if (rpos + 1 < path.len and path[rpos] == ':' and path[rpos + 1] == ':') {
            if (wpos >= buf.len) return null;
            buf[wpos] = '.';
            wpos += 1;
            rpos += 2;
        } else {
            if (wpos >= buf.len) return null;
            buf[wpos] = path[rpos];
            wpos += 1;
            rpos += 1;
        }
    }
    return buf[0..wpos];
}
