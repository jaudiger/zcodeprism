const std = @import("std");
const graph_mod = @import("../../core/graph.zig");
const node_mod = @import("../../core/node.zig");
const types_mod = @import("../../core/types.zig");
const phantom_mod = @import("../../core/phantom.zig");
const shared_types = @import("types.zig");
const scope_index_mod = @import("../../core/scope_index.zig");

const Graph = graph_mod.Graph;
const Node = node_mod.Node;
const NodeId = types_mod.NodeId;
const NodeKind = types_mod.NodeKind;
const PhantomManager = phantom_mod.PhantomManager;
const EdgeContext = shared_types.EdgeContext;
const ScopeIndex = scope_index_mod.ScopeIndex;

/// Search imported project files then the phantom registry for a PascalCase type.
/// Deduplicates target file ids to avoid false ambiguity when multiple import
/// entries point to the same file. Returns the NodeId only if exactly one distinct
/// match exists across all sources.
pub fn findTypeCrossFile(graph: *const Graph, name: []const u8, ctx: *const EdgeContext, scope_index: *const ScopeIndex, phantom_mgr: *const PhantomManager) ?NodeId {
    if (name.len == 0 or !std.ascii.isUpper(name[0])) return null;

    var unique_targets: [512]NodeId = undefined;
    var unique_count: usize = 0;
    for (ctx.imports.items) |entry| {
        var already = false;
        for (unique_targets[0..unique_count]) |existing| {
            if (existing == entry.target) {
                already = true;
                break;
            }
        }
        if (!already and unique_count < unique_targets.len) {
            unique_targets[unique_count] = entry.target;
            unique_count += 1;
        }
    }

    var match: ?NodeId = null;
    var match_count: usize = 0;
    for (unique_targets[0..unique_count]) |target_file_id| {
        for (scope_index.childrenOf(target_file_id)) |child_idx| {
            const n = graph.nodes.items[child_idx];
            if (!n.kind.isTypeContainer()) continue;
            if (!std.mem.eql(u8, n.name, name)) continue;
            // Skip Rust impl_blocks and type_aliases: they share the type name
            // but are not the defining declaration.
            if (n.lang_meta == .rust) {
                const sk = n.lang_meta.rust.sub_kind;
                if (sk == .impl_block or sk == .type_alias) continue;
            }
            match = @enumFromInt(child_idx);
            match_count += 1;
        }
    }
    if (match_count == 1) return match;
    if (match_count > 1) return null;

    return phantom_mgr.findByShortName(name);
}

/// Find a function node by name with scope-aware resolution.
/// Walks up the parent_id chain from caller_parent_id, preferring the narrowest scope.
/// Falls back to flat file-scope search, returning null if ambiguous.
/// `extra_kinds` is checked alongside .function (pass &.{} for none, &.{.test_def} for Rust).
pub fn findFunctionByNameScoped(graph: *const Graph, name: []const u8, scope_start: usize, scope_end: usize, caller_parent_id: ?NodeId, scope_index: *const ScopeIndex, extra_kinds: []const NodeKind) ?NodeId {
    if (caller_parent_id) |cpid| {
        var current_scope: ?NodeId = cpid;
        var hops: usize = 0;
        while (current_scope != null and hops < 100) : (hops += 1) {
            const scope_id = current_scope.?;
            for (scope_index.childrenOf(scope_id)) |child_idx| {
                const n = graph.nodes.items[child_idx];
                if (matchesFunctionKind(n.kind, extra_kinds) and std.mem.eql(u8, n.name, name)) {
                    return @enumFromInt(child_idx);
                }
            }
            const scope_node = graph.getNode(scope_id) orelse break;
            current_scope = scope_node.parent_id;
        }
    }
    const items = graph.nodes.items;
    const end = @min(scope_end, items.len);
    var sole_match: ?NodeId = null;
    var match_count: usize = 0;
    for (items[scope_start..end], scope_start..) |n, idx| {
        if (matchesFunctionKind(n.kind, extra_kinds) and std.mem.eql(u8, n.name, name)) {
            sole_match = @enumFromInt(idx);
            match_count += 1;
            if (match_count > 1) return null;
        }
    }
    return sole_match;
}

/// Find a type node by name with scope-aware resolution.
/// `match_fn` is called with (node, name) and returns true if the node qualifies.
pub fn findTypeByNameScoped(graph: *const Graph, name: []const u8, scope_start: usize, scope_end: usize, caller_parent_id: ?NodeId, scope_index: *const ScopeIndex, match_fn: *const fn (Node, []const u8) bool) ?NodeId {
    if (caller_parent_id) |cpid| {
        var current_scope: ?NodeId = cpid;
        var hops: usize = 0;
        while (current_scope != null and hops < 100) : (hops += 1) {
            const scope_id = current_scope.?;
            for (scope_index.childrenOf(scope_id)) |child_idx| {
                const n = graph.nodes.items[child_idx];
                if (match_fn(n, name)) return @enumFromInt(child_idx);
            }
            const scope_node = graph.getNode(scope_id) orelse break;
            current_scope = scope_node.parent_id;
        }
    }
    const items = graph.nodes.items;
    const end = @min(scope_end, items.len);
    var sole_match: ?NodeId = null;
    var match_count: usize = 0;
    for (items[scope_start..end], scope_start..) |n, idx| {
        if (match_fn(n, name)) {
            sole_match = @enumFromInt(idx);
            match_count += 1;
            if (match_count > 1) return null;
        }
    }
    return sole_match;
}

/// Find a function node by name and line number within a scope range.
/// Tries .function kind first, then falls back to type containers
/// (for generic type-returning functions stored as type_def).
pub fn findFunctionByNameAndLine(graph: *const Graph, name: []const u8, line: u32, scope_start: usize, scope_end: usize) ?NodeId {
    const items = graph.nodes.items;
    const end = @min(scope_end, items.len);
    for (items[scope_start..end], scope_start..) |n, i| {
        if (n.kind == .function and std.mem.eql(u8, n.name, name) and
            n.line_start != null and n.line_start.? == line)
        {
            return @enumFromInt(i);
        }
    }
    for (items[scope_start..end], scope_start..) |n, i| {
        if (n.kind.isTypeContainer() and
            std.mem.eql(u8, n.name, name) and
            n.line_start != null and n.line_start.? == line)
        {
            return @enumFromInt(i);
        }
    }
    return null;
}

fn matchesFunctionKind(kind: NodeKind, extra_kinds: []const NodeKind) bool {
    if (kind == .function) return true;
    for (extra_kinds) |ek| {
        if (kind == ek) return true;
    }
    return false;
}
