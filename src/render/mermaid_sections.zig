const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const node_mod = @import("../core/node.zig");
const types = @import("../core/types.zig");
const common = @import("common.zig");

const Graph = graph_mod.Graph;
const Node = node_mod.Node;
const NodeId = types.NodeId;
const NodeKind = types.NodeKind;
const EdgeType = types.EdgeType;

const IdEntry = common.IdEntry;
const PhantomPackage = common.PhantomPackage;

const isInternal = common.isInternal;
const inScope = common.inScope;
const appendNum = common.appendNum;
const edgeTypeName = common.edgeTypeName;
const edgeTypeSortKey = common.edgeTypeSortKey;
const prefixOrder = common.prefixOrder;
const ChildrenIndex = common.ChildrenIndex;
const PhantomNodeInfo = common.PhantomNodeInfo;

// ---------------------------------------------------------------------------
// File subgraph rendering
// ---------------------------------------------------------------------------

pub fn renderFileSubgraphs(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    file_indices: []const usize,
    ids: []const ?IdEntry,
    num_buf: *[20]u8,
    children_index: *const ChildrenIndex,
) !void {
    for (file_indices) |fi| {
        const file_node = g.nodes.items[fi];
        const file_id = ids[fi].?;

        // subgraph f_N["path"]
        try out.appendSlice(allocator, "    subgraph ");
        try appendMermaidId(out, allocator, file_id.prefix, file_id.num, num_buf);
        try out.appendSlice(allocator, "[\"");
        try out.appendSlice(allocator, file_node.file_path orelse file_node.name);
        try out.appendSlice(allocator, "\"]\n");

        // Collect all descendants of this file that have IDs.
        var file_members = std.ArrayListUnmanaged(MermaidNode){};
        defer file_members.deinit(allocator);
        try collectDescendants(g, ids, children_index, fi, &file_members, allocator);

        // Sort by ID: prefix order then num.
        std.mem.sort(MermaidNode, file_members.items, {}, struct {
            fn lessThan(_: void, a: MermaidNode, b: MermaidNode) bool {
                const a_ord = prefixOrder(a.id.prefix);
                const b_ord = prefixOrder(b.id.prefix);
                if (a_ord != b_ord) return a_ord < b_ord;
                return a.id.num < b.id.num;
            }
        }.lessThan);

        for (file_members.items) |member| {
            const n = g.nodes.items[member.node_idx];
            try out.appendSlice(allocator, "        ");
            try renderNodeShape(out, allocator, g, n, member.id, num_buf);
            try out.append(allocator, '\n');
        }

        try out.appendSlice(allocator, "    end\n");
    }
}

const MermaidNode = struct {
    node_idx: usize,
    id: IdEntry,
};

/// Recursively collect renderable descendants of a parent via the children index.
fn collectDescendants(
    g: *const Graph,
    ids: []const ?IdEntry,
    children_index: *const ChildrenIndex,
    parent_idx: usize,
    result: *std.ArrayListUnmanaged(MermaidNode),
    allocator: std.mem.Allocator,
) !void {
    for (children_index.childrenOf(parent_idx)) |ci| {
        const n = g.nodes.items[ci];
        if (!isInternal(n)) continue;
        if (n.kind == .file or n.kind == .field or n.kind == .import_decl) continue;
        if (ids[ci]) |id_entry| {
            try result.append(allocator, .{ .node_idx = ci, .id = id_entry });
        }
        // Recurse into children (e.g. struct methods).
        try collectDescendants(g, ids, children_index, ci, result, allocator);
    }
}

/// Render a node in its Mermaid shape syntax.
fn renderNodeShape(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    n: Node,
    id: IdEntry,
    num_buf: *[20]u8,
) !void {
    try appendMermaidId(out, allocator, id.prefix, id.num, num_buf);

    switch (n.kind) {
        .function => {
            // fn_N["fn: name"]. For methods use "fn: Parent.name".
            try out.appendSlice(allocator, "[\"fn: ");
            if (n.parent_id) |pid| {
                const parent = g.nodes.items[@intFromEnum(pid)];
                if (parent.kind == .type_def) {
                    try appendEscaped(out, allocator, parent.name);
                    try out.append(allocator, '.');
                }
            }
            try appendEscaped(out, allocator, n.name);
            try out.appendSlice(allocator, "\"]");
        },
        .type_def => {
            // st_N[["struct: name"]]
            try out.appendSlice(allocator, "[[\"struct: ");
            try appendEscaped(out, allocator, n.name);
            try out.appendSlice(allocator, "\"]]");
        },
        .enum_def => {
            // en_N{{"enum: name"}}
            try out.appendSlice(allocator, "{{\"enum: ");
            try appendEscaped(out, allocator, n.name);
            try out.appendSlice(allocator, "\"}}");
        },
        .constant => {
            // c_N[/"const: name"/]
            try out.appendSlice(allocator, "[/\"const: ");
            try appendEscaped(out, allocator, n.name);
            try out.appendSlice(allocator, "\"/]");
        },
        .test_def => {
            // t_N(["test: name"])
            try out.appendSlice(allocator, "([\"test: ");
            try appendEscaped(out, allocator, n.name);
            try out.appendSlice(allocator, "\"])");
        },
        .error_def => {
            // err_N[/"error: name"\]
            try out.appendSlice(allocator, "[/\"error: ");
            try appendEscaped(out, allocator, n.name);
            try out.appendSlice(allocator, "\"\\]");
        },
        .module => {
            // m_N("mod: name")
            try out.appendSlice(allocator, "(\"mod: ");
            try appendEscaped(out, allocator, n.name);
            try out.appendSlice(allocator, "\")");
        },
        .file, .field, .import_decl => {},
    }
}

// ---------------------------------------------------------------------------
// Phantom subgraph rendering
// ---------------------------------------------------------------------------

pub fn renderPhantomSubgraphs(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    phantom_packages: []const PhantomPackage,
    num_buf: *[20]u8,
) !void {
    for (phantom_packages) |pkg| {
        const root = g.nodes.items[pkg.root_idx];
        const source_label: []const u8 = switch (root.external) {
            .stdlib => "stdlib",
            .dependency => "dependency",
            .none => "unknown",
        };

        // subgraph x_name["name (source)"]
        try out.appendSlice(allocator, "    subgraph x_");
        try out.appendSlice(allocator, root.name);
        try out.appendSlice(allocator, "[\"");
        try out.appendSlice(allocator, root.name);
        try out.appendSlice(allocator, " (");
        try out.appendSlice(allocator, source_label);
        try out.appendSlice(allocator, ")\"]\n");

        // Child symbols: x_{pkgId}_{symbol_name}["ext: qualified_path"]
        for (pkg.symbols.items) |sym| {
            try out.appendSlice(allocator, "        x_");
            try appendNum(out, allocator, pkg.x_num, num_buf);
            try out.append(allocator, '_');
            // Replace dots with underscores in the symbol name for the ID.
            try appendDotToUnderscore(out, allocator, sym.qualified_path);
            try out.appendSlice(allocator, "[\"ext: ");
            try appendEscaped(out, allocator, sym.qualified_path);
            try out.appendSlice(allocator, "\"]\n");
        }

        try out.appendSlice(allocator, "    end\n");
    }
}

// ---------------------------------------------------------------------------
// Ghost node rendering
// ---------------------------------------------------------------------------

/// Build a map from out-of-scope internal node index → ghost ID number.
pub fn buildGhostNodes(
    allocator: std.mem.Allocator,
    g: *const Graph,
    ids: []const ?IdEntry,
    scope: []const u8,
    ghost_map: *std.AutoHashMapUnmanaged(usize, u32),
) !void {
    var ghost_counter: u32 = 0;
    for (g.edges.items) |e| {
        const src_idx = @intFromEnum(e.source_id);
        const tgt_idx = @intFromEnum(e.target_id);
        if (src_idx >= g.nodes.items.len or tgt_idx >= g.nodes.items.len) continue;

        const src = g.nodes.items[src_idx];
        const tgt = g.nodes.items[tgt_idx];

        // Source must be in scope.
        if (!inScope(src.file_path, scope)) continue;
        // Source must have an ID.
        if (ids[src_idx] == null) continue;
        // Target must be internal (not phantom) and out of scope.
        if (!isInternal(tgt)) continue;
        if (inScope(tgt.file_path, scope)) continue;
        // Skip if target has no renderable kind.
        if (tgt.kind == .field or tgt.kind == .import_decl) continue;

        // Deduplicate: one ghost per target.
        if (!ghost_map.contains(tgt_idx)) {
            ghost_counter += 1;
            try ghost_map.put(allocator, tgt_idx, ghost_counter);
        }
    }
}

pub fn renderGhostNodes(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    ghost_map: *const std.AutoHashMapUnmanaged(usize, u32),
    num_buf: *[20]u8,
) !void {
    // Collect and sort by ghost ID.
    var entries = std.ArrayListUnmanaged(GhostEntry){};
    defer entries.deinit(allocator);

    var it = ghost_map.iterator();
    while (it.next()) |entry| {
        try entries.append(allocator, .{
            .node_idx = entry.key_ptr.*,
            .ghost_num = entry.value_ptr.*,
        });
    }
    std.mem.sort(GhostEntry, entries.items, {}, struct {
        fn lessThan(_: void, a: GhostEntry, b: GhostEntry) bool {
            return a.ghost_num < b.ghost_num;
        }
    }.lessThan);

    for (entries.items) |entry| {
        const n = g.nodes.items[entry.node_idx];
        const kind_prefix = nodeKindPrefix(n.kind);

        try out.appendSlice(allocator, "    g_");
        try appendNum(out, allocator, entry.ghost_num, num_buf);
        try out.appendSlice(allocator, "[\"");
        try out.appendSlice(allocator, kind_prefix);
        try out.appendSlice(allocator, ": ");
        try appendEscaped(out, allocator, n.name);
        try out.appendSlice(allocator, " ...\"]:::ghost_style\n");
    }
}

const GhostEntry = struct {
    node_idx: usize,
    ghost_num: u32,
};

fn nodeKindPrefix(kind: NodeKind) []const u8 {
    return switch (kind) {
        .function => "fn",
        .type_def => "struct",
        .enum_def => "enum",
        .constant => "const",
        .test_def => "test",
        .error_def => "error",
        .module => "mod",
        .file => "file",
        .field => "field",
        .import_decl => "import",
    };
}

// ---------------------------------------------------------------------------
// Edge rendering
// ---------------------------------------------------------------------------

pub fn renderEdges(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    ids: []const ?IdEntry,
    phantom_lookup: *const std.AutoHashMapUnmanaged(usize, PhantomNodeInfo),
    scope: ?[]const u8,
    filter: common.FilterOptions,
    ghost_map: *const std.AutoHashMapUnmanaged(usize, u32),
    num_buf: *[20]u8,
) !void {
    const MermaidEdge = struct {
        edge_type: EdgeType,
        source_order: u64,
        target_order: u64,
        source_idx: usize,
        target_idx: usize,
        target_is_ghost: bool,
        target_is_phantom: bool,
    };

    var entries = std.ArrayListUnmanaged(MermaidEdge){};
    defer entries.deinit(allocator);

    for (g.edges.items) |e| {
        // Skip similar_to and exports per spec.
        if (e.edge_type == .similar_to or e.edge_type == .exports) continue;

        const src_idx = @intFromEnum(e.source_id);
        const tgt_idx = @intFromEnum(e.target_id);
        if (src_idx >= g.nodes.items.len or tgt_idx >= g.nodes.items.len) continue;

        // Source must have an ID.
        const src_id = ids[src_idx] orelse continue;

        // Scope filtering: source must be in scope.
        if (scope) |s| {
            if (!inScope(g.nodes.items[src_idx].file_path, s)) continue;
        }

        const tgt_is_phantom = !isInternal(g.nodes.items[tgt_idx]);
        const tgt_is_ghost = ghost_map.contains(tgt_idx);
        const tgt_has_id = ids[tgt_idx] != null;

        // Target must be resolvable: has an ID, is phantom, or is a ghost.
        if (!tgt_has_id and !tgt_is_phantom and !tgt_is_ghost) continue;

        if (!filter.include_external_nodes) {
            const src_is_phantom = !isInternal(g.nodes.items[src_idx]);
            if (src_is_phantom or tgt_is_phantom) continue;
        }

        const src_order = prefixOrder(src_id.prefix) * @as(u64, 1 << 32) + src_id.num;
        var tgt_order: u64 = 0;
        if (tgt_is_ghost) {
            if (ghost_map.get(tgt_idx)) |gnum| {
                // Ghost nodes sort after everything else.
                tgt_order = 10 * @as(u64, 1 << 32) + gnum;
            }
        } else if (ids[tgt_idx]) |tgt_id| {
            tgt_order = prefixOrder(tgt_id.prefix) * @as(u64, 1 << 32) + tgt_id.num;
        } else if (tgt_is_phantom) {
            if (phantom_lookup.get(tgt_idx)) |pi| {
                tgt_order = prefixOrder("x:") * @as(u64, 1 << 32) + pi.pkg_x_num;
            }
        }

        try entries.append(allocator, .{
            .edge_type = e.edge_type,
            .source_order = src_order,
            .target_order = tgt_order,
            .source_idx = src_idx,
            .target_idx = tgt_idx,
            .target_is_ghost = tgt_is_ghost,
            .target_is_phantom = tgt_is_phantom,
        });
    }

    // Sort: type (alpha) → source → target.
    std.mem.sort(MermaidEdge, entries.items, {}, struct {
        fn lessThan(_: void, a: MermaidEdge, b: MermaidEdge) bool {
            const a_key = edgeTypeSortKey(a.edge_type);
            const b_key = edgeTypeSortKey(b.edge_type);
            if (a_key != b_key) return a_key < b_key;
            if (a.source_order != b.source_order) return a.source_order < b.source_order;
            return a.target_order < b.target_order;
        }
    }.lessThan);

    for (entries.items) |entry| {
        try out.appendSlice(allocator, "    ");

        // Source ID.
        const src_id = ids[entry.source_idx].?;
        try appendMermaidId(out, allocator, src_id.prefix, src_id.num, num_buf);

        // Arrow style.
        const arrow = mermaidArrow(entry.edge_type);
        try out.append(allocator, ' ');
        try out.appendSlice(allocator, arrow);
        try out.append(allocator, ' ');

        // Target ID.
        if (entry.target_is_ghost) {
            if (ghost_map.get(entry.target_idx)) |gnum| {
                try out.appendSlice(allocator, "g_");
                try appendNum(out, allocator, gnum, num_buf);
            }
        } else if (ids[entry.target_idx]) |tgt_id| {
            try appendMermaidId(out, allocator, tgt_id.prefix, tgt_id.num, num_buf);
        } else if (entry.target_is_phantom) {
            if (phantom_lookup.get(entry.target_idx)) |pi| {
                try out.appendSlice(allocator, "x_");
                try appendNum(out, allocator, pi.pkg_x_num, num_buf);
                try out.append(allocator, '_');
                try appendDotToUnderscore(out, allocator, pi.symbol_path);
            }
        }

        try out.append(allocator, '\n');
    }
}

fn mermaidArrow(et: EdgeType) []const u8 {
    return switch (et) {
        .calls, .uses_type => "-->",
        .imports => "-.->",
        .implements => "==>",
        .similar_to, .exports => "-->", // not rendered, but provide a fallback
    };
}

// ---------------------------------------------------------------------------
// Class assignment rendering
// ---------------------------------------------------------------------------

pub fn renderClassAssignments(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    ids: []const ?IdEntry,
    phantom_packages: []const PhantomPackage,
    ghost_map: *const std.AutoHashMapUnmanaged(usize, u32),
    num_buf: *[20]u8,
) !void {
    // Collect node IDs per style class.
    const StyleClass = enum { const_style, en_style, err_style, fn_style, ghost_style, phantom_style, st_style, test_style };

    const ClassEntry = struct {
        style: StyleClass,
        sort_order: u64,
        node_idx: usize,
        is_phantom_symbol: bool,
        phantom_pkg_num: u32,
        phantom_path: []const u8,
        is_ghost: bool,
        ghost_num: u32,
    };

    var entries = std.ArrayListUnmanaged(ClassEntry){};
    defer entries.deinit(allocator);

    // Internal nodes with IDs.
    for (g.nodes.items, 0..) |n, i| {
        const id_entry = ids[i] orelse continue;
        if (n.kind == .file or n.kind == .field or n.kind == .import_decl) continue;

        const style: StyleClass = switch (n.kind) {
            .function => .fn_style,
            .type_def => .st_style,
            .enum_def => .en_style,
            .constant => .const_style,
            .test_def => .test_style,
            .error_def => .err_style,
            .module => .fn_style, // modules get fn_style as fallback
            else => continue,
        };

        if (!isInternal(n)) continue;

        try entries.append(allocator, .{
            .style = style,
            .sort_order = prefixOrder(id_entry.prefix) * @as(u64, 1 << 32) + id_entry.num,
            .node_idx = i,
            .is_phantom_symbol = false,
            .phantom_pkg_num = 0,
            .phantom_path = "",
            .is_ghost = false,
            .ghost_num = 0,
        });
    }

    // Phantom symbols.
    for (phantom_packages) |pkg| {
        for (pkg.symbols.items) |sym| {
            try entries.append(allocator, .{
                .style = .phantom_style,
                .sort_order = prefixOrder("x:") * @as(u64, 1 << 32) + pkg.x_num,
                .node_idx = sym.node_idx,
                .is_phantom_symbol = true,
                .phantom_pkg_num = pkg.x_num,
                .phantom_path = sym.qualified_path,
                .is_ghost = false,
                .ghost_num = 0,
            });
        }
    }

    // Ghost nodes.
    var ghost_it = ghost_map.iterator();
    while (ghost_it.next()) |entry| {
        try entries.append(allocator, .{
            .style = .ghost_style,
            .sort_order = 10 * @as(u64, 1 << 32) + entry.value_ptr.*,
            .node_idx = entry.key_ptr.*,
            .is_phantom_symbol = false,
            .phantom_pkg_num = 0,
            .phantom_path = "",
            .is_ghost = true,
            .ghost_num = entry.value_ptr.*,
        });
    }

    // Sort by style name, then by sort_order within each style.
    std.mem.sort(ClassEntry, entries.items, {}, struct {
        fn lessThan(_: void, a: ClassEntry, b: ClassEntry) bool {
            const a_ord = @intFromEnum(a.style);
            const b_ord = @intFromEnum(b.style);
            if (a_ord != b_ord) return a_ord < b_ord;
            return a.sort_order < b.sort_order;
        }
    }.lessThan);

    // Render one "class id1,id2 style_name" line per style.
    const style_names = [_][]const u8{
        "const_style", "en_style", "err_style", "fn_style",
        "ghost_style", "phantom_style", "st_style", "test_style",
    };

    for (style_names, 0..) |style_name, style_idx| {
        const target_style: StyleClass = @enumFromInt(style_idx);

        // Collect IDs for this style.
        var id_strings = std.ArrayListUnmanaged(u8){};
        defer id_strings.deinit(allocator);
        var count: usize = 0;

        for (entries.items) |entry| {
            if (entry.style != target_style) continue;

            if (count > 0) {
                try id_strings.append(allocator, ',');
            }

            if (entry.is_ghost) {
                try id_strings.appendSlice(allocator, "g_");
                const s = std.fmt.bufPrint(num_buf, "{d}", .{entry.ghost_num}) catch unreachable;
                try id_strings.appendSlice(allocator, s);
            } else if (entry.is_phantom_symbol) {
                try id_strings.appendSlice(allocator, "x_");
                const s = std.fmt.bufPrint(num_buf, "{d}", .{entry.phantom_pkg_num}) catch unreachable;
                try id_strings.appendSlice(allocator, s);
                try id_strings.append(allocator, '_');
                for (entry.phantom_path) |c| {
                    if (c == '.') {
                        try id_strings.append(allocator, '_');
                    } else {
                        try id_strings.append(allocator, c);
                    }
                }
            } else if (ids[entry.node_idx]) |id| {
                // Regular internal node.
                if (id.prefix.len > 0 and id.prefix[id.prefix.len - 1] == ':') {
                    try id_strings.appendSlice(allocator, id.prefix[0 .. id.prefix.len - 1]);
                } else {
                    try id_strings.appendSlice(allocator, id.prefix);
                }
                try id_strings.append(allocator, '_');
                const s = std.fmt.bufPrint(num_buf, "{d}", .{id.num}) catch unreachable;
                try id_strings.appendSlice(allocator, s);
            }

            count += 1;
        }

        if (count > 0) {
            try out.appendSlice(allocator, "    class ");
            try out.appendSlice(allocator, id_strings.items);
            try out.append(allocator, ' ');
            try out.appendSlice(allocator, style_name);
            try out.append(allocator, '\n');
        }
    }
}

// ---------------------------------------------------------------------------
// Shared Mermaid utilities
// ---------------------------------------------------------------------------

/// Render a Mermaid-safe ID from a CTG-style prefix and number.
/// Replaces ":" with "_" so "fn:1" becomes "fn_1".
fn appendMermaidId(out: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, prefix: []const u8, num: u32, buf: *[20]u8) !void {
    // Strip trailing colon and append underscore.
    if (prefix.len > 0 and prefix[prefix.len - 1] == ':') {
        try out.appendSlice(allocator, prefix[0 .. prefix.len - 1]);
    } else {
        try out.appendSlice(allocator, prefix);
    }
    try out.append(allocator, '_');
    try appendNum(out, allocator, num, buf);
}

/// Append a string with Mermaid-special characters escaped.
fn appendEscaped(out: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, s: []const u8) !void {
    var start: usize = 0;
    for (s, 0..) |c, i| {
        const replacement: ?[]const u8 = switch (c) {
            '"' => "&quot;",
            '<' => "&lt;",
            '>' => "&gt;",
            else => null,
        };
        if (replacement) |r| {
            if (i > start) try out.appendSlice(allocator, s[start..i]);
            try out.appendSlice(allocator, r);
            start = i + 1;
        }
    }
    if (start < s.len) try out.appendSlice(allocator, s[start..]);
}

/// Append a string but replace dots with underscores (for Mermaid IDs).
fn appendDotToUnderscore(out: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, s: []const u8) !void {
    var start: usize = 0;
    for (s, 0..) |c, i| {
        if (c == '.') {
            if (i > start) try out.appendSlice(allocator, s[start..i]);
            try out.append(allocator, '_');
            start = i + 1;
        }
    }
    if (start < s.len) try out.appendSlice(allocator, s[start..]);
}
