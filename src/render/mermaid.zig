const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const node_mod = @import("../core/node.zig");
const edge_mod = @import("../core/edge.zig");
const types = @import("../core/types.zig");
const lang = @import("../languages/language.zig");
const common = @import("common.zig");

const Graph = graph_mod.Graph;
const Node = node_mod.Node;
const Edge = edge_mod.Edge;
const NodeId = types.NodeId;
const NodeKind = types.NodeKind;
const EdgeType = types.EdgeType;
const Visibility = types.Visibility;
const ExternalInfo = lang.ExternalInfo;

const IdEntry = common.IdEntry;
const PhantomPackage = common.PhantomPackage;
const PhantomNodeInfo = common.PhantomNodeInfo;

const isInternal = common.isInternal;
const inScope = common.inScope;
const appendNum = common.appendNum;
const appendU32 = common.appendU32;
const appendCurrentTimestamp = common.appendCurrentTimestamp;
const edgeTypeName = common.edgeTypeName;
const prefixOrder = common.prefixOrder;
const findPhantomPackageForNode = common.findPhantomPackageForNode;

/// Options controlling Mermaid flowchart rendering output.
pub const MermaidOptions = struct {
    project_name: []const u8,
    scope: ?[]const u8 = null,
    /// For deterministic tests; null = use system clock.
    timestamp: ?[]const u8 = null,
};

/// Render the graph as a Mermaid flowchart.
///
/// The output is deterministic: same graph + same options = byte-identical output.
/// See `docs/zcodeprism-mermaid-spec.md` for the full format specification.
pub fn renderMermaid(
    allocator: std.mem.Allocator,
    g: *const Graph,
    options: MermaidOptions,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    var assignment = try common.buildIdAssignment(allocator, g, options.scope);
    defer assignment.deinit(allocator);

    const ids = assignment.ids;
    const file_count = assignment.file_indices.items.len;
    const fn_count = assignment.fn_indices.items.len;
    const struct_count = assignment.struct_indices.items.len;
    const enum_count = assignment.enum_indices.items.len;
    const const_count = assignment.const_indices.items.len;
    const test_count = assignment.test_indices.items.len;
    const external_count = assignment.phantom_packages.items.len;

    var num_buf: [20]u8 = undefined;

    // Header: 3 comment lines.
    try out.appendSlice(allocator, "%% zcodeprism mermaid \xe2\x80\x94 ");
    try out.appendSlice(allocator, options.project_name);
    try out.append(allocator, '\n');

    try out.appendSlice(allocator, "%% ");
    try appendNum(out, allocator, file_count, &num_buf);
    try out.appendSlice(allocator, " files, ");
    try appendNum(out, allocator, fn_count, &num_buf);
    try out.appendSlice(allocator, " functions, ");
    try appendNum(out, allocator, struct_count, &num_buf);
    try out.appendSlice(allocator, " structs, ");
    try appendNum(out, allocator, enum_count, &num_buf);
    try out.appendSlice(allocator, " enums, ");
    try appendNum(out, allocator, const_count, &num_buf);
    try out.appendSlice(allocator, " constants, ");
    try appendNum(out, allocator, test_count, &num_buf);
    try out.appendSlice(allocator, " tests, ");
    try appendNum(out, allocator, external_count, &num_buf);
    try out.appendSlice(allocator, " externals\n");

    try out.appendSlice(allocator, "%% generated ");
    if (options.timestamp) |ts| {
        try out.appendSlice(allocator, ts);
    } else {
        try appendCurrentTimestamp(out, allocator);
    }
    try out.append(allocator, '\n');

    // Flowchart directive.
    try out.appendSlice(allocator, "\nflowchart TB\n");

    // Class definitions (alphabetical).
    try out.appendSlice(allocator, "    %% === Class definitions ===\n");
    try out.appendSlice(allocator, "    classDef const_style fill:#e8d5f5,stroke:#7b2d8e,color:#4a1a5e\n");
    try out.appendSlice(allocator, "    classDef en_style fill:#fef3cd,stroke:#c09853,color:#6d5a2e\n");
    try out.appendSlice(allocator, "    classDef err_style fill:#e2e3e5,stroke:#6c757d,color:#3d4248\n");
    try out.appendSlice(allocator, "    classDef fn_style fill:#d6eaf8,stroke:#2e6da4,color:#1a3d5c\n");
    try out.appendSlice(allocator, "    classDef ghost_style fill:#ffffff,stroke:#cccccc,stroke-dasharray:3 3,color:#999999\n");
    try out.appendSlice(allocator, "    classDef phantom_style fill:#f0f0f0,stroke:#999999,stroke-dasharray:5 5,color:#888888\n");
    try out.appendSlice(allocator, "    classDef st_style fill:#d4edda,stroke:#28a745,color:#155724\n");
    try out.appendSlice(allocator, "    classDef test_style fill:#f8d7da,stroke:#c0392b,color:#721c24\n");

    // File subgraphs.
    try out.appendSlice(allocator, "\n    %% === File subgraphs ===\n");
    try renderFileSubgraphs(out, allocator, g, assignment.file_indices.items, ids, &num_buf);

    // Phantom subgraphs.
    try out.appendSlice(allocator, "\n    %% === Phantom subgraphs ===\n");
    try renderPhantomSubgraphs(out, allocator, g, assignment.phantom_packages.items, &num_buf);

    // Ghost nodes (only when scope is active).
    try out.appendSlice(allocator, "\n    %% === Ghost nodes ===\n");
    var ghost_map = std.AutoHashMapUnmanaged(usize, u32){};
    defer ghost_map.deinit(allocator);
    if (options.scope) |scope| {
        try buildGhostNodes(allocator, g, ids, scope, &ghost_map);
        try renderGhostNodes(out, allocator, g, &ghost_map, &num_buf);
    }

    // Edges.
    try out.appendSlice(allocator, "\n    %% === Edges ===\n");
    try renderEdges(out, allocator, g, ids, assignment.phantom_packages.items, options.scope, &ghost_map, &num_buf);

    // Class assignments.
    try out.appendSlice(allocator, "\n    %% === Class assignments ===\n");
    try renderClassAssignments(out, allocator, g, ids, assignment.phantom_packages.items, &ghost_map, &num_buf);
}

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
    try appendU32(out, allocator, num, buf);
}

fn renderFileSubgraphs(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    file_indices: []const usize,
    ids: []const ?IdEntry,
    num_buf: *[20]u8,
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

        // Collect all nodes belonging to this file (direct children and nested).
        var file_members = std.ArrayListUnmanaged(MermaidNode){};
        defer file_members.deinit(allocator);

        for (g.nodes.items, 0..) |n, i| {
            if (!isInternal(n)) continue;
            if (n.kind == .file) continue;
            if (n.kind == .field or n.kind == .import_decl) continue;
            const id_entry = ids[i] orelse continue;

            // Check if this node belongs to this file by walking parent chain.
            if (belongsToFile(g, @enumFromInt(i), @enumFromInt(fi))) {
                try file_members.append(allocator, .{
                    .node_idx = i,
                    .id = id_entry,
                });
            }
        }

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

/// Check if a node ultimately belongs to a given file by walking its parent chain.
fn belongsToFile(g: *const Graph, node_id: NodeId, file_id: NodeId) bool {
    var current = node_id;
    while (true) {
        if (current == file_id) return true;
        const idx = @intFromEnum(current);
        if (idx >= g.nodes.items.len) return false;
        const n = g.nodes.items[idx];
        if (n.parent_id) |pid| {
            current = pid;
        } else {
            return false;
        }
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

/// Append a string with Mermaid-special characters escaped.
fn appendEscaped(out: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, s: []const u8) !void {
    for (s) |c| {
        switch (c) {
            '"' => try out.appendSlice(allocator, "&quot;"),
            '<' => try out.appendSlice(allocator, "&lt;"),
            '>' => try out.appendSlice(allocator, "&gt;"),
            else => try out.append(allocator, c),
        }
    }
}

fn renderPhantomSubgraphs(
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
            try appendU32(out, allocator, pkg.x_num, num_buf);
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

/// Append a string but replace dots with underscores (for Mermaid IDs).
fn appendDotToUnderscore(out: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, s: []const u8) !void {
    for (s) |c| {
        if (c == '.') {
            try out.append(allocator, '_');
        } else {
            try out.append(allocator, c);
        }
    }
}

/// Build a map from out-of-scope internal node index → ghost ID number.
fn buildGhostNodes(
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

fn renderGhostNodes(
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
        try appendU32(out, allocator, entry.ghost_num, num_buf);
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

fn renderEdges(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    ids: []const ?IdEntry,
    phantom_packages: []const PhantomPackage,
    scope: ?[]const u8,
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
            const pkg_info = findPhantomPackageForNode(g, tgt_idx, phantom_packages);
            if (pkg_info) |pi| {
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
            const a_type = edgeTypeName(a.edge_type);
            const b_type = edgeTypeName(b.edge_type);
            const type_cmp = std.mem.order(u8, a_type, b_type);
            if (type_cmp != .eq) return type_cmp == .lt;
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
                try appendU32(out, allocator, gnum, num_buf);
            }
        } else if (ids[entry.target_idx]) |tgt_id| {
            try appendMermaidId(out, allocator, tgt_id.prefix, tgt_id.num, num_buf);
        } else if (entry.target_is_phantom) {
            const pkg_info = findPhantomPackageForNode(g, entry.target_idx, phantom_packages);
            if (pkg_info) |pi| {
                try out.appendSlice(allocator, "x_");
                try appendU32(out, allocator, pi.pkg_x_num, num_buf);
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

fn renderClassAssignments(
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

// --- Test helpers ---

/// Creates a diverse graph exercising all Mermaid sections.
///
/// Same structure as the CTG test graph:
/// - 2 file nodes: src/lib.zig (100L), src/main.zig (50L)
/// - 1 struct (Tokenizer) under lib.zig with 1 field child + 1 method child
/// - 1 enum (TokenKind) under lib.zig
/// - 2 top-level functions under main.zig (1 pub main, 1 private helper)
/// - 1 constant (MAX_SIZE) under main.zig
/// - 1 error_def (ParseError) under lib.zig
/// - 1 test_def under main.zig
/// - 2 phantom nodes: std (module, stdlib) + Allocator (type_def, stdlib, parent=std)
/// - 5 edges: imports, 2x calls, 2x uses_type
fn createMermaidTestGraph(allocator: std.mem.Allocator) !Graph {
    var g = Graph.init(allocator, "/tmp/project");
    errdefer g.deinit();

    // File nodes
    const lib_file = try g.addNode(.{
        .id = .root,
        .name = "src/lib.zig",
        .kind = .file,
        .language = .zig,
        .file_path = "src/lib.zig",
        .line_start = 1,
        .line_end = 100,
        .visibility = .public,
    });

    const main_file = try g.addNode(.{
        .id = .root,
        .name = "src/main.zig",
        .kind = .file,
        .language = .zig,
        .file_path = "src/main.zig",
        .line_start = 1,
        .line_end = 50,
        .visibility = .public,
    });

    // Struct: Tokenizer under lib.zig
    const tokenizer_struct = try g.addNode(.{
        .id = .root,
        .name = "Tokenizer",
        .kind = .type_def,
        .language = .zig,
        .file_path = "src/lib.zig",
        .line_start = 5,
        .line_end = 40,
        .parent_id = lib_file,
        .visibility = .public,
        .signature = "pub const Tokenizer = struct",
    });

    // Field: source under Tokenizer
    _ = try g.addNode(.{
        .id = .root,
        .name = "source",
        .kind = .field,
        .language = .zig,
        .file_path = "src/lib.zig",
        .line_start = 6,
        .line_end = 6,
        .parent_id = tokenizer_struct,
        .visibility = .public,
        .signature = "source: []const u8",
    });

    // Method: next under Tokenizer
    const method_next = try g.addNode(.{
        .id = .root,
        .name = "next",
        .kind = .function,
        .language = .zig,
        .file_path = "src/lib.zig",
        .line_start = 10,
        .line_end = 30,
        .parent_id = tokenizer_struct,
        .visibility = .public,
        .signature = "pub fn next(self: *Tokenizer) ?Token",
    });

    // Enum: TokenKind under lib.zig
    _ = try g.addNode(.{
        .id = .root,
        .name = "TokenKind",
        .kind = .enum_def,
        .language = .zig,
        .file_path = "src/lib.zig",
        .line_start = 45,
        .line_end = 55,
        .parent_id = lib_file,
        .visibility = .public,
    });

    // Function: main (pub) under main.zig
    const fn_main = try g.addNode(.{
        .id = .root,
        .name = "main",
        .kind = .function,
        .language = .zig,
        .file_path = "src/main.zig",
        .line_start = 5,
        .line_end = 20,
        .parent_id = main_file,
        .visibility = .public,
        .signature = "pub fn main() void",
        .doc = "/// Entry point.",
    });

    // Function: helper (private) under main.zig
    const fn_helper = try g.addNode(.{
        .id = .root,
        .name = "helper",
        .kind = .function,
        .language = .zig,
        .file_path = "src/main.zig",
        .line_start = 25,
        .line_end = 35,
        .parent_id = main_file,
        .visibility = .private,
        .signature = "fn helper() void",
    });

    // Constant: MAX_SIZE under main.zig
    _ = try g.addNode(.{
        .id = .root,
        .name = "MAX_SIZE",
        .kind = .constant,
        .language = .zig,
        .file_path = "src/main.zig",
        .line_start = 2,
        .line_end = 2,
        .parent_id = main_file,
        .visibility = .public,
        .signature = "pub const MAX_SIZE: usize = 1024",
    });

    // Error: ParseError under lib.zig
    _ = try g.addNode(.{
        .id = .root,
        .name = "ParseError",
        .kind = .error_def,
        .language = .zig,
        .file_path = "src/lib.zig",
        .line_start = 60,
        .line_end = 65,
        .parent_id = lib_file,
    });

    // Test: under main.zig
    _ = try g.addNode(.{
        .id = .root,
        .name = "main works correctly",
        .kind = .test_def,
        .language = .zig,
        .file_path = "src/main.zig",
        .line_start = 40,
        .line_end = 48,
        .parent_id = main_file,
    });

    // Phantom: std (module, stdlib)
    const phantom_std = try g.addNode(.{
        .id = .root,
        .name = "std",
        .kind = .module,
        .language = .zig,
        .external = .{ .stdlib = {} },
    });

    // Phantom: Allocator (type_def, stdlib, parent=std)
    const phantom_allocator = try g.addNode(.{
        .id = .root,
        .name = "Allocator",
        .kind = .type_def,
        .language = .zig,
        .external = .{ .stdlib = {} },
        .parent_id = phantom_std,
    });

    // Edges
    _ = try g.addEdge(.{
        .source_id = main_file,
        .target_id = lib_file,
        .edge_type = .imports,
    });

    _ = try g.addEdge(.{
        .source_id = fn_main,
        .target_id = fn_helper,
        .edge_type = .calls,
    });

    _ = try g.addEdge(.{
        .source_id = fn_main,
        .target_id = method_next,
        .edge_type = .calls,
    });

    _ = try g.addEdge(.{
        .source_id = fn_main,
        .target_id = tokenizer_struct,
        .edge_type = .uses_type,
    });

    _ = try g.addEdge(.{
        .source_id = fn_helper,
        .target_id = phantom_allocator,
        .edge_type = .uses_type,
    });

    return g;
}

// ==========================================================================
// Mermaid Tests: header
// ==========================================================================

test "header line 1 matches spec" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createMermaidTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = MermaidOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderMermaid(allocator, &g, options, &out);

    // Assert: first line: "%% zcodeprism mermaid \xe2\x80\x94 myproject"
    const output = out.items;
    const first_line_end = std.mem.indexOfScalar(u8, output, '\n') orelse output.len;
    const first_line = output[0..first_line_end];
    try std.testing.expectEqualStrings("%% zcodeprism mermaid \xe2\x80\x94 myproject", first_line);
}

test "header line 2 has stats" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createMermaidTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = MermaidOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderMermaid(allocator, &g, options, &out);

    // Assert: second line starts with "%% " and contains "files" and "functions"
    const output = out.items;
    var lines = std.mem.splitScalar(u8, output, '\n');
    _ = lines.next(); // skip line 1
    const line2 = lines.next() orelse return error.MissingLine;
    try std.testing.expect(std.mem.startsWith(u8, line2, "%% "));
    try std.testing.expect(std.mem.indexOf(u8, line2, "files") != null);
    try std.testing.expect(std.mem.indexOf(u8, line2, "functions") != null);
}

test "header line 3 has timestamp" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createMermaidTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = MermaidOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderMermaid(allocator, &g, options, &out);

    // Assert: third line: "%% generated 2026-02-14T10:30:00Z"
    const output = out.items;
    var lines = std.mem.splitScalar(u8, output, '\n');
    _ = lines.next();
    _ = lines.next();
    const line3 = lines.next() orelse return error.MissingLine;
    try std.testing.expectEqualStrings("%% generated 2026-02-14T10:30:00Z", line3);
}

// ==========================================================================
// Mermaid Tests: structure
// ==========================================================================

test "starts with flowchart TB" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createMermaidTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = MermaidOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderMermaid(allocator, &g, options, &out);

    // Assert: first non-comment non-empty line is "flowchart TB"
    const output = out.items;
    var lines = std.mem.splitScalar(u8, output, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        if (std.mem.startsWith(u8, line, "%%")) continue;
        try std.testing.expectEqualStrings("flowchart TB", line);
        break;
    }
}

test "sections appear in correct order" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createMermaidTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = MermaidOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderMermaid(allocator, &g, options, &out);

    // Assert: section markers in order
    const output = out.items;
    const markers = [_][]const u8{
        "%% === Class definitions ===",
        "%% === File subgraphs ===",
        "%% === Phantom subgraphs ===",
        "%% === Edges ===",
        "%% === Class assignments ===",
    };

    var last_pos: usize = 0;
    for (markers) |marker| {
        const pos = std.mem.indexOf(u8, output[last_pos..], marker) orelse
            return error.MissingSection;
        last_pos = last_pos + pos + marker.len;
    }
}

test "classDef styles are alphabetical" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createMermaidTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = MermaidOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderMermaid(allocator, &g, options, &out);

    // Assert: classDef names appear in alphabetical order
    const output = out.items;
    const expected_order = [_][]const u8{
        "classDef const_style",
        "classDef en_style",
        "classDef err_style",
        "classDef fn_style",
        "classDef ghost_style",
        "classDef phantom_style",
        "classDef st_style",
        "classDef test_style",
    };

    var last_pos: usize = 0;
    for (expected_order) |name| {
        const pos = std.mem.indexOf(u8, output[last_pos..], name) orelse
            return error.MissingClassDef;
        last_pos = last_pos + pos + name.len;
    }
}

test "deterministic output" {
    // Arrange
    const allocator = std.testing.allocator;

    var g1 = try createMermaidTestGraph(allocator);
    defer g1.deinit();
    var out1: std.ArrayListUnmanaged(u8) = .{};
    defer out1.deinit(allocator);

    var g2 = try createMermaidTestGraph(allocator);
    defer g2.deinit();
    var out2: std.ArrayListUnmanaged(u8) = .{};
    defer out2.deinit(allocator);

    const options = MermaidOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderMermaid(allocator, &g1, options, &out1);
    try renderMermaid(allocator, &g2, options, &out2);

    // Assert: byte-identical output
    try std.testing.expectEqualSlices(u8, out1.items, out2.items);
}

// ==========================================================================
// Mermaid Tests: node shapes
// ==========================================================================

test "functions use rectangle shape" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createMermaidTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = MermaidOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderMermaid(allocator, &g, options, &out);

    // Assert: output contains rectangle shape for function: fn_N["fn: main"]
    const output = out.items;
    try std.testing.expect(std.mem.indexOf(u8, output, "[\"fn: main\"]") != null);
}

test "structs use subroutine shape" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createMermaidTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = MermaidOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderMermaid(allocator, &g, options, &out);

    // Assert: output contains subroutine shape: st_N[["struct: Tokenizer"]]
    const output = out.items;
    try std.testing.expect(std.mem.indexOf(u8, output, "[[\"struct: Tokenizer\"]]") != null);
}

test "enums use hexagon shape" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createMermaidTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = MermaidOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderMermaid(allocator, &g, options, &out);

    // Assert: output contains hexagon shape: en_N{{"enum: TokenKind"}}
    const output = out.items;
    try std.testing.expect(std.mem.indexOf(u8, output, "{{\"enum: TokenKind\"}}") != null);
}

test "constants use parallelogram shape" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createMermaidTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = MermaidOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderMermaid(allocator, &g, options, &out);

    // Assert: output contains parallelogram shape: c_N[/"const: MAX_SIZE"/]
    const output = out.items;
    try std.testing.expect(std.mem.indexOf(u8, output, "[/\"const: MAX_SIZE\"/]") != null);
}

test "methods labeled with parent type" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createMermaidTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = MermaidOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderMermaid(allocator, &g, options, &out);

    // Assert: method "next" on Tokenizer appears as "fn: Tokenizer.next"
    const output = out.items;
    try std.testing.expect(std.mem.indexOf(u8, output, "fn: Tokenizer.next") != null);
}

// ==========================================================================
// Mermaid Tests: edges
// ==========================================================================

test "calls edges use solid arrow" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createMermaidTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = MermaidOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderMermaid(allocator, &g, options, &out);

    // Assert: calls edges use "-->" syntax
    const output = out.items;
    try std.testing.expect(std.mem.indexOf(u8, output, " --> ") != null);
}

test "imports edges use dotted arrow" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createMermaidTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = MermaidOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderMermaid(allocator, &g, options, &out);

    // Assert: imports edges use "-.->'" syntax
    const output = out.items;
    try std.testing.expect(std.mem.indexOf(u8, output, " -.-> ") != null);
}

// ==========================================================================
// Mermaid Tests: edge cases
// ==========================================================================

test "empty graph renders without crash" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = Graph.init(allocator, "/tmp/project");
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = MermaidOptions{
        .project_name = "empty",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderMermaid(allocator, &g, options, &out);

    // Assert: has header and flowchart directive, no crash
    const output = out.items;
    try std.testing.expect(output.len > 0);
    try std.testing.expect(std.mem.indexOf(u8, output, "flowchart TB") != null);
}

test "phantom nodes in subgraphs" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createMermaidTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = MermaidOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderMermaid(allocator, &g, options, &out);

    // Assert: phantom stdlib node renders as subgraph with "std (stdlib)" label
    // and child nodes use "ext:" prefix
    const output = out.items;
    try std.testing.expect(std.mem.indexOf(u8, output, "\"std (stdlib)\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "ext:") != null);
}

test "scoped export creates ghost nodes" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createMermaidTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    // Scope to src/main. main.zig calls method_next which is in src/lib.zig (out of scope)
    // This should create a ghost node for method_next (internal, not phantom)
    const options = MermaidOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
        .scope = "src/main",
    };

    // Act
    try renderMermaid(allocator, &g, options, &out);

    // Assert: ghost nodes appear with ghost_style
    const output = out.items;
    try std.testing.expect(std.mem.indexOf(u8, output, ":::ghost_style") != null or
        std.mem.indexOf(u8, output, "ghost_style") != null);
    // Ghost node should have "..." suffix per spec
    try std.testing.expect(std.mem.indexOf(u8, output, "...") != null);
}
