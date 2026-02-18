const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const types = @import("../core/types.zig");
const common = @import("common.zig");

const Graph = graph_mod.Graph;
const NodeId = types.NodeId;
const EdgeType = types.EdgeType;

const IdEntry = common.IdEntry;
const SortableNode = common.SortableNode;
const PhantomPackage = common.PhantomPackage;

const isInternal = common.isInternal;
const inScope = common.inScope;
const appendNum = common.appendNum;
const edgeTypeName = common.edgeTypeName;
const prefixOrder = common.prefixOrder;
const findFileId = common.findFileId;
const findPhantomPackageForNode = common.findPhantomPackageForNode;

pub fn renderFilesSection(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    file_indices: []const usize,
    ids: []const ?IdEntry,
    num_buf: *[20]u8,
) !void {
    if (file_indices.len == 0) return;
    try out.appendSlice(allocator, "[files]\n");
    for (file_indices) |fi| {
        const n = g.nodes.items[fi];
        const id = ids[fi].?;
        try out.appendSlice(allocator, id.prefix);
        try appendNum(out, allocator, id.num, num_buf);
        try out.append(allocator, ' ');
        try out.appendSlice(allocator, n.file_path orelse n.name);
        try out.append(allocator, ' ');
        try appendNum(out, allocator, n.line_end orelse 0, num_buf);
        try out.appendSlice(allocator, "L\n");
    }
}

pub fn renderStructsSection(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    struct_indices: []const usize,
    ids: []const ?IdEntry,
    num_buf: *[20]u8,
) !void {
    if (struct_indices.len == 0) return;
    try out.appendSlice(allocator, "[structs]\n");
    for (struct_indices) |si| {
        const n = g.nodes.items[si];
        const id = ids[si].?;
        const file_id = findFileId(g, n, ids);

        try out.appendSlice(allocator, id.prefix);
        try appendNum(out, allocator, id.num, num_buf);
        try out.append(allocator, ' ');
        try out.appendSlice(allocator, n.name);
        if (file_id) |fid| {
            try out.appendSlice(allocator, " f:");
            try appendNum(out, allocator, fid, num_buf);
            try out.append(allocator, ':');
            try appendNum(out, allocator, n.line_start orelse 0, num_buf);
        }
        if (n.visibility == .public) {
            try out.appendSlice(allocator, " pub");
        }
        try out.append(allocator, '\n');

        try renderStructChildren(out, allocator, g, @enumFromInt(si));
    }
}

fn renderStructChildren(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    struct_id: NodeId,
) !void {
    var fields = std.ArrayListUnmanaged(SortableNode){};
    defer fields.deinit(allocator);
    var methods = std.ArrayListUnmanaged(SortableNode){};
    defer methods.deinit(allocator);

    for (g.nodes.items, 0..) |n, i| {
        if (n.parent_id) |pid| {
            if (pid == struct_id) {
                const entry = SortableNode{
                    .node_idx = i,
                    .line_start = n.line_start orelse 0,
                };
                if (n.kind == .field) {
                    try fields.append(allocator, entry);
                } else if (n.kind == .function) {
                    try methods.append(allocator, entry);
                }
            }
        }
    }

    const sortFn = struct {
        fn lessThan(_: void, a: SortableNode, b: SortableNode) bool {
            return a.line_start < b.line_start;
        }
    }.lessThan;
    std.mem.sort(SortableNode, fields.items, {}, sortFn);
    std.mem.sort(SortableNode, methods.items, {}, sortFn);

    if (fields.items.len > 0) {
        try out.appendSlice(allocator, "  ");
        for (fields.items, 0..) |f, i| {
            const n = g.nodes.items[f.node_idx];
            if (i > 0) try out.append(allocator, ' ');
            try out.append(allocator, '.');
            try out.appendSlice(allocator, n.name);
        }
        try out.append(allocator, '\n');
    }

    if (methods.items.len > 0) {
        try out.appendSlice(allocator, "  ");
        for (methods.items, 0..) |m, i| {
            const n = g.nodes.items[m.node_idx];
            if (i > 0) try out.append(allocator, ' ');
            try out.appendSlice(allocator, "fn ");
            try out.appendSlice(allocator, n.name);
            try out.appendSlice(allocator, "()");
        }
        try out.append(allocator, '\n');
    }
}

pub fn renderEnumsSection(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    enum_indices: []const usize,
    ids: []const ?IdEntry,
    num_buf: *[20]u8,
) !void {
    if (enum_indices.len == 0) return;
    try out.appendSlice(allocator, "[enums]\n");
    for (enum_indices) |ei| {
        const n = g.nodes.items[ei];
        const id = ids[ei].?;
        const file_id = findFileId(g, n, ids);

        try out.appendSlice(allocator, id.prefix);
        try appendNum(out, allocator, id.num, num_buf);
        try out.append(allocator, ' ');
        try out.appendSlice(allocator, n.name);
        if (file_id) |fid| {
            try out.appendSlice(allocator, " f:");
            try appendNum(out, allocator, fid, num_buf);
            try out.append(allocator, ':');
            try appendNum(out, allocator, n.line_start orelse 0, num_buf);
        }
        if (n.visibility == .public) {
            try out.appendSlice(allocator, " pub");
        }
        try out.append(allocator, '\n');
    }
}

pub fn renderFunctionsSection(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    fn_indices: []const usize,
    ids: []const ?IdEntry,
    num_buf: *[20]u8,
) !void {
    if (fn_indices.len == 0) return;
    try out.appendSlice(allocator, "[functions]\n");
    for (fn_indices) |fi| {
        const n = g.nodes.items[fi];
        const id = ids[fi].?;
        const file_id = findFileId(g, n, ids);

        try out.appendSlice(allocator, id.prefix);
        try appendNum(out, allocator, id.num, num_buf);
        try out.append(allocator, ' ');
        try out.appendSlice(allocator, n.name);
        try out.appendSlice(allocator, "()");

        if (file_id) |fid| {
            try out.appendSlice(allocator, " f:");
            try appendNum(out, allocator, fid, num_buf);
            try out.append(allocator, ':');
            try appendNum(out, allocator, n.line_start orelse 0, num_buf);
        }
        if (n.visibility == .public) {
            try out.appendSlice(allocator, " pub");
        }
        try out.append(allocator, '\n');
    }
}

pub fn renderConstantsSection(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    const_indices: []const usize,
    ids: []const ?IdEntry,
    num_buf: *[20]u8,
) !void {
    if (const_indices.len == 0) return;
    try out.appendSlice(allocator, "[constants]\n");
    for (const_indices) |ci| {
        const n = g.nodes.items[ci];
        const id = ids[ci].?;
        const file_id = findFileId(g, n, ids);

        try out.appendSlice(allocator, id.prefix);
        try appendNum(out, allocator, id.num, num_buf);
        try out.append(allocator, ' ');
        try out.appendSlice(allocator, n.name);

        if (file_id) |fid| {
            try out.appendSlice(allocator, " f:");
            try appendNum(out, allocator, fid, num_buf);
            try out.append(allocator, ':');
            try appendNum(out, allocator, n.line_start orelse 0, num_buf);
        }
        if (n.visibility == .public) {
            try out.appendSlice(allocator, " pub");
        }
        try out.append(allocator, '\n');
    }
}

pub fn renderErrorsSection(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    err_indices: []const usize,
    ids: []const ?IdEntry,
    num_buf: *[20]u8,
) !void {
    if (err_indices.len == 0) return;
    try out.appendSlice(allocator, "[errors]\n");
    for (err_indices) |ei| {
        const n = g.nodes.items[ei];
        const id = ids[ei].?;
        const file_id = findFileId(g, n, ids);

        try out.appendSlice(allocator, id.prefix);
        try appendNum(out, allocator, id.num, num_buf);
        try out.append(allocator, ' ');
        try out.appendSlice(allocator, n.name);
        if (file_id) |fid| {
            try out.appendSlice(allocator, " f:");
            try appendNum(out, allocator, fid, num_buf);
            try out.append(allocator, ':');
            try appendNum(out, allocator, n.line_start orelse 0, num_buf);
        }
        try out.append(allocator, '\n');
    }
}

pub fn renderTestsSection(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    test_indices: []const usize,
    ids: []const ?IdEntry,
    num_buf: *[20]u8,
) !void {
    if (test_indices.len == 0) return;
    try out.appendSlice(allocator, "[tests]\n");
    for (test_indices) |ti| {
        const n = g.nodes.items[ti];
        const id = ids[ti].?;
        const file_id = findFileId(g, n, ids);

        try out.appendSlice(allocator, id.prefix);
        try appendNum(out, allocator, id.num, num_buf);
        try out.appendSlice(allocator, " \"");
        try out.appendSlice(allocator, n.name);
        try out.append(allocator, '"');
        if (file_id) |fid| {
            try out.appendSlice(allocator, " f:");
            try appendNum(out, allocator, fid, num_buf);
            try out.append(allocator, ':');
            try appendNum(out, allocator, n.line_start orelse 0, num_buf);
        }
        const lines = if (n.line_end != null and n.line_start != null)
            n.line_end.? - n.line_start.? + 1
        else
            0;
        try out.append(allocator, ' ');
        try appendNum(out, allocator, lines, num_buf);
        try out.appendSlice(allocator, "L\n");
    }
}

pub fn renderExternalsSection(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    phantom_packages: []const PhantomPackage,
) !void {
    if (phantom_packages.len == 0) return;
    try out.appendSlice(allocator, "[externals]\n");
    var num_buf: [20]u8 = undefined;
    for (phantom_packages) |pkg| {
        const n = g.nodes.items[pkg.root_idx];
        try out.appendSlice(allocator, "x:");
        try appendNum(out, allocator, pkg.x_num, &num_buf);
        try out.append(allocator, ' ');
        try out.appendSlice(allocator, n.name);
        switch (n.external) {
            .stdlib => try out.appendSlice(allocator, " (stdlib)"),
            .dependency => |dep| {
                try out.appendSlice(allocator, " (dependency)");
                if (dep.version) |ver| {
                    try out.append(allocator, ' ');
                    try out.appendSlice(allocator, ver);
                }
            },
            .none => {},
        }
        try out.append(allocator, '\n');
        for (pkg.symbols.items) |sym| {
            try out.appendSlice(allocator, "  ");
            try out.appendSlice(allocator, sym.qualified_path);
            try out.append(allocator, '\n');
        }
    }
}

pub fn renderEdgesSection(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    ids: []const ?IdEntry,
    phantom_packages: []const PhantomPackage,
    scope: ?[]const u8,
    filter: common.FilterOptions,
    num_buf: *[20]u8,
) !void {
    const EdgeEntry = struct {
        edge_type: EdgeType,
        source_str_order: u64,
        target_str_order: u64,
        source_idx: usize,
        target_idx: usize,
    };

    var entries = std.ArrayListUnmanaged(EdgeEntry){};
    defer entries.deinit(allocator);

    for (g.edges.items) |e| {
        const src_idx = @intFromEnum(e.source_id);
        const tgt_idx = @intFromEnum(e.target_id);
        if (src_idx >= g.nodes.items.len or tgt_idx >= g.nodes.items.len) continue;

        const src_id = ids[src_idx] orelse continue;
        const tgt_has_id = ids[tgt_idx] != null;
        const tgt_is_phantom = !isInternal(g.nodes.items[tgt_idx]);

        if (!tgt_has_id and !tgt_is_phantom) continue;

        if (!filter.include_external_nodes) {
            const src_is_phantom = !isInternal(g.nodes.items[src_idx]);
            if (src_is_phantom or tgt_is_phantom) continue;
        }

        if (scope) |s| {
            if (!inScope(g.nodes.items[src_idx].file_path, s)) continue;
        }

        const src_order = prefixOrder(src_id.prefix) * @as(u64, 1 << 32) + src_id.num;
        var tgt_order: u64 = 0;
        if (ids[tgt_idx]) |tgt_id| {
            tgt_order = prefixOrder(tgt_id.prefix) * @as(u64, 1 << 32) + tgt_id.num;
        } else if (tgt_is_phantom) {
            const pkg_info = findPhantomPackageForNode(g, tgt_idx, phantom_packages);
            if (pkg_info) |pi| {
                tgt_order = prefixOrder("x:") * @as(u64, 1 << 32) + pi.pkg_x_num;
            }
        }

        try entries.append(allocator, .{
            .edge_type = e.edge_type,
            .source_str_order = src_order,
            .target_str_order = tgt_order,
            .source_idx = src_idx,
            .target_idx = tgt_idx,
        });
    }

    std.mem.sort(EdgeEntry, entries.items, {}, struct {
        fn lessThan(_: void, a: EdgeEntry, b: EdgeEntry) bool {
            const a_type = edgeTypeName(a.edge_type);
            const b_type = edgeTypeName(b.edge_type);
            const type_cmp = std.mem.order(u8, a_type, b_type);
            if (type_cmp != .eq) return type_cmp == .lt;
            if (a.source_str_order != b.source_str_order) return a.source_str_order < b.source_str_order;
            return a.target_str_order < b.target_str_order;
        }
    }.lessThan);

    if (entries.items.len == 0) return;
    try out.appendSlice(allocator, "[edges]\n");

    var i: usize = 0;
    while (i < entries.items.len) {
        const group_edge_type = entries.items[i].edge_type;
        const group_source_idx = entries.items[i].source_idx;

        // Write source ID and edge type
        const src_id = ids[group_source_idx].?;
        try out.appendSlice(allocator, src_id.prefix);
        try appendNum(out, allocator, src_id.num, num_buf);
        try out.append(allocator, ' ');
        try out.appendSlice(allocator, edgeTypeName(group_edge_type));

        // Write all targets in this group
        while (i < entries.items.len and
            entries.items[i].edge_type == group_edge_type and
            entries.items[i].source_idx == group_source_idx)
        {
            const entry = entries.items[i];
            try out.append(allocator, ' ');

            if (ids[entry.target_idx]) |tgt_id| {
                try out.appendSlice(allocator, tgt_id.prefix);
                try appendNum(out, allocator, tgt_id.num, num_buf);
            } else {
                const pkg_info = findPhantomPackageForNode(g, entry.target_idx, phantom_packages);
                if (pkg_info) |pi| {
                    try out.appendSlice(allocator, "x:");
                    try appendNum(out, allocator, pi.pkg_x_num, num_buf);
                    try out.append(allocator, ':');
                    try out.appendSlice(allocator, pi.symbol_path);
                } else {
                    try out.appendSlice(allocator, "x:?");
                }
            }

            i += 1;
        }

        try out.append(allocator, '\n');
    }
}
