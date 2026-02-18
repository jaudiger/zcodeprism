const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const node_mod = @import("../core/node.zig");
const types = @import("../core/types.zig");
const lang = @import("../languages/language.zig");

pub const Graph = graph_mod.Graph;
pub const Node = node_mod.Node;
pub const NodeId = types.NodeId;
pub const NodeKind = types.NodeKind;
pub const EdgeType = types.EdgeType;
pub const Language = types.Language;
pub const Visibility = types.Visibility;
pub const ExternalInfo = lang.ExternalInfo;

/// Entry in the ID assignment table. Each node that gets a renderer ID
/// receives a prefix (e.g., "f:", "fn:") and a sequential number.
pub const IdEntry = struct {
    prefix: []const u8,
    num: u32,
};

/// A node index paired with its sort key for deterministic ordering.
pub const SortableNode = struct {
    node_idx: usize,
    line_start: u32,
};

/// A phantom package root with its assigned x: ID and child symbols.
pub const PhantomPackage = struct {
    root_idx: usize,
    x_num: u32,
    symbols: std.ArrayListUnmanaged(PhantomSymbol),

    pub fn deinit(self: *PhantomPackage, allocator: std.mem.Allocator) void {
        for (self.symbols.items) |sym| allocator.free(sym.qualified_path);
        self.symbols.deinit(allocator);
    }
};

pub const PhantomSymbol = struct {
    node_idx: usize,
    qualified_path: []const u8,
};

/// Result of looking up which phantom package a node belongs to.
pub const PhantomNodeInfo = struct {
    pkg_x_num: u32,
    symbol_path: []const u8,
};

/// Controls which nodes and edges are included in rendered output.
/// Both fields default to `false` — the common case is architecture-focused
/// output that excludes test noise and external dependency clutter.
pub const FilterOptions = struct {
    /// When false, `test_def` nodes receive no IDs, produce no section entries,
    /// and generate no edges. Set to true to include test nodes.
    include_test_nodes: bool = false,
    /// When false, external (phantom) nodes receive no IDs, produce no
    /// section entries ([externals]/subgraphs), and generate no edges.
    /// Set to true to include external nodes.
    include_external_nodes: bool = false,
};

pub fn isInternal(n: Node) bool {
    return switch (n.external) {
        .none => true,
        else => false,
    };
}

pub fn inScope(file_path: ?[]const u8, scope: []const u8) bool {
    const fp = file_path orelse return false;
    return std.mem.startsWith(u8, fp, scope);
}

pub fn appendNum(out: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, val: usize, buf: *[20]u8) !void {
    const s = std.fmt.bufPrint(buf, "{d}", .{val}) catch unreachable;
    try out.appendSlice(allocator, s);
}

pub fn appendU32(out: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, val: u32, buf: *[20]u8) !void {
    const s = std.fmt.bufPrint(buf, "{d}", .{val}) catch unreachable;
    try out.appendSlice(allocator, s);
}

pub fn appendCurrentTimestamp(out: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator) !void {
    const epoch = std.time.timestamp();
    const es = std.time.epoch.EpochSeconds{ .secs = @intCast(@max(0, epoch)) };
    const day = es.getEpochDay();
    const yd = day.calculateYearDay();
    const md = yd.calculateMonthDay();
    const ds = es.getDaySeconds();

    var buf: [25]u8 = undefined;
    const ts = std.fmt.bufPrint(&buf, "{d:0>4}-{d:0>2}-{d:0>2}T{d:0>2}:{d:0>2}:{d:0>2}Z", .{
        yd.year,
        @as(u32, @intFromEnum(md.month)),
        @as(u32, md.day_index) + 1,
        ds.getHoursIntoDay(),
        ds.getMinutesIntoHour(),
        ds.getSecondsIntoMinute(),
    }) catch unreachable;
    try out.appendSlice(allocator, ts);
}

pub fn edgeTypeName(et: EdgeType) []const u8 {
    return switch (et) {
        .calls => "calls",
        .imports => "imports",
        .uses_type => "uses_type",
        .similar_to => "similar_to",
        .exports => "exports",
        .implements => "implements",
    };
}

/// Numeric order for prefix-based sorting of IDs.
pub fn prefixOrder(prefix: []const u8) u64 {
    if (std.mem.eql(u8, prefix, "c:")) return 0;
    if (std.mem.eql(u8, prefix, "en:")) return 1;
    if (std.mem.eql(u8, prefix, "err:")) return 2;
    if (std.mem.eql(u8, prefix, "f:")) return 3;
    if (std.mem.eql(u8, prefix, "fn:")) return 4;
    if (std.mem.eql(u8, prefix, "m:")) return 5;
    if (std.mem.eql(u8, prefix, "st:")) return 6;
    if (std.mem.eql(u8, prefix, "t:")) return 7;
    if (std.mem.eql(u8, prefix, "x:")) return 8;
    return 9;
}

/// Walk up the parent chain to find the file node and return its assigned ID number.
pub fn findFileId(g: *const Graph, n: Node, ids: []const ?IdEntry) ?u32 {
    var current_id = n.parent_id;
    while (current_id) |pid| {
        const idx = @intFromEnum(pid);
        if (idx >= g.nodes.items.len) return null;
        const parent = g.nodes.items[idx];
        if (parent.kind == .file) {
            if (ids[idx]) |id| return id.num;
            return null;
        }
        current_id = parent.parent_id;
    }
    return null;
}

/// Find which phantom package a node belongs to and return its info.
pub fn findPhantomPackageForNode(
    g: *const Graph,
    node_idx: usize,
    phantom_packages: []const PhantomPackage,
) ?PhantomNodeInfo {
    for (phantom_packages) |pkg| {
        if (pkg.root_idx == node_idx) {
            return .{ .pkg_x_num = pkg.x_num, .symbol_path = g.nodes.items[node_idx].name };
        }
        for (pkg.symbols.items) |sym| {
            if (sym.node_idx == node_idx) {
                return .{ .pkg_x_num = pkg.x_num, .symbol_path = sym.qualified_path };
            }
        }
    }
    return null;
}

/// Recursively collect phantom child nodes with their qualified paths.
pub fn collectPhantomSymbols(
    allocator: std.mem.Allocator,
    g: *const Graph,
    parent_id: NodeId,
    parent_path: []const u8,
    symbols: *std.ArrayListUnmanaged(PhantomSymbol),
) !void {
    for (g.nodes.items, 0..) |n, i| {
        if (n.parent_id) |pid| {
            if (pid == parent_id) {
                var path_buf = std.ArrayListUnmanaged(u8){};
                defer path_buf.deinit(allocator);
                if (parent_path.len > 0) {
                    try path_buf.appendSlice(allocator, parent_path);
                    try path_buf.append(allocator, '.');
                }
                try path_buf.appendSlice(allocator, n.name);
                const path = try allocator.dupe(u8, path_buf.items);
                try symbols.append(allocator, .{
                    .node_idx = i,
                    .qualified_path = path,
                });
                try collectPhantomSymbols(allocator, g, @enumFromInt(i), path, symbols);
            }
        }
    }
}

/// Recursively assign IDs to children of a parent node, collecting indices per section.
pub fn assignChildrenIds(
    allocator: std.mem.Allocator,
    g: *const Graph,
    parent_id: NodeId,
    ids: []?IdEntry,
    scope: ?[]const u8,
    filter: FilterOptions,
    st_counter: *u32,
    en_counter: *u32,
    fn_counter: *u32,
    c_counter: *u32,
    err_counter: *u32,
    t_counter: *u32,
    m_counter: *u32,
    struct_indices: *std.ArrayListUnmanaged(usize),
    enum_indices: *std.ArrayListUnmanaged(usize),
    fn_indices: *std.ArrayListUnmanaged(usize),
    const_indices: *std.ArrayListUnmanaged(usize),
    err_indices: *std.ArrayListUnmanaged(usize),
    test_indices: *std.ArrayListUnmanaged(usize),
) !void {
    var children = std.ArrayListUnmanaged(SortableNode){};
    defer children.deinit(allocator);
    for (g.nodes.items, 0..) |n, i| {
        if (n.parent_id) |pid| {
            if (pid == parent_id) {
                if (scope) |s| {
                    if (!inScope(n.file_path, s)) continue;
                }
                try children.append(allocator, .{
                    .node_idx = i,
                    .line_start = n.line_start orelse 0,
                });
            }
        }
    }
    std.mem.sort(SortableNode, children.items, {}, struct {
        fn lessThan(_: void, a: SortableNode, b: SortableNode) bool {
            return a.line_start < b.line_start;
        }
    }.lessThan);

    for (children.items) |child| {
        const n = g.nodes.items[child.node_idx];
        switch (n.kind) {
            .type_def => {
                st_counter.* += 1;
                ids[child.node_idx] = .{ .prefix = "st:", .num = st_counter.* };
                try struct_indices.append(allocator, child.node_idx);
                try assignChildrenIds(allocator, g, @enumFromInt(child.node_idx), ids, scope, filter, st_counter, en_counter, fn_counter, c_counter, err_counter, t_counter, m_counter, struct_indices, enum_indices, fn_indices, const_indices, err_indices, test_indices);
            },
            .enum_def => {
                en_counter.* += 1;
                ids[child.node_idx] = .{ .prefix = "en:", .num = en_counter.* };
                try enum_indices.append(allocator, child.node_idx);
            },
            .function => {
                fn_counter.* += 1;
                ids[child.node_idx] = .{ .prefix = "fn:", .num = fn_counter.* };
                const parent_node = g.nodes.items[@intFromEnum(parent_id)];
                if (parent_node.kind == .file or parent_node.kind == .module) {
                    try fn_indices.append(allocator, child.node_idx);
                }
            },
            .constant => {
                c_counter.* += 1;
                ids[child.node_idx] = .{ .prefix = "c:", .num = c_counter.* };
                try const_indices.append(allocator, child.node_idx);
            },
            .error_def => {
                err_counter.* += 1;
                ids[child.node_idx] = .{ .prefix = "err:", .num = err_counter.* };
                try err_indices.append(allocator, child.node_idx);
            },
            .test_def => {
                if (filter.include_test_nodes) {
                    t_counter.* += 1;
                    ids[child.node_idx] = .{ .prefix = "t:", .num = t_counter.* };
                    try test_indices.append(allocator, child.node_idx);
                }
            },
            .module => {
                m_counter.* += 1;
                ids[child.node_idx] = .{ .prefix = "m:", .num = m_counter.* };
                try assignChildrenIds(allocator, g, @enumFromInt(child.node_idx), ids, scope, filter, st_counter, en_counter, fn_counter, c_counter, err_counter, t_counter, m_counter, struct_indices, enum_indices, fn_indices, const_indices, err_indices, test_indices);
            },
            .field, .import_decl, .file => {},
        }
    }
}

/// Build the full ID assignment table and section index lists by walking all files in path order.
/// Returns the phantom packages list and the x_counter. Caller must deinit phantom_packages entries.
pub const IdAssignment = struct {
    ids: []?IdEntry,
    file_indices: std.ArrayListUnmanaged(usize),
    struct_indices: std.ArrayListUnmanaged(usize),
    enum_indices: std.ArrayListUnmanaged(usize),
    fn_indices: std.ArrayListUnmanaged(usize),
    const_indices: std.ArrayListUnmanaged(usize),
    err_indices: std.ArrayListUnmanaged(usize),
    test_indices: std.ArrayListUnmanaged(usize),
    phantom_packages: std.ArrayListUnmanaged(PhantomPackage),

    pub fn deinit(self: *IdAssignment, allocator: std.mem.Allocator) void {
        allocator.free(self.ids);
        self.file_indices.deinit(allocator);
        self.struct_indices.deinit(allocator);
        self.enum_indices.deinit(allocator);
        self.fn_indices.deinit(allocator);
        self.const_indices.deinit(allocator);
        self.err_indices.deinit(allocator);
        self.test_indices.deinit(allocator);
        for (self.phantom_packages.items) |*pkg| pkg.deinit(allocator);
        self.phantom_packages.deinit(allocator);
    }
};

pub fn buildIdAssignment(
    allocator: std.mem.Allocator,
    g: *const Graph,
    scope: ?[]const u8,
    filter: FilterOptions,
) !IdAssignment {
    const node_count = g.nodes.items.len;
    const ids = try allocator.alloc(?IdEntry, node_count);
    @memset(ids, null);

    var file_indices = std.ArrayListUnmanaged(usize){};
    var struct_indices = std.ArrayListUnmanaged(usize){};
    var enum_indices = std.ArrayListUnmanaged(usize){};
    var fn_indices = std.ArrayListUnmanaged(usize){};
    var const_indices = std.ArrayListUnmanaged(usize){};
    var err_indices = std.ArrayListUnmanaged(usize){};
    var test_indices = std.ArrayListUnmanaged(usize){};

    var f_counter: u32 = 0;
    var st_counter: u32 = 0;
    var en_counter: u32 = 0;
    var fn_counter: u32 = 0;
    var c_counter: u32 = 0;
    var err_counter: u32 = 0;
    var t_counter: u32 = 0;
    var m_counter: u32 = 0;
    var x_counter: u32 = 0;

    var phantom_packages = std.ArrayListUnmanaged(PhantomPackage){};

    // Collect and sort file nodes by path.
    var file_nodes = std.ArrayListUnmanaged(usize){};
    defer file_nodes.deinit(allocator);
    for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .file and isInternal(n)) {
            if (scope) |s| {
                if (!inScope(n.file_path, s)) continue;
            }
            try file_nodes.append(allocator, i);
        }
    }
    std.mem.sort(usize, file_nodes.items, g, struct {
        fn lessThan(graph: *const Graph, a: usize, b: usize) bool {
            const pa = graph.nodes.items[a].file_path orelse "";
            const pb = graph.nodes.items[b].file_path orelse "";
            return std.mem.order(u8, pa, pb) == .lt;
        }
    }.lessThan);

    // Assign file IDs and walk children.
    for (file_nodes.items) |fi| {
        f_counter += 1;
        ids[fi] = .{ .prefix = "f:", .num = f_counter };
        try file_indices.append(allocator, fi);

        try assignChildrenIds(allocator, g, @enumFromInt(fi), ids, scope, filter, &st_counter, &en_counter, &fn_counter, &c_counter, &err_counter, &t_counter, &m_counter, &struct_indices, &enum_indices, &fn_indices, &const_indices, &err_indices, &test_indices);
    }

    // Collect phantom packages (only when external nodes are included).
    if (filter.include_external_nodes) {
        for (g.nodes.items, 0..) |n, i| {
            if (!isInternal(n)) {
                const is_root = if (n.parent_id) |pid| isInternal(g.nodes.items[@intFromEnum(pid)]) else true;
                if (is_root) {
                    x_counter += 1;
                    ids[i] = .{ .prefix = "x:", .num = x_counter };

                    var pkg = PhantomPackage{
                        .root_idx = i,
                        .x_num = x_counter,
                        .symbols = .{},
                    };
                    try collectPhantomSymbols(allocator, g, @enumFromInt(i), "", &pkg.symbols);
                    std.mem.sort(PhantomSymbol, pkg.symbols.items, {}, struct {
                        fn lessThan(_: void, a: PhantomSymbol, b: PhantomSymbol) bool {
                            return std.mem.order(u8, a.qualified_path, b.qualified_path) == .lt;
                        }
                    }.lessThan);
                    try phantom_packages.append(allocator, pkg);
                }
            }
        }
    }

    return .{
        .ids = ids,
        .file_indices = file_indices,
        .struct_indices = struct_indices,
        .enum_indices = enum_indices,
        .fn_indices = fn_indices,
        .const_indices = const_indices,
        .err_indices = err_indices,
        .test_indices = test_indices,
        .phantom_packages = phantom_packages,
    };
}

/// Collect unique languages from in-scope internal nodes.
pub const LanguageSet = struct {
    has_zig: bool,
    has_rust: bool,
};

pub fn collectLanguages(g: *const Graph, scope: ?[]const u8) LanguageSet {
    var has_zig = false;
    var has_rust = false;
    for (g.nodes.items) |n| {
        if (!isInternal(n)) continue;
        if (scope) |s| {
            if (!inScope(n.file_path, s)) continue;
        }
        switch (n.language) {
            .zig => has_zig = true,
            .rust => has_rust = true,
        }
    }
    return .{ .has_zig = has_zig, .has_rust = has_rust };
}
