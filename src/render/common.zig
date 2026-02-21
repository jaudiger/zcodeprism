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

/// Pre-built index mapping parent node indices to their sorted children.
pub const ChildrenIndex = struct {
    map: std.AutoHashMapUnmanaged(usize, []const usize),
    storage: []usize,

    pub fn deinit(self: *ChildrenIndex, allocator: std.mem.Allocator) void {
        self.map.deinit(allocator);
        allocator.free(self.storage);
    }

    pub fn childrenOf(self: *const ChildrenIndex, parent_idx: usize) []const usize {
        return self.map.get(parent_idx) orelse &.{};
    }
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
    if (prefix.len < 2) return 9;
    return switch ((@as(u16, prefix[0]) << 8) | prefix[1]) {
        'c' << 8 | ':' => 0, // "c:"
        'e' << 8 | 'n' => 1, // "en:"
        'e' << 8 | 'r' => 2, // "err:"
        'f' << 8 | ':' => 3, // "f:"
        'f' << 8 | 'n' => 4, // "fn:"
        'm' << 8 | ':' => 5, // "m:"
        's' << 8 | 't' => 6, // "st:"
        'u' << 8 | 'n' => 7, // "un:"
        't' << 8 | ':' => 8, // "t:"
        'x' << 8 | ':' => 9, // "x:"
        else => 10,
    };
}

/// Numeric sort key for edge types, matching alphabetical order of their names.
pub fn edgeTypeSortKey(et: EdgeType) u8 {
    return switch (et) {
        .calls => 0,
        .exports => 1,
        .implements => 2,
        .imports => 3,
        .similar_to => 4,
        .uses_type => 5,
    };
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

/// Recursively collect phantom child nodes with their qualified paths.
pub fn collectPhantomSymbols(
    allocator: std.mem.Allocator,
    g: *const Graph,
    parent_idx: usize,
    parent_path: []const u8,
    symbols: *std.ArrayListUnmanaged(PhantomSymbol),
    children_index: *const ChildrenIndex,
) !void {
    for (children_index.childrenOf(parent_idx)) |child_idx| {
        const n = g.nodes.items[child_idx];
        var path_buf = std.ArrayListUnmanaged(u8){};
        defer path_buf.deinit(allocator);
        if (parent_path.len > 0) {
            try path_buf.appendSlice(allocator, parent_path);
            try path_buf.append(allocator, '.');
        }
        try path_buf.appendSlice(allocator, n.name);
        const path = try allocator.dupe(u8, path_buf.items);
        try symbols.append(allocator, .{
            .node_idx = child_idx,
            .qualified_path = path,
        });
        try collectPhantomSymbols(allocator, g, child_idx, path, symbols, children_index);
    }
}

/// Mutable state carried through the recursive ID assignment walk.
pub const IdWalkState = struct {
    st_counter: u32 = 0,
    un_counter: u32 = 0,
    en_counter: u32 = 0,
    fn_counter: u32 = 0,
    c_counter: u32 = 0,
    err_counter: u32 = 0,
    t_counter: u32 = 0,
    m_counter: u32 = 0,
    struct_indices: std.ArrayListUnmanaged(usize) = .{},
    union_indices: std.ArrayListUnmanaged(usize) = .{},
    enum_indices: std.ArrayListUnmanaged(usize) = .{},
    fn_indices: std.ArrayListUnmanaged(usize) = .{},
    const_indices: std.ArrayListUnmanaged(usize) = .{},
    err_indices: std.ArrayListUnmanaged(usize) = .{},
    test_indices: std.ArrayListUnmanaged(usize) = .{},

    pub fn deinit(self: *IdWalkState, allocator: std.mem.Allocator) void {
        self.struct_indices.deinit(allocator);
        self.union_indices.deinit(allocator);
        self.enum_indices.deinit(allocator);
        self.fn_indices.deinit(allocator);
        self.const_indices.deinit(allocator);
        self.err_indices.deinit(allocator);
        self.test_indices.deinit(allocator);
    }
};

/// Recursively assign IDs to children of a parent node, collecting indices per section.
pub fn assignChildrenIds(
    allocator: std.mem.Allocator,
    g: *const Graph,
    parent_idx: usize,
    ids: []?IdEntry,
    filter: FilterOptions,
    children_index: *const ChildrenIndex,
    state: *IdWalkState,
) !void {
    for (children_index.childrenOf(parent_idx)) |child_idx| {
        const n = g.nodes.items[child_idx];
        switch (n.kind) {
            .type_def => {
                state.st_counter += 1;
                ids[child_idx] = .{ .prefix = "st:", .num = state.st_counter };
                try state.struct_indices.append(allocator, child_idx);
                try assignChildrenIds(allocator, g, child_idx, ids, filter, children_index, state);
            },
            .union_def => {
                state.un_counter += 1;
                ids[child_idx] = .{ .prefix = "un:", .num = state.un_counter };
                try state.union_indices.append(allocator, child_idx);
                try assignChildrenIds(allocator, g, child_idx, ids, filter, children_index, state);
            },
            .enum_def => {
                state.en_counter += 1;
                ids[child_idx] = .{ .prefix = "en:", .num = state.en_counter };
                try state.enum_indices.append(allocator, child_idx);
            },
            .function => {
                state.fn_counter += 1;
                ids[child_idx] = .{ .prefix = "fn:", .num = state.fn_counter };
                const parent_node = g.nodes.items[parent_idx];
                if (parent_node.kind == .file or parent_node.kind == .module) {
                    try state.fn_indices.append(allocator, child_idx);
                }
            },
            .constant => {
                state.c_counter += 1;
                ids[child_idx] = .{ .prefix = "c:", .num = state.c_counter };
                try state.const_indices.append(allocator, child_idx);
            },
            .error_def => {
                state.err_counter += 1;
                ids[child_idx] = .{ .prefix = "err:", .num = state.err_counter };
                try state.err_indices.append(allocator, child_idx);
            },
            .test_def => {
                if (filter.include_test_nodes) {
                    state.t_counter += 1;
                    ids[child_idx] = .{ .prefix = "t:", .num = state.t_counter };
                    try state.test_indices.append(allocator, child_idx);
                }
            },
            .module => {
                state.m_counter += 1;
                ids[child_idx] = .{ .prefix = "m:", .num = state.m_counter };
                try assignChildrenIds(allocator, g, child_idx, ids, filter, children_index, state);
            },
            .field, .import_decl, .file, .comptime_block => {},
        }
    }
}

/// Build the full ID assignment table and section index lists by walking all files in path order.
/// Returns the phantom packages list and the x_counter. Caller must deinit phantom_packages entries.
pub const IdAssignment = struct {
    ids: []?IdEntry,
    file_indices: std.ArrayListUnmanaged(usize),
    struct_indices: std.ArrayListUnmanaged(usize),
    union_indices: std.ArrayListUnmanaged(usize),
    enum_indices: std.ArrayListUnmanaged(usize),
    fn_indices: std.ArrayListUnmanaged(usize),
    const_indices: std.ArrayListUnmanaged(usize),
    err_indices: std.ArrayListUnmanaged(usize),
    test_indices: std.ArrayListUnmanaged(usize),
    phantom_packages: std.ArrayListUnmanaged(PhantomPackage),
    children_index: ChildrenIndex,
    phantom_lookup: std.AutoHashMapUnmanaged(usize, PhantomNodeInfo),
    languages: LanguageSet,

    pub fn deinit(self: *IdAssignment, allocator: std.mem.Allocator) void {
        allocator.free(self.ids);
        self.file_indices.deinit(allocator);
        self.struct_indices.deinit(allocator);
        self.union_indices.deinit(allocator);
        self.enum_indices.deinit(allocator);
        self.fn_indices.deinit(allocator);
        self.const_indices.deinit(allocator);
        self.err_indices.deinit(allocator);
        self.test_indices.deinit(allocator);
        for (self.phantom_packages.items) |*pkg| pkg.deinit(allocator);
        self.phantom_packages.deinit(allocator);
        self.children_index.deinit(allocator);
        self.phantom_lookup.deinit(allocator);
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

    // Count children per parent and collect file nodes + languages in one scan.
    var child_counts = std.AutoHashMapUnmanaged(usize, u32){};
    defer child_counts.deinit(allocator);
    var total_children: usize = 0;
    var languages = LanguageSet{ .has_zig = false, .has_rust = false };
    var file_nodes = std.ArrayListUnmanaged(usize){};
    defer file_nodes.deinit(allocator);

    for (g.nodes.items, 0..) |n, i| {
        if (isInternal(n)) {
            const in_s = if (scope) |s| inScope(n.file_path, s) else true;
            if (in_s) {
                switch (n.language) {
                    .zig => languages.has_zig = true,
                    .rust => languages.has_rust = true,
                }
            }
            if (n.kind == .file and in_s) {
                try file_nodes.append(allocator, i);
            }
        }

        if (n.parent_id) |pid| {
            const parent_idx = @intFromEnum(pid);
            if (scope) |s| {
                if (!inScope(n.file_path, s)) continue;
            }
            const gop = try child_counts.getOrPut(allocator, parent_idx);
            if (!gop.found_existing) gop.value_ptr.* = 0;
            gop.value_ptr.* += 1;
            total_children += 1;
        }
    }

    // Allocate flat storage for the children index.
    const storage = try allocator.alloc(usize, total_children);
    var map = std.AutoHashMapUnmanaged(usize, []const usize){};

    // Compute offsets: each parent gets a contiguous slice in storage.
    var offsets = std.AutoHashMapUnmanaged(usize, usize){};
    defer offsets.deinit(allocator);
    var offset: usize = 0;
    {
        var it = child_counts.iterator();
        while (it.next()) |entry| {
            try offsets.put(allocator, entry.key_ptr.*, offset);
            offset += entry.value_ptr.*;
        }
    }

    // Fill storage with child indices.
    var write_pos = std.AutoHashMapUnmanaged(usize, usize){};
    defer write_pos.deinit(allocator);
    {
        var it = offsets.iterator();
        while (it.next()) |entry| {
            try write_pos.put(allocator, entry.key_ptr.*, entry.value_ptr.*);
        }
    }
    for (g.nodes.items, 0..) |n, i| {
        if (n.parent_id) |pid| {
            const parent_idx = @intFromEnum(pid);
            if (scope) |s| {
                if (!inScope(n.file_path, s)) continue;
            }
            if (write_pos.getPtr(parent_idx)) |pos| {
                storage[pos.*] = i;
                pos.* += 1;
            }
        }
    }

    // Sort each parent's children by line_start for deterministic ordering,
    // then record the slice in the map.
    {
        var it = offsets.iterator();
        while (it.next()) |entry| {
            const pidx = entry.key_ptr.*;
            const start = entry.value_ptr.*;
            const count = child_counts.get(pidx).?;
            const slice = storage[start .. start + count];

            std.mem.sort(usize, slice, g, struct {
                fn lessThan(graph: *const Graph, a: usize, b: usize) bool {
                    const la = graph.nodes.items[a].line_start orelse 0;
                    const lb = graph.nodes.items[b].line_start orelse 0;
                    return la < lb;
                }
            }.lessThan);

            try map.put(allocator, pidx, slice);
        }
    }

    var children_index = ChildrenIndex{ .map = map, .storage = storage };

    // Sort file nodes by path.
    std.mem.sort(usize, file_nodes.items, g, struct {
        fn lessThan(graph: *const Graph, a: usize, b: usize) bool {
            const pa = graph.nodes.items[a].file_path orelse "";
            const pb = graph.nodes.items[b].file_path orelse "";
            return std.mem.order(u8, pa, pb) == .lt;
        }
    }.lessThan);

    // Assign file IDs and walk children.
    var file_indices = std.ArrayListUnmanaged(usize){};
    var state = IdWalkState{};
    var f_counter: u32 = 0;

    for (file_nodes.items) |fi| {
        f_counter += 1;
        ids[fi] = .{ .prefix = "f:", .num = f_counter };
        try file_indices.append(allocator, fi);
        try assignChildrenIds(allocator, g, fi, ids, filter, &children_index, &state);
    }

    // Collect phantom packages (only when external nodes are included).
    var phantom_packages = std.ArrayListUnmanaged(PhantomPackage){};
    var x_counter: u32 = 0;

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
                    try collectPhantomSymbols(allocator, g, i, "", &pkg.symbols, &children_index);
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

    // Build phantom lookup HashMap.
    var phantom_lookup = std.AutoHashMapUnmanaged(usize, PhantomNodeInfo){};
    for (phantom_packages.items) |pkg| {
        try phantom_lookup.put(allocator, pkg.root_idx, .{
            .pkg_x_num = pkg.x_num,
            .symbol_path = g.nodes.items[pkg.root_idx].name,
        });
        for (pkg.symbols.items) |sym| {
            try phantom_lookup.put(allocator, sym.node_idx, .{
                .pkg_x_num = pkg.x_num,
                .symbol_path = sym.qualified_path,
            });
        }
    }

    return .{
        .ids = ids,
        .file_indices = file_indices,
        .struct_indices = state.struct_indices,
        .union_indices = state.union_indices,
        .enum_indices = state.enum_indices,
        .fn_indices = state.fn_indices,
        .const_indices = state.const_indices,
        .err_indices = state.err_indices,
        .test_indices = state.test_indices,
        .phantom_packages = phantom_packages,
        .children_index = children_index,
        .phantom_lookup = phantom_lookup,
        .languages = languages,
    };
}

pub const LanguageSet = struct {
    has_zig: bool,
    has_rust: bool,
};
