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
const Language = types.Language;
const Visibility = types.Visibility;
const ExternalInfo = lang.ExternalInfo;

const IdEntry = common.IdEntry;
const SortableNode = common.SortableNode;
const PhantomPackage = common.PhantomPackage;
const PhantomSymbol = common.PhantomSymbol;
const PhantomNodeInfo = common.PhantomNodeInfo;

const isInternal = common.isInternal;
const inScope = common.inScope;
const appendNum = common.appendNum;
const appendU32 = common.appendU32;
const appendCurrentTimestamp = common.appendCurrentTimestamp;
const edgeTypeName = common.edgeTypeName;
const prefixOrder = common.prefixOrder;
const extractFnSignature = common.extractFnSignature;
const stripConstPrefix = common.stripConstPrefix;
const findFileId = common.findFileId;
const findPhantomPackageForNode = common.findPhantomPackageForNode;

/// Options controlling CTG rendering output.
pub const RenderOptions = struct {
    project_name: []const u8,
    snapshot_name: ?[]const u8 = null,
    source_hash: ?[12]u8 = null,
    scope: ?[]const u8 = null,
    /// For deterministic tests; null = use system clock.
    timestamp: ?[]const u8 = null,
};

/// Render the graph in Compact Text Graph (CTG) format.
///
/// The output is deterministic: same graph + same options = byte-identical output.
/// See `docs/zcodeprism-ctg-spec.md` for the full format specification.
pub fn renderCtg(
    allocator: std.mem.Allocator,
    g: *const Graph,
    options: RenderOptions,
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

    const langs = common.collectLanguages(g, options.scope);

    // Header line 1: project name with em dash (U+2014).
    try out.appendSlice(allocator, "# zcodeprism graph \xe2\x80\x94 ");
    try out.appendSlice(allocator, options.project_name);
    try out.append(allocator, '\n');

    // Header line 2: stats summary.
    var num_buf: [20]u8 = undefined;
    try out.appendSlice(allocator, "# ");
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

    // Header line 3: languages (alphabetical).
    try out.appendSlice(allocator, "# languages: ");
    var first_lang = true;
    if (langs.has_rust) {
        try out.appendSlice(allocator, "rust");
        first_lang = false;
    }
    if (langs.has_zig) {
        if (!first_lang) try out.appendSlice(allocator, ", ");
        try out.appendSlice(allocator, "zig");
        first_lang = false;
    }
    if (first_lang) {
        try out.appendSlice(allocator, "none");
    }
    try out.append(allocator, '\n');

    // Header line 4: timestamp.
    try out.appendSlice(allocator, "# generated ");
    if (options.timestamp) |ts| {
        try out.appendSlice(allocator, ts);
    } else {
        try appendCurrentTimestamp(out, allocator);
    }
    try out.append(allocator, '\n');

    // Header line 5 (optional): snapshot.
    if (options.snapshot_name) |snap_name| {
        try out.appendSlice(allocator, "# snapshot: ");
        try out.appendSlice(allocator, snap_name);
        try out.appendSlice(allocator, " | source_hash: ");
        if (options.source_hash) |hash| {
            try out.appendSlice(allocator, &hash);
        }
        try out.append(allocator, '\n');
    }

    // Sections.
    try out.append(allocator, '\n');
    try renderFilesSection(out, allocator, g, assignment.file_indices.items, ids, &num_buf);
    try out.append(allocator, '\n');
    try renderStructsSection(out, allocator, g, assignment.struct_indices.items, ids, &num_buf);
    try out.append(allocator, '\n');
    try renderEnumsSection(out, allocator, g, assignment.enum_indices.items, ids, &num_buf);
    try out.append(allocator, '\n');
    try renderFunctionsSection(out, allocator, g, assignment.fn_indices.items, ids, &num_buf);
    try out.append(allocator, '\n');
    try renderConstantsSection(out, allocator, g, assignment.const_indices.items, ids, &num_buf);
    try out.append(allocator, '\n');
    try renderErrorsSection(out, allocator, g, assignment.err_indices.items, ids, &num_buf);
    try out.append(allocator, '\n');
    try renderTestsSection(out, allocator, g, assignment.test_indices.items, ids, &num_buf);
    try out.append(allocator, '\n');
    try renderExternalsSection(out, allocator, g, assignment.phantom_packages.items);
    try out.append(allocator, '\n');
    try renderEdgesSection(out, allocator, g, ids, assignment.phantom_packages.items, options.scope, &num_buf);
}

fn renderFilesSection(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    file_indices: []const usize,
    ids: []const ?IdEntry,
    num_buf: *[20]u8,
) !void {
    try out.appendSlice(allocator, "[files]\n");
    for (file_indices) |fi| {
        const n = g.nodes.items[fi];
        const id = ids[fi].?;
        try out.appendSlice(allocator, id.prefix);
        try appendU32(out, allocator, id.num, num_buf);
        try out.append(allocator, ' ');
        try out.appendSlice(allocator, n.file_path orelse n.name);
        try out.append(allocator, ' ');
        try appendU32(out, allocator, n.line_end orelse 0, num_buf);
        try out.appendSlice(allocator, "L\n");
    }
}

fn renderStructsSection(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    struct_indices: []const usize,
    ids: []const ?IdEntry,
    num_buf: *[20]u8,
) !void {
    try out.appendSlice(allocator, "[structs]\n");
    for (struct_indices) |si| {
        const n = g.nodes.items[si];
        const id = ids[si].?;
        const file_id = findFileId(g, n, ids);

        try out.appendSlice(allocator, id.prefix);
        try appendU32(out, allocator, id.num, num_buf);
        try out.append(allocator, ' ');
        try out.appendSlice(allocator, n.name);
        if (file_id) |fid| {
            try out.appendSlice(allocator, " f:");
            try appendU32(out, allocator, fid, num_buf);
            try out.append(allocator, ':');
            try appendU32(out, allocator, n.line_start orelse 0, num_buf);
        }
        if (n.visibility == .public) {
            try out.appendSlice(allocator, " pub");
        }
        try out.append(allocator, '\n');

        try renderStructChildren(out, allocator, g, @enumFromInt(si));

        try out.append(allocator, '\n');
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

    for (fields.items) |f| {
        const n = g.nodes.items[f.node_idx];
        try out.appendSlice(allocator, "  .");
        if (n.signature) |sig| {
            try out.appendSlice(allocator, sig);
        } else {
            try out.appendSlice(allocator, n.name);
        }
        try out.append(allocator, '\n');
    }

    for (methods.items) |m| {
        const n = g.nodes.items[m.node_idx];
        try out.appendSlice(allocator, "  fn ");
        if (n.signature) |sig| {
            const fn_part = extractFnSignature(sig);
            try out.appendSlice(allocator, fn_part);
        } else {
            try out.appendSlice(allocator, n.name);
            try out.appendSlice(allocator, "()");
        }
        try out.append(allocator, '\n');
    }
}

fn renderEnumsSection(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    enum_indices: []const usize,
    ids: []const ?IdEntry,
    num_buf: *[20]u8,
) !void {
    try out.appendSlice(allocator, "[enums]\n");
    for (enum_indices) |ei| {
        const n = g.nodes.items[ei];
        const id = ids[ei].?;
        const file_id = findFileId(g, n, ids);

        try out.appendSlice(allocator, id.prefix);
        try appendU32(out, allocator, id.num, num_buf);
        try out.append(allocator, ' ');
        try out.appendSlice(allocator, n.name);
        if (file_id) |fid| {
            try out.appendSlice(allocator, " f:");
            try appendU32(out, allocator, fid, num_buf);
            try out.append(allocator, ':');
            try appendU32(out, allocator, n.line_start orelse 0, num_buf);
        }
        if (n.visibility == .public) {
            try out.appendSlice(allocator, " pub");
        }
        try out.append(allocator, '\n');
        try out.append(allocator, '\n');
    }
}

fn renderFunctionsSection(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    fn_indices: []const usize,
    ids: []const ?IdEntry,
    num_buf: *[20]u8,
) !void {
    try out.appendSlice(allocator, "[functions]\n");
    for (fn_indices) |fi| {
        const n = g.nodes.items[fi];
        const id = ids[fi].?;
        const file_id = findFileId(g, n, ids);

        try out.appendSlice(allocator, id.prefix);
        try appendU32(out, allocator, id.num, num_buf);
        try out.append(allocator, ' ');

        if (n.signature) |sig| {
            const fn_part = extractFnSignature(sig);
            try out.appendSlice(allocator, fn_part);
        } else {
            try out.appendSlice(allocator, n.name);
            try out.appendSlice(allocator, "()");
        }

        if (file_id) |fid| {
            try out.appendSlice(allocator, " f:");
            try appendU32(out, allocator, fid, num_buf);
            try out.append(allocator, ':');
            try appendU32(out, allocator, n.line_start orelse 0, num_buf);
        }
        if (n.visibility == .public) {
            try out.appendSlice(allocator, " pub");
        }
        try out.append(allocator, '\n');

        if (n.doc) |doc| {
            var lines = std.mem.splitScalar(u8, doc, '\n');
            while (lines.next()) |line| {
                try out.appendSlice(allocator, "  ");
                try out.appendSlice(allocator, line);
                try out.append(allocator, '\n');
            }
        }

        try out.append(allocator, '\n');
    }
}

fn renderConstantsSection(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    const_indices: []const usize,
    ids: []const ?IdEntry,
    num_buf: *[20]u8,
) !void {
    try out.appendSlice(allocator, "[constants]\n");
    for (const_indices) |ci| {
        const n = g.nodes.items[ci];
        const id = ids[ci].?;
        const file_id = findFileId(g, n, ids);

        try out.appendSlice(allocator, id.prefix);
        try appendU32(out, allocator, id.num, num_buf);
        try out.append(allocator, ' ');

        if (n.signature) |sig| {
            const stripped = stripConstPrefix(sig);
            try out.appendSlice(allocator, stripped);
        } else {
            try out.appendSlice(allocator, n.name);
        }

        if (file_id) |fid| {
            try out.appendSlice(allocator, " f:");
            try appendU32(out, allocator, fid, num_buf);
            try out.append(allocator, ':');
            try appendU32(out, allocator, n.line_start orelse 0, num_buf);
        }
        if (n.visibility == .public) {
            try out.appendSlice(allocator, " pub");
        }
        try out.append(allocator, '\n');
    }
}

fn renderErrorsSection(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    err_indices: []const usize,
    ids: []const ?IdEntry,
    num_buf: *[20]u8,
) !void {
    try out.appendSlice(allocator, "[errors]\n");
    for (err_indices) |ei| {
        const n = g.nodes.items[ei];
        const id = ids[ei].?;
        const file_id = findFileId(g, n, ids);

        try out.appendSlice(allocator, id.prefix);
        try appendU32(out, allocator, id.num, num_buf);
        try out.append(allocator, ' ');
        try out.appendSlice(allocator, n.name);
        if (file_id) |fid| {
            try out.appendSlice(allocator, " f:");
            try appendU32(out, allocator, fid, num_buf);
            try out.append(allocator, ':');
            try appendU32(out, allocator, n.line_start orelse 0, num_buf);
        }
        try out.append(allocator, '\n');
        try out.append(allocator, '\n');
    }
}

fn renderTestsSection(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    test_indices: []const usize,
    ids: []const ?IdEntry,
    num_buf: *[20]u8,
) !void {
    try out.appendSlice(allocator, "[tests]\n");
    for (test_indices) |ti| {
        const n = g.nodes.items[ti];
        const id = ids[ti].?;
        const file_id = findFileId(g, n, ids);

        try out.appendSlice(allocator, id.prefix);
        try appendU32(out, allocator, id.num, num_buf);
        try out.appendSlice(allocator, " \"");
        try out.appendSlice(allocator, n.name);
        try out.append(allocator, '"');
        if (file_id) |fid| {
            try out.appendSlice(allocator, " f:");
            try appendU32(out, allocator, fid, num_buf);
            try out.append(allocator, ':');
            try appendU32(out, allocator, n.line_start orelse 0, num_buf);
        }
        const lines = if (n.line_end != null and n.line_start != null)
            n.line_end.? - n.line_start.? + 1
        else
            0;
        try out.append(allocator, ' ');
        try appendU32(out, allocator, lines, num_buf);
        try out.appendSlice(allocator, "L\n");
    }
}

fn renderExternalsSection(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    phantom_packages: []const PhantomPackage,
) !void {
    try out.appendSlice(allocator, "[externals]\n");
    var num_buf: [20]u8 = undefined;
    for (phantom_packages) |pkg| {
        const n = g.nodes.items[pkg.root_idx];
        try out.appendSlice(allocator, "x:");
        try appendU32(out, allocator, pkg.x_num, &num_buf);
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
        try out.append(allocator, '\n');
    }
}

fn renderEdgesSection(
    out: *std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    g: *const Graph,
    ids: []const ?IdEntry,
    phantom_packages: []const PhantomPackage,
    scope: ?[]const u8,
    num_buf: *[20]u8,
) !void {
    try out.appendSlice(allocator, "[edges]\n");

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

    for (entries.items) |entry| {
        const src_id = ids[entry.source_idx].?;
        try out.appendSlice(allocator, src_id.prefix);
        try appendU32(out, allocator, src_id.num, num_buf);

        try out.appendSlice(allocator, " -> ");

        if (ids[entry.target_idx]) |tgt_id| {
            try out.appendSlice(allocator, tgt_id.prefix);
            try appendU32(out, allocator, tgt_id.num, num_buf);
        } else {
            const pkg_info = findPhantomPackageForNode(g, entry.target_idx, phantom_packages);
            if (pkg_info) |pi| {
                try out.appendSlice(allocator, "x:");
                try appendU32(out, allocator, pi.pkg_x_num, num_buf);
                try out.append(allocator, ':');
                try out.appendSlice(allocator, pi.symbol_path);
            } else {
                try out.appendSlice(allocator, "x:?");
            }
        }

        try out.append(allocator, ' ');
        try out.appendSlice(allocator, edgeTypeName(entry.edge_type));
        try out.append(allocator, '\n');
    }
}

// --- Test helpers ---

/// Creates a diverse graph exercising all CTG sections.
///
/// Structure:
/// - 2 file nodes: src/lib.zig (100L), src/main.zig (50L)
/// - 1 struct (Tokenizer) under lib.zig with 1 field child + 1 method child
/// - 1 enum (TokenKind) under lib.zig
/// - 2 top-level functions under main.zig (1 pub main, 1 private helper)
/// - 1 constant (MAX_SIZE) under main.zig
/// - 1 error_def (ParseError) under lib.zig
/// - 1 test_def under main.zig
/// - 2 phantom nodes: std (module, stdlib) + Allocator (type_def, stdlib, parent=std)
/// - 5 edges: imports, 2x calls, 2x uses_type
fn createCtgTestGraph(allocator: std.mem.Allocator) !Graph {
    var g = Graph.init(allocator, "/tmp/project");
    errdefer g.deinit();

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

    const phantom_std = try g.addNode(.{
        .id = .root,
        .name = "std",
        .kind = .module,
        .language = .zig,
        .external = .{ .stdlib = {} },
    });

    const phantom_allocator = try g.addNode(.{
        .id = .root,
        .name = "Allocator",
        .kind = .type_def,
        .language = .zig,
        .external = .{ .stdlib = {} },
        .parent_id = phantom_std,
    });

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

/// Creates a graph with a single file node and no functions.
fn createSingleFileGraph(allocator: std.mem.Allocator) !Graph {
    var g = Graph.init(allocator, "/tmp/project");
    errdefer g.deinit();

    _ = try g.addNode(.{
        .id = .root,
        .name = "src/empty.zig",
        .kind = .file,
        .language = .zig,
        .file_path = "src/empty.zig",
        .line_start = 1,
        .line_end = 10,
        .visibility = .public,
    });

    return g;
}

// ==========================================================================
// CTG Tests: nominal
// ==========================================================================

test "header line 1 matches spec" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderCtg(allocator, &g, options, &out);

    // Assert: first line: "# zcodeprism graph \xe2\x80\x94 myproject"
    const output = out.items;
    const first_line_end = std.mem.indexOfScalar(u8, output, '\n') orelse output.len;
    const first_line = output[0..first_line_end];
    try std.testing.expectEqualStrings("# zcodeprism graph \xe2\x80\x94 myproject", first_line);
}

test "header line 2 has stats" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderCtg(allocator, &g, options, &out);

    // Assert: second line starts with "# " and contains "files" and "functions"
    const output = out.items;
    var lines = std.mem.splitScalar(u8, output, '\n');
    _ = lines.next(); // skip line 1
    const line2 = lines.next() orelse return error.MissingLine;
    try std.testing.expect(std.mem.startsWith(u8, line2, "# "));
    try std.testing.expect(std.mem.indexOf(u8, line2, "files") != null);
    try std.testing.expect(std.mem.indexOf(u8, line2, "functions") != null);
}

test "header line 3 has languages" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderCtg(allocator, &g, options, &out);

    // Assert: third line: "# languages: zig"
    const output = out.items;
    var lines = std.mem.splitScalar(u8, output, '\n');
    _ = lines.next();
    _ = lines.next();
    const line3 = lines.next() orelse return error.MissingLine;
    try std.testing.expectEqualStrings("# languages: zig", line3);
}

test "header line 4 has timestamp" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderCtg(allocator, &g, options, &out);

    // Assert: fourth line: "# generated 2026-02-14T10:30:00Z"
    const output = out.items;
    var lines = std.mem.splitScalar(u8, output, '\n');
    _ = lines.next();
    _ = lines.next();
    _ = lines.next();
    const line4 = lines.next() orelse return error.MissingLine;
    try std.testing.expectEqualStrings("# generated 2026-02-14T10:30:00Z", line4);
}

test "sections appear in correct order" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderCtg(allocator, &g, options, &out);

    // Assert: sections in order
    const output = out.items;
    const sections = [_][]const u8{
        "[files]",
        "[structs]",
        "[enums]",
        "[functions]",
        "[constants]",
        "[errors]",
        "[tests]",
        "[externals]",
        "[edges]",
    };

    var last_pos: usize = 0;
    for (sections) |section| {
        const pos = std.mem.indexOf(u8, output[last_pos..], section) orelse
            return error.MissingSection;
        last_pos = last_pos + pos + section.len;
    }
}

test "file IDs use f: prefix" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderCtg(allocator, &g, options, &out);

    // Assert
    const output = out.items;
    const files_start = (std.mem.indexOf(u8, output, "[files]\n") orelse return error.MissingSection) + "[files]\n".len;
    const files_end = std.mem.indexOf(u8, output[files_start..], "\n[") orelse (output.len - files_start);
    const files_section = output[files_start .. files_start + files_end];

    var lines = std.mem.splitScalar(u8, files_section, '\n');
    var found_any = false;
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        try std.testing.expect(std.mem.startsWith(u8, line, "f:"));
        found_any = true;
    }
    try std.testing.expect(found_any);
}

test "function IDs use fn: prefix" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderCtg(allocator, &g, options, &out);

    // Assert
    const output = out.items;
    const fns_start = (std.mem.indexOf(u8, output, "[functions]\n") orelse return error.MissingSection) + "[functions]\n".len;
    const fns_end = std.mem.indexOf(u8, output[fns_start..], "\n[") orelse (output.len - fns_start);
    const fns_section = output[fns_start .. fns_start + fns_end];

    var lines = std.mem.splitScalar(u8, fns_section, '\n');
    var found_any = false;
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        if (std.mem.startsWith(u8, line, "  ")) continue;
        try std.testing.expect(std.mem.startsWith(u8, line, "fn:"));
        found_any = true;
    }
    try std.testing.expect(found_any);
}

test "struct IDs use st: prefix" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderCtg(allocator, &g, options, &out);

    // Assert
    const output = out.items;
    const sts_start = (std.mem.indexOf(u8, output, "[structs]\n") orelse return error.MissingSection) + "[structs]\n".len;
    const sts_end = std.mem.indexOf(u8, output[sts_start..], "\n[") orelse (output.len - sts_start);
    const sts_section = output[sts_start .. sts_start + sts_end];

    var lines = std.mem.splitScalar(u8, sts_section, '\n');
    var found_any = false;
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        if (std.mem.startsWith(u8, line, "  ")) continue;
        try std.testing.expect(std.mem.startsWith(u8, line, "st:"));
        found_any = true;
    }
    try std.testing.expect(found_any);
}

test "enum IDs use en: prefix" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderCtg(allocator, &g, options, &out);

    // Assert
    const output = out.items;
    const ens_start = (std.mem.indexOf(u8, output, "[enums]\n") orelse return error.MissingSection) + "[enums]\n".len;
    const ens_end = std.mem.indexOf(u8, output[ens_start..], "\n[") orelse (output.len - ens_start);
    const ens_section = output[ens_start .. ens_start + ens_end];

    var lines = std.mem.splitScalar(u8, ens_section, '\n');
    var found_any = false;
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        if (std.mem.startsWith(u8, line, "  ")) continue;
        try std.testing.expect(std.mem.startsWith(u8, line, "en:"));
        found_any = true;
    }
    try std.testing.expect(found_any);
}

test "constant IDs use c: prefix" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderCtg(allocator, &g, options, &out);

    // Assert
    const output = out.items;
    const cs_start = (std.mem.indexOf(u8, output, "[constants]\n") orelse return error.MissingSection) + "[constants]\n".len;
    const cs_end = std.mem.indexOf(u8, output[cs_start..], "\n[") orelse (output.len - cs_start);
    const cs_section = output[cs_start .. cs_start + cs_end];

    var lines = std.mem.splitScalar(u8, cs_section, '\n');
    var found_any = false;
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        if (std.mem.startsWith(u8, line, "  ")) continue;
        try std.testing.expect(std.mem.startsWith(u8, line, "c:"));
        found_any = true;
    }
    try std.testing.expect(found_any);
}

test "error IDs use err: prefix" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderCtg(allocator, &g, options, &out);

    // Assert
    const output = out.items;
    const errs_start = (std.mem.indexOf(u8, output, "[errors]\n") orelse return error.MissingSection) + "[errors]\n".len;
    const errs_end = std.mem.indexOf(u8, output[errs_start..], "\n[") orelse (output.len - errs_start);
    const errs_section = output[errs_start .. errs_start + errs_end];

    var lines = std.mem.splitScalar(u8, errs_section, '\n');
    var found_any = false;
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        if (std.mem.startsWith(u8, line, "  ")) continue;
        try std.testing.expect(std.mem.startsWith(u8, line, "err:"));
        found_any = true;
    }
    try std.testing.expect(found_any);
}

test "test IDs use t: prefix" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderCtg(allocator, &g, options, &out);

    // Assert
    const output = out.items;
    const ts_start = (std.mem.indexOf(u8, output, "[tests]\n") orelse return error.MissingSection) + "[tests]\n".len;
    const ts_end = std.mem.indexOf(u8, output[ts_start..], "\n[") orelse (output.len - ts_start);
    const ts_section = output[ts_start .. ts_start + ts_end];

    var lines = std.mem.splitScalar(u8, ts_section, '\n');
    var found_any = false;
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        try std.testing.expect(std.mem.startsWith(u8, line, "t:"));
        found_any = true;
    }
    try std.testing.expect(found_any);
}

test "external IDs use x: prefix" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderCtg(allocator, &g, options, &out);

    // Assert
    const output = out.items;
    const xs_start = (std.mem.indexOf(u8, output, "[externals]\n") orelse return error.MissingSection) + "[externals]\n".len;
    const xs_end = std.mem.indexOf(u8, output[xs_start..], "\n[") orelse (output.len - xs_start);
    const xs_section = output[xs_start .. xs_start + xs_end];

    var lines = std.mem.splitScalar(u8, xs_section, '\n');
    var found_any = false;
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        if (std.mem.startsWith(u8, line, "  ")) continue;
        try std.testing.expect(std.mem.startsWith(u8, line, "x:"));
        found_any = true;
    }
    try std.testing.expect(found_any);
}

test "files sorted by path alphabetical" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderCtg(allocator, &g, options, &out);

    // Assert: file entries are sorted: src/lib.zig before src/main.zig
    const output = out.items;
    const lib_pos = std.mem.indexOf(u8, output, "src/lib.zig") orelse return error.MissingEntry;
    const main_pos = std.mem.indexOf(u8, output, "src/main.zig") orelse return error.MissingEntry;
    try std.testing.expect(lib_pos < main_pos);
}

test "edges sorted by type then source then target" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderCtg(allocator, &g, options, &out);

    // Assert: within [edges], "calls" < "imports" < "uses_type" (alphabetical)
    const output = out.items;
    const edges_start = (std.mem.indexOf(u8, output, "[edges]\n") orelse return error.MissingSection) + "[edges]\n".len;
    const edges_section = output[edges_start..];

    const calls_pos = std.mem.indexOf(u8, edges_section, " calls");
    const imports_pos = std.mem.indexOf(u8, edges_section, " imports");
    const uses_type_pos = std.mem.indexOf(u8, edges_section, " uses_type");

    try std.testing.expect(calls_pos != null);
    try std.testing.expect(imports_pos != null);
    try std.testing.expect(uses_type_pos != null);

    try std.testing.expect(calls_pos.? < imports_pos.?);
    try std.testing.expect(imports_pos.? < uses_type_pos.?);
}

test "deterministic output" {
    // Arrange
    const allocator = std.testing.allocator;

    var g1 = try createCtgTestGraph(allocator);
    defer g1.deinit();
    var out1: std.ArrayListUnmanaged(u8) = .{};
    defer out1.deinit(allocator);

    var g2 = try createCtgTestGraph(allocator);
    defer g2.deinit();
    var out2: std.ArrayListUnmanaged(u8) = .{};
    defer out2.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderCtg(allocator, &g1, options, &out1);
    try renderCtg(allocator, &g2, options, &out2);

    // Assert: byte-identical output
    try std.testing.expectEqualSlices(u8, out1.items, out2.items);
}

// ==========================================================================
// CTG Tests: edge cases
// ==========================================================================

test "empty graph renders without crash" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = Graph.init(allocator, "/tmp/project");
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "empty",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderCtg(allocator, &g, options, &out);

    // Assert: has header, no crash
    const output = out.items;
    try std.testing.expect(output.len > 0);
    try std.testing.expect(std.mem.startsWith(u8, output, "# zcodeprism graph"));
}

test "single file no functions" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createSingleFileGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "minimal",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderCtg(allocator, &g, options, &out);

    // Assert
    const output = out.items;
    try std.testing.expect(std.mem.indexOf(u8, output, "[files]") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "src/empty.zig") != null);
}

test "with scope filters nodes" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
        .scope = "src/main",
    };

    // Act
    try renderCtg(allocator, &g, options, &out);

    // Assert: only src/main.zig nodes appear, not src/lib.zig nodes
    const output = out.items;
    try std.testing.expect(std.mem.indexOf(u8, output, "src/main.zig") != null);
    const structs_start = std.mem.indexOf(u8, output, "[structs]");
    if (structs_start) |start| {
        const next_section = std.mem.indexOf(u8, output[start + 1 ..], "\n[");
        const end = if (next_section) |ns| start + 1 + ns else output.len;
        const structs_content = output[start..end];
        try std.testing.expect(std.mem.indexOf(u8, structs_content, "Tokenizer") == null);
    }
}

test "phantom nodes in externals section" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderCtg(allocator, &g, options, &out);

    // Assert
    const output = out.items;
    const ext_start = std.mem.indexOf(u8, output, "[externals]") orelse return error.MissingSection;
    const ext_section = output[ext_start..];
    try std.testing.expect(std.mem.indexOf(u8, ext_section, "(stdlib)") != null);
    try std.testing.expect(std.mem.indexOf(u8, ext_section, "std") != null);
}

// ==========================================================================
// CTG Tests: snapshot
// ==========================================================================

test "snapshot line present when options set" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
        .snapshot_name = "main-baseline",
        .source_hash = "a7f3b2c9e1d4".*,
    };

    // Act
    try renderCtg(allocator, &g, options, &out);

    // Assert
    const output = out.items;
    try std.testing.expect(std.mem.indexOf(u8, output, "# snapshot: main-baseline | source_hash: a7f3b2c9e1d4") != null);
}

test "snapshot line absent for export" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit();
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    try renderCtg(allocator, &g, options, &out);

    // Assert: no snapshot line, exactly 4 header lines
    const output = out.items;
    try std.testing.expect(std.mem.indexOf(u8, output, "# snapshot:") == null);

    var header_count: usize = 0;
    var lines = std.mem.splitScalar(u8, output, '\n');
    while (lines.next()) |line| {
        if (std.mem.startsWith(u8, line, "# ")) {
            header_count += 1;
        } else {
            break;
        }
    }
    try std.testing.expectEqual(@as(usize, 4), header_count);
}
