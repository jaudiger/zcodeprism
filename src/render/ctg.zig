const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const types = @import("../core/types.zig");
const common = @import("common.zig");
const sections = @import("ctg_sections.zig");

const Graph = graph_mod.Graph;
const FrozenGraph = graph_mod.FrozenGraph;

const appendNum = common.appendNum;
const appendCurrentTimestamp = common.appendCurrentTimestamp;

/// Configuration for a single CTG rendering pass.
/// Required: project_name. Optional fields control snapshot metadata,
/// scope filtering, timestamp override, and node category filtering.
pub const RenderOptions = struct {
    /// Project name emitted in the header line.
    project_name: []const u8,
    /// When set, a "# snapshot:" header line is emitted with this name.
    snapshot_name: ?[]const u8 = null,
    /// Hash of the source tree, shown alongside snapshot_name.
    source_hash: ?types.ContentHash = null,
    /// Path prefix filter: only nodes whose file_path starts with this
    /// value are included. Null means no filtering (all files).
    scope: ?[]const u8 = null,
    /// Explicit timestamp for the "# generated" header line.
    /// When null, the current system clock is used.
    timestamp: ?[]const u8 = null,
    /// Controls inclusion of test_def and external (phantom) nodes.
    filter: common.FilterOptions = .{},
};

/// Header counts passed to the header renderer.
const HeaderCounts = struct {
    files: usize,
    functions: usize,
    types: usize,
    unions: usize,
    enums: usize,
    constants: usize,
    tests: usize,
    externals: usize,
};

/// Render the CTG header block (lines 1-5).
fn renderCtgHeader(
    out: *std.ArrayList(u8),
    allocator: std.mem.Allocator,
    options: RenderOptions,
    counts: HeaderCounts,
    langs: common.LanguageSet,
) !void {
    var num_buf: [20]u8 = undefined;

    // Line 1: project name.
    try out.appendSlice(allocator, "# zcodeprism graph -- ");
    try out.appendSlice(allocator, options.project_name);
    try out.append(allocator, '\n');

    // Line 2: stats summary.
    try out.appendSlice(allocator, "# ");
    try appendNum(out, allocator, counts.files, &num_buf);
    try out.appendSlice(allocator, " files, ");
    try appendNum(out, allocator, counts.functions, &num_buf);
    try out.appendSlice(allocator, " functions, ");
    try appendNum(out, allocator, counts.types, &num_buf);
    try out.appendSlice(allocator, " types, ");
    try appendNum(out, allocator, counts.unions, &num_buf);
    try out.appendSlice(allocator, " unions, ");
    try appendNum(out, allocator, counts.enums, &num_buf);
    try out.appendSlice(allocator, " enums, ");
    try appendNum(out, allocator, counts.constants, &num_buf);
    try out.appendSlice(allocator, " constants, ");
    try appendNum(out, allocator, counts.tests, &num_buf);
    try out.appendSlice(allocator, " tests, ");
    try appendNum(out, allocator, counts.externals, &num_buf);
    try out.appendSlice(allocator, " externals\n");

    // Line 3: languages (alphabetical).
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

    // Line 4: timestamp.
    try out.appendSlice(allocator, "# generated ");
    if (options.timestamp) |ts| {
        try out.appendSlice(allocator, ts);
    } else {
        try appendCurrentTimestamp(out, allocator);
    }
    try out.append(allocator, '\n');

    // Line 5 (optional): snapshot.
    if (options.snapshot_name) |snap_name| {
        try out.appendSlice(allocator, "# snapshot: ");
        try out.appendSlice(allocator, snap_name);
        try out.appendSlice(allocator, " | source_hash: ");
        if (options.source_hash) |hash| {
            const hex = types.formatHash(hash);
            try out.appendSlice(allocator, &hex);
        }
        try out.append(allocator, '\n');
    }
}

/// Append a section separator (blank line) if content has already been written.
fn appendSeparator(out: *std.ArrayList(u8), allocator: std.mem.Allocator, written: *bool) !void {
    if (written.*) try out.append(allocator, '\n');
    written.* = true;
}

/// Renders the graph in Compact Text Graph (CTG) format, appending the
/// result to `out`. Output is deterministic: identical graph + options
/// produces byte-identical output. See docs/zcodeprism-ctg-spec.md for
/// the full format specification.
pub fn renderCtg(
    allocator: std.mem.Allocator,
    fg: FrozenGraph,
    options: RenderOptions,
    out: *std.ArrayList(u8),
) !void {
    const g = fg.graph;
    var assignment = try common.buildIdAssignment(allocator, fg, options.scope, options.filter);
    defer assignment.deinit(allocator);

    var total_name_bytes: usize = 0;
    for (g.nodes.items) |n| {
        total_name_bytes += n.name.len;
        if (n.file_path) |fp| total_name_bytes += fp.len;
    }
    const per_node_overhead = 30; // fixed per-entry bytes the name scan doesn't cover
    const per_edge_overhead = 40; // compact IDs make edge size well-bounded
    const header_overhead = 512; // section labels and header comment block
    try out.ensureTotalCapacity(allocator, out.items.len +
        total_name_bytes +
        g.nodes.items.len * per_node_overhead +
        g.edges.items.len * per_edge_overhead +
        header_overhead);

    const ids = assignment.ids;

    try renderCtgHeader(out, allocator, options, .{
        .files = assignment.file_indices.items.len,
        .functions = assignment.fn_indices.items.len,
        .types = assignment.type_indices.items.len,
        .unions = assignment.union_indices.items.len,
        .enums = assignment.enum_indices.items.len,
        .constants = assignment.const_indices.items.len,
        .tests = assignment.test_indices.items.len,
        .externals = assignment.phantom_packages.items.len,
    }, assignment.languages);

    // Sections: each renderer returns early if it has no content.
    // A blank line separates the header from the first section and each section from the next.
    try out.append(allocator, '\n');
    var written = false;

    var num_buf: [20]u8 = undefined;
    const ctx = common.SectionCtx{ .g = g, .ids = ids, .num_buf = &num_buf };

    if (assignment.file_indices.items.len > 0) {
        try sections.renderFilesSection(out, allocator, &ctx, assignment.file_indices.items);
        written = true;
    }
    if (assignment.type_indices.items.len > 0) {
        try appendSeparator(out, allocator, &written);
        try sections.renderTypesSection(out, allocator, &ctx, assignment.type_indices.items, &assignment.children_index);
    }
    if (assignment.union_indices.items.len > 0) {
        try appendSeparator(out, allocator, &written);
        try sections.renderUnionsSection(out, allocator, &ctx, assignment.union_indices.items, &assignment.children_index);
    }
    if (assignment.enum_indices.items.len > 0) {
        try appendSeparator(out, allocator, &written);
        try sections.renderEnumsSection(out, allocator, &ctx, assignment.enum_indices.items);
    }
    if (assignment.fn_indices.items.len > 0) {
        try appendSeparator(out, allocator, &written);
        try sections.renderFunctionsSection(out, allocator, &ctx, assignment.fn_indices.items);
    }
    if (assignment.const_indices.items.len > 0) {
        try appendSeparator(out, allocator, &written);
        try sections.renderConstantsSection(out, allocator, &ctx, assignment.const_indices.items);
    }
    if (assignment.err_indices.items.len > 0) {
        try appendSeparator(out, allocator, &written);
        try sections.renderErrorsSection(out, allocator, &ctx, assignment.err_indices.items);
    }
    if (assignment.test_indices.items.len > 0) {
        try appendSeparator(out, allocator, &written);
        try sections.renderTestsSection(out, allocator, &ctx, assignment.test_indices.items);
    }
    if (assignment.phantom_packages.items.len > 0) {
        try appendSeparator(out, allocator, &written);
        try sections.renderExternalsSection(out, allocator, g, assignment.phantom_packages.items);
    }

    // Edges: the renderer checks internally and writes nothing if all edges are filtered.
    {
        const mark = out.items.len;
        if (written) try out.append(allocator, '\n');
        const before_section = out.items.len;
        try sections.renderEdgesSection(out, allocator, &ctx, &assignment.phantom_lookup, options.scope, options.filter);
        if (out.items.len == before_section) {
            out.shrinkRetainingCapacity(mark);
        }
    }
}

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
    var g = Graph.init("/tmp/project");
    errdefer g.deinit(allocator);

    const lib_file = try g.addNode(allocator, .{
        .id = .root,
        .name = "src/lib.zig",
        .kind = .file,
        .language = .zig,
        .file_path = "src/lib.zig",
        .line_start = 1,
        .line_end = 100,
        .visibility = .public,
    });

    const main_file = try g.addNode(allocator, .{
        .id = .root,
        .name = "src/main.zig",
        .kind = .file,
        .language = .zig,
        .file_path = "src/main.zig",
        .line_start = 1,
        .line_end = 50,
        .visibility = .public,
    });

    const tokenizer_struct = try g.addNode(allocator, .{
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

    _ = try g.addNode(allocator, .{
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

    const method_next = try g.addNode(allocator, .{
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

    _ = try g.addNode(allocator, .{
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

    const fn_main = try g.addNode(allocator, .{
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

    const fn_helper = try g.addNode(allocator, .{
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

    _ = try g.addNode(allocator, .{
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

    _ = try g.addNode(allocator, .{
        .id = .root,
        .name = "ParseError",
        .kind = .error_def,
        .language = .zig,
        .file_path = "src/lib.zig",
        .line_start = 60,
        .line_end = 65,
        .parent_id = lib_file,
    });

    _ = try g.addNode(allocator, .{
        .id = .root,
        .name = "main works correctly",
        .kind = .test_def,
        .language = .zig,
        .file_path = "src/main.zig",
        .line_start = 40,
        .line_end = 48,
        .parent_id = main_file,
    });

    const phantom_std = try g.addNode(allocator, .{
        .id = .root,
        .name = "std",
        .kind = .module,
        .language = .zig,
        .external = .{ .stdlib = {} },
    });

    const phantom_allocator = try g.addNode(allocator, .{
        .id = .root,
        .name = "Allocator",
        .kind = .type_def,
        .language = .zig,
        .external = .{ .stdlib = {} },
        .parent_id = phantom_std,
    });

    _ = try g.addEdgeIfNew(allocator, .{
        .source_id = main_file,
        .target_id = lib_file,
        .edge_type = .imports,
    });

    _ = try g.addEdgeIfNew(allocator, .{
        .source_id = fn_main,
        .target_id = fn_helper,
        .edge_type = .calls,
    });

    _ = try g.addEdgeIfNew(allocator, .{
        .source_id = fn_main,
        .target_id = method_next,
        .edge_type = .calls,
    });

    _ = try g.addEdgeIfNew(allocator, .{
        .source_id = fn_main,
        .target_id = tokenizer_struct,
        .edge_type = .uses_type,
    });

    _ = try g.addEdgeIfNew(allocator, .{
        .source_id = fn_helper,
        .target_id = phantom_allocator,
        .edge_type = .uses_type,
    });

    return g;
}

/// Creates a graph with a single file node and no functions.
fn createSingleFileGraph(allocator: std.mem.Allocator) !Graph {
    var g = Graph.init("/tmp/project");
    errdefer g.deinit(allocator);

    _ = try g.addNode(allocator, .{
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

test "header line 1 matches spec" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit(allocator);
    var out: std.ArrayList(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    const fg = try g.freeze(allocator);
    try renderCtg(allocator, fg, options, &out);

    // Assert: first line: "# zcodeprism graph -- myproject"
    const output = out.items;
    const first_line_end = std.mem.indexOfScalar(u8, output, '\n') orelse output.len;
    const first_line = output[0..first_line_end];
    try std.testing.expectEqualStrings("# zcodeprism graph -- myproject", first_line);
}

test "header line 2 has stats" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit(allocator);
    var out: std.ArrayList(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    const fg = try g.freeze(allocator);
    try renderCtg(allocator, fg, options, &out);

    // Assert: second line starts with "# " and contains "files" and "functions"
    const output = out.items;
    var lines = std.mem.splitScalar(u8, output, '\n');
    _ = lines.next(); // skip line 1
    const line2 = lines.next() orelse return error.MissingLine;
    try std.testing.expect(std.mem.startsWith(u8, line2, "# "));
    try std.testing.expect(std.mem.indexOf(u8, line2, "files") != null);
    try std.testing.expect(std.mem.indexOf(u8, line2, "functions") != null);
    // Header counts free functions only; the method next() under Tokenizer must not inflate the count.
    try std.testing.expect(std.mem.indexOf(u8, line2, "2 functions") != null);
    try std.testing.expect(std.mem.indexOf(u8, line2, "3 functions") == null);
}

test "header line 3 has languages" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit(allocator);
    var out: std.ArrayList(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    const fg = try g.freeze(allocator);
    try renderCtg(allocator, fg, options, &out);

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
    defer g.deinit(allocator);
    var out: std.ArrayList(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    const fg = try g.freeze(allocator);
    try renderCtg(allocator, fg, options, &out);

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
    defer g.deinit(allocator);
    var out: std.ArrayList(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
        .filter = .{ .include_test_nodes = true, .include_external_nodes = true },
    };

    // Act
    const fg = try g.freeze(allocator);
    try renderCtg(allocator, fg, options, &out);

    // Assert: sections in order
    const output = out.items;
    const section_markers = [_][]const u8{
        "[files]",
        "[types]",
        "[enums]",
        "[functions]",
        "[constants]",
        "[errors]",
        "[tests]",
        "[externals]",
        "[edges]",
    };

    var last_pos: usize = 0;
    for (section_markers) |section| {
        const pos = std.mem.indexOf(u8, output[last_pos..], section) orelse
            return error.MissingSection;
        last_pos = last_pos + pos + section.len;
    }
}

test "file IDs use f: prefix" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit(allocator);
    var out: std.ArrayList(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    const fg = try g.freeze(allocator);
    try renderCtg(allocator, fg, options, &out);

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
    defer g.deinit(allocator);
    var out: std.ArrayList(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    const fg = try g.freeze(allocator);
    try renderCtg(allocator, fg, options, &out);

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

test "type IDs use ty: prefix" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit(allocator);
    var out: std.ArrayList(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    const fg = try g.freeze(allocator);
    try renderCtg(allocator, fg, options, &out);

    // Assert
    const output = out.items;
    const types_start = (std.mem.indexOf(u8, output, "[types]\n") orelse return error.MissingSection) + "[types]\n".len;
    const types_end = std.mem.indexOf(u8, output[types_start..], "\n[") orelse (output.len - types_start);
    const types_section = output[types_start .. types_start + types_end];

    var lines = std.mem.splitScalar(u8, types_section, '\n');
    var found_any = false;
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        if (std.mem.startsWith(u8, line, "  ")) continue;
        try std.testing.expect(std.mem.startsWith(u8, line, "ty:"));
        found_any = true;
    }
    try std.testing.expect(found_any);
}

test "enum IDs use en: prefix" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit(allocator);
    var out: std.ArrayList(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    const fg = try g.freeze(allocator);
    try renderCtg(allocator, fg, options, &out);

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

test "union IDs use un: prefix" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = Graph.init("/tmp/project");
    defer g.deinit(allocator);

    const file_id = try g.addNode(allocator, .{
        .id = .root,
        .name = "src/lib.zig",
        .kind = .file,
        .language = .zig,
        .file_path = "src/lib.zig",
        .line_start = 1,
        .line_end = 50,
        .visibility = .public,
    });
    _ = try g.addNode(allocator, .{
        .id = .root,
        .name = "Value",
        .kind = .union_def,
        .language = .zig,
        .file_path = "src/lib.zig",
        .line_start = 5,
        .line_end = 15,
        .parent_id = file_id,
        .visibility = .public,
    });

    var out: std.ArrayList(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    const fg = try g.freeze(allocator);
    try renderCtg(allocator, fg, options, &out);

    // Assert: [unions] section exists with un: prefix
    const output = out.items;
    const uns_start = (std.mem.indexOf(u8, output, "[unions]\n") orelse return error.MissingSection) + "[unions]\n".len;
    const section_start = uns_start;
    const section_end = std.mem.indexOf(u8, output[section_start..], "\n[") orelse (output.len - section_start);
    const section = output[section_start .. section_start + section_end];

    var lines = std.mem.splitScalar(u8, section, '\n');
    var found = false;
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        if (std.mem.startsWith(u8, line, "  ")) continue;
        try std.testing.expect(std.mem.startsWith(u8, line, "un:"));
        found = true;
    }
    try std.testing.expect(found);

    // Assert: header mentions unions count
    try std.testing.expect(std.mem.indexOf(u8, output, "unions") != null);
}

test "constant IDs use c: prefix" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit(allocator);
    var out: std.ArrayList(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    const fg = try g.freeze(allocator);
    try renderCtg(allocator, fg, options, &out);

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
    defer g.deinit(allocator);
    var out: std.ArrayList(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    const fg = try g.freeze(allocator);
    try renderCtg(allocator, fg, options, &out);

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
    defer g.deinit(allocator);
    var out: std.ArrayList(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
        .filter = .{ .include_test_nodes = true },
    };

    // Act
    const fg = try g.freeze(allocator);
    try renderCtg(allocator, fg, options, &out);

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
    defer g.deinit(allocator);
    var out: std.ArrayList(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
        .filter = .{ .include_external_nodes = true },
    };

    // Act
    const fg = try g.freeze(allocator);
    try renderCtg(allocator, fg, options, &out);

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
    defer g.deinit(allocator);
    var out: std.ArrayList(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    const fg = try g.freeze(allocator);
    try renderCtg(allocator, fg, options, &out);

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
    defer g.deinit(allocator);
    var out: std.ArrayList(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    const fg = try g.freeze(allocator);
    try renderCtg(allocator, fg, options, &out);

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
    defer g1.deinit(allocator);
    var out1: std.ArrayList(u8) = .{};
    defer out1.deinit(allocator);

    var g2 = try createCtgTestGraph(allocator);
    defer g2.deinit(allocator);
    var out2: std.ArrayList(u8) = .{};
    defer out2.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    const fg1 = try g1.freeze(allocator);
    try renderCtg(allocator, fg1, options, &out1);
    const fg2 = try g2.freeze(allocator);
    try renderCtg(allocator, fg2, options, &out2);

    // Assert: byte-identical output
    try std.testing.expectEqualSlices(u8, out1.items, out2.items);
}

test "empty graph renders without crash" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = Graph.init("/tmp/project");
    defer g.deinit(allocator);
    var out: std.ArrayList(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "empty",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    const fg = try g.freeze(allocator);
    try renderCtg(allocator, fg, options, &out);

    // Assert: has header, no crash
    const output = out.items;
    try std.testing.expect(output.len > 0);
    try std.testing.expect(std.mem.startsWith(u8, output, "# zcodeprism graph"));
}

test "single file no functions" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createSingleFileGraph(allocator);
    defer g.deinit(allocator);
    var out: std.ArrayList(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "minimal",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    const fg = try g.freeze(allocator);
    try renderCtg(allocator, fg, options, &out);

    // Assert
    const output = out.items;
    try std.testing.expect(std.mem.indexOf(u8, output, "[files]") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "src/empty.zig") != null);
}

test "with scope filters nodes" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit(allocator);
    var out: std.ArrayList(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
        .scope = "src/main",
    };

    // Act
    const fg = try g.freeze(allocator);
    try renderCtg(allocator, fg, options, &out);

    // Assert: only src/main.zig nodes appear, not src/lib.zig nodes
    const output = out.items;
    try std.testing.expect(std.mem.indexOf(u8, output, "src/main.zig") != null);
    const types_start = std.mem.indexOf(u8, output, "[types]");
    if (types_start) |start| {
        const next_section = std.mem.indexOf(u8, output[start + 1 ..], "\n[");
        const end = if (next_section) |ns| start + 1 + ns else output.len;
        const types_content = output[start..end];
        try std.testing.expect(std.mem.indexOf(u8, types_content, "Tokenizer") == null);
    }
}

test "phantom nodes in externals section" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit(allocator);
    var out: std.ArrayList(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
        .filter = .{ .include_external_nodes = true },
    };

    // Act
    const fg = try g.freeze(allocator);
    try renderCtg(allocator, fg, options, &out);

    // Assert
    const output = out.items;
    const ext_start = std.mem.indexOf(u8, output, "[externals]") orelse return error.MissingSection;
    const ext_section = output[ext_start..];
    try std.testing.expect(std.mem.indexOf(u8, ext_section, "(stdlib)") != null);
    try std.testing.expect(std.mem.indexOf(u8, ext_section, "std") != null);
}

test "snapshot line present when options set" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit(allocator);
    var out: std.ArrayList(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
        .snapshot_name = "main-baseline",
        .source_hash = "\xa7\xf3\xb2\xc9\xe1\xd4\x00\x11\x22\x33\x44\x55\x66\x77\x88\x99".*,
    };

    // Act
    const fg = try g.freeze(allocator);
    try renderCtg(allocator, fg, options, &out);

    // Assert
    const output = out.items;
    try std.testing.expect(std.mem.indexOf(u8, output, "# snapshot: main-baseline | source_hash: a7f3b2c9e1d400112233445566778899") != null);
}

test "snapshot line absent for export" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = try createCtgTestGraph(allocator);
    defer g.deinit(allocator);
    var out: std.ArrayList(u8) = .{};
    defer out.deinit(allocator);

    const options = RenderOptions{
        .project_name = "myproject",
        .timestamp = "2026-02-14T10:30:00Z",
    };

    // Act
    const fg = try g.freeze(allocator);
    try renderCtg(allocator, fg, options, &out);

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
