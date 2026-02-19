const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const common = @import("common.zig");
const sections = @import("mermaid_sections.zig");

const Graph = graph_mod.Graph;

const appendNum = common.appendNum;
const appendCurrentTimestamp = common.appendCurrentTimestamp;

/// Options controlling Mermaid flowchart rendering output.
pub const MermaidOptions = struct {
    project_name: []const u8,
    scope: ?[]const u8 = null,
    /// For deterministic tests; null = use system clock.
    timestamp: ?[]const u8 = null,
    filter: common.FilterOptions = .{},
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
    var assignment = try common.buildIdAssignment(allocator, g, options.scope, options.filter);
    defer assignment.deinit(allocator);

    const bytes_per_node = 120; // subgraph nesting + shape syntax + label + class assignment
    const bytes_per_edge = 50; // indentation + source ID + arrow + target ID + newline
    const header_overhead = 1024; // header lines + classDef block + section markers
    try out.ensureTotalCapacity(allocator, out.items.len +
        g.nodes.items.len * bytes_per_node +
        g.edges.items.len * bytes_per_edge +
        header_overhead);

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
    try sections.renderFileSubgraphs(out, allocator, g, assignment.file_indices.items, ids, &num_buf, &assignment.children_index);

    // Phantom subgraphs.
    try out.appendSlice(allocator, "\n    %% === Phantom subgraphs ===\n");
    try sections.renderPhantomSubgraphs(out, allocator, g, assignment.phantom_packages.items, &num_buf);

    // Ghost nodes (only when scope is active).
    try out.appendSlice(allocator, "\n    %% === Ghost nodes ===\n");
    var ghost_map = std.AutoHashMapUnmanaged(usize, u32){};
    defer ghost_map.deinit(allocator);
    if (options.scope) |scope| {
        try sections.buildGhostNodes(allocator, g, ids, scope, &ghost_map);
        try sections.renderGhostNodes(out, allocator, g, &ghost_map, &num_buf);
    }

    // Edges.
    try out.appendSlice(allocator, "\n    %% === Edges ===\n");
    try sections.renderEdges(out, allocator, g, ids, &assignment.phantom_lookup, options.scope, options.filter, &ghost_map, &num_buf);

    // Class assignments.
    try out.appendSlice(allocator, "\n    %% === Class assignments ===\n");
    try sections.renderClassAssignments(out, allocator, g, ids, assignment.phantom_packages.items, &ghost_map, &num_buf);
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
        .filter = .{ .include_external_nodes = true },
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
