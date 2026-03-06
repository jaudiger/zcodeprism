const std = @import("std");
const zcodeprism = @import("zcodeprism");
const fixtures = @import("test-fixtures");
const helpers = @import("test-helpers");

const Graph = zcodeprism.graph.Graph;
const Node = zcodeprism.node.Node;
const NodeId = zcodeprism.types.NodeId;
const NodeKind = zcodeprism.types.NodeKind;
const EdgeType = zcodeprism.types.EdgeType;
const EdgeSource = zcodeprism.types.EdgeSource;
const Language = zcodeprism.types.Language;

const indexDirectory = zcodeprism.indexer.indexDirectory;

/// Write Rust project fixture files into a temporary directory and return the real path.
fn setupProjectFixtures(tmp_dir: *std.testing.TmpDir) ![]const u8 {
    try tmp_dir.dir.writeFile(.{ .sub_path = "lib.rs", .data = fixtures.rust.project.lib_rs });
    try tmp_dir.dir.writeFile(.{ .sub_path = "parser.rs", .data = fixtures.rust.project.parser_rs });
    try tmp_dir.dir.makePath("parser");
    try tmp_dir.dir.writeFile(.{ .sub_path = "parser/helpers.rs", .data = fixtures.rust.project.parser_helpers_rs });
    try tmp_dir.dir.writeFile(.{ .sub_path = "utils.rs", .data = fixtures.rust.project.utils_rs });
    return try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
}

/// Index the Rust project fixtures using default options.
fn indexProjectFixtures(graph: *Graph, tmp_dir: *std.testing.TmpDir) !zcodeprism.indexer.IndexResult {
    const project_root = try setupProjectFixtures(tmp_dir);
    defer std.testing.allocator.free(project_root);
    return indexDirectory(std.testing.allocator, project_root, graph, .{});
}

// --- Nominal tests (project/) ---

test "indexes all rs files" {
    // Arrange
    var g = Graph.init("/tmp/rust-project");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = try indexProjectFixtures(&g, &tmp_dir);

    // Assert
    try std.testing.expectEqual(@as(usize, 4), helpers.countNodesByKind(&g, .file));
    try std.testing.expect(helpers.findNode(&g, "lib.rs", .file) != null);
    try std.testing.expect(helpers.findNode(&g, "parser.rs", .file) != null);
    try std.testing.expect(helpers.findNode(&g, "helpers.rs", .file) != null);
    try std.testing.expect(helpers.findNode(&g, "utils.rs", .file) != null);
}

test "creates import edges from mod declarations" {
    // Arrange
    var g = Graph.init("/tmp/rust-project");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = try indexProjectFixtures(&g, &tmp_dir);

    // Assert: lib.rs declares mod parser and mod utils
    const lib_file = helpers.findNode(&g, "lib.rs", .file) orelse return error.TestExpectedEqual;
    const parser_file = helpers.findNode(&g, "parser.rs", .file) orelse return error.TestExpectedEqual;
    const utils_file = helpers.findNode(&g, "utils.rs", .file) orelse return error.TestExpectedEqual;

    try std.testing.expect(helpers.hasEdge(&g, lib_file.id, parser_file.id, .imports));
    try std.testing.expect(helpers.hasEdge(&g, lib_file.id, utils_file.id, .imports));
}

test "creates phantom nodes for std" {
    // Arrange
    var g = Graph.init("/tmp/rust-project");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = try indexProjectFixtures(&g, &tmp_dir);

    // Assert: at least one phantom stdlib node exists (from use std::collections::HashMap)
    var found_stdlib = false;
    for (g.nodes.items) |n| {
        switch (n.external) {
            .stdlib => {
                found_stdlib = true;
                break;
            },
            else => {},
        }
    }
    try std.testing.expect(found_stdlib);

    // Assert: no phantom nodes for in-project modules (parser, utils, Token, etc.)
    for (g.nodes.items) |n| {
        if (n.external == .none) continue;
        const in_project = std.mem.eql(u8, n.name, "parser") or
            std.mem.eql(u8, n.name, "utils") or
            std.mem.eql(u8, n.name, "Token") or
            std.mem.eql(u8, n.name, "trim_whitespace") or
            std.mem.eql(u8, n.name, "repeat");
        try std.testing.expect(!in_project);
    }
}

test "phantom nodes have no file_path" {
    // Arrange
    var g = Graph.init("/tmp/rust-project");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = try indexProjectFixtures(&g, &tmp_dir);

    // Assert
    for (g.nodes.items) |n| {
        switch (n.external) {
            .stdlib => try std.testing.expectEqual(@as(?[]const u8, null), n.file_path),
            else => {},
        }
    }
}

test "file nodes have content_hash" {
    // Arrange
    var g = Graph.init("/tmp/rust-project");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = try indexProjectFixtures(&g, &tmp_dir);

    // Assert
    for (g.nodes.items) |n| {
        if (n.kind == .file) {
            try std.testing.expect(n.content_hash != null);
        }
    }
}

test "all nodes have language rust" {
    // Arrange
    var g = Graph.init("/tmp/rust-project");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = try indexProjectFixtures(&g, &tmp_dir);

    // Assert: all non-phantom, non-structural nodes have language=.rust
    for (g.nodes.items) |n| {
        if (n.language == null) continue; // structural nodes (directories)
        switch (n.external) {
            .none => try std.testing.expectEqual(@as(?Language, .rust), n.language),
            else => {},
        }
    }
}

test "parent_id chain is consistent" {
    // Arrange
    var g = Graph.init("/tmp/rust-project");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = try indexProjectFixtures(&g, &tmp_dir);

    // Assert: for every node with a parent_id, getNode(parent_id) succeeds
    for (g.nodes.items) |n| {
        if (n.parent_id) |pid| {
            try std.testing.expect(g.getNode(pid) != null);
        }
    }

    // Assert: no parent_id cycles
    for (g.nodes.items) |n| {
        var current_id: ?NodeId = n.parent_id;
        var hops: usize = 0;
        while (current_id) |cid| {
            hops += 1;
            if (hops > 100) return error.TestExpectedEqual;
            const parent = g.getNode(cid) orelse break;
            current_id = parent.parent_id;
        }
    }
}

// --- Incremental tests ---

test "incremental skips unchanged files" {
    // Arrange
    var g = Graph.init("/tmp/rust-project");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const project_root = setupProjectFixtures(&tmp_dir) catch return error.SkipZigTest;
    defer std.testing.allocator.free(project_root);

    // Act: index twice with incremental=true
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{ .incremental = true }) catch |err| return err;
    const result2 = indexDirectory(std.testing.allocator, project_root, &g, .{ .incremental = true }) catch |err| return err;

    // Assert
    try std.testing.expect(result2.files_skipped > 0);
}

// --- Edge case tests ---

test "single file project" {
    // Arrange
    var g = Graph.init("/tmp/rust-single");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(.{
        .sub_path = "main.rs",
        .data = "fn main() { println!(\"hello\"); }\n",
    });
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert
    try std.testing.expectEqual(@as(usize, 1), helpers.countNodesByKind(&g, .file));
    try std.testing.expect(g.nodeCount() > 1);
}

test "directory with no rs files" {
    // Arrange
    var g = Graph.init("/tmp/rust-empty");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(.{
        .sub_path = "readme.txt",
        .data = "no rust here",
    });
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert
    try std.testing.expectEqual(@as(usize, 0), helpers.countNodesByKind(&g, .file));
    try std.testing.expectEqual(@as(usize, 0), g.nodeCount());
}

// --- Module resolution tests ---

test "mod foo resolves to foo.rs" {
    // Arrange
    var g = Graph.init("/tmp/rust-mod");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // lib.rs declares mod parser;
    try tmp_dir.dir.writeFile(.{
        .sub_path = "lib.rs",
        .data = "mod parser;\npub fn run() { parser::parse(\"\"); }\n",
    });
    try tmp_dir.dir.writeFile(.{
        .sub_path = "parser.rs",
        .data = "pub fn parse(input: &str) -> String { input.to_string() }\n",
    });
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: lib.rs has imports edge to parser.rs
    const lib_file = helpers.findNode(&g, "lib.rs", .file) orelse return error.TestExpectedEqual;
    const parser_file = helpers.findNode(&g, "parser.rs", .file) orelse return error.TestExpectedEqual;

    try std.testing.expect(helpers.hasEdge(&g, lib_file.id, parser_file.id, .imports));
}

test "module-prefix import resolves to qualified phantom" {
    // Arrange
    var g = Graph.init("/tmp/rust-prefix");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // "use std::fmt;" + "impl fmt::Display for Point" should produce
    // a phantom "std.fmt.Display", not a bare "Display".
    try tmp_dir.dir.writeFile(.{
        .sub_path = "lib.rs",
        .data =
        \\use std::fmt;
        \\
        \\pub struct Point { pub x: f64, pub y: f64 }
        \\
        \\impl fmt::Display for Point {
        \\    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        \\        write!(f, "({}, {})", self.x, self.y)
        \\    }
        \\}
        ,
    });
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = try indexDirectory(std.testing.allocator, project_root, &g, .{});

    // Assert: a phantom "Display" node exists whose parent is "fmt"
    var found_qualified = false;
    for (g.nodes.items) |n| {
        if (!std.mem.eql(u8, n.name, "Display")) continue;
        if (n.external != .stdlib) continue;
        const pid = n.parent_id orelse continue;
        const parent = g.getNode(pid) orelse continue;
        if (std.mem.eql(u8, parent.name, "fmt") and parent.external == .stdlib) {
            found_qualified = true;
            try std.testing.expectEqual(NodeKind.type_def, n.kind);
            try std.testing.expectEqual(NodeKind.module, parent.kind);
            break;
        }
    }
    try std.testing.expect(found_qualified);

    // Assert: the "fmt" phantom has an imports edge from the file (module import)
    const lib_file = helpers.findNode(&g, "lib.rs", .file) orelse return error.TestExpectedEqual;
    var has_fmt_import = false;
    for (g.edges.items) |e| {
        if (e.source_id == lib_file.id and e.edge_type == .imports) {
            const target = g.getNode(e.target_id) orelse continue;
            if (std.mem.eql(u8, target.name, "fmt") and target.external == .stdlib) {
                has_fmt_import = true;
                break;
            }
        }
    }
    try std.testing.expect(has_fmt_import);

    // Assert: an implements edge exists from Point to the phantom Display
    const point_node = helpers.findNode(&g, "Point", .type_def) orelse return error.TestExpectedEqual;
    var has_implements = false;
    for (g.edges.items) |e| {
        if (e.source_id == point_node.id and e.edge_type == .implements) {
            const target = g.getNode(e.target_id) orelse continue;
            if (std.mem.eql(u8, target.name, "Display") and target.external == .stdlib) {
                has_implements = true;
                break;
            }
        }
    }
    try std.testing.expect(has_implements);
}

test "aliased use import resolves to qualified phantom" {
    // Arrange
    var g = Graph.init("/tmp/rust-alias");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // "use std::fmt::Display as Disp;" + "impl Disp for Point" should produce
    // a phantom "std.fmt.Display" (the original path), not a bare "Disp".
    try tmp_dir.dir.writeFile(.{
        .sub_path = "lib.rs",
        .data =
        \\use std::fmt::Display as Disp;
        \\
        \\pub struct Point { pub x: f64, pub y: f64 }
        \\
        \\impl Disp for Point {
        \\    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        \\        write!(f, "({}, {})", self.x, self.y)
        \\    }
        \\}
        ,
    });
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = try indexDirectory(std.testing.allocator, project_root, &g, .{});

    // Assert: a phantom "Display" node exists whose parent is "fmt"
    var found_qualified = false;
    for (g.nodes.items) |n| {
        if (!std.mem.eql(u8, n.name, "Display")) continue;
        if (n.external != .stdlib) continue;
        const pid = n.parent_id orelse continue;
        const parent = g.getNode(pid) orelse continue;
        if (std.mem.eql(u8, parent.name, "fmt") and parent.external == .stdlib) {
            found_qualified = true;
            break;
        }
    }
    try std.testing.expect(found_qualified);

    // Assert: no bare "Disp" phantom exists
    var found_bare_disp = false;
    for (g.nodes.items) |n| {
        if (std.mem.eql(u8, n.name, "Disp") and n.external == .stdlib) {
            found_bare_disp = true;
            break;
        }
    }
    try std.testing.expect(!found_bare_disp);

    // Assert: an implements edge exists from Point to the phantom Display
    const point_node = helpers.findNode(&g, "Point", .type_def) orelse return error.TestExpectedEqual;
    var has_implements = false;
    for (g.edges.items) |e| {
        if (e.source_id == point_node.id and e.edge_type == .implements) {
            const target = g.getNode(e.target_id) orelse continue;
            if (std.mem.eql(u8, target.name, "Display") and target.external == .stdlib) {
                has_implements = true;
                break;
            }
        }
    }
    try std.testing.expect(has_implements);
}

test "phantom module kind and edge type follow Rust naming convention" {
    // Arrange
    var g = Graph.init("/tmp/rust-phantom-kinds");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Two files exercising both terminal-module and terminal-type phantoms.
    // "io" appears as terminal (use std::io) AND intermediate (use std::io::Read),
    // so this also verifies order-independence.
    try tmp_dir.dir.writeFile(.{
        .sub_path = "lib.rs",
        .data =
        \\mod reader;
        \\use std::io;
        \\
        \\pub fn get_io() -> io::Result<()> { Ok(()) }
        ,
    });
    try tmp_dir.dir.writeFile(.{
        .sub_path = "reader.rs",
        .data =
        \\use std::io::Read;
        \\
        \\pub fn read_all<R: Read>(r: &mut R) -> Vec<u8> { Vec::new() }
        ,
    });
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = try indexDirectory(std.testing.allocator, project_root, &g, .{});

    // Assert: snake_case "io" is kind=module, PascalCase "Read" is kind=type_def
    for (g.nodes.items) |n| {
        if (n.external != .stdlib) continue;
        if (std.mem.eql(u8, n.name, "io")) {
            try std.testing.expectEqual(NodeKind.module, n.kind);
        }
        if (std.mem.eql(u8, n.name, "Read")) {
            try std.testing.expectEqual(NodeKind.type_def, n.kind);
        }
    }

    // Assert: lib.rs has imports edge (not uses_type) to the io module phantom
    const lib_file = helpers.findNode(&g, "lib.rs", .file) orelse return error.TestExpectedEqual;
    var has_io_import = false;
    for (g.edges.items) |e| {
        if (e.source_id == lib_file.id and e.edge_type == .imports) {
            const target = g.getNode(e.target_id) orelse continue;
            if (std.mem.eql(u8, target.name, "io") and target.external == .stdlib) {
                has_io_import = true;
                break;
            }
        }
    }
    try std.testing.expect(has_io_import);
}

test "mod foo resolves to foo/mod.rs" {
    // Arrange
    var g = Graph.init("/tmp/rust-mod2");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // lib.rs declares mod parser; and parser lives at parser/mod.rs
    try tmp_dir.dir.writeFile(.{
        .sub_path = "lib.rs",
        .data = "mod parser;\npub fn run() {}\n",
    });
    try tmp_dir.dir.makePath("parser");
    try tmp_dir.dir.writeFile(.{
        .sub_path = "parser/mod.rs",
        .data = "pub fn parse(input: &str) -> String { input.to_string() }\n",
    });
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = indexDirectory(std.testing.allocator, project_root, &g, .{}) catch |err| return err;

    // Assert: lib.rs has imports edge to parser/mod.rs
    const lib_file = helpers.findNode(&g, "lib.rs", .file) orelse return error.TestExpectedEqual;
    const mod_file = helpers.findNode(&g, "mod.rs", .file) orelse return error.TestExpectedEqual;

    try std.testing.expect(helpers.hasEdge(&g, lib_file.id, mod_file.id, .imports));
}

test "super:: resolves to parent module across topological ordering" {
    // Arrange
    var g = Graph.init("/tmp/rust-super");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = try indexProjectFixtures(&g, &tmp_dir);

    // Assert: helpers.rs file exists
    const helpers_file = helpers.findNode(&g, "helpers.rs", .file) orelse return error.TestExpectedEqual;
    const parser_file = helpers.findNode(&g, "parser.rs", .file) orelse return error.TestExpectedEqual;

    // Assert: parser.rs imports helpers.rs (mod helpers;)
    try std.testing.expect(helpers.hasEdge(&g, parser_file.id, helpers_file.id, .imports));

    // Assert: uses_type edge from helpers.rs to Token in parser.rs
    const token_node = helpers.findNodeInFile(&g, "Token", .type_def, parser_file.id) orelse
        return error.TestExpectedEqual;
    var has_uses_type = false;
    for (g.edges.items) |e| {
        if (e.target_id == token_node and e.edge_type == .uses_type) {
            const src = g.getNode(e.source_id) orelse continue;
            if (helpers.findNodeInFile(&g, src.name, src.kind, helpers_file.id) != null) {
                has_uses_type = true;
                break;
            }
        }
    }
    try std.testing.expect(has_uses_type);

    // Assert: calls edge from parse_trimmed in helpers.rs to parse in parser.rs
    const parse_fn = helpers.findNodeInFile(&g, "parse", .function, parser_file.id) orelse
        return error.TestExpectedEqual;
    const parse_trimmed = helpers.findNodeInFile(&g, "parse_trimmed", .function, helpers_file.id) orelse
        return error.TestExpectedEqual;
    try std.testing.expect(helpers.hasEdge(&g, parse_trimmed, parse_fn, .calls));
}

test "qualified cross-file call resolves through impl block" {
    // Arrange
    var g = Graph.init("/tmp/rust-project");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Act
    _ = try indexProjectFixtures(&g, &tmp_dir);

    // Assert: make_token in lib.rs calls Token::new in parser.rs
    const lib_file = helpers.findNode(&g, "lib.rs", .file) orelse return error.TestExpectedEqual;
    const parser_file = helpers.findNode(&g, "parser.rs", .file) orelse return error.TestExpectedEqual;
    const make_token = helpers.findNodeInFile(&g, "make_token", .function, lib_file.id) orelse
        return error.TestExpectedEqual;
    const token_new = helpers.findNodeInFile(&g, "new", .function, parser_file.id) orelse
        return error.TestExpectedEqual;
    try std.testing.expect(helpers.hasEdge(&g, make_token, token_new, .calls));
}

// --- Transitive re-export tests ---

test "transitive re-export resolves through chain" {
    // Arrange
    var g = Graph.init("/tmp/rust-reexport");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(.{ .sub_path = "lib.rs", .data = fixtures.rust.reexport_chain.lib_rs });
    try tmp_dir.dir.writeFile(.{ .sub_path = "mid.rs", .data = fixtures.rust.reexport_chain.mid_rs });
    try tmp_dir.dir.makePath("mid");
    try tmp_dir.dir.writeFile(.{ .sub_path = "mid/deep.rs", .data = fixtures.rust.reexport_chain.deep_rs });
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = try indexDirectory(std.testing.allocator, project_root, &g, .{});

    // Assert: Widget is defined in deep.rs
    const deep_file = helpers.findNode(&g, "deep.rs", .file) orelse return error.TestExpectedEqual;
    const widget = helpers.findNodeInFile(&g, "Widget", .type_def, deep_file.id) orelse
        return error.TestExpectedEqual;
    _ = widget;

    // Assert: create_widget in lib.rs has a uses_type edge to Widget in deep.rs
    const lib_file = helpers.findNode(&g, "lib.rs", .file) orelse return error.TestExpectedEqual;
    const create_fn = helpers.findNodeInFile(&g, "create_widget", .function, lib_file.id) orelse
        return error.TestExpectedEqual;
    var has_cross_file_edge = false;
    for (g.edges.items) |e| {
        if (e.source_id != create_fn) continue;
        if (e.edge_type != .uses_type and e.edge_type != .calls) continue;
        const target = g.getNode(e.target_id) orelse continue;
        const target_file = g.findContainingFile(e.target_id) orelse continue;
        if (target_file == deep_file.id and std.mem.eql(u8, target.name, "Widget")) {
            has_cross_file_edge = true;
            break;
        }
        if (target_file == deep_file.id and std.mem.eql(u8, target.name, "new")) {
            has_cross_file_edge = true;
            break;
        }
    }
    try std.testing.expect(has_cross_file_edge);
}

test "pub use emits exports edge to resolved type" {
    // Arrange
    var g = Graph.init("/tmp/rust-exports");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(.{ .sub_path = "lib.rs", .data = fixtures.rust.reexport_chain.lib_rs });
    try tmp_dir.dir.writeFile(.{ .sub_path = "mid.rs", .data = fixtures.rust.reexport_chain.mid_rs });
    try tmp_dir.dir.makePath("mid");
    try tmp_dir.dir.writeFile(.{ .sub_path = "mid/deep.rs", .data = fixtures.rust.reexport_chain.deep_rs });
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = try indexDirectory(std.testing.allocator, project_root, &g, .{});

    // Assert: Widget and Gadget are type nodes in deep.rs
    const deep_file = helpers.findNode(&g, "deep.rs", .file) orelse return error.TestExpectedEqual;
    const widget = helpers.findNodeInFile(&g, "Widget", .type_def, deep_file.id) orelse
        return error.TestExpectedEqual;
    const gadget = helpers.findNodeInFile(&g, "Gadget", .type_def, deep_file.id) orelse
        return error.TestExpectedEqual;

    // Assert: lib.rs has exports edges to Widget and Gadget (pub use mid::{Widget, Gadget})
    const lib_file = helpers.findNode(&g, "lib.rs", .file) orelse return error.TestExpectedEqual;
    try std.testing.expect(helpers.hasEdge(&g, lib_file.id, widget, .exports));
    try std.testing.expect(helpers.hasEdge(&g, lib_file.id, gadget, .exports));

    // Assert: mid.rs has exports edges to Widget and Gadget (pub use deep::{Widget, Gadget})
    const mid_file = helpers.findNode(&g, "mid.rs", .file) orelse return error.TestExpectedEqual;
    try std.testing.expect(helpers.hasEdge(&g, mid_file.id, widget, .exports));
    try std.testing.expect(helpers.hasEdge(&g, mid_file.id, gadget, .exports));
}

// --- Glob import tests ---

test "glob import resolves public symbols but not private ones" {
    // Arrange
    var g = Graph.init("/tmp/rust-glob");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(.{ .sub_path = "lib.rs", .data = fixtures.rust.glob_import.lib_rs });
    try tmp_dir.dir.writeFile(.{ .sub_path = "sub.rs", .data = fixtures.rust.glob_import.sub_rs });
    try tmp_dir.dir.writeFile(.{ .sub_path = "utils.rs", .data = fixtures.rust.glob_import.utils_rs });
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = try indexDirectory(std.testing.allocator, project_root, &g, .{});

    // Assert: lib.rs functions have calls edges to public utils.rs functions
    const lib_file = helpers.findNode(&g, "lib.rs", .file) orelse return error.TestExpectedEqual;
    const utils_file = helpers.findNode(&g, "utils.rs", .file) orelse return error.TestExpectedEqual;
    const run_fn = helpers.findNodeInFile(&g, "run", .function, lib_file.id) orelse
        return error.TestExpectedEqual;
    const helper_fn = helpers.findNodeInFile(&g, "helper", .function, utils_file.id) orelse
        return error.TestExpectedEqual;
    try std.testing.expect(helpers.hasEdge(&g, run_fn, helper_fn, .calls));

    const run_other_fn = helpers.findNodeInFile(&g, "run_other", .function, lib_file.id) orelse
        return error.TestExpectedEqual;
    const other_fn = helpers.findNodeInFile(&g, "other", .function, utils_file.id) orelse
        return error.TestExpectedEqual;
    try std.testing.expect(helpers.hasEdge(&g, run_other_fn, other_fn, .calls));

    // Assert: no edge from lib.rs to the private function in utils.rs
    const private_fn = helpers.findNodeInFile(&g, "private_fn", .function, utils_file.id) orelse
        return error.TestExpectedEqual;
    var has_edge_to_private = false;
    for (g.edges.items) |e| {
        if (e.target_id != private_fn) continue;
        if (g.findContainingFile(e.source_id)) |fid| {
            if (fid == lib_file.id) {
                has_edge_to_private = true;
                break;
            }
        }
    }
    try std.testing.expect(!has_edge_to_private);

    // Assert: multi-segment glob (use utils::inner::*) resolves deep_helper
    const run_deep_fn = helpers.findNodeInFile(&g, "run_deep", .function, lib_file.id) orelse
        return error.TestExpectedEqual;
    const deep_helper_fn = helpers.findNodeInFile(&g, "deep_helper", .function, utils_file.id) orelse
        return error.TestExpectedEqual;
    try std.testing.expect(helpers.hasEdge(&g, run_deep_fn, deep_helper_fn, .calls));

    // Assert: super glob (use super::*) resolves run from parent
    const sub_file = helpers.findNode(&g, "sub.rs", .file) orelse return error.TestExpectedEqual;
    const call_parent_fn = helpers.findNodeInFile(&g, "call_parent", .function, sub_file.id) orelse
        return error.TestExpectedEqual;
    try std.testing.expect(helpers.hasEdge(&g, call_parent_fn, run_fn, .calls));
}

test "scoped field type creates phantom uses_type edge" {
    // Arrange
    var g = Graph.init("/tmp/rust-scoped-field");
    defer g.deinit(std.testing.allocator);
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(.{
        .sub_path = "lib.rs",
        .data =
        \\use std::io;
        \\
        \\pub enum AppError {
        \\    IoError(io::Error),
        \\    Plain(u32),
        \\}
        ,
    });
    const project_root = try tmp_dir.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(project_root);

    // Act
    _ = try indexDirectory(std.testing.allocator, project_root, &g, .{});

    // Assert: phantom Error node exists under phantom io module
    var found_error = false;
    for (g.nodes.items) |n| {
        if (!std.mem.eql(u8, n.name, "Error")) continue;
        if (n.external != .stdlib) continue;
        const pid = n.parent_id orelse continue;
        const parent = g.getNode(pid) orelse continue;
        if (std.mem.eql(u8, parent.name, "io") and parent.external == .stdlib) {
            found_error = true;
            break;
        }
    }
    try std.testing.expect(found_error);

    // Assert: AppError has uses_type edge to the phantom Error
    var app_error_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .enum_def and std.mem.eql(u8, n.name, "AppError")) {
            app_error_id = @enumFromInt(i);
            break;
        }
    }
    const ae_id = app_error_id orelse return error.TestExpectedEqual;
    var has_uses_type = false;
    for (g.edges.items) |e| {
        if (e.source_id == ae_id and e.edge_type == .uses_type) {
            const target = g.getNode(e.target_id) orelse continue;
            if (std.mem.eql(u8, target.name, "Error") and target.external == .stdlib) {
                has_uses_type = true;
                break;
            }
        }
    }
    try std.testing.expect(has_uses_type);
}
