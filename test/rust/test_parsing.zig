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
const Visibility = zcodeprism.types.Visibility;
const LangMeta = zcodeprism.language.LangMeta;
const Logger = zcodeprism.logging.Logger;
const GraphIndex = zcodeprism.graph_index_mod.GraphIndex;
const parse = zcodeprism.rust_visitor.parse;
const buildEdges = zcodeprism.rust_visitor.buildEdges;

/// Parse source and build edges in one step, for single-file tests.
fn parseWithEdges(allocator: std.mem.Allocator, source: []const u8, g: *Graph) !void {
    try parse(allocator, std.testing.io, source, g, null, Logger.noop);
    var gi = try GraphIndex.build(allocator, g.nodes.items);
    defer gi.deinit(allocator);
    var phantom_mgr = zcodeprism.phantom.PhantomManager.init(g);
    defer phantom_mgr.deinit(allocator);
    var wl = zcodeprism.lsp.worklist.LspWorklist{};
    defer wl.deinit(allocator);
    var ntm = zcodeprism.language_support.NodeTypeMap{};
    defer ntm.deinit(allocator);
    try buildEdges(allocator, std.testing.io, source, g, 0, g.nodeCount(), null, &gi, &phantom_mgr, &ntm, &wl, Logger.noop);
}

// --- Nominal tests (simple.rs) ---

test "parses public function" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    var found = false;
    for (g.nodes.items) |n| {
        if (n.kind == .function and n.visibility == .public) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "parses private function" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    var found = false;
    for (g.nodes.items) |n| {
        if (n.kind == .function and n.visibility == .private) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "parses struct" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    try std.testing.expect(helpers.findNode(&g, "Point", .type_def) != null);
}

test "parses enum" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    try std.testing.expect(helpers.findNode(&g, "Color", .enum_def) != null);
}

test "parses union" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    try std.testing.expect(helpers.findNode(&g, "IntOrFloat", .union_def) != null);
}

test "parses trait" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    const node = helpers.findNode(&g, "Drawable", .type_def);
    try std.testing.expect(node != null);
    try std.testing.expectEqual(LangMeta{ .rust = .{ .sub_kind = .trait_ } }, node.?.lang_meta);
}

test "parses impl block" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    var found = false;
    for (g.nodes.items) |n| {
        if (n.kind == .type_def) {
            if (n.lang_meta == .rust and n.lang_meta.rust.sub_kind == .impl_block) {
                found = true;
                break;
            }
        }
    }
    try std.testing.expect(found);
}

test "parses inline module" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    try std.testing.expect(helpers.findNode(&g, "utils", .module) != null);
}

test "parses constant" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    try std.testing.expect(helpers.findNode(&g, "MAX_SIZE", .constant) != null);
}

test "parses static item" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    const node = helpers.findNode(&g, "COUNTER", .constant);
    try std.testing.expect(node != null);
    try std.testing.expect(node.?.lang_meta == .rust);
    try std.testing.expectEqual(zcodeprism.rust_meta.RustSubKind.static_item, node.?.lang_meta.rust.sub_kind);
}

test "parses type alias" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    const node = helpers.findNode(&g, "Result", .type_def);
    try std.testing.expect(node != null);
    try std.testing.expect(node.?.lang_meta == .rust);
    try std.testing.expectEqual(zcodeprism.rust_meta.RustSubKind.type_alias, node.?.lang_meta.rust.sub_kind);
}

test "parses macro_rules" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    const node = helpers.findNode(&g, "say_hello", .function);
    try std.testing.expect(node != null);
    try std.testing.expect(node.?.lang_meta == .rust);
    try std.testing.expectEqual(zcodeprism.rust_meta.RustSubKind.macro_rules, node.?.lang_meta.rust.sub_kind);
}

test "parses test function" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    try std.testing.expect(helpers.countNodesByKind(&g, .test_def) >= 1);
}

test "parses use declaration" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    try std.testing.expect(helpers.countNodesByKind(&g, .import_decl) >= 1);
}

test "captures attributes and doc comments on use declarations" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    var found_cfg = false;
    var found_plain = false;
    var found_doc = false;
    for (g.nodes.items) |n| {
        if (n.kind != .import_decl) continue;
        const sig = n.signature orelse continue;

        if (std.mem.indexOf(u8, sig, "HashMap") != null) {
            try std.testing.expect(n.lang_meta == .rust);
            try std.testing.expect(n.lang_meta.rust.attributes != null);
            try std.testing.expect(std.mem.indexOf(u8, n.lang_meta.rust.attributes.?, "cfg") != null);
            found_cfg = true;
        } else if (std.mem.eql(u8, sig, "use std::fmt")) {
            try std.testing.expect(n.lang_meta == .rust);
            try std.testing.expectEqual(@as(?[]const u8, null), n.lang_meta.rust.attributes);
            found_plain = true;
        } else if (std.mem.indexOf(u8, sig, "std::io") != null) {
            try std.testing.expect(n.doc != null);
            try std.testing.expect(std.mem.indexOf(u8, n.doc.?, "convenience") != null);
            found_doc = true;
        }
    }
    try std.testing.expect(found_cfg);
    try std.testing.expect(found_plain);
    try std.testing.expect(found_doc);
}

test "attaches outer doc comment" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    const node = helpers.findNode(&g, "Point", .type_def);
    try std.testing.expect(node != null);
    try std.testing.expect(node.?.doc != null);
    try std.testing.expect(std.mem.indexOf(u8, node.?.doc.?, "point") != null or
        std.mem.indexOf(u8, node.?.doc.?, "Point") != null or
        std.mem.indexOf(u8, node.?.doc.?, "2D") != null);
}

test "attaches inner doc comment" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert: file node (first node) has doc from //! lines
    try std.testing.expect(g.nodes.items.len > 0);
    const file_node = g.nodes.items[0];
    try std.testing.expectEqual(NodeKind.file, file_node.kind);
    try std.testing.expect(file_node.doc != null);
    try std.testing.expect(std.mem.indexOf(u8, file_node.doc.?, "geometry") != null);
}

test "sets parent_id for method in impl" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert: "new" method has a parent_id pointing to an impl block
    const new_node = helpers.findNode(&g, "new", .function);
    try std.testing.expect(new_node != null);
    try std.testing.expect(new_node.?.parent_id != null);
    const parent = g.getNode(new_node.?.parent_id.?);
    try std.testing.expect(parent != null);
    try std.testing.expectEqual(NodeKind.type_def, parent.?.kind);
}

test "sets parent_id for top-level function" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert: top-level "helper" function has parent_id pointing to file
    const node = helpers.findNode(&g, "helper", .function);
    try std.testing.expect(node != null);
    try std.testing.expect(node.?.parent_id != null);
    const parent = g.getNode(node.?.parent_id.?);
    try std.testing.expect(parent != null);
    try std.testing.expectEqual(NodeKind.file, parent.?.kind);
}

test "detects unsafe" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    const node = helpers.findNode(&g, "dangerous_operation", .function);
    try std.testing.expect(node != null);
    try std.testing.expect(node.?.lang_meta == .rust);
    try std.testing.expect(node.?.lang_meta.rust.is_unsafe);
}

test "detects async" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    const node = helpers.findNode(&g, "fetch_data", .function);
    try std.testing.expect(node != null);
    try std.testing.expect(node.?.lang_meta == .rust);
    try std.testing.expect(node.?.lang_meta.rust.is_async);
}

test "detects const fn" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    const node = helpers.findNode(&g, "const_add", .function);
    try std.testing.expect(node != null);
    try std.testing.expect(node.?.lang_meta == .rust);
    try std.testing.expect(node.?.lang_meta.rust.is_const);
}

test "detects extern with abi" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    const node = helpers.findNode(&g, "c_callback", .function);
    try std.testing.expect(node != null);
    try std.testing.expect(node.?.lang_meta == .rust);
    try std.testing.expect(node.?.lang_meta.rust.is_extern);
    try std.testing.expect(node.?.lang_meta.rust.abi != null);
    try std.testing.expectEqualStrings("C", node.?.lang_meta.rust.abi.?);
}

test "extracts derives" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    const node = helpers.findNode(&g, "Color", .enum_def);
    try std.testing.expect(node != null);
    try std.testing.expect(node.?.lang_meta == .rust);
    try std.testing.expect(node.?.lang_meta.rust.derives != null);
    const derives = node.?.lang_meta.rust.derives.?;
    try std.testing.expect(std.mem.indexOf(u8, derives, "Debug") != null);
    try std.testing.expect(std.mem.indexOf(u8, derives, "Clone") != null);
}

test "creates calls edge" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert: at least one calls edge exists
    var found = false;
    for (g.edges.items) |e| {
        if (e.edge_type == .calls) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "creates uses_type edge from signature and body" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert: signature-detected uses_type (parameter type reference)
    const distance_id = for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "distance")) break @as(NodeId, @enumFromInt(i));
    } else return error.NodeNotFound;

    const point_id = for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Point") and
            (n.lang_meta != .rust or n.lang_meta.rust.sub_kind != .impl_block))
            break @as(NodeId, @enumFromInt(i));
    } else return error.NodeNotFound;

    try std.testing.expect(helpers.hasEdge(&g, distance_id, point_id, .uses_type));

    // Assert: body-detected uses_type (struct literal in function body)
    const literal_id = for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "creates_point_literal")) break @as(NodeId, @enumFromInt(i));
    } else return error.NodeNotFound;

    try std.testing.expect(helpers.hasEdge(&g, literal_id, point_id, .uses_type));
}

test "creates implements edge" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert: impl Drawable for Point creates an implements edge
    var found = false;
    for (g.edges.items) |e| {
        if (e.edge_type == .implements) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "all edges have source tree_sitter" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    try std.testing.expect(g.edgeCount() > 0);
    for (g.edges.items) |e| {
        try std.testing.expectEqual(EdgeSource.tree_sitter, e.source);
    }
}

test "all nodes have language rust" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    for (g.nodes.items) |n| {
        try std.testing.expectEqual(@as(?Language, .rust), n.language);
    }
}

test "file node has correct line count" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    try std.testing.expect(g.nodes.items.len > 0);
    const file_node = g.nodes.items[0];
    try std.testing.expectEqual(NodeKind.file, file_node.kind);
    try std.testing.expect(file_node.line_end != null);
    try std.testing.expect(file_node.line_end.? > 50);
}

test "function signature extraction" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    const node = helpers.findNode(&g, "new", .function);
    try std.testing.expect(node != null);
    try std.testing.expect(node.?.signature != null);
}

// --- Generic impl block tests ---

test "parses generic impl blocks and their methods" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert: impl<T> Wrapper<T> creates an impl_block named "Wrapper"
    var wrapper_impl_count: usize = 0;
    for (g.nodes.items) |n| {
        if (n.kind == .type_def and
            n.lang_meta == .rust and
            n.lang_meta.rust.sub_kind == .impl_block and
            std.mem.eql(u8, n.name, "Wrapper"))
        {
            wrapper_impl_count += 1;
        }
    }
    // Four generic impl blocks for Wrapper: inherent, Display, From, IntoIterator (ref)
    try std.testing.expectEqual(@as(usize, 4), wrapper_impl_count);

    // Assert: methods inside generic impl blocks are captured
    try std.testing.expect(helpers.findNode(&g, "into_inner", .function) != null);
    try std.testing.expect(helpers.findNode(&g, "fmt", .function) != null);
    try std.testing.expect(helpers.findNode(&g, "from", .function) != null);
}

test "trait impl for reference type names node after target type" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert: impl<'a, T> IntoIterator for &'a Wrapper<T> creates an impl_block
    // named "Wrapper" (the target type), with the full impl header as signature.
    var found = false;
    for (g.nodes.items) |n| {
        if (n.kind == .type_def and
            n.lang_meta == .rust and
            n.lang_meta.rust.sub_kind == .impl_block and
            std.mem.eql(u8, n.name, "Wrapper") and
            n.signature != null and
            std.mem.eql(u8, n.signature.?, "impl<'a, T> IntoIterator for &'a Wrapper<T>"))
        {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

// --- Edge case tests ---

test "empty file produces only file node" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.edge_cases.empty, &g);

    // Assert
    try std.testing.expectEqual(@as(usize, 1), g.nodeCount());
    try std.testing.expectEqual(NodeKind.file, g.nodes.items[0].kind);
}

test "only comments produces only file node" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.edge_cases.only_comments, &g);

    // Assert
    try std.testing.expectEqual(@as(usize, 1), g.nodeCount());
    try std.testing.expectEqual(NodeKind.file, g.nodes.items[0].kind);
}

test "no pub file has all private nodes" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.edge_cases.no_pub, &g);

    // Assert: all non-file nodes are private
    for (g.nodes.items) |n| {
        if (n.kind != .file) {
            try std.testing.expectEqual(Visibility.private, n.visibility);
        }
    }
}

test "deeply nested sets correct parent chain" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.edge_cases.deeply_nested, &g);

    // Assert: get_value exists and has a parent chain leading to file
    const method = helpers.findNode(&g, "get_value", .function);
    try std.testing.expect(method != null);
    try std.testing.expect(method.?.parent_id != null);

    // Walk up the parent chain and verify it terminates at file
    var current_id = method.?.parent_id.?;
    var depth: usize = 0;
    while (depth < 20) : (depth += 1) {
        const n = g.getNode(current_id) orelse break;
        if (n.kind == .file) break;
        current_id = n.parent_id orelse break;
    }
    const final_node = g.getNode(current_id);
    try std.testing.expect(final_node != null);
    try std.testing.expectEqual(NodeKind.file, final_node.?.kind);
}

test "many attributes do not crash" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.edge_cases.many_attrs, &g);

    // Assert: parse completed, at least file + struct + function
    try std.testing.expect(g.nodeCount() >= 3);
}

test "captures non-derive attributes on struct" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.edge_cases.many_attrs, &g);

    // Assert
    const node = helpers.findNode(&g, "Annotated", .type_def);
    try std.testing.expect(node != null);
    try std.testing.expect(node.?.lang_meta == .rust);
    const attrs = node.?.lang_meta.rust.attributes;
    try std.testing.expect(attrs != null);
    try std.testing.expect(std.mem.indexOf(u8, attrs.?, "cfg") != null);
    try std.testing.expect(std.mem.indexOf(u8, attrs.?, "allow") != null);
}

test "captures non-derive attributes on function" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.edge_cases.many_attrs, &g);

    // Assert
    const node = helpers.findNode(&g, "heavily_attributed", .function);
    try std.testing.expect(node != null);
    try std.testing.expect(node.?.lang_meta == .rust);
    const attrs = node.?.lang_meta.rust.attributes;
    try std.testing.expect(attrs != null);
    try std.testing.expect(std.mem.indexOf(u8, attrs.?, "inline") != null);
    try std.testing.expect(std.mem.indexOf(u8, attrs.?, "must_use") != null);
    try std.testing.expect(std.mem.indexOf(u8, attrs.?, "cfg") != null);
}

test "struct with only derive has null attributes" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert: Color has #[derive(Debug, Clone)] but no other attributes
    const node = helpers.findNode(&g, "Color", .enum_def);
    try std.testing.expect(node != null);
    try std.testing.expect(node.?.lang_meta == .rust);
    try std.testing.expect(node.?.lang_meta.rust.derives != null);
    try std.testing.expectEqual(@as(?[]const u8, null), node.?.lang_meta.rust.attributes);
}

test "captures attributes on struct fields" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.edge_cases.many_attrs, &g);

    // Assert: attributed field has serde rename captured
    const id_node = helpers.findNode(&g, "id", .field);
    try std.testing.expect(id_node != null);
    try std.testing.expect(id_node.?.lang_meta == .rust);
    const attrs = id_node.?.lang_meta.rust.attributes;
    try std.testing.expect(attrs != null);
    try std.testing.expect(std.mem.indexOf(u8, attrs.?, "serde") != null);

    // Assert: unattributed field has null attributes
    const plain_node = helpers.findNode(&g, "plain", .field);
    try std.testing.expect(plain_node != null);
    try std.testing.expect(plain_node.?.lang_meta == .rust);
    try std.testing.expectEqual(@as(?[]const u8, null), plain_node.?.lang_meta.rust.attributes);
}

test "captures attributes on enum variants" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.edge_cases.many_attrs, &g);

    // Assert: #[default] on Active
    const active = helpers.findNode(&g, "Active", .field);
    try std.testing.expect(active != null);
    try std.testing.expect(active.?.lang_meta == .rust);
    const active_attrs = active.?.lang_meta.rust.attributes;
    try std.testing.expect(active_attrs != null);
    try std.testing.expect(std.mem.indexOf(u8, active_attrs.?, "default") != null);

    // Assert: #[serde(rename = "off")] on Inactive
    const inactive = helpers.findNode(&g, "Inactive", .field);
    try std.testing.expect(inactive != null);
    try std.testing.expect(inactive.?.lang_meta == .rust);
    const inactive_attrs = inactive.?.lang_meta.rust.attributes;
    try std.testing.expect(inactive_attrs != null);
    try std.testing.expect(std.mem.indexOf(u8, inactive_attrs.?, "serde") != null);

    // Assert: unattributed variant has null attributes
    const plain = helpers.findNode(&g, "Plain", .field);
    try std.testing.expect(plain != null);
    try std.testing.expect(plain.?.lang_meta == .rust);
    try std.testing.expectEqual(@as(?[]const u8, null), plain.?.lang_meta.rust.attributes);
}

// --- Visibility inheritance tests ---

test "pub enum variants inherit public visibility" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert: Color is a pub enum, so its variants should be public
    const red = helpers.findNode(&g, "Red", .field);
    try std.testing.expect(red != null);
    try std.testing.expectEqual(Visibility.public, red.?.visibility);

    const green = helpers.findNode(&g, "Green", .field);
    try std.testing.expect(green != null);
    try std.testing.expectEqual(Visibility.public, green.?.visibility);

    const custom = helpers.findNode(&g, "Custom", .field);
    try std.testing.expect(custom != null);
    try std.testing.expectEqual(Visibility.public, custom.?.visibility);
}

test "macro_export makes macro public" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert: exported_macro has #[macro_export] so it should be public
    const node = helpers.findNode(&g, "exported_macro", .function);
    try std.testing.expect(node != null);
    try std.testing.expectEqual(Visibility.public, node.?.visibility);
    try std.testing.expect(node.?.lang_meta == .rust);
    try std.testing.expectEqual(zcodeprism.rust_meta.RustSubKind.macro_rules, node.?.lang_meta.rust.sub_kind);
}

test "macro without macro_export stays private" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert: say_hello has no #[macro_export] so it should be private
    const node = helpers.findNode(&g, "say_hello", .function);
    try std.testing.expect(node != null);
    try std.testing.expectEqual(Visibility.private, node.?.visibility);
}

test "associated type inherits trait visibility" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert: Output is an associated type declared in pub trait Drawable,
    // so it inherits the trait's public visibility.
    const output_node = helpers.findNode(&g, "Output", .type_def);
    try std.testing.expect(output_node != null);
    try std.testing.expect(output_node.?.lang_meta == .rust);
    try std.testing.expectEqual(zcodeprism.rust_meta.RustSubKind.associated_type, output_node.?.lang_meta.rust.sub_kind);
    try std.testing.expectEqual(Visibility.public, output_node.?.visibility);
}

// --- Doc comment extraction on fields, variants, impl blocks ---

test "attaches doc comment to struct fields and enum variants" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert: struct field has doc
    const x_node = helpers.findNode(&g, "x", .field);
    try std.testing.expect(x_node != null);
    try std.testing.expect(x_node.?.doc != null);
    try std.testing.expect(std.mem.indexOf(u8, x_node.?.doc.?, "x coordinate") != null);

    // Assert: enum variant with doc
    const red_node = helpers.findNode(&g, "Red", .field);
    try std.testing.expect(red_node != null);
    try std.testing.expect(red_node.?.doc != null);
    try std.testing.expect(std.mem.indexOf(u8, red_node.?.doc.?, "red") != null);

    // Assert: enum variant without doc has null
    const custom_node = helpers.findNode(&g, "Custom", .field);
    try std.testing.expect(custom_node != null);
    try std.testing.expectEqual(@as(?[]const u8, null), custom_node.?.doc);
}

test "attaches doc comment to impl blocks" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert: inherent impl Point has doc
    var found_inherent = false;
    var found_trait = false;
    for (g.nodes.items) |n| {
        if (n.kind == .type_def and
            n.lang_meta == .rust and
            n.lang_meta.rust.sub_kind == .impl_block and
            std.mem.eql(u8, n.name, "Point") and
            n.doc != null)
        {
            if (n.signature != null and
                std.mem.eql(u8, n.signature.?, "impl Point") and
                std.mem.indexOf(u8, n.doc.?, "Inherent") != null)
            {
                found_inherent = true;
            }
            if (n.signature != null and
                std.mem.eql(u8, n.signature.?, "impl Drawable for Point") and
                std.mem.indexOf(u8, n.doc.?, "Drawable") != null)
            {
                found_trait = true;
            }
        }
    }
    try std.testing.expect(found_inherent);
    try std.testing.expect(found_trait);
}

// --- Tuple variant field attribute tests ---

test "attributed tuple variant has one field with type signature and attribute in lang_meta" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.edge_cases.tuple_field_attrs, &g);

    // Assert: IoError(#[from] io::Error) has exactly one child field
    const variant_id = for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .field and std.mem.eql(u8, n.name, "IoError")) break @as(NodeId, @enumFromInt(i));
    } else unreachable;

    var child_count: usize = 0;
    var field: ?*const Node = null;
    for (g.nodes.items) |*n| {
        if (n.parent_id != null and n.parent_id.? == variant_id and n.kind == .field) {
            child_count += 1;
            field = n;
        }
    }
    try std.testing.expectEqual(@as(usize, 1), child_count);

    const f = field.?;
    try std.testing.expectEqualStrings("0", f.name);
    try std.testing.expect(f.signature != null);
    try std.testing.expect(std.mem.indexOf(u8, f.signature.?, "Error") != null);
    try std.testing.expect(std.mem.indexOf(u8, f.signature.?, "#[") == null);
    try std.testing.expect(f.lang_meta == .rust);
    try std.testing.expect(f.lang_meta.rust.attributes != null);
    try std.testing.expect(std.mem.indexOf(u8, f.lang_meta.rust.attributes.?, "from") != null);
}

test "tuple variant with multiple fields and attributes has correct indices" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.edge_cases.tuple_field_attrs, &g);

    // Assert: Custom(#[source] io::Error, String) has two child fields "0" and "1"
    const variant_id = for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .field and std.mem.eql(u8, n.name, "Custom")) break @as(NodeId, @enumFromInt(i));
    } else unreachable;

    var found_0 = false;
    var found_1 = false;
    for (g.nodes.items) |n| {
        if (n.parent_id != null and n.parent_id.? == variant_id and n.kind == .field) {
            if (std.mem.eql(u8, n.name, "0")) found_0 = true;
            if (std.mem.eql(u8, n.name, "1")) found_1 = true;
        }
    }
    try std.testing.expect(found_0);
    try std.testing.expect(found_1);
}

test "tuple variant without attributes has no lang_meta on fields" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.edge_cases.tuple_field_attrs, &g);

    // Assert: Plain(u32) field "0" has no attributes
    const variant_id = for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .field and std.mem.eql(u8, n.name, "Plain")) break @as(NodeId, @enumFromInt(i));
    } else unreachable;

    for (g.nodes.items) |n| {
        if (n.parent_id != null and n.parent_id.? == variant_id and
            n.kind == .field and std.mem.eql(u8, n.name, "0"))
        {
            try std.testing.expect(n.lang_meta == .none);
            return;
        }
    }
    return error.FieldNotFound;
}

test "tuple struct field attribute stored in lang_meta" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.edge_cases.tuple_field_attrs, &g);

    // Assert: Wrapper(#[serde(...)] pub String) field "0" has serde attribute
    const wrapper_id = for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Wrapper")) break @as(NodeId, @enumFromInt(i));
    } else unreachable;

    for (g.nodes.items) |n| {
        if (n.parent_id != null and n.parent_id.? == wrapper_id and
            n.kind == .field and std.mem.eql(u8, n.name, "0"))
        {
            try std.testing.expect(n.lang_meta == .rust);
            try std.testing.expect(n.lang_meta.rust.attributes != null);
            try std.testing.expect(std.mem.indexOf(u8, n.lang_meta.rust.attributes.?, "serde") != null);
            try std.testing.expectEqual(Visibility.public, n.visibility);
            return;
        }
    }
    return error.FieldNotFound;
}

// --- Attribute-before-doc ordering tests ---

test "captures attributes, derives, and test marker when placed before doc comment" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.edge_cases.many_attrs, &g);

    // Assert: function with #[allow(...)] before /// doc
    const fn_node = helpers.findNode(&g, "attr_before_doc", .function);
    try std.testing.expect(fn_node != null);
    try std.testing.expect(fn_node.?.doc != null);
    try std.testing.expect(fn_node.?.lang_meta == .rust);
    try std.testing.expect(fn_node.?.lang_meta.rust.attributes != null);
    try std.testing.expect(std.mem.indexOf(u8, fn_node.?.lang_meta.rust.attributes.?, "allow") != null);

    // Assert: struct with #[derive(...)] #[allow(...)] before /// doc
    const struct_node = helpers.findNode(&g, "DeriveBeforeDoc", .type_def);
    try std.testing.expect(struct_node != null);
    try std.testing.expect(struct_node.?.doc != null);
    try std.testing.expect(struct_node.?.lang_meta == .rust);
    try std.testing.expect(struct_node.?.lang_meta.rust.derives != null);
    try std.testing.expect(std.mem.indexOf(u8, struct_node.?.lang_meta.rust.derives.?, "Debug") != null);
    try std.testing.expect(struct_node.?.lang_meta.rust.attributes != null);
    try std.testing.expect(std.mem.indexOf(u8, struct_node.?.lang_meta.rust.attributes.?, "allow") != null);

    // Assert: #[test] after #[allow(...)] and /// doc still detected as test_def
    const test_node = helpers.findNode(&g, "test_attr_before_doc", .test_def);
    try std.testing.expect(test_node != null);
    try std.testing.expect(test_node.?.lang_meta == .rust);
    try std.testing.expect(test_node.?.lang_meta.rust.attributes != null);
    try std.testing.expect(std.mem.indexOf(u8, test_node.?.lang_meta.rust.attributes.?, "allow") != null);
}

test "sandwiched derive excluded from attrs without losing surrounding attributes" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.edge_cases.many_attrs, &g);

    // Assert
    const node = helpers.findNode(&g, "SandwichedDerive", .type_def);
    try std.testing.expect(node != null);
    try std.testing.expect(node.?.lang_meta == .rust);
    try std.testing.expect(node.?.lang_meta.rust.derives != null);
    try std.testing.expect(std.mem.indexOf(u8, node.?.lang_meta.rust.derives.?, "Debug") != null);
    const attrs = node.?.lang_meta.rust.attributes;
    try std.testing.expect(attrs != null);
    try std.testing.expect(std.mem.indexOf(u8, attrs.?, "repr") != null);
    try std.testing.expect(std.mem.indexOf(u8, attrs.?, "serde") != null);
    try std.testing.expectEqual(std.mem.indexOf(u8, attrs.?, "derive"), null);
}

test "macro_export appears in attrs" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.edge_cases.many_attrs, &g);

    // Assert
    const node = helpers.findNode(&g, "exported", .function);
    try std.testing.expect(node != null);
    try std.testing.expect(node.?.lang_meta == .rust);
    try std.testing.expectEqual(node.?.lang_meta.rust.sub_kind, .macro_rules);
    try std.testing.expectEqual(node.?.visibility, .public);
    const attrs = node.?.lang_meta.rust.attributes;
    try std.testing.expect(attrs != null);
    try std.testing.expect(std.mem.indexOf(u8, attrs.?, "macro_export") != null);
}

test "generic return type creates uses_type edge" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.project.parser_rs, &g);

    // Assert: tokenize() -> Vec<Token> creates uses_type to the local Token struct
    const tokenize_id = for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "tokenize")) break @as(NodeId, @enumFromInt(i));
    } else return error.NodeNotFound;

    const token_id = for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Token")) break @as(NodeId, @enumFromInt(i));
    } else return error.NodeNotFound;

    try std.testing.expect(helpers.hasEdge(&g, tokenize_id, token_id, .uses_type));
}

test "struct and enum field types create uses_type for local types" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator,
        \\pub struct Color { pub r: u8, pub g: u8, pub b: u8 }
        \\pub struct Pixel { pub pos: u32, pub color: Color }
        \\pub enum Container { Item(Color), Empty }
    , &g);

    const color_id = for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Color")) break @as(NodeId, @enumFromInt(i));
    } else return error.NodeNotFound;

    // Assert: Pixel (struct) has uses_type edge to Color
    const pixel_id = for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Pixel")) break @as(NodeId, @enumFromInt(i));
    } else return error.NodeNotFound;
    try std.testing.expect(helpers.hasEdge(&g, pixel_id, color_id, .uses_type));

    // Assert: Container (enum) has uses_type edge to Color
    const container_id = for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .enum_def and std.mem.eql(u8, n.name, "Container")) break @as(NodeId, @enumFromInt(i));
    } else return error.NodeNotFound;
    try std.testing.expect(helpers.hasEdge(&g, container_id, color_id, .uses_type));
}

test "Self shorthand struct init creates accesses_field edges to fields" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act: simple.rs has `Self { x, y }` inside Point::new
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    const new_id = for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "new") and
            n.parent_id != null) break @as(NodeId, @enumFromInt(i));
    } else return error.NodeNotFound;

    const x_id = for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .field and std.mem.eql(u8, n.name, "x")) break @as(NodeId, @enumFromInt(i));
    } else return error.NodeNotFound;

    const y_id = for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .field and std.mem.eql(u8, n.name, "y")) break @as(NodeId, @enumFromInt(i));
    } else return error.NodeNotFound;

    // Assert: Point::new -> accesses_field -> x and y (shorthand Self { x, y })
    try std.testing.expect(helpers.hasEdge(&g, new_id, x_id, .accesses_field));
    try std.testing.expect(helpers.hasEdge(&g, new_id, y_id, .accesses_field));
}

test "let-bound struct literal field access creates accesses_field edge" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act: simple.rs has `let p = Point { x: 1.0, y: 2.0 }; p.x`
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    const fn_id = for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "field_access_after_binding")) break @as(NodeId, @enumFromInt(i));
    } else return error.NodeNotFound;

    const x_id = for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .field and std.mem.eql(u8, n.name, "x")) break @as(NodeId, @enumFromInt(i));
    } else return error.NodeNotFound;

    // Assert: field_access_after_binding -> accesses_field -> Point::x
    try std.testing.expect(helpers.hasEdge(&g, fn_id, x_id, .accesses_field));
}
