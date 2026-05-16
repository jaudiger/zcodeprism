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
const rust_meta = zcodeprism.rust_meta;
const RustMeta = rust_meta.RustMeta;
const RustSubKind = rust_meta.RustSubKind;
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
    try std.testing.expectEqual(RustSubKind.trait_, rust_meta.metaOf(node.?).?.sub_kind);
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
            if (if (rust_meta.metaOf(&n)) |m| m.sub_kind == .impl_block else false) {
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
    try std.testing.expect(rust_meta.metaOf(node.?) != null);
    try std.testing.expectEqual(RustSubKind.static_item, rust_meta.metaOf(node.?).?.sub_kind);
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
    try std.testing.expect(rust_meta.metaOf(node.?) != null);
    try std.testing.expectEqual(RustSubKind.type_alias, rust_meta.metaOf(node.?).?.sub_kind);
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
    try std.testing.expect(rust_meta.metaOf(node.?) != null);
    try std.testing.expectEqual(RustSubKind.macro_rules, rust_meta.metaOf(node.?).?.sub_kind);
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
            try std.testing.expect(rust_meta.metaOf(&n) != null);
            try std.testing.expect(rust_meta.metaOf(&n).?.attributes != null);
            try std.testing.expect(std.mem.indexOf(u8, rust_meta.metaOf(&n).?.attributes.?, "cfg") != null);
            found_cfg = true;
        } else if (std.mem.eql(u8, sig, "use std::fmt")) {
            try std.testing.expect(rust_meta.metaOf(&n) != null);
            try std.testing.expectEqual(@as(?[]const u8, null), rust_meta.metaOf(&n).?.attributes);
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

    // Assert
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

    // Assert
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

    // Assert
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
    try std.testing.expect(rust_meta.metaOf(node.?) != null);
    try std.testing.expect(rust_meta.metaOf(node.?).?.is_unsafe);
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
    try std.testing.expect(rust_meta.metaOf(node.?) != null);
    try std.testing.expect(rust_meta.metaOf(node.?).?.is_async);
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
    try std.testing.expect(rust_meta.metaOf(node.?) != null);
    try std.testing.expect(rust_meta.metaOf(node.?).?.is_const);
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
    try std.testing.expect(rust_meta.metaOf(node.?) != null);
    try std.testing.expect(rust_meta.metaOf(node.?).?.is_extern);
    try std.testing.expect(rust_meta.metaOf(node.?).?.abi != null);
    try std.testing.expectEqualStrings("C", rust_meta.metaOf(node.?).?.abi.?);
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
    try std.testing.expect(rust_meta.metaOf(node.?) != null);
    try std.testing.expect(rust_meta.metaOf(node.?).?.derives != null);
    const derives = rust_meta.metaOf(node.?).?.derives.?;
    try std.testing.expect(std.mem.indexOf(u8, derives, "Debug") != null);
    try std.testing.expect(std.mem.indexOf(u8, derives, "Clone") != null);
}

test "creates calls edge" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
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

    // Assert
    const distance_id = for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "distance")) break @as(NodeId, @enumFromInt(i));
    } else return error.NodeNotFound;

    const point_id = for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Point") and
            (if (rust_meta.metaOf(&n)) |m| m.sub_kind != .impl_block else true))
            break @as(NodeId, @enumFromInt(i));
    } else return error.NodeNotFound;

    try std.testing.expect(helpers.hasEdge(&g, distance_id, point_id, .uses_type));

    // Assert
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

    // Assert
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

test "parses generic impl blocks and their methods" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    var wrapper_impl_count: usize = 0;
    for (g.nodes.items) |n| {
        if (n.kind == .type_def and
            std.mem.eql(u8, n.name, "Wrapper") and
            (if (rust_meta.metaOf(&n)) |m| m.sub_kind == .impl_block else false))
        {
            wrapper_impl_count += 1;
        }
    }
    // Four generic impl blocks for Wrapper: inherent, Display, From, IntoIterator (ref)
    try std.testing.expectEqual(@as(usize, 4), wrapper_impl_count);

    // Assert
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

    // Assert
    // named "Wrapper" (the target type), with the full impl header as signature.
    var found = false;
    for (g.nodes.items) |n| {
        if (n.kind == .type_def and
            std.mem.eql(u8, n.name, "Wrapper") and
            n.signature != null and
            std.mem.eql(u8, n.signature.?, "impl<'a, T> IntoIterator for &'a Wrapper<T>") and
            (if (rust_meta.metaOf(&n)) |m| m.sub_kind == .impl_block else false))
        {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

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

    // Assert
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

    // Assert
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

    // Assert
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
    try std.testing.expect(rust_meta.metaOf(node.?) != null);
    const attrs = rust_meta.metaOf(node.?).?.attributes;
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
    try std.testing.expect(rust_meta.metaOf(node.?) != null);
    const attrs = rust_meta.metaOf(node.?).?.attributes;
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

    // Assert
    const node = helpers.findNode(&g, "Color", .enum_def);
    try std.testing.expect(node != null);
    try std.testing.expect(rust_meta.metaOf(node.?) != null);
    try std.testing.expect(rust_meta.metaOf(node.?).?.derives != null);
    try std.testing.expectEqual(@as(?[]const u8, null), rust_meta.metaOf(node.?).?.attributes);
}

test "captures attributes on struct fields" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.edge_cases.many_attrs, &g);

    // Assert
    const id_node = helpers.findNode(&g, "id", .field);
    try std.testing.expect(id_node != null);
    try std.testing.expect(rust_meta.metaOf(id_node.?) != null);
    const attrs = rust_meta.metaOf(id_node.?).?.attributes;
    try std.testing.expect(attrs != null);
    try std.testing.expect(std.mem.indexOf(u8, attrs.?, "serde") != null);

    // Assert
    const plain_node = helpers.findNode(&g, "plain", .field);
    try std.testing.expect(plain_node != null);
    try std.testing.expect(rust_meta.metaOf(plain_node.?) != null);
    try std.testing.expectEqual(@as(?[]const u8, null), rust_meta.metaOf(plain_node.?).?.attributes);
}

test "captures attributes on enum variants" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.edge_cases.many_attrs, &g);

    // Assert
    const active = helpers.findNode(&g, "Active", .field);
    try std.testing.expect(active != null);
    try std.testing.expect(rust_meta.metaOf(active.?) != null);
    const active_attrs = rust_meta.metaOf(active.?).?.attributes;
    try std.testing.expect(active_attrs != null);
    try std.testing.expect(std.mem.indexOf(u8, active_attrs.?, "default") != null);

    // Assert
    const inactive = helpers.findNode(&g, "Inactive", .field);
    try std.testing.expect(inactive != null);
    try std.testing.expect(rust_meta.metaOf(inactive.?) != null);
    const inactive_attrs = rust_meta.metaOf(inactive.?).?.attributes;
    try std.testing.expect(inactive_attrs != null);
    try std.testing.expect(std.mem.indexOf(u8, inactive_attrs.?, "serde") != null);

    // Assert
    const plain = helpers.findNode(&g, "Plain", .field);
    try std.testing.expect(plain != null);
    try std.testing.expect(rust_meta.metaOf(plain.?) != null);
    try std.testing.expectEqual(@as(?[]const u8, null), rust_meta.metaOf(plain.?).?.attributes);
}

test "pub enum variants inherit public visibility" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
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

    // Assert
    const node = helpers.findNode(&g, "exported_macro", .function);
    try std.testing.expect(node != null);
    try std.testing.expectEqual(Visibility.public, node.?.visibility);
    try std.testing.expect(rust_meta.metaOf(node.?) != null);
    try std.testing.expectEqual(RustSubKind.macro_rules, rust_meta.metaOf(node.?).?.sub_kind);
}

test "macro without macro_export stays private" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
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

    // Assert
    // so it inherits the trait's public visibility.
    const output_node = helpers.findNode(&g, "Output", .type_def);
    try std.testing.expect(output_node != null);
    try std.testing.expect(rust_meta.metaOf(output_node.?) != null);
    try std.testing.expectEqual(RustSubKind.associated_type, rust_meta.metaOf(output_node.?).?.sub_kind);
    try std.testing.expectEqual(Visibility.public, output_node.?.visibility);
}

test "attaches doc comment to struct fields and enum variants" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    // Assert
    const x_node = helpers.findNode(&g, "x", .field);
    try std.testing.expect(x_node != null);
    try std.testing.expect(x_node.?.doc != null);
    try std.testing.expect(std.mem.indexOf(u8, x_node.?.doc.?, "x coordinate") != null);

    // Assert
    const red_node = helpers.findNode(&g, "Red", .field);
    try std.testing.expect(red_node != null);
    try std.testing.expect(red_node.?.doc != null);
    try std.testing.expect(std.mem.indexOf(u8, red_node.?.doc.?, "red") != null);

    // Assert
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

    // Assert
    var found_inherent = false;
    var found_trait = false;
    for (g.nodes.items) |n| {
        if (n.kind == .type_def and
            std.mem.eql(u8, n.name, "Point") and
            n.doc != null and
            (if (rust_meta.metaOf(&n)) |m| m.sub_kind == .impl_block else false))
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

test "attributed tuple variant has one field with type signature and attribute in lang_meta" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.edge_cases.tuple_field_attrs, &g);

    // Assert
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
    try std.testing.expect(rust_meta.metaOf(f) != null);
    try std.testing.expect(rust_meta.metaOf(f).?.attributes != null);
    try std.testing.expect(std.mem.indexOf(u8, rust_meta.metaOf(f).?.attributes.?, "from") != null);
}

test "tuple variant with multiple fields and attributes has correct indices" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.edge_cases.tuple_field_attrs, &g);

    // Assert
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

    // Assert
    const variant_id = for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .field and std.mem.eql(u8, n.name, "Plain")) break @as(NodeId, @enumFromInt(i));
    } else unreachable;

    for (g.nodes.items) |n| {
        if (n.parent_id != null and n.parent_id.? == variant_id and
            n.kind == .field and std.mem.eql(u8, n.name, "0"))
        {
            try std.testing.expect(n.lang_meta == null);
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

    // Assert
    const wrapper_id = for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Wrapper")) break @as(NodeId, @enumFromInt(i));
    } else unreachable;

    for (g.nodes.items) |n| {
        if (n.parent_id != null and n.parent_id.? == wrapper_id and
            n.kind == .field and std.mem.eql(u8, n.name, "0"))
        {
            try std.testing.expect(rust_meta.metaOf(&n) != null);
            try std.testing.expect(rust_meta.metaOf(&n).?.attributes != null);
            try std.testing.expect(std.mem.indexOf(u8, rust_meta.metaOf(&n).?.attributes.?, "serde") != null);
            try std.testing.expectEqual(Visibility.public, n.visibility);
            return;
        }
    }
    return error.FieldNotFound;
}

test "captures attributes, derives, and test marker when placed before doc comment" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.edge_cases.many_attrs, &g);

    // Assert
    const fn_node = helpers.findNode(&g, "attr_before_doc", .function);
    try std.testing.expect(fn_node != null);
    try std.testing.expect(fn_node.?.doc != null);
    try std.testing.expect(rust_meta.metaOf(fn_node.?) != null);
    try std.testing.expect(rust_meta.metaOf(fn_node.?).?.attributes != null);
    try std.testing.expect(std.mem.indexOf(u8, rust_meta.metaOf(fn_node.?).?.attributes.?, "allow") != null);

    // Assert
    const struct_node = helpers.findNode(&g, "DeriveBeforeDoc", .type_def);
    try std.testing.expect(struct_node != null);
    try std.testing.expect(struct_node.?.doc != null);
    try std.testing.expect(rust_meta.metaOf(struct_node.?) != null);
    try std.testing.expect(rust_meta.metaOf(struct_node.?).?.derives != null);
    try std.testing.expect(std.mem.indexOf(u8, rust_meta.metaOf(struct_node.?).?.derives.?, "Debug") != null);
    try std.testing.expect(rust_meta.metaOf(struct_node.?).?.attributes != null);
    try std.testing.expect(std.mem.indexOf(u8, rust_meta.metaOf(struct_node.?).?.attributes.?, "allow") != null);

    // Assert
    const test_node = helpers.findNode(&g, "test_attr_before_doc", .test_def);
    try std.testing.expect(test_node != null);
    try std.testing.expect(rust_meta.metaOf(test_node.?) != null);
    try std.testing.expect(rust_meta.metaOf(test_node.?).?.attributes != null);
    try std.testing.expect(std.mem.indexOf(u8, rust_meta.metaOf(test_node.?).?.attributes.?, "allow") != null);
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
    try std.testing.expect(rust_meta.metaOf(node.?) != null);
    try std.testing.expect(rust_meta.metaOf(node.?).?.derives != null);
    try std.testing.expect(std.mem.indexOf(u8, rust_meta.metaOf(node.?).?.derives.?, "Debug") != null);
    const attrs = rust_meta.metaOf(node.?).?.attributes;
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
    try std.testing.expect(rust_meta.metaOf(node.?) != null);
    try std.testing.expectEqual(rust_meta.metaOf(node.?).?.sub_kind, .macro_rules);
    try std.testing.expectEqual(node.?.visibility, .public);
    const attrs = rust_meta.metaOf(node.?).?.attributes;
    try std.testing.expect(attrs != null);
    try std.testing.expect(std.mem.indexOf(u8, attrs.?, "macro_export") != null);
}

test "generic return type creates uses_type edge" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.project.parser_rs, &g);

    // Assert
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

    // Assert
    const pixel_id = for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Pixel")) break @as(NodeId, @enumFromInt(i));
    } else return error.NodeNotFound;
    try std.testing.expect(helpers.hasEdge(&g, pixel_id, color_id, .uses_type));

    // Assert
    const container_id = for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .enum_def and std.mem.eql(u8, n.name, "Container")) break @as(NodeId, @enumFromInt(i));
    } else return error.NodeNotFound;
    try std.testing.expect(helpers.hasEdge(&g, container_id, color_id, .uses_type));
}

test "Self shorthand struct init creates accesses_field edges to fields" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
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

    // Assert
    try std.testing.expect(helpers.hasEdge(&g, new_id, x_id, .accesses_field));
    try std.testing.expect(helpers.hasEdge(&g, new_id, y_id, .accesses_field));
}

test "let-bound struct literal field access creates accesses_field edge" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parseWithEdges(std.testing.allocator, fixtures.rust.simple, &g);

    const fn_id = for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "field_access_after_binding")) break @as(NodeId, @enumFromInt(i));
    } else return error.NodeNotFound;

    const x_id = for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .field and std.mem.eql(u8, n.name, "x")) break @as(NodeId, @enumFromInt(i));
    } else return error.NodeNotFound;

    // Assert
    try std.testing.expect(helpers.hasEdge(&g, fn_id, x_id, .accesses_field));
}
