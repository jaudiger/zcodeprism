const std = @import("std");
const graph_mod = @import("../../core/graph.zig");
const logging = @import("../../logging.zig");
const node_mod = @import("../../core/node.zig");
const types = @import("../../core/types.zig");
const lang = @import("../language.zig");
const ts = @import("tree-sitter");
const ts_api = @import("../../parser/tree_sitter_api.zig");
const ast = @import("ast_analysis.zig");
const cf = @import("cross_file.zig");
const eb = @import("edge_builder.zig");

const Field = logging.Field;
const Logger = logging.Logger;

const Graph = graph_mod.Graph;
const Node = node_mod.Node;
const NodeId = types.NodeId;
const NodeKind = types.NodeKind;
const Visibility = types.Visibility;
const Language = types.Language;
const LangMeta = lang.LangMeta;

/// Parse Zig source code and populate the graph with nodes and edges.
/// This is the entry point used by the LanguageSupport registry.
pub fn parse(source: []const u8, graph: *anyopaque, logger: Logger) anyerror!void {
    const g: *Graph = @ptrCast(@alignCast(graph));
    const log = logger.withScope("zig-visitor");

    log.debug("parsing source", &.{Field.uint("bytes", source.len)});

    const line_count = ts_api.countLines(source);

    // Parse source with tree-sitter first so we can collect module doc comments.
    const tree = ts_api.parseSource(ts_api.zigLanguage(), source) orelse {
        log.warn("tree-sitter parse failed", &.{});
        // If tree-sitter parsing fails, create a bare file node.
        _ = try g.addNode(.{
            .id = .root,
            .name = "",
            .kind = .file,
            .language = .zig,
            .visibility = .public,
            .line_start = 1,
            .line_end = if (line_count > 0) line_count else null,
        });
        return;
    };
    defer tree.destroy();

    const root = tree.rootNode();

    // Collect module doc comments (//!) from the beginning of the file.
    const module_doc = ast.collectModuleDocComment(source, root);

    // Create file node (always the first node).
    const file_id = try g.addNode(.{
        .id = .root,
        .name = "",
        .kind = .file,
        .language = .zig,
        .visibility = .public,
        .line_start = 1,
        .line_end = if (line_count > 0) line_count else null,
        .doc = module_doc,
    });

    // Create nodes from declarations.
    var i: u32 = 0;
    while (i < root.childCount()) : (i += 1) {
        const child = root.child(i) orelse continue;
        if (!child.isNamed()) continue;
        try processDeclaration(g, source, child, file_id, log);
    }

    // Build edge resolution context: scope range + import map.
    var ctx = cf.EdgeContext{
        .scope_start = @intFromEnum(file_id),
        .scope_end = g.nodeCount(),
    };
    cf.buildImportMap(g, source, root, &ctx, log);

    // Create edges (calls, uses_type) with cross-file resolution.
    log.debug("building edges", &.{});
    try eb.walkForEdges(g, source, root, &ctx, log);
}

// =========================================================================
// Node creation
// =========================================================================

fn processDeclaration(g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, log: Logger) anyerror!void {
    const kind_str = ts_node.kind();

    if (std.mem.eql(u8, kind_str, "variable_declaration")) {
        try processVariableDecl(g, source, ts_node, parent_id, log);
    } else if (std.mem.eql(u8, kind_str, "function_declaration")) {
        try processFunctionDecl(g, source, ts_node, parent_id, log);
    } else if (std.mem.eql(u8, kind_str, "test_declaration")) {
        try processTestDecl(g, source, ts_node, parent_id);
    } else if (std.mem.eql(u8, kind_str, "container_field")) {
        try processContainerField(g, source, ts_node, parent_id, log);
    }
}

fn processVariableDecl(g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, log: Logger) anyerror!void {
    const name = ast.getIdentifierName(source, ts_node) orelse {
        log.trace("skipping variable: no identifier", &.{});
        return;
    };
    const visibility = ast.detectVisibility(ts_node);
    const doc = ast.collectDocComment(source, ts_node);

    // Classify the value: struct, enum, error set, import, or plain constant.
    const classification = ast.classifyVariableValue(source, ts_node);
    const kind = classification.kind;

    // Filter noise constants that produce no useful graph information.
    if (kind == .constant) {
        // Skip @This() aliases (e.g., `const Self = @This()`).
        if (ast.isThisBuiltin(source, ts_node)) {
            log.trace("skipping @This() alias", &.{Field.string("name", name)});
            return;
        }

        // Skip same-name re-exports from imports (e.g., `const Graph = graph_mod.Graph`).
        if (ast.getFieldExprRootAndLeaf(source, ts_node)) |info| {
            if (std.mem.eql(u8, info.leaf, name)) {
                if (isImportSibling(g, parent_id, info.root)) {
                    log.trace("skipping re-export", &.{Field.string("name", name)});
                    return;
                }
            }
        }
    }

    // Comptime detection: constants with explicit type annotation are comptime-known.
    var lang_meta: LangMeta = .{ .none = {} };
    if (kind == .constant and ast.hasTypeAnnotation(ts_node)) {
        lang_meta = .{ .zig = .{ .is_comptime = true } };
    }

    // For import declarations, store the import path extracted from the AST.
    const sig: ?[]const u8 = if (kind == .import_decl) cf.extractImportPath(source, ts_node) else null;

    const node_id = try g.addNode(.{
        .id = .root,
        .name = name,
        .kind = kind,
        .language = .zig,
        .parent_id = parent_id,
        .visibility = visibility,
        .doc = doc,
        .signature = sig,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .lang_meta = lang_meta,
    });

    // Recurse into struct body for nested declarations (methods, nested types).
    if (classification.struct_body) |body| {
        var i: u32 = 0;
        while (i < body.childCount()) : (i += 1) {
            const child = body.child(i) orelse continue;
            if (!child.isNamed()) continue;
            try processDeclaration(g, source, child, node_id, log);
        }
    }
}

fn processFunctionDecl(g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, log: Logger) anyerror!void {
    const name = ast.getIdentifierName(source, ts_node) orelse {
        log.trace("skipping function: no identifier", &.{});
        return;
    };
    const visibility = ast.detectVisibility(ts_node);
    const doc = ast.collectDocComment(source, ts_node);

    // Detect type-returning generic functions: `fn Foo(comptime T: type) type { return struct { ... }; }`
    if (ast.returnsType(source, ts_node)) {
        if (ast.findReturnedTypeBody(ts_node)) |body_info| {
            const kind: NodeKind = if (body_info.is_enum) .enum_def else .type_def;
            const type_id = try g.addNode(.{
                .id = .root,
                .name = name,
                .kind = kind,
                .language = .zig,
                .parent_id = parent_id,
                .visibility = visibility,
                .doc = doc,
                .line_start = ts_node.startPoint().row + 1,
                .line_end = ts_node.endPoint().row + 1,
            });
            // Recurse into the returned struct/union/enum body.
            var i: u32 = 0;
            while (i < body_info.body.childCount()) : (i += 1) {
                const child = body_info.body.child(i) orelse continue;
                if (!child.isNamed()) continue;
                try processDeclaration(g, source, child, type_id, log);
            }
            return;
        } else {
            log.debug("type-returning function: body not found", &.{Field.string("name", name)});
        }
    }

    _ = try g.addNode(.{
        .id = .root,
        .name = name,
        .kind = .function,
        .language = .zig,
        .parent_id = parent_id,
        .visibility = visibility,
        .doc = doc,
        .signature = extractFunctionSignature(source, ts_node),
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
    });
}

/// Extract the function header text (from "fn" keyword to the body block start).
/// For `pub fn create() svc_mod.Service { ... }`, returns `pub fn create() svc_mod.Service`.
fn extractFunctionSignature(source: []const u8, ts_node: ts.Node) ?[]const u8 {
    const start = ts_node.startByte();
    var i: u32 = 0;
    while (i < ts_node.childCount()) : (i += 1) {
        const child = ts_node.child(i) orelse continue;
        if (std.mem.eql(u8, child.kind(), "block")) {
            const end = child.startByte();
            if (end > start and end <= source.len) {
                return std.mem.trimRight(u8, source[start..end], " \t\n\r");
            }
            return null;
        }
    }
    return null;
}

fn processTestDecl(g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId) anyerror!void {
    const name = ast.getTestName(source, ts_node);
    const doc = ast.collectDocComment(source, ts_node);

    _ = try g.addNode(.{
        .id = .root,
        .name = name,
        .kind = .test_def,
        .language = .zig,
        .parent_id = parent_id,
        .visibility = .private,
        .doc = doc,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
    });
}

fn processContainerField(g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, log: Logger) anyerror!void {
    const name = ast.getIdentifierName(source, ts_node) orelse {
        log.trace("skipping field: no identifier", &.{});
        return;
    };
    const doc = ast.collectDocComment(source, ts_node);

    _ = try g.addNode(.{
        .id = .root,
        .name = name,
        .kind = .field,
        .language = .zig,
        .parent_id = parent_id,
        .visibility = .private,
        .doc = doc,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
    });
}

/// Check if a given name refers to an import_decl node that is a sibling
/// (same parent) of the current node. Used to detect same-name re-exports
/// like `const Graph = graph_mod.Graph` where "graph_mod" is an import sibling.
fn isImportSibling(g: *const Graph, parent_id: NodeId, name: []const u8) bool {
    for (g.nodes.items) |n| {
        if (n.kind == .import_decl and
            n.parent_id != null and n.parent_id.? == parent_id and
            std.mem.eql(u8, n.name, name))
        {
            return true;
        }
    }
    return false;
}

// =========================================================================
// Nominal tests: parse test/fixtures/simple.zig
// =========================================================================

const fixtures = @import("test-fixtures");

test "parses public function" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: at least one node with kind=function and visibility=public
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and n.visibility == .public) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "parses private function" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: at least one node with kind=function and visibility=private
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and n.visibility == .private) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "parses struct" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: at least one node with kind=type_def
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .type_def) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "parses enum" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: at least one node with kind=enum_def
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .enum_def) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "parses constant" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: at least one node with kind=constant
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .constant) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "parses test block" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: at least one node with kind=test_def
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .test_def) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "parses error set" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: at least one node with kind=error_def
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .error_def) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "attaches doc comment" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: the public function "defaultPoint" has doc != null
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and n.visibility == .public and
            std.mem.eql(u8, n.name, "defaultPoint"))
        {
            try std.testing.expect(n.doc != null);
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "sets parent_id for method" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: method "manhattan" has parent_id pointing to the struct "Point"
    var method_node: ?*const Node = null;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and std.mem.eql(u8, n.name, "manhattan")) {
            method_node = n;
            break;
        }
    }
    try std.testing.expect(method_node != null);
    try std.testing.expect(method_node.?.parent_id != null);

    const parent = g.getNode(method_node.?.parent_id.?);
    try std.testing.expect(parent != null);
    try std.testing.expectEqual(NodeKind.type_def, parent.?.kind);
    try std.testing.expectEqualStrings("Point", parent.?.name);
}

test "sets parent_id for struct" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: struct "Point" has parent_id pointing to the file node
    var struct_node: ?*const Node = null;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Point")) {
            struct_node = n;
            break;
        }
    }
    try std.testing.expect(struct_node != null);
    try std.testing.expect(struct_node.?.parent_id != null);

    const parent = g.getNode(struct_node.?.parent_id.?);
    try std.testing.expect(parent != null);
    try std.testing.expectEqual(NodeKind.file, parent.?.kind);
}

test "sets parent_id for top-level function" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: top-level function "defaultPoint" has parent_id pointing to file node
    var fn_node: ?*const Node = null;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and std.mem.eql(u8, n.name, "defaultPoint")) {
            fn_node = n;
            break;
        }
    }
    try std.testing.expect(fn_node != null);
    try std.testing.expect(fn_node.?.parent_id != null);

    const parent = g.getNode(fn_node.?.parent_id.?);
    try std.testing.expect(parent != null);
    try std.testing.expectEqual(NodeKind.file, parent.?.kind);
}

test "populates ZigMeta for comptime" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: constant "buffer_size" has lang_meta.zig.is_comptime == true
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .constant and std.mem.eql(u8, n.name, "buffer_size")) {
            switch (n.lang_meta) {
                .zig => |zm| {
                    try std.testing.expect(zm.is_comptime);
                    found = true;
                },
                .none => return error.ExpectedZigMeta,
            }
            break;
        }
    }
    try std.testing.expect(found);
}

test "all nodes have language zig" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: every node has language == .zig
    try std.testing.expect(g.nodeCount() > 0);
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        try std.testing.expectEqual(Language.zig, n.language);
    }
}

test "file node has correct line count" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: file node (first node, kind=file) has line_end == 75
    const file_node = g.getNode(@enumFromInt(0));
    try std.testing.expect(file_node != null);
    try std.testing.expectEqual(NodeKind.file, file_node.?.kind);
    try std.testing.expect(file_node.?.line_end != null);
    try std.testing.expectEqual(@as(u32, 75), file_node.?.line_end.?);
}

// =========================================================================
// Edge case tests
// =========================================================================

test "empty file produces only file node" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.empty, &g, Logger.noop);

    // Assert: exactly 1 node (kind=file), zero edges
    try std.testing.expectEqual(@as(usize, 1), g.nodeCount());
    try std.testing.expectEqual(@as(usize, 0), g.edgeCount());
    const file_node = g.getNode(@enumFromInt(0));
    try std.testing.expect(file_node != null);
    try std.testing.expectEqual(NodeKind.file, file_node.?.kind);
}

test "only comments produces only file node" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.only_comments, &g, Logger.noop);

    // Assert: exactly 1 node (kind=file), no declarations parsed
    try std.testing.expectEqual(@as(usize, 1), g.nodeCount());
    const file_node = g.getNode(@enumFromInt(0));
    try std.testing.expect(file_node != null);
    try std.testing.expectEqual(NodeKind.file, file_node.?.kind);
}

test "no pub file has all private nodes" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.no_pub, &g, Logger.noop);

    // Assert: all function nodes have visibility=private
    var fn_count: usize = 0;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function) {
            try std.testing.expectEqual(Visibility.private, n.visibility);
            fn_count += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 3), fn_count);
}

test "deeply nested sets correct parent chain" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.deeply_nested, &g, Logger.noop);

    // Assert: innerMethod → Inner → Middle → Outer → file
    var inner_method: ?*const Node = null;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and std.mem.eql(u8, n.name, "innerMethod")) {
            inner_method = n;
            break;
        }
    }
    try std.testing.expect(inner_method != null);

    try std.testing.expect(inner_method.?.parent_id != null);
    const inner_struct = g.getNode(inner_method.?.parent_id.?);
    try std.testing.expect(inner_struct != null);
    try std.testing.expectEqual(NodeKind.type_def, inner_struct.?.kind);
    try std.testing.expectEqualStrings("Inner", inner_struct.?.name);

    try std.testing.expect(inner_struct.?.parent_id != null);
    const middle_struct = g.getNode(inner_struct.?.parent_id.?);
    try std.testing.expect(middle_struct != null);
    try std.testing.expectEqual(NodeKind.type_def, middle_struct.?.kind);
    try std.testing.expectEqualStrings("Middle", middle_struct.?.name);

    try std.testing.expect(middle_struct.?.parent_id != null);
    const outer_struct = g.getNode(middle_struct.?.parent_id.?);
    try std.testing.expect(outer_struct != null);
    try std.testing.expectEqual(NodeKind.type_def, outer_struct.?.kind);
    try std.testing.expectEqualStrings("Outer", outer_struct.?.name);

    try std.testing.expect(outer_struct.?.parent_id != null);
    const file_node = g.getNode(outer_struct.?.parent_id.?);
    try std.testing.expect(file_node != null);
    try std.testing.expectEqual(NodeKind.file, file_node.?.kind);
}

test "many params function is parsed" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.many_params, &g, Logger.noop);

    // Assert: a function node named "manyParams" exists
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and std.mem.eql(u8, n.name, "manyParams")) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "unicode names are preserved" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.unicode_names, &g, Logger.noop);

    // Assert: a node named "café" or "résumé" exists
    var found_cafe = false;
    var found_resume = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "café")) found_cafe = true;
        if (std.mem.eql(u8, n.name, "résumé")) found_resume = true;
    }
    try std.testing.expect(found_cafe or found_resume);
}

// =========================================================================
// Generic type-returning function tests
// =========================================================================

test "generic type-returning function produces a type_def node" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g, Logger.noop);

    // Assert: a node named "Container" with kind=type_def exists
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Container")) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "generic type-returning function has file as parent" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g, Logger.noop);

    // Assert: Container's parent_id points to the file node
    var container_node: ?*const Node = null;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Container")) {
            container_node = n;
            break;
        }
    }
    try std.testing.expect(container_node != null);
    try std.testing.expect(container_node.?.parent_id != null);

    const parent = g.getNode(container_node.?.parent_id.?);
    try std.testing.expect(parent != null);
    try std.testing.expectEqual(NodeKind.file, parent.?.kind);
}

test "generic type-returning function exposes inner public methods" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g, Logger.noop);

    // Assert: public methods init, deinit, count, isEmpty exist as function nodes
    var found_init = false;
    var found_deinit = false;
    var found_count = false;
    var found_isEmpty = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and n.visibility == .public) {
            if (std.mem.eql(u8, n.name, "init")) found_init = true;
            if (std.mem.eql(u8, n.name, "deinit")) found_deinit = true;
            if (std.mem.eql(u8, n.name, "count")) found_count = true;
            if (std.mem.eql(u8, n.name, "isEmpty")) found_isEmpty = true;
        }
    }
    try std.testing.expect(found_init);
    try std.testing.expect(found_deinit);
    try std.testing.expect(found_count);
    try std.testing.expect(found_isEmpty);
}

test "generic type-returning function exposes inner private helper" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g, Logger.noop);

    // Assert: private function "validate" exists with visibility=private
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and n.visibility == .private and
            std.mem.eql(u8, n.name, "validate"))
        {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "generic type-returning function inner method has correct parent" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g, Logger.noop);

    // Assert: method "init" has parent_id pointing to Container (type_def)
    var init_node: ?*const Node = null;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and std.mem.eql(u8, n.name, "init")) {
            init_node = n;
            break;
        }
    }
    try std.testing.expect(init_node != null);
    try std.testing.expect(init_node.?.parent_id != null);

    const parent = g.getNode(init_node.?.parent_id.?);
    try std.testing.expect(parent != null);
    try std.testing.expectEqual(NodeKind.type_def, parent.?.kind);
    try std.testing.expectEqualStrings("Container", parent.?.name);
}

test "generic type-returning function inner type has correct parent" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g, Logger.noop);

    // Assert: inner type "Entry" has parent_id pointing to Container (type_def)
    var entry_node: ?*const Node = null;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Entry")) {
            entry_node = n;
            break;
        }
    }
    try std.testing.expect(entry_node != null);
    try std.testing.expect(entry_node.?.parent_id != null);

    const parent = g.getNode(entry_node.?.parent_id.?);
    try std.testing.expect(parent != null);
    try std.testing.expectEqual(NodeKind.type_def, parent.?.kind);
    try std.testing.expectEqualStrings("Container", parent.?.name);
}

test "generic type-returning function doc comment on inner method is captured" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g, Logger.noop);

    // Assert: public method "count" inside Container has doc != null
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and std.mem.eql(u8, n.name, "count")) {
            try std.testing.expect(n.doc != null);
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "generic union type-returning function produces nodes" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g, Logger.noop);

    // Assert: a node named "Result" exists and a method "isOk" exists inside it
    var found_result = false;
    var found_isOk = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "Result") and
            (n.kind == .type_def or n.kind == .enum_def))
        {
            found_result = true;
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "isOk")) {
            found_isOk = true;
        }
    }
    try std.testing.expect(found_result);
    try std.testing.expect(found_isOk);
}

test "non-generic struct still works alongside generic type" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g, Logger.noop);

    // Assert: Config exists as type_def with file as parent, defaults method present
    var config_node: ?*const Node = null;
    var found_defaults = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Config")) {
            config_node = n;
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "defaults")) {
            found_defaults = true;
        }
    }
    try std.testing.expect(config_node != null);
    try std.testing.expect(found_defaults);

    try std.testing.expect(config_node.?.parent_id != null);
    const parent = g.getNode(config_node.?.parent_id.?);
    try std.testing.expect(parent != null);
    try std.testing.expectEqual(NodeKind.file, parent.?.kind);
}

test "generic type-returning function doc comment on outer function is captured" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g, Logger.noop);

    // Assert: Container node has doc != null
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Container")) {
            try std.testing.expect(n.doc != null);
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

// =========================================================================
// Union recognition tests
// =========================================================================

test "tagged union is classified as type_def" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: Shape node exists with kind=type_def, visibility=public, doc != null
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "Shape")) {
            try std.testing.expectEqual(NodeKind.type_def, n.kind);
            try std.testing.expectEqual(Visibility.public, n.visibility);
            try std.testing.expect(n.doc != null);
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "plain union is classified as type_def" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: RawValue node exists with kind=type_def, visibility=private
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "RawValue")) {
            try std.testing.expectEqual(NodeKind.type_def, n.kind);
            try std.testing.expectEqual(Visibility.private, n.visibility);
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "tagged union with nested struct is still type_def" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: Shape is classified as type_def
    var shape_node: ?*const Node = null;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "Shape")) {
            shape_node = n;
            break;
        }
    }
    try std.testing.expect(shape_node != null);
    try std.testing.expectEqual(NodeKind.type_def, shape_node.?.kind);

    try std.testing.expect(shape_node.?.parent_id != null);
    const parent = g.getNode(shape_node.?.parent_id.?);
    try std.testing.expect(parent != null);
    try std.testing.expectEqual(NodeKind.file, parent.?.kind);
}

// =========================================================================
// Enum method extraction tests
// =========================================================================

test "enum method is extracted as function child" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: Color exists as enum_def, isWarm exists as function with parent == Color
    var color_id: ?NodeId = null;
    var iswarm_node: ?*const Node = null;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .enum_def and std.mem.eql(u8, n.name, "Color")) {
            color_id = n.id;
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "isWarm")) {
            iswarm_node = n;
        }
    }
    try std.testing.expect(color_id != null);
    try std.testing.expect(iswarm_node != null);
    try std.testing.expectEqual(Visibility.public, iswarm_node.?.visibility);
    try std.testing.expect(iswarm_node.?.parent_id != null);
    try std.testing.expectEqual(color_id.?, iswarm_node.?.parent_id.?);
}

// =========================================================================
// @import detection tests
// =========================================================================

test "import declaration is classified as import_decl" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: node named "std" has kind=import_decl and visibility=private
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "std")) {
            try std.testing.expectEqual(NodeKind.import_decl, n.kind);
            try std.testing.expectEqual(Visibility.private, n.visibility);
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "import with field access is classified as import_decl" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: node named "ZigMeta" has kind=import_decl
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "ZigMeta")) {
            try std.testing.expectEqual(NodeKind.import_decl, n.kind);
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "non-import constant is still constant" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: node named "max_iterations" has kind=constant
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "max_iterations")) {
            try std.testing.expectEqual(NodeKind.constant, n.kind);
            try std.testing.expectEqual(Visibility.private, n.visibility);
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

// =========================================================================
// @This() file-struct node tests
// =========================================================================

test "file-struct methods are extracted as function nodes" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.file_struct, &g, Logger.noop);

    // Assert: init, getValue, validate, isValid exist as function nodes
    var found_init = false;
    var found_getValue = false;
    var found_validate = false;
    var found_isValid = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind != .function) continue;

        if (n.parent_id) |pid| {
            const parent = g.getNode(pid) orelse continue;
            if (parent.kind != .file) continue;
        } else continue;

        if (std.mem.eql(u8, n.name, "init")) found_init = true;
        if (std.mem.eql(u8, n.name, "getValue")) found_getValue = true;
        if (std.mem.eql(u8, n.name, "validate")) found_validate = true;
        if (std.mem.eql(u8, n.name, "isValid")) found_isValid = true;
    }
    try std.testing.expect(found_init);
    try std.testing.expect(found_getValue);
    try std.testing.expect(found_validate);
    try std.testing.expect(found_isValid);
}

test "file-struct nested type is extracted" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.file_struct, &g, Logger.noop);

    // Assert: Config struct is detected as type_def with parent = file node
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Config")) {
            try std.testing.expect(n.parent_id != null);
            const parent = g.getNode(n.parent_id.?);
            try std.testing.expect(parent != null);
            try std.testing.expectEqual(NodeKind.file, parent.?.kind);
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "file-struct Self constant is filtered out" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.file_struct, &g, Logger.noop);

    // Assert: const Self = @This() is filtered at parse time — no Self node exists
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "Self")) {
            found = true;
            break;
        }
    }
    try std.testing.expect(!found);
}

// =========================================================================
// Decl-reference test names
// =========================================================================

test "decl-reference test name is captured as identifier" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\fn helper() i32 { return 42; }
        \\test helper { _ = helper(); }
    ;

    // Act
    try parse(source, &g, Logger.noop);

    // Assert: the test_def node's name should be "helper", not "test"
    var found_test = false;
    for (g.nodes.items) |n| {
        if (n.kind == .test_def) {
            try std.testing.expectEqualStrings("helper", n.name);
            found_test = true;
            break;
        }
    }
    try std.testing.expect(found_test);
}

test "decl-reference test name for type is captured" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const Foo = struct {
        \\    pub fn bar() void {}
        \\};
        \\test Foo { Foo.bar(); }
    ;

    // Act
    try parse(source, &g, Logger.noop);

    // Assert: the test_def node's name should be "Foo", not "test"
    var found_test = false;
    for (g.nodes.items) |n| {
        if (n.kind == .test_def) {
            try std.testing.expectEqualStrings("Foo", n.name);
            found_test = true;
            break;
        }
    }
    try std.testing.expect(found_test);
}

test "two decl-reference tests get distinct names" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\fn alpha() i32 { return 1; }
        \\fn beta() i32 { return 2; }
        \\test alpha { _ = alpha(); }
        \\test beta { _ = beta(); }
    ;

    // Act
    try parse(source, &g, Logger.noop);

    // Assert: exactly 2 test_def nodes, named "alpha" and "beta"
    var test_count: usize = 0;
    var found_alpha = false;
    var found_beta = false;
    for (g.nodes.items) |n| {
        if (n.kind == .test_def) {
            test_count += 1;
            if (std.mem.eql(u8, n.name, "alpha")) found_alpha = true;
            if (std.mem.eql(u8, n.name, "beta")) found_beta = true;
            try std.testing.expect(!std.mem.eql(u8, n.name, "test"));
        }
    }
    try std.testing.expectEqual(@as(usize, 2), test_count);
    try std.testing.expect(found_alpha);
    try std.testing.expect(found_beta);
}

test "quoted identifier test name is stripped" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\test @"edge case name" {}
    ;

    // Act
    try parse(source, &g, Logger.noop);

    // Assert: the test_def node's name should be "edge case name"
    var found_test = false;
    for (g.nodes.items) |n| {
        if (n.kind == .test_def) {
            try std.testing.expectEqualStrings("edge case name", n.name);
            found_test = true;
            break;
        }
    }
    try std.testing.expect(found_test);
}

test "string-literal test names still work" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\fn foo() void {}
        \\test "calls foo" { _ = foo(); }
    ;

    // Act
    try parse(source, &g, Logger.noop);

    // Assert: the test_def node's name should be "calls foo"
    var test_id: ?NodeId = null;
    var foo_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .test_def) {
            try std.testing.expectEqualStrings("calls foo", n.name);
            test_id = @enumFromInt(idx);
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "foo")) foo_id = @enumFromInt(idx);
    }
    try std.testing.expect(test_id != null);
    try std.testing.expect(foo_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == test_id.? and e.target_id == foo_id.? and e.edge_type == .calls) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

// =========================================================================
// Struct/enum field extraction
// =========================================================================

test "struct fields are emitted as field nodes" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: Point struct has two field children (x and y)
    var point_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Point")) {
            point_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(point_id != null);

    var field_count: usize = 0;
    for (g.nodes.items) |n| {
        if (n.kind == .field and n.parent_id != null and n.parent_id.? == point_id.?) {
            field_count += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 2), field_count);
}

test "field node has correct name" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: Point's field nodes are named "x" and "y"
    var point_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Point")) {
            point_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(point_id != null);

    var found_x = false;
    var found_y = false;
    for (g.nodes.items) |n| {
        if (n.kind == .field and n.parent_id != null and n.parent_id.? == point_id.?) {
            if (std.mem.eql(u8, n.name, "x")) found_x = true;
            if (std.mem.eql(u8, n.name, "y")) found_y = true;
        }
    }
    try std.testing.expect(found_x);
    try std.testing.expect(found_y);
}

test "field node visibility is private by default" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: Point's x and y fields have visibility == .private
    var point_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Point")) {
            point_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(point_id != null);

    var checked: usize = 0;
    for (g.nodes.items) |n| {
        if (n.kind == .field and n.parent_id != null and n.parent_id.? == point_id.?) {
            try std.testing.expectEqual(Visibility.private, n.visibility);
            checked += 1;
        }
    }
    try std.testing.expect(checked >= 2);
}

test "enum variants are emitted as field nodes" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: Direction enum has three field children (north, south, east)
    var dir_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .enum_def and std.mem.eql(u8, n.name, "Direction")) {
            dir_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(dir_id != null);

    var found_north = false;
    var found_south = false;
    var found_east = false;
    for (g.nodes.items) |n| {
        if (n.kind == .field and n.parent_id != null and n.parent_id.? == dir_id.?) {
            if (std.mem.eql(u8, n.name, "north")) found_north = true;
            if (std.mem.eql(u8, n.name, "south")) found_south = true;
            if (std.mem.eql(u8, n.name, "east")) found_east = true;
        }
    }
    try std.testing.expect(found_north);
    try std.testing.expect(found_south);
    try std.testing.expect(found_east);
}

// =========================================================================
// Module doc comment tests (//!)
// =========================================================================

test "file node captures module doc comment" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: file node (node 0) has doc != null containing module doc text
    const file_node = g.getNode(@enumFromInt(0));
    try std.testing.expect(file_node != null);
    try std.testing.expectEqual(NodeKind.file, file_node.?.kind);
    try std.testing.expect(file_node.?.doc != null);

    const doc = file_node.?.doc.?;
    try std.testing.expect(std.mem.indexOf(u8, doc, "Simple fixture") != null);
    try std.testing.expect(std.mem.indexOf(u8, doc, "Compute the Manhattan") == null);
}

test "file node doc is null when no module doc comment" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source = "const x = 42;\n";

    // Act
    try parse(source, &g, Logger.noop);

    // Assert: file node has doc == null
    const file_node = g.getNode(@enumFromInt(0));
    try std.testing.expect(file_node != null);
    try std.testing.expectEqual(NodeKind.file, file_node.?.kind);
    try std.testing.expectEqual(@as(?[]const u8, null), file_node.?.doc);
}

test "module doc is separate from item doc" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\//! Module doc.
        \\/// Item doc.
        \\const x = 42;
    ;

    // Act
    try parse(source, &g, Logger.noop);

    // Assert: file node doc contains "Module doc." but not "Item doc."
    const file_node = g.getNode(@enumFromInt(0));
    try std.testing.expect(file_node != null);
    try std.testing.expect(file_node.?.doc != null);
    const file_doc = file_node.?.doc.?;
    try std.testing.expect(std.mem.indexOf(u8, file_doc, "Module doc.") != null);
    try std.testing.expect(std.mem.indexOf(u8, file_doc, "Item doc.") == null);

    // Assert: x node doc contains "Item doc." but not "Module doc."
    var x_node: ?*const Node = null;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "x")) {
            x_node = n;
            break;
        }
    }
    try std.testing.expect(x_node != null);
    try std.testing.expect(x_node.?.doc != null);
    const item_doc = x_node.?.doc.?;
    try std.testing.expect(std.mem.indexOf(u8, item_doc, "Item doc.") != null);
    try std.testing.expect(std.mem.indexOf(u8, item_doc, "Module doc.") == null);
}
