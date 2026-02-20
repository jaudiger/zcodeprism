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
const pc = @import("parse_context.zig");

const KindIds = pc.KindIds;
const ScopeIndex = pc.ScopeIndex;
const FileIndex = pc.FileIndex;

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
    const ts_lang = ts_api.zigLanguage();
    const k = KindIds.init(ts_lang);

    // Parse source with tree-sitter first so we can collect module doc comments.
    const tree = ts_api.parseSource(ts_lang, source) orelse {
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
    const module_doc = ast.collectModuleDocComment(source, root, &k);

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
        try processDeclaration(g, source, child, file_id, &k, log);
    }

    // Build edge resolution context: scope range + import map.
    var ctx = cf.EdgeContext{
        .scope_start = @intFromEnum(file_id),
        .scope_end = g.nodeCount(),
    };

    // Build lookup indices for scope-based and file-based resolution.
    var file_index = try FileIndex.build(g.allocator, g.nodes.items);
    defer file_index.deinit(g.allocator);

    var scope_index = try ScopeIndex.build(g.allocator, g.nodes.items, 0);
    defer scope_index.deinit(g.allocator);

    cf.buildImportMap(g, source, root, &ctx, &file_index, &k, log);

    // Create edges (calls, uses_type) with cross-file resolution.
    log.debug("building edges", &.{});
    try eb.walkForEdges(g, source, root, &ctx, &k, &scope_index, &file_index, log);
}

// =========================================================================
// Node creation
// =========================================================================

fn processDeclaration(g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
    const kid = ts_node.kindId();

    if (kid == k.variable_declaration) {
        try processVariableDecl(g, source, ts_node, parent_id, k, log);
    } else if (kid == k.function_declaration) {
        try processFunctionDecl(g, source, ts_node, parent_id, k, log);
    } else if (kid == k.test_declaration) {
        try processTestDecl(g, source, ts_node, parent_id, k);
    } else if (kid == k.container_field) {
        try processContainerField(g, source, ts_node, parent_id, k, log);
    }
}

fn processVariableDecl(g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
    const name = ast.getIdentifierName(source, ts_node, k) orelse {
        log.trace("skipping variable: no identifier", &.{});
        return;
    };
    const visibility = ast.detectVisibility(ts_node, k);
    const doc = ast.collectDocComment(source, ts_node, k);

    // Classify the value: struct, enum, error set, import, or plain constant.
    const classification = ast.classifyVariableValue(source, ts_node, k);
    const kind = classification.kind;

    // Filter noise constants that produce no useful graph information.
    if (kind == .constant) {
        // Skip @This() aliases (e.g., `const Self = @This()`).
        if (ast.isThisBuiltin(source, ts_node, k)) {
            log.trace("skipping @This() alias", &.{Field.string("name", name)});
            return;
        }

        // Skip same-name re-exports from imports (e.g., `const Graph = graph_mod.Graph`).
        if (ast.getFieldExprRootAndLeaf(source, ts_node, k)) |info| {
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
    if (kind == .constant and ast.hasTypeAnnotation(ts_node, k)) {
        lang_meta = .{ .zig = .{ .is_comptime = true } };
    }

    // For import declarations, store the import path extracted from the AST.
    const sig: ?[]const u8 = if (kind == .import_decl) cf.extractImportPath(source, ts_node, k) else null;

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
            try processDeclaration(g, source, child, node_id, k, log);
        }
    }
}

fn processFunctionDecl(g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
    const name = ast.getIdentifierName(source, ts_node, k) orelse {
        log.trace("skipping function: no identifier", &.{});
        return;
    };
    const visibility = ast.detectVisibility(ts_node, k);
    const doc = ast.collectDocComment(source, ts_node, k);

    // Detect type-returning generic functions: `fn Foo(comptime T: type) type { return struct { ... }; }`
    if (ast.returnsType(source, ts_node, k)) {
        if (ast.findReturnedTypeBody(ts_node, k)) |body_info| {
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
                try processDeclaration(g, source, child, type_id, k, log);
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
        .signature = extractFunctionSignature(source, ts_node, k),
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
    });
}

/// Extract the function header text (from "fn" keyword to the body block start).
/// For `pub fn create() svc_mod.Service { ... }`, returns `pub fn create() svc_mod.Service`.
fn extractFunctionSignature(source: []const u8, ts_node: ts.Node, k: *const KindIds) ?[]const u8 {
    const start = ts_node.startByte();
    var i: u32 = 0;
    while (i < ts_node.childCount()) : (i += 1) {
        const child = ts_node.child(i) orelse continue;
        if (child.kindId() == k.block) {
            const end = child.startByte();
            if (end > start and end <= source.len) {
                return std.mem.trimRight(u8, source[start..end], " \t\n\r");
            }
            return null;
        }
    }
    return null;
}

fn processTestDecl(g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds) anyerror!void {
    const name = ast.getTestName(source, ts_node, k);
    const doc = ast.collectDocComment(source, ts_node, k);

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

fn processContainerField(g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
    const name = ast.getIdentifierName(source, ts_node, k) orelse {
        log.trace("skipping field: no identifier", &.{});
        return;
    };
    const doc = ast.collectDocComment(source, ts_node, k);

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
// Tests
// =========================================================================

const fixtures = @import("test-fixtures");

// ---------------------------------------------------------------------------
// simple.zig fixture: nodes, visibility, parents, doc comments
// ---------------------------------------------------------------------------

test "simple fixture: nodes, visibility, parents, doc comments" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: at least one node of each kind exists
    var found_pub_fn = false;
    var found_priv_fn = false;
    var found_type_def = false;
    var found_enum_def = false;
    var found_constant = false;
    var found_test_def = false;
    var found_error_def = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and n.visibility == .public) found_pub_fn = true;
        if (n.kind == .function and n.visibility == .private) found_priv_fn = true;
        if (n.kind == .type_def) found_type_def = true;
        if (n.kind == .enum_def) found_enum_def = true;
        if (n.kind == .constant) found_constant = true;
        if (n.kind == .test_def) found_test_def = true;
        if (n.kind == .error_def) found_error_def = true;
    }
    try std.testing.expect(found_pub_fn);
    try std.testing.expect(found_priv_fn);
    try std.testing.expect(found_type_def);
    try std.testing.expect(found_enum_def);
    try std.testing.expect(found_constant);
    try std.testing.expect(found_test_def);
    try std.testing.expect(found_error_def);

    // Assert: doc comment attached to defaultPoint
    var found_doc = false;
    i = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and n.visibility == .public and
            std.mem.eql(u8, n.name, "defaultPoint"))
        {
            try std.testing.expect(n.doc != null);
            found_doc = true;
            break;
        }
    }
    try std.testing.expect(found_doc);

    // Assert: parent_id for method manhattan → struct Point
    var method_node: ?*const Node = null;
    i = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and std.mem.eql(u8, n.name, "manhattan")) {
            method_node = n;
            break;
        }
    }
    try std.testing.expect(method_node != null);
    try std.testing.expect(method_node.?.parent_id != null);
    const method_parent = g.getNode(method_node.?.parent_id.?);
    try std.testing.expect(method_parent != null);
    try std.testing.expectEqual(NodeKind.type_def, method_parent.?.kind);
    try std.testing.expectEqualStrings("Point", method_parent.?.name);

    // Assert: parent_id for struct Point → file node
    var struct_node: ?*const Node = null;
    i = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Point")) {
            struct_node = n;
            break;
        }
    }
    try std.testing.expect(struct_node != null);
    try std.testing.expect(struct_node.?.parent_id != null);
    const struct_parent = g.getNode(struct_node.?.parent_id.?);
    try std.testing.expect(struct_parent != null);
    try std.testing.expectEqual(NodeKind.file, struct_parent.?.kind);

    // Assert: top-level function defaultPoint → file node
    var fn_node: ?*const Node = null;
    i = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and std.mem.eql(u8, n.name, "defaultPoint")) {
            fn_node = n;
            break;
        }
    }
    try std.testing.expect(fn_node != null);
    try std.testing.expect(fn_node.?.parent_id != null);
    const fn_parent = g.getNode(fn_node.?.parent_id.?);
    try std.testing.expect(fn_parent != null);
    try std.testing.expectEqual(NodeKind.file, fn_parent.?.kind);

    // Assert: comptime constant has ZigMeta.is_comptime
    var found_comptime = false;
    i = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .constant and std.mem.eql(u8, n.name, "buffer_size")) {
            switch (n.lang_meta) {
                .zig => |zm| {
                    try std.testing.expect(zm.is_comptime);
                    found_comptime = true;
                },
                .none => return error.ExpectedZigMeta,
            }
            break;
        }
    }
    try std.testing.expect(found_comptime);

    // Assert: all nodes have language == .zig
    try std.testing.expect(g.nodeCount() > 0);
    i = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        try std.testing.expectEqual(Language.zig, n.language);
    }

    // Assert: file node has correct line count
    const file_node = g.getNode(@enumFromInt(0));
    try std.testing.expect(file_node != null);
    try std.testing.expectEqual(NodeKind.file, file_node.?.kind);
    try std.testing.expect(file_node.?.line_end != null);
    try std.testing.expectEqual(@as(u32, 75), file_node.?.line_end.?);
}

// ---------------------------------------------------------------------------
// simple.zig fixture: unions, enum methods, imports, fields, variants, module doc
// ---------------------------------------------------------------------------

test "simple fixture: unions, enum methods, imports, fields, variants, module doc" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // --- unions ---
    // tagged union Shape: type_def, public, doc
    var found_shape = false;
    // plain union RawValue: type_def, private
    var found_raw = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "Shape")) {
            try std.testing.expectEqual(NodeKind.type_def, n.kind);
            try std.testing.expectEqual(Visibility.public, n.visibility);
            try std.testing.expect(n.doc != null);
            try std.testing.expect(n.parent_id != null);
            const parent = g.getNode(n.parent_id.?) orelse continue;
            try std.testing.expectEqual(NodeKind.file, parent.kind);
            found_shape = true;
        }
        if (std.mem.eql(u8, n.name, "RawValue")) {
            try std.testing.expectEqual(NodeKind.type_def, n.kind);
            try std.testing.expectEqual(Visibility.private, n.visibility);
            found_raw = true;
        }
    }
    try std.testing.expect(found_shape);
    try std.testing.expect(found_raw);

    // --- enum method isWarm is function child of Color ---
    var color_id: ?NodeId = null;
    var iswarm_node: ?*const Node = null;
    i = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .enum_def and std.mem.eql(u8, n.name, "Color")) color_id = n.id;
        if (n.kind == .function and std.mem.eql(u8, n.name, "isWarm")) iswarm_node = n;
    }
    try std.testing.expect(color_id != null);
    try std.testing.expect(iswarm_node != null);
    try std.testing.expectEqual(Visibility.public, iswarm_node.?.visibility);
    try std.testing.expect(iswarm_node.?.parent_id != null);
    try std.testing.expectEqual(color_id.?, iswarm_node.?.parent_id.?);

    // --- imports ---
    // std is import_decl, private
    var found_std = false;
    // ZigMeta is import_decl (field access)
    var found_zigmeta = false;
    // max_iterations is constant, not import
    var found_max = false;
    i = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "std")) {
            try std.testing.expectEqual(NodeKind.import_decl, n.kind);
            try std.testing.expectEqual(Visibility.private, n.visibility);
            found_std = true;
        }
        if (std.mem.eql(u8, n.name, "ZigMeta")) {
            try std.testing.expectEqual(NodeKind.import_decl, n.kind);
            found_zigmeta = true;
        }
        if (std.mem.eql(u8, n.name, "max_iterations")) {
            try std.testing.expectEqual(NodeKind.constant, n.kind);
            try std.testing.expectEqual(Visibility.private, n.visibility);
            found_max = true;
        }
    }
    try std.testing.expect(found_std);
    try std.testing.expect(found_zigmeta);
    try std.testing.expect(found_max);

    // --- struct fields ---
    var point_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Point")) {
            point_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(point_id != null);

    var field_count: usize = 0;
    var found_x = false;
    var found_y = false;
    for (g.nodes.items) |n| {
        if (n.kind == .field and n.parent_id != null and n.parent_id.? == point_id.?) {
            field_count += 1;
            try std.testing.expectEqual(Visibility.private, n.visibility);
            if (std.mem.eql(u8, n.name, "x")) found_x = true;
            if (std.mem.eql(u8, n.name, "y")) found_y = true;
        }
    }
    try std.testing.expectEqual(@as(usize, 2), field_count);
    try std.testing.expect(found_x);
    try std.testing.expect(found_y);

    // --- enum variants ---
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

    // --- module doc comment ---
    const file_node2 = g.getNode(@enumFromInt(0));
    try std.testing.expect(file_node2 != null);
    try std.testing.expectEqual(NodeKind.file, file_node2.?.kind);
    try std.testing.expect(file_node2.?.doc != null);
    const doc = file_node2.?.doc.?;
    try std.testing.expect(std.mem.indexOf(u8, doc, "Simple fixture") != null);
    try std.testing.expect(std.mem.indexOf(u8, doc, "Compute the Manhattan") == null);
}

// ---------------------------------------------------------------------------
// generic_type.zig fixture: type_def nodes, parents, visibility, doc comments
// ---------------------------------------------------------------------------

test "generic type fixture: type_def nodes, parents, visibility, doc comments" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g, Logger.noop);

    // --- Container exists as type_def with file as parent ---
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
    try std.testing.expect(container_node.?.doc != null);
    const container_parent = g.getNode(container_node.?.parent_id.?);
    try std.testing.expect(container_parent != null);
    try std.testing.expectEqual(NodeKind.file, container_parent.?.kind);

    // --- public methods init, deinit, count, isEmpty exist ---
    var found_init = false;
    var found_deinit = false;
    var found_count = false;
    var found_isEmpty = false;
    i = 0;
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

    // --- private helper validate exists ---
    var found_validate = false;
    i = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and n.visibility == .private and
            std.mem.eql(u8, n.name, "validate"))
        {
            found_validate = true;
            break;
        }
    }
    try std.testing.expect(found_validate);

    // --- init has parent Container ---
    var init_node: ?*const Node = null;
    i = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and std.mem.eql(u8, n.name, "init")) {
            init_node = n;
            break;
        }
    }
    try std.testing.expect(init_node != null);
    try std.testing.expect(init_node.?.parent_id != null);
    const init_parent = g.getNode(init_node.?.parent_id.?);
    try std.testing.expect(init_parent != null);
    try std.testing.expectEqual(NodeKind.type_def, init_parent.?.kind);
    try std.testing.expectEqualStrings("Container", init_parent.?.name);

    // --- Entry has parent Container ---
    var entry_node: ?*const Node = null;
    i = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Entry")) {
            entry_node = n;
            break;
        }
    }
    try std.testing.expect(entry_node != null);
    try std.testing.expect(entry_node.?.parent_id != null);
    const entry_parent = g.getNode(entry_node.?.parent_id.?);
    try std.testing.expect(entry_parent != null);
    try std.testing.expectEqualStrings("Container", entry_parent.?.name);

    // --- count method has doc ---
    var found_count_doc = false;
    i = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and std.mem.eql(u8, n.name, "count")) {
            try std.testing.expect(n.doc != null);
            found_count_doc = true;
            break;
        }
    }
    try std.testing.expect(found_count_doc);

    // --- Result union type and isOk method exist ---
    var found_result = false;
    var found_isOk = false;
    i = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "Result") and
            (n.kind == .type_def or n.kind == .enum_def)) found_result = true;
        if (n.kind == .function and std.mem.eql(u8, n.name, "isOk")) found_isOk = true;
    }
    try std.testing.expect(found_result);
    try std.testing.expect(found_isOk);

    // --- non-generic Config struct with defaults method ---
    var config_node: ?*const Node = null;
    var found_defaults = false;
    i = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Config")) config_node = n;
        if (n.kind == .function and std.mem.eql(u8, n.name, "defaults")) found_defaults = true;
    }
    try std.testing.expect(config_node != null);
    try std.testing.expect(found_defaults);
    try std.testing.expect(config_node.?.parent_id != null);
    const config_parent = g.getNode(config_node.?.parent_id.?);
    try std.testing.expect(config_parent != null);
    try std.testing.expectEqual(NodeKind.file, config_parent.?.kind);
}

// ---------------------------------------------------------------------------
// file_struct.zig fixture: methods, nested types, Self filtering
// ---------------------------------------------------------------------------

test "file struct fixture: methods, nested types, Self filtering" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.file_struct, &g, Logger.noop);

    // --- methods are direct children of file node ---
    var found_init = false;
    var found_getValue = false;
    var found_validate = false;
    var found_isValid = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind != .function) continue;
        const pid = n.parent_id orelse continue;
        const parent = g.getNode(pid) orelse continue;
        if (parent.kind != .file) continue;
        if (std.mem.eql(u8, n.name, "init")) found_init = true;
        if (std.mem.eql(u8, n.name, "getValue")) found_getValue = true;
        if (std.mem.eql(u8, n.name, "validate")) found_validate = true;
        if (std.mem.eql(u8, n.name, "isValid")) found_isValid = true;
    }
    try std.testing.expect(found_init);
    try std.testing.expect(found_getValue);
    try std.testing.expect(found_validate);
    try std.testing.expect(found_isValid);

    // --- Config nested type with file as parent ---
    var found_config = false;
    i = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Config")) {
            try std.testing.expect(n.parent_id != null);
            const parent = g.getNode(n.parent_id.?) orelse continue;
            try std.testing.expectEqual(NodeKind.file, parent.kind);
            found_config = true;
            break;
        }
    }
    try std.testing.expect(found_config);

    // --- Self constant is filtered ---
    var found_self = false;
    i = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "Self")) {
            found_self = true;
            break;
        }
    }
    try std.testing.expect(!found_self);
}

// ---------------------------------------------------------------------------
// Edge case fixtures: empty, comments-only, no-pub, deep nesting, many params, unicode
// ---------------------------------------------------------------------------

test "edge case fixtures: empty, comments-only, no-pub, deep nesting, many params, unicode" {
    // --- empty file ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        try parse(fixtures.zig.edge_cases.empty, &g, Logger.noop);

        try std.testing.expectEqual(@as(usize, 1), g.nodeCount());
        try std.testing.expectEqual(@as(usize, 0), g.edgeCount());
        const f = g.getNode(@enumFromInt(0));
        try std.testing.expect(f != null);
        try std.testing.expectEqual(NodeKind.file, f.?.kind);
    }

    // --- only comments ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        try parse(fixtures.zig.edge_cases.only_comments, &g, Logger.noop);

        try std.testing.expectEqual(@as(usize, 1), g.nodeCount());
        const f = g.getNode(@enumFromInt(0));
        try std.testing.expect(f != null);
        try std.testing.expectEqual(NodeKind.file, f.?.kind);
    }

    // --- no pub: all private ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        try parse(fixtures.zig.edge_cases.no_pub, &g, Logger.noop);

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

    // --- deeply nested: innerMethod → Inner → Middle → Outer → file ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        try parse(fixtures.zig.edge_cases.deeply_nested, &g, Logger.noop);

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

        const inner_struct = g.getNode(inner_method.?.parent_id.?);
        try std.testing.expect(inner_struct != null);
        try std.testing.expectEqualStrings("Inner", inner_struct.?.name);

        const middle_struct = g.getNode(inner_struct.?.parent_id.?);
        try std.testing.expect(middle_struct != null);
        try std.testing.expectEqualStrings("Middle", middle_struct.?.name);

        const outer_struct = g.getNode(middle_struct.?.parent_id.?);
        try std.testing.expect(outer_struct != null);
        try std.testing.expectEqualStrings("Outer", outer_struct.?.name);

        const file_node = g.getNode(outer_struct.?.parent_id.?);
        try std.testing.expect(file_node != null);
        try std.testing.expectEqual(NodeKind.file, file_node.?.kind);
    }

    // --- many params ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        try parse(fixtures.zig.edge_cases.many_params, &g, Logger.noop);

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

    // --- unicode names ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        try parse(fixtures.zig.edge_cases.unicode_names, &g, Logger.noop);

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
}

// ---------------------------------------------------------------------------
// Decl-reference test names
// ---------------------------------------------------------------------------

test "decl-reference and string-literal test names" {
    // --- bare identifier test name ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        const source =
            \\fn helper() i32 { return 42; }
            \\test helper { _ = helper(); }
        ;
        try parse(source, &g, Logger.noop);
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

    // --- type identifier test name ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        const source =
            \\const Foo = struct {
            \\    pub fn bar() void {}
            \\};
            \\test Foo { Foo.bar(); }
        ;
        try parse(source, &g, Logger.noop);
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

    // --- two decl-reference tests get distinct names ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        const source =
            \\fn alpha() i32 { return 1; }
            \\fn beta() i32 { return 2; }
            \\test alpha { _ = alpha(); }
            \\test beta { _ = beta(); }
        ;
        try parse(source, &g, Logger.noop);
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

    // --- @"quoted identifier" test name ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        const source =
            \\test @"edge case name" {}
        ;
        try parse(source, &g, Logger.noop);
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

    // --- string-literal test name ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        const source =
            \\fn foo() void {}
            \\test "calls foo" { _ = foo(); }
        ;
        try parse(source, &g, Logger.noop);
        for (g.nodes.items) |n| {
            if (n.kind == .test_def) {
                try std.testing.expectEqualStrings("calls foo", n.name);
                break;
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Module doc comment separation
// ---------------------------------------------------------------------------

test "module doc comment separation" {
    // --- no module doc → null ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        try parse("const x = 42;\n", &g, Logger.noop);

        const f = g.getNode(@enumFromInt(0));
        try std.testing.expect(f != null);
        try std.testing.expectEqual(NodeKind.file, f.?.kind);
        try std.testing.expectEqual(@as(?[]const u8, null), f.?.doc);
    }

    // --- module doc separate from item doc ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        const source =
            \\//! Module doc.
            \\/// Item doc.
            \\const x = 42;
        ;
        try parse(source, &g, Logger.noop);

        const f = g.getNode(@enumFromInt(0));
        try std.testing.expect(f != null);
        try std.testing.expect(f.?.doc != null);
        const file_doc = f.?.doc.?;
        try std.testing.expect(std.mem.indexOf(u8, file_doc, "Module doc.") != null);
        try std.testing.expect(std.mem.indexOf(u8, file_doc, "Item doc.") == null);

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
}
