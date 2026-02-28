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
const fixtures = @import("test-fixtures");

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
///
/// `source` - raw Zig source bytes to parse (owned by caller).
/// `g` - the graph to populate; a file node is always added as the first node.
/// `file_path` - relative path within the project, used for cross-file import
///   resolution. When null, import resolution falls back to basename-only lookup.
/// `logger` - structured logger; pass Logger.noop for silent operation.
///
/// Returns `anyerror` because tree-sitter and graph operations may fail.
/// On tree-sitter parse failure, a bare file node is still created so the
/// graph remains consistent.
pub fn parse(allocator: std.mem.Allocator, source: []const u8, g: *Graph, file_path: ?[]const u8, logger: Logger) anyerror!void {
    const log = logger.withScope("zig-visitor");

    log.debug("parsing source", &.{Field.uint("bytes", source.len)});

    const line_count = ts_api.countLines(source);
    const ts_lang = ts_api.zigLanguage();
    const k = KindIds.init(ts_lang);

    // Parse source with tree-sitter first so we can collect module doc comments.
    const tree = ts_api.parseSource(ts_lang, source) orelse {
        log.warn("tree-sitter parse failed", &.{});
        // If tree-sitter parsing fails, create a bare file node.
        _ = try g.addNode(allocator, .{
            .id = .root,
            .name = "",
            .kind = .file,
            .language = .zig,
            .visibility = .public,
            .line_start = 1,
            .line_end = if (line_count > 0) line_count else null,
            .file_path = file_path,
        });
        return;
    };
    defer tree.destroy();

    const root = tree.rootNode();

    // Collect module doc comments (//!) from the beginning of the file.
    const module_doc = ast.collectModuleDocComment(source, root, &k);

    // Create file node (always the first node).
    const file_id = try g.addNode(allocator, .{
        .id = .root,
        .name = "",
        .kind = .file,
        .language = .zig,
        .visibility = .public,
        .line_start = 1,
        .line_end = if (line_count > 0) line_count else null,
        .doc = module_doc,
        .file_path = file_path,
    });

    // -- Phase 1: Node creation --
    // Walk top-level declarations and recursively create graph nodes.
    var i: u32 = 0;
    while (i < root.childCount()) : (i += 1) {
        const child = root.child(i) orelse continue;
        if (!child.isNamed()) continue;
        try processDeclaration(allocator, g, source, child, file_id, &k, log);
    }

    // -- Phase 2: Import map construction --
    // Build edge resolution context (scope range + import map) and lookup indices
    // needed by the edge walker to resolve cross-file references.
    var ctx = cf.EdgeContext{
        .scope_start = @intFromEnum(file_id),
        .scope_end = g.nodeCount(),
    };

    // Build lookup indices for scope-based and file-based resolution.
    var file_index = try FileIndex.build(allocator, g.nodes.items);
    defer file_index.deinit(allocator);

    var scope_index = try ScopeIndex.build(allocator, g.nodes.items, 0);
    defer scope_index.deinit(allocator);

    cf.buildImportMap(g, source, root, &ctx, &file_index, file_path, &k, log);

    // -- Phase 3: Edge creation --
    // Walk the AST a second time to discover call and uses_type relationships,
    // resolving targets via the scope index, file index, and import map.
    log.debug("building edges", &.{});
    try eb.walkForEdges(allocator, g, source, root, &ctx, &k, &scope_index, &file_index, log);
}

/// Dispatch a single top-level or nested declaration to the appropriate
/// handler based on its tree-sitter node kind. Unrecognized kinds are
/// silently skipped (they produce no graph node).
fn processDeclaration(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
    const kid = ts_node.kindId();

    if (kid == k.variable_declaration) {
        try processVariableDecl(allocator, g, source, ts_node, parent_id, k, log);
    } else if (kid == k.function_declaration) {
        try processFunctionDecl(allocator, g, source, ts_node, parent_id, k, log);
    } else if (kid == k.test_declaration) {
        try processTestDecl(allocator, g, source, ts_node, parent_id, k, log);
    } else if (kid == k.container_field) {
        try processContainerField(allocator, g, source, ts_node, parent_id, k, log);
    } else if (kid == k.comptime_declaration) {
        try processComptimeDecl(allocator, g, source, ts_node, parent_id, k, log);
    }
}

/// Process a const/var declaration and add a graph node for it.
/// Classifies the value to determine the node kind (struct, enum, union,
/// error set, import, or plain constant). Filters noise constants such as
/// @This() aliases and private same-name re-exports. Detects Zig-specific
/// qualifiers (mutable, comptime, packed, extern) and stores them as LangMeta.
/// Recurses into container bodies for nested declarations.
fn processVariableDecl(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
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

        // Skip same-name re-exports from imports, but only when private.
        // Public re-exports (e.g., `pub const iovec = std.posix.iovec`) are intentional API.
        if (ast.getFieldExprRootAndLeaf(source, ts_node, k)) |info| {
            if (std.mem.eql(u8, info.leaf, name) and visibility == .private) {
                if (isImportSibling(g, parent_id, info.root)) {
                    log.trace("skipping private re-export", &.{Field.string("name", name)});
                    return;
                }
            }
        }
    }

    // Detect all Zig-specific qualifiers.
    const is_mutable = ast.hasKeyword(ts_node, k.var_kw);
    const is_comptime = kind == .constant and ast.hasTypeAnnotation(ts_node, k);
    const is_packed = if (classification.body) |body| ast.hasKeyword(body, k.packed_kw) else false;
    const is_extern = if (classification.body) |body| ast.hasKeyword(body, k.extern_kw) else false;
    const comptime_conditional = classification.comptime_conditional;

    const has_zig_meta = is_mutable or is_comptime or is_packed or is_extern or comptime_conditional;
    const lang_meta: LangMeta = if (has_zig_meta) .{ .zig = .{
        .is_mutable = is_mutable,
        .is_comptime = is_comptime,
        .is_packed = is_packed,
        .is_extern = is_extern,
        .comptime_conditional = comptime_conditional,
    } } else .{ .none = {} };

    // For import declarations, store the import path extracted from the AST.
    const sig: ?[]const u8 = if (kind == .import_decl)
        cf.extractImportPath(source, ts_node, k)
    else if (kind == .error_def)
        if (classification.body) |body| source[body.startByte()..body.endByte()] else null
    else
        null;

    const node_id = try g.addNode(allocator, .{
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
    if (classification.body) |body| {
        var i: u32 = 0;
        while (i < body.childCount()) : (i += 1) {
            const child = body.child(i) orelse continue;
            if (!child.isNamed()) continue;
            try processDeclaration(allocator, g, source, child, node_id, k, log);
        }
    }
}

/// Process a function declaration and add a graph node for it.
/// Handles two cases: (1) type-returning generic functions like
/// `fn Foo(comptime T: type) type { return struct { ... }; }` are
/// promoted to type_def/enum_def/union_def nodes with their returned
/// body's children; (2) regular functions become .function nodes.
/// Extracts the function signature, visibility, doc comment, and
/// Zig-specific qualifiers (extern, inline, calling convention).
/// Recurses into the block body to discover inner type definitions.
fn processFunctionDecl(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
    const name = ast.getIdentifierName(source, ts_node, k) orelse {
        log.trace("skipping function: no identifier", &.{});
        return;
    };
    const visibility = ast.detectVisibility(ts_node, k);
    const doc = ast.collectDocComment(source, ts_node, k);

    // Shared: extract function signature and is_inline before branching.
    // Both type-returning and regular function paths need these.
    const signature = extractFunctionSignature(source, ts_node, k);
    const is_inline = ast.hasKeyword(ts_node, k.inline_kw);

    // Detect type-returning generic functions: `fn Foo(comptime T: type) type { return struct { ... }; }`
    if (ast.returnsType(source, ts_node, k)) {
        if (ast.findReturnedTypeBody(ts_node, k)) |body_info| {
            const kind: NodeKind = switch (body_info.kind) {
                .enum_like => .enum_def,
                .union_like => .union_def,
                .struct_like => .type_def,
            };
            // Container qualifiers from the returned body.
            const is_packed = ast.hasKeyword(body_info.body, k.packed_kw);
            const is_extern_container = ast.hasKeyword(body_info.body, k.extern_kw);
            // Combine container qualifiers with function-level is_inline.
            const has_zig_meta = is_packed or is_extern_container or is_inline;
            const type_lang_meta: LangMeta = if (has_zig_meta) .{ .zig = .{
                .is_packed = is_packed,
                .is_extern = is_extern_container,
                .is_inline = is_inline,
            } } else .{ .none = {} };

            const type_id = try g.addNode(allocator, .{
                .id = .root,
                .name = name,
                .kind = kind,
                .language = .zig,
                .parent_id = parent_id,
                .visibility = visibility,
                .doc = doc,
                .signature = signature,
                .lang_meta = type_lang_meta,
                .line_start = ts_node.startPoint().row + 1,
                .line_end = ts_node.endPoint().row + 1,
            });
            // Recurse into the returned struct/union/enum body.
            var i: u32 = 0;
            while (i < body_info.body.childCount()) : (i += 1) {
                const child = body_info.body.child(i) orelse continue;
                if (!child.isNamed()) continue;
                try processDeclaration(allocator, g, source, child, type_id, k, log);
            }
            return;
        } else {
            log.debug("type-returning function: body not found", &.{Field.string("name", name)});
        }
    }

    // Regular function path: extract function-specific qualifiers.
    const is_extern = ast.hasKeyword(ts_node, k.extern_kw);
    const lang_meta: LangMeta = if (is_extern or is_inline) .{ .zig = .{
        .is_extern = is_extern,
        .is_inline = is_inline,
        .calling_convention = if (is_extern) ast.extractCallingConvention(source, ts_node, k) else null,
    } } else .{ .none = {} };

    const fn_id = try g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = .function,
        .language = .zig,
        .parent_id = parent_id,
        .visibility = visibility,
        .doc = doc,
        .signature = signature,
        .lang_meta = lang_meta,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
    });

    // Discover inner type definitions inside the function block body.
    var fi: u32 = 0;
    while (fi < ts_node.childCount()) : (fi += 1) {
        const child = ts_node.child(fi) orelse continue;
        if (child.kindId() == k.block) {
            try discoverInnerTypes(allocator, g, source, child, fn_id, k, log);
            break;
        }
    }
}

/// Extract the function header text from the declaration start up to (but
/// not including) the block body, with trailing whitespace trimmed.
/// For `pub fn create() svc_mod.Service { ... }`, returns
/// `pub fn create() svc_mod.Service`.
/// For extern functions (no block body), uses the full declaration minus
/// the trailing semicolon. Returns null if byte offsets are out of range.
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
    // No block found (extern function), use full declaration minus trailing semicolon.
    const end = ts_node.endByte();
    if (end > start and end <= source.len) {
        return std.mem.trimRight(u8, source[start..end], " \t\n\r;");
    }
    return null;
}

/// Process a test declaration and add a .test_def node.
/// Extracts the test name (string literal, decl-reference, or quoted identifier)
/// and any preceding doc comment. Recurses into the test body for inner types.
fn processTestDecl(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
    const name = ast.getTestName(source, ts_node, k);
    const doc = ast.collectDocComment(source, ts_node, k);

    const test_id = try g.addNode(allocator, .{
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

    // Discover inner type definitions inside the test block body.
    var i: u32 = 0;
    while (i < ts_node.childCount()) : (i += 1) {
        const child = ts_node.child(i) orelse continue;
        if (child.kindId() == k.block) {
            try discoverInnerTypes(allocator, g, source, child, test_id, k, log);
            break;
        }
    }
}

/// Process a comptime block declaration.
/// Comptime blocks are syntactic containers, not semantic entities --
/// they produce no graph node themselves. Instead, inner type definitions
/// are promoted to children of the enclosing scope (parent_id).
fn processComptimeDecl(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
    var i: u32 = 0;
    while (i < ts_node.childCount()) : (i += 1) {
        const child = ts_node.child(i) orelse continue;
        if (child.kindId() == k.block) {
            try discoverInnerTypes(allocator, g, source, child, parent_id, k, log);
            break;
        }
    }
}

/// Scan a block node for variable_declarations whose value classifies as a type
/// container (struct, enum, union). For each match, calls processVariableDecl which
/// recursively handles nested declarations (methods, fields, inner types).
/// Also recurses into nested blocks (if/while/for/comptime bodies) to catch
/// type definitions at any depth within the block.
fn discoverInnerTypes(allocator: std.mem.Allocator, g: *Graph, source: []const u8, block: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
    var i: u32 = 0;
    while (i < block.childCount()) : (i += 1) {
        const child = block.child(i) orelse continue;
        const kid = child.kindId();

        if (kid == k.variable_declaration) {
            const classification = ast.classifyVariableValue(source, child, k);
            if (classification.body != null) {
                try processVariableDecl(allocator, g, source, child, parent_id, k, log);
            }
            continue;
        }

        // Recurse into nested block-like nodes to find deeper type definitions.
        if (kid == k.block or
            kid == k.if_statement or
            kid == k.if_expression or
            kid == k.for_statement or
            kid == k.while_statement or
            kid == k.expression_statement or
            kid == k.defer_statement)
        {
            try discoverInnerTypes(allocator, g, source, child, parent_id, k, log);
        }
    }
}

/// Process a container field (struct field or enum variant) and add a .field node.
/// Fields are always private. Skips unnamed fields (e.g. padding fields).
fn processContainerField(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
    const name = ast.getIdentifierName(source, ts_node, k) orelse {
        log.trace("skipping field: no identifier", &.{});
        return;
    };
    const doc = ast.collectDocComment(source, ts_node, k);

    _ = try g.addNode(allocator, .{
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

test "simple fixture: nodes, visibility, parents, doc comments" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.simple, &g, null, Logger.noop);

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

    // Assert: parent_id for manhattan points to struct Point
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

    // Assert: parent_id for struct Point points to file node
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

    // Assert: top-level function defaultPoint points to file node
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

test "simple fixture: unions, enum methods, imports, fields, variants, module doc" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.simple, &g, null, Logger.noop);

    // unions
    // tagged union Shape: union_def, public, doc
    var found_shape = false;
    // plain union RawValue: union_def, private
    var found_raw = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "Shape")) {
            try std.testing.expectEqual(NodeKind.union_def, n.kind);
            try std.testing.expectEqual(Visibility.public, n.visibility);
            try std.testing.expect(n.doc != null);
            try std.testing.expect(n.parent_id != null);
            const parent = g.getNode(n.parent_id.?) orelse continue;
            try std.testing.expectEqual(NodeKind.file, parent.kind);
            found_shape = true;
        }
        if (std.mem.eql(u8, n.name, "RawValue")) {
            try std.testing.expectEqual(NodeKind.union_def, n.kind);
            try std.testing.expectEqual(Visibility.private, n.visibility);
            found_raw = true;
        }
    }
    try std.testing.expect(found_shape);
    try std.testing.expect(found_raw);

    // enum method isWarm is function child of Color
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

    // imports
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

    // struct fields
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

    // enum variants
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

    // module doc comment
    const file_node2 = g.getNode(@enumFromInt(0));
    try std.testing.expect(file_node2 != null);
    try std.testing.expectEqual(NodeKind.file, file_node2.?.kind);
    try std.testing.expect(file_node2.?.doc != null);
    const doc = file_node2.?.doc.?;
    try std.testing.expect(std.mem.indexOf(u8, doc, "Simple fixture") != null);
    try std.testing.expect(std.mem.indexOf(u8, doc, "Compute the Manhattan") == null);
}

test "generic type fixture: type_def nodes, parents, visibility, doc comments" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.generic_type, &g, null, Logger.noop);

    // Container exists as type_def with file as parent
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

    // public methods init, deinit, count, isEmpty exist
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

    // private helper validate exists
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

    // init has parent Container
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

    // Entry has parent Container
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

    // count method has doc
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

    // Result union type and isOk method exist
    var found_result = false;
    var found_isOk = false;
    i = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "Result") and
            n.kind.isTypeContainer()) found_result = true;
        if (n.kind == .function and std.mem.eql(u8, n.name, "isOk")) found_isOk = true;
    }
    try std.testing.expect(found_result);
    try std.testing.expect(found_isOk);

    // non-generic Config struct with defaults method
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

test "file struct fixture: methods, nested types, Self filtering" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.file_struct, &g, null, Logger.noop);

    // methods are direct children of file node
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

    // Config nested type with file as parent
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

    // Self constant is filtered
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

test "edge case fixtures: empty, comments-only, no-pub, deep nesting, many params, unicode" {
    // empty file
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        try parse(std.testing.allocator, fixtures.zig.edge_cases.empty, &g, null, Logger.noop);

        try std.testing.expectEqual(@as(usize, 1), g.nodeCount());
        try std.testing.expectEqual(@as(usize, 0), g.edgeCount());
        const f = g.getNode(@enumFromInt(0));
        try std.testing.expect(f != null);
        try std.testing.expectEqual(NodeKind.file, f.?.kind);
    }

    // only comments
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        try parse(std.testing.allocator, fixtures.zig.edge_cases.only_comments, &g, null, Logger.noop);

        try std.testing.expectEqual(@as(usize, 1), g.nodeCount());
        const f = g.getNode(@enumFromInt(0));
        try std.testing.expect(f != null);
        try std.testing.expectEqual(NodeKind.file, f.?.kind);
    }

    // no pub: all private
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        try parse(std.testing.allocator, fixtures.zig.edge_cases.no_pub, &g, null, Logger.noop);

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

    // deeply nested: innerMethod -> Inner -> Middle -> Outer -> file
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        try parse(std.testing.allocator, fixtures.zig.edge_cases.deeply_nested, &g, null, Logger.noop);

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

    // many params
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        try parse(std.testing.allocator, fixtures.zig.edge_cases.many_params, &g, null, Logger.noop);

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

    // unicode names
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        try parse(std.testing.allocator, fixtures.zig.edge_cases.unicode_names, &g, null, Logger.noop);

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

test "decl-reference and string-literal test names" {
    // bare identifier test name
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\fn helper() i32 { return 42; }
            \\test helper { _ = helper(); }
        ;
        try parse(std.testing.allocator, source, &g, null, Logger.noop);
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

    // type identifier test name
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\const Foo = struct {
            \\    pub fn bar() void {}
            \\};
            \\test Foo { Foo.bar(); }
        ;
        try parse(std.testing.allocator, source, &g, null, Logger.noop);
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

    // two decl-reference tests get distinct names
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\fn alpha() i32 { return 1; }
            \\fn beta() i32 { return 2; }
            \\test alpha { _ = alpha(); }
            \\test beta { _ = beta(); }
        ;
        try parse(std.testing.allocator, source, &g, null, Logger.noop);
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

    // @"quoted identifier" test name
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\test @"edge case name" {}
        ;
        try parse(std.testing.allocator, source, &g, null, Logger.noop);
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

    // string-literal test name
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\fn foo() void {}
            \\test "calls foo" { _ = foo(); }
        ;
        try parse(std.testing.allocator, source, &g, null, Logger.noop);
        for (g.nodes.items) |n| {
            if (n.kind == .test_def) {
                try std.testing.expectEqualStrings("calls foo", n.name);
                break;
            }
        }
    }
}

test "module doc comment separation" {
    // no module doc -> null
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        try parse(std.testing.allocator, "const x = 42;\n", &g, null, Logger.noop);

        const f = g.getNode(@enumFromInt(0));
        try std.testing.expect(f != null);
        try std.testing.expectEqual(NodeKind.file, f.?.kind);
        try std.testing.expectEqual(@as(?[]const u8, null), f.?.doc);
    }

    // module doc separate from item doc
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\//! Module doc.
            \\/// Item doc.
            \\const x = 42;
        ;
        try parse(std.testing.allocator, source, &g, null, Logger.noop);

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

test "extern functions fixture: qualifiers, calling convention, and signatures" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.edge_cases.extern_functions, &g, null, Logger.noop);

    // Assert: find all function nodes
    var c_write_node: ?*const Node = null;
    var bare_extern_node: ?*const Node = null;
    var private_extern_node: ?*const Node = null;
    var fast_add_node: ?*const Node = null;
    var internal_helper_node: ?*const Node = null;
    var normal_function_node: ?*const Node = null;
    var also_normal_node: ?*const Node = null;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind != .function) continue;
        if (std.mem.eql(u8, n.name, "c_write")) c_write_node = n;
        if (std.mem.eql(u8, n.name, "bare_extern")) bare_extern_node = n;
        if (std.mem.eql(u8, n.name, "private_extern")) private_extern_node = n;
        if (std.mem.eql(u8, n.name, "fast_add")) fast_add_node = n;
        if (std.mem.eql(u8, n.name, "internal_helper")) internal_helper_node = n;
        if (std.mem.eql(u8, n.name, "normal_function")) normal_function_node = n;
        if (std.mem.eql(u8, n.name, "also_normal")) also_normal_node = n;
    }
    try std.testing.expect(c_write_node != null);
    try std.testing.expect(bare_extern_node != null);
    try std.testing.expect(private_extern_node != null);
    try std.testing.expect(fast_add_node != null);
    try std.testing.expect(internal_helper_node != null);
    try std.testing.expect(normal_function_node != null);
    try std.testing.expect(also_normal_node != null);

    // Assert: c_write has is_extern=true, calling_convention="c", and a signature
    switch (c_write_node.?.lang_meta) {
        .zig => |zm| {
            try std.testing.expect(zm.is_extern);
            try std.testing.expect(!zm.is_inline);
            try std.testing.expectEqualStrings("c", zm.calling_convention.?);
        },
        .none => return error.ExpectedZigMeta,
    }
    try std.testing.expect(c_write_node.?.signature != null);

    // Assert: bare_extern has is_extern=true, no calling convention, and a signature
    switch (bare_extern_node.?.lang_meta) {
        .zig => |zm| {
            try std.testing.expect(zm.is_extern);
            try std.testing.expectEqual(@as(?[]const u8, null), zm.calling_convention);
        },
        .none => return error.ExpectedZigMeta,
    }
    try std.testing.expect(bare_extern_node.?.signature != null);

    // Assert: private_extern has is_extern=true and visibility=private
    try std.testing.expectEqual(Visibility.private, private_extern_node.?.visibility);
    switch (private_extern_node.?.lang_meta) {
        .zig => |zm| try std.testing.expect(zm.is_extern),
        .none => return error.ExpectedZigMeta,
    }

    // Assert: fast_add has is_inline=true, not extern, no calling convention
    switch (fast_add_node.?.lang_meta) {
        .zig => |zm| {
            try std.testing.expect(zm.is_inline);
            try std.testing.expect(!zm.is_extern);
            try std.testing.expectEqual(@as(?[]const u8, null), zm.calling_convention);
        },
        .none => return error.ExpectedZigMeta,
    }

    // Assert: internal_helper is inline and private
    try std.testing.expectEqual(Visibility.private, internal_helper_node.?.visibility);
    switch (internal_helper_node.?.lang_meta) {
        .zig => |zm| try std.testing.expect(zm.is_inline),
        .none => return error.ExpectedZigMeta,
    }

    // Assert: regular functions have no zig meta
    try std.testing.expectEqual(LangMeta{ .none = {} }, normal_function_node.?.lang_meta);
    try std.testing.expectEqual(LangMeta{ .none = {} }, also_normal_node.?.lang_meta);
}

test "mutability fixture: var vs const metadata" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.edge_cases.mutability, &g, null, Logger.noop);

    // Assert: find relevant nodes
    var max_size_node: ?*const Node = null;
    var default_name_node: ?*const Node = null;
    var counter_node: ?*const Node = null;
    var internal_state_node: ?*const Node = null;
    var verbose_node: ?*const Node = null;
    var config_node: ?*const Node = null;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "max_size")) max_size_node = n;
        if (std.mem.eql(u8, n.name, "default_name")) default_name_node = n;
        if (std.mem.eql(u8, n.name, "counter")) counter_node = n;
        if (std.mem.eql(u8, n.name, "internal_state")) internal_state_node = n;
        if (std.mem.eql(u8, n.name, "verbose")) verbose_node = n;
        if (std.mem.eql(u8, n.name, "Config")) config_node = n;
    }
    try std.testing.expect(max_size_node != null);
    try std.testing.expect(default_name_node != null);
    try std.testing.expect(counter_node != null);
    try std.testing.expect(internal_state_node != null);
    try std.testing.expect(verbose_node != null);
    try std.testing.expect(config_node != null);

    // Assert: const declarations have is_mutable false
    try std.testing.expectEqual(LangMeta{ .zig = .{ .is_comptime = true } }, max_size_node.?.lang_meta);
    try std.testing.expectEqual(LangMeta{ .none = {} }, default_name_node.?.lang_meta);

    // Assert: var declarations have is_mutable true and keep kind .constant
    try std.testing.expectEqual(NodeKind.constant, counter_node.?.kind);
    try std.testing.expectEqual(LangMeta{ .zig = .{ .is_mutable = true, .is_comptime = true } }, counter_node.?.lang_meta);

    try std.testing.expectEqual(NodeKind.constant, internal_state_node.?.kind);
    try std.testing.expectEqual(LangMeta{ .zig = .{ .is_mutable = true, .is_comptime = true } }, internal_state_node.?.lang_meta);

    // Assert: var without type annotation has is_mutable but not is_comptime
    try std.testing.expectEqual(NodeKind.constant, verbose_node.?.kind);
    try std.testing.expectEqual(LangMeta{ .zig = .{ .is_mutable = true } }, verbose_node.?.lang_meta);

    // Assert: struct const is still classified as type_def (unaffected)
    try std.testing.expectEqual(NodeKind.type_def, config_node.?.kind);
}
