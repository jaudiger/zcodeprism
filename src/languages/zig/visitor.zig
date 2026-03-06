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
const phantom_mod = @import("../../core/phantom.zig");

const KindIds = pc.KindIds;
const GraphIndex = @import("../../core/graph_index.zig").GraphIndex;

const Field = logging.Field;
const Logger = logging.Logger;

const Graph = graph_mod.Graph;
const Node = node_mod.Node;
const NodeId = types.NodeId;
const NodeKind = types.NodeKind;
const Visibility = types.Visibility;
const Language = types.Language;
const LangMeta = lang.LangMeta;

/// Bundles shared state threaded through all process* functions.
const VisitorContext = struct {
    g: *Graph,
    source: []const u8,
    k: *const KindIds,
    log: Logger,
};

/// Parse Zig source code and populate the graph with nodes and edges.
/// This is the entry point used by the LanguageSupport registry.
///
/// `source` - raw Zig source bytes to parse (owned by caller).
/// `g` - the graph to populate; a file node is always added as the first node.
/// `file_path` - relative path within the project, used for cross-file import
///   resolution. When null, import resolution falls back to basename-only lookup.
/// `logger` - structured logger; pass Logger.noop for silent operation.
///
/// On tree-sitter parse failure, a bare file node is still created so the
/// graph remains consistent.
pub fn parse(allocator: std.mem.Allocator, source: []const u8, g: *Graph, file_path: ?[]const u8, logger: Logger) error{OutOfMemory}!void {
    const log = logger.withScope("zig-visitor");

    log.debug("parsing source", &.{Field.uint("bytes", source.len)});

    const line_count = ts_api.countLines(source);
    const ts_lang = ts_api.tree_sitter_zig();
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

    const ctx = VisitorContext{ .g = g, .source = source, .k = &k, .log = log };

    // Walk top-level declarations and recursively create graph nodes.
    var i: u32 = 0;
    while (i < root.childCount()) : (i += 1) {
        const child = root.child(i) orelse continue;
        if (!child.isNamed()) continue;
        try processDeclaration(allocator, &ctx, child, file_id);
    }
}

/// Re-parse source and emit cross-file edges for the Zig file at file_idx.
pub fn buildEdges(allocator: std.mem.Allocator, source: []const u8, g: *Graph, file_idx: usize, scope_end: usize, file_path: ?[]const u8, graph_index: *const GraphIndex, phantom_mgr: *const phantom_mod.PhantomManager, logger: Logger) error{OutOfMemory}!void {
    const log = logger.withScope("zig-edges");

    const ts_lang = ts_api.tree_sitter_zig();
    const k = KindIds.init(ts_lang);

    const tree = ts_api.parseSource(ts_lang, source) orelse return;
    defer tree.destroy();
    const root = tree.rootNode();

    const file_id: NodeId = @enumFromInt(file_idx);

    var ctx = cf.EdgeContext{
        .scope_start = @intFromEnum(file_id),
        .scope_end = scope_end,
    };
    defer ctx.deinit(allocator);

    try cf.buildImportMap(allocator, g, source, root, &ctx, &graph_index.files, file_path, &k, log);

    log.debug("building edges", &.{});
    try eb.walkForEdges(allocator, g, source, root, &ctx, &k, graph_index, phantom_mgr, log);
}

/// Dispatch a single top-level or nested declaration to the appropriate
/// handler based on its tree-sitter node kind. Unrecognized kinds are
/// silently skipped (they produce no graph node).
fn processDeclaration(allocator: std.mem.Allocator, ctx: *const VisitorContext, ts_node: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    const kid = ts_node.kindId();

    if (kid == ctx.k.variable_declaration) {
        try processVariableDecl(allocator, ctx, ts_node, parent_id);
    } else if (kid == ctx.k.function_declaration) {
        try processFunctionDecl(allocator, ctx, ts_node, parent_id);
    } else if (kid == ctx.k.test_declaration) {
        try processTestDecl(allocator, ctx, ts_node, parent_id);
    } else if (kid == ctx.k.container_field) {
        try processContainerField(allocator, ctx, ts_node, parent_id);
    } else if (kid == ctx.k.comptime_declaration) {
        try processComptimeDecl(allocator, ctx, ts_node, parent_id);
    }
}

/// Process a const/var declaration and add a graph node for it.
/// Classifies the value to determine the node kind (struct, enum, union,
/// error set, import, or plain constant). Filters noise constants:
/// @This() aliases and private same-name re-exports. Detects Zig-specific
/// qualifiers (mutable, comptime, packed, extern) and stores them as LangMeta.
/// Recurses into container bodies for nested declarations.
fn processVariableDecl(allocator: std.mem.Allocator, ctx: *const VisitorContext, ts_node: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    const name = ast.getIdentifierName(ctx.source, ts_node, ctx.k) orelse {
        ctx.log.trace("skipping variable: no identifier", &.{});
        return;
    };
    const visibility = ast.detectVisibility(ts_node, ctx.k);
    const doc = ast.collectDocComment(ctx.source, ts_node, ctx.k);

    // Classify the value: struct, enum, error set, import, or plain constant.
    const classification = ast.classifyVariableValue(ctx.source, ts_node, ctx.k);
    const kind = classification.kind;

    // Filter noise constants that produce no useful graph information.
    if (kind == .constant) {
        // Skip @This() aliases.
        if (ast.isThisBuiltin(ctx.source, ts_node, ctx.k)) {
            ctx.log.trace("skipping @This() alias", &.{Field.string("name", name)});
            return;
        }

        // Skip same-name re-exports from imports, but only when private.
        // Public re-exports are intentional API.
        if (ast.getFieldExprRootAndLeaf(ctx.source, ts_node, ctx.k)) |info| {
            if (std.mem.eql(u8, info.leaf, name) and visibility == .private) {
                if (isImportSibling(ctx.g, parent_id, info.root)) {
                    ctx.log.trace("skipping private re-export", &.{Field.string("name", name)});
                    return;
                }
            }
        }
    }

    // Detect all Zig-specific qualifiers.
    const is_mutable = ast.hasKeyword(ts_node, ctx.k.var_kw);
    const is_comptime = kind == .constant and ast.hasTypeAnnotation(ts_node, ctx.k);
    const is_packed = if (classification.body) |body| ast.hasKeyword(body, ctx.k.packed_kw) else false;
    const is_extern = if (classification.body) |body| ast.hasKeyword(body, ctx.k.extern_kw) else false;
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
        cf.extractImportPath(ctx.source, ts_node, ctx.k)
    else if (kind == .error_def)
        if (classification.body) |body| ctx.source[body.startByte()..body.endByte()] else null
    else
        null;

    const node_id = try ctx.g.addNode(allocator, .{
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
            try processDeclaration(allocator, ctx, child, node_id);
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
fn processFunctionDecl(allocator: std.mem.Allocator, ctx: *const VisitorContext, ts_node: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    const name = ast.getIdentifierName(ctx.source, ts_node, ctx.k) orelse {
        ctx.log.trace("skipping function: no identifier", &.{});
        return;
    };
    const visibility = ast.detectVisibility(ts_node, ctx.k);
    const doc = ast.collectDocComment(ctx.source, ts_node, ctx.k);

    // Shared: extract function signature and is_inline before branching.
    // Both type-returning and regular function paths need these.
    const signature = extractFunctionSignature(ctx.source, ts_node, ctx.k);
    const is_inline = ast.hasKeyword(ts_node, ctx.k.inline_kw);

    // Detect type-returning generic functions: `fn Foo(comptime T: type) type { return struct { ... }; }`
    if (ast.returnsType(ctx.source, ts_node, ctx.k)) {
        if (ast.findReturnedTypeBody(ts_node, ctx.k)) |body_info| {
            const fkind: NodeKind = switch (body_info.kind) {
                .enum_like => .enum_def,
                .union_like => .union_def,
                .struct_like => .type_def,
            };
            // Container qualifiers from the returned body.
            const is_packed = ast.hasKeyword(body_info.body, ctx.k.packed_kw);
            const is_extern_container = ast.hasKeyword(body_info.body, ctx.k.extern_kw);
            // Combine container qualifiers with function-level is_inline.
            const has_zig_meta = is_packed or is_extern_container or is_inline;
            const type_lang_meta: LangMeta = if (has_zig_meta) .{ .zig = .{
                .is_packed = is_packed,
                .is_extern = is_extern_container,
                .is_inline = is_inline,
            } } else .{ .none = {} };

            const type_id = try ctx.g.addNode(allocator, .{
                .id = .root,
                .name = name,
                .kind = fkind,
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
                try processDeclaration(allocator, ctx, child, type_id);
            }
            return;
        } else {
            ctx.log.debug("type-returning function: body not found", &.{Field.string("name", name)});
        }
    }

    // Regular function path: extract function-specific qualifiers.
    const is_extern = ast.hasKeyword(ts_node, ctx.k.extern_kw);
    const lang_meta: LangMeta = if (is_extern or is_inline) .{ .zig = .{
        .is_extern = is_extern,
        .is_inline = is_inline,
        .calling_convention = if (is_extern) ast.extractCallingConvention(ctx.source, ts_node, ctx.k) else null,
    } } else .{ .none = {} };

    const fn_id = try ctx.g.addNode(allocator, .{
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
        if (child.kindId() == ctx.k.block) {
            try discoverInnerTypes(allocator, ctx, child, fn_id);
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
fn processTestDecl(allocator: std.mem.Allocator, ctx: *const VisitorContext, ts_node: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    const name = ast.getTestName(ctx.source, ts_node, ctx.k);
    const doc = ast.collectDocComment(ctx.source, ts_node, ctx.k);

    const test_id = try ctx.g.addNode(allocator, .{
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
        if (child.kindId() == ctx.k.block) {
            try discoverInnerTypes(allocator, ctx, child, test_id);
            break;
        }
    }
}

/// Process a comptime block declaration.
/// Comptime blocks are syntactic containers, not semantic entities --
/// they produce no graph node themselves. Instead, inner type definitions
/// are promoted to children of the enclosing scope (parent_id).
fn processComptimeDecl(allocator: std.mem.Allocator, ctx: *const VisitorContext, ts_node: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    var i: u32 = 0;
    while (i < ts_node.childCount()) : (i += 1) {
        const child = ts_node.child(i) orelse continue;
        if (child.kindId() == ctx.k.block) {
            try discoverInnerTypes(allocator, ctx, child, parent_id);
            break;
        }
    }
}

/// Scan a block node for variable_declarations whose value classifies as a type
/// container (struct, enum, union). For each match, calls processVariableDecl which
/// recursively handles nested declarations (methods, fields, inner types).
/// Also recurses into nested blocks (if/while/for/comptime bodies) to catch
/// type definitions at any depth within the block.
fn discoverInnerTypes(allocator: std.mem.Allocator, ctx: *const VisitorContext, block: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    var i: u32 = 0;
    while (i < block.childCount()) : (i += 1) {
        const child = block.child(i) orelse continue;
        const kid = child.kindId();

        if (kid == ctx.k.variable_declaration) {
            const classification = ast.classifyVariableValue(ctx.source, child, ctx.k);
            if (classification.body != null) {
                try processVariableDecl(allocator, ctx, child, parent_id);
            }
            continue;
        }

        // Recurse into nested block nodes to find deeper type definitions.
        if (kid == ctx.k.block or
            kid == ctx.k.if_statement or
            kid == ctx.k.if_expression or
            kid == ctx.k.for_statement or
            kid == ctx.k.while_statement or
            kid == ctx.k.expression_statement or
            kid == ctx.k.defer_statement)
        {
            try discoverInnerTypes(allocator, ctx, child, parent_id);
        }
    }
}

/// Process a container field (struct field or enum variant) and add a .field node.
/// Fields are always private. Skips unnamed fields.
fn processContainerField(allocator: std.mem.Allocator, ctx: *const VisitorContext, ts_node: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    const name = ast.getIdentifierName(ctx.source, ts_node, ctx.k) orelse {
        ctx.log.trace("skipping field: no identifier", &.{});
        return;
    };
    const doc = ast.collectDocComment(ctx.source, ts_node, ctx.k);

    _ = try ctx.g.addNode(allocator, .{
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
/// Scopes the search to the parent's subtree since nodes are appended in order.
fn isImportSibling(g: *const Graph, parent_id: NodeId, name: []const u8) bool {
    const start = @intFromEnum(parent_id);
    for (g.nodes.items[start..]) |n| {
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
}

test "file node is always first" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.simple, &g, "test/fixtures/zig/simple.zig", Logger.noop);

    // Assert: first node is file node
    const first = g.getNode(@enumFromInt(0)).?;
    try std.testing.expectEqual(NodeKind.file, first.kind);
}

test "file node has line_end" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.simple, &g, null, Logger.noop);

    // Assert: file node line_end > 1 for non-empty source
    const file_node = g.getNode(@enumFromInt(0)).?;
    try std.testing.expect(file_node.line_end != null);
    try std.testing.expect(file_node.line_end.? > 1);
}

test "struct methods are children" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.simple, &g, null, Logger.noop);

    // Assert: at least one function has a type_def parent
    var found_method = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and n.parent_id != null) {
            if (g.getNode(n.parent_id.?)) |parent| {
                if (parent.kind == .type_def) {
                    found_method = true;
                    break;
                }
            }
        }
    }
    try std.testing.expect(found_method);
}

test "doc comment attached to function" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.simple, &g, null, Logger.noop);

    // Assert: at least one function has a doc comment
    var found_doc = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and n.doc != null) {
            found_doc = true;
            break;
        }
    }
    try std.testing.expect(found_doc);
}

test "empty file produces single file node" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.edge_cases.empty, &g, null, Logger.noop);

    // Assert: exactly 1 node
    try std.testing.expectEqual(@as(usize, 1), g.nodeCount());
    const n = g.getNode(@enumFromInt(0)).?;
    try std.testing.expectEqual(NodeKind.file, n.kind);
}

test "only_comments file produces single file node" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.edge_cases.only_comments, &g, null, Logger.noop);

    // Assert: only file node
    try std.testing.expectEqual(@as(usize, 1), g.nodeCount());
}

test "no_pub file has no public declarations" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.edge_cases.no_pub, &g, null, Logger.noop);

    // Assert: no public nodes except the file node itself
    var i: usize = 1;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        try std.testing.expectEqual(Visibility.private, n.visibility);
    }
}

test "language is always zig" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.simple, &g, null, Logger.noop);

    // Assert: every node has language == .zig
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        try std.testing.expectEqual(Language.zig, n.language);
    }
}

test "file_struct fixture: @This aliases skipped" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.file_struct, &g, null, Logger.noop);

    // Assert: no node named "Self" (the @This() alias is skipped)
    var found_self = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "Self")) {
            found_self = true;
            break;
        }
    }
    try std.testing.expect(!found_self);
}

test "generic_type fixture: type-returning functions promoted to types" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.generic_type, &g, null, Logger.noop);

    // Assert: at least one type_def exists with a method child (promoted generic fn)
    var found_promoted_type = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind.isTypeContainer() and n.parent_id != null) {
            if (g.getNode(n.parent_id.?)) |parent| {
                if (parent.kind == .file) {
                    found_promoted_type = true;
                    break;
                }
            }
        }
    }
    try std.testing.expect(found_promoted_type);
}

test "deeply_nested fixture: types at multiple nesting levels" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.edge_cases.deeply_nested, &g, null, Logger.noop);

    // Assert: at least 3 different nesting depth levels (file -> fn -> inner type)
    var max_depth: u32 = 0;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        var depth: u32 = 0;
        var pid = n.parent_id;
        while (pid) |p| : (depth += 1) {
            pid = if (g.getNode(p)) |pn| pn.parent_id else null;
        }
        if (depth > max_depth) max_depth = depth;
    }
    try std.testing.expect(max_depth >= 3);
}

test "function signatures extracted correctly" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.simple, &g, null, Logger.noop);

    // Assert: at least one function has a non-null signature starting with "pub fn" or "fn"
    var found_sig = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and n.signature != null) {
            const sig = n.signature.?;
            if (std.mem.startsWith(u8, sig, "pub fn") or std.mem.startsWith(u8, sig, "fn")) {
                found_sig = true;
                break;
            }
        }
    }
    try std.testing.expect(found_sig);
}

test "import_decl has signature with path" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.simple, &g, null, Logger.noop);

    // Assert: at least one import_decl has a non-null signature
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .import_decl and n.signature != null) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "module doc comment attached to file node" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.simple, &g, null, Logger.noop);

    // Assert: file node (index 0) has doc (module doc comment)
    const file_node = g.getNode(@enumFromInt(0)).?;
    try std.testing.expect(file_node.doc != null);
}

test "fields are private" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.simple, &g, null, Logger.noop);

    // Assert: all .field nodes are private
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .field) {
            try std.testing.expectEqual(Visibility.private, n.visibility);
        }
    }
}

test "error_def has signature with error set body" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.simple, &g, null, Logger.noop);

    // Assert: at least one error_def has a non-null signature
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .error_def and n.signature != null) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "line numbers are 1-based" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.simple, &g, null, Logger.noop);

    // Assert: all nodes have line_start >= 1
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.line_start) |ls| try std.testing.expect(ls >= 1);
    }
}

test "local_type_param fixture: method calls via local-type params" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.edge_cases.local_type_param, &g, null, Logger.noop);

    // Assert: the Processor type_def exists with the expected methods
    var found_processor = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Processor")) {
            found_processor = true;
            break;
        }
    }
    try std.testing.expect(found_processor);
}

test "generic_type fixture: enum-returning generic promoted to enum_def" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.generic_type, &g, null, Logger.noop);

    // Assert: find a node named "StatusEnum" that is an enum_def
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .enum_def and std.mem.eql(u8, n.name, "StatusEnum")) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "generic_type fixture: union-returning generic promoted to union_def" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.generic_type, &g, null, Logger.noop);

    // Assert: find a node named "ValueUnion" that is a union_def
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .union_def and std.mem.eql(u8, n.name, "ValueUnion")) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "generic_type fixture: type signature preserved on promoted types" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.generic_type, &g, null, Logger.noop);

    // Assert: find a node that is a type container with a non-null signature
    // containing parameter info from the generic function header
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind.isTypeContainer() and n.signature != null) {
            if (std.mem.indexOf(u8, n.signature.?, "comptime") != null) {
                found = true;
                break;
            }
        }
    }
    try std.testing.expect(found);
}
