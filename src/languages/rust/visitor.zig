const std = @import("std");
const graph_mod = @import("../../core/graph.zig");
const logging = @import("../../logging.zig");
const metrics_mod = @import("../../core/metrics.zig");
const node_mod = @import("../../core/node.zig");
const types = @import("../../core/types.zig");
const lang = @import("../language.zig");
const ts = @import("tree-sitter");
const ts_api = @import("../../parser/tree_sitter_api.zig");
const ast = @import("ast_analysis.zig");
const cf = @import("cross_file.zig");
const eb = @import("edge_builder.zig");
const pc = @import("parse_context.zig");
const rust_meta = @import("meta.zig");
const phantom_mod = @import("../../core/phantom.zig");

const Metrics = metrics_mod.Metrics;

const KindIds = pc.KindIds;
const GraphIndex = @import("../../core/graph_index.zig").GraphIndex;
const PhantomManager = phantom_mod.PhantomManager;

const Field = logging.Field;
const Logger = logging.Logger;

const Graph = graph_mod.Graph;
const Node = node_mod.Node;
const NodeId = types.NodeId;
const NodeKind = types.NodeKind;
const Visibility = types.Visibility;
const Language = types.Language;
const LangMeta = lang.LangMeta;
const RustMeta = rust_meta.RustMeta;
const RustSubKind = rust_meta.RustSubKind;

/// Bundles shared state threaded through all process* functions.
const VisitorContext = struct {
    g: *Graph,
    source: []const u8,
    k: *const KindIds,
    log: Logger,
};

/// Parse Rust source code and populate the graph with nodes and edges.
/// This is the entry point used by the LanguageSupport registry.
///
/// `source` - raw Rust source bytes to parse (owned by caller).
/// `g` - the graph to populate; a file node is always added as the first node.
/// `file_path` - relative path within the project, used for cross-file import
///   resolution. When null, import resolution falls back to basename-only lookup.
/// `logger` - structured logger; pass Logger.noop for silent operation.
pub fn parse(allocator: std.mem.Allocator, io: std.Io, source: []const u8, g: *Graph, file_path: ?[]const u8, logger: Logger) error{OutOfMemory}!void {
    const log = logger.withScope("rust-visitor");

    log.debug(io, "parsing source", &.{Field.uint("bytes", source.len)});

    const line_count = ts_api.countLines(source);
    const ts_lang = ts_api.tree_sitter_rust();
    const k = KindIds.init(ts_lang);

    const tree = ts_api.parseSource(ts_lang, source) orelse {
        log.warn(io, "tree-sitter parse failed", &.{});
        _ = try g.addNode(allocator, .{
            .id = .root,
            .name = "",
            .kind = .file,
            .language = .rust,
            .visibility = .public,
            .line_start = 1,
            .line_end = if (line_count > 0) line_count else null,
            .file_path = file_path,
        });
        return;
    };
    defer tree.destroy();

    const root = tree.rootNode();

    // Collect inner doc comments (//!) from the file start.
    const module_doc = ast.collectInnerDocComment(source, root, &k);

    // Collect crate-level inner attributes (#![...]).
    const inner_attrs = ast.collectInnerAttributes(source, root, &k);

    // Create file node (always the first node).
    const file_id = try g.addNode(allocator, .{
        .id = .root,
        .name = "",
        .kind = .file,
        .language = .rust,
        .visibility = .public,
        .line_start = 1,
        .line_end = if (line_count > 0) line_count else null,
        .doc = module_doc,
        .file_path = file_path,
        .lang_meta = if (inner_attrs != null) .{ .rust = .{ .inner_attributes = inner_attrs } } else .{ .none = {} },
    });

    const ctx = VisitorContext{ .g = g, .source = source, .k = &k, .log = log };

    // Walk top-level declarations and recursively create graph nodes.
    var i: u32 = 0;
    while (i < root.childCount()) : (i += 1) {
        const child = root.child(i) orelse continue;
        if (!child.isNamed()) continue;
        try processDeclaration(allocator, io, &ctx, child, file_id);
    }
}

/// Re-parse source and emit cross-file edges for the Rust file at file_idx.
/// Unresolved references are appended to `wl`.
pub fn buildEdges(allocator: std.mem.Allocator, io: std.Io, source: []const u8, g: *Graph, file_idx: usize, scope_end: usize, file_path: ?[]const u8, graph_index: *const GraphIndex, phantom_mgr: *const PhantomManager, node_type_map: *@import("../language_support.zig").NodeTypeMap, wl: *@import("../../lsp/worklist.zig").LspWorklist, logger: Logger) error{OutOfMemory}!void {
    _ = node_type_map;
    const log = logger.withScope("rust-edges");

    const ts_lang = ts_api.tree_sitter_rust();
    const k = KindIds.init(ts_lang);

    const tree = ts_api.parseSource(ts_lang, source) orelse return;
    defer tree.destroy();
    const root = tree.rootNode();

    var ctx = cf.EdgeContext{
        .scope_start = file_idx,
        .scope_end = scope_end,
    };
    defer ctx.deinit(allocator);

    try cf.buildImportMap(allocator, io, g, source, root, &ctx, graph_index, file_path, &k, log);
    try cf.buildExportEdges(allocator, io, g, &ctx, graph_index, log);

    log.debug(io, "building edges", &.{});
    try eb.walkForEdges(allocator, io, g, source, root, &k, &ctx, graph_index, phantom_mgr, wl, log);
}

/// Extract outer attributes and register the allocated buffer with the
/// graph so it gets freed on deinit. Centralizes the allocation +
/// ownership-transfer pattern for every process* function.
fn extractAndRegisterAttributes(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, k: *const KindIds) !?[]const u8 {
    const attributes = try ast.extractAttributes(allocator, source, ts_node, k);
    if (attributes) |a| {
        g.addOwnedBuffer(allocator, a) catch {
            allocator.free(a);
            return error.OutOfMemory;
        };
    }
    return attributes;
}

/// Dispatch a top-level or nested declaration to the appropriate handler.
fn processDeclaration(allocator: std.mem.Allocator, io: std.Io, ctx: *const VisitorContext, ts_node: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    const kid = ts_node.kindId();

    if (kid == ctx.k.function_item) {
        try processFunctionItem(allocator, io, ctx, ts_node, parent_id);
    } else if (kid == ctx.k.function_signature_item) {
        try processFunctionSignatureItem(allocator, io, ctx, ts_node, parent_id);
    } else if (kid == ctx.k.struct_item) {
        try processStructItem(allocator, io, ctx, ts_node, parent_id);
    } else if (kid == ctx.k.enum_item) {
        try processEnumItem(allocator, io, ctx, ts_node, parent_id);
    } else if (kid == ctx.k.union_item) {
        try processUnionItem(allocator, io, ctx, ts_node, parent_id);
    } else if (kid == ctx.k.trait_item) {
        try processTraitItem(allocator, io, ctx, ts_node, parent_id);
    } else if (kid == ctx.k.impl_item) {
        try processImplItem(allocator, io, ctx, ts_node, parent_id);
    } else if (kid == ctx.k.const_item) {
        try processConstItem(allocator, io, ctx, ts_node, parent_id);
    } else if (kid == ctx.k.static_item) {
        try processStaticItem(allocator, io, ctx, ts_node, parent_id);
    } else if (kid == ctx.k.type_item) {
        try processTypeItem(allocator, io, ctx, ts_node, parent_id);
    } else if (kid == ctx.k.macro_definition) {
        try processMacroDefinition(allocator, io, ctx, ts_node, parent_id);
    } else if (kid == ctx.k.mod_item) {
        try processModItem(allocator, io, ctx, ts_node, parent_id);
    } else if (kid == ctx.k.use_declaration) {
        try processUseDeclaration(allocator, io, ctx, ts_node, parent_id);
    } else if (kid == ctx.k.field_declaration) {
        try processFieldDeclaration(allocator, io, ctx, ts_node, parent_id);
    } else if (kid == ctx.k.enum_variant) {
        try processEnumVariant(allocator, io, ctx, ts_node, parent_id);
    } else if (kid == ctx.k.associated_type) {
        try processAssociatedType(allocator, io, ctx, ts_node, parent_id);
    }
}

/// Running tallies for control-flow metrics.
const MetricsAccum = struct {
    complexity: u16 = 1,
    branches: u16 = 0,
    loops: u16 = 0,
    error_paths: u16 = 0,
    max_depth: u16 = 0,
};

/// Walk a function body AST and return intrinsic metrics.
fn computeFunctionMetrics(body: ts.Node, k: *const KindIds) Metrics {
    var acc = MetricsAccum{};
    walkForMetrics(body, k, 0, &acc);
    return .{
        .complexity = acc.complexity,
        .branches = acc.branches,
        .loops = acc.loops,
        .error_paths = acc.error_paths,
        .nesting_depth_max = if (acc.max_depth > std.math.maxInt(u8)) std.math.maxInt(u8) else @intCast(acc.max_depth),
    };
}

/// Recursive walk over AST children, tallying control-flow nodes.
fn walkForMetrics(node: ts.Node, k: *const KindIds, depth: u16, acc: *MetricsAccum) void {
    const kid = node.kindId();

    var child_depth = depth;
    if (kid == k.block) {
        child_depth = depth + 1;
        if (child_depth > acc.max_depth) acc.max_depth = child_depth;
    }

    if (kid == k.if_expression) {
        acc.complexity += 1;
        acc.branches += 1;
    } else if (kid == k.for_expression) {
        acc.complexity += 1;
        acc.loops += 1;
    } else if (kid == k.while_expression) {
        acc.complexity += 1;
        acc.loops += 1;
    } else if (kid == k.loop_expression) {
        acc.complexity += 1;
        acc.loops += 1;
    } else if (kid == k.match_expression) {
        acc.complexity += 1;
        acc.branches += 1;
    } else if (kid == k.closure_expression) {
        acc.complexity += 1;
    } else if (kid == k.try_expression) {
        acc.error_paths += 1;
    }

    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        walkForMetrics(child, k, child_depth, acc);
    }
}

/// Find the first direct child that is a block node.
fn findBlockChild(parent: ts.Node, k: *const KindIds) ?ts.Node {
    var i: u32 = 0;
    while (i < parent.childCount()) : (i += 1) {
        const child = parent.child(i) orelse continue;
        if (child.kindId() == k.block) return child;
    }
    return null;
}

/// Process a function_item. Detects modifiers (unsafe, async, const, extern),
/// #[test] attribute, and creates the appropriate node.
fn processFunctionItem(allocator: std.mem.Allocator, io: std.Io, ctx: *const VisitorContext, ts_node: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    const name = ast.getIdentifierName(ctx.source, ts_node, ctx.k) orelse {
        ctx.log.trace(io,"skipping function: no identifier", &.{});
        return;
    };

    const is_test = ast.hasAttribute(ctx.source, ts_node, ctx.k, "test");
    const vis_info = if (is_test)
        ast.VisibilityInfo{ .visibility = .private, .scope = null }
    else
        ast.detectVisibility(ctx.source, ts_node, ctx.k);
    const doc = ast.collectOuterDocComment(ctx.source, ts_node, ctx.k);
    const signature = ast.extractFunctionSignature(ctx.source, ts_node, ctx.k);

    const is_unsafe = ast.hasFunctionModifier(ts_node, ctx.k.kw_unsafe, ctx.k);
    const is_async = ast.hasFunctionModifier(ts_node, ctx.k.kw_async, ctx.k);
    const is_const = ast.hasFunctionModifier(ts_node, ctx.k.kw_const, ctx.k);
    const has_extern = ast.hasExternModifier(ts_node, ctx.k);
    const abi = if (has_extern) ast.extractExternAbi(ctx.source, ts_node, ctx.k) else null;
    const attributes = try extractAndRegisterAttributes(allocator, ctx.g, ctx.source, ts_node, ctx.k);

    const kind: NodeKind = if (is_test) .test_def else .function;
    const block_body = findBlockChild(ts_node, ctx.k);
    const metrics: ?Metrics = if (kind == .function)
        if (block_body) |body| computeFunctionMetrics(body, ctx.k) else null
    else
        null;

    const fn_id = try ctx.g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = kind,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = vis_info.visibility,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .col_start = if (ast.getIdentifierNode(ts_node, ctx.k)) |id| id.startPoint().column else null,
        .col_end = if (ast.getIdentifierNode(ts_node, ctx.k)) |id| id.endPoint().column else null,
        .doc = doc,
        .signature = signature,
        .metrics = metrics,
        .lang_meta = .{ .rust = .{
            .is_unsafe = is_unsafe,
            .is_async = is_async,
            .is_const = is_const,
            .is_extern = has_extern,
            .abi = abi,
            .attributes = attributes,
            .visibility_scope = vis_info.scope,
        } },
    });

    try emitParameterNodes(allocator, io, ctx, ts_node, fn_id);
}

/// Process a function_signature_item (in trait bodies).
fn processFunctionSignatureItem(allocator: std.mem.Allocator, io: std.Io, ctx: *const VisitorContext, ts_node: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    const name = ast.getIdentifierName(ctx.source, ts_node, ctx.k) orelse {
        ctx.log.trace(io,"skipping fn signature: no identifier", &.{});
        return;
    };

    const vis_info = ast.detectVisibility(ctx.source, ts_node, ctx.k);
    const doc = ast.collectOuterDocComment(ctx.source, ts_node, ctx.k);
    const signature = ast.extractFunctionSignature(ctx.source, ts_node, ctx.k);
    const attributes = try extractAndRegisterAttributes(allocator, ctx.g, ctx.source, ts_node, ctx.k);

    const fn_id = try ctx.g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = .function,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = vis_info.visibility,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .col_start = if (ast.getIdentifierNode(ts_node, ctx.k)) |id| id.startPoint().column else null,
        .col_end = if (ast.getIdentifierNode(ts_node, ctx.k)) |id| id.endPoint().column else null,
        .doc = doc,
        .signature = signature,
        .lang_meta = .{ .rust = .{ .sub_kind = .fn_signature, .attributes = attributes, .visibility_scope = vis_info.scope } },
    });

    try emitParameterNodes(allocator, io, ctx, ts_node, fn_id);
}

/// Iterate the `parameters` child of a function and emit a `.parameter`
/// node for each named parameter. Self parameters are skipped.
fn emitParameterNodes(allocator: std.mem.Allocator, io: std.Io, ctx: *const VisitorContext, fn_node: ts.Node, fn_id: NodeId) error{OutOfMemory}!void {
    _ = io;
    var i: u32 = 0;
    while (i < fn_node.childCount()) : (i += 1) {
        const child = fn_node.child(i) orelse continue;
        if (child.kindId() != ctx.k.parameters) continue;

        var j: u32 = 0;
        while (j < child.namedChildCount()) : (j += 1) {
            const param = child.namedChild(j) orelse continue;
            const pk = param.kindId();
            if (pk == ctx.k.self_parameter) continue;
            if (pk != ctx.k.parameter) continue;
            const name_node = param.namedChild(0) orelse continue;
            if (name_node.kindId() != ctx.k.identifier) continue;
            const param_name = ts_api.nodeText(ctx.source, name_node);
            _ = try ctx.g.addNode(allocator, .{
                .id = .root,
                .name = param_name,
                .kind = .parameter,
                .language = .rust,
                .parent_id = fn_id,
                .visibility = .private,
                .line_start = param.startPoint().row + 1,
                .line_end = param.endPoint().row + 1,
            });
        }
        break;
    }
}

/// Process a struct_item.
fn processStructItem(allocator: std.mem.Allocator, io: std.Io, ctx: *const VisitorContext, ts_node: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    const name = ast.getTypeIdentifierName(ctx.source, ts_node, ctx.k) orelse {
        ctx.log.trace(io,"skipping struct: no type_identifier", &.{});
        return;
    };

    const vis_info = ast.detectVisibility(ctx.source, ts_node, ctx.k);
    const doc = ast.collectOuterDocComment(ctx.source, ts_node, ctx.k);
    const derives = ast.extractDerives(ctx.source, ts_node, ctx.k);
    const attributes = try extractAndRegisterAttributes(allocator, ctx.g, ctx.source, ts_node, ctx.k);

    const signature = ast.extractDeclarationSignature(ctx.source, ts_node, ctx.k);

    const node_id = try ctx.g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = .type_def,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = vis_info.visibility,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .col_start = if (ast.getTypeIdentifierNode(ts_node, ctx.k)) |id| id.startPoint().column else null,
        .col_end = if (ast.getTypeIdentifierNode(ts_node, ctx.k)) |id| id.endPoint().column else null,
        .doc = doc,
        .signature = signature,
        .lang_meta = .{ .rust = .{ .derives = derives, .attributes = attributes, .visibility_scope = vis_info.scope } },
    });

    // Recurse into field_declaration_list for fields.
    try recurseIntoBody(allocator, io, ctx, ts_node, node_id);
}

/// Process an enum_item.
fn processEnumItem(allocator: std.mem.Allocator, io: std.Io, ctx: *const VisitorContext, ts_node: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    const name = ast.getTypeIdentifierName(ctx.source, ts_node, ctx.k) orelse {
        ctx.log.trace(io,"skipping enum: no type_identifier", &.{});
        return;
    };

    const vis_info = ast.detectVisibility(ctx.source, ts_node, ctx.k);
    const doc = ast.collectOuterDocComment(ctx.source, ts_node, ctx.k);
    const derives = ast.extractDerives(ctx.source, ts_node, ctx.k);
    const attributes = try extractAndRegisterAttributes(allocator, ctx.g, ctx.source, ts_node, ctx.k);

    const signature = ast.extractDeclarationSignature(ctx.source, ts_node, ctx.k);

    const node_id = try ctx.g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = .enum_def,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = vis_info.visibility,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .col_start = if (ast.getTypeIdentifierNode(ts_node, ctx.k)) |id| id.startPoint().column else null,
        .col_end = if (ast.getTypeIdentifierNode(ts_node, ctx.k)) |id| id.endPoint().column else null,
        .doc = doc,
        .signature = signature,
        .lang_meta = .{ .rust = .{ .derives = derives, .attributes = attributes, .visibility_scope = vis_info.scope } },
    });

    // Recurse into enum_variant_list for variants.
    try recurseIntoBody(allocator, io, ctx, ts_node, node_id);
}

/// Process a union_item.
fn processUnionItem(allocator: std.mem.Allocator, io: std.Io, ctx: *const VisitorContext, ts_node: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    const name = ast.getTypeIdentifierName(ctx.source, ts_node, ctx.k) orelse {
        ctx.log.trace(io,"skipping union: no type_identifier", &.{});
        return;
    };

    const vis_info = ast.detectVisibility(ctx.source, ts_node, ctx.k);
    const doc = ast.collectOuterDocComment(ctx.source, ts_node, ctx.k);
    const derives = ast.extractDerives(ctx.source, ts_node, ctx.k);
    const attributes = try extractAndRegisterAttributes(allocator, ctx.g, ctx.source, ts_node, ctx.k);

    const signature = ast.extractDeclarationSignature(ctx.source, ts_node, ctx.k);

    const node_id = try ctx.g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = .union_def,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = vis_info.visibility,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .col_start = if (ast.getTypeIdentifierNode(ts_node, ctx.k)) |id| id.startPoint().column else null,
        .col_end = if (ast.getTypeIdentifierNode(ts_node, ctx.k)) |id| id.endPoint().column else null,
        .doc = doc,
        .signature = signature,
        .lang_meta = .{ .rust = .{ .derives = derives, .attributes = attributes, .visibility_scope = vis_info.scope } },
    });

    // Recurse into field_declaration_list for fields.
    try recurseIntoBody(allocator, io, ctx, ts_node, node_id);
}

/// Process a trait_item.
fn processTraitItem(allocator: std.mem.Allocator, io: std.Io, ctx: *const VisitorContext, ts_node: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    const name = ast.getTypeIdentifierName(ctx.source, ts_node, ctx.k) orelse {
        ctx.log.trace(io,"skipping trait: no type_identifier", &.{});
        return;
    };

    const vis_info = ast.detectVisibility(ctx.source, ts_node, ctx.k);
    const doc = ast.collectOuterDocComment(ctx.source, ts_node, ctx.k);
    const attributes = try extractAndRegisterAttributes(allocator, ctx.g, ctx.source, ts_node, ctx.k);

    const signature = ast.extractDeclarationSignature(ctx.source, ts_node, ctx.k);

    const node_id = try ctx.g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = .type_def,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = vis_info.visibility,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .col_start = if (ast.getTypeIdentifierNode(ts_node, ctx.k)) |id| id.startPoint().column else null,
        .col_end = if (ast.getTypeIdentifierNode(ts_node, ctx.k)) |id| id.endPoint().column else null,
        .doc = doc,
        .signature = signature,
        .lang_meta = .{ .rust = .{ .sub_kind = .trait_, .attributes = attributes, .visibility_scope = vis_info.scope } },
    });

    // Recurse into declaration_list for trait methods.
    try recurseIntoBody(allocator, io, ctx, ts_node, node_id);
}

/// Process an impl_item. Creates a type_def node with sub_kind=.impl_block.
fn processImplItem(allocator: std.mem.Allocator, io: std.Io, ctx: *const VisitorContext, ts_node: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    const impl_info = ast.getImplInfo(ctx.source, ts_node, ctx.k) orelse {
        ctx.log.trace(io,"skipping impl: cannot determine target", &.{});
        return;
    };

    const name = impl_info.type_name;
    const vis_info = ast.detectVisibility(ctx.source, ts_node, ctx.k);
    const doc = ast.collectOuterDocComment(ctx.source, ts_node, ctx.k);
    const signature = ast.extractDeclarationSignature(ctx.source, ts_node, ctx.k);

    const node_id = try ctx.g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = .type_def,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = vis_info.visibility,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .doc = doc,
        .lang_meta = .{ .rust = .{ .sub_kind = .impl_block, .visibility_scope = vis_info.scope } },
        .signature = signature,
    });

    // Recurse into declaration_list for impl methods.
    try recurseIntoBody(allocator, io, ctx, ts_node, node_id);
}

/// Process a const_item.
fn processConstItem(allocator: std.mem.Allocator, io: std.Io, ctx: *const VisitorContext, ts_node: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    const name = ast.getIdentifierName(ctx.source, ts_node, ctx.k) orelse {
        ctx.log.trace(io,"skipping const: no identifier", &.{});
        return;
    };

    const vis_info = ast.detectVisibility(ctx.source, ts_node, ctx.k);
    const doc = ast.collectOuterDocComment(ctx.source, ts_node, ctx.k);
    const attributes = try extractAndRegisterAttributes(allocator, ctx.g, ctx.source, ts_node, ctx.k);

    _ = try ctx.g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = .constant,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = vis_info.visibility,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .col_start = if (ast.getIdentifierNode(ts_node, ctx.k)) |id| id.startPoint().column else null,
        .col_end = if (ast.getIdentifierNode(ts_node, ctx.k)) |id| id.endPoint().column else null,
        .doc = doc,
        .lang_meta = .{ .rust = .{ .attributes = attributes, .visibility_scope = vis_info.scope } },
    });
}

/// Process a static_item.
fn processStaticItem(allocator: std.mem.Allocator, io: std.Io, ctx: *const VisitorContext, ts_node: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    const name = ast.getIdentifierName(ctx.source, ts_node, ctx.k) orelse {
        ctx.log.trace(io,"skipping static: no identifier", &.{});
        return;
    };

    const vis_info = ast.detectVisibility(ctx.source, ts_node, ctx.k);
    const doc = ast.collectOuterDocComment(ctx.source, ts_node, ctx.k);
    const attributes = try extractAndRegisterAttributes(allocator, ctx.g, ctx.source, ts_node, ctx.k);

    _ = try ctx.g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = .constant,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = vis_info.visibility,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .col_start = if (ast.getIdentifierNode(ts_node, ctx.k)) |id| id.startPoint().column else null,
        .col_end = if (ast.getIdentifierNode(ts_node, ctx.k)) |id| id.endPoint().column else null,
        .doc = doc,
        .lang_meta = .{ .rust = .{ .sub_kind = .static_item, .attributes = attributes, .visibility_scope = vis_info.scope } },
    });
}

/// Process a type_item (type alias).
fn processTypeItem(allocator: std.mem.Allocator, io: std.Io, ctx: *const VisitorContext, ts_node: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    const name = ast.getTypeIdentifierName(ctx.source, ts_node, ctx.k) orelse {
        ctx.log.trace(io,"skipping type alias: no type_identifier", &.{});
        return;
    };

    const vis_info = ast.detectVisibility(ctx.source, ts_node, ctx.k);
    const doc = ast.collectOuterDocComment(ctx.source, ts_node, ctx.k);
    const attributes = try extractAndRegisterAttributes(allocator, ctx.g, ctx.source, ts_node, ctx.k);
    const signature = ast.extractDeclarationSignature(ctx.source, ts_node, ctx.k);

    _ = try ctx.g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = .type_def,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = vis_info.visibility,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .col_start = if (ast.getTypeIdentifierNode(ts_node, ctx.k)) |id| id.startPoint().column else null,
        .col_end = if (ast.getTypeIdentifierNode(ts_node, ctx.k)) |id| id.endPoint().column else null,
        .doc = doc,
        .signature = signature,
        .lang_meta = .{ .rust = .{ .sub_kind = .type_alias, .attributes = attributes, .visibility_scope = vis_info.scope } },
    });
}

/// Process a macro_definition (macro_rules!). Checks for #[macro_export]
/// which makes the macro crate-public in Rust.
fn processMacroDefinition(allocator: std.mem.Allocator, io: std.Io, ctx: *const VisitorContext, ts_node: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    const name = ast.getIdentifierName(ctx.source, ts_node, ctx.k) orelse {
        ctx.log.trace(io,"skipping macro: no identifier", &.{});
        return;
    };

    const has_macro_export = ast.hasAttribute(ctx.source, ts_node, ctx.k, "macro_export");
    const vis_info = if (has_macro_export)
        ast.VisibilityInfo{ .visibility = .public, .scope = null }
    else
        ast.detectVisibility(ctx.source, ts_node, ctx.k);
    const doc = ast.collectOuterDocComment(ctx.source, ts_node, ctx.k);
    const attributes = try extractAndRegisterAttributes(allocator, ctx.g, ctx.source, ts_node, ctx.k);

    _ = try ctx.g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = .function,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = vis_info.visibility,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .col_start = if (ast.getIdentifierNode(ts_node, ctx.k)) |id| id.startPoint().column else null,
        .col_end = if (ast.getIdentifierNode(ts_node, ctx.k)) |id| id.endPoint().column else null,
        .doc = doc,
        .lang_meta = .{ .rust = .{ .sub_kind = .macro_rules, .attributes = attributes, .visibility_scope = vis_info.scope } },
    });
}

/// Process a mod_item. Inline modules (with declaration_list) create a module
/// node with recursion into the body. External modules (mod foo;) create an
/// import_decl node.
fn processModItem(allocator: std.mem.Allocator, io: std.Io, ctx: *const VisitorContext, ts_node: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    const name = ast.getIdentifierName(ctx.source, ts_node, ctx.k) orelse {
        ctx.log.trace(io,"skipping mod: no identifier", &.{});
        return;
    };

    const vis_info = ast.detectVisibility(ctx.source, ts_node, ctx.k);
    const doc = ast.collectOuterDocComment(ctx.source, ts_node, ctx.k);
    const attributes = try extractAndRegisterAttributes(allocator, ctx.g, ctx.source, ts_node, ctx.k);

    if (ast.isInlineMod(ts_node, ctx.k)) {
        const inner_attrs = ast.collectInnerAttributesFromMod(ctx.source, ts_node, ctx.k);
        const node_id = try ctx.g.addNode(allocator, .{
            .id = .root,
            .name = name,
            .kind = .module,
            .language = .rust,
            .parent_id = parent_id,
            .visibility = vis_info.visibility,
            .line_start = ts_node.startPoint().row + 1,
            .line_end = ts_node.endPoint().row + 1,
            .doc = doc,
            .lang_meta = .{ .rust = .{ .attributes = attributes, .inner_attributes = inner_attrs, .visibility_scope = vis_info.scope } },
        });
        try recurseIntoBody(allocator, io, ctx, ts_node, node_id);
    } else {
        _ = try ctx.g.addNode(allocator, .{
            .id = .root,
            .name = name,
            .kind = .import_decl,
            .language = .rust,
            .parent_id = parent_id,
            .visibility = vis_info.visibility,
            .line_start = ts_node.startPoint().row + 1,
            .line_end = ts_node.endPoint().row + 1,
            .doc = doc,
            .signature = name,
            .lang_meta = .{ .rust = .{ .attributes = attributes, .visibility_scope = vis_info.scope } },
        });
    }
}

/// Process a use_declaration.
fn processUseDeclaration(allocator: std.mem.Allocator, io: std.Io, ctx: *const VisitorContext, ts_node: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    _ = io;
    const start = ts_node.startByte();
    const end = ts_node.endByte();
    const text = ctx.source[start..end];
    const signature = std.mem.trimEnd(u8, text, " \t\n\r;");
    const vis_info = ast.detectVisibility(ctx.source, ts_node, ctx.k);
    const doc = ast.collectOuterDocComment(ctx.source, ts_node, ctx.k);
    const attributes = try extractAndRegisterAttributes(allocator, ctx.g, ctx.source, ts_node, ctx.k);

    _ = try ctx.g.addNode(allocator, .{
        .id = .root,
        .name = signature,
        .kind = .import_decl,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = vis_info.visibility,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .doc = doc,
        .signature = signature,
        .lang_meta = .{ .rust = .{ .attributes = attributes, .visibility_scope = vis_info.scope } },
    });
}

/// Process a field_declaration (inside struct or union).
fn processFieldDeclaration(allocator: std.mem.Allocator, io: std.Io, ctx: *const VisitorContext, ts_node: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    _ = io;
    var name: ?[]const u8 = null;
    var i: u32 = 0;
    while (i < ts_node.childCount()) : (i += 1) {
        const child = ts_node.child(i) orelse continue;
        if (child.kindId() == ctx.k.field_identifier) {
            name = ts_api.nodeText(ctx.source, child);
            break;
        }
    }

    if (name) |n| {
        const vis_info = ast.detectVisibility(ctx.source, ts_node, ctx.k);
        const doc = ast.collectOuterDocComment(ctx.source, ts_node, ctx.k);
        const attributes = try extractAndRegisterAttributes(allocator, ctx.g, ctx.source, ts_node, ctx.k);
        _ = try ctx.g.addNode(allocator, .{
            .id = .root,
            .name = n,
            .kind = .field,
            .language = .rust,
            .parent_id = parent_id,
            .visibility = vis_info.visibility,
            .line_start = ts_node.startPoint().row + 1,
            .line_end = ts_node.endPoint().row + 1,
            .doc = doc,
            .lang_meta = .{ .rust = .{ .attributes = attributes, .visibility_scope = vis_info.scope } },
        });
    }
}

/// Process an enum_variant. Inherits the parent enum's visibility since
/// Rust enum variants share their enum's access level.
/// Recurses into struct variants (named fields) and tuple variants
/// (positional fields) when present.
fn processEnumVariant(allocator: std.mem.Allocator, io: std.Io, ctx: *const VisitorContext, ts_node: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    const name = ast.getIdentifierName(ctx.source, ts_node, ctx.k) orelse return;
    const doc = ast.collectOuterDocComment(ctx.source, ts_node, ctx.k);
    const attributes = try extractAndRegisterAttributes(allocator, ctx.g, ctx.source, ts_node, ctx.k);

    const parent_vis = if (ctx.g.getNode(parent_id)) |p| p.visibility else .private;

    const variant_id = try ctx.g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = .field,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = parent_vis,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .col_start = if (ast.getIdentifierNode(ts_node, ctx.k)) |id| id.startPoint().column else null,
        .col_end = if (ast.getIdentifierNode(ts_node, ctx.k)) |id| id.endPoint().column else null,
        .doc = doc,
        .lang_meta = .{ .rust = .{ .attributes = attributes } },
    });

    var i: u32 = 0;
    while (i < ts_node.childCount()) : (i += 1) {
        const child = ts_node.child(i) orelse continue;
        const kid = child.kindId();
        if (kid == ctx.k.field_declaration_list) {
            var j: u32 = 0;
            while (j < child.childCount()) : (j += 1) {
                const decl = child.child(j) orelse continue;
                if (!decl.isNamed()) continue;
                if (decl.kindId() == ctx.k.field_declaration) {
                    try processFieldDeclaration(allocator, io, ctx, decl, variant_id);
                }
            }
        } else if (kid == ctx.k.ordered_field_declaration_list) {
            try processTupleFields(allocator, io, ctx, child, variant_id);
        }
    }
}

/// Process an associated_type declaration inside a trait body.
/// Inherits the parent trait's visibility.
fn processAssociatedType(allocator: std.mem.Allocator, io: std.Io, ctx: *const VisitorContext, ts_node: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    _ = io;
    const name = ast.getTypeIdentifierName(ctx.source, ts_node, ctx.k) orelse return;
    const doc = ast.collectOuterDocComment(ctx.source, ts_node, ctx.k);
    const attributes = try extractAndRegisterAttributes(allocator, ctx.g, ctx.source, ts_node, ctx.k);

    const parent_vis = if (ctx.g.getNode(parent_id)) |p| p.visibility else .private;

    _ = try ctx.g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = .type_def,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = parent_vis,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .col_start = if (ast.getTypeIdentifierNode(ts_node, ctx.k)) |id| id.startPoint().column else null,
        .col_end = if (ast.getTypeIdentifierNode(ts_node, ctx.k)) |id| id.endPoint().column else null,
        .doc = doc,
        .lang_meta = .{ .rust = .{ .sub_kind = .associated_type, .attributes = attributes } },
    });
}

/// Recurse into the body of a container (struct fields, enum variants,
/// impl methods, trait methods, inline mod declarations).
fn recurseIntoBody(allocator: std.mem.Allocator, io: std.Io, ctx: *const VisitorContext, ts_node: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    var i: u32 = 0;
    while (i < ts_node.childCount()) : (i += 1) {
        const child = ts_node.child(i) orelse continue;
        const kid = child.kindId();

        if (kid == ctx.k.declaration_list or kid == ctx.k.field_declaration_list or kid == ctx.k.enum_variant_list) {
            var j: u32 = 0;
            while (j < child.childCount()) : (j += 1) {
                const decl = child.child(j) orelse continue;
                if (!decl.isNamed()) continue;
                try processDeclaration(allocator, io, ctx, decl, parent_id);
            }
        } else if (kid == ctx.k.ordered_field_declaration_list) {
            try processTupleFields(allocator, io, ctx, child, parent_id);
        }
    }
}

/// Positional names for tuple struct fields.
const tuple_field_names = [_][]const u8{
    "0", "1", "2",  "3",  "4",  "5",  "6",  "7",
    "8", "9", "10", "11", "12", "13", "14", "15",
};

/// Process tuple struct fields from an ordered_field_declaration_list.
/// Tuple fields are positional, so they get names "0", "1", etc.
/// Attributes on fields are accumulated and attached to the field's lang_meta.
fn processTupleFields(allocator: std.mem.Allocator, io: std.Io, ctx: *const VisitorContext, list_node: ts.Node, parent_id: NodeId) error{OutOfMemory}!void {
    _ = io;
    var field_index: usize = 0;
    var pending_vis: Visibility = .private;
    var pending_attrs_start: ?u32 = null;
    var pending_attrs_end: u32 = 0;

    var i: u32 = 0;
    while (i < list_node.childCount()) : (i += 1) {
        const child = list_node.child(i) orelse continue;
        if (!child.isNamed()) continue;
        const kid = child.kindId();

        if (kid == ctx.k.visibility_modifier) {
            pending_vis = .public;
            continue;
        }

        if (kid == ctx.k.attribute_item) {
            if (pending_attrs_start == null) pending_attrs_start = child.startByte();
            pending_attrs_end = child.endByte();
            continue;
        }

        if (field_index >= tuple_field_names.len) break;

        const attrs: ?[]const u8 = if (pending_attrs_start) |start|
            ctx.source[start..pending_attrs_end]
        else
            null;

        _ = try ctx.g.addNode(allocator, .{
            .id = .root,
            .name = tuple_field_names[field_index],
            .kind = .field,
            .language = .rust,
            .parent_id = parent_id,
            .visibility = pending_vis,
            .line_start = child.startPoint().row + 1,
            .line_end = child.endPoint().row + 1,
            .signature = ts_api.nodeText(ctx.source, child),
            .lang_meta = if (attrs != null) .{ .rust = .{ .attributes = attrs } } else .{ .none = {} },
        });
        field_index += 1;
        pending_vis = .private;
        pending_attrs_start = null;
    }
}
