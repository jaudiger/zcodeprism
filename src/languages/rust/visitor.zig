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
const rust_meta = @import("meta.zig");
const phantom_mod = @import("../../core/phantom.zig");

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

/// Parse Rust source code and populate the graph with nodes and edges.
/// This is the entry point used by the LanguageSupport registry.
///
/// `source` - raw Rust source bytes to parse (owned by caller).
/// `g` - the graph to populate; a file node is always added as the first node.
/// `file_path` - relative path within the project, used for cross-file import
///   resolution. When null, import resolution falls back to basename-only lookup.
/// `logger` - structured logger; pass Logger.noop for silent operation.
pub fn parse(allocator: std.mem.Allocator, source: []const u8, g: *Graph, file_path: ?[]const u8, logger: Logger) anyerror!void {
    const log = logger.withScope("rust-visitor");

    log.debug("parsing source", &.{Field.uint("bytes", source.len)});

    const line_count = ts_api.countLines(source);
    const ts_lang = ts_api.tree_sitter_rust();
    const k = KindIds.init(ts_lang);

    const tree = ts_api.parseSource(ts_lang, source) orelse {
        log.warn("tree-sitter parse failed", &.{});
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

    // -- Node creation --
    // Walk top-level declarations and recursively create graph nodes.
    var i: u32 = 0;
    while (i < root.childCount()) : (i += 1) {
        const child = root.child(i) orelse continue;
        if (!child.isNamed()) continue;
        try processDeclaration(allocator, g, source, child, file_id, &k, log);
    }
}

/// Re-parse source and emit cross-file edges for the Rust file at file_idx.
pub fn buildEdges(allocator: std.mem.Allocator, source: []const u8, g: *Graph, file_idx: usize, scope_end: usize, file_path: ?[]const u8, graph_index: *const GraphIndex, phantom_mgr: *const PhantomManager, logger: Logger) anyerror!void {
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

    try cf.buildImportMap(allocator, g, source, root, &ctx, graph_index, file_path, &k, log);

    log.debug("building edges", &.{});
    try eb.walkForEdges(allocator, g, source, root, &k, &ctx, graph_index, phantom_mgr, log);
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
fn processDeclaration(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
    const kid = ts_node.kindId();

    if (kid == k.function_item) {
        try processFunctionItem(allocator, g, source, ts_node, parent_id, k, log);
    } else if (kid == k.function_signature_item) {
        try processFunctionSignatureItem(allocator, g, source, ts_node, parent_id, k, log);
    } else if (kid == k.struct_item) {
        try processStructItem(allocator, g, source, ts_node, parent_id, k, log);
    } else if (kid == k.enum_item) {
        try processEnumItem(allocator, g, source, ts_node, parent_id, k, log);
    } else if (kid == k.union_item) {
        try processUnionItem(allocator, g, source, ts_node, parent_id, k, log);
    } else if (kid == k.trait_item) {
        try processTraitItem(allocator, g, source, ts_node, parent_id, k, log);
    } else if (kid == k.impl_item) {
        try processImplItem(allocator, g, source, ts_node, parent_id, k, log);
    } else if (kid == k.const_item) {
        try processConstItem(allocator, g, source, ts_node, parent_id, k, log);
    } else if (kid == k.static_item) {
        try processStaticItem(allocator, g, source, ts_node, parent_id, k, log);
    } else if (kid == k.type_item) {
        try processTypeItem(allocator, g, source, ts_node, parent_id, k, log);
    } else if (kid == k.macro_definition) {
        try processMacroDefinition(allocator, g, source, ts_node, parent_id, k, log);
    } else if (kid == k.mod_item) {
        try processModItem(allocator, g, source, ts_node, parent_id, k, log);
    } else if (kid == k.use_declaration) {
        try processUseDeclaration(allocator, g, source, ts_node, parent_id, k, log);
    } else if (kid == k.field_declaration) {
        try processFieldDeclaration(allocator, g, source, ts_node, parent_id, k, log);
    } else if (kid == k.enum_variant) {
        try processEnumVariant(allocator, g, source, ts_node, parent_id, k, log);
    } else if (kid == k.associated_type) {
        try processAssociatedType(allocator, g, source, ts_node, parent_id, k);
    }
}

/// Process a function_item. Detects modifiers (unsafe, async, const, extern),
/// #[test] attribute, and creates the appropriate node.
fn processFunctionItem(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
    const name = ast.getIdentifierName(source, ts_node, k) orelse {
        log.trace("skipping function: no identifier", &.{});
        return;
    };

    const is_test = ast.hasAttribute(source, ts_node, k, "test");
    const vis_info = if (is_test)
        ast.VisibilityInfo{ .visibility = .private, .scope = null }
    else
        ast.detectVisibility(source, ts_node, k);
    const doc = ast.collectOuterDocComment(source, ts_node, k);
    const signature = ast.extractFunctionSignature(source, ts_node, k);

    const is_unsafe = ast.hasFunctionModifier(ts_node, k.kw_unsafe, k);
    const is_async = ast.hasFunctionModifier(ts_node, k.kw_async, k);
    const is_const = ast.hasFunctionModifier(ts_node, k.kw_const, k);
    const has_extern = ast.hasExternModifier(ts_node, k);
    const abi = if (has_extern) ast.extractExternAbi(source, ts_node, k) else null;
    const attributes = try extractAndRegisterAttributes(allocator, g, source, ts_node, k);

    const kind: NodeKind = if (is_test) .test_def else .function;

    _ = try g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = kind,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = vis_info.visibility,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .doc = doc,
        .signature = signature,
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
}

/// Process a function_signature_item (in trait bodies).
fn processFunctionSignatureItem(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
    const name = ast.getIdentifierName(source, ts_node, k) orelse {
        log.trace("skipping fn signature: no identifier", &.{});
        return;
    };

    const vis_info = ast.detectVisibility(source, ts_node, k);
    const doc = ast.collectOuterDocComment(source, ts_node, k);
    const signature = ast.extractFunctionSignature(source, ts_node, k);
    const attributes = try extractAndRegisterAttributes(allocator, g, source, ts_node, k);

    _ = try g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = .function,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = vis_info.visibility,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .doc = doc,
        .signature = signature,
        .lang_meta = .{ .rust = .{ .sub_kind = .fn_signature, .attributes = attributes, .visibility_scope = vis_info.scope } },
    });
}

/// Process a struct_item.
fn processStructItem(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
    const name = ast.getTypeIdentifierName(source, ts_node, k) orelse {
        log.trace("skipping struct: no type_identifier", &.{});
        return;
    };

    const vis_info = ast.detectVisibility(source, ts_node, k);
    const doc = ast.collectOuterDocComment(source, ts_node, k);
    const derives = ast.extractDerives(source, ts_node, k);
    const attributes = try extractAndRegisterAttributes(allocator, g, source, ts_node, k);

    const signature = ast.extractDeclarationSignature(source, ts_node, k);

    const node_id = try g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = .type_def,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = vis_info.visibility,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .doc = doc,
        .signature = signature,
        .lang_meta = .{ .rust = .{ .derives = derives, .attributes = attributes, .visibility_scope = vis_info.scope } },
    });

    // Recurse into field_declaration_list for fields.
    try recurseIntoBody(allocator, g, source, ts_node, node_id, k, log);
}

/// Process an enum_item.
fn processEnumItem(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
    const name = ast.getTypeIdentifierName(source, ts_node, k) orelse {
        log.trace("skipping enum: no type_identifier", &.{});
        return;
    };

    const vis_info = ast.detectVisibility(source, ts_node, k);
    const doc = ast.collectOuterDocComment(source, ts_node, k);
    const derives = ast.extractDerives(source, ts_node, k);
    const attributes = try extractAndRegisterAttributes(allocator, g, source, ts_node, k);

    const signature = ast.extractDeclarationSignature(source, ts_node, k);

    const node_id = try g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = .enum_def,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = vis_info.visibility,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .doc = doc,
        .signature = signature,
        .lang_meta = .{ .rust = .{ .derives = derives, .attributes = attributes, .visibility_scope = vis_info.scope } },
    });

    // Recurse into enum_variant_list for variants.
    try recurseIntoBody(allocator, g, source, ts_node, node_id, k, log);
}

/// Process a union_item.
fn processUnionItem(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
    const name = ast.getTypeIdentifierName(source, ts_node, k) orelse {
        log.trace("skipping union: no type_identifier", &.{});
        return;
    };

    const vis_info = ast.detectVisibility(source, ts_node, k);
    const doc = ast.collectOuterDocComment(source, ts_node, k);
    const derives = ast.extractDerives(source, ts_node, k);
    const attributes = try extractAndRegisterAttributes(allocator, g, source, ts_node, k);

    const signature = ast.extractDeclarationSignature(source, ts_node, k);

    const node_id = try g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = .union_def,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = vis_info.visibility,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .doc = doc,
        .signature = signature,
        .lang_meta = .{ .rust = .{ .derives = derives, .attributes = attributes, .visibility_scope = vis_info.scope } },
    });

    // Recurse into field_declaration_list for fields.
    try recurseIntoBody(allocator, g, source, ts_node, node_id, k, log);
}

/// Process a trait_item.
fn processTraitItem(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
    const name = ast.getTypeIdentifierName(source, ts_node, k) orelse {
        log.trace("skipping trait: no type_identifier", &.{});
        return;
    };

    const vis_info = ast.detectVisibility(source, ts_node, k);
    const doc = ast.collectOuterDocComment(source, ts_node, k);
    const attributes = try extractAndRegisterAttributes(allocator, g, source, ts_node, k);

    const signature = ast.extractDeclarationSignature(source, ts_node, k);

    const node_id = try g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = .type_def,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = vis_info.visibility,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .doc = doc,
        .signature = signature,
        .lang_meta = .{ .rust = .{ .sub_kind = .trait_, .attributes = attributes, .visibility_scope = vis_info.scope } },
    });

    // Recurse into declaration_list for trait methods.
    try recurseIntoBody(allocator, g, source, ts_node, node_id, k, log);
}

/// Process an impl_item. Creates a type_def node with sub_kind=.impl_block.
fn processImplItem(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
    const impl_info = ast.getImplInfo(source, ts_node, k) orelse {
        log.trace("skipping impl: cannot determine target", &.{});
        return;
    };

    const name = impl_info.type_name;
    const vis_info = ast.detectVisibility(source, ts_node, k);
    const doc = ast.collectOuterDocComment(source, ts_node, k);
    const signature = ast.extractDeclarationSignature(source, ts_node, k);

    const node_id = try g.addNode(allocator, .{
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
    try recurseIntoBody(allocator, g, source, ts_node, node_id, k, log);
}

/// Process a const_item.
fn processConstItem(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
    const name = ast.getIdentifierName(source, ts_node, k) orelse {
        log.trace("skipping const: no identifier", &.{});
        return;
    };

    const vis_info = ast.detectVisibility(source, ts_node, k);
    const doc = ast.collectOuterDocComment(source, ts_node, k);
    const attributes = try extractAndRegisterAttributes(allocator, g, source, ts_node, k);

    _ = try g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = .constant,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = vis_info.visibility,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .doc = doc,
        .lang_meta = .{ .rust = .{ .attributes = attributes, .visibility_scope = vis_info.scope } },
    });
}

/// Process a static_item.
fn processStaticItem(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
    const name = ast.getIdentifierName(source, ts_node, k) orelse {
        log.trace("skipping static: no identifier", &.{});
        return;
    };

    const vis_info = ast.detectVisibility(source, ts_node, k);
    const doc = ast.collectOuterDocComment(source, ts_node, k);
    const attributes = try extractAndRegisterAttributes(allocator, g, source, ts_node, k);

    _ = try g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = .constant,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = vis_info.visibility,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .doc = doc,
        .lang_meta = .{ .rust = .{ .sub_kind = .static_item, .attributes = attributes, .visibility_scope = vis_info.scope } },
    });
}

/// Process a type_item (type alias).
fn processTypeItem(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
    const name = ast.getTypeIdentifierName(source, ts_node, k) orelse {
        log.trace("skipping type alias: no type_identifier", &.{});
        return;
    };

    const vis_info = ast.detectVisibility(source, ts_node, k);
    const doc = ast.collectOuterDocComment(source, ts_node, k);
    const attributes = try extractAndRegisterAttributes(allocator, g, source, ts_node, k);
    const signature = ast.extractDeclarationSignature(source, ts_node, k);

    _ = try g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = .type_def,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = vis_info.visibility,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .doc = doc,
        .signature = signature,
        .lang_meta = .{ .rust = .{ .sub_kind = .type_alias, .attributes = attributes, .visibility_scope = vis_info.scope } },
    });
}

/// Process a macro_definition (macro_rules!). Checks for #[macro_export]
/// which makes the macro crate-public in Rust.
fn processMacroDefinition(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
    const name = ast.getIdentifierName(source, ts_node, k) orelse {
        log.trace("skipping macro: no identifier", &.{});
        return;
    };

    const has_macro_export = ast.hasAttribute(source, ts_node, k, "macro_export");
    const vis_info = if (has_macro_export)
        ast.VisibilityInfo{ .visibility = .public, .scope = null }
    else
        ast.detectVisibility(source, ts_node, k);
    const doc = ast.collectOuterDocComment(source, ts_node, k);
    const attributes = try extractAndRegisterAttributes(allocator, g, source, ts_node, k);

    _ = try g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = .function,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = vis_info.visibility,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .doc = doc,
        .lang_meta = .{ .rust = .{ .sub_kind = .macro_rules, .attributes = attributes, .visibility_scope = vis_info.scope } },
    });
}

/// Process a mod_item. Inline modules (with declaration_list) create a module
/// node with recursion into the body. External modules (mod foo;) create an
/// import_decl node.
fn processModItem(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
    const name = ast.getIdentifierName(source, ts_node, k) orelse {
        log.trace("skipping mod: no identifier", &.{});
        return;
    };

    const vis_info = ast.detectVisibility(source, ts_node, k);
    const doc = ast.collectOuterDocComment(source, ts_node, k);
    const attributes = try extractAndRegisterAttributes(allocator, g, source, ts_node, k);

    if (ast.isInlineMod(ts_node, k)) {
        const inner_attrs = ast.collectInnerAttributesFromMod(source, ts_node, k);
        const node_id = try g.addNode(allocator, .{
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
        try recurseIntoBody(allocator, g, source, ts_node, node_id, k, log);
    } else {
        _ = try g.addNode(allocator, .{
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
fn processUseDeclaration(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, _: Logger) anyerror!void {
    const start = ts_node.startByte();
    const end = ts_node.endByte();
    const text = source[start..end];
    const signature = std.mem.trimRight(u8, text, " \t\n\r;");
    const vis_info = ast.detectVisibility(source, ts_node, k);
    const doc = ast.collectOuterDocComment(source, ts_node, k);
    const attributes = try extractAndRegisterAttributes(allocator, g, source, ts_node, k);

    _ = try g.addNode(allocator, .{
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
fn processFieldDeclaration(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, _: Logger) anyerror!void {
    var name: ?[]const u8 = null;
    var i: u32 = 0;
    while (i < ts_node.childCount()) : (i += 1) {
        const child = ts_node.child(i) orelse continue;
        if (child.kindId() == k.field_identifier) {
            name = ts_api.nodeText(source, child);
            break;
        }
    }

    if (name) |n| {
        const vis_info = ast.detectVisibility(source, ts_node, k);
        const doc = ast.collectOuterDocComment(source, ts_node, k);
        const attributes = try extractAndRegisterAttributes(allocator, g, source, ts_node, k);
        _ = try g.addNode(allocator, .{
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
fn processEnumVariant(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
    const name = ast.getIdentifierName(source, ts_node, k) orelse return;
    const doc = ast.collectOuterDocComment(source, ts_node, k);
    const attributes = try extractAndRegisterAttributes(allocator, g, source, ts_node, k);

    const parent_vis = if (g.getNode(parent_id)) |p| p.visibility else .private;

    const variant_id = try g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = .field,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = parent_vis,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .doc = doc,
        .lang_meta = .{ .rust = .{ .attributes = attributes } },
    });

    var i: u32 = 0;
    while (i < ts_node.childCount()) : (i += 1) {
        const child = ts_node.child(i) orelse continue;
        const kid = child.kindId();
        if (kid == k.field_declaration_list) {
            var j: u32 = 0;
            while (j < child.childCount()) : (j += 1) {
                const decl = child.child(j) orelse continue;
                if (!decl.isNamed()) continue;
                if (decl.kindId() == k.field_declaration) {
                    try processFieldDeclaration(allocator, g, source, decl, variant_id, k, log);
                }
            }
        } else if (kid == k.ordered_field_declaration_list) {
            try processTupleFields(allocator, g, source, child, variant_id, k);
        }
    }
}

/// Process an associated_type declaration inside a trait body.
/// Inherits the parent trait's visibility.
fn processAssociatedType(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds) anyerror!void {
    const name = ast.getTypeIdentifierName(source, ts_node, k) orelse return;
    const doc = ast.collectOuterDocComment(source, ts_node, k);
    const attributes = try extractAndRegisterAttributes(allocator, g, source, ts_node, k);

    const parent_vis = if (g.getNode(parent_id)) |p| p.visibility else .private;

    _ = try g.addNode(allocator, .{
        .id = .root,
        .name = name,
        .kind = .type_def,
        .language = .rust,
        .parent_id = parent_id,
        .visibility = parent_vis,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .doc = doc,
        .lang_meta = .{ .rust = .{ .sub_kind = .associated_type, .attributes = attributes } },
    });
}

/// Recurse into the body of a container (struct fields, enum variants,
/// impl methods, trait methods, inline mod declarations).
fn recurseIntoBody(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId, k: *const KindIds, log: Logger) anyerror!void {
    var i: u32 = 0;
    while (i < ts_node.childCount()) : (i += 1) {
        const child = ts_node.child(i) orelse continue;
        const kid = child.kindId();

        if (kid == k.declaration_list or kid == k.field_declaration_list or kid == k.enum_variant_list) {
            var j: u32 = 0;
            while (j < child.childCount()) : (j += 1) {
                const decl = child.child(j) orelse continue;
                if (!decl.isNamed()) continue;
                try processDeclaration(allocator, g, source, decl, parent_id, k, log);
            }
        } else if (kid == k.ordered_field_declaration_list) {
            try processTupleFields(allocator, g, source, child, parent_id, k);
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
fn processTupleFields(allocator: std.mem.Allocator, g: *Graph, source: []const u8, list_node: ts.Node, parent_id: NodeId, k: *const KindIds) anyerror!void {
    var field_index: usize = 0;
    var pending_vis: Visibility = .private;
    var pending_attrs_start: ?u32 = null;
    var pending_attrs_end: u32 = 0;

    var i: u32 = 0;
    while (i < list_node.childCount()) : (i += 1) {
        const child = list_node.child(i) orelse continue;
        if (!child.isNamed()) continue;
        const kid = child.kindId();

        if (kid == k.visibility_modifier) {
            pending_vis = .public;
            continue;
        }

        if (kid == k.attribute_item) {
            if (pending_attrs_start == null) pending_attrs_start = child.startByte();
            pending_attrs_end = child.endByte();
            continue;
        }

        if (field_index >= tuple_field_names.len) break;

        const attrs: ?[]const u8 = if (pending_attrs_start) |start|
            source[start..pending_attrs_end]
        else
            null;

        _ = try g.addNode(allocator, .{
            .id = .root,
            .name = tuple_field_names[field_index],
            .kind = .field,
            .language = .rust,
            .parent_id = parent_id,
            .visibility = pending_vis,
            .line_start = child.startPoint().row + 1,
            .line_end = child.endPoint().row + 1,
            .signature = ts_api.nodeText(source, child),
            .lang_meta = if (attrs != null) .{ .rust = .{ .attributes = attrs } } else .{ .none = {} },
        });
        field_index += 1;
        pending_vis = .private;
        pending_attrs_start = null;
    }
}
