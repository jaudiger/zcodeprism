const std = @import("std");
const graph_mod = @import("../../core/graph.zig");
const node_mod = @import("../../core/node.zig");
const types = @import("../../core/types.zig");
const ts = @import("tree-sitter");
const ts_api = @import("../../parser/tree_sitter_api.zig");
const ast = @import("ast_analysis.zig");
const cf = @import("cross_file.zig");
const source_scan = @import("../../parser/source_scan.zig");
const pc = @import("parse_context.zig");
const logging = @import("../../logging.zig");
const rust_meta = @import("meta.zig");
const phantom_mod = @import("../../core/phantom.zig");
const shared_resolve = @import("../shared/resolve.zig");
const shared_lookup = @import("../shared/lookup.zig");
const type_env_mod = @import("../shared/type_env.zig");
const worklist_mod = @import("../../lsp/worklist.zig");

const LspWorklist = worklist_mod.LspWorklist;

const Field = logging.Field;
const KindIds = pc.KindIds;
const ScopeIndex = @import("../../core/scope_index.zig").ScopeIndex;
const GraphIndex = @import("../../core/graph_index.zig").GraphIndex;
const Graph = graph_mod.Graph;
const Node = node_mod.Node;
const NodeId = types.NodeId;
const NodeKind = types.NodeKind;
const Logger = logging.Logger;
const RustSubKind = rust_meta.RustSubKind;
const EdgeContext = cf.EdgeContext;
const PhantomManager = phantom_mod.PhantomManager;
const TypeEnv = type_env_mod.TypeEnv;

/// Per-function/test scanning context, created once per declaration in
/// processFunction and passed by const pointer to all scan/resolve helpers.
const ScanContext = struct {
    graph: *Graph,
    source: []const u8,
    caller_id: NodeId,
    caller_parent_id: ?NodeId,
    fn_node: ts.Node,
    edge_ctx: *const EdgeContext,
    k: *const KindIds,
    graph_index: *const GraphIndex,
    phantom_mgr: *const PhantomManager,
    type_env: *const TypeEnv,
    wl: *LspWorklist,
    io: std.Io,
    log: Logger,
    /// NodeId of the impl target type when this function is an impl method; null otherwise.
    self_type_id: ?NodeId = null,
};

fn addResolvedEdges(allocator: std.mem.Allocator, sctx: *const ScanContext, target_file_id: NodeId, chain: []const []const u8, is_call: bool) !bool {
    const rctx = shared_resolve.ResolveContext{
        .graph_index = sctx.graph_index,
        .io = sctx.io,
        .log = sctx.log,
        .resolve_return_type = cf.resolveReturnTypeScope,
        .find_in_type_scope = findMethodInImplBlocks,
    };
    return try shared_resolve.addResolvedEdges(allocator, sctx.graph, sctx.caller_id, target_file_id, chain, is_call, &rctx);
}

fn resolveOriginCall(allocator: std.mem.Allocator, sctx: *const ScanContext, origin: cf.SymbolOrigin, call_chain: []const []const u8, is_call: bool) !bool {
    const rctx = shared_resolve.ResolveContext{
        .graph_index = sctx.graph_index,
        .io = sctx.io,
        .log = sctx.log,
        .resolve_return_type = cf.resolveReturnTypeScope,
        .find_in_type_scope = findMethodInImplBlocks,
    };
    return try shared_resolve.resolveOriginCall(allocator, sctx.graph, sctx.caller_id, origin, call_chain, is_call, &rctx);
}

/// Walk the AST to discover edges (calls, uses_type, implements).
pub fn walkForEdges(
    allocator: std.mem.Allocator,
    io: std.Io,
    graph: *Graph,
    source: []const u8,
    root: ts.Node,
    k: *const KindIds,
    edge_ctx: *const EdgeContext,
    graph_index: *const GraphIndex,
    phantom_mgr: *const PhantomManager,
    wl: *LspWorklist,
    log: Logger,
) !void {
    var i: u32 = 0;
    while (i < root.childCount()) : (i += 1) {
        const child = root.child(i) orelse continue;
        const kid = child.kindId();

        if (kid == k.function_item) {
            try processFunction(allocator, io, graph, source, child, k, edge_ctx, graph_index, phantom_mgr, wl, log, null);
        } else if (kid == k.impl_item) {
            try processImpl(allocator, io, graph, source, child, k, edge_ctx, graph_index, phantom_mgr, wl, log);
        } else if (kid == k.mod_item) {
            try processInlineMod(allocator, io, graph, source, child, k, edge_ctx, graph_index, phantom_mgr, wl, log);
        } else if (kid == k.struct_item or kid == k.enum_item or kid == k.union_item) {
            try processStructOrEnum(allocator, io, graph, source, child, k, edge_ctx, graph_index, phantom_mgr, wl, log);
        }
    }
}

/// Build TypeEnv and run a single-pass body scan for the matched function.
/// Process a function_item: scan its body for call expressions and type references.
fn processFunction(
    allocator: std.mem.Allocator,
    io: std.Io,
    graph: *Graph,
    source: []const u8,
    fn_node: ts.Node,
    k: *const KindIds,
    edge_ctx: *const EdgeContext,
    graph_index: *const GraphIndex,
    phantom_mgr: *const PhantomManager,
    wl: *LspWorklist,
    log: Logger,
    self_type_id: ?NodeId,
) !void {
    const fn_name = findFunctionName(source, fn_node, k) orelse return;
    const fn_line = fn_node.startPoint().row + 1;
    const caller_id = findNodeByNameAndLine(graph, fn_name, fn_line, edge_ctx.scope_start, edge_ctx.scope_end) orelse return;
    const caller_parent_id = graph.getNode(caller_id).?.parent_id;

    var type_env = TypeEnv{};
    defer type_env.deinit(allocator);
    try buildTypeEnv(allocator, io, graph, source, fn_node, caller_id, caller_parent_id, edge_ctx, k, graph_index, self_type_id, &type_env);

    const sctx = ScanContext{
        .graph = graph,
        .source = source,
        .caller_id = caller_id,
        .caller_parent_id = caller_parent_id,
        .fn_node = fn_node,
        .edge_ctx = edge_ctx,
        .k = k,
        .graph_index = graph_index,
        .phantom_mgr = phantom_mgr,
        .type_env = &type_env,
        .wl = wl,
        .io = io,
        .log = log,
        .self_type_id = self_type_id,
    };

    var i: u32 = 0;
    while (i < fn_node.childCount()) : (i += 1) {
        const child = fn_node.child(i) orelse continue;
        if (child.kindId() == k.block) {
            try scanBodyForEdges(allocator, &sctx, child, 0);
        }
    }

    try scanSignatureForTypeRefs(allocator, &sctx);
}

/// Build TypeEnv for a single function: binds self/Self, parameters, and block-local variables.
fn buildTypeEnv(
    allocator: std.mem.Allocator,
    io: std.Io,
    graph: *const Graph,
    source: []const u8,
    fn_node: ts.Node,
    caller_id: NodeId,
    caller_parent_id: ?NodeId,
    edge_ctx: *const EdgeContext,
    k: *const KindIds,
    graph_index: *const GraphIndex,
    self_type_id: ?NodeId,
    type_env: *TypeEnv,
) !void {
    _ = caller_id;
    if (self_type_id) |tid| {
        try type_env.bindLocal(allocator, "self", tid);
        try type_env.bindLocal(allocator, "Self", tid);
    }
    try buildTypeEnvFromParams(allocator, graph, source, fn_node, caller_parent_id, edge_ctx, k, graph_index, type_env);
    var i: u32 = 0;
    while (i < fn_node.childCount()) : (i += 1) {
        const child = fn_node.child(i) orelse continue;
        if (child.kindId() == k.block) {
            try buildTypeEnvFromBlock(allocator, io, graph, source, child, caller_parent_id, edge_ctx, k, graph_index, type_env);
            break;
        }
    }
}

/// Scan function parameters and bind cross-file origins and local type IDs into TypeEnv.
fn buildTypeEnvFromParams(
    allocator: std.mem.Allocator,
    graph: *const Graph,
    source: []const u8,
    fn_node: ts.Node,
    caller_parent_id: ?NodeId,
    edge_ctx: *const EdgeContext,
    k: *const KindIds,
    graph_index: *const GraphIndex,
    type_env: *TypeEnv,
) !void {
    var i: u32 = 0;
    while (i < fn_node.childCount()) : (i += 1) {
        const child = fn_node.child(i) orelse continue;
        if (child.kindId() != k.parameters) continue;

        var j: u32 = 0;
        while (j < child.namedChildCount()) : (j += 1) {
            const param = child.namedChild(j) orelse continue;
            if (param.kindId() != k.parameter) continue;
            if (param.kindId() == k.self_parameter) continue;

            const name_node = param.namedChild(0) orelse continue;
            if (name_node.kindId() != k.identifier) continue;
            const param_name = ts_api.nodeText(source, name_node);

            const type_node = param.namedChild(1) orelse continue;

            // Cross-file: scoped path (module::Type) -> param_origin.
            var chain: [cf.max_chain_depth][]const u8 = undefined;
            const chain_len = extractScopedTypeChain(source, type_node, &chain, k);
            if (chain_len >= 2) {
                if (edge_ctx.findImportTarget(chain[0])) |target_file| {
                    try type_env.addParamOrigin(allocator, param_name, target_file, chain[1..chain_len]);
                    continue;
                }
            }

            // Local: bare type_identifier resolved at bind time.
            const unwrapped = unwrapTypeNode(type_node, k);
            if (unwrapped.kindId() == k.type_identifier) {
                const type_name = ts_api.nodeText(source, unwrapped);
                if (findTypeByNameScoped(graph, type_name, edge_ctx.scope_start, edge_ctx.scope_end, caller_parent_id, &graph_index.scope)) |type_id| {
                    try type_env.bindLocal(allocator, param_name, type_id);
                }
            }
        }
        break;
    }
}

/// Walk a block recursively, binding let declarations into TypeEnv for cross-file and local types.
fn buildTypeEnvFromBlock(
    allocator: std.mem.Allocator,
    io: std.Io,
    graph: *const Graph,
    source: []const u8,
    node: ts.Node,
    caller_parent_id: ?NodeId,
    edge_ctx: *const EdgeContext,
    k: *const KindIds,
    graph_index: *const GraphIndex,
    type_env: *TypeEnv,
) !void {
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        const kid = child.kindId();

        if (kid == k.let_declaration) {
            const var_name = getLetVarName(source, child, k) orelse continue;

            // Import-qualified RHS -> cross_file binding.
            if (cf.findImportQualifiedRoot(source, child, edge_ctx, k)) |target_file| {
                const resolved = cf.resolveVarTargetThroughReturnType(io, graph, source, child, edge_ctx, k, graph_index, Logger.noop) orelse target_file;
                try type_env.bindCrossFile(allocator, var_name, resolved);
                continue;
            }

            // Struct literal, static call, or explicit annotation -> local binding.
            // type_env.local has Self/self already; graph lookup covers other types.
            if (extractLetType(source, child)) |type_name| {
                const type_id = type_env.local.get(type_name) orelse
                    findTypeDefByNameScoped(graph, type_name, edge_ctx.scope_start, edge_ctx.scope_end, &graph_index.scope);
                if (type_id) |tid| {
                    try type_env.bindLocal(allocator, var_name, tid);
                }
                continue;
            }

            // Call expression RHS -> bind variable to the callee's return type.
            if (findLetCallExpr(child, k)) |call_expr| {
                if (resolveCallResultType(graph, source, call_expr, type_env, edge_ctx.scope_start, edge_ctx.scope_end, caller_parent_id, k, graph_index)) |type_id| {
                    try type_env.bindLocal(allocator, var_name, type_id);
                }
            }
            continue;
        }

        if (kid != k.function_item) {
            try buildTypeEnvFromBlock(allocator, io, graph, source, child, caller_parent_id, edge_ctx, k, graph_index, type_env);
        }
    }
}

/// Process an impl_item: recurse into its methods and create implements edges.
fn processImpl(
    allocator: std.mem.Allocator,
    io: std.Io,
    graph: *Graph,
    source: []const u8,
    impl_node: ts.Node,
    k: *const KindIds,
    edge_ctx: *const EdgeContext,
    graph_index: *const GraphIndex,
    phantom_mgr: *const PhantomManager,
    wl: *LspWorklist,
    log: Logger,
) !void {
    const scope_start = edge_ctx.scope_start;
    const scope_end = edge_ctx.scope_end;

    // Check for "impl Trait for Type" pattern to create implements edge.
    const impl_info = ast.getImplInfo(source, impl_node, k);
    if (impl_info) |info| {
        if (info.has_for) {
            if (info.trait_name) |trait_name| {
                const impl_line = impl_node.startPoint().row + 1;
                const impl_id = findImplNode(graph, impl_line, scope_start, scope_end);
                if (impl_id) |iid| {
                    const trait_id = findTypeByNameScoped(graph, trait_name, scope_start, scope_end, null, &graph_index.scope);
                    const type_id = findTypeDefByNameScoped(graph, info.type_name, scope_start, scope_end, &graph_index.scope);
                    if (trait_id) |tid| {
                        if (type_id) |uid| {
                            _ = try graph.addEdgeIfNew(allocator, .{ .source_id = uid, .target_id = tid, .edge_type = .implements });
                        } else {
                            _ = try graph.addEdgeIfNew(allocator, .{ .source_id = iid, .target_id = tid, .edge_type = .implements });
                        }
                    }
                }
            }
        }
    }

    // Resolve the impl target type so methods know their self type.
    const impl_self_type_id: ?NodeId = if (impl_info) |info|
        findTypeDefByNameScoped(graph, info.type_name, scope_start, scope_end, &graph_index.scope)
    else
        null;

    // Recurse into the declaration_list to process methods.
    var i: u32 = 0;
    while (i < impl_node.childCount()) : (i += 1) {
        const child = impl_node.child(i) orelse continue;
        if (child.kindId() == k.declaration_list) {
            var j: u32 = 0;
            while (j < child.childCount()) : (j += 1) {
                const decl = child.child(j) orelse continue;
                if (decl.kindId() == k.function_item) {
                    try processFunction(allocator, io, graph, source, decl, k, edge_ctx, graph_index, phantom_mgr, wl, log, impl_self_type_id);
                }
            }
        }
    }
}

/// Process an inline mod_item: recurse into its declaration_list.
fn processInlineMod(
    allocator: std.mem.Allocator,
    io: std.Io,
    graph: *Graph,
    source: []const u8,
    mod_node: ts.Node,
    k: *const KindIds,
    edge_ctx: *const EdgeContext,
    graph_index: *const GraphIndex,
    phantom_mgr: *const PhantomManager,
    wl: *LspWorklist,
    log: Logger,
) !void {
    var i: u32 = 0;
    while (i < mod_node.childCount()) : (i += 1) {
        const child = mod_node.child(i) orelse continue;
        if (child.kindId() == k.declaration_list) {
            var j: u32 = 0;
            while (j < child.childCount()) : (j += 1) {
                const decl = child.child(j) orelse continue;
                const kid = decl.kindId();
                if (kid == k.function_item) {
                    try processFunction(allocator, io, graph, source, decl, k, edge_ctx, graph_index, phantom_mgr, wl, log, null);
                } else if (kid == k.impl_item) {
                    try processImpl(allocator, io, graph, source, decl, k, edge_ctx, graph_index, phantom_mgr, wl, log);
                } else if (kid == k.mod_item) {
                    try processInlineMod(allocator, io, graph, source, decl, k, edge_ctx, graph_index, phantom_mgr, wl, log);
                } else if (kid == k.struct_item or kid == k.enum_item or kid == k.union_item) {
                    try processStructOrEnum(allocator, io, graph, source, decl, k, edge_ctx, graph_index, phantom_mgr, wl, log);
                }
            }
        }
    }
}

/// Scan struct/enum/union field declarations for type references.
fn processStructOrEnum(
    allocator: std.mem.Allocator,
    io: std.Io,
    graph: *Graph,
    source: []const u8,
    item_node: ts.Node,
    k: *const KindIds,
    edge_ctx: *const EdgeContext,
    graph_index: *const GraphIndex,
    phantom_mgr: *const PhantomManager,
    wl: *LspWorklist,
    log: Logger,
) !void {
    const name = findTypeName(source, item_node, k) orelse return;
    const line = item_node.startPoint().row + 1;
    const owner_id = findNodeByNameAndLine(graph, name, line, edge_ctx.scope_start, edge_ctx.scope_end) orelse return;

    var i: u32 = 0;
    while (i < item_node.childCount()) : (i += 1) {
        const child = item_node.child(i) orelse continue;
        const kid = child.kindId();
        if (kid == k.field_declaration_list or
            kid == k.ordered_field_declaration_list or
            kid == k.enum_variant_list)
        {
            try scanFieldTypesRecursive(allocator, io, graph, source, child, owner_id, k, edge_ctx, graph_index, phantom_mgr, wl, log);
        }
    }
}

/// Extract the type name (type_identifier child) from a struct/enum/union AST node.
fn findTypeName(source: []const u8, node: ts.Node, k: *const KindIds) ?[]const u8 {
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        if (child.kindId() == k.type_identifier) {
            return ts_api.nodeText(source, child);
        }
    }
    return null;
}

fn scanFieldTypesRecursive(
    allocator: std.mem.Allocator,
    io: std.Io,
    graph: *Graph,
    source: []const u8,
    node: ts.Node,
    owner_id: NodeId,
    k: *const KindIds,
    edge_ctx: *const EdgeContext,
    graph_index: *const GraphIndex,
    phantom_mgr: *const PhantomManager,
    wl: *LspWorklist,
    log: Logger,
) !void {
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        const kid = child.kindId();

        if (kid == k.type_identifier) {
            const type_name = ts_api.nodeText(source, child);
            if (!isPrimitiveOrSelf(type_name)) {
                const target_id = findTypeByNameScoped(graph, type_name, edge_ctx.scope_start, edge_ctx.scope_end, null, &graph_index.scope) orelse
                    findTypeCrossFile(graph, type_name, edge_ctx, &graph_index.scope, phantom_mgr);
                if (target_id) |tid| {
                    _ = try graph.addEdgeIfNew(allocator, .{ .source_id = owner_id, .target_id = tid, .edge_type = .uses_type });
                } else {
                    const pos = child.startPoint();
                    try wl.append(allocator, .{
                        .source_node_id = owner_id,
                        .file_path = graph.nodes.items[@intFromEnum(owner_id)].file_path orelse "",
                        .line = pos.row,
                        .col = pos.column,
                        .query_kind = .definition,
                        .hint_name = type_name,
                    });
                }
            }
        } else if (kid == k.scoped_type_identifier) {
            try resolveScopedFieldType(allocator, io, graph, source, child, owner_id, k, edge_ctx, graph_index, wl, log);
        } else if (kid != k.attribute_item) {
            try scanFieldTypesRecursive(allocator, io, graph, source, child, owner_id, k, edge_ctx, graph_index, phantom_mgr, wl, log);
        }
    }
}

fn resolveScopedFieldType(
    allocator: std.mem.Allocator,
    io: std.Io,
    graph: *Graph,
    source: []const u8,
    scoped_node: ts.Node,
    owner_id: NodeId,
    k: *const KindIds,
    edge_ctx: *const EdgeContext,
    graph_index: *const GraphIndex,
    wl: *LspWorklist,
    log: Logger,
) !void {
    var segments: [cf.max_chain_depth][]const u8 = undefined;
    var seg_count: usize = 0;
    cf.collectScopedSegments(source, scoped_node, &segments, &seg_count, k, 0);
    if (seg_count < 2) return;

    const qualifier = segments[0];

    // Skip if qualifier is a local type (inner identifiers handled by recursion).
    if (findTypeDefByNameScoped(graph, qualifier, edge_ctx.scope_start, edge_ctx.scope_end, &graph_index.scope) != null) return;

    // Resolve through import map for cross-file types.
    const origin = edge_ctx.findImportOrigin(qualifier) orelse {
        const pos = leafIdentifierPos(scoped_node, k);
        try wl.append(allocator, .{
            .source_node_id = owner_id,
            .file_path = graph.nodes.items[@intFromEnum(owner_id)].file_path orelse "",
            .line = pos.row,
            .col = pos.column,
            .query_kind = .type_definition,
            .hint_name = segments[seg_count - 1],
        });
        return;
    };

    var resolve_chain: [cf.max_chain_depth][]const u8 = undefined;
    var len: usize = 0;
    for (origin.chain) |seg| {
        if (len >= cf.max_chain_depth) break;
        resolve_chain[len] = seg;
        len += 1;
    }
    for (segments[1..seg_count]) |seg| {
        if (len >= cf.max_chain_depth) break;
        resolve_chain[len] = seg;
        len += 1;
    }
    if (len == 0) return;

    const rctx = shared_resolve.ResolveContext{
        .graph_index = graph_index,
        .io = io,
        .log = log,
        .resolve_return_type = cf.resolveReturnTypeScope,
        .find_in_type_scope = findMethodInImplBlocks,
    };
    var edge_buf: [cf.max_chain_depth]cf.ResolvedEdge = undefined;
    const edge_count = cf.resolveQualifiedCall(
        graph,
        origin.file_id,
        resolve_chain[0..len],
        false,
        &rctx,
        &edge_buf,
    );
    if (edge_count > 0) {
        for (edge_buf[0..edge_count]) |edge| {
            _ = try graph.addEdgeIfNew(allocator, .{
                .source_id = owner_id,
                .target_id = edge.target_id,
                .edge_type = edge.edge_type,
            });
        }
    } else {
        const pos = leafIdentifierPos(scoped_node, k);
        try wl.append(allocator, .{
            .source_node_id = owner_id,
            .file_path = graph.nodes.items[@intFromEnum(owner_id)].file_path orelse "",
            .line = pos.row,
            .col = pos.column,
            .query_kind = .type_definition,
            .hint_name = segments[seg_count - 1],
        });
    }
}

/// Single recursive pass over a function body emitting all edge kinds.
fn scanBodyForEdges(allocator: std.mem.Allocator, sctx: *const ScanContext, ts_node: ts.Node, depth: u32) !void {
    if (depth >= cf.max_ast_scan_depth) return;
    const kid = ts_node.kindId();

    if (kid == sctx.k.function_item) return;

    if (kid == sctx.k.call_expression) {
        try handleCall(allocator, sctx, ts_node);
    } else if (kid == sctx.k.field_expression) {
        // Skip field_expressions that are the fn_ref of a call (handled by handleCall).
        const parent = ts_node.parent();
        const is_call_fn_ref = if (parent) |p| p.kindId() == sctx.k.call_expression and blk: {
            break :blk if (p.namedChild(0)) |first|
                first.startByte() == ts_node.startByte() and first.endByte() == ts_node.endByte()
            else
                false;
        } else false;
        if (!is_call_fn_ref) {
            try handleFieldAccess(allocator, sctx, ts_node);
        }
    } else if (kid == sctx.k.struct_expression) {
        try handleStructExpr(allocator, sctx, ts_node);
    } else if (kid == sctx.k.type_identifier) {
        try handleTypeRef(allocator, sctx, ts_node);
    } else if (kid == sctx.k.identifier) {
        const name = ts_api.nodeText(sctx.source, ts_node);
        if (name.len > 0 and std.ascii.isUpper(name[0]) and !isPrimitiveOrSelf(name)) {
            if (findTypeCrossFile(sctx.graph, name, sctx.edge_ctx, &sctx.graph_index.scope, sctx.phantom_mgr)) |tid| {
                _ = try sctx.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = tid, .edge_type = .uses_type });
            }
        }
    }

    var i: u32 = 0;
    while (i < ts_node.childCount()) : (i += 1) {
        const child = ts_node.child(i) orelse continue;
        try scanBodyForEdges(allocator, sctx, child, depth + 1);
    }
}

/// Emit calls edges for a call_expression node.
fn handleCall(allocator: std.mem.Allocator, sctx: *const ScanContext, call_node: ts.Node) !void {
    const func_ref = call_node.child(0) orelse return;
    const ref_kid = func_ref.kindId();

    if (ref_kid == sctx.k.identifier) {
        const name = ts_api.nodeText(sctx.source, func_ref);
        if (findFunctionByNameScoped(sctx.graph, name, sctx.edge_ctx.scope_start, sctx.edge_ctx.scope_end, sctx.caller_parent_id, &sctx.graph_index.scope)) |target_id| {
            _ = try sctx.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = target_id, .edge_type = .calls });
        } else if (sctx.edge_ctx.findImportOrigin(name)) |origin| {
            if (!try addResolvedEdges(allocator, sctx, origin.file_id, origin.chain, true)) {
                const pos = call_node.startPoint();
                try sctx.wl.append(allocator, .{
                    .source_node_id = sctx.caller_id,
                    .file_path = sctx.graph.nodes.items[@intFromEnum(sctx.caller_id)].file_path orelse "",
                    .line = pos.row,
                    .col = pos.column,
                    .query_kind = .definition,
                    .hint_name = name,
                });
            }
        } else {
            const pos = call_node.startPoint();
            try sctx.wl.append(allocator, .{
                .source_node_id = sctx.caller_id,
                .file_path = sctx.graph.nodes.items[@intFromEnum(sctx.caller_id)].file_path orelse "",
                .line = pos.row,
                .col = pos.column,
                .query_kind = .definition,
                .hint_name = name,
            });
        }
    } else if (ref_kid == sctx.k.scoped_identifier) {
        try resolveScopedCall(allocator, sctx, func_ref);
    } else if (ref_kid == sctx.k.field_expression) {
        try resolveFieldCall(allocator, sctx, func_ref);
    } else if (ref_kid == sctx.k.generic_function) {
        try resolveGenericFunctionCall(allocator, sctx, func_ref);
    }
}

/// Resolve Type::method() or module::function() scoped calls.
fn resolveScopedCall(allocator: std.mem.Allocator, sctx: *const ScanContext, scoped_node: ts.Node) !void {
    var segments: [cf.max_chain_depth][]const u8 = undefined;
    var seg_count: usize = 0;
    const scope_start = sctx.edge_ctx.scope_start;
    const scope_end = sctx.edge_ctx.scope_end;

    cf.collectScopedSegments(sctx.source, scoped_node, &segments, &seg_count, sctx.k, 0);

    if (seg_count < 2) {
        if (seg_count == 1) {
            if (findFunctionByNameScoped(sctx.graph, segments[0], scope_start, scope_end, sctx.caller_parent_id, &sctx.graph_index.scope)) |target_id| {
                _ = try sctx.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = target_id, .edge_type = .calls });
            }
        }
        return;
    }

    const qualifier = segments[0];
    const method_name = segments[seg_count - 1];

    if (findTypeDefByNameScoped(sctx.graph, qualifier, scope_start, scope_end, &sctx.graph_index.scope)) |type_id| {
        if (findMethodInTypeOrImpls(sctx.graph, type_id, method_name, scope_start, scope_end, &sctx.graph_index.scope)) |target_id| {
            _ = try sctx.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = target_id, .edge_type = .calls });
            return;
        }
        const pos = leafIdentifierPos(scoped_node, sctx.k);
        try sctx.wl.append(allocator, .{
            .source_node_id = sctx.caller_id,
            .file_path = sctx.graph.nodes.items[@intFromEnum(sctx.caller_id)].file_path orelse "",
            .line = pos.row,
            .col = pos.column,
            .query_kind = .definition,
            .hint_name = method_name,
        });
        return;
    }

    if (sctx.edge_ctx.findImportOrigin(qualifier)) |origin| {
        var resolve_chain: [cf.max_chain_depth][]const u8 = undefined;
        var len: usize = 0;
        for (origin.chain) |seg| {
            if (len >= cf.max_chain_depth) break;
            resolve_chain[len] = seg;
            len += 1;
        }
        for (segments[1..seg_count]) |seg| {
            if (len >= cf.max_chain_depth) break;
            resolve_chain[len] = seg;
            len += 1;
        }
        if (len > 0) {
            if (try addResolvedEdges(allocator, sctx, origin.file_id, resolve_chain[0..len], true)) return;
        }
    }

    const pos = leafIdentifierPos(scoped_node, sctx.k);
    try sctx.wl.append(allocator, .{
        .source_node_id = sctx.caller_id,
        .file_path = sctx.graph.nodes.items[@intFromEnum(sctx.caller_id)].file_path orelse "",
        .line = pos.row,
        .col = pos.column,
        .query_kind = .definition,
        .hint_name = method_name,
    });
}

/// Resolve obj.method() field expression calls using TypeEnv lookups.
fn resolveFieldCall(allocator: std.mem.Allocator, sctx: *const ScanContext, field_node: ts.Node) !void {
    const scope_start = sctx.edge_ctx.scope_start;
    const scope_end = sctx.edge_ctx.scope_end;

    var method_name: ?[]const u8 = null;
    var i: u32 = 0;
    while (i < field_node.childCount()) : (i += 1) {
        const child = field_node.child(i) orelse continue;
        if (child.kindId() == sctx.k.field_identifier) {
            method_name = ts_api.nodeText(sctx.source, child);
        }
    }
    const name = method_name orelse return;

    var chain: [cf.max_chain_depth][]const u8 = undefined;
    var chain_len: usize = 0;
    cf.collectFieldChainForVar(sctx.source, field_node, &chain, &chain_len, sctx.k, 0);
    if (chain_len >= 1) {
        const root_name = chain[0];

        if (sctx.type_env.local.get(root_name)) |type_id| {
            if (findMethodInTypeOrImpls(sctx.graph, type_id, name, scope_start, scope_end, &sctx.graph_index.scope)) |target_id| {
                _ = try sctx.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = target_id, .edge_type = .calls });
                return;
            }
            if (findFunctionByNameScoped(sctx.graph, name, scope_start, scope_end, sctx.caller_parent_id, &sctx.graph_index.scope)) |target_id| {
                _ = try sctx.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = target_id, .edge_type = .calls });
                return;
            }
            const pos = leafIdentifierPos(field_node, sctx.k);
            try sctx.wl.append(allocator, .{
                .source_node_id = sctx.caller_id,
                .file_path = sctx.graph.nodes.items[@intFromEnum(sctx.caller_id)].file_path orelse "",
                .line = pos.row,
                .col = pos.column,
                .query_kind = .definition,
                .hint_name = name,
            });
            return;
        }

        if (sctx.type_env.cross_file.get(root_name)) |target_file_id| {
            if (!try addResolvedEdges(allocator, sctx, target_file_id, &.{name}, true)) {
                const pos = leafIdentifierPos(field_node, sctx.k);
                try sctx.wl.append(allocator, .{
                    .source_node_id = sctx.caller_id,
                    .file_path = sctx.graph.nodes.items[@intFromEnum(sctx.caller_id)].file_path orelse "",
                    .line = pos.row,
                    .col = pos.column,
                    .query_kind = .definition,
                    .hint_name = name,
                });
            }
            return;
        }

        if (sctx.type_env.findParamOrigin(root_name)) |origin| {
            if (!try resolveOriginCall(allocator, sctx, origin, &.{name}, true)) {
                const pos = leafIdentifierPos(field_node, sctx.k);
                try sctx.wl.append(allocator, .{
                    .source_node_id = sctx.caller_id,
                    .file_path = sctx.graph.nodes.items[@intFromEnum(sctx.caller_id)].file_path orelse "",
                    .line = pos.row,
                    .col = pos.column,
                    .query_kind = .definition,
                    .hint_name = name,
                });
            }
            return;
        }
    }

    // Fallback: self/local type via scope search.
    if (findFunctionByNameScoped(sctx.graph, name, scope_start, scope_end, sctx.caller_parent_id, &sctx.graph_index.scope)) |target_id| {
        _ = try sctx.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = target_id, .edge_type = .calls });
    } else {
        // Append unresolved method call to the worklist.
        const pos = field_node.startPoint();
        try sctx.wl.append(allocator, .{
            .source_node_id = sctx.caller_id,
            .file_path = sctx.graph.nodes.items[@intFromEnum(sctx.caller_id)].file_path orelse "",
            .line = pos.row,
            .col = pos.column,
            .query_kind = .definition,
            .hint_name = name,
        });
    }
}

/// Handle a call_expression whose function reference is a generic_function (turbofish).
fn resolveGenericFunctionCall(allocator: std.mem.Allocator, sctx: *const ScanContext, generic_node: ts.Node) !void {
    var i: u32 = 0;
    while (i < generic_node.childCount()) : (i += 1) {
        const child = generic_node.child(i) orelse continue;
        if (child.kindId() == sctx.k.type_arguments) {
            try scanNodeForTypeRefs(allocator, sctx, child);
        }
    }

    const inner_ref = generic_node.namedChild(0) orelse return;
    const inner_kid = inner_ref.kindId();

    if (inner_kid == sctx.k.identifier) {
        const name = ts_api.nodeText(sctx.source, inner_ref);
        if (findFunctionByNameScoped(sctx.graph, name, sctx.edge_ctx.scope_start, sctx.edge_ctx.scope_end, sctx.caller_parent_id, &sctx.graph_index.scope)) |target_id| {
            _ = try sctx.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = target_id, .edge_type = .calls });
        } else {
            const pos = inner_ref.startPoint();
            try sctx.wl.append(allocator, .{
                .source_node_id = sctx.caller_id,
                .file_path = sctx.graph.nodes.items[@intFromEnum(sctx.caller_id)].file_path orelse "",
                .line = pos.row,
                .col = pos.column,
                .query_kind = .definition,
                .hint_name = name,
            });
        }
    } else if (inner_kid == sctx.k.scoped_identifier) {
        try resolveScopedCall(allocator, sctx, inner_ref);
    } else if (inner_kid == sctx.k.field_expression) {
        try resolveFieldCall(allocator, sctx, inner_ref);
    }
}

/// Emit accesses_field for a field_expression whose receiver is in type_env.local.
fn handleFieldAccess(allocator: std.mem.Allocator, sctx: *const ScanContext, field_expr: ts.Node) !void {
    var chain: [cf.max_chain_depth][]const u8 = undefined;
    var chain_len: usize = 0;
    cf.collectFieldChainForVar(sctx.source, field_expr, &chain, &chain_len, sctx.k, 0);
    if (chain_len < 2) return;
    const root_name = chain[0];
    const field_name = chain[chain_len - 1];
    const type_id = sctx.type_env.local.get(root_name) orelse return;
    if (shared_lookup.findFieldByName(sctx.graph, type_id, field_name, &sctx.graph_index.scope)) |field_id| {
        _ = try sctx.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = field_id, .edge_type = .accesses_field });
    }
}

/// Emit uses_type and accesses_field edges for a struct_expression node.
/// Handles both field_initializer and shorthand_field_initializer.
fn handleStructExpr(allocator: std.mem.Allocator, sctx: *const ScanContext, struct_node: ts.Node) !void {
    var type_name: ?[]const u8 = null;
    var i: u32 = 0;
    while (i < struct_node.childCount()) : (i += 1) {
        const child = struct_node.child(i) orelse continue;
        const ck = child.kindId();
        if (ck == sctx.k.type_identifier) {
            type_name = ts_api.nodeText(sctx.source, child);
            break;
        }
        if (ck == sctx.k.scoped_type_identifier) {
            const nc = child.namedChildCount();
            if (nc > 0) {
                if (child.namedChild(nc - 1)) |last| {
                    type_name = ts_api.nodeText(sctx.source, last);
                }
            }
            break;
        }
    }

    // Resolve "Self" through TypeEnv.
    var type_id_opt: ?NodeId = null;
    if (type_name) |tname| {
        if (std.mem.eql(u8, tname, "Self") or std.mem.eql(u8, tname, "self")) {
            type_id_opt = sctx.type_env.local.get(tname);
        } else {
            type_id_opt = findTypeByNameScoped(sctx.graph, tname, sctx.edge_ctx.scope_start, sctx.edge_ctx.scope_end, sctx.caller_parent_id, &sctx.graph_index.scope);
        }
    }

    const type_id = type_id_opt orelse {
        if (type_name) |tname| {
            const pos = struct_node.startPoint();
            try sctx.wl.append(allocator, .{
                .source_node_id = sctx.caller_id,
                .file_path = sctx.graph.nodes.items[@intFromEnum(sctx.caller_id)].file_path orelse "",
                .line = pos.row,
                .col = pos.column,
                .query_kind = .type_definition,
                .hint_name = tname,
            });
        }
        return;
    };
    _ = try sctx.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = type_id, .edge_type = .uses_type });

    // Collect explicit field names so we know which are inherited from `..base`.
    var explicit_fields: [max_struct_explicit_fields][]const u8 = undefined;
    var explicit_count: usize = 0;
    var base_var_name: ?[]const u8 = null;

    var j: u32 = 0;
    while (j < struct_node.childCount()) : (j += 1) {
        const child = struct_node.child(j) orelse continue;
        if (child.kindId() != sctx.k.field_initializer_list) continue;

        var m: u32 = 0;
        while (m < child.childCount()) : (m += 1) {
            const fi = child.child(m) orelse continue;
            const fi_kid = fi.kindId();

            if (fi_kid == sctx.k.field_initializer) {
                if (fi.namedChild(0)) |fn_node| {
                    if (fn_node.kindId() == sctx.k.field_identifier) {
                        const field_name = ts_api.nodeText(sctx.source, fn_node);
                        if (explicit_count < max_struct_explicit_fields) {
                            explicit_fields[explicit_count] = field_name;
                            explicit_count += 1;
                        }
                        if (shared_lookup.findFieldByName(sctx.graph, type_id, field_name, &sctx.graph_index.scope)) |field_id| {
                            _ = try sctx.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = field_id, .edge_type = .accesses_field });
                        }
                    }
                }
            } else if (fi_kid == sctx.k.shorthand_field_initializer) {
                // Shorthand `Foo { bar }`: field name equals the identifier.
                if (fi.namedChild(0)) |id_node| {
                    if (id_node.kindId() == sctx.k.identifier) {
                        const field_name = ts_api.nodeText(sctx.source, id_node);
                        if (explicit_count < max_struct_explicit_fields) {
                            explicit_fields[explicit_count] = field_name;
                            explicit_count += 1;
                        }
                        if (shared_lookup.findFieldByName(sctx.graph, type_id, field_name, &sctx.graph_index.scope)) |field_id| {
                            _ = try sctx.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = field_id, .edge_type = .accesses_field });
                        }
                    }
                }
            } else if (fi_kid == sctx.k.base_field_initializer) {
                // `..base` spread: record the base variable name for post-walk resolution.
                if (fi.namedChild(0)) |id_node| {
                    if (id_node.kindId() == sctx.k.identifier) {
                        base_var_name = ts_api.nodeText(sctx.source, id_node);
                    }
                }
            }
        }
    }

    // Emit accesses_field for fields of the base that were NOT explicitly set.
    // The spread copies those fields out of the base value, reading each of them.
    if (base_var_name) |bname| {
        if (sctx.type_env.local.get(bname)) |base_type_id| {
            for (sctx.graph_index.scope.childrenOf(base_type_id)) |child_idx| {
                const n = sctx.graph.nodes.items[child_idx];
                if (n.kind != .field) continue;
                var is_explicit = false;
                for (explicit_fields[0..explicit_count]) |ef| {
                    if (std.mem.eql(u8, ef, n.name)) {
                        is_explicit = true;
                        break;
                    }
                }
                if (!is_explicit) {
                    _ = try sctx.graph.addEdgeIfNew(allocator, .{
                        .source_id = sctx.caller_id,
                        .target_id = @enumFromInt(child_idx),
                        .edge_type = .accesses_field,
                    });
                }
            }
        }
    }
}

/// Maximum number of explicitly initialized fields tracked per struct expression for spread resolution.
const max_struct_explicit_fields = 64;

/// Emit uses_type for a type_identifier that resolves to a type in scope or cross-file.
fn handleTypeRef(allocator: std.mem.Allocator, sctx: *const ScanContext, id_node: ts.Node) !void {
    const type_name = ts_api.nodeText(sctx.source, id_node);
    if (isPrimitiveOrSelf(type_name)) return;
    const target_id = findTypeByNameScoped(sctx.graph, type_name, sctx.edge_ctx.scope_start, sctx.edge_ctx.scope_end, sctx.caller_parent_id, &sctx.graph_index.scope) orelse
        findTypeCrossFile(sctx.graph, type_name, sctx.edge_ctx, &sctx.graph_index.scope, sctx.phantom_mgr);
    if (target_id) |tid| {
        _ = try sctx.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = tid, .edge_type = .uses_type });
    } else {
        const pos = id_node.startPoint();
        try sctx.wl.append(allocator, .{
            .source_node_id = sctx.caller_id,
            .file_path = sctx.graph.nodes.items[@intFromEnum(sctx.caller_id)].file_path orelse "",
            .line = pos.row,
            .col = pos.column,
            .query_kind = .type_definition,
            .hint_name = type_name,
        });
    }
}

/// Scan function parameter types and return type for type references.
fn scanSignatureForTypeRefs(allocator: std.mem.Allocator, sctx: *const ScanContext) !void {
    const scope_start = sctx.edge_ctx.scope_start;
    const scope_end = sctx.edge_ctx.scope_end;

    var i: u32 = 0;
    while (i < sctx.fn_node.childCount()) : (i += 1) {
        const child = sctx.fn_node.child(i) orelse continue;
        const kid = child.kindId();

        if (kid == sctx.k.type_identifier) {
            const type_name = ts_api.nodeText(sctx.source, child);
            if (!isPrimitiveOrSelf(type_name)) {
                const target_id = findTypeByNameScoped(sctx.graph, type_name, scope_start, scope_end, sctx.caller_parent_id, &sctx.graph_index.scope) orelse
                    findTypeCrossFile(sctx.graph, type_name, sctx.edge_ctx, &sctx.graph_index.scope, sctx.phantom_mgr);
                if (target_id) |tid| {
                    _ = try sctx.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = tid, .edge_type = .uses_type });
                }
            }
        }

        if (kid == sctx.k.parameters) {
            try scanNodeForTypeRefs(allocator, sctx, child);
        } else if (kid == sctx.k.generic_type or kid == sctx.k.reference_type or kid == sctx.k.scoped_type_identifier) {
            try scanNodeForTypeRefs(allocator, sctx, child);
        }
    }
}

/// Recursively scan for type_identifier nodes and create uses_type edges.
fn scanNodeForTypeRefs(allocator: std.mem.Allocator, sctx: *const ScanContext, node: ts.Node) !void {
    const scope_start = sctx.edge_ctx.scope_start;
    const scope_end = sctx.edge_ctx.scope_end;

    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        if (child.kindId() == sctx.k.type_identifier) {
            const type_name = ts_api.nodeText(sctx.source, child);
            if (!isPrimitiveOrSelf(type_name)) {
                const target_id = findTypeByNameScoped(sctx.graph, type_name, scope_start, scope_end, sctx.caller_parent_id, &sctx.graph_index.scope) orelse
                    findTypeCrossFile(sctx.graph, type_name, sctx.edge_ctx, &sctx.graph_index.scope, sctx.phantom_mgr);
                if (target_id) |tid| {
                    _ = try sctx.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = tid, .edge_type = .uses_type });
                }
            }
        } else {
            try scanNodeForTypeRefs(allocator, sctx, child);
        }
    }
}

/// Check if a type name is a Rust primitive, Self, or another name to skip.
fn isPrimitiveOrSelf(name: []const u8) bool {
    const skip_list = [_][]const u8{
        "Self", "bool",  "char", "str",
        "u8",   "u16",   "u32",  "u64",
        "u128", "usize", "i8",   "i16",
        "i32",  "i64",   "i128", "isize",
        "f32",  "f64",
    };
    for (skip_list) |s| {
        if (std.mem.eql(u8, name, s)) return true;
    }
    return false;
}

/// Walk to the leaf identifier of a scoped or field expression for LSP position.
fn leafIdentifierPos(node: ts.Node, k: *const KindIds) ts.Point {
    const kid = node.kindId();
    if (kid == k.field_expression) {
        const count = node.namedChildCount();
        if (count >= 2) {
            if (node.namedChild(count - 1)) |last| {
                if (last.kindId() == k.field_identifier) return last.startPoint();
            }
        }
    } else if (kid == k.scoped_identifier) {
        const count = node.namedChildCount();
        if (count >= 1) {
            if (node.namedChild(count - 1)) |last| {
                if (last.kindId() == k.identifier or last.kindId() == k.type_identifier) return last.startPoint();
            }
        }
    }
    return node.startPoint();
}

fn findTypeCrossFile(graph: *const Graph, name: []const u8, edge_ctx: *const EdgeContext, scope_index: *const ScopeIndex, phantom_mgr: *const PhantomManager) ?NodeId {
    return shared_lookup.findTypeCrossFile(graph, name, edge_ctx, scope_index, phantom_mgr);
}

/// Extract the type name from a let_declaration.
fn extractLetType(source: []const u8, let_node: ts.Node) ?[]const u8 {
    const start = let_node.startByte();
    const end = let_node.endByte();
    if (start >= end or end > source.len) return null;
    const text = source[start..end];

    const eq_pos = std.mem.indexOfScalar(u8, text, '=') orelse return null;

    {
        var ci: usize = 0;
        while (ci < eq_pos) : (ci += 1) {
            if (text[ci] != ':') continue;
            if (ci + 1 < eq_pos and text[ci + 1] == ':') {
                ci += 1;
                continue;
            }
            var pos = ci + 1;
            while (pos < eq_pos and source_scan.isWhitespace(text[pos])) : (pos += 1) {}
            if (pos < eq_pos and text[pos] == '&') {
                pos += 1;
                while (pos < eq_pos and source_scan.isWhitespace(text[pos])) : (pos += 1) {}
            }
            if (pos + 4 <= eq_pos and std.mem.eql(u8, text[pos..][0..3], "mut") and
                source_scan.isWhitespace(text[pos + 3]))
            {
                pos += 4;
                while (pos < eq_pos and source_scan.isWhitespace(text[pos])) : (pos += 1) {}
            }
            const type_start = pos;
            while (pos < eq_pos and source_scan.isIdentChar(text[pos])) : (pos += 1) {}
            if (pos > type_start) {
                const type_name = text[type_start..pos];
                if (type_name.len > 0 and std.ascii.isUpper(type_name[0])) {
                    return type_name;
                }
            }
            break;
        }
    }

    var pos = eq_pos + 1;
    while (pos < text.len and source_scan.isWhitespace(text[pos])) : (pos += 1) {}

    const type_start = pos;
    while (pos < text.len and source_scan.isIdentChar(text[pos])) : (pos += 1) {}
    if (pos == type_start) return null;
    const type_name = text[type_start..pos];

    if (!std.ascii.isUpper(type_name[0])) return null;

    if (pos + 1 < text.len and text[pos] == ':' and text[pos + 1] == ':') {
        return type_name;
    }
    var brace_pos = pos;
    while (brace_pos < text.len and source_scan.isWhitespace(text[brace_pos])) : (brace_pos += 1) {}
    if (brace_pos < text.len and text[brace_pos] == '{') {
        return type_name;
    }

    return null;
}

/// Return the call_expression node from the RHS of a let_declaration, if the
/// initializer is a direct call. Handles one level of try-expression wrapping.
fn findLetCallExpr(let_node: ts.Node, k: *const KindIds) ?ts.Node {
    var i: u32 = 0;
    while (i < let_node.namedChildCount()) : (i += 1) {
        const child = let_node.namedChild(i) orelse continue;
        const kid = child.kindId();
        if (kid == k.call_expression) return child;
        // Unwrap one level of generic_function wrapping (turbofish or await).
        if (kid == k.generic_function) {
            if (child.namedChild(0)) |inner| {
                if (inner.kindId() == k.call_expression) return inner;
            }
        }
    }
    return null;
}

/// Given a call_expression node, resolve the return type of the callee to a graph NodeId.
/// Handles bare identifier calls, field-expression method calls, scoped path calls,
/// and turbofish calls. Returns null when the callee or its return type cannot be resolved.
fn resolveCallResultType(
    graph: *const Graph,
    source: []const u8,
    call_node: ts.Node,
    type_env: *const TypeEnv,
    scope_start: usize,
    scope_end: usize,
    caller_parent_id: ?NodeId,
    k: *const KindIds,
    graph_index: *const GraphIndex,
) ?NodeId {
    const func_ref = call_node.child(0) orelse return null;
    const ref_kid = func_ref.kindId();

    var fn_id: ?NodeId = null;

    if (ref_kid == k.identifier) {
        const name = ts_api.nodeText(source, func_ref);
        fn_id = findFunctionByNameScoped(graph, name, scope_start, scope_end, caller_parent_id, &graph_index.scope);
    } else if (ref_kid == k.field_expression) {
        // obj.method() -- look up obj's type, find method in impl blocks.
        var chain: [cf.max_chain_depth][]const u8 = undefined;
        var chain_len: usize = 0;
        cf.collectFieldChainForVar(source, func_ref, &chain, &chain_len, k, 0);
        if (chain_len >= 2) {
            const root_name = chain[0];
            const method_name = chain[chain_len - 1];
            if (type_env.local.get(root_name)) |type_id| {
                fn_id = findMethodInTypeOrImpls(graph, type_id, method_name, scope_start, scope_end, &graph_index.scope);
            }
        }
    } else if (ref_kid == k.scoped_identifier) {
        // Type::method() -- find the type and then the method.
        var segments: [cf.max_chain_depth][]const u8 = undefined;
        var seg_count: usize = 0;
        cf.collectScopedSegments(source, func_ref, &segments, &seg_count, k, 0);
        if (seg_count >= 2) {
            const type_name = segments[0];
            const method_name = segments[seg_count - 1];
            if (findTypeDefByNameScoped(graph, type_name, scope_start, scope_end, &graph_index.scope)) |type_id| {
                fn_id = findMethodInTypeOrImpls(graph, type_id, method_name, scope_start, scope_end, &graph_index.scope);
            }
        }
    } else if (ref_kid == k.generic_function) {
        // Turbofish: foo::<T>() -- resolve the inner function reference.
        if (func_ref.namedChild(0)) |inner| {
            if (inner.kindId() == k.identifier) {
                const name = ts_api.nodeText(source, inner);
                fn_id = findFunctionByNameScoped(graph, name, scope_start, scope_end, caller_parent_id, &graph_index.scope);
            } else if (inner.kindId() == k.scoped_identifier) {
                var segments: [cf.max_chain_depth][]const u8 = undefined;
                var seg_count: usize = 0;
                cf.collectScopedSegments(source, inner, &segments, &seg_count, k, 0);
                if (seg_count >= 2) {
                    const type_name = segments[0];
                    const method_name = segments[seg_count - 1];
                    if (findTypeDefByNameScoped(graph, type_name, scope_start, scope_end, &graph_index.scope)) |type_id| {
                        fn_id = findMethodInTypeOrImpls(graph, type_id, method_name, scope_start, scope_end, &graph_index.scope);
                    }
                }
            }
        }
    }

    const fid = fn_id orelse return null;
    return cf.resolveReturnTypeScope(graph, fid, graph_index);
}

/// Get the variable name from a let_declaration AST node. Skips mutable_specifier if present.
fn getLetVarName(source: []const u8, let_node: ts.Node, k: *const KindIds) ?[]const u8 {
    var i: u32 = 0;
    while (i < let_node.namedChildCount()) : (i += 1) {
        const child = let_node.namedChild(i) orelse continue;
        const kid = child.kindId();
        if (kid == k.mutable_specifier) continue;
        if (kid == k.identifier) return ts_api.nodeText(source, child);
        break;
    }
    return null;
}

/// Extract scoped type segments from a parameter's type node.
fn extractScopedTypeChain(source: []const u8, type_node: ts.Node, out: *[cf.max_chain_depth][]const u8, k: *const KindIds) usize {
    const unwrapped = unwrapTypeNode(type_node, k);
    const kid = unwrapped.kindId();
    if (kid == k.scoped_type_identifier or kid == k.scoped_identifier) {
        var count: usize = 0;
        cf.collectScopedSegments(source, unwrapped, out, &count, k, 0);
        return count;
    }
    return 0;
}

/// Unwrap reference_type, generic_type, and other wrappers to find the inner type node.
fn unwrapTypeNode(node: ts.Node, k: *const KindIds) ts.Node {
    var current = node;
    var iterations: usize = 0;
    while (iterations < 10) : (iterations += 1) {
        const kid = current.kindId();
        if (kid == k.reference_type) {
            const nc = current.namedChildCount();
            if (nc > 0) {
                if (current.namedChild(nc - 1)) |inner| {
                    current = inner;
                    continue;
                }
            }
        }
        if (kid == k.generic_type) {
            if (current.namedChild(0)) |inner| {
                current = inner;
                continue;
            }
        }
        break;
    }
    return current;
}

/// Callback for resolveQualifiedCall: search impl blocks for a named member.
fn findMethodInImplBlocks(g: *const Graph, type_id: NodeId, name: []const u8, graph_index: *const GraphIndex) ?NodeId {
    const scope_index = &graph_index.scope;
    const type_node = g.getNode(type_id) orelse return null;
    if (!type_node.kind.isTypeContainer()) return null;
    const type_name = type_node.name;
    if (type_name.len == 0) return null;

    const file_id = g.findContainingFile(type_id) orelse return null;
    for (scope_index.childrenOf(file_id)) |sibling_idx| {
        const sib = g.nodes.items[sibling_idx];
        if (sib.kind != .type_def) continue;
        if (sib.lang_meta != .rust) continue;
        if (sib.lang_meta.rust.sub_kind != .impl_block) continue;
        if (!std.mem.eql(u8, sib.name, type_name)) continue;

        const impl_id: NodeId = @enumFromInt(sibling_idx);
        for (scope_index.childrenOf(impl_id)) |child_idx| {
            const child = g.nodes.items[child_idx];
            if (std.mem.eql(u8, child.name, name)) {
                return @enumFromInt(child_idx);
            }
        }
    }
    return null;
}

/// Find a method by name within a type's direct children or any impl_block targeting the same type name.
fn findMethodInTypeOrImpls(
    graph: *const Graph,
    type_id: NodeId,
    method_name: []const u8,
    scope_start: usize,
    scope_end: usize,
    scope_index: *const ScopeIndex,
) ?NodeId {
    const type_node = graph.getNode(type_id) orelse return null;
    const type_name = type_node.name;

    for (scope_index.childrenOf(type_id)) |child_idx| {
        const n = graph.nodes.items[child_idx];
        if (n.kind == .function and std.mem.eql(u8, n.name, method_name)) {
            return @enumFromInt(child_idx);
        }
    }

    const items = graph.nodes.items;
    const end = @min(scope_end, items.len);
    for (items[scope_start..end], scope_start..) |n, idx| {
        if (n.kind == .type_def and n.lang_meta == .rust and
            n.lang_meta.rust.sub_kind == .impl_block and
            std.mem.eql(u8, n.name, type_name))
        {
            const impl_id: NodeId = @enumFromInt(idx);
            for (scope_index.childrenOf(impl_id)) |child_idx| {
                const child = items[child_idx];
                if (child.kind == .function and std.mem.eql(u8, child.name, method_name)) {
                    return @enumFromInt(child_idx);
                }
            }
        }
    }

    return null;
}

/// Find the identifier name in a function_item or function_signature_item.
fn findFunctionName(source: []const u8, fn_node: ts.Node, k: *const KindIds) ?[]const u8 {
    var i: u32 = 0;
    while (i < fn_node.childCount()) : (i += 1) {
        const child = fn_node.child(i) orelse continue;
        if (child.kindId() == k.identifier) {
            return ts_api.nodeText(source, child);
        }
    }
    return null;
}

/// Find a graph node by name and line number within a scope range.
fn findNodeByNameAndLine(graph: *const Graph, name: []const u8, line: u32, scope_start: usize, scope_end: usize) ?NodeId {
    const items = graph.nodes.items;
    const end = @min(scope_end, items.len);
    for (items[scope_start..end], scope_start..) |n, idx| {
        if (n.line_start != null and n.line_start.? == line and std.mem.eql(u8, n.name, name)) {
            return @enumFromInt(idx);
        }
    }
    return null;
}

fn findFunctionByNameScoped(graph: *const Graph, name: []const u8, scope_start: usize, scope_end: usize, caller_parent_id: ?NodeId, scope_index: *const ScopeIndex) ?NodeId {
    return shared_lookup.findFunctionByNameScoped(graph, name, scope_start, scope_end, caller_parent_id, scope_index, &.{.test_def});
}

fn findTypeByNameScoped(graph: *const Graph, name: []const u8, scope_start: usize, scope_end: usize, caller_parent_id: ?NodeId, scope_index: *const ScopeIndex) ?NodeId {
    return shared_lookup.findTypeByNameScoped(graph, name, scope_start, scope_end, caller_parent_id, scope_index, isTypeNodeWithName);
}

fn isTypeNodeWithName(n: Node, name: []const u8) bool {
    return isTypeNode(n) and std.mem.eql(u8, n.name, name);
}

/// Find an actual type definition (struct, enum, union) by name,
/// excluding impl_blocks, traits, type aliases, and associated types.
fn findTypeDefByNameScoped(graph: *const Graph, name: []const u8, scope_start: usize, scope_end: usize, scope_index: *const ScopeIndex) ?NodeId {
    _ = scope_index;
    const items = graph.nodes.items;
    const end = @min(scope_end, items.len);
    var sole_match: ?NodeId = null;
    var match_count: usize = 0;
    for (items[scope_start..end], scope_start..) |n, idx| {
        if (isTypeDefNode(n) and std.mem.eql(u8, n.name, name)) {
            sole_match = @enumFromInt(idx);
            match_count += 1;
            if (match_count > 1) return null;
        }
    }
    return sole_match;
}

/// Check whether a node is any type container (type_def, enum_def, union_def).
pub fn isTypeNode(n: Node) bool {
    return n.kind == .type_def or n.kind == .enum_def or n.kind == .union_def;
}

/// Check whether a node is a concrete type definition (struct, enum, union),
/// excluding impl_blocks, traits, type aliases, and associated types.
pub fn isTypeDefNode(n: Node) bool {
    if (n.kind == .enum_def or n.kind == .union_def) return true;
    if (n.kind != .type_def) return false;
    if (n.lang_meta == .rust) {
        const sk = n.lang_meta.rust.sub_kind;
        if (sk == .impl_block or sk == .trait_ or sk == .type_alias or sk == .associated_type) return false;
    }
    return true;
}

/// Find an impl block node by line number within a scope range.
fn findImplNode(graph: *const Graph, line: u32, scope_start: usize, scope_end: usize) ?NodeId {
    const items = graph.nodes.items;
    const end = @min(scope_end, items.len);
    for (items[scope_start..end], scope_start..) |n, idx| {
        if (n.kind == .type_def and n.lang_meta == .rust and
            n.lang_meta.rust.sub_kind == .impl_block and
            n.line_start != null and n.line_start.? == line)
        {
            return @enumFromInt(idx);
        }
    }
    return null;
}
