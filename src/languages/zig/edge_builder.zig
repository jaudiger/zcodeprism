const std = @import("std");
const graph_mod = @import("../../core/graph.zig");
const logging = @import("../../logging.zig");
const types = @import("../../core/types.zig");
const ts = @import("tree-sitter");
const ts_api = @import("../../parser/tree_sitter_api.zig");
const ast = @import("ast_analysis.zig");
const cf = @import("cross_file.zig");
const source_scan = @import("../../parser/source_scan.zig");
const pc = @import("parse_context.zig");
const phantom_mod = @import("../../core/phantom.zig");
const shared_resolve = @import("../shared/resolve.zig");
const shared_lookup = @import("../shared/lookup.zig");
const shared_scan = @import("../shared/scan.zig");
const shared_types = @import("../shared/types.zig");
const shared_ast = @import("../shared/ast.zig");
const type_env_mod = @import("../shared/type_env.zig");
const worklist_mod = @import("../../lsp/worklist.zig");
const zig_meta = @import("meta.zig");

const LspWorklist = worklist_mod.LspWorklist;

const Field = logging.Field;
const Logger = logging.Logger;

const Graph = graph_mod.Graph;
const NodeId = types.NodeId;
const EdgeType = types.EdgeType;
const PhantomManager = phantom_mod.PhantomManager;

const EdgeContext = cf.EdgeContext;
const KindIds = pc.KindIds;
const GraphIndex = @import("../../core/graph_index.zig").GraphIndex;
const ScopeIndex = @import("../../core/scope_index.zig").ScopeIndex;
const TypeEnv = type_env_mod.TypeEnv;

/// Maps node IDs (fields and parameters) to their resolved type node IDs.
/// Populated by processContainerFieldEdges and processParameterTypeEdges,
/// then read by inferExpectedType to resolve field_expression operands
/// and by inferTypeFromCallArg to resolve function argument types.
pub const NodeTypeMap = std.AutoHashMapUnmanaged(NodeId, NodeId);

/// Bundles all state needed to scan a single function or test body for edges.
/// Common fields live in base; Zig-specific extras (field_types, k) are direct.
const ScanContext = struct {
    base: shared_scan.BaseScanContext,
    k: *const KindIds,
    field_types: *const NodeTypeMap,

    pub fn isBoundary(self: *const @This(), node: ts.Node) bool {
        const kid = node.kindId();
        return kid == self.k.function_declaration or kid == self.k.test_declaration;
    }

    pub fn dispatch(self: *const @This(), allocator: std.mem.Allocator, node: ts.Node, depth: u32) !void {
        _ = depth;
        const kid = node.kindId();
        if (kid == self.k.call_expression) {
            try handleCall(allocator, self, node);
        } else if (kid == self.k.field_expression) {
            const parent = node.parent();
            const is_call_fn_ref = if (parent) |p| p.kindId() == self.k.call_expression and blk: {
                break :blk if (p.namedChild(0)) |first|
                    first.startByte() == node.startByte() and first.endByte() == node.endByte()
                else
                    false;
            } else false;
            if (!is_call_fn_ref) {
                if (try handleQualifiedValueUse(allocator, self, node)) return;
                if (node.namedChildCount() < 2) {
                    try handleBareEnumLiteral(allocator, self, node);
                } else {
                    try handleFieldAccess(allocator, self, node);
                }
            }
        } else if (kid == self.k.identifier or kid == self.k.property_identifier) {
            try handleIdentifier(allocator, self, node);
        } else if (kid == self.k.variable_declaration) {
            const is_named_literal = extractStructLiteralType(self.base.source, node) != null;
            const is_anon_literal = !is_named_literal and hasAnonymousStructLiteralRHS(self.base.source, node);
            if (is_named_literal or is_anon_literal) {
                try handleStructLiteral(allocator, self, node);
            }
        } else if (kid == self.k.return_expression) {
            try handleReturnStructLiteral(allocator, self, node);
        }
    }
};

/// Build TypeEnv and run a single-pass body scan for the matched declaration.
fn processDeclarationEdges(
    allocator: std.mem.Allocator,
    io: std.Io,
    g: *Graph,
    source: []const u8,
    ts_node: ts.Node,
    caller_id: NodeId,
    ctx: *const EdgeContext,
    k: *const KindIds,
    graph_index: *const GraphIndex,
    phantom_mgr: *const PhantomManager,
    field_types: *const NodeTypeMap,
    wl: *LspWorklist,
    log: Logger,
) !void {
    const caller_parent_id = shared_scan.BaseScanContext.parentOf(g, caller_id);

    var type_env = TypeEnv{};
    defer type_env.deinit(allocator);
    try buildTypeEnv(allocator, io, g, source, ts_node, caller_id, caller_parent_id, ctx, k, graph_index, field_types, &type_env);

    const sctx = ScanContext{
        .base = .{
            .graph = g,
            .source = source,
            .caller_id = caller_id,
            .caller_parent_id = caller_parent_id,
            .fn_node = ts_node,
            .edge_ctx = ctx,
            .type_env = &type_env,
            .graph_index = graph_index,
            .phantom_mgr = phantom_mgr,
            .wl = wl,
            .io = io,
            .log = log,
            .resolve = .{
                .graph_index = graph_index,
                .io = io,
                .log = log,
                .return_type_resolver = cf.return_type_resolver,
                .find_in_type_scope = null,
            },
        },
        .k = k,
        .field_types = field_types,
    };

    try scanSignatureForTypeRefs(allocator, &sctx);

    var bi: u32 = 0;
    while (bi < ts_node.childCount()) : (bi += 1) {
        const child = ts_node.child(bi) orelse continue;
        if (child.kindId() == k.block) {
            try shared_scan.walkBody(ScanContext, allocator, &sctx, child, 0);
            break;
        }
    }
}

/// Populate TypeEnv for a single function or test declaration.
/// Binds self, @This() aliases, parameters, and block-local variables.
fn buildTypeEnv(
    allocator: std.mem.Allocator,
    io: std.Io,
    g: *const Graph,
    source: []const u8,
    fn_decl_node: ts.Node,
    caller_id: NodeId,
    caller_parent_id: ?NodeId,
    ctx: *const EdgeContext,
    k: *const KindIds,
    graph_index: *const GraphIndex,
    field_types: *const NodeTypeMap,
    type_env: *TypeEnv,
) !void {
    if (g.getNode(caller_id)) |caller_node| {
        if (caller_node.parent_id) |parent_id| {
            try type_env.bindLocal(allocator, "self", parent_id);
            const scope = fn_decl_node.parent() orelse return;
            var i: u32 = 0;
            while (i < scope.namedChildCount()) : (i += 1) {
                const sibling = scope.namedChild(i) orelse continue;
                if (sibling.kindId() != k.variable_declaration) continue;
                if (!ast.isThisBuiltin(source, sibling, k)) continue;
                const alias_name = ast.getIdentifierName(source, sibling, k) orelse continue;
                try type_env.bindLocal(allocator, alias_name, parent_id);
            }
        }
    }

    try buildTypeEnvFromParams(allocator, g, source, fn_decl_node, caller_parent_id, ctx, k, graph_index, type_env);

    const return_type_id: ?NodeId = blk: {
        if (cf.resolveReturnTypeScope(g, caller_id, graph_index)) |rt| break :blk rt;
        if (g.getNode(caller_id)) |fn_node| {
            if (fn_node.signature) |sig| {
                if (std.mem.indexOf(u8, sig, "@This()") != null) break :blk fn_node.parent_id;
            }
        }
        break :blk null;
    };
    if (return_type_id) |rt_id| {
        try type_env.bindLocal(allocator, "_return", rt_id);
    }

    var ci: u32 = 0;
    while (ci < fn_decl_node.childCount()) : (ci += 1) {
        const child = fn_decl_node.child(ci) orelse continue;
        if (child.kindId() == k.block) {
            try buildTypeEnvFromBlock(allocator, io, g, source, child, caller_parent_id, ctx, k, graph_index, field_types, type_env);
            break;
        }
    }
}

/// Bind parameters into TypeEnv: cross-file origins and local types.
fn buildTypeEnvFromParams(
    allocator: std.mem.Allocator,
    g: *const Graph,
    source: []const u8,
    fn_decl_node: ts.Node,
    caller_parent_id: ?NodeId,
    ctx: *const EdgeContext,
    k: *const KindIds,
    graph_index: *const GraphIndex,
    type_env: *TypeEnv,
) !void {
    var i: u32 = 0;
    while (i < fn_decl_node.childCount()) : (i += 1) {
        const child = fn_decl_node.child(i) orelse continue;
        if (child.kindId() != k.parameters) continue;

        var j: u32 = 0;
        while (j < child.namedChildCount()) : (j += 1) {
            const param = child.namedChild(j) orelse continue;
            if (param.kindId() != k.parameter) continue;
            const name_node = param.namedChild(0) orelse continue;
            if (name_node.kindId() != k.identifier) continue;
            const name = ts_api.nodeText(source, name_node);
            if (std.mem.eql(u8, name, "self")) continue;
            const type_node = param.namedChild(1) orelse continue;

            var chain: [cf.max_chain_depth][]const u8 = undefined;
            const chain_len = extractParamTypeChain(source, type_node, &chain, k);
            if (chain_len >= 2) {
                if (ctx.findImportTarget(chain[0])) |target_file_id| {
                    try type_env.addParamOrigin(allocator, name, target_file_id, chain[1..chain_len]);
                    continue;
                }
            }

            const base = unwrapTypeNode(type_node, k);
            if (base.kindId() == k.identifier) {
                const type_name = ts_api.nodeText(source, base);
                if (findTypeByNameScoped(g, type_name, ctx.scope_start, ctx.scope_end, caller_parent_id, &graph_index.scope)) |type_id| {
                    try type_env.bindLocal(allocator, name, type_id);
                }
            }
            j += 1;
        }
        break;
    }
}

/// Bind block-local variables into TypeEnv.
fn buildTypeEnvFromBlock(
    allocator: std.mem.Allocator,
    io: std.Io,
    g: *const Graph,
    source: []const u8,
    block: ts.Node,
    caller_parent_id: ?NodeId,
    ctx: *const EdgeContext,
    k: *const KindIds,
    graph_index: *const GraphIndex,
    field_types: *const NodeTypeMap,
    type_env: *TypeEnv,
) !void {
    var i: u32 = 0;
    while (i < block.childCount()) : (i += 1) {
        const child = block.child(i) orelse continue;
        const kid = child.kindId();

        if (kid == k.variable_declaration) {
            const var_name = ast.getIdentifierName(source, child, k) orelse continue;

            if (cf.findImportQualifiedRoot(source, child, ctx, k)) |target_file_id| {
                const resolved = cf.resolveVarTargetThroughReturnType(g, source, child, ctx, k, graph_index, Logger.noop) orelse target_file_id;
                try type_env.bindCrossFile(allocator, var_name, resolved);
                continue;
            }

            const resolved_type_name: ?[]const u8 = blk: {
                if (extractAnnotationTypeNode(child, k)) |ann| {
                    const base = unwrapTypeNode(ann, k);
                    if (base.kindId() == k.identifier) break :blk ts_api.nodeText(source, base);
                }
                break :blk extractStructLiteralType(source, child);
            };
            if (resolved_type_name) |type_name| {
                const type_id = type_env.local.get(type_name) orelse
                    findTypeByNameScoped(g, type_name, ctx.scope_start, ctx.scope_end, caller_parent_id, &graph_index.scope);
                if (type_id) |tid| {
                    try type_env.bindLocal(allocator, var_name, tid);
                }
                continue;
            }

            if (findVarDeclCallExpr(child, k)) |call_expr| {
                if (resolveCallResultType(g, source, call_expr, type_env, ctx.scope_start, ctx.scope_end, caller_parent_id, k, graph_index)) |type_id| {
                    try type_env.bindLocal(allocator, var_name, type_id);
                }
            }
            continue;
        }

        if (kid == k.function_declaration or kid == k.test_declaration) continue;

        if (kid == k.if_statement or kid == k.if_expression) {
            var cond_ident: ?[]const u8 = null;
            var cond_chain: [cf.max_chain_depth][]const u8 = undefined;
            var cond_chain_len: usize = 0;
            var capture_name: ?[]const u8 = null;
            var j: u32 = 0;
            while (j < child.childCount()) : (j += 1) {
                const ic = child.child(j) orelse continue;
                const ic_kid = ic.kindId();
                if (ic_kid == k.identifier and cond_ident == null and cond_chain_len == 0) {
                    cond_ident = ts_api.nodeText(source, ic);
                } else if (ic_kid == k.field_expression and cond_ident == null and cond_chain_len == 0) {
                    cond_chain_len = cf.collectFieldExprChain(source, ic, &cond_chain, k);
                } else if (ic_kid == k.payload or ic_kid == k.payload_identifier) {
                    if (ic.namedChild(0)) |inner| {
                        if (inner.kindId() == k.identifier) {
                            capture_name = ts_api.nodeText(source, inner);
                        }
                    }
                }
            }
            if (capture_name) |cap| {
                if (cond_ident) |ci| {
                    if (type_env.findParamOrigin(ci)) |origin| {
                        try type_env.addParamOrigin(allocator, cap, origin.file_id, origin.chain);
                    }
                    if (type_env.local.get(ci)) |tid| {
                        try type_env.bindLocal(allocator, cap, tid);
                    }
                }
                if (cond_chain_len >= 2) {
                    const root = cond_chain[0];
                    if (type_env.local.get(root)) |root_tid| {
                        if (resolveChainToType(g, root_tid, cond_chain[1..cond_chain_len], field_types, &graph_index.scope, ctx.scope_start, ctx.scope_end, caller_parent_id)) |resolved_tid| {
                            try type_env.bindLocal(allocator, cap, resolved_tid);
                        }
                    }
                }
            }
        }

        try buildTypeEnvFromBlock(allocator, io, g, source, child, caller_parent_id, ctx, k, graph_index, field_types, type_env);
    }
}

/// For each `.parameter` child of a function, resolve the type annotation
/// from the AST, emit a `uses_type` edge from the parameter node to the
/// type node, and record the mapping in the shared NodeTypeMap.
fn processParameterTypeEdges(
    allocator: std.mem.Allocator,
    g: *Graph,
    source: []const u8,
    fn_decl_node: ts.Node,
    fn_id: NodeId,
    ctx: *const EdgeContext,
    k: *const KindIds,
    graph_index: *const GraphIndex,
    phantom_mgr: *const PhantomManager,
    node_type_map: *NodeTypeMap,
    wl: *LspWorklist,
    log: Logger,
) !void {
    _ = wl;
    _ = log;
    _ = phantom_mgr;

    const param_children = graph_index.scope.childrenOf(fn_id);

    var pi: u32 = 0;
    while (pi < fn_decl_node.childCount()) : (pi += 1) {
        const params_node = fn_decl_node.child(pi) orelse continue;
        if (params_node.kindId() != k.parameters) continue;

        var param_graph_idx: usize = 0;
        var j: u32 = 0;
        while (j < params_node.namedChildCount()) : (j += 1) {
            const param_ast = params_node.namedChild(j) orelse continue;
            if (param_ast.kindId() != k.parameter) continue;
            const type_node = param_ast.namedChild(1) orelse {
                param_graph_idx += 1;
                continue;
            };

            const param_node_id: ?NodeId = blk: {
                while (param_graph_idx < param_children.len) {
                    const ci = param_children[param_graph_idx];
                    const n = g.nodes.items[ci];
                    if (n.kind == .parameter) {
                        param_graph_idx += 1;
                        break :blk @enumFromInt(ci);
                    }
                    param_graph_idx += 1;
                }
                break :blk null;
            };
            const param_id = param_node_id orelse continue;

            const base = unwrapTypeNode(type_node, k);
            if (base.kindId() == k.identifier) {
                const type_name = ts_api.nodeText(source, base);
                const owner_node = g.getNode(fn_id);
                const owner_parent_id: ?NodeId = if (owner_node) |n| n.parent_id else null;
                const type_id =
                    findTypeByNameScoped(g, type_name, ctx.scope_start, ctx.scope_end, owner_parent_id, &graph_index.scope) orelse
                    continue;
                _ = try g.addEdgeIfNew(allocator, .{ .source_id = param_id, .target_id = type_id, .edge_type = .uses_type });
                try node_type_map.put(allocator, param_id, type_id);
            }
        }
        break;
    }
}

/// Recursively walk a tree-sitter AST and create edges in the graph.
///
/// For each function or test declaration, matches it to a graph node then
/// delegates to processDeclarationEdges for env building and edge emission.
/// Recurses into named children to find nested declarations.
pub fn walkForEdges(allocator: std.mem.Allocator, io: std.Io, g: *Graph, source: []const u8, ts_node: ts.Node, ctx: *const EdgeContext, k: *const KindIds, graph_index: *const GraphIndex, phantom_mgr: *const PhantomManager, node_type_map: *NodeTypeMap, wl: *LspWorklist, log: Logger) !void {
    try walkForEdgesInner(allocator, io, g, source, ts_node, ctx, k, graph_index, phantom_mgr, node_type_map, wl, log);
}

fn walkForEdgesInner(allocator: std.mem.Allocator, io: std.Io, g: *Graph, source: []const u8, ts_node: ts.Node, ctx: *const EdgeContext, k: *const KindIds, graph_index: *const GraphIndex, phantom_mgr: *const PhantomManager, field_types: *NodeTypeMap, wl: *LspWorklist, log: Logger) !void {
    const kid = ts_node.kindId();

    if (kid == k.function_declaration) {
        if (ast.getIdentifierName(source, ts_node, k)) |name| {
            const decl_line = ts_node.startPoint().row + 1;
            if (shared_lookup.findFunctionByNameAndLine(g, name, decl_line, ctx.scope_start, ctx.scope_end)) |fn_id| {
                try processDeclarationEdges(allocator, io, g, source, ts_node, fn_id, ctx, k, graph_index, phantom_mgr, field_types, wl, log);
                try processParameterTypeEdges(allocator, g, source, ts_node, fn_id, ctx, k, graph_index, phantom_mgr, field_types, wl, log);
            } else {
                log.trace("function not found in graph", &.{
                    Field.string("name", name),
                    Field.uint("line", decl_line),
                });
            }
        }
    } else if (kid == k.test_declaration) {
        const test_name = ast.getTestName(source, ts_node, k);
        if (findTestByName(g, test_name, ctx.scope_start, ctx.scope_end)) |test_id| {
            try processDeclarationEdges(allocator, io, g, source, ts_node, test_id, ctx, k, graph_index, phantom_mgr, field_types, wl, log);
        } else {
            log.trace("test not found in graph", &.{Field.string("name", test_name)});
        }
    } else if (kid == k.variable_declaration) {
        try processConstantInitEdges(allocator, io, g, source, ts_node, ctx, k, graph_index, phantom_mgr, wl, log);
    } else if (kid == k.struct_declaration or kid == k.enum_declaration or kid == k.union_declaration) {
        const parent = ts_node.parent();
        const name: ?[]const u8 = if (parent) |p|
            if (p.kindId() == k.variable_declaration) ast.getIdentifierName(source, p, k) else null
        else
            null;
        if (name) |n| {
            const decl_line = ts_node.startPoint().row + 1;
            if (shared_lookup.findFunctionByNameAndLine(g, n, decl_line, ctx.scope_start, ctx.scope_end)) |container_id| {
                try processContainerFieldEdges(allocator, g, source, ts_node, container_id, ctx, k, graph_index, phantom_mgr, field_types, wl, log);
            } else {
                log.trace("container not found in graph", &.{
                    Field.string("name", n),
                    Field.uint("line", decl_line),
                });
            }
        }
    }

    var i: u32 = 0;
    while (i < ts_node.namedChildCount()) : (i += 1) {
        const child = ts_node.namedChild(i) orelse continue;
        try walkForEdgesInner(allocator, io, g, source, child, ctx, k, graph_index, phantom_mgr, field_types, wl, log);
    }
}

/// Emit uses_type edges for type identifiers in the function signature (parameters and
/// return type). Skips block children.
fn scanSignatureForTypeRefs(allocator: std.mem.Allocator, sctx: *const ScanContext) !void {
    var i: u32 = 0;
    while (i < sctx.base.fn_node.childCount()) : (i += 1) {
        const child = sctx.base.fn_node.child(i) orelse continue;
        if (child.kindId() == sctx.k.block) continue;
        try scanNodeForTypeRefs(allocator, sctx, child);
    }
}

/// Recursively walk a signature subtree, emitting uses_type for each identifier that
/// resolves to a type. Stops at leaf nodes; does not cross block boundaries.
fn scanNodeForTypeRefs(allocator: std.mem.Allocator, sctx: *const ScanContext, node: ts.Node) !void {
    const kid = node.kindId();
    if (kid == sctx.k.identifier or kid == sctx.k.property_identifier) {
        const name = ts_api.nodeText(sctx.base.source, node);
        if (lookupAsType(sctx, name)) |target_id| {
            _ = try sctx.base.emitEdge(allocator, target_id, .uses_type);
        }
        return;
    }
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        try scanNodeForTypeRefs(allocator, sctx, child);
    }
}

/// Emit calls edges for a call_expression node.
fn handleCall(allocator: std.mem.Allocator, sctx: *const ScanContext, call_node: ts.Node) !void {
    const fn_ref = call_node.namedChild(0) orelse return;
    const ref = extractCallFnRef(sctx.base.source, fn_ref, sctx.k) orelse return;
    switch (ref) {
        .bare => |callee_name| {
            if (findFunctionByNameScoped(sctx.base.graph, callee_name, sctx.base.edge_ctx.scope_start, sctx.base.edge_ctx.scope_end, sctx.base.caller_parent_id, &sctx.base.graph_index.scope)) |callee_id| {
                _ = try sctx.base.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.base.caller_id, .target_id = callee_id, .edge_type = .calls });
            } else if (sctx.base.edge_ctx.findImportOrigin(callee_name)) |origin| {
                if (origin.chain.len == 0 or !try sctx.base.resolveOriginCall(allocator, origin, &.{}, .calls)) {
                    const pos = call_node.startPoint();
                    try sctx.base.appendWorklist(allocator, pos, .definition, callee_name);
                }
            } else {
                sctx.base.log.trace("bare call unresolved", &.{Field.string("callee", callee_name)});
                const pos = call_node.startPoint();
                try sctx.base.appendWorklist(allocator, pos, .definition, callee_name);
            }
        },
        .qualified => |field_expr| {
            var chain: [cf.max_chain_depth][]const u8 = undefined;
            const chain_len = cf.collectFieldExprChain(sctx.base.source, field_expr, &chain, sctx.k);

            if (chain_len >= 2) {
                const root_name = chain[0];
                const leaf_name = chain[chain_len - 1];

                const resolved_via_origin = if (sctx.base.edge_ctx.findImportOrigin(root_name)) |origin|
                    try sctx.base.resolveOriginCall(allocator, origin, chain[1..chain_len], .calls)
                else
                    false;

                const resolved_via_cross = !resolved_via_origin and if (sctx.base.type_env.cross_file.get(root_name)) |target_file_id|
                    try sctx.base.addResolvedEdges(allocator, target_file_id, chain[1..chain_len], .calls)
                else
                    false;

                if (resolved_via_origin or resolved_via_cross) {
                    // Resolved, nothing more to do.
                } else if (sctx.base.type_env.local.get(root_name)) |type_id| {
                    if (!try resolveLocalChain(allocator, sctx, type_id, chain[1..chain_len])) {
                        if (findFunctionByNameScoped(sctx.base.graph, leaf_name, sctx.base.edge_ctx.scope_start, sctx.base.edge_ctx.scope_end, sctx.base.caller_parent_id, &sctx.base.graph_index.scope)) |callee_id| {
                            _ = try sctx.base.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.base.caller_id, .target_id = callee_id, .edge_type = .calls });
                        } else {
                            const pos = leafIdentifierPos(field_expr, call_node, sctx.k);
                            try sctx.base.appendWorklist(allocator, pos, .definition, leaf_name);
                        }
                    }
                } else if (sctx.base.type_env.findParamOrigin(root_name)) |origin| {
                    if (!try sctx.base.resolveOriginCall(allocator, origin, chain[1..chain_len], .calls)) {
                        const pos = leafIdentifierPos(field_expr, call_node, sctx.k);
                        try sctx.base.appendWorklist(allocator, pos, .definition, leaf_name);
                    }
                } else {
                    if (findTypeByNameScoped(sctx.base.graph, root_name, sctx.base.edge_ctx.scope_start, sctx.base.edge_ctx.scope_end, sctx.base.caller_parent_id, &sctx.base.graph_index.scope)) |type_id| {
                        if (!try resolveLocalChain(allocator, sctx, type_id, chain[1..chain_len])) {
                            const pos = leafIdentifierPos(field_expr, call_node, sctx.k);
                            try sctx.base.appendWorklist(allocator, pos, .definition, leaf_name);
                        }
                    } else {
                        sctx.base.log.trace("qualified call unresolved", &.{
                            Field.string("root", root_name),
                            Field.string("leaf", leaf_name),
                        });
                        const pos = leafIdentifierPos(field_expr, call_node, sctx.k);
                        try sctx.base.appendWorklist(allocator, pos, .definition, leaf_name);
                    }
                }
            } else if (chain_len == 1) {
                if (findFunctionByNameScoped(sctx.base.graph, chain[0], sctx.base.edge_ctx.scope_start, sctx.base.edge_ctx.scope_end, sctx.base.caller_parent_id, &sctx.base.graph_index.scope)) |callee_id| {
                    _ = try sctx.base.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.base.caller_id, .target_id = callee_id, .edge_type = .calls });
                }
            }
        },
    }
}

/// Emit accesses_field for a field_expression whose receiver is in type_env.
/// When the receiver type is a comptime switch alias, emits one edge per
/// candidate type that owns a matching field. Appends a type_definition
/// worklist entry when the receiver type is unknown.
fn handleFieldAccess(allocator: std.mem.Allocator, sctx: *const ScanContext, field_expr: ts.Node) !void {
    var chain: [cf.max_chain_depth][]const u8 = undefined;
    const chain_len = cf.collectFieldExprChain(sctx.base.source, field_expr, &chain, sctx.k);
    if (chain_len < 2) return;
    const root_name = chain[0];
    const field_name = chain[chain_len - 1];
    const type_id = sctx.base.type_env.local.get(root_name) orelse {
        const pos = field_expr.startPoint();
        try sctx.base.appendWorklist(allocator, pos, .type_definition, root_name);
        return;
    };
    const target_type_id = if (chain_len > 2)
        resolveChainToType(
            sctx.base.graph,
            type_id,
            chain[1 .. chain_len - 1],
            sctx.field_types,
            &sctx.base.graph_index.scope,
            sctx.base.edge_ctx.scope_start,
            sctx.base.edge_ctx.scope_end,
            sctx.base.caller_parent_id,
        ) orelse return
    else
        type_id;
    if (try emitFieldAccessOverAlias(allocator, sctx, target_type_id, field_name)) return;
    if (shared_lookup.findFieldByName(sctx.base.graph, target_type_id, field_name, &sctx.base.graph_index.scope)) |field_id| {
        _ = try sctx.base.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.base.caller_id, .target_id = field_id, .edge_type = .accesses_field });
    }
}

/// If `scope_id` is a comptime switch alias, emit `.accesses_field` edges
/// to the matching field on each candidate type. Returns true when the
/// node was a comptime alias, false otherwise.
fn emitFieldAccessOverAlias(
    allocator: std.mem.Allocator,
    sctx: *const ScanContext,
    scope_id: NodeId,
    field_name: []const u8,
) !bool {
    const node = sctx.base.graph.nodes.items[@intFromEnum(scope_id)];
    if (node.kind != .constant) return false;
    const zm = zig_meta.metaOf(&node) orelse return false;
    const arm_names = zm.comptime_switch_arm_names orelse return false;
    if (arm_names.len == 0) return false;

    for (arm_names) |arm_name| {
        if (std.mem.eql(u8, arm_name, node.name)) continue;
        const arm_type_id = findTypeByNameScoped(
            sctx.base.graph,
            arm_name,
            sctx.base.edge_ctx.scope_start,
            sctx.base.edge_ctx.scope_end,
            sctx.base.caller_parent_id,
            &sctx.base.graph_index.scope,
        ) orelse continue;
        if (arm_type_id == scope_id) continue;
        if (shared_lookup.findFieldByName(sctx.base.graph, arm_type_id, field_name, &sctx.base.graph_index.scope)) |field_id| {
            _ = try sctx.base.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.base.caller_id, .target_id = field_id, .edge_type = .accesses_field });
        }
    }
    return true;
}

/// Resolve a bare enum literal (`.variant`) to its parent enum type and emit
/// accesses_field. Infers the expected type from the AST context and emits a
/// worklist entry when the type cannot be determined statically.
fn handleBareEnumLiteral(allocator: std.mem.Allocator, sctx: *const ScanContext, field_expr: ts.Node) !void {
    const name_node = field_expr.namedChild(0) orelse return;
    const nk = name_node.kindId();
    if (nk != sctx.k.identifier and nk != sctx.k.property_identifier) return;
    const variant_name = ts_api.nodeText(sctx.base.source, name_node);

    if (inferExpectedType(sctx, field_expr)) |type_id| {
        if (shared_lookup.findFieldByName(sctx.base.graph, type_id, variant_name, &sctx.base.graph_index.scope)) |field_id| {
            _ = try sctx.base.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.base.caller_id, .target_id = field_id, .edge_type = .accesses_field });
            return;
        }
    }

    const pos = field_expr.startPoint();
    try sctx.base.appendWorklist(allocator, pos, .type_definition, variant_name);
}

/// Walk up from a bare enum literal to determine the expected type from context.
/// Returns the type node ID when the AST position provides enough information.
fn inferExpectedType(sctx: *const ScanContext, field_expr: ts.Node) ?NodeId {
    const parent = field_expr.parent() orelse return null;
    const pk = parent.kindId();

    if (pk == sctx.k.binary_expression) {
        return inferTypeFromBinaryPeer(sctx, parent, field_expr);
    }
    if (pk == sctx.k.switch_case) {
        return inferTypeFromSwitchOperand(sctx, parent);
    }
    if (pk == sctx.k.return_expression) {
        return sctx.base.type_env.local.get("_return");
    }
    if (pk == sctx.k.variable_declaration) {
        const var_name = ast.getIdentifierName(sctx.base.source, parent, sctx.k) orelse return null;
        return sctx.base.type_env.local.get(var_name);
    }
    if (pk == sctx.k.expression_statement) {
        const lhs = parent.child(0) orelse return null;
        if (lhs.kindId() != sctx.k.identifier) return null;
        return sctx.base.type_env.local.get(ts_api.nodeText(sctx.base.source, lhs));
    }
    if (pk == sctx.k.arguments) {
        return inferTypeFromCallArg(sctx, parent, field_expr);
    }
    return null;
}

/// Find the type of the other operand in a binary_expression.
fn inferTypeFromBinaryPeer(sctx: *const ScanContext, binary_node: ts.Node, enum_literal: ts.Node) ?NodeId {
    var i: u32 = 0;
    while (i < binary_node.namedChildCount()) : (i += 1) {
        const child = binary_node.namedChild(i) orelse continue;
        if (child.startByte() == enum_literal.startByte()) continue;
        return resolveExprType(sctx, child);
    }
    return null;
}

/// Walk from a switch_case up to the switch_expression and resolve the operand type.
fn inferTypeFromSwitchOperand(sctx: *const ScanContext, switch_case_node: ts.Node) ?NodeId {
    const switch_expr = switch_case_node.parent() orelse return null;
    if (switch_expr.kindId() != sctx.k.switch_expression) return null;
    const operand = switch_expr.namedChild(0) orelse return null;
    return resolveExprType(sctx, operand);
}

/// Find the callee's parameter type at the argument position of the literal.
fn inferTypeFromCallArg(sctx: *const ScanContext, args_node: ts.Node, enum_literal: ts.Node) ?NodeId {
    var arg_index: ?u32 = null;
    var ai: u32 = 0;
    while (ai < args_node.namedChildCount()) : (ai += 1) {
        const arg = args_node.namedChild(ai) orelse continue;
        if (arg.startByte() == enum_literal.startByte()) {
            arg_index = ai;
            break;
        }
    }
    const idx = arg_index orelse return null;

    const call_node = args_node.parent() orelse return null;
    if (call_node.kindId() != sctx.k.call_expression) return null;
    const fn_ref = call_node.namedChild(0) orelse return null;

    const is_method = fn_ref.kindId() == sctx.k.field_expression;
    const callee_id = resolveCalleeNodeId(sctx, fn_ref) orelse return null;
    const param_offset: u32 = if (is_method) 1 else 0;
    return resolveParamType(sctx, callee_id, idx + param_offset);
}

/// Resolve an expression AST node to its type node ID.
/// Handles identifiers (type_env lookup) and field_expression chains
/// (resolve root via type_env, find field, look up field type).
fn resolveExprType(sctx: *const ScanContext, expr: ts.Node) ?NodeId {
    if (expr.kindId() == sctx.k.identifier) {
        return sctx.base.type_env.local.get(ts_api.nodeText(sctx.base.source, expr));
    }
    if (expr.kindId() == sctx.k.field_expression) {
        var chain: [cf.max_chain_depth][]const u8 = undefined;
        const chain_len = cf.collectFieldExprChain(sctx.base.source, expr, &chain, sctx.k);
        if (chain_len >= 2) {
            const root_type_id = sctx.base.type_env.local.get(chain[0]) orelse return null;
            const field_id = shared_lookup.findFieldByName(sctx.base.graph, root_type_id, chain[chain_len - 1], &sctx.base.graph_index.scope) orelse return null;
            return sctx.field_types.get(field_id);
        }
    }
    return null;
}

/// Resolve a call_expression's fn_ref to the callee's graph node ID.
fn resolveCalleeNodeId(sctx: *const ScanContext, fn_ref: ts.Node) ?NodeId {
    const ref = extractCallFnRef(sctx.base.source, fn_ref, sctx.k) orelse return null;
    switch (ref) {
        .bare => |callee_name| {
            return findFunctionByNameScoped(sctx.base.graph, callee_name, sctx.base.edge_ctx.scope_start, sctx.base.edge_ctx.scope_end, sctx.base.caller_parent_id, &sctx.base.graph_index.scope);
        },
        .qualified => |field_expr| {
            var chain: [cf.max_chain_depth][]const u8 = undefined;
            const chain_len = cf.collectFieldExprChain(sctx.base.source, field_expr, &chain, sctx.k);
            if (chain_len >= 2) {
                const root_name = chain[0];
                const leaf_name = chain[chain_len - 1];
                if (sctx.base.type_env.local.get(root_name)) |type_id| {
                    for (sctx.base.graph_index.scope.childrenOf(type_id)) |child_idx| {
                        const n = sctx.base.graph.nodes.items[child_idx];
                        if (n.kind == .function and std.mem.eql(u8, n.name, leaf_name)) {
                            return @enumFromInt(child_idx);
                        }
                    }
                }
            }
            return null;
        },
    }
}

/// Resolve the Nth parameter's type via graph traversal. Walks the callee's
/// `.parameter` children (via ScopeIndex) and looks up the type in NodeTypeMap.
fn resolveParamType(sctx: *const ScanContext, callee_id: NodeId, param_index: u32) ?NodeId {
    const children = sctx.base.graph_index.scope.childrenOf(callee_id);
    var pi: u32 = 0;
    for (children) |ci| {
        const n = sctx.base.graph.nodes.items[ci];
        if (n.kind != .parameter) continue;
        if (pi == param_index) {
            return sctx.field_types.get(@enumFromInt(ci));
        }
        pi += 1;
    }
    return null;
}

/// Resolve `name` to a type node: try the caller's scope first, then the
/// cross-file import + phantom registry. Returns null when the name does not
/// designate a type.
fn lookupAsType(sctx: *const ScanContext, name: []const u8) ?NodeId {
    return findTypeByNameScoped(sctx.base.graph, name, sctx.base.edge_ctx.scope_start, sctx.base.edge_ctx.scope_end, sctx.base.caller_parent_id, &sctx.base.graph_index.scope) orelse
        shared_lookup.findTypeCrossFile(sctx.base.graph, name, sctx.base.edge_ctx, &sctx.base.graph_index.scope, sctx.base.phantom_mgr);
}

/// Dispatch an identifier into the correct edge kind. If the name resolves to
/// a type, emit `uses_type`. Otherwise, when the identifier sits in
/// value-taking syntactic position, route to `emitValueUse`.
fn handleIdentifier(allocator: std.mem.Allocator, sctx: *const ScanContext, id_node: ts.Node) !void {
    const name = ts_api.nodeText(sctx.base.source, id_node);
    if (lookupAsType(sctx, name)) |target_id| {
        _ = try sctx.base.emitEdge(allocator, target_id, .uses_type);
        return;
    }
    if (!isValuePosition(id_node, sctx.k)) return;
    _ = try sctx.base.emitValueUse(allocator, name, &.{});
}

/// True when `node` sits in a syntactic position that consumes its value:
/// operand of an address-of unary, call argument, RHS of an assignment or
/// variable declaration, or value of a return expression.
fn isValuePosition(node: ts.Node, k: *const KindIds) bool {
    const parent = node.parent() orelse return false;
    const pk = parent.kindId();
    if (pk == k.unary_expression) return unaryOpIs(parent, k.amp_op);
    if (pk == k.arguments or pk == k.return_expression) return true;
    if (pk == k.variable_declaration) return shared_scan.matchesParentField(parent, node, "value");
    if (pk == k.assignment_expression) return shared_scan.matchesParentField(parent, node, "right");
    return false;
}

/// True when `unary_node`'s leading operator token has the given kind id.
fn unaryOpIs(unary_node: ts.Node, op_kid: u16) bool {
    const first = unary_node.child(0) orelse return false;
    return first.kindId() == op_kid;
}

/// Resolve a `field_expression` in value position to a function and emit
/// `.uses_value`. Dispatches on the receiver shape: an anonymous
/// `struct_declaration` literal, an in-scope import root, or a local
/// variable bound through TypeEnv. Returns true when the dispatcher
/// handled the field_expression.
fn handleQualifiedValueUse(allocator: std.mem.Allocator, sctx: *const ScanContext, field_expr: ts.Node) !bool {
    if (!isValuePosition(field_expr, sctx.k)) return false;

    const receiver = field_expr.namedChild(0) orelse return false;

    if (receiver.kindId() == sctx.k.struct_declaration) {
        const field_name_node = field_expr.namedChild(1) orelse return false;
        const field_kid = field_name_node.kindId();
        if (field_kid != sctx.k.identifier and field_kid != sctx.k.property_identifier) return false;
        const field_name = shared_ast.stripQuotedIdentifier(ts_api.nodeText(sctx.base.source, field_name_node));
        return try emitAnonStructValueUse(allocator, sctx, receiver, field_name);
    }

    var chain: [cf.max_chain_depth][]const u8 = undefined;
    const chain_len = cf.collectFieldExprChain(sctx.base.source, field_expr, &chain, sctx.k);
    if (chain_len < 2) return false;
    const root = chain[0];

    if (sctx.base.edge_ctx.findImportOrigin(root)) |origin| {
        _ = try sctx.base.resolveOriginCall(allocator, origin, chain[1..chain_len], .uses_value);
        return true;
    }

    if (sctx.base.type_env.local.get(root)) |type_id| {
        return try sctx.base.addResolvedEdges(allocator, type_id, chain[1..chain_len], .uses_value);
    }

    return false;
}

/// Locate a function declared inline as a direct child of an anonymous
/// `struct_declaration` receiver and emit a `.uses_value` edge to it.
fn emitAnonStructValueUse(
    allocator: std.mem.Allocator,
    sctx: *const ScanContext,
    receiver: ts.Node,
    field_name: []const u8,
) !bool {
    var i: u32 = 0;
    while (i < receiver.childCount()) : (i += 1) {
        const child = receiver.child(i) orelse continue;
        if (child.kindId() != sctx.k.function_declaration) continue;
        const child_name = shared_ast.getIdentifierName(sctx.base.source, child, sctx.k.identifier) orelse continue;
        if (!std.mem.eql(u8, child_name, field_name)) continue;
        const line: u32 = @intCast(child.startPoint().row + 1);
        const fn_id = shared_lookup.findNodeByNameAndLine(
            sctx.base.graph,
            child_name,
            line,
            sctx.base.edge_ctx.scope_start,
            sctx.base.edge_ctx.scope_end,
            &.{.function},
        ) orelse continue;
        return try sctx.base.emitEdge(allocator, fn_id, .uses_value);
    }
    return false;
}

/// Emit uses_type and accesses_field for a variable_declaration whose RHS is a struct literal.
/// The type binding was established by buildTypeEnvFromBlock; we look it up here by variable name.
fn handleStructLiteral(allocator: std.mem.Allocator, sctx: *const ScanContext, var_decl: ts.Node) !void {
    const var_name = ast.getIdentifierName(sctx.base.source, var_decl, sctx.k) orelse return;
    const type_id = sctx.base.type_env.local.get(var_name) orelse return;
    _ = try sctx.base.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.base.caller_id, .target_id = type_id, .edge_type = .uses_type });
    var fields: [max_struct_fields][]const u8 = undefined;
    const field_count = collectStructLiteralFields(sctx.base.source, var_decl, &fields);
    for (fields[0..field_count]) |field_name| {
        if (shared_lookup.findFieldByName(sctx.base.graph, type_id, field_name, &sctx.base.graph_index.scope)) |field_id| {
            _ = try sctx.base.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.base.caller_id, .target_id = field_id, .edge_type = .accesses_field });
        }
    }
}

/// Emit uses_type and accesses_field for an anonymous struct literal in return position.
fn handleReturnStructLiteral(allocator: std.mem.Allocator, sctx: *const ScanContext, return_node: ts.Node) !void {
    const return_type_id = sctx.base.type_env.local.get("_return") orelse return;
    const ret_expr = return_node.namedChild(0) orelse return;
    const s = ret_expr.startByte();
    const e = ret_expr.endByte();
    if (s >= e or e > sctx.base.source.len) return;
    if (!textStartsWithAnonLiteral(sctx.base.source[s..e])) return;
    _ = try sctx.base.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.base.caller_id, .target_id = return_type_id, .edge_type = .uses_type });
    var fields: [max_struct_fields][]const u8 = undefined;
    const field_count = collectStructLiteralFields(sctx.base.source, return_node, &fields);
    for (fields[0..field_count]) |field_name| {
        if (shared_lookup.findFieldByName(sctx.base.graph, return_type_id, field_name, &sctx.base.graph_index.scope)) |field_id| {
            _ = try sctx.base.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.base.caller_id, .target_id = field_id, .edge_type = .accesses_field });
        }
    }
}

/// Maximum recursion depth for comptime-alias expansion.
const max_alias_depth: u8 = 4;

/// Traverse a chain starting from a known type scope, following field -> type
/// transitions via the field_types map. Returns true if the leaf function was
/// found and a calls edge was emitted.
fn resolveLocalChain(
    allocator: std.mem.Allocator,
    sctx: *const ScanContext,
    start_type_id: NodeId,
    chain: []const []const u8,
) !bool {
    return resolveLocalChainWithDepth(allocator, sctx, start_type_id, chain, 0);
}

fn resolveLocalChainWithDepth(
    allocator: std.mem.Allocator,
    sctx: *const ScanContext,
    start_type_id: NodeId,
    chain: []const []const u8,
    depth: u8,
) !bool {
    var current_scope = start_type_id;
    for (chain, 0..) |segment, seg_idx| {
        if (try tryExpandComptimeAlias(allocator, sctx, current_scope, chain[seg_idx..], depth)) |any_resolved| {
            return any_resolved;
        }

        const is_last = seg_idx == chain.len - 1;
        const matched_id = findChildByName(sctx.base.graph, &sctx.base.graph_index.scope, current_scope, segment) orelse return false;
        const matched_node = sctx.base.graph.nodes.items[@intFromEnum(matched_id)];

        if (is_last and matched_node.kind == .function) {
            _ = try sctx.base.graph.addEdgeIfNew(allocator, .{
                .source_id = sctx.base.caller_id,
                .target_id = matched_id,
                .edge_type = .calls,
            });
            return true;
        }

        if (matched_node.kind.isTypeContainer()) {
            current_scope = matched_id;
        } else if (matched_node.kind == .field) {
            current_scope = sctx.field_types.get(matched_id) orelse return false;
        } else if (matched_node.kind == .function) {
            _ = try sctx.base.graph.addEdgeIfNew(allocator, .{
                .source_id = sctx.base.caller_id,
                .target_id = matched_id,
                .edge_type = .calls,
            });
            current_scope = cf.resolveReturnTypeScope(sctx.base.graph, matched_id, sctx.base.graph_index) orelse return false;
        } else if (matched_node.kind == .constant) {
            current_scope = matched_id;
        } else {
            return false;
        }
    }
    return false;
}

/// If `scope_id` names a `.constant` carrying `comptime_switch_arm_names`,
/// fan out the remaining `chain` over each candidate type and emit `.calls`
/// edges for every leaf that resolves. Returns null when the node is not a
/// comptime alias, true when at least one candidate resolved the chain, and
/// false when no candidate matched.
fn tryExpandComptimeAlias(
    allocator: std.mem.Allocator,
    sctx: *const ScanContext,
    scope_id: NodeId,
    chain: []const []const u8,
    depth: u8,
) error{OutOfMemory}!?bool {
    if (chain.len == 0) return null;
    if (depth >= max_alias_depth) return null;
    const node = sctx.base.graph.nodes.items[@intFromEnum(scope_id)];
    if (node.kind != .constant) return null;
    const zm = zig_meta.metaOf(&node) orelse return null;
    const arm_names = zm.comptime_switch_arm_names orelse return null;
    if (arm_names.len == 0) return null;

    var any_resolved = false;
    for (arm_names) |arm_name| {
        if (std.mem.eql(u8, arm_name, node.name)) continue;
        const arm_type_id = findTypeByNameScoped(
            sctx.base.graph,
            arm_name,
            sctx.base.edge_ctx.scope_start,
            sctx.base.edge_ctx.scope_end,
            sctx.base.caller_parent_id,
            &sctx.base.graph_index.scope,
        ) orelse continue;
        if (arm_type_id == scope_id) continue;
        if (try resolveLocalChainWithDepth(allocator, sctx, arm_type_id, chain, depth + 1)) {
            any_resolved = true;
        }
    }
    return any_resolved;
}

/// Traverse a chain from a known type scope to resolve the final type.
/// Used by if-capture and field access to determine the type at the end
/// of a multi-segment field expression. When the walk crosses through a
/// comptime switch alias with remaining segments to resolve, fans out
/// across each candidate type and returns the first whose subchain
/// resolves to a type.
fn resolveChainToType(
    g: *const Graph,
    start_type_id: NodeId,
    chain: []const []const u8,
    field_types: *const NodeTypeMap,
    scope_index: *const ScopeIndex,
    scope_start: usize,
    scope_end: usize,
    caller_parent_id: ?NodeId,
) ?NodeId {
    return resolveChainToTypeWithDepth(g, start_type_id, chain, field_types, scope_index, scope_start, scope_end, caller_parent_id, 0);
}

fn resolveChainToTypeWithDepth(
    g: *const Graph,
    start_type_id: NodeId,
    chain: []const []const u8,
    field_types: *const NodeTypeMap,
    scope_index: *const ScopeIndex,
    scope_start: usize,
    scope_end: usize,
    caller_parent_id: ?NodeId,
    depth: u8,
) ?NodeId {
    var current = start_type_id;
    for (chain, 0..) |segment, seg_idx| {
        if (tryExpandComptimeAliasToType(g, current, chain[seg_idx..], field_types, scope_index, scope_start, scope_end, caller_parent_id, depth)) |resolved| {
            return resolved;
        }
        const child_id = findChildByName(g, scope_index, current, segment) orelse return null;
        const child_node = g.nodes.items[@intFromEnum(child_id)];
        if (child_node.kind.isTypeContainer()) {
            current = child_id;
        } else if (child_node.kind == .field) {
            current = field_types.get(child_id) orelse return null;
        } else if (child_node.kind == .constant) {
            current = child_id;
        } else {
            return null;
        }
    }
    return current;
}

/// If `scope_id` names a `.constant` carrying `comptime_switch_arm_names`,
/// fan out the remaining `chain` over each candidate type and return the
/// first candidate whose subchain resolves to a type. Returns null when
/// the node is not a comptime alias or no candidate resolved the chain.
fn tryExpandComptimeAliasToType(
    g: *const Graph,
    scope_id: NodeId,
    chain: []const []const u8,
    field_types: *const NodeTypeMap,
    scope_index: *const ScopeIndex,
    scope_start: usize,
    scope_end: usize,
    caller_parent_id: ?NodeId,
    depth: u8,
) ?NodeId {
    if (chain.len == 0) return null;
    if (depth >= max_alias_depth) return null;
    const node = g.nodes.items[@intFromEnum(scope_id)];
    if (node.kind != .constant) return null;
    const zm = zig_meta.metaOf(&node) orelse return null;
    const arm_names = zm.comptime_switch_arm_names orelse return null;
    if (arm_names.len == 0) return null;

    for (arm_names) |arm_name| {
        if (std.mem.eql(u8, arm_name, node.name)) continue;
        const arm_type_id = findTypeByNameScoped(g, arm_name, scope_start, scope_end, caller_parent_id, scope_index) orelse continue;
        if (arm_type_id == scope_id) continue;
        if (resolveChainToTypeWithDepth(g, arm_type_id, chain, field_types, scope_index, scope_start, scope_end, caller_parent_id, depth + 1)) |resolved| {
            return resolved;
        }
    }
    return null;
}

/// Find a direct child of scope_id with the given name.
fn findChildByName(
    g: *const Graph,
    scope_index: *const @import("../../core/scope_index.zig").ScopeIndex,
    scope_id: NodeId,
    name: []const u8,
) ?NodeId {
    for (scope_index.childrenOf(scope_id)) |child_idx| {
        if (std.mem.eql(u8, g.nodes.items[child_idx].name, name)) {
            return @enumFromInt(child_idx);
        }
    }
    return null;
}

/// Extract the leaf identifier position from a field_expression for LSP queries.
/// For `self.field.method`, returns the position of "method".
fn leafIdentifierPos(field_expr: ts.Node, fallback: ts.Node, k: *const KindIds) ts.Point {
    const count = field_expr.namedChildCount();
    if (count >= 2) {
        if (field_expr.namedChild(count - 1)) |last| {
            if (last.kindId() == k.property_identifier or last.kindId() == k.identifier) {
                return last.startPoint();
            }
        }
    }
    return fallback.startPoint();
}

/// Normalized form of the function reference extracted from a call_expression's first named child.
const CallFnRef = union(enum) {
    bare: []const u8,
    qualified: ts.Node,
};

/// Extract and normalize the function reference from a call_expression's first named child.
/// Handles identifier, field_expression, and error_union_type (recurses into the inner node).
fn extractCallFnRef(source: []const u8, fn_ref: ts.Node, k: *const KindIds) ?CallFnRef {
    const kid = fn_ref.kindId();
    if (kid == k.identifier) return .{ .bare = ts_api.nodeText(source, fn_ref) };
    if (kid == k.field_expression) return .{ .qualified = fn_ref };
    if (kid == k.error_union_type) {
        if (fn_ref.namedChild(0)) |inner| return extractCallFnRef(source, inner, k);
    }
    return null;
}

/// Extract the import-qualified type chain from a parameter type AST node.
/// Handles wrappers: `svc_mod.Service`, `*svc_mod.Service`, `?svc_mod.Service`,
/// `*const svc_mod.Service`. Returns the segment count written into `chain`.
fn extractParamTypeChain(
    source: []const u8,
    type_node: ts.Node,
    chain: *[cf.max_chain_depth][]const u8,
    k: *const KindIds,
) usize {
    const kid = type_node.kindId();

    if (kid == k.field_expression) {
        const len = cf.collectFieldExprChain(source, type_node, chain, k);
        if (len >= 2) return len;

        const first_child = type_node.child(0) orelse return len;
        const unwrapped = unwrapTypeNode(first_child, k);
        if (unwrapped.kindId() == k.identifier and len == 1) {
            chain[1] = chain[0];
            chain[0] = ts_api.nodeText(source, unwrapped);
            return 2;
        }
        return len;
    }

    if (kid == k.pointer_type or
        kid == k.nullable_type or
        kid == k.optional_type or
        kid == k.error_union_type)
    {
        const count = type_node.namedChildCount();
        if (count > 0) {
            if (type_node.namedChild(count - 1)) |inner| {
                return extractParamTypeChain(source, inner, chain, k);
            }
        }
        return 0;
    }

    return 0;
}

/// Unwrap pointer_type, optional_type, and error_union_type AST wrappers to get the base type node.
/// For bare identifiers, returns unchanged.
fn unwrapTypeNode(type_node: ts.Node, k: *const KindIds) ts.Node {
    const kid = type_node.kindId();
    if (kid == k.pointer_type) {
        const count = type_node.namedChildCount();
        if (count > 0) {
            if (type_node.namedChild(count - 1)) |inner| return unwrapTypeNode(inner, k);
        }
        return type_node;
    }
    if (kid == k.nullable_type or kid == k.optional_type) {
        if (type_node.namedChild(0)) |inner| return unwrapTypeNode(inner, k);
        return type_node;
    }
    if (kid == k.error_union_type) {
        const count = type_node.namedChildCount();
        if (count > 0) {
            if (type_node.namedChild(count - 1)) |inner| return unwrapTypeNode(inner, k);
        }
        return type_node;
    }
    return type_node;
}

/// Return the child node immediately after the ':' in a variable_declaration.
/// That node is the explicit type annotation, if any.
fn extractAnnotationTypeNode(var_decl: ts.Node, k: *const KindIds) ?ts.Node {
    var found_colon = false;
    var i: u32 = 0;
    while (i < var_decl.childCount()) : (i += 1) {
        const child = var_decl.child(i) orelse continue;
        if (found_colon) return child;
        if (child.kindId() == k.colon) found_colon = true;
    }
    return null;
}

/// Returns true when a source text slice starts with an anonymous struct literal after whitespace.
/// The Zig grammar has no distinct node kind for `.{...}`, so detection is text-based.
fn textStartsWithAnonLiteral(text: []const u8) bool {
    var pos: usize = 0;
    while (pos < text.len and source_scan.isWhitespace(text[pos])) : (pos += 1) {}
    return pos + 1 < text.len and text[pos] == '.' and text[pos + 1] == '{';
}

/// Returns true when the RHS of a variable declaration is an anonymous struct literal.
fn hasAnonymousStructLiteralRHS(source: []const u8, var_decl: ts.Node) bool {
    const start = var_decl.startByte();
    const end = var_decl.endByte();
    if (start >= end or end > source.len) return false;
    const text = source[start..end];
    const eq_pos = std.mem.indexOfScalar(u8, text, '=') orelse return false;
    return textStartsWithAnonLiteral(text[eq_pos + 1 ..]);
}

/// Return the call_expression node from a variable_declaration's initializer, if the RHS
/// is a direct call or a `try`-wrapped call. Returns null for struct literals and other forms.
fn findVarDeclCallExpr(var_decl: ts.Node, k: *const KindIds) ?ts.Node {
    var past_name = false;
    var i: u32 = 0;
    while (i < var_decl.namedChildCount()) : (i += 1) {
        const child = var_decl.namedChild(i) orelse continue;
        const kid = child.kindId();
        if (!past_name and kid == k.identifier) {
            past_name = true;
            continue;
        }
        if (kid == k.call_expression) return child;
        if (kid == k.try_expression) {
            var j: u32 = 0;
            while (j < child.namedChildCount()) : (j += 1) {
                const inner = child.namedChild(j) orelse continue;
                if (inner.kindId() == k.call_expression) return inner;
            }
        }
    }
    return null;
}

/// Given a call_expression node, resolve the return type of the callee to a graph NodeId.
/// Handles bare identifier calls and field-expression method calls. Returns null when the
/// callee or its return type cannot be determined. For @This() return types, falls back
/// to the function's containing type.
fn resolveCallResultType(
    g: *const Graph,
    source: []const u8,
    call_node: ts.Node,
    type_env: *const TypeEnv,
    scope_start: usize,
    scope_end: usize,
    caller_parent_id: ?NodeId,
    k: *const KindIds,
    graph_index: *const GraphIndex,
) ?NodeId {
    const fn_ref = call_node.namedChild(0) orelse return null;
    const ref_kid = fn_ref.kindId();

    var fn_id: ?NodeId = null;

    if (ref_kid == k.identifier) {
        const name = ts_api.nodeText(source, fn_ref);
        fn_id = findFunctionByNameScoped(g, name, scope_start, scope_end, caller_parent_id, &graph_index.scope);
    } else if (ref_kid == k.field_expression) {
        var chain: [cf.max_chain_depth][]const u8 = undefined;
        const chain_len = cf.collectFieldExprChain(source, fn_ref, &chain, k);
        if (chain_len >= 2) {
            const root_name = chain[0];
            const method_name = chain[chain_len - 1];
            if (type_env.local.get(root_name)) |type_id| {
                for (graph_index.scope.childrenOf(type_id)) |child_idx| {
                    const n = g.nodes.items[child_idx];
                    if (n.kind == .function and std.mem.eql(u8, n.name, method_name)) {
                        fn_id = @enumFromInt(child_idx);
                        break;
                    }
                }
            }
        }
    }

    const fid = fn_id orelse return null;

    if (cf.resolveReturnTypeScope(g, fid, graph_index)) |ret_id| return ret_id;

    if (g.getNode(fid)) |fn_node| {
        if (fn_node.signature) |sig| {
            if (std.mem.indexOf(u8, sig, "@This()") != null) return fn_node.parent_id;
        }
    }

    return null;
}

/// Extract the type name from a variable_declaration whose value is a struct
/// literal (`Point{ .x = 3 }`) or a static method call (`Builder.init(5)`).
/// Returns the PascalCase type name as a slice of `source`, or null.
fn extractStructLiteralType(source: []const u8, var_decl: ts.Node) ?[]const u8 {
    const start = var_decl.startByte();
    const end = var_decl.endByte();
    if (start >= end or end > source.len) return null;
    const text = source[start..end];

    const eq_pos = std.mem.indexOfScalar(u8, text, '=') orelse return null;
    var pos = eq_pos + 1;

    while (pos < text.len and source_scan.isWhitespace(text[pos])) : (pos += 1) {}

    if (pos + 3 <= text.len and std.mem.eql(u8, text[pos..][0..3], "try") and
        (pos + 3 >= text.len or !source_scan.isIdentChar(text[pos + 3])))
    {
        pos += 3;
        while (pos < text.len and source_scan.isWhitespace(text[pos])) : (pos += 1) {}
    }

    if (pos >= text.len or text[pos] < 'A' or text[pos] > 'Z') return null;
    const id_start = pos;
    while (pos < text.len and source_scan.isIdentChar(text[pos])) : (pos += 1) {}
    if (pos >= text.len) return null;

    if (text[pos] != '{' and text[pos] != '.') return null;

    return source[start + id_start .. start + pos];
}

/// Collect field names from a struct literal initializer in the source text.
/// Given a variable_declaration node whose RHS is `Type{ .a = x, .b = y }`,
/// returns the field names (a, b) as slices of source. Returns the count
/// of fields written into `out`.
fn collectStructLiteralFields(source: []const u8, var_decl: ts.Node, out: *[max_struct_fields][]const u8) usize {
    const start = var_decl.startByte();
    const end = var_decl.endByte();
    if (start >= end or end > source.len) return 0;
    const text = source[start..end];

    const brace_pos = std.mem.indexOfScalar(u8, text, '{') orelse return 0;
    var pos = brace_pos + 1;
    var count: usize = 0;

    while (pos < text.len and count < max_struct_fields) {
        while (pos < text.len and (source_scan.isWhitespace(text[pos]) or text[pos] == ',')) : (pos += 1) {}
        if (pos >= text.len or text[pos] == '}') break;
        if (text[pos] != '.') {
            while (pos < text.len and text[pos] != ',' and text[pos] != '}') : (pos += 1) {}
            continue;
        }
        pos += 1;
        const field_start = pos;
        while (pos < text.len and source_scan.isIdentChar(text[pos])) : (pos += 1) {}
        if (pos > field_start) {
            out[count] = source[start + field_start .. start + pos];
            count += 1;
        }
        var depth: u32 = 0;
        while (pos < text.len) : (pos += 1) {
            if (text[pos] == '{') {
                depth += 1;
            } else if (text[pos] == '}') {
                if (depth == 0) break;
                depth -= 1;
            } else if (text[pos] == ',' and depth == 0) {
                break;
            }
        }
    }
    return count;
}

/// Context for scanning container field type annotations.
const FieldScanContext = struct {
    g: *Graph,
    source: []const u8,
    owner_id: NodeId,
    field_id: ?NodeId = null,
    edge_ctx: *const EdgeContext,
    k: *const KindIds,
    graph_index: *const GraphIndex,
    phantom_mgr: *const PhantomManager,
    field_types: *NodeTypeMap,
    wl: *LspWorklist,
    log: Logger,
};

/// Scan a struct/enum/union node's container_field children for type references.
/// Emits uses_type edges from the container node and from each field node to the
/// resolved type.
fn processContainerFieldEdges(
    allocator: std.mem.Allocator,
    g: *Graph,
    source: []const u8,
    container_node: ts.Node,
    owner_id: NodeId,
    ctx: *const EdgeContext,
    k: *const KindIds,
    graph_index: *const GraphIndex,
    phantom_mgr: *const PhantomManager,
    field_types: *NodeTypeMap,
    wl: *LspWorklist,
    log: Logger,
) !void {
    var fctx = FieldScanContext{
        .g = g,
        .source = source,
        .owner_id = owner_id,
        .edge_ctx = ctx,
        .k = k,
        .graph_index = graph_index,
        .phantom_mgr = phantom_mgr,
        .field_types = field_types,
        .wl = wl,
        .log = log,
    };
    var i: u32 = 0;
    while (i < container_node.namedChildCount()) : (i += 1) {
        const child = container_node.namedChild(i) orelse continue;
        if (child.kindId() == k.container_field) {
            const field_name = ast.getIdentifierName(source, child, k);
            fctx.field_id = if (field_name) |fn_name|
                shared_lookup.findFieldByName(g, owner_id, fn_name, &graph_index.scope)
            else
                null;
            try scanContainerField(allocator, &fctx, child);
        }
    }
}

/// Skip the field name (first identifier named child) and scan the type annotation.
fn scanContainerField(allocator: std.mem.Allocator, fctx: *const FieldScanContext, field_node: ts.Node) !void {
    var skipped_name = false;
    var i: u32 = 0;
    while (i < field_node.namedChildCount()) : (i += 1) {
        const child = field_node.namedChild(i) orelse continue;
        if (!skipped_name and child.kindId() == fctx.k.identifier) {
            skipped_name = true;
            continue;
        }
        try scanFieldTypeNode(allocator, fctx, child);
    }
}

/// Recursively walk a type annotation AST node for type identifier references.
/// Skips builtin primitives; handles qualified types; recurses into wrappers.
fn scanFieldTypeNode(allocator: std.mem.Allocator, fctx: *const FieldScanContext, node: ts.Node) !void {
    const kid = node.kindId();
    const k = fctx.k;
    if (kid == k.builtin_type or kid == k.builtin_function or kid == k.builtin_identifier) return;
    if (kid == k.identifier or kid == k.property_identifier) {
        try emitFieldTypeRef(allocator, fctx, node);
        return;
    }
    if (kid == k.field_expression) {
        try handleFieldQualifiedType(allocator, fctx, node);
        return;
    }
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        try scanFieldTypeNode(allocator, fctx, child);
    }
}

/// Emit uses_type from the container to the resolved type, or append a worklist
/// entry when the name is PascalCase and could not be resolved locally.
fn emitFieldTypeRef(allocator: std.mem.Allocator, fctx: *const FieldScanContext, id_node: ts.Node) !void {
    const name = ts_api.nodeText(fctx.source, id_node);
    const owner_node = fctx.g.getNode(fctx.owner_id);
    const owner_parent_id: ?NodeId = if (owner_node) |n| n.parent_id else null;
    const target_id =
        findTypeByNameScoped(fctx.g, name, fctx.edge_ctx.scope_start, fctx.edge_ctx.scope_end, owner_parent_id, &fctx.graph_index.scope) orelse
        shared_lookup.findTypeCrossFile(fctx.g, name, fctx.edge_ctx, &fctx.graph_index.scope, fctx.phantom_mgr);
    if (target_id) |tid| {
        if (shared_scan.edgeCarriesNewInformation(fctx.g, fctx.owner_id, tid)) {
            _ = try fctx.g.addEdgeIfNew(allocator, .{ .source_id = fctx.owner_id, .target_id = tid, .edge_type = .uses_type });
        }
        if (fctx.field_id) |fid| {
            _ = try fctx.g.addEdgeIfNew(allocator, .{ .source_id = fid, .target_id = tid, .edge_type = .uses_type });
            try fctx.field_types.put(allocator, fid, tid);
        }
        return;
    }
    if (name.len > 0 and name[0] >= 'A' and name[0] <= 'Z') {
        const pos = id_node.startPoint();
        try fctx.wl.append(allocator, .{
            .source_node_id = fctx.owner_id,
            .file_path = if (fctx.g.getNode(fctx.owner_id)) |n| n.file_path orelse "" else "",
            .line = pos.row,
            .col = pos.column,
            .query_kind = .definition,
            .hint_name = name,
        });
    }
}

/// Resolve a field_expression used as a type annotation (e.g. mod.MyType).
/// Resolves through the import map; falls back to a worklist entry when unresolved.
fn handleFieldQualifiedType(allocator: std.mem.Allocator, fctx: *const FieldScanContext, field_expr: ts.Node) !void {
    var chain: [cf.max_chain_depth][]const u8 = undefined;
    const chain_len = cf.collectFieldExprChain(fctx.source, field_expr, &chain, fctx.k);
    if (chain_len < 2) return;
    const root_name = chain[0];
    const type_name = chain[chain_len - 1];
    if (fctx.edge_ctx.findImportTarget(root_name)) |target_file_id| {
        for (fctx.graph_index.scope.childrenOf(target_file_id)) |child_idx| {
            const n = fctx.g.nodes.items[child_idx];
            if (isTypeReference(n, type_name)) {
                const tid: NodeId = @enumFromInt(child_idx);
                _ = try fctx.g.addEdgeIfNew(allocator, .{
                    .source_id = fctx.owner_id,
                    .target_id = tid,
                    .edge_type = .uses_type,
                });
                if (fctx.field_id) |fid| {
                    _ = try fctx.g.addEdgeIfNew(allocator, .{ .source_id = fid, .target_id = tid, .edge_type = .uses_type });
                    try fctx.field_types.put(allocator, fid, tid);
                }
                return;
            }
        }
    }
    const pos = field_expr.startPoint();
    try fctx.wl.append(allocator, .{
        .source_node_id = fctx.owner_id,
        .file_path = if (fctx.g.getNode(fctx.owner_id)) |n| n.file_path orelse "" else "",
        .line = pos.row,
        .col = pos.column,
        .query_kind = .definition,
        .hint_name = type_name,
    });
}

const max_struct_fields = 32;

/// Check whether a graph node represents a type reference with the given name.
/// Matches type containers (type_def, enum_def, union_def), PascalCase constants
/// (type aliases), and PascalCase import_decl nodes.
pub fn isTypeReference(n: @import("../../core/node.zig").Node, name: []const u8) bool {
    if (!std.mem.eql(u8, n.name, name)) return false;
    if (n.kind.isTypeContainer()) return true;
    const is_pascal = n.name.len > 0 and n.name[0] >= 'A' and n.name[0] <= 'Z';
    if (is_pascal and (n.kind == .constant or n.kind == .import_decl)) return true;
    return false;
}

fn findTypeByNameScoped(g: *const Graph, name: []const u8, scope_start: usize, scope_end: usize, caller_parent_id: ?NodeId, scope_index: *const ScopeIndex) ?NodeId {
    return shared_lookup.findTypeByNameScoped(g, name, scope_start, scope_end, caller_parent_id, scope_index, isTypeReference);
}

fn findFunctionByNameScoped(g: *const Graph, name: []const u8, scope_start: usize, scope_end: usize, caller_parent_id: ?NodeId, scope_index: *const ScopeIndex) ?NodeId {
    return shared_lookup.findFunctionByNameScoped(g, name, scope_start, scope_end, caller_parent_id, scope_index, &.{});
}

/// Minimal scan context for walking a constant's initializer with the shared
/// `walkBody` machinery. Carries only the state needed to identify a value-
/// position identifier and emit a `.uses_value` edge. The `dispatch` and
/// `isBoundary` methods satisfy the contract of `shared_scan.walkBody`.
const ConstInitScanContext = struct {
    base: shared_scan.BaseScanContext,
    k: *const KindIds,

    pub fn isBoundary(self: *const @This(), node: ts.Node) bool {
        const kid = node.kindId();
        return kid == self.k.function_declaration or kid == self.k.test_declaration;
    }

    pub fn dispatch(self: *const @This(), allocator: std.mem.Allocator, node: ts.Node, depth: u32) !void {
        _ = depth;
        const kid = node.kindId();
        if (kid != self.k.identifier and kid != self.k.property_identifier) return;
        if (!isValuePosition(node, self.k)) return;
        const name = ts_api.nodeText(self.base.source, node);
        _ = try self.base.emitValueUse(allocator, name, &.{});
    }
};

/// Match a constant declaration to its graph node and scan its initializer
/// for value-position identifiers that name in-scope functions. Each match
/// becomes a `.uses_value` edge from the constant to the function.
fn processConstantInitEdges(
    allocator: std.mem.Allocator,
    io: std.Io,
    g: *Graph,
    source: []const u8,
    var_decl: ts.Node,
    ctx: *const EdgeContext,
    k: *const KindIds,
    graph_index: *const GraphIndex,
    phantom_mgr: *const PhantomManager,
    wl: *LspWorklist,
    log: Logger,
) !void {
    const name = ast.getIdentifierName(source, var_decl, k) orelse return;
    const decl_line = var_decl.startPoint().row + 1;
    const const_id = shared_lookup.findNodeByNameAndLine(g, name, decl_line, ctx.scope_start, ctx.scope_end, &.{.constant}) orelse return;
    const const_parent_id = shared_scan.BaseScanContext.parentOf(g, const_id);

    var type_env = TypeEnv{};
    defer type_env.deinit(allocator);

    const sctx = ConstInitScanContext{
        .base = .{
            .graph = g,
            .source = source,
            .caller_id = const_id,
            .caller_parent_id = const_parent_id,
            .fn_node = var_decl,
            .edge_ctx = ctx,
            .type_env = &type_env,
            .graph_index = graph_index,
            .phantom_mgr = phantom_mgr,
            .wl = wl,
            .io = io,
            .log = log,
            .resolve = .{
                .graph_index = graph_index,
                .io = io,
                .log = log,
                .return_type_resolver = cf.return_type_resolver,
                .find_in_type_scope = null,
            },
        },
        .k = k,
    };
    try shared_scan.walkBody(ConstInitScanContext, allocator, &sctx, var_decl, 0);
}

/// Find a test_def node by name within the given scope range.
/// Returns the first match, or null if no test with that name exists.
fn findTestByName(g: *const Graph, name: []const u8, scope_start: usize, scope_end: usize) ?NodeId {
    const scoped_nodes = g.nodes.items[scope_start..scope_end];
    for (scoped_nodes, scope_start..) |n, i| {
        if (n.kind == .test_def and std.mem.eql(u8, n.name, name)) {
            return @enumFromInt(i);
        }
    }
    return null;
}
