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
const VarTracker = cf.VarTracker;

/// A local variable bound to a type name inferred from its initializer.
const TypeBinding = struct {
    var_name: []const u8,
    type_name: []const u8,
};

/// Tracks local variable bindings from let declarations to their types.
/// Populated during prescan, queried during call resolution.
const LocalTypeTracker = struct {
    bindings: std.ArrayListUnmanaged(TypeBinding) = .empty,

    fn deinit(self: *LocalTypeTracker, allocator: std.mem.Allocator) void {
        self.bindings.deinit(allocator);
    }

    /// Record that local variable `name` was initialized from type `type_name`.
    fn addBinding(self: *LocalTypeTracker, allocator: std.mem.Allocator, name: []const u8, type_name: []const u8) !void {
        try self.bindings.append(allocator, .{ .var_name = name, .type_name = type_name });
    }

    /// Return the type name bound to `name`, or null if not tracked.
    fn findTypeName(self: *const LocalTypeTracker, name: []const u8) ?[]const u8 {
        for (self.bindings.items) |b| {
            if (std.mem.eql(u8, b.var_name, name)) return b.type_name;
        }
        return null;
    }
};

/// A parameter bound to its import-qualified type origin.
const ParamBinding = struct {
    name: []const u8,
    target_file: NodeId,
    type_chain: [cf.max_chain_depth][]const u8 = undefined,
    chain_len: usize = 0,
};

/// Maps function parameter names to their import-qualified type origins.
/// Populated during prescan from parameter declarations whose type is a
/// scoped path (module::Type). Each binding stores the resolved target file
/// NodeId and the remaining member chain for cross-file call resolution.
const ParamTypeTracker = struct {
    bindings: std.ArrayListUnmanaged(ParamBinding) = .empty,

    fn deinit(self: *ParamTypeTracker, allocator: std.mem.Allocator) void {
        self.bindings.deinit(allocator);
    }

    /// Record that parameter `name` has an import-qualified type rooted at
    /// `target_file` with the given member chain. No-op if chain is empty.
    fn addBinding(self: *ParamTypeTracker, allocator: std.mem.Allocator, name: []const u8, target_file: NodeId, chain: []const []const u8) !void {
        if (chain.len == 0) return;
        var entry = ParamBinding{ .name = name, .target_file = target_file };
        const copy_len = @min(chain.len, cf.max_chain_depth);
        for (chain[0..copy_len], 0..) |seg, i| {
            entry.type_chain[i] = seg;
        }
        entry.chain_len = copy_len;
        try self.bindings.append(allocator, entry);
    }

    /// Return the SymbolOrigin for parameter `name`, or null if not tracked.
    fn findOrigin(self: *const ParamTypeTracker, name: []const u8) ?cf.SymbolOrigin {
        for (self.bindings.items) |*b| {
            if (std.mem.eql(u8, b.name, name)) {
                return .{
                    .file_id = b.target_file,
                    .chain = b.type_chain[0..b.chain_len],
                };
            }
        }
        return null;
    }
};

/// Classification of the leftmost receiver in a field_expression chain.
///
/// - `self_receiver`: the identifier is literally "self".
/// - `local_type`: the identifier names a type defined in the current file,
///   or a parameter whose type is a local type.
/// - `external`: none of the above; the receiver comes from outside the file.
const ReceiverKind = enum { self_receiver, local_type, external };

/// Per-function/test scanning context, created once per declaration in
/// processFunction and passed by const pointer to all scan/resolve helpers.
/// Owns no memory; all pointers borrow from the caller's stack or the graph.
const ScanContext = struct {
    graph: *Graph,
    source: []const u8,
    caller_id: NodeId,
    caller_parent_id: ?NodeId,
    fn_node: ts.Node,
    edge_ctx: *const EdgeContext,
    k: *const KindIds,
    graph_index: *const GraphIndex,
    local_tracker: *const LocalTypeTracker,
    var_tracker: *const VarTracker,
    param_tracker: *const ParamTypeTracker,
    log: Logger,
};

/// Resolve a qualified chain and add the resulting edges to the graph.
fn addResolvedEdges(
    allocator: std.mem.Allocator,
    sctx: *const ScanContext,
    target_file_id: NodeId,
    chain: []const []const u8,
    is_call: bool,
) !void {
    var edge_buf: [cf.max_chain_depth]cf.ResolvedEdge = undefined;
    const edge_count = cf.resolveQualifiedCall(
        sctx.graph,
        target_file_id,
        chain,
        is_call,
        sctx.graph_index,
        sctx.log,
        &edge_buf,
    );
    for (edge_buf[0..edge_count]) |edge| {
        _ = try sctx.graph.addEdgeIfNew(allocator, .{
            .source_id = sctx.caller_id,
            .target_id = edge.target_id,
            .edge_type = edge.edge_type,
        });
    }
}

/// Walk the AST to discover edges (calls, uses_type, implements).
/// Processes function bodies for call expressions and type references.
/// Also processes impl blocks for implements edges.
pub fn walkForEdges(
    allocator: std.mem.Allocator,
    graph: *Graph,
    source: []const u8,
    root: ts.Node,
    k: *const KindIds,
    edge_ctx: *const EdgeContext,
    graph_index: *const GraphIndex,
    log: Logger,
) !void {
    var i: u32 = 0;
    while (i < root.childCount()) : (i += 1) {
        const child = root.child(i) orelse continue;
        const kid = child.kindId();

        if (kid == k.function_item) {
            try processFunction(allocator, graph, source, child, k, edge_ctx, graph_index, log);
        } else if (kid == k.impl_item) {
            try processImpl(allocator, graph, source, child, k, edge_ctx, graph_index, log);
        } else if (kid == k.mod_item) {
            try processInlineMod(allocator, graph, source, child, k, edge_ctx, graph_index, log);
        } else if (kid == k.struct_item or kid == k.enum_item or kid == k.union_item) {
            try processStructOrEnum(allocator, graph, source, child, k, edge_ctx, graph_index, log);
        }
    }
}

/// Process a function_item: scan its body for call expressions and type references.
fn processFunction(
    allocator: std.mem.Allocator,
    graph: *Graph,
    source: []const u8,
    fn_node: ts.Node,
    k: *const KindIds,
    edge_ctx: *const EdgeContext,
    graph_index: *const GraphIndex,
    log: Logger,
) !void {
    const fn_name = findFunctionName(source, fn_node, k) orelse return;
    const fn_line = fn_node.startPoint().row + 1;
    const caller_id = findNodeByNameAndLine(graph, fn_name, fn_line, edge_ctx.scope_start, edge_ctx.scope_end) orelse return;
    const caller_parent_id = graph.getNode(caller_id).?.parent_id;

    var param_tracker = ParamTypeTracker{};
    defer param_tracker.deinit(allocator);
    try prescanForParamTypeBindings(allocator, source, fn_node, k, edge_ctx, &param_tracker);

    var local_tracker = LocalTypeTracker{};
    defer local_tracker.deinit(allocator);
    try prescanBlock(allocator, source, fn_node, k, &local_tracker);

    var var_tracker = VarTracker{};
    defer var_tracker.deinit(allocator);
    try prescanBlockForVarBindings(allocator, graph, source, fn_node, k, edge_ctx, graph_index, &var_tracker, log);

    const sctx = ScanContext{
        .graph = graph,
        .source = source,
        .caller_id = caller_id,
        .caller_parent_id = caller_parent_id,
        .fn_node = fn_node,
        .edge_ctx = edge_ctx,
        .k = k,
        .graph_index = graph_index,
        .local_tracker = &local_tracker,
        .var_tracker = &var_tracker,
        .param_tracker = &param_tracker,
        .log = log,
    };

    var i: u32 = 0;
    while (i < fn_node.childCount()) : (i += 1) {
        const child = fn_node.child(i) orelse continue;
        if (child.kindId() == k.block) {
            try scanBlockForCalls(allocator, &sctx, child);
            try scanForTypeIdentifiersInBody(allocator, &sctx, child, 0);
        }
    }

    try scanFunctionTypesForUsesType(allocator, &sctx);
}

/// Process an impl_item: recurse into its methods and create implements edges.
fn processImpl(
    allocator: std.mem.Allocator,
    graph: *Graph,
    source: []const u8,
    impl_node: ts.Node,
    k: *const KindIds,
    edge_ctx: *const EdgeContext,
    graph_index: *const GraphIndex,
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

    // Recurse into the declaration_list to process methods.
    var i: u32 = 0;
    while (i < impl_node.childCount()) : (i += 1) {
        const child = impl_node.child(i) orelse continue;
        if (child.kindId() == k.declaration_list) {
            var j: u32 = 0;
            while (j < child.childCount()) : (j += 1) {
                const decl = child.child(j) orelse continue;
                if (decl.kindId() == k.function_item) {
                    try processFunction(allocator, graph, source, decl, k, edge_ctx, graph_index, log);
                }
            }
        }
    }
}

/// Process an inline mod_item: recurse into its declaration_list.
fn processInlineMod(
    allocator: std.mem.Allocator,
    graph: *Graph,
    source: []const u8,
    mod_node: ts.Node,
    k: *const KindIds,
    edge_ctx: *const EdgeContext,
    graph_index: *const GraphIndex,
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
                    try processFunction(allocator, graph, source, decl, k, edge_ctx, graph_index, log);
                } else if (kid == k.impl_item) {
                    try processImpl(allocator, graph, source, decl, k, edge_ctx, graph_index, log);
                } else if (kid == k.mod_item) {
                    try processInlineMod(allocator, graph, source, decl, k, edge_ctx, graph_index, log);
                } else if (kid == k.struct_item or kid == k.enum_item or kid == k.union_item) {
                    try processStructOrEnum(allocator, graph, source, decl, k, edge_ctx, graph_index, log);
                }
            }
        }
    }
}

/// Scan struct/enum/union field declarations for type references and create uses_type edges.
fn processStructOrEnum(
    allocator: std.mem.Allocator,
    graph: *Graph,
    source: []const u8,
    item_node: ts.Node,
    k: *const KindIds,
    edge_ctx: *const EdgeContext,
    graph_index: *const GraphIndex,
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
            try scanFieldTypesRecursive(allocator, graph, source, child, owner_id, k, edge_ctx, graph_index, log);
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

/// Recursively scan AST children for type_identifier and scoped_type_identifier
/// nodes, creating uses_type edges from owner_id to resolved types.
fn scanFieldTypesRecursive(
    allocator: std.mem.Allocator,
    graph: *Graph,
    source: []const u8,
    node: ts.Node,
    owner_id: NodeId,
    k: *const KindIds,
    edge_ctx: *const EdgeContext,
    graph_index: *const GraphIndex,
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
                    findTypeCrossFile(graph, type_name, edge_ctx, &graph_index.scope);
                if (target_id) |tid| {
                    _ = try graph.addEdgeIfNew(allocator, .{ .source_id = owner_id, .target_id = tid, .edge_type = .uses_type });
                }
            }
        } else if (kid == k.scoped_type_identifier) {
            try resolveScopedFieldType(allocator, graph, source, child, owner_id, k, edge_ctx, graph_index, log);
        } else if (kid != k.attribute_item) {
            try scanFieldTypesRecursive(allocator, graph, source, child, owner_id, k, edge_ctx, graph_index, log);
        }
    }
}

/// Resolve a scoped_type_identifier in a struct/enum field through the import
/// map, creating uses_type edges for cross-file types.
fn resolveScopedFieldType(
    allocator: std.mem.Allocator,
    graph: *Graph,
    source: []const u8,
    scoped_node: ts.Node,
    owner_id: NodeId,
    k: *const KindIds,
    edge_ctx: *const EdgeContext,
    graph_index: *const GraphIndex,
    log: Logger,
) !void {
    var segments: [cf.max_chain_depth][]const u8 = undefined;
    var seg_count: usize = 0;
    cf.collectScopedSegments(source, scoped_node, &segments, &seg_count, k);
    if (seg_count < 2) return;

    const qualifier = segments[0];

    // Skip if qualifier is a local type (inner identifiers handled by recursion).
    if (findTypeDefByNameScoped(graph, qualifier, edge_ctx.scope_start, edge_ctx.scope_end, &graph_index.scope) != null) return;

    // Resolve through import map for cross-file types.
    const origin = edge_ctx.findImportOrigin(qualifier) orelse return;

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

    var edge_buf: [cf.max_chain_depth]cf.ResolvedEdge = undefined;
    const edge_count = cf.resolveQualifiedCall(
        graph,
        origin.file_id,
        resolve_chain[0..len],
        false,
        graph_index,
        log,
        &edge_buf,
    );
    for (edge_buf[0..edge_count]) |edge| {
        _ = try graph.addEdgeIfNew(allocator, .{
            .source_id = owner_id,
            .target_id = edge.target_id,
            .edge_type = edge.edge_type,
        });
    }
}

/// Scan a block recursively for call_expression nodes and create calls edges.
fn scanBlockForCalls(allocator: std.mem.Allocator, sctx: *const ScanContext, node: ts.Node) !void {
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        const kid = child.kindId();

        if (kid == sctx.k.call_expression) {
            try resolveCallTarget(allocator, sctx, child);
        }

        // Recurse into all children but skip nested closures.
        if (kid != sctx.k.function_item) {
            try scanBlockForCalls(allocator, sctx, child);
        }
    }
}

/// Resolve the target of a call expression and create a calls edge.
fn resolveCallTarget(allocator: std.mem.Allocator, sctx: *const ScanContext, call_node: ts.Node) !void {
    const func_ref = call_node.child(0) orelse return;
    const ref_kid = func_ref.kindId();

    if (ref_kid == sctx.k.identifier) {
        const name = ts_api.nodeText(sctx.source, func_ref);
        if (findFunctionByNameScoped(sctx.graph, name, sctx.edge_ctx.scope_start, sctx.edge_ctx.scope_end, sctx.caller_parent_id, &sctx.graph_index.scope)) |target_id| {
            _ = try sctx.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = target_id, .edge_type = .calls });
        } else if (sctx.edge_ctx.findImportOrigin(name)) |origin| {
            try addResolvedEdges(allocator, sctx, origin.file_id, origin.chain, true);
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
/// Tries local type resolution first, then cross-file resolution via the import map.
fn resolveScopedCall(allocator: std.mem.Allocator, sctx: *const ScanContext, scoped_node: ts.Node) !void {
    var segments: [cf.max_chain_depth][]const u8 = undefined;
    var seg_count: usize = 0;
    const scope_start = sctx.edge_ctx.scope_start;
    const scope_end = sctx.edge_ctx.scope_end;

    cf.collectScopedSegments(sctx.source, scoped_node, &segments, &seg_count, sctx.k);

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
            try addResolvedEdges(allocator, sctx, origin.file_id, resolve_chain[0..len], true);
            return;
        }
    }
}

/// Resolve obj.method() field expression calls using receiver classification.
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

    if (getLeftmostIdentifier(field_node, sctx.k)) |receiver_node| {
        const receiver_name = ts_api.nodeText(sctx.source, receiver_node);
        if (sctx.local_tracker.findTypeName(receiver_name)) |type_name| {
            if (findTypeDefByNameScoped(sctx.graph, type_name, scope_start, scope_end, &sctx.graph_index.scope)) |type_id| {
                if (findMethodInTypeOrImpls(sctx.graph, type_id, name, scope_start, scope_end, &sctx.graph_index.scope)) |target_id| {
                    _ = try sctx.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = target_id, .edge_type = .calls });
                    return;
                }
            }
        }

        if (sctx.var_tracker.findTarget(receiver_name)) |target_file_id| {
            try addResolvedEdges(allocator, sctx, target_file_id, &.{name}, true);
            return;
        }

        // Check param_tracker for import-qualified parameter types.
        if (sctx.param_tracker.findOrigin(receiver_name)) |origin| {
            try resolveOriginCall(allocator, sctx, origin, &.{name}, true);
            return;
        }
    }

    const receiver = classifyReceiver(sctx.graph, sctx.source, field_node, scope_start, scope_end, sctx.caller_parent_id, sctx.fn_node, sctx.k, &sctx.graph_index.scope);
    switch (receiver) {
        .self_receiver, .local_type => {
            if (findFunctionByNameScoped(sctx.graph, name, scope_start, scope_end, sctx.caller_parent_id, &sctx.graph_index.scope)) |target_id| {
                _ = try sctx.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = target_id, .edge_type = .calls });
            }
        },
        .external => {},
    }
}

/// Merge an origin's type chain with the call-site chain and resolve.
fn resolveOriginCall(allocator: std.mem.Allocator, sctx: *const ScanContext, origin: cf.SymbolOrigin, call_chain: []const []const u8, is_call: bool) !void {
    var merged: [cf.max_chain_depth][]const u8 = undefined;
    var len: usize = 0;
    for (origin.chain) |seg| {
        if (len >= cf.max_chain_depth) break;
        merged[len] = seg;
        len += 1;
    }
    for (call_chain) |seg| {
        if (len >= cf.max_chain_depth) break;
        merged[len] = seg;
        len += 1;
    }
    if (len > 0) {
        try addResolvedEdges(allocator, sctx, origin.file_id, merged[0..len], is_call);
    }
}

/// Handle a call_expression whose function reference is a generic_function
/// (turbofish syntax). Unwraps the generic_function to find the inner
/// function reference and dispatches to the appropriate resolver.
/// Also scans the type_arguments for uses_type edges.
fn resolveGenericFunctionCall(allocator: std.mem.Allocator, sctx: *const ScanContext, generic_node: ts.Node) !void {
    var i: u32 = 0;
    while (i < generic_node.childCount()) : (i += 1) {
        const child = generic_node.child(i) orelse continue;
        if (child.kindId() == sctx.k.type_arguments) {
            try scanForTypeIdentifiersInBody(allocator, sctx, child, 0);
        }
    }

    const inner_ref = generic_node.namedChild(0) orelse return;
    const inner_kid = inner_ref.kindId();

    if (inner_kid == sctx.k.identifier) {
        const name = ts_api.nodeText(sctx.source, inner_ref);
        if (findFunctionByNameScoped(sctx.graph, name, sctx.edge_ctx.scope_start, sctx.edge_ctx.scope_end, sctx.caller_parent_id, &sctx.graph_index.scope)) |target_id| {
            _ = try sctx.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = target_id, .edge_type = .calls });
        }
    } else if (inner_kid == sctx.k.scoped_identifier) {
        try resolveScopedCall(allocator, sctx, inner_ref);
    } else if (inner_kid == sctx.k.field_expression) {
        try resolveFieldCall(allocator, sctx, inner_ref);
    }
}

/// Scan function parameter types and return type for type references.
fn scanFunctionTypesForUsesType(allocator: std.mem.Allocator, sctx: *const ScanContext) !void {
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
                    findTypeCrossFile(sctx.graph, type_name, sctx.edge_ctx, &sctx.graph_index.scope);
                if (target_id) |tid| {
                    _ = try sctx.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = tid, .edge_type = .uses_type });
                }
            }
        }

        if (kid == sctx.k.parameters) {
            try scanForTypeIdentifiers(allocator, sctx, child);
        } else if (kid == sctx.k.generic_type or kid == sctx.k.reference_type or kid == sctx.k.scoped_type_identifier) {
            // Return type wrapped in a generic, reference, or scoped path.
            try scanForTypeIdentifiers(allocator, sctx, child);
        }
    }
}

/// Recursively scan for type_identifier nodes and create uses_type edges.
fn scanForTypeIdentifiers(allocator: std.mem.Allocator, sctx: *const ScanContext, node: ts.Node) !void {
    const scope_start = sctx.edge_ctx.scope_start;
    const scope_end = sctx.edge_ctx.scope_end;

    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        if (child.kindId() == sctx.k.type_identifier) {
            const type_name = ts_api.nodeText(sctx.source, child);
            if (!isPrimitiveOrSelf(type_name)) {
                const target_id = findTypeByNameScoped(sctx.graph, type_name, scope_start, scope_end, sctx.caller_parent_id, &sctx.graph_index.scope) orelse
                    findTypeCrossFile(sctx.graph, type_name, sctx.edge_ctx, &sctx.graph_index.scope);
                if (target_id) |tid| {
                    _ = try sctx.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = tid, .edge_type = .uses_type });
                }
            }
        } else {
            try scanForTypeIdentifiers(allocator, sctx, child);
        }
    }
}

/// Check if a type name is a primitive, Self, or should be skipped.
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

/// Recursively scan an AST subtree for type_identifier nodes and create
/// uses_type edges. Checks both local and cross-file type definitions.
/// Stops at nested function boundaries. Depth-capped.
fn scanForTypeIdentifiersInBody(allocator: std.mem.Allocator, sctx: *const ScanContext, ts_node: ts.Node, depth: u32) !void {
    if (depth >= cf.max_ast_scan_depth) return;
    const kid = ts_node.kindId();
    const scope_start = sctx.edge_ctx.scope_start;
    const scope_end = sctx.edge_ctx.scope_end;

    if (kid == sctx.k.type_identifier) {
        const type_name = ts_api.nodeText(sctx.source, ts_node);
        if (!isPrimitiveOrSelf(type_name)) {
            const target_id = findTypeByNameScoped(sctx.graph, type_name, scope_start, scope_end, sctx.caller_parent_id, &sctx.graph_index.scope) orelse
                findTypeCrossFile(sctx.graph, type_name, sctx.edge_ctx, &sctx.graph_index.scope);
            if (target_id) |tid| {
                _ = try sctx.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = tid, .edge_type = .uses_type });
            }
        }
        return;
    }

    // Uppercase identifiers used as values (struct literals, unit variants)
    // are type references too.
    if (kid == sctx.k.identifier) {
        const name = ts_api.nodeText(sctx.source, ts_node);
        if (name.len > 0 and std.ascii.isUpper(name[0]) and !isPrimitiveOrSelf(name)) {
            if (findTypeCrossFile(sctx.graph, name, sctx.edge_ctx, &sctx.graph_index.scope)) |tid| {
                _ = try sctx.graph.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = tid, .edge_type = .uses_type });
            }
        }
        return;
    }

    if (kid == sctx.k.function_item) return;

    var i: u32 = 0;
    while (i < ts_node.childCount()) : (i += 1) {
        const child = ts_node.child(i) orelse continue;
        try scanForTypeIdentifiersInBody(allocator, sctx, child, depth + 1);
    }
}

/// Search imported files for a type definition with the given name.
/// Deduplicates target file ids to avoid false ambiguity when multiple
/// import entries point to the same file. Returns the target NodeId only
/// if exactly one distinct type matches across all imported files.
fn findTypeCrossFile(graph: *const Graph, name: []const u8, edge_ctx: *const EdgeContext, scope_index: *const ScopeIndex) ?NodeId {
    const ctx = edge_ctx;
    if (name.len == 0 or !std.ascii.isUpper(name[0])) return null;

    // Collect unique target file ids. The local buffer is generously sized;
    // if imports exceed it, the remainder is simply not searched.
    var unique_targets: [512]NodeId = undefined;
    var unique_count: usize = 0;
    for (ctx.imports.items) |entry| {
        var already = false;
        for (unique_targets[0..unique_count]) |existing| {
            if (existing == entry.target) {
                already = true;
                break;
            }
        }
        if (!already and unique_count < unique_targets.len) {
            unique_targets[unique_count] = entry.target;
            unique_count += 1;
        }
    }

    var match: ?NodeId = null;
    var match_count: usize = 0;
    for (unique_targets[0..unique_count]) |target_file_id| {
        for (scope_index.childrenOf(target_file_id)) |child_idx| {
            const n = graph.nodes.items[child_idx];
            if (!n.kind.isTypeContainer()) continue;
            if (!std.mem.eql(u8, n.name, name)) continue;
            // Skip impl blocks and type aliases; they share the type name
            // but aren't the defining declaration.
            if (n.lang_meta == .rust and
                (n.lang_meta.rust.sub_kind == .impl_block or
                    n.lang_meta.rust.sub_kind == .type_alias)) continue;
            match = @enumFromInt(child_idx);
            match_count += 1;
        }
    }
    if (match_count == 1) return match;
    return null;
}

// --- Receiver classification ---

/// Determine whether the receiver of a field_expression refers to `self`,
/// a locally-defined type, or an external entity.
///
/// Walks the leftmost spine of the field_expression to find the root
/// identifier, then checks (in order): literal "self", scope-visible type
/// name, and parameter type name. Falls back to `external` if none match.
fn classifyReceiver(
    graph: *const Graph,
    source: []const u8,
    field_expr: ts.Node,
    scope_start: usize,
    scope_end: usize,
    caller_parent_id: ?NodeId,
    fn_decl_node: ts.Node,
    k: *const KindIds,
    scope_index: *const ScopeIndex,
) ReceiverKind {
    const receiver_node = getLeftmostIdentifier(field_expr, k) orelse return .external;
    const receiver_name = ts_api.nodeText(source, receiver_node);

    // Check if receiver is "self".
    if (std.mem.eql(u8, receiver_name, "self")) {
        return .self_receiver;
    }

    // Check if receiver matches a locally-defined type.
    if (findTypeByNameScoped(graph, receiver_name, scope_start, scope_end, caller_parent_id, scope_index) != null) {
        return .local_type;
    }

    // Check if receiver is a parameter whose type is a locally-defined type.
    if (findParamTypeName(source, fn_decl_node, receiver_name, k)) |type_name| {
        if (findTypeByNameScoped(graph, type_name, scope_start, scope_end, caller_parent_id, scope_index) != null) {
            return .local_type;
        }
    }

    return .external;
}

/// Walk down the left spine of nested field_expressions to find the
/// leftmost identifier node (the actual receiver).
/// In Rust, `self` in expression context is a named node of kind "self",
/// not "identifier", so both kinds are checked.
fn getLeftmostIdentifier(node: ts.Node, k: *const KindIds) ?ts.Node {
    const kid = node.kindId();
    if (kid == k.identifier or kid == k.self_expr) {
        return node;
    }
    if (kid == k.field_expression) {
        if (node.namedChild(0)) |child| {
            return getLeftmostIdentifier(child, k);
        }
    }
    return null;
}

/// Given a function_item AST node and a parameter name, return the
/// type identifier if the parameter's type is a bare type_identifier.
/// Unwraps reference_type (&T, &mut T) to reach the inner type.
fn findParamTypeName(source: []const u8, fn_node: ts.Node, param_name: []const u8, k: *const KindIds) ?[]const u8 {
    const type_node = findParamTypeNode(source, fn_node, param_name, k) orelse return null;
    const unwrapped = unwrapTypeNode(type_node, k);
    if (unwrapped.kindId() == k.type_identifier) {
        return ts_api.nodeText(source, unwrapped);
    }
    return null;
}

/// Extract the type AST node for the named parameter.
fn findParamTypeNode(source: []const u8, fn_node: ts.Node, param_name: []const u8, k: *const KindIds) ?ts.Node {
    var i: u32 = 0;
    while (i < fn_node.childCount()) : (i += 1) {
        const child = fn_node.child(i) orelse continue;
        if (child.kindId() != k.parameters) continue;

        var j: u32 = 0;
        while (j < child.namedChildCount()) : (j += 1) {
            const param = child.namedChild(j) orelse continue;
            if (param.kindId() != k.parameter) continue;

            const name_node = param.namedChild(0) orelse continue;
            if (name_node.kindId() != k.identifier) continue;
            if (!std.mem.eql(u8, ts_api.nodeText(source, name_node), param_name)) continue;

            return param.namedChild(1);
        }
        break;
    }
    return null;
}

/// Unwrap reference_type, generic_type, and other wrappers to find the
/// inner type node.
fn unwrapTypeNode(node: ts.Node, k: *const KindIds) ts.Node {
    var current = node;
    var iterations: usize = 0;
    while (iterations < 10) : (iterations += 1) {
        const kid = current.kindId();
        if (kid == k.reference_type) {
            // &T or &mut T: the type is the last named child.
            const nc = current.namedChildCount();
            if (nc > 0) {
                if (current.namedChild(nc - 1)) |inner| {
                    current = inner;
                    continue;
                }
            }
        }
        if (kid == k.generic_type) {
            // Type<Params>: unwrap to the type identifier (first named child).
            if (current.namedChild(0)) |inner| {
                current = inner;
                continue;
            }
        }
        break;
    }
    return current;
}

/// Extract scoped type segments from a parameter's type node.
/// For `param: module::Type`, returns segments ["module", "Type"].
/// Unwraps reference_type wrappers first.
fn extractScopedTypeChain(source: []const u8, type_node: ts.Node, out: *[cf.max_chain_depth][]const u8, k: *const KindIds) usize {
    const unwrapped = unwrapTypeNode(type_node, k);
    const kid = unwrapped.kindId();
    if (kid == k.scoped_type_identifier or kid == k.scoped_identifier) {
        var count: usize = 0;
        cf.collectScopedSegments(source, unwrapped, out, &count, k);
        return count;
    }
    return 0;
}

/// Pre-scan function parameters for import-qualified type annotations.
/// For each parameter whose type is a scoped path (module::Type), records
/// the target file and type chain in the ParamTypeTracker.
fn prescanForParamTypeBindings(
    allocator: std.mem.Allocator,
    source: []const u8,
    fn_node: ts.Node,
    k: *const KindIds,
    edge_ctx: *const EdgeContext,
    tracker: *ParamTypeTracker,
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
            var chain: [cf.max_chain_depth][]const u8 = undefined;
            const chain_len = extractScopedTypeChain(source, type_node, &chain, k);
            if (chain_len >= 2) {
                if (edge_ctx.findImportTarget(chain[0])) |target_file| {
                    try tracker.addBinding(allocator, param_name, target_file, chain[1..chain_len]);
                }
            }
        }
        break;
    }
}

/// Find a method by name within a type's direct children or any
/// impl_block that targets the same type name.
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

    // Search direct children of the type itself.
    for (scope_index.childrenOf(type_id)) |child_idx| {
        const n = graph.nodes.items[child_idx];
        if (n.kind == .function and std.mem.eql(u8, n.name, method_name)) {
            return @enumFromInt(child_idx);
        }
    }

    // Search children of impl_blocks that target this type name.
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

// --- Local type prescan ---

/// Pre-scan a function body to populate the LocalTypeTracker with
/// local variable bindings whose type can be inferred from the RHS.
fn prescanBlock(
    allocator: std.mem.Allocator,
    source: []const u8,
    fn_node: ts.Node,
    k: *const KindIds,
    local_tracker: *LocalTypeTracker,
) !void {
    var i: u32 = 0;
    while (i < fn_node.childCount()) : (i += 1) {
        const child = fn_node.child(i) orelse continue;
        if (child.kindId() == k.block) {
            try prescanBlockForLocals(allocator, source, child, k, local_tracker);
            return;
        }
    }
}

/// Recursively walk a block node, populating LocalTypeTracker from
/// let declarations that bind a variable to a known local type.
fn prescanBlockForLocals(
    allocator: std.mem.Allocator,
    source: []const u8,
    node: ts.Node,
    k: *const KindIds,
    local_tracker: *LocalTypeTracker,
) !void {
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        const kid = child.kindId();

        if (kid == k.let_declaration) {
            const var_name = getLetVarName(source, child, k) orelse continue;
            const type_name = extractLetType(source, child) orelse continue;
            try local_tracker.addBinding(allocator, var_name, type_name);
        }

        // Recurse into blocks, if/else, match arms, loops, etc.
        // but NOT into nested function_item (closures).
        if (kid != k.function_item) {
            try prescanBlockForLocals(allocator, source, child, k, local_tracker);
        }
    }
}

/// Pre-scan a function body for variable bindings from module-qualified
/// expressions. Populates the VarTracker for cross-file method resolution.
/// Uses resolveVarTargetThroughReturnType to refine targets through return types.
fn prescanBlockForVarBindings(
    allocator: std.mem.Allocator,
    graph: *const Graph,
    source: []const u8,
    fn_node: ts.Node,
    k: *const KindIds,
    edge_ctx: *const EdgeContext,
    graph_index: *const GraphIndex,
    var_tracker: *VarTracker,
    log: Logger,
) !void {
    var i: u32 = 0;
    while (i < fn_node.childCount()) : (i += 1) {
        const child = fn_node.child(i) orelse continue;
        if (child.kindId() == k.block) {
            try prescanBlockForVarBindingsRecursive(allocator, graph, source, child, k, edge_ctx, graph_index, var_tracker, log);
            return;
        }
    }
}

/// Recursively walk block nodes looking for let declarations with
/// module-qualified initializers.
fn prescanBlockForVarBindingsRecursive(
    allocator: std.mem.Allocator,
    graph: *const Graph,
    source: []const u8,
    node: ts.Node,
    k: *const KindIds,
    edge_ctx: *const EdgeContext,
    graph_index: *const GraphIndex,
    var_tracker: *VarTracker,
    log: Logger,
) !void {
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        const kid = child.kindId();

        if (kid == k.let_declaration) {
            const var_name = getLetVarName(source, child, k) orelse continue;
            if (cf.findImportQualifiedRoot(source, child, edge_ctx, k)) |target_file| {
                const resolved = cf.resolveVarTargetThroughReturnType(graph, source, child, edge_ctx, k, graph_index, log) orelse target_file;
                try var_tracker.addBinding(allocator, var_name, resolved);
            }
        }

        if (kid != k.function_item) {
            try prescanBlockForVarBindingsRecursive(allocator, graph, source, child, k, edge_ctx, graph_index, var_tracker, log);
        }
    }
}

/// Get the variable name from a let_declaration AST node.
/// Skips mutable_specifier if present.
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

/// Extract the type name from a let_declaration.
/// Handles struct literals, static method calls, and type annotations.
/// Returns the PascalCase type name as a slice of source, or null.
fn extractLetType(source: []const u8, let_node: ts.Node) ?[]const u8 {
    const start = let_node.startByte();
    const end = let_node.endByte();
    if (start >= end or end > source.len) return null;
    const text = source[start..end];

    // Find '=' in the declaration text.
    const eq_pos = std.mem.indexOfScalar(u8, text, '=') orelse return null;

    // Check for type annotation before '=': `let name: Type = ...`
    // Search for a lone ':' (not part of '::') in the text before '='.
    {
        var ci: usize = 0;
        while (ci < eq_pos) : (ci += 1) {
            if (text[ci] != ':') continue;
            // Skip '::' scope operator.
            if (ci + 1 < eq_pos and text[ci + 1] == ':') {
                ci += 1;
                continue;
            }
            // Found type annotation colon. Extract type name after it.
            var pos = ci + 1;
            while (pos < eq_pos and source_scan.isWhitespace(text[pos])) : (pos += 1) {}
            // Skip '&' reference.
            if (pos < eq_pos and text[pos] == '&') {
                pos += 1;
                while (pos < eq_pos and source_scan.isWhitespace(text[pos])) : (pos += 1) {}
            }
            // Skip 'mut' keyword.
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

    // Check after '=' for PascalCase identifier (struct literal or static call).
    var pos = eq_pos + 1;
    while (pos < text.len and source_scan.isWhitespace(text[pos])) : (pos += 1) {}

    const type_start = pos;
    while (pos < text.len and source_scan.isIdentChar(text[pos])) : (pos += 1) {}
    if (pos == type_start) return null;
    const type_name = text[type_start..pos];

    // Must start with uppercase letter (PascalCase = local type).
    if (!std.ascii.isUpper(type_name[0])) return null;

    // Followed by '::' = static method call.
    if (pos + 1 < text.len and text[pos] == ':' and text[pos + 1] == ':') {
        return type_name;
    }
    // Followed by '{' (possibly with whitespace) = struct literal.
    var brace_pos = pos;
    while (brace_pos < text.len and source_scan.isWhitespace(text[brace_pos])) : (brace_pos += 1) {}
    if (brace_pos < text.len and text[brace_pos] == '{') {
        return type_name;
    }

    return null;
}

// --- Node lookup helpers ---

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

/// Find a function node by name with scope-aware resolution.
/// Walks up the parent_id chain from caller_parent_id, preferring the
/// narrowest scope. Falls back to flat file-scope search, returning
/// null if ambiguous (more than one match).
fn findFunctionByNameScoped(graph: *const Graph, name: []const u8, scope_start: usize, scope_end: usize, caller_parent_id: ?NodeId, scope_index: *const ScopeIndex) ?NodeId {
    if (caller_parent_id) |cpid| {
        var current_scope: ?NodeId = cpid;
        var hops: usize = 0;
        while (current_scope != null and hops < 100) : (hops += 1) {
            const scope_id = current_scope.?;
            for (scope_index.childrenOf(scope_id)) |child_idx| {
                const n = graph.nodes.items[child_idx];
                if ((n.kind == .function or n.kind == .test_def) and std.mem.eql(u8, n.name, name)) {
                    return @enumFromInt(child_idx);
                }
            }
            const scope_node = graph.getNode(scope_id) orelse break;
            current_scope = scope_node.parent_id;
        }
    }
    // Fallback: flat file-scope search, return only if unambiguous.
    const items = graph.nodes.items;
    const end = @min(scope_end, items.len);
    var sole_match: ?NodeId = null;
    var match_count: usize = 0;
    for (items[scope_start..end], scope_start..) |n, idx| {
        if ((n.kind == .function or n.kind == .test_def) and std.mem.eql(u8, n.name, name)) {
            sole_match = @enumFromInt(idx);
            match_count += 1;
            if (match_count > 1) return null;
        }
    }
    return sole_match;
}

/// Find a type node (type_def, enum_def, union_def) by name with
/// scope-aware resolution. Walks up the parent_id chain, preferring
/// the narrowest scope. Falls back to flat search with ambiguity rejection.
fn findTypeByNameScoped(graph: *const Graph, name: []const u8, scope_start: usize, scope_end: usize, caller_parent_id: ?NodeId, scope_index: *const ScopeIndex) ?NodeId {
    if (caller_parent_id) |cpid| {
        var current_scope: ?NodeId = cpid;
        var hops: usize = 0;
        while (current_scope != null and hops < 100) : (hops += 1) {
            const scope_id = current_scope.?;
            for (scope_index.childrenOf(scope_id)) |child_idx| {
                const n = graph.nodes.items[child_idx];
                if (isTypeNode(n) and std.mem.eql(u8, n.name, name)) {
                    return @enumFromInt(child_idx);
                }
            }
            const scope_node = graph.getNode(scope_id) orelse break;
            current_scope = scope_node.parent_id;
        }
    }
    // Fallback: flat file-scope search, return only if unambiguous.
    const items = graph.nodes.items;
    const end = @min(scope_end, items.len);
    var sole_match: ?NodeId = null;
    var match_count: usize = 0;
    for (items[scope_start..end], scope_start..) |n, idx| {
        if (isTypeNode(n) and std.mem.eql(u8, n.name, name)) {
            sole_match = @enumFromInt(idx);
            match_count += 1;
            if (match_count > 1) return null;
        }
    }
    return sole_match;
}

/// Find an actual type definition (struct, enum, union) by name,
/// excluding impl_blocks and traits. Used for impl target resolution
/// and scoped call qualifier resolution.
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

/// Check whether a node is a type definition or alias (struct, enum, union,
/// type alias), excluding impl_blocks, traits, and associated types.
pub fn isTypeOrAliasNode(n: Node) bool {
    if (n.kind == .enum_def or n.kind == .union_def) return true;
    if (n.kind != .type_def) return false;
    if (n.lang_meta == .rust) {
        const sk = n.lang_meta.rust.sub_kind;
        if (sk == .impl_block or sk == .trait_ or sk == .associated_type) return false;
    }
    return true;
}

/// Check whether a node is a trait definition.
pub fn isTraitNode(n: Node) bool {
    return n.kind == .type_def and
        n.lang_meta == .rust and
        n.lang_meta.rust.sub_kind == .trait_;
}

/// Find an impl block node by line number.
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
