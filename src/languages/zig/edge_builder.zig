const std = @import("std");
const graph_mod = @import("../../core/graph.zig");
const logging = @import("../../logging.zig");
const edge_mod = @import("../../core/edge.zig");
const types = @import("../../core/types.zig");
const ts = @import("tree-sitter");
const ts_api = @import("../../parser/tree_sitter_api.zig");
const ast = @import("ast_analysis.zig");
const cf = @import("cross_file.zig");
const source_scan = @import("../../parser/source_scan.zig");
const pc = @import("parse_context.zig");
const phantom_mod = @import("../../core/phantom.zig");
const shared_types = @import("../shared/types.zig");
const shared_resolve = @import("../shared/resolve.zig");
const shared_lookup = @import("../shared/lookup.zig");

const Field = logging.Field;
const Logger = logging.Logger;

const Graph = graph_mod.Graph;
const NodeId = types.NodeId;
const NodeKind = types.NodeKind;
const EdgeType = types.EdgeType;
const EdgeSource = types.EdgeSource;
const PhantomManager = phantom_mod.PhantomManager;

const EdgeContext = cf.EdgeContext;
const VarTracker = cf.VarTracker;
const KindIds = pc.KindIds;
const GraphIndex = @import("../../core/graph_index.zig").GraphIndex;
const ScopeIndex = @import("../../core/scope_index.zig").ScopeIndex;
const TypeBinding = shared_types.TypeBinding;
const LocalTypeTracker = shared_types.LocalTypeTracker;

/// A parameter bound to its import-qualified type origin.
const ParamBinding = struct {
    name: []const u8,
    target_file: NodeId,
    type_chain: [cf.max_chain_depth][]const u8 = undefined,
    chain_len: usize = 0,
};

/// Maps function parameter names to their import-qualified type origins.
/// Populated during prescan from parameter declarations whose type is a
/// dotted import path. Each binding stores the resolved target file NodeId
/// and the remaining member chain for cross-file call resolution.
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

/// Bundles all state needed to scan a single function or test body for edges.
///
/// Created once per function/test declaration inside walkForEdges, then
/// passed by const pointer to scanForCalls and scanForTypeIdentifiersScoped.
/// Owns no memory; all pointers borrow from the caller's stack or the graph.
const ScanContext = struct {
    g: *Graph,
    source: []const u8,
    caller_id: NodeId,
    caller_parent_id: ?NodeId,
    fn_decl_node: ts.Node,
    edge_ctx: *const EdgeContext,
    k: *const KindIds,
    var_tracker: *const VarTracker,
    local_tracker: *const LocalTypeTracker,
    param_tracker: *const ParamTypeTracker,
    graph_index: *const GraphIndex,
    phantom_mgr: *const PhantomManager,
    log: Logger,
};

/// Recursively walk a tree-sitter AST and create call/uses_type edges in the graph.
///
/// For each function or test declaration encountered, this function:
///   1. Matches the AST node to its corresponding graph node (by name + line).
///   2. Pre-scans the body to populate variable, local-type, and param-type trackers.
///   3. Scans call expressions to create `calls` edges.
///   4. Scans type identifiers to create `uses_type` edges.
///
/// The walk recurses into named children only; anonymous nodes are skipped at
/// this level because declarations are always named in tree-sitter.
///
/// `source` is the full file content (borrowed, not owned).
/// `ctx` provides import/scope boundaries for the current file.
/// Returns `error.OutOfMemory` if graph edge insertion fails.
pub fn walkForEdges(allocator: std.mem.Allocator, g: *Graph, source: []const u8, ts_node: ts.Node, ctx: *const EdgeContext, k: *const KindIds, graph_index: *const GraphIndex, phantom_mgr: *const PhantomManager, log: Logger) !void {
    const kid = ts_node.kindId();

    if (kid == k.function_declaration) {
        if (ast.getIdentifierName(source, ts_node, k)) |name| {
            // Phase 1: Match AST declaration to graph node by name + line.
            const decl_line = ts_node.startPoint().row + 1;
            if (findFunctionByNameAndLine(g, name, decl_line, ctx.scope_start, ctx.scope_end)) |fn_id| {
                const fn_node = g.getNode(fn_id);
                _ = if (fn_node) |n| n.parent_id else null; // Scope walk starts at fn_id and walks up through parents.
                const caller_scope_id: ?NodeId = fn_id;

                // Phase 2: Prescan -- populate all per-function trackers in a single block walk.
                var var_tracker = VarTracker{};
                defer var_tracker.deinit(allocator);
                var local_type_tracker = LocalTypeTracker{};
                defer local_type_tracker.deinit(allocator);
                var param_type_tracker = ParamTypeTracker{};
                defer param_type_tracker.deinit(allocator);
                try prescanForParamTypeBindings(allocator, source, ts_node, ctx, k, &param_type_tracker);
                try prescanBlock(allocator, g, source, ts_node, ctx, k, &var_tracker, &local_type_tracker, &param_type_tracker, graph_index);

                // Phase 3: Build ScanContext, then scan for call and type edges.
                const sctx = ScanContext{
                    .g = g,
                    .source = source,
                    .caller_id = fn_id,
                    .caller_parent_id = caller_scope_id,
                    .fn_decl_node = ts_node,
                    .edge_ctx = ctx,
                    .k = k,
                    .var_tracker = &var_tracker,
                    .local_tracker = &local_type_tracker,
                    .param_tracker = &param_type_tracker,
                    .graph_index = graph_index,
                    .phantom_mgr = phantom_mgr,
                    .log = log,
                };

                // Phase 3a: scan for call expressions -> `calls` edges.
                try scanForCalls(allocator, &sctx, ts_node, 0);

                // Phase 3b: scan for type identifiers -> `uses_type` edges.
                try scanForTypeIdentifiersScoped(allocator, &sctx, ts_node, 0);
            } else {
                log.trace("function not found in graph", &.{
                    Field.string("name", name),
                    Field.uint("line", decl_line),
                });
            }
        }
    } else if (kid == k.test_declaration) {
        // Phase 1: Match test AST node to graph node by name.
        const test_name = ast.getTestName(source, ts_node, k);
        if (findTestByName(g, test_name, ctx.scope_start, ctx.scope_end)) |test_id| {
            _ = g.getNode(test_id); // Scope walk starts at test_id and walks up through parents.
            const test_parent_id: ?NodeId = test_id;

            // Phase 2: Prescan -- populate all per-test trackers.
            var var_tracker = VarTracker{};
            defer var_tracker.deinit(allocator);
            var local_type_tracker = LocalTypeTracker{};
            defer local_type_tracker.deinit(allocator);
            var param_type_tracker = ParamTypeTracker{};
            defer param_type_tracker.deinit(allocator);
            try prescanForParamTypeBindings(allocator, source, ts_node, ctx, k, &param_type_tracker);
            try prescanBlock(allocator, g, source, ts_node, ctx, k, &var_tracker, &local_type_tracker, &param_type_tracker, graph_index);

            // Phase 3: Build ScanContext, then scan for call and type edges.
            const sctx = ScanContext{
                .g = g,
                .source = source,
                .caller_id = test_id,
                .caller_parent_id = test_parent_id,
                .fn_decl_node = ts_node,
                .edge_ctx = ctx,
                .k = k,
                .var_tracker = &var_tracker,
                .local_tracker = &local_type_tracker,
                .param_tracker = &param_type_tracker,
                .graph_index = graph_index,
                .phantom_mgr = phantom_mgr,
                .log = log,
            };

            // Phase 3a: scan for call expressions -> `calls` edges.
            try scanForCalls(allocator, &sctx, ts_node, 0);

            // Phase 3b: scan for type identifiers -> `uses_type` edges.
            try scanForTypeIdentifiersScoped(allocator, &sctx, ts_node, 0);
        } else {
            log.trace("test not found in graph", &.{Field.string("name", test_name)});
        }
    }

    // Phase 4: Recurse into named children to find nested declarations.
    // Anonymous nodes are skipped because declarations (fn, test, struct)
    // are always named nodes in tree-sitter.
    var i: u32 = 0;
    while (i < ts_node.namedChildCount()) : (i += 1) {
        const child = ts_node.namedChild(i) orelse continue;
        try walkForEdges(allocator, g, source, child, ctx, k, graph_index, phantom_mgr, log);
    }
}

fn resolveOriginCall(allocator: std.mem.Allocator, sctx: *const ScanContext, origin: cf.SymbolOrigin, call_chain: []const []const u8, is_call: bool) !void {
    const rctx = shared_resolve.ResolveContext{
        .graph_index = sctx.graph_index,
        .log = sctx.log,
        .resolve_return_type = cf.resolveReturnTypeScope,
        .find_in_type_scope = null,
    };
    try shared_resolve.resolveOriginCall(allocator, sctx.g, sctx.caller_id, origin, call_chain, is_call, &rctx);
}

fn addResolvedEdges(allocator: std.mem.Allocator, sctx: *const ScanContext, target_file_id: NodeId, chain: []const []const u8, is_call: bool) !void {
    const rctx = shared_resolve.ResolveContext{
        .graph_index = sctx.graph_index,
        .log = sctx.log,
        .resolve_return_type = cf.resolveReturnTypeScope,
        .find_in_type_scope = null,
    };
    try shared_resolve.addResolvedEdges(allocator, sctx.g, sctx.caller_id, target_file_id, chain, is_call, &rctx);
}

/// Normalized form of the function reference in a call_expression.
const CallFnRef = union(enum) {
    bare: []const u8,
    qualified: ts.Node,
};

/// Extract and normalize the function reference from a call_expression's first
/// named child. Handles identifier, field_expression, and error_union_type.
/// The Zig grammar parses `!fn(args)` as error_union_type wrapping the real ref,
/// so we recurse into the inner node to recover it.
fn extractCallFnRef(source: []const u8, fn_ref: ts.Node, k: *const KindIds) ?CallFnRef {
    const kid = fn_ref.kindId();
    if (kid == k.identifier) return .{ .bare = ts_api.nodeText(source, fn_ref) };
    if (kid == k.field_expression) return .{ .qualified = fn_ref };
    if (kid == k.error_union_type) {
        if (fn_ref.namedChild(0)) |inner| return extractCallFnRef(source, inner, k);
    }
    return null;
}

/// Recursively scan an AST subtree for call_expression nodes and create
/// `calls` edges in the graph.
///
/// Handles three call shapes:
///   - Bare calls (`foo()`) -- resolved in the caller's scope.
///   - Qualified calls (`a.b()`) -- resolved via import tracker, var tracker,
///     local-type tracker, param tracker, or receiver classification fallback.
///   - Single-segment field expressions -- treated as bare calls.
///
/// Recurses into all children (including anonymous nodes) but stops at
/// nested function/test declarations to respect scope boundaries.
/// Depth is capped at `cf.max_ast_scan_depth` to avoid runaway recursion.
fn scanForCalls(allocator: std.mem.Allocator, sctx: *const ScanContext, ts_node: ts.Node, depth: u32) !void {
    // Depth guard: stop descending, edges in deeper subtrees are skipped.
    if (depth >= cf.max_ast_scan_depth) {
        sctx.log.trace("scan depth cap reached", &.{Field.uint("depth", depth)});
        return;
    }
    if (ts_node.kindId() == sctx.k.call_expression) {
        if (ts_node.namedChild(0)) |fn_ref| {
            if (extractCallFnRef(sctx.source, fn_ref, sctx.k)) |ref| {
                switch (ref) {
                    .bare => |callee_name| {
                        if (findFunctionByNameScoped(sctx.g, callee_name, sctx.edge_ctx.scope_start, sctx.edge_ctx.scope_end, sctx.caller_parent_id, &sctx.graph_index.scope)) |callee_id| {
                            _ = try sctx.g.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = callee_id, .edge_type = .calls });
                        } else if (sctx.edge_ctx.findImportOrigin(callee_name)) |origin| {
                            if (origin.chain.len > 0) {
                                try resolveOriginCall(allocator, sctx, origin, &.{}, true);
                            }
                        } else {
                            sctx.log.trace("bare call unresolved", &.{Field.string("callee", callee_name)});
                        }
                    },
                    .qualified => |field_expr| {
                        var chain: [cf.max_chain_depth][]const u8 = undefined;
                        const chain_len = cf.collectFieldExprChain(sctx.source, field_expr, &chain, sctx.k);

                        if (chain_len >= 2) {
                            const root_name = chain[0];

                            if (sctx.edge_ctx.findImportOrigin(root_name)) |origin| {
                                try resolveOriginCall(allocator, sctx, origin, chain[1..chain_len], true);
                            } else if (sctx.var_tracker.findTarget(root_name)) |target_file_id| {
                                try addResolvedEdges(allocator, sctx, target_file_id, chain[1..chain_len], true);
                            } else if (sctx.local_tracker.findTypeName(root_name)) |type_name| {
                                if (findTypeByNameScoped(sctx.g, type_name, sctx.edge_ctx.scope_start, sctx.edge_ctx.scope_end, sctx.caller_parent_id, &sctx.graph_index.scope)) |type_id| {
                                    const leaf_name = chain[chain_len - 1];
                                    for (sctx.graph_index.scope.childrenOf(type_id)) |child_idx| {
                                        const n = sctx.g.nodes.items[child_idx];
                                        if (n.kind == .function and
                                            std.mem.eql(u8, n.name, leaf_name))
                                        {
                                            _ = try sctx.g.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = @enumFromInt(child_idx), .edge_type = .calls });
                                            break;
                                        }
                                    }
                                }
                            } else if (sctx.param_tracker.findOrigin(root_name)) |origin| {
                                try resolveOriginCall(allocator, sctx, origin, chain[1..chain_len], true);
                            } else {
                                const leaf_name = chain[chain_len - 1];
                                const receiver = classifyReceiver(sctx.g, sctx.source, field_expr, sctx.edge_ctx.scope_start, sctx.edge_ctx.scope_end, sctx.caller_parent_id, sctx.caller_id, sctx.fn_decl_node, sctx.k, &sctx.graph_index.scope);
                                switch (receiver) {
                                    .self_receiver => {
                                        if (findFunctionByNameScoped(sctx.g, leaf_name, sctx.edge_ctx.scope_start, sctx.edge_ctx.scope_end, sctx.caller_parent_id, &sctx.graph_index.scope)) |callee_id| {
                                            _ = try sctx.g.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = callee_id, .edge_type = .calls });
                                        }
                                    },
                                    .local_type => |matched_type_id| {
                                        var found = false;
                                        for (sctx.graph_index.scope.childrenOf(matched_type_id)) |child_idx| {
                                            const n = sctx.g.nodes.items[child_idx];
                                            if (n.kind == .function and std.mem.eql(u8, n.name, leaf_name)) {
                                                _ = try sctx.g.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = @enumFromInt(child_idx), .edge_type = .calls });
                                                found = true;
                                                break;
                                            }
                                        }
                                        if (!found) {
                                            if (findFunctionByNameScoped(sctx.g, leaf_name, sctx.edge_ctx.scope_start, sctx.edge_ctx.scope_end, sctx.caller_parent_id, &sctx.graph_index.scope)) |callee_id| {
                                                _ = try sctx.g.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = callee_id, .edge_type = .calls });
                                            }
                                        }
                                    },
                                    .external => {
                                        sctx.log.trace("external receiver, skipping edge", &.{
                                            Field.string("root", root_name),
                                            Field.string("leaf", leaf_name),
                                        });
                                    },
                                }
                            }
                        } else if (chain_len == 1) {
                            if (findFunctionByNameScoped(sctx.g, chain[0], sctx.edge_ctx.scope_start, sctx.edge_ctx.scope_end, sctx.caller_parent_id, &sctx.graph_index.scope)) |callee_id| {
                                _ = try sctx.g.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = callee_id, .edge_type = .calls });
                            }
                        }
                    },
                }
            }
        }
    }

    // -- Recurse: descend into all children (including anonymous nodes)
    // because call expressions can appear inside assignment RHS, return
    // values, etc. Stop at fn/test boundaries to respect scope.
    var i: u32 = 0;
    while (i < ts_node.childCount()) : (i += 1) {
        const child = ts_node.child(i) orelse continue;
        const child_kid = child.kindId();
        if (child_kid == sctx.k.function_declaration or
            child_kid == sctx.k.test_declaration) continue;
        try scanForCalls(allocator, sctx, child, depth + 1);
    }
}

/// Classification of the leftmost receiver in a field_expression chain.
///
/// - `self_receiver`: the identifier is literally "self".
/// - `local_type`: the identifier names a type or type-alias defined in the
///   current file's scope, or a parameter whose type is a local type.
///   Carries the matched type's NodeId for targeted child lookup.
/// - `external`: none of the above; the receiver comes from outside the file
///   (an untracked import or a runtime value).
const ReceiverKind = union(enum) {
    self_receiver,
    local_type: NodeId,
    external,
};

/// Classify the leftmost receiver of a field_expression chain as self,
/// a locally-defined type, or external. Checks in priority order:
/// literal "self", scope-visible type name, parameter type name,
/// then @This() alias. Falls back to external if none match.
fn classifyReceiver(g: *const Graph, source: []const u8, field_expr: ts.Node, scope_start: usize, scope_end: usize, caller_parent_id: ?NodeId, caller_id: NodeId, fn_decl_node: ts.Node, k: *const KindIds, scope_index: *const ScopeIndex) ReceiverKind {
    // The receiver is the first named child of the outermost field_expression.
    // For nested chains like `a.b.c()`, we want the leftmost identifier.
    const receiver_node = getLeftmostIdentifier(field_expr, k) orelse return .external;
    const receiver_name = ts_api.nodeText(source, receiver_node);

    // Check if receiver is the literal identifier "self".
    if (std.mem.eql(u8, receiver_name, "self")) {
        return .self_receiver;
    }

    // Check if receiver matches a locally-defined type or type-alias constant.
    if (findTypeByNameScoped(g, receiver_name, scope_start, scope_end, caller_parent_id, scope_index)) |type_id| {
        return .{ .local_type = type_id };
    }

    // Check if receiver matches a parameter whose type is a locally-defined type.
    if (findParamTypeName(source, fn_decl_node, receiver_name, k)) |type_name| {
        if (findTypeByNameScoped(g, type_name, scope_start, scope_end, caller_parent_id, scope_index)) |type_id| {
            return .{ .local_type = type_id };
        }
    }

    // @This() alias resolves to the enclosing type (caller's parent).
    if (findThisAliasScope(source, fn_decl_node, receiver_name, k)) {
        if (g.getNode(caller_id)) |caller_node| {
            if (caller_node.parent_id) |parent_id| {
                return .{ .local_type = parent_id };
            }
        }
    }

    return .external;
}

/// Check whether `name` is a @This() alias in the enclosing AST scope.
/// Walks up to the parent node and scans sibling declarations.
fn findThisAliasScope(source: []const u8, fn_decl_node: ts.Node, name: []const u8, k: *const KindIds) bool {
    const scope = fn_decl_node.parent() orelse return false;
    var i: u32 = 0;
    while (i < scope.namedChildCount()) : (i += 1) {
        const sibling = scope.namedChild(i) orelse continue;
        if (sibling.kindId() != k.variable_declaration) continue;
        if (!ast.isThisBuiltin(source, sibling, k)) continue;
        const alias_name = ast.getIdentifierName(source, sibling, k) orelse continue;
        if (std.mem.eql(u8, alias_name, name)) return true;
    }
    return false;
}

/// Given a function_declaration AST node and a parameter name, return
/// the base type identifier. Unwraps pointer, optional, and error-union
/// wrappers before checking for a bare identifier.
fn findParamTypeName(source: []const u8, fn_decl_node: ts.Node, param_name: []const u8, k: *const KindIds) ?[]const u8 {
    // Find the "parameters" child of the function_declaration.
    var i: u32 = 0;
    while (i < fn_decl_node.childCount()) : (i += 1) {
        const child = fn_decl_node.child(i) orelse continue;
        if (child.kindId() != k.parameters) continue;

        // Iterate over named children of "parameters", each is a "parameter" node.
        var j: u32 = 0;
        while (j < child.namedChildCount()) : (j += 1) {
            const param = child.namedChild(j) orelse continue;
            if (param.kindId() != k.parameter) continue;

            // A parameter node has named children: identifier (name) and type node.
            // The first named child is the identifier, the second is the type.
            const name_node = param.namedChild(0) orelse continue;
            if (name_node.kindId() != k.identifier) continue;
            const name = ts_api.nodeText(source, name_node);
            if (!std.mem.eql(u8, name, param_name)) continue;

            // Found the parameter. Get the type node (second named child).
            const type_node = param.namedChild(1) orelse return null;
            // Unwrap pointer/optional/error-union to find the base type.
            const base = unwrapTypeNode(type_node, k);
            if (base.kindId() == k.identifier) {
                return ts_api.nodeText(source, base);
            }
            return null;
        }
        break;
    }
    return null;
}

/// Walk down the left spine of nested field_expressions to find the
/// leftmost identifier node (the actual receiver).
fn getLeftmostIdentifier(node: ts.Node, k: *const KindIds) ?ts.Node {
    const kid = node.kindId();
    if (kid == k.identifier) {
        return node;
    }
    if (kid == k.field_expression) {
        if (node.namedChild(0)) |child| {
            return getLeftmostIdentifier(child, k);
        }
    }
    return null;
}

/// Pre-scan a function/test body, populating VarTracker (import-qualified
/// variable bindings), LocalTypeTracker (struct literal/static call bindings),
/// and ParamTypeTracker (if-capture bindings) in a single block walk.
fn prescanBlock(
    allocator: std.mem.Allocator,
    g: *const Graph,
    source: []const u8,
    fn_node: ts.Node,
    ctx: *const EdgeContext,
    k: *const KindIds,
    var_tracker: *VarTracker,
    local_tracker: *LocalTypeTracker,
    param_tracker: *ParamTypeTracker,
    graph_index: *const GraphIndex,
) !void {
    var i: u32 = 0;
    while (i < fn_node.childCount()) : (i += 1) {
        const child = fn_node.child(i) orelse continue;
        if (child.kindId() == k.block) {
            try scanBlockPrescan(allocator, g, source, child, ctx, k, var_tracker, local_tracker, param_tracker, graph_index);
            return;
        }
    }
}

/// Recursively walk a block node, populating VarTracker, LocalTypeTracker,
/// and ParamTypeTracker from variable declarations and if-capture patterns.
fn scanBlockPrescan(
    allocator: std.mem.Allocator,
    g: *const Graph,
    source: []const u8,
    block: ts.Node,
    ctx: *const EdgeContext,
    k: *const KindIds,
    var_tracker: *VarTracker,
    local_tracker: *LocalTypeTracker,
    param_tracker: *ParamTypeTracker,
    graph_index: *const GraphIndex,
) !void {
    var i: u32 = 0;
    while (i < block.childCount()) : (i += 1) {
        const child = block.child(i) orelse continue;
        const kid = child.kindId();

        if (kid == k.variable_declaration) {
            const var_name = ast.getIdentifierName(source, child, k) orelse continue;
            // VarTracker: import-qualified initializer.
            if (cf.findImportQualifiedRoot(source, child, ctx, k)) |target_file_id| {
                const resolved = cf.resolveVarTargetThroughReturnType(g, source, child, ctx, k, graph_index, Logger.noop) orelse target_file_id;
                try var_tracker.addBinding(allocator, var_name, resolved);
            }
            // LocalTypeTracker: struct literal or static method call initializer.
            if (extractStructLiteralType(source, child)) |type_name| {
                try local_tracker.addBinding(allocator, var_name, type_name);
            }
            continue;
        }

        // ParamTypeTracker: if-capture patterns.
        if (kid == k.if_statement or kid == k.if_expression) {
            var cond_ident: ?[]const u8 = null;
            var capture_name: ?[]const u8 = null;

            var j: u32 = 0;
            while (j < child.childCount()) : (j += 1) {
                const ic = child.child(j) orelse continue;
                const ic_kid = ic.kindId();
                if (ic_kid == k.identifier and cond_ident == null) {
                    cond_ident = ts_api.nodeText(source, ic);
                } else if (ic_kid == k.payload or
                    ic_kid == k.payload_identifier)
                {
                    if (ic.namedChild(0)) |inner| {
                        if (inner.kindId() == k.identifier) {
                            capture_name = ts_api.nodeText(source, inner);
                        }
                    }
                }
            }

            if (cond_ident != null and capture_name != null) {
                if (param_tracker.findOrigin(cond_ident.?)) |origin| {
                    try param_tracker.addBinding(allocator, capture_name.?, origin.file_id, origin.chain);
                }
            }
        }

        // Recurse into nested blocks and control-flow statements.
        if (kid == k.block or
            kid == k.defer_statement or
            kid == k.if_statement or
            kid == k.if_expression or
            kid == k.expression_statement or
            kid == k.for_statement or
            kid == k.while_statement)
        {
            try scanBlockPrescan(allocator, g, source, child, ctx, k, var_tracker, local_tracker, param_tracker, graph_index);
        }
    }
}

/// Extract the type name from a variable_declaration whose value is a struct
/// literal (`Point{ .x = 3 }`) or a static method call (`Builder.init(5)`).
/// Returns the PascalCase type name as a slice of `source`, or null.
fn extractStructLiteralType(source: []const u8, var_decl: ts.Node) ?[]const u8 {
    const start = var_decl.startByte();
    const end = var_decl.endByte();
    if (start >= end or end > source.len) return null;
    const text = source[start..end];

    // Find '=' in the declaration text.
    const eq_pos = std.mem.indexOfScalar(u8, text, '=') orelse return null;
    var pos = eq_pos + 1;

    // Skip whitespace after =.
    while (pos < text.len and source_scan.isWhitespace(text[pos])) : (pos += 1) {}

    // Skip optional 'try' keyword.
    if (pos + 3 <= text.len and std.mem.eql(u8, text[pos..][0..3], "try") and
        (pos + 3 >= text.len or !source_scan.isIdentChar(text[pos + 3])))
    {
        pos += 3;
        while (pos < text.len and source_scan.isWhitespace(text[pos])) : (pos += 1) {}
    }

    // Must start with an uppercase letter (PascalCase type name).
    if (pos >= text.len or text[pos] < 'A' or text[pos] > 'Z') return null;
    const id_start = pos;
    while (pos < text.len and source_scan.isIdentChar(text[pos])) : (pos += 1) {}
    if (pos >= text.len) return null;

    // Must be followed by '{' (struct literal) or '.' (static method/field).
    if (text[pos] != '{' and text[pos] != '.') return null;

    // Return a slice of source (not of local text).
    return source[start + id_start .. start + pos];
}

/// Pre-scan a function's parameter list for import-qualified types.
/// For each parameter whose type is `mod.Type` (possibly wrapped in `*` or `?`),
/// record a binding from the parameter name to the import target file and type chain.
fn prescanForParamTypeBindings(
    allocator: std.mem.Allocator,
    source: []const u8,
    fn_decl_node: ts.Node,
    ctx: *const EdgeContext,
    k: *const KindIds,
    tracker: *ParamTypeTracker,
) !void {
    // Find the "parameters" child of the function_declaration.
    var i: u32 = 0;
    while (i < fn_decl_node.childCount()) : (i += 1) {
        const child = fn_decl_node.child(i) orelse continue;
        if (child.kindId() != k.parameters) continue;

        // Iterate over each parameter.
        var j: u32 = 0;
        while (j < child.namedChildCount()) : (j += 1) {
            const param = child.namedChild(j) orelse continue;
            if (param.kindId() != k.parameter) continue;

            // First named child is the identifier (name).
            const name_node = param.namedChild(0) orelse continue;
            if (name_node.kindId() != k.identifier) continue;
            const name = ts_api.nodeText(source, name_node);

            // Skip "self" parameters.
            if (std.mem.eql(u8, name, "self")) continue;

            // Second named child is the type node.
            const type_node = param.namedChild(1) orelse continue;

            // Extract the import-qualified type chain, handling pointer/optional
            // wrappers. Tree-sitter parses `*svc_mod.Service` as
            // field_expression(pointer_type(*svc_mod), identifier "Service"),
            // so the wrapper is inside the field expression's left child.
            var chain: [cf.max_chain_depth][]const u8 = undefined;
            const chain_len = extractParamTypeChain(source, type_node, &chain, k);
            if (chain_len >= 2) {
                if (ctx.findImportTarget(chain[0])) |target_file_id| {
                    try tracker.addBinding(allocator, name, target_file_id, chain[1..chain_len]);
                }
            }
        }
        break;
    }
}

/// Extract the import-qualified type chain from a parameter type AST node.
/// Handles wrappers: `svc_mod.Service`, `*svc_mod.Service`, `?svc_mod.Service`,
/// `*const svc_mod.Service`. Tree-sitter parses `*svc_mod.Service` as
/// `field_expression(pointer_type(*svc_mod), identifier "Service")`, so the
/// wrapper is inside the field expression's left child, not outside.
fn extractParamTypeChain(
    source: []const u8,
    type_node: ts.Node,
    chain: *[cf.max_chain_depth][]const u8,
    k: *const KindIds,
) usize {
    const kid = type_node.kindId();

    if (kid == k.field_expression) {
        // Try standard chain extraction first (works for bare svc_mod.Service).
        const len = cf.collectFieldExprChain(source, type_node, chain, k);
        if (len >= 2) return len;

        // Standard extraction got only the right side; the left side is wrapped
        // in pointer_type or nullable_type.
        // Unwrap the left child to recover the import identifier.
        const first_child = type_node.child(0) orelse return len;
        const unwrapped = unwrapTypeNode(first_child, k);
        if (unwrapped.kindId() == k.identifier and len == 1) {
            // Shift the existing segment ("Service") right, insert import name left.
            chain[1] = chain[0];
            chain[0] = ts_api.nodeText(source, unwrapped);
            return 2;
        }

        return len;
    }

    // Pointer/optional/error-union wrapping a field_expression: unwrap and recurse.
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

/// Unwrap pointer_type, optional_type, and error_union_type AST wrappers to
/// get the base type node. For bare identifiers, returns unchanged.
fn unwrapTypeNode(type_node: ts.Node, k: *const KindIds) ts.Node {
    const kid = type_node.kindId();
    // pointer_type: *T, *const T. Pointee is the last named child.
    if (kid == k.pointer_type) {
        const count = type_node.namedChildCount();
        if (count > 0) {
            if (type_node.namedChild(count - 1)) |inner| {
                return unwrapTypeNode(inner, k);
            }
        }
        return type_node;
    }
    // nullable_type / optional_type: ?T
    if (kid == k.nullable_type or kid == k.optional_type) {
        if (type_node.namedChild(0)) |inner| {
            return unwrapTypeNode(inner, k);
        }
        return type_node;
    }
    // error_union_type: E!T. Result type is the last named child.
    if (kid == k.error_union_type) {
        const count = type_node.namedChildCount();
        if (count > 0) {
            if (type_node.namedChild(count - 1)) |inner| {
                return unwrapTypeNode(inner, k);
            }
        }
        return type_node;
    }
    return type_node;
}

/// Recursively scan an AST subtree for type identifiers and create
/// `uses_type` edges. Skips edges to the caller's own child declarations.
/// Stops at nested fn/test boundaries. Depth-capped.
fn scanForTypeIdentifiersScoped(allocator: std.mem.Allocator, sctx: *const ScanContext, ts_node: ts.Node, depth: u32) !void {
    // Graceful cap: stop descending, type refs in deeper subtrees are skipped.
    if (depth >= cf.max_ast_scan_depth) return;
    const kid = ts_node.kindId();
    if (kid == sctx.k.identifier or kid == sctx.k.property_identifier) {
        const name = ts_api.nodeText(sctx.source, ts_node);
        const target_id = findTypeByNameScoped(sctx.g, name, sctx.edge_ctx.scope_start, sctx.edge_ctx.scope_end, sctx.caller_parent_id, &sctx.graph_index.scope) orelse
            findTypeCrossFile(sctx.g, name, sctx.edge_ctx, &sctx.graph_index.scope, sctx.phantom_mgr) orelse {
            // No match locally or cross-file; skip.
            return;
        };
        // Skip edges to direct children of caller_id (prevents type_def to own inner declarations).
        const target_node = sctx.g.getNode(target_id);
        const is_own_child = if (target_node) |tn| tn.parent_id != null and tn.parent_id.? == sctx.caller_id else false;
        if (!is_own_child) {
            _ = try sctx.g.addEdgeIfNew(allocator, .{ .source_id = sctx.caller_id, .target_id = target_id, .edge_type = .uses_type });
        }
    }

    // Recurse into all children, but stop at scope boundaries.
    var i: u32 = 0;
    while (i < ts_node.childCount()) : (i += 1) {
        const child = ts_node.child(i) orelse continue;
        const child_kid = child.kindId();
        if (child_kid == sctx.k.function_declaration or
            child_kid == sctx.k.test_declaration) continue;
        try scanForTypeIdentifiersScoped(allocator, sctx, child, depth + 1);
    }
}

fn findTypeCrossFile(g: *const Graph, name: []const u8, ctx: *const EdgeContext, scope_index: *const ScopeIndex, phantom_mgr: *const PhantomManager) ?NodeId {
    return shared_lookup.findTypeCrossFile(g, name, ctx, scope_index, phantom_mgr);
}

fn findTypeByNameScoped(g: *const Graph, name: []const u8, scope_start: usize, scope_end: usize, caller_parent_id: ?NodeId, scope_index: *const ScopeIndex) ?NodeId {
    return shared_lookup.findTypeByNameScoped(g, name, scope_start, scope_end, caller_parent_id, scope_index, isTypeReference);
}

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

fn findFunctionByNameScoped(g: *const Graph, name: []const u8, scope_start: usize, scope_end: usize, caller_parent_id: ?NodeId, scope_index: *const ScopeIndex) ?NodeId {
    return shared_lookup.findFunctionByNameScoped(g, name, scope_start, scope_end, caller_parent_id, scope_index, &.{});
}

fn findFunctionByNameAndLine(g: *const Graph, name: []const u8, line: u32, scope_start: usize, scope_end: usize) ?NodeId {
    return shared_lookup.findFunctionByNameAndLine(g, name, line, scope_start, scope_end);
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
