const std = @import("std");
const graph_mod = @import("../../core/graph.zig");
const logging = @import("../../logging.zig");
const edge_mod = @import("../../core/edge.zig");
const types = @import("../../core/types.zig");
const ts = @import("tree-sitter");
const ts_api = @import("../../parser/tree_sitter_api.zig");
const ast = @import("ast_analysis.zig");
const cf = @import("cross_file.zig");
const pc = @import("parse_context.zig");

const Field = logging.Field;
const Logger = logging.Logger;

const Graph = graph_mod.Graph;
const NodeId = types.NodeId;
const NodeKind = types.NodeKind;
const EdgeType = types.EdgeType;
const EdgeSource = types.EdgeSource;

const EdgeContext = cf.EdgeContext;
const VarTracker = cf.VarTracker;
const KindIds = pc.KindIds;
const ScopeIndex = pc.ScopeIndex;
const FileIndex = pc.FileIndex;

/// Tracks per-function local variable bindings from struct literal or
/// static-method-call initializers. Used to resolve method calls like
/// `p.manhattan()` where `const p = Point{ .x = 3, .y = -4 }`.
const LocalTypeTracker = struct {
    var_names: [max_vars][]const u8 = undefined,
    type_names: [max_vars][]const u8 = undefined,
    var_count: usize = 0,

    const max_vars = 32;

    fn addBinding(self: *LocalTypeTracker, name: []const u8, type_name: []const u8) void {
        if (self.var_count >= max_vars) return;
        self.var_names[self.var_count] = name;
        self.type_names[self.var_count] = type_name;
        self.var_count += 1;
    }

    fn findTypeName(self: *const LocalTypeTracker, name: []const u8) ?[]const u8 {
        for (self.var_names[0..self.var_count], self.type_names[0..self.var_count]) |vname, tname| {
            if (std.mem.eql(u8, vname, name)) return tname;
        }
        return null;
    }
};

/// Tracks per-function parameter bindings whose type is import-qualified.
/// Used to resolve method calls like `svc.process()` where the parameter
/// `svc` has type `svc_mod.Service` (an import-qualified struct from another file).
const ParamTypeTracker = struct {
    names: [max_params][]const u8 = undefined,
    target_files: [max_params]NodeId = undefined,
    type_chains: [max_params][cf.max_chain_depth][]const u8 = undefined,
    chain_lens: [max_params]usize = undefined,
    count: usize = 0,

    const max_params = 16;

    fn addBinding(self: *ParamTypeTracker, name: []const u8, target_file: NodeId, chain: []const []const u8) void {
        if (self.count >= max_params or chain.len == 0) return;
        self.names[self.count] = name;
        self.target_files[self.count] = target_file;
        const copy_len = @min(chain.len, cf.max_chain_depth);
        for (chain[0..copy_len], 0..) |seg, i| {
            self.type_chains[self.count][i] = seg;
        }
        self.chain_lens[self.count] = copy_len;
        self.count += 1;
    }

    const ParamOrigin = struct {
        target_file: NodeId,
        type_chain: []const []const u8,
    };

    fn findOrigin(self: *const ParamTypeTracker, name: []const u8) ?ParamOrigin {
        for (0..self.count) |i| {
            if (std.mem.eql(u8, self.names[i], name)) {
                return .{
                    .target_file = self.target_files[i],
                    .type_chain = self.type_chains[i][0..self.chain_lens[i]],
                };
            }
        }
        return null;
    }
};

// =========================================================================
// ScanContext: per-function/test scan state
// =========================================================================

/// Groups all per-function scan state into one struct passed by pointer,
/// replacing many loose parameters on scanForCalls and scanForTypeIdentifiers.
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
    scope_index: *const ScopeIndex,
    file_index: *const FileIndex,
    log: Logger,
};

// =========================================================================
// Edge creation
// =========================================================================

pub fn walkForEdges(g: *Graph, source: []const u8, ts_node: ts.Node, ctx: *const EdgeContext, k: *const KindIds, scope_index: *const ScopeIndex, file_index: *const FileIndex, log: Logger) !void {
    const kid = ts_node.kindId();

    if (kid == k.function_declaration) {
        if (ast.getIdentifierName(source, ts_node, k)) |name| {
            // Look up the function node by name and line position to correctly
            // identify which graph node corresponds to this AST declaration
            // when multiple functions share the same name.
            const decl_line = ts_node.startPoint().row + 1;
            if (findFunctionByNameAndLine(g, name, decl_line, ctx.scope_start, ctx.scope_end)) |fn_id| {
                const fn_node = g.getNode(fn_id);
                const fn_parent_id = if (fn_node) |n| n.parent_id else null;

                // For generic type-returning functions (stored as type_def or
                // enum_def), inner declarations (Self, init, etc.) are children
                // of fn_id itself, not of fn_parent_id. Use fn_id as the scope
                // to prevent cross-scope Self resolution.
                const is_type_scope = if (fn_node) |n|
                    n.kind == .type_def or n.kind == .enum_def
                else
                    false;
                const caller_scope_id = if (is_type_scope) fn_id else fn_parent_id;

                // Build all per-function trackers in a single block walk.
                var var_tracker = VarTracker{};
                var local_type_tracker = LocalTypeTracker{};
                var param_type_tracker = ParamTypeTracker{};
                prescanForParamTypeBindings(source, ts_node, ctx, k, &param_type_tracker);
                prescanBlock(g, source, ts_node, ctx, k, &var_tracker, &local_type_tracker, &param_type_tracker, scope_index, file_index, log);

                // Build ScanContext for this function.
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
                    .scope_index = scope_index,
                    .file_index = file_index,
                    .log = log,
                };

                // Scan function body for call expressions.
                try scanForCalls(&sctx, ts_node, 0);

                // Scan full function node (signature + body) for type references.
                try scanForTypeIdentifiersScoped(&sctx, ts_node, 0);
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
            const test_node = g.getNode(test_id);
            const test_parent_id = if (test_node) |n| n.parent_id else null;

            // Build all per-test trackers in a single block walk.
            var var_tracker = VarTracker{};
            var local_type_tracker = LocalTypeTracker{};
            var param_type_tracker = ParamTypeTracker{};
            prescanForParamTypeBindings(source, ts_node, ctx, k, &param_type_tracker);
            prescanBlock(g, source, ts_node, ctx, k, &var_tracker, &local_type_tracker, &param_type_tracker, scope_index, file_index, log);

            // Build ScanContext for this test.
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
                .scope_index = scope_index,
                .file_index = file_index,
                .log = log,
            };

            // Scan test body for call expressions.
            try scanForCalls(&sctx, ts_node, 0);

            // Scan test node for type references.
            try scanForTypeIdentifiersScoped(&sctx, ts_node, 0);
        } else {
            log.trace("test not found in graph", &.{Field.string("name", test_name)});
        }
    }

    // Recurse into named children only. Declarations (fn, test, struct) are always
    // named nodes in tree-sitter, so skipping anonymous nodes is safe and avoids
    // descending into expression-level subtrees that walkForEdges doesn't handle.
    var i: u32 = 0;
    while (i < ts_node.namedChildCount()) : (i += 1) {
        const child = ts_node.namedChild(i) orelse continue;
        try walkForEdges(g, source, child, ctx, k, scope_index, file_index, log);
    }
}

fn scanForCalls(sctx: *const ScanContext, ts_node: ts.Node, depth: u32) !void {
    // Graceful cap: stop descending, edges in deeper subtrees are skipped.
    if (depth >= cf.max_ast_scan_depth) {
        sctx.log.trace("scan depth cap reached", &.{Field.uint("depth", depth)});
        return;
    }
    if (ts_node.kindId() == sctx.k.call_expression) {
        if (ts_node.namedChild(0)) |fn_ref| {
            const fn_ref_kid = fn_ref.kindId();

            if (fn_ref_kid == sctx.k.identifier) {
                // Bare call: foo(). Resolve within caller's scope.
                const callee_name = ts_api.nodeText(sctx.source, fn_ref);
                if (findFunctionByNameScoped(sctx.g, callee_name, sctx.edge_ctx.scope_start, sctx.edge_ctx.scope_end, sctx.caller_parent_id, sctx.scope_index)) |callee_id| {
                    if (@intFromEnum(sctx.caller_id) != @intFromEnum(callee_id)) {
                        try cf.addEdgeIfNew(sctx.g, sctx.caller_id, callee_id, .calls, sctx.log);
                    }
                } else {
                    sctx.log.trace("bare call unresolved", &.{Field.string("callee", callee_name)});
                }
            } else if (fn_ref_kid == sctx.k.field_expression) {
                // Qualified call: extract full chain from nested field_expression.
                var chain: [cf.max_chain_depth][]const u8 = undefined;
                const chain_len = cf.collectFieldExprChain(sctx.source, fn_ref, &chain, sctx.k);

                if (chain_len >= 2) {
                    const root_name = chain[0];

                    if (sctx.edge_ctx.findImportTarget(root_name)) |target_file_id| {
                        // Import-qualified call: alpha.Self.init(42).
                        try cf.resolveQualifiedCall(sctx.g, sctx.caller_id, target_file_id, chain[1..chain_len], true, sctx.scope_index, sctx.file_index, sctx.log);
                    } else if (sctx.var_tracker.findTarget(root_name)) |target_file_id| {
                        // Variable method call: a.deinit() where a was assigned from import.
                        try cf.resolveQualifiedCall(sctx.g, sctx.caller_id, target_file_id, chain[1..chain_len], true, sctx.scope_index, sctx.file_index, sctx.log);
                    } else if (sctx.local_tracker.findTypeName(root_name)) |type_name| {
                        // Struct-literal local variable: p.method() where const p = Point{...}.
                        // Resolve method as a child of the type using the scope index.
                        if (findTypeByNameScoped(sctx.g, type_name, sctx.edge_ctx.scope_start, sctx.edge_ctx.scope_end, sctx.caller_parent_id, sctx.scope_index)) |type_id| {
                            const leaf_name = chain[chain_len - 1];
                            for (sctx.scope_index.childrenOf(type_id)) |child_idx| {
                                const n = sctx.g.nodes.items[child_idx];
                                if (n.kind == .function and
                                    std.mem.eql(u8, n.name, leaf_name))
                                {
                                    try cf.addEdgeIfNew(sctx.g, sctx.caller_id, @enumFromInt(child_idx), .calls, sctx.log);
                                    break;
                                }
                            }
                        }
                    } else if (sctx.param_tracker.findOrigin(root_name)) |origin| {
                        // Parameter with import-qualified type: svc.method() where
                        // svc's type comes from an imported module (e.g., svc_mod.Service).
                        // Build resolution chain: [TypeName, ..., method_name].
                        var resolve_chain: [cf.max_chain_depth][]const u8 = undefined;
                        var resolve_len: usize = 0;
                        for (origin.type_chain) |seg| {
                            if (resolve_len >= cf.max_chain_depth) break;
                            resolve_chain[resolve_len] = seg;
                            resolve_len += 1;
                        }
                        for (chain[1..chain_len]) |seg| {
                            if (resolve_len >= cf.max_chain_depth) break;
                            resolve_chain[resolve_len] = seg;
                            resolve_len += 1;
                        }
                        try cf.resolveQualifiedCall(sctx.g, sctx.caller_id, origin.target_file, resolve_chain[0..resolve_len], true, sctx.scope_index, sctx.file_index, sctx.log);
                    } else {
                        // Fallback: use the AST to determine if the receiver is
                        // local (self or a known local type) vs external.
                        const leaf_name = chain[chain_len - 1];
                        const receiver = classifyReceiver(sctx.g, sctx.source, fn_ref, sctx.edge_ctx.scope_start, sctx.edge_ctx.scope_end, sctx.caller_parent_id, sctx.fn_decl_node, sctx.k, sctx.scope_index);
                        switch (receiver) {
                            .self_receiver => {
                                // self.method(). Resolve in caller's parent scope.
                                if (findFunctionByNameScoped(sctx.g, leaf_name, sctx.edge_ctx.scope_start, sctx.edge_ctx.scope_end, sctx.caller_parent_id, sctx.scope_index)) |callee_id| {
                                    if (@intFromEnum(sctx.caller_id) != @intFromEnum(callee_id)) {
                                        try cf.addEdgeIfNew(sctx.g, sctx.caller_id, callee_id, .calls, sctx.log);
                                    }
                                }
                            },
                            .local_type => {
                                // Type.staticMethod(). Resolve in caller's scope.
                                if (findFunctionByNameScoped(sctx.g, leaf_name, sctx.edge_ctx.scope_start, sctx.edge_ctx.scope_end, sctx.caller_parent_id, sctx.scope_index)) |callee_id| {
                                    if (@intFromEnum(sctx.caller_id) != @intFromEnum(callee_id)) {
                                        try cf.addEdgeIfNew(sctx.g, sctx.caller_id, callee_id, .calls, sctx.log);
                                    }
                                }
                            },
                            .external => {
                                // param.method() or var.method() where var holds
                                // an external type. Do not create a local edge.
                                sctx.log.trace("external receiver, skipping edge", &.{
                                    Field.string("root", root_name),
                                    Field.string("leaf", leaf_name),
                                });
                            },
                        }
                    }
                } else if (chain_len == 1) {
                    // Single-segment field expression, treat as bare call.
                    if (findFunctionByNameScoped(sctx.g, chain[0], sctx.edge_ctx.scope_start, sctx.edge_ctx.scope_end, sctx.caller_parent_id, sctx.scope_index)) |callee_id| {
                        if (@intFromEnum(sctx.caller_id) != @intFromEnum(callee_id)) {
                            try cf.addEdgeIfNew(sctx.g, sctx.caller_id, callee_id, .calls, sctx.log);
                        }
                    }
                }
            }
        }
    }

    // Recurse into all children, not just named ones. Call expressions like
    // `foo()` can appear inside anonymous nodes (e.g., assignment right-hand sides,
    // return values) that namedChild() would skip. Stop at scope boundaries.
    var i: u32 = 0;
    while (i < ts_node.childCount()) : (i += 1) {
        const child = ts_node.child(i) orelse continue;
        const child_kid = child.kindId();
        if (child_kid == sctx.k.function_declaration or
            child_kid == sctx.k.test_declaration) continue;
        try scanForCalls(sctx, child, depth + 1);
    }
}

/// Classify the receiver (first child) of a field_expression to determine
/// whether it refers to `self`, a locally-defined type, or an external entity.
const ReceiverKind = enum { self_receiver, local_type, external };

fn classifyReceiver(g: *const Graph, source: []const u8, field_expr: ts.Node, scope_start: usize, scope_end: usize, caller_parent_id: ?NodeId, fn_decl_node: ts.Node, k: *const KindIds, scope_index: *const ScopeIndex) ReceiverKind {
    // The receiver is the first named child of the outermost field_expression.
    // For nested chains like `a.b.c()`, we want the leftmost identifier.
    const receiver_node = getLeftmostIdentifier(field_expr, k) orelse return .external;
    const receiver_name = ts_api.nodeText(source, receiver_node);

    // Check if receiver is 'self', i.e. the identifier "self".
    if (std.mem.eql(u8, receiver_name, "self")) {
        return .self_receiver;
    }

    // Check if receiver matches a locally-defined type or type-alias constant.
    if (findTypeByNameScoped(g, receiver_name, scope_start, scope_end, caller_parent_id, scope_index) != null) {
        return .local_type;
    }

    // Check if receiver matches a parameter whose type is a locally-defined type.
    // For example: fn process(p: Point) void { p.method(); }. "p" is a parameter
    // of type "Point", and Point is a local type, so p.method() should resolve locally.
    if (findParamTypeName(source, fn_decl_node, receiver_name, k)) |type_name| {
        if (findTypeByNameScoped(g, type_name, scope_start, scope_end, caller_parent_id, scope_index) != null) {
            return .local_type;
        }
    }

    return .external;
}

/// Given a function_declaration AST node and a parameter name, return
/// the type identifier if the parameter's type is a bare identifier
/// (e.g. `p: Point` returns "Point"). Returns null for pointer types,
/// optional types, or if the parameter is not found.
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
            // Only handle bare identifier types (e.g. Point), not pointer (*Point)
            // or optional (?Point).
            if (type_node.kindId() == k.identifier) {
                return ts_api.nodeText(source, type_node);
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

// =========================================================================
// Merged prescan: single block walk populating all three trackers
// =========================================================================

/// Pre-scan a function/test body, populating VarTracker (import-qualified
/// variable bindings), LocalTypeTracker (struct literal/static call bindings),
/// and ParamTypeTracker (if-capture bindings) in a single block walk.
fn prescanBlock(
    g: *const Graph,
    source: []const u8,
    fn_node: ts.Node,
    ctx: *const EdgeContext,
    k: *const KindIds,
    var_tracker: *VarTracker,
    local_tracker: *LocalTypeTracker,
    param_tracker: *ParamTypeTracker,
    scope_index: *const ScopeIndex,
    file_index: *const FileIndex,
    log: Logger,
) void {
    var i: u32 = 0;
    while (i < fn_node.childCount()) : (i += 1) {
        const child = fn_node.child(i) orelse continue;
        if (child.kindId() == k.block) {
            scanBlockPrescan(g, source, child, ctx, k, var_tracker, local_tracker, param_tracker, scope_index, file_index, log);
            return;
        }
    }
}

fn scanBlockPrescan(
    g: *const Graph,
    source: []const u8,
    block: ts.Node,
    ctx: *const EdgeContext,
    k: *const KindIds,
    var_tracker: *VarTracker,
    local_tracker: *LocalTypeTracker,
    param_tracker: *ParamTypeTracker,
    scope_index: *const ScopeIndex,
    file_index: *const FileIndex,
    log: Logger,
) void {
    var i: u32 = 0;
    while (i < block.childCount()) : (i += 1) {
        const child = block.child(i) orelse continue;
        const kid = child.kindId();

        if (kid == k.variable_declaration) {
            const var_name = ast.getIdentifierName(source, child, k) orelse continue;
            // VarTracker: import-qualified initializer.
            if (cf.findImportQualifiedRoot(source, child, ctx, k)) |target_file_id| {
                const resolved = cf.resolveVarTargetThroughReturnType(g, source, child, ctx, k, target_file_id, scope_index, file_index, log) orelse target_file_id;
                var_tracker.addBinding(var_name, resolved, log);
            }
            // LocalTypeTracker: struct literal or static method call initializer.
            if (extractStructLiteralType(source, child)) |type_name| {
                local_tracker.addBinding(var_name, type_name);
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
                    param_tracker.addBinding(capture_name.?, origin.target_file, origin.type_chain);
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
            scanBlockPrescan(g, source, child, ctx, k, var_tracker, local_tracker, param_tracker, scope_index, file_index, log);
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
    while (pos < text.len and pc.isWhitespace(text[pos])) : (pos += 1) {}

    // Skip optional 'try' keyword.
    if (pos + 3 <= text.len and std.mem.eql(u8, text[pos..][0..3], "try") and
        (pos + 3 >= text.len or !pc.isIdentChar(text[pos + 3])))
    {
        pos += 3;
        while (pos < text.len and pc.isWhitespace(text[pos])) : (pos += 1) {}
    }

    // Must start with an uppercase letter (PascalCase type name).
    if (pos >= text.len or text[pos] < 'A' or text[pos] > 'Z') return null;
    const id_start = pos;
    while (pos < text.len and pc.isIdentChar(text[pos])) : (pos += 1) {}
    if (pos >= text.len) return null;

    // Must be followed by '{' (struct literal) or '.' (static method/field).
    if (text[pos] != '{' and text[pos] != '.') return null;

    // Return a slice of source (not of local text).
    return source[start + id_start .. start + pos];
}

// =========================================================================
// Parameter type binding prescan
// =========================================================================

/// Pre-scan a function's parameter list for import-qualified types.
/// For each parameter whose type is `mod.Type` (possibly wrapped in `*` or `?`),
/// record a binding from the parameter name to the import target file and type chain.
fn prescanForParamTypeBindings(
    source: []const u8,
    fn_decl_node: ts.Node,
    ctx: *const EdgeContext,
    k: *const KindIds,
    tracker: *ParamTypeTracker,
) void {
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
                    tracker.addBinding(name, target_file_id, chain[1..chain_len]);
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
        // in pointer_type or nullable_type (e.g., *svc_mod parses as pointer_type).
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

fn scanForTypeIdentifiersScoped(sctx: *const ScanContext, ts_node: ts.Node, depth: u32) !void {
    // Graceful cap: stop descending, type refs in deeper subtrees are skipped.
    if (depth >= cf.max_ast_scan_depth) return;
    const kid = ts_node.kindId();
    if (kid == sctx.k.identifier or kid == sctx.k.property_identifier) {
        const name = ts_api.nodeText(sctx.source, ts_node);
        const target_id = findTypeByNameScoped(sctx.g, name, sctx.edge_ctx.scope_start, sctx.edge_ctx.scope_end, sctx.caller_parent_id, sctx.scope_index) orelse
            findTypeCrossFile(sctx.g, name, sctx.edge_ctx, sctx.scope_index) orelse {
            // No match locally or cross-file; skip.
            return;
        };
        // Skip edges to direct children of caller_id (prevents type_def to own inner declarations).
        const target_node = sctx.g.getNode(target_id);
        const is_own_child = if (target_node) |tn| tn.parent_id != null and tn.parent_id.? == sctx.caller_id else false;
        if (!is_own_child) {
            try cf.addEdgeIfNew(sctx.g, sctx.caller_id, target_id, .uses_type, sctx.log);
        }
    }

    // Recurse into all children, but stop at scope boundaries.
    var i: u32 = 0;
    while (i < ts_node.childCount()) : (i += 1) {
        const child = ts_node.child(i) orelse continue;
        const child_kid = child.kindId();
        if (child_kid == sctx.k.function_declaration or
            child_kid == sctx.k.test_declaration) continue;
        try scanForTypeIdentifiersScoped(sctx, child, depth + 1);
    }
}

/// Search imported files for a type_def or enum_def with the given PascalCase name.
/// Returns the target NodeId only if exactly one match is found (unambiguous).
fn findTypeCrossFile(g: *const Graph, name: []const u8, ctx: *const EdgeContext, scope_index: *const ScopeIndex) ?NodeId {
    if (name.len == 0 or name[0] < 'A' or name[0] > 'Z') return null;
    var match: ?NodeId = null;
    var match_count: usize = 0;
    for (ctx.import_targets[0..ctx.import_count]) |target_file_id| {
        for (scope_index.childrenOf(target_file_id)) |child_idx| {
            const n = g.nodes.items[child_idx];
            if (n.kind != .type_def and n.kind != .enum_def) continue;
            if (!std.mem.eql(u8, n.name, name)) continue;
            match = @enumFromInt(child_idx);
            match_count += 1;
        }
    }
    if (match_count == 1) return match;
    return null;
}

// =========================================================================
// Graph lookup functions
// =========================================================================

/// Find a type/constant node by name, with scope-aware resolution.
/// Walks up the parent_id chain from caller_parent_id, preferring
/// types in the narrowest scope first.
fn findTypeByNameScoped(g: *const Graph, name: []const u8, scope_start: usize, scope_end: usize, caller_parent_id: ?NodeId, scope_index: *const ScopeIndex) ?NodeId {
    if (caller_parent_id) |cpid| {
        var current_scope: ?NodeId = cpid;
        var hops: usize = 0;
        while (current_scope != null and hops < 100) : (hops += 1) {
            const scope_id = current_scope.?;
            for (scope_index.childrenOf(scope_id)) |child_idx| {
                const n = g.nodes.items[child_idx];
                if (isTypeReference(n, name)) return @enumFromInt(child_idx);
            }
            const scope_node = g.getNode(scope_id) orelse break;
            current_scope = scope_node.parent_id;
        }
    }
    // Fallback: flat file-scope search, return only if unambiguous.
    const scoped_nodes = g.nodes.items[scope_start..scope_end];
    var sole_match: ?NodeId = null;
    var match_count: usize = 0;
    for (scoped_nodes, scope_start..) |n, idx| {
        if (isTypeReference(n, name)) {
            sole_match = @enumFromInt(idx);
            match_count += 1;
            if (match_count > 1) return null;
        }
    }
    return sole_match;
}

/// Check whether a graph node represents a type reference with the given name.
/// Matches type_def, enum_def, PascalCase constants (type aliases), and
/// PascalCase import_decl nodes (e.g. `const ZigMeta = @import("...").ZigMeta`).
fn isTypeReference(n: @import("../../core/node.zig").Node, name: []const u8) bool {
    if (!std.mem.eql(u8, n.name, name)) return false;
    if (n.kind == .type_def or n.kind == .enum_def) return true;
    const is_pascal = n.name.len > 0 and n.name[0] >= 'A' and n.name[0] <= 'Z';
    if (is_pascal and (n.kind == .constant or n.kind == .import_decl)) return true;
    return false;
}

fn findFunctionByNameScoped(g: *const Graph, name: []const u8, scope_start: usize, scope_end: usize, caller_parent_id: ?NodeId, scope_index: *const ScopeIndex) ?NodeId {
    if (caller_parent_id) |cpid| {
        // Walk up the parent chain, searching at each scope level.
        var current_scope: ?NodeId = cpid;
        var hops: usize = 0;
        while (current_scope != null and hops < 100) : (hops += 1) {
            const scope_id = current_scope.?;
            for (scope_index.childrenOf(scope_id)) |child_idx| {
                const n = g.nodes.items[child_idx];
                if (n.kind == .function and std.mem.eql(u8, n.name, name)) {
                    return @enumFromInt(child_idx);
                }
            }
            // Move up to the parent's parent.
            const scope_node = g.getNode(scope_id) orelse break;
            current_scope = scope_node.parent_id;
        }
    }
    // Fallback: flat file-scope search, return only if unambiguous.
    const scoped_nodes = g.nodes.items[scope_start..scope_end];
    var sole_match: ?NodeId = null;
    var match_count: usize = 0;
    for (scoped_nodes, scope_start..) |n, i| {
        if (n.kind == .function and std.mem.eql(u8, n.name, name)) {
            sole_match = @enumFromInt(i);
            match_count += 1;
            if (match_count > 1) return null;
        }
    }
    return sole_match;
}

/// Find a function node by name and line number. Used by walkForEdges to
/// identify the correct graph node when multiple functions share the same
/// name (e.g. init in two different structs).
fn findFunctionByNameAndLine(g: *const Graph, name: []const u8, line: u32, scope_start: usize, scope_end: usize) ?NodeId {
    const scoped_nodes = g.nodes.items[scope_start..scope_end];
    for (scoped_nodes, scope_start..) |n, i| {
        if (n.kind == .function and std.mem.eql(u8, n.name, name) and
            n.line_start != null and n.line_start.? == line)
        {
            return @enumFromInt(i);
        }
    }
    // Fallback: for generic type-returning functions that are stored as
    // type_def nodes, match by name and line.
    for (scoped_nodes, scope_start..) |n, i| {
        if ((n.kind == .type_def or n.kind == .enum_def) and
            std.mem.eql(u8, n.name, name) and
            n.line_start != null and n.line_start.? == line)
        {
            return @enumFromInt(i);
        }
    }
    return null;
}

fn findTestByName(g: *const Graph, name: []const u8, scope_start: usize, scope_end: usize) ?NodeId {
    const scoped_nodes = g.nodes.items[scope_start..scope_end];
    for (scoped_nodes, scope_start..) |n, i| {
        if (n.kind == .test_def and std.mem.eql(u8, n.name, name)) {
            return @enumFromInt(i);
        }
    }
    return null;
}
