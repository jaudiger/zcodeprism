const std = @import("std");
const graph_mod = @import("../../core/graph.zig");
const edge_mod = @import("../../core/edge.zig");
const types = @import("../../core/types.zig");
const ts = @import("tree-sitter");
const ts_api = @import("../../parser/tree_sitter_api.zig");
const ast = @import("ast_analysis.zig");
const cf = @import("cross_file.zig");

const Graph = graph_mod.Graph;
const NodeId = types.NodeId;
const NodeKind = types.NodeKind;
const EdgeType = types.EdgeType;
const EdgeSource = types.EdgeSource;

const EdgeContext = cf.EdgeContext;
const VarTracker = cf.VarTracker;

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
// Edge creation
// =========================================================================

pub fn walkForEdges(g: *Graph, source: []const u8, ts_node: ts.Node, ctx: *const EdgeContext) !void {
    const kind_str = ts_node.kind();

    if (std.mem.eql(u8, kind_str, "function_declaration")) {
        if (ast.getIdentifierName(source, ts_node)) |name| {
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

                // Build per-function variable tracker for import-assigned variables.
                var var_tracker = VarTracker{};
                cf.prescanForVarBindings(g, source, ts_node, ctx, &var_tracker);

                // Build per-function local type tracker for struct-literal bindings.
                var local_type_tracker = LocalTypeTracker{};
                prescanForLocalTypeBindings(source, ts_node, &local_type_tracker);

                // Build per-function parameter type tracker for import-qualified params.
                var param_type_tracker = ParamTypeTracker{};
                prescanForParamTypeBindings(source, ts_node, ctx, &param_type_tracker);
                prescanForIfCaptures(source, ts_node, &param_type_tracker);

                // Scan function body for call expressions.
                try scanForCalls(g, source, fn_id, caller_scope_id, ts_node, ctx, &var_tracker, &local_type_tracker, &param_type_tracker, ts_node, 0);

                // Scan function signature (excluding body) for type references.
                try scanSignatureForTypes(g, source, fn_id, caller_scope_id, ts_node, ctx);

                // Scan function body for type references (struct literals, static
                // method calls, comptime type arguments).
                try scanBodyForTypes(g, source, fn_id, caller_scope_id, ts_node, ctx);
            }
        }
    } else if (std.mem.eql(u8, kind_str, "test_declaration")) {
        const test_name = ast.getTestName(source, ts_node);
        if (findTestByName(g, test_name, ctx.scope_start, ctx.scope_end)) |test_id| {
            const test_node = g.getNode(test_id);
            const test_parent_id = if (test_node) |n| n.parent_id else null;
            var var_tracker = VarTracker{};
            cf.prescanForVarBindings(g, source, ts_node, ctx, &var_tracker);
            var local_type_tracker = LocalTypeTracker{};
            prescanForLocalTypeBindings(source, ts_node, &local_type_tracker);
            var param_type_tracker = ParamTypeTracker{};
            prescanForParamTypeBindings(source, ts_node, ctx, &param_type_tracker);
            prescanForIfCaptures(source, ts_node, &param_type_tracker);
            try scanForCalls(g, source, test_id, test_parent_id, ts_node, ctx, &var_tracker, &local_type_tracker, &param_type_tracker, ts_node, 0);
            try scanBodyForTypes(g, source, test_id, test_parent_id, ts_node, ctx);
        }
    }

    // Recurse into named children only. Declarations (fn, test, struct) are always
    // named nodes in tree-sitter, so skipping anonymous nodes is safe and avoids
    // descending into expression-level subtrees that walkForEdges doesn't handle.
    var i: u32 = 0;
    while (i < ts_node.namedChildCount()) : (i += 1) {
        const child = ts_node.namedChild(i) orelse continue;
        try walkForEdges(g, source, child, ctx);
    }
}

fn scanForCalls(g: *Graph, source: []const u8, caller_id: NodeId, caller_parent_id: ?NodeId, ts_node: ts.Node, ctx: *const EdgeContext, tracker: *const VarTracker, local_tracker: *const LocalTypeTracker, param_tracker: *const ParamTypeTracker, fn_decl_node: ts.Node, depth: u32) !void {
    // Graceful cap: stop descending, edges in deeper subtrees are skipped.
    if (depth >= cf.max_ast_scan_depth) return;
    if (std.mem.eql(u8, ts_node.kind(), "call_expression")) {
        if (ts_node.namedChild(0)) |fn_ref| {
            const fn_ref_kind = fn_ref.kind();

            if (std.mem.eql(u8, fn_ref_kind, "identifier")) {
                // Bare call: foo(). Resolve within caller's scope.
                const callee_name = ts_api.nodeText(source, fn_ref);
                if (findFunctionByNameScoped(g, callee_name, ctx.scope_start, ctx.scope_end, caller_parent_id)) |callee_id| {
                    if (@intFromEnum(caller_id) != @intFromEnum(callee_id)) {
                        try cf.addEdgeIfNew(g, caller_id, callee_id, .calls);
                    }
                }
            } else if (std.mem.eql(u8, fn_ref_kind, "field_expression")) {
                // Qualified call: extract full chain from nested field_expression.
                var chain: [cf.max_chain_depth][]const u8 = undefined;
                const chain_len = cf.collectFieldExprChain(source, fn_ref, &chain);

                if (chain_len >= 2) {
                    const root_name = chain[0];

                    if (ctx.findImportTarget(root_name)) |target_file_id| {
                        // Import-qualified call: alpha.Self.init(42).
                        try cf.resolveQualifiedCall(g, caller_id, target_file_id, chain[1..chain_len], true);
                    } else if (tracker.findTarget(root_name)) |target_file_id| {
                        // Variable method call: a.deinit() where a was assigned from import.
                        try cf.resolveQualifiedCall(g, caller_id, target_file_id, chain[1..chain_len], true);
                    } else if (local_tracker.findTypeName(root_name)) |type_name| {
                        // Struct-literal local variable: p.method() where const p = Point{...}.
                        // Resolve method as a child of the type.
                        if (findTypeByNameScoped(g, type_name, ctx.scope_start, ctx.scope_end, caller_parent_id)) |type_id| {
                            const leaf_name = chain[chain_len - 1];
                            const scoped = g.nodes.items[ctx.scope_start..ctx.scope_end];
                            for (scoped, ctx.scope_start..) |n, idx| {
                                if (n.kind == .function and
                                    n.parent_id != null and n.parent_id.? == type_id and
                                    std.mem.eql(u8, n.name, leaf_name))
                                {
                                    try cf.addEdgeIfNew(g, caller_id, @enumFromInt(idx), .calls);
                                    break;
                                }
                            }
                        }
                    } else if (param_tracker.findOrigin(root_name)) |origin| {
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
                        try cf.resolveQualifiedCall(g, caller_id, origin.target_file, resolve_chain[0..resolve_len], true);
                    } else {
                        // Fallback: use the AST to determine if the receiver is
                        // local (self or a known local type) vs external.
                        const leaf_name = chain[chain_len - 1];
                        const receiver = classifyReceiver(g, source, fn_ref, ctx.scope_start, ctx.scope_end, caller_parent_id, fn_decl_node);
                        switch (receiver) {
                            .self_receiver => {
                                // self.method(). Resolve in caller's parent scope.
                                if (findFunctionByNameScoped(g, leaf_name, ctx.scope_start, ctx.scope_end, caller_parent_id)) |callee_id| {
                                    if (@intFromEnum(caller_id) != @intFromEnum(callee_id)) {
                                        try cf.addEdgeIfNew(g, caller_id, callee_id, .calls);
                                    }
                                }
                            },
                            .local_type => {
                                // Type.staticMethod(). Resolve in caller's scope.
                                if (findFunctionByNameScoped(g, leaf_name, ctx.scope_start, ctx.scope_end, caller_parent_id)) |callee_id| {
                                    if (@intFromEnum(caller_id) != @intFromEnum(callee_id)) {
                                        try cf.addEdgeIfNew(g, caller_id, callee_id, .calls);
                                    }
                                }
                            },
                            .external => {
                                // param.method() or var.method() where var holds
                                // an external type. Do not create a local edge.
                            },
                        }
                    }
                } else if (chain_len == 1) {
                    // Single-segment field expression, treat as bare call.
                    if (findFunctionByNameScoped(g, chain[0], ctx.scope_start, ctx.scope_end, caller_parent_id)) |callee_id| {
                        if (@intFromEnum(caller_id) != @intFromEnum(callee_id)) {
                            try cf.addEdgeIfNew(g, caller_id, callee_id, .calls);
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
        const child_kind = child.kind();
        if (std.mem.eql(u8, child_kind, "function_declaration") or
            std.mem.eql(u8, child_kind, "test_declaration")) continue;
        try scanForCalls(g, source, caller_id, caller_parent_id, child, ctx, tracker, local_tracker, param_tracker, fn_decl_node, depth + 1);
    }
}

/// Classify the receiver (first child) of a field_expression to determine
/// whether it refers to `self`, a locally-defined type, or an external entity.
const ReceiverKind = enum { self_receiver, local_type, external };

fn classifyReceiver(g: *const Graph, source: []const u8, field_expr: ts.Node, scope_start: usize, scope_end: usize, caller_parent_id: ?NodeId, fn_decl_node: ts.Node) ReceiverKind {
    // The receiver is the first named child of the outermost field_expression.
    // For nested chains like `a.b.c()`, we want the leftmost identifier.
    const receiver_node = getLeftmostIdentifier(field_expr) orelse return .external;
    const receiver_name = ts_api.nodeText(source, receiver_node);

    // Check if receiver is 'self', i.e. the identifier "self".
    if (std.mem.eql(u8, receiver_name, "self")) {
        return .self_receiver;
    }

    // Check if receiver matches a locally-defined type or type-alias constant.
    if (findTypeByNameScoped(g, receiver_name, scope_start, scope_end, caller_parent_id) != null) {
        return .local_type;
    }

    // Check if receiver matches a parameter whose type is a locally-defined type.
    // For example: fn process(p: Point) void { p.method(); }. "p" is a parameter
    // of type "Point", and Point is a local type, so p.method() should resolve locally.
    if (findParamTypeName(source, fn_decl_node, receiver_name)) |type_name| {
        if (findTypeByNameScoped(g, type_name, scope_start, scope_end, caller_parent_id) != null) {
            return .local_type;
        }
    }

    return .external;
}

/// Given a function_declaration AST node and a parameter name, return
/// the type identifier if the parameter's type is a bare identifier
/// (e.g. `p: Point` returns "Point"). Returns null for pointer types,
/// optional types, or if the parameter is not found.
fn findParamTypeName(source: []const u8, fn_decl_node: ts.Node, param_name: []const u8) ?[]const u8 {
    // Find the "parameters" child of the function_declaration.
    var i: u32 = 0;
    while (i < fn_decl_node.childCount()) : (i += 1) {
        const child = fn_decl_node.child(i) orelse continue;
        if (!std.mem.eql(u8, child.kind(), "parameters")) continue;

        // Iterate over named children of "parameters", each is a "parameter" node.
        var j: u32 = 0;
        while (j < child.namedChildCount()) : (j += 1) {
            const param = child.namedChild(j) orelse continue;
            if (!std.mem.eql(u8, param.kind(), "parameter")) continue;

            // A parameter node has named children: identifier (name) and type node.
            // The first named child is the identifier, the second is the type.
            const name_node = param.namedChild(0) orelse continue;
            if (!std.mem.eql(u8, name_node.kind(), "identifier")) continue;
            const name = ts_api.nodeText(source, name_node);
            if (!std.mem.eql(u8, name, param_name)) continue;

            // Found the parameter. Get the type node (second named child).
            const type_node = param.namedChild(1) orelse return null;
            // Only handle bare identifier types (e.g. Point), not pointer (*Point)
            // or optional (?Point).
            if (std.mem.eql(u8, type_node.kind(), "identifier")) {
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
fn getLeftmostIdentifier(node: ts.Node) ?ts.Node {
    const kind = node.kind();
    if (std.mem.eql(u8, kind, "identifier")) {
        return node;
    }
    if (std.mem.eql(u8, kind, "field_expression")) {
        if (node.namedChild(0)) |child| {
            return getLeftmostIdentifier(child);
        }
    }
    return null;
}

fn scanSignatureForTypes(g: *Graph, source: []const u8, fn_id: NodeId, caller_parent_id: ?NodeId, fn_node: ts.Node, ctx: *const EdgeContext) !void {
    // Scan all children except the body block for type identifier references.
    var i: u32 = 0;
    while (i < fn_node.childCount()) : (i += 1) {
        const child = fn_node.child(i) orelse continue;
        if (std.mem.eql(u8, child.kind(), "block")) continue;
        try scanForTypeIdentifiersScoped(g, source, fn_id, child, caller_parent_id, ctx, 0);
    }
}

fn scanBodyForTypes(g: *Graph, source: []const u8, fn_id: NodeId, caller_parent_id: ?NodeId, fn_node: ts.Node, ctx: *const EdgeContext) !void {
    // Scan only the body block for type identifier references.
    var i: u32 = 0;
    while (i < fn_node.childCount()) : (i += 1) {
        const child = fn_node.child(i) orelse continue;
        if (std.mem.eql(u8, child.kind(), "block")) {
            try scanForTypeIdentifiersScoped(g, source, fn_id, child, caller_parent_id, ctx, 0);
            return;
        }
    }
}

/// Pre-scan a function/test body for local variable bindings where the
/// initializer is a struct literal (`const p = Point{ ... }`) or a
/// static method call on a known type (`const b = Builder.init(5)`).
fn prescanForLocalTypeBindings(
    source: []const u8,
    fn_node: ts.Node,
    tracker: *LocalTypeTracker,
) void {
    var i: u32 = 0;
    while (i < fn_node.childCount()) : (i += 1) {
        const child = fn_node.child(i) orelse continue;
        if (std.mem.eql(u8, child.kind(), "block")) {
            scanBlockForLocalTypeBindings(source, child, tracker);
            return;
        }
    }
}

fn scanBlockForLocalTypeBindings(
    source: []const u8,
    block: ts.Node,
    tracker: *LocalTypeTracker,
) void {
    var i: u32 = 0;
    while (i < block.childCount()) : (i += 1) {
        const child = block.child(i) orelse continue;
        const kind = child.kind();

        if (std.mem.eql(u8, kind, "variable_declaration")) {
            const var_name = ast.getIdentifierName(source, child) orelse continue;
            if (extractStructLiteralType(source, child)) |type_name| {
                tracker.addBinding(var_name, type_name);
            }
            continue;
        }

        if (std.mem.eql(u8, kind, "block") or
            std.mem.eql(u8, kind, "defer_statement") or
            std.mem.eql(u8, kind, "if_statement") or
            std.mem.eql(u8, kind, "expression_statement"))
        {
            scanBlockForLocalTypeBindings(source, child, tracker);
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
    while (pos < text.len and isWhitespace(text[pos])) : (pos += 1) {}

    // Skip optional 'try' keyword.
    if (pos + 3 <= text.len and std.mem.eql(u8, text[pos..][0..3], "try") and
        (pos + 3 >= text.len or !isIdentChar(text[pos + 3])))
    {
        pos += 3;
        while (pos < text.len and isWhitespace(text[pos])) : (pos += 1) {}
    }

    // Must start with an uppercase letter (PascalCase type name).
    if (pos >= text.len or text[pos] < 'A' or text[pos] > 'Z') return null;
    const id_start = pos;
    while (pos < text.len and isIdentChar(text[pos])) : (pos += 1) {}
    if (pos >= text.len) return null;

    // Must be followed by '{' (struct literal) or '.' (static method/field).
    if (text[pos] != '{' and text[pos] != '.') return null;

    // Return a slice of source (not of local text).
    return source[start + id_start .. start + pos];
}

fn isIdentChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_';
}

fn isWhitespace(c: u8) bool {
    return c == ' ' or c == '\t' or c == '\n' or c == '\r';
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
    tracker: *ParamTypeTracker,
) void {
    // Find the "parameters" child of the function_declaration.
    var i: u32 = 0;
    while (i < fn_decl_node.childCount()) : (i += 1) {
        const child = fn_decl_node.child(i) orelse continue;
        if (!std.mem.eql(u8, child.kind(), "parameters")) continue;

        // Iterate over each parameter.
        var j: u32 = 0;
        while (j < child.namedChildCount()) : (j += 1) {
            const param = child.namedChild(j) orelse continue;
            if (!std.mem.eql(u8, param.kind(), "parameter")) continue;

            // First named child is the identifier (name).
            const name_node = param.namedChild(0) orelse continue;
            if (!std.mem.eql(u8, name_node.kind(), "identifier")) continue;
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
            const chain_len = extractParamTypeChain(source, type_node, &chain);
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
) usize {
    const kind = type_node.kind();

    if (std.mem.eql(u8, kind, "field_expression")) {
        // Try standard chain extraction first (works for bare svc_mod.Service).
        const len = cf.collectFieldExprChain(source, type_node, chain);
        if (len >= 2) return len;

        // Standard extraction got only the right side — the left side is wrapped
        // in pointer_type or nullable_type (e.g., *svc_mod parses as pointer_type).
        // Unwrap the left child to recover the import identifier.
        const first_child = type_node.child(0) orelse return len;
        const unwrapped = unwrapTypeNode(first_child);
        if (std.mem.eql(u8, unwrapped.kind(), "identifier") and len == 1) {
            // Shift the existing segment ("Service") right, insert import name left.
            chain[1] = chain[0];
            chain[0] = ts_api.nodeText(source, unwrapped);
            return 2;
        }

        return len;
    }

    // Pointer/optional/error-union wrapping a field_expression — unwrap and recurse.
    if (std.mem.eql(u8, kind, "pointer_type") or
        std.mem.eql(u8, kind, "nullable_type") or
        std.mem.eql(u8, kind, "optional_type") or
        std.mem.eql(u8, kind, "error_union_type"))
    {
        const count = type_node.namedChildCount();
        if (count > 0) {
            if (type_node.namedChild(count - 1)) |inner| {
                return extractParamTypeChain(source, inner, chain);
            }
        }
        return 0;
    }

    return 0;
}

/// Unwrap pointer_type, optional_type, and error_union_type AST wrappers to
/// get the base type node. For bare identifiers, returns unchanged.
fn unwrapTypeNode(type_node: ts.Node) ts.Node {
    const kind = type_node.kind();
    // pointer_type: *T, *const T — pointee is the last named child.
    if (std.mem.eql(u8, kind, "pointer_type")) {
        const count = type_node.namedChildCount();
        if (count > 0) {
            if (type_node.namedChild(count - 1)) |inner| {
                return unwrapTypeNode(inner);
            }
        }
        return type_node;
    }
    // nullable_type / optional_type: ?T
    if (std.mem.eql(u8, kind, "nullable_type") or std.mem.eql(u8, kind, "optional_type")) {
        if (type_node.namedChild(0)) |inner| {
            return unwrapTypeNode(inner);
        }
        return type_node;
    }
    // error_union_type: E!T — result type is the last named child.
    if (std.mem.eql(u8, kind, "error_union_type")) {
        const count = type_node.namedChildCount();
        if (count > 0) {
            if (type_node.namedChild(count - 1)) |inner| {
                return unwrapTypeNode(inner);
            }
        }
        return type_node;
    }
    return type_node;
}

/// Pre-scan a function body for if-capture patterns where the condition is
/// a known parameter. Maps capture variable names to the same type origin.
/// Handles `if (svc) |s| { s.method(); }` where svc has an import-qualified type.
fn prescanForIfCaptures(
    source: []const u8,
    fn_decl_node: ts.Node,
    tracker: *ParamTypeTracker,
) void {
    var i: u32 = 0;
    while (i < fn_decl_node.childCount()) : (i += 1) {
        const child = fn_decl_node.child(i) orelse continue;
        if (std.mem.eql(u8, child.kind(), "block")) {
            scanBlockForIfCaptures(source, child, tracker);
            return;
        }
    }
}

fn scanBlockForIfCaptures(
    source: []const u8,
    block: ts.Node,
    tracker: *ParamTypeTracker,
) void {
    var i: u32 = 0;
    while (i < block.childCount()) : (i += 1) {
        const child = block.child(i) orelse continue;
        const kind = child.kind();

        if (std.mem.eql(u8, kind, "if_statement") or std.mem.eql(u8, kind, "if_expression")) {
            // Look for pattern: if (IDENT) |CAPTURE|
            var cond_ident: ?[]const u8 = null;
            var capture_name: ?[]const u8 = null;

            var j: u32 = 0;
            while (j < child.childCount()) : (j += 1) {
                const ic = child.child(j) orelse continue;
                const ic_kind = ic.kind();
                if (std.mem.eql(u8, ic_kind, "identifier") and cond_ident == null) {
                    cond_ident = ts_api.nodeText(source, ic);
                } else if (std.mem.eql(u8, ic_kind, "payload") or
                    std.mem.eql(u8, ic_kind, "payload_identifier"))
                {
                    // payload wraps the capture: |name|
                    if (ic.namedChild(0)) |inner| {
                        if (std.mem.eql(u8, inner.kind(), "identifier")) {
                            capture_name = ts_api.nodeText(source, inner);
                        }
                    }
                }
            }

            if (cond_ident != null and capture_name != null) {
                if (tracker.findOrigin(cond_ident.?)) |origin| {
                    tracker.addBinding(capture_name.?, origin.target_file, origin.type_chain);
                }
            }
        }

        // Recurse into nested blocks and control-flow statements.
        if (std.mem.eql(u8, kind, "block") or
            std.mem.eql(u8, kind, "if_statement") or
            std.mem.eql(u8, kind, "if_expression") or
            std.mem.eql(u8, kind, "for_statement") or
            std.mem.eql(u8, kind, "while_statement"))
        {
            scanBlockForIfCaptures(source, child, tracker);
        }
    }
}

fn scanForTypeIdentifiersScoped(g: *Graph, source: []const u8, fn_id: NodeId, ts_node: ts.Node, caller_parent_id: ?NodeId, ctx: *const EdgeContext, depth: u32) !void {
    // Graceful cap: stop descending, type refs in deeper subtrees are skipped.
    if (depth >= cf.max_ast_scan_depth) return;
    const kind_str = ts_node.kind();
    if (std.mem.eql(u8, kind_str, "identifier") or std.mem.eql(u8, kind_str, "property_identifier")) {
        const name = ts_api.nodeText(source, ts_node);
        const target_id = findTypeByNameScoped(g, name, ctx.scope_start, ctx.scope_end, caller_parent_id) orelse
            findTypeCrossFile(g, name, ctx) orelse {
            // No match locally or cross-file; skip.
            return;
        };
        // Skip edges to direct children of fn_id (prevents type_def → own inner declarations).
        const target_node = g.getNode(target_id);
        const is_own_child = if (target_node) |tn| tn.parent_id != null and tn.parent_id.? == fn_id else false;
        if (!is_own_child) {
            try cf.addEdgeIfNew(g, fn_id, target_id, .uses_type);
        }
    }

    // Recurse into all children, but stop at scope boundaries.
    var i: u32 = 0;
    while (i < ts_node.childCount()) : (i += 1) {
        const child = ts_node.child(i) orelse continue;
        const child_kind = child.kind();
        if (std.mem.eql(u8, child_kind, "function_declaration") or
            std.mem.eql(u8, child_kind, "test_declaration")) continue;
        try scanForTypeIdentifiersScoped(g, source, fn_id, child, caller_parent_id, ctx, depth + 1);
    }
}

/// Search imported files for a type_def or enum_def with the given PascalCase name.
/// Returns the target NodeId only if exactly one match is found (unambiguous).
fn findTypeCrossFile(g: *const Graph, name: []const u8, ctx: *const EdgeContext) ?NodeId {
    if (name.len == 0 or name[0] < 'A' or name[0] > 'Z') return null;
    var match: ?NodeId = null;
    var match_count: usize = 0;
    for (ctx.import_targets[0..ctx.import_count]) |target_file_id| {
        for (g.nodes.items, 0..) |n, idx| {
            if (n.parent_id == null or n.parent_id.? != target_file_id) continue;
            if (n.kind != .type_def and n.kind != .enum_def) continue;
            if (!std.mem.eql(u8, n.name, name)) continue;
            match = @enumFromInt(idx);
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
fn findTypeByNameScoped(g: *const Graph, name: []const u8, scope_start: usize, scope_end: usize, caller_parent_id: ?NodeId) ?NodeId {
    if (caller_parent_id) |cpid| {
        var current_scope: ?NodeId = cpid;
        var hops: usize = 0;
        while (current_scope != null and hops < 100) : (hops += 1) {
            const scope_id = current_scope.?;
            const scoped_nodes = g.nodes.items[scope_start..scope_end];
            for (scoped_nodes, scope_start..) |n, idx| {
                if (n.parent_id == null or n.parent_id.? != scope_id) continue;
                if (isTypeReference(n, name)) return @enumFromInt(idx);
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

fn findFunctionByNameScoped(g: *const Graph, name: []const u8, scope_start: usize, scope_end: usize, caller_parent_id: ?NodeId) ?NodeId {
    if (caller_parent_id) |cpid| {
        // Walk up the parent chain, searching at each scope level.
        var current_scope: ?NodeId = cpid;
        var hops: usize = 0;
        while (current_scope != null and hops < 100) : (hops += 1) {
            const scope_id = current_scope.?;
            const scoped_nodes = g.nodes.items[scope_start..scope_end];
            for (scoped_nodes, scope_start..) |n, i| {
                if (n.kind == .function and std.mem.eql(u8, n.name, name) and
                    n.parent_id != null and n.parent_id.? == scope_id)
                {
                    return @enumFromInt(i);
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

// =========================================================================
// Tests
// =========================================================================

const parse = @import("visitor.zig").parse;
const fixtures = @import("test-fixtures");
const Visibility = types.Visibility;

// --- Nominal edge tests ---

test "creates calls edge" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert: at least one edge with edge_type=calls exists
    //          (manhattan calls abs in the fixture)
    var found = false;
    for (g.edges.items) |e| {
        if (e.edge_type == .calls) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "creates calls edge for method call via self" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert: isWithinRadius calls manhattan via self.manhattan()
    var caller_id: ?NodeId = null;
    var callee_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "isWithinRadius")) {
            caller_id = @enumFromInt(idx);
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "manhattan")) {
            callee_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(caller_id != null);
    try std.testing.expect(callee_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == caller_id.? and e.target_id == callee_id.? and e.edge_type == .calls) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "creates uses_type edge" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert: at least one edge with edge_type=uses_type exists
    //          (defaultPoint uses/returns Point in the fixture)
    var found = false;
    for (g.edges.items) |e| {
        if (e.edge_type == .uses_type) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "all edges have source tree_sitter" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert: every edge has source == .tree_sitter
    try std.testing.expect(g.edgeCount() > 0);
    for (g.edges.items) |e| {
        try std.testing.expectEqual(EdgeSource.tree_sitter, e.source);
    }
}

test "generic type inner method call edge is detected" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g);

    // Assert: isEmpty calls count (via self.count())
    var caller_id: ?NodeId = null;
    var callee_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "isEmpty")) {
            caller_id = @enumFromInt(idx);
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "count")) {
            callee_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(caller_id != null);
    try std.testing.expect(callee_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == caller_id.? and e.target_id == callee_id.? and e.edge_type == .calls) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "enum method has uses_type edge to parent enum" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert: isWarm has a uses_type edge pointing to Color
    //          (its signature contains `self: Color`)
    var color_id: ?NodeId = null;
    var iswarm_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .enum_def and std.mem.eql(u8, n.name, "Color")) {
            color_id = @enumFromInt(idx);
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "isWarm")) {
            iswarm_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(color_id != null);
    try std.testing.expect(iswarm_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == iswarm_id.? and e.target_id == color_id.? and e.edge_type == .uses_type) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

// --- File-struct edge tests ---

test "file-struct call edge between methods" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.file_struct, &g);

    // Assert: isValid has a calls edge to validate (via self.validate())
    var isValid_id: ?NodeId = null;
    var validate_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "isValid")) {
            isValid_id = @enumFromInt(idx);
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "validate")) {
            validate_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(isValid_id != null);
    try std.testing.expect(validate_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == isValid_id.? and e.target_id == validate_id.? and e.edge_type == .calls) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

// --- Test edge scanning ---

test "test block creates calls edge to local function" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\fn helper() i32 { return 42; }
        \\test "uses helper" { _ = helper(); }
    ;

    // Act
    try parse(source, &g);

    // Assert: the test_def node should have a calls edge to helper
    var test_id: ?NodeId = null;
    var helper_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .test_def) test_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "helper")) helper_id = @enumFromInt(idx);
    }
    try std.testing.expect(test_id != null);
    try std.testing.expect(helper_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == test_id.? and e.target_id == helper_id.? and e.edge_type == .calls) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "test block creates calls edge to method via dot syntax" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const Foo = struct {
        \\    pub fn bar() void {}
        \\};
        \\test "calls bar" { Foo.bar(); }
    ;

    // Act
    try parse(source, &g);

    // Assert: the test_def node should have a calls edge to bar
    var test_id: ?NodeId = null;
    var bar_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .test_def) test_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "bar")) bar_id = @enumFromInt(idx);
    }
    try std.testing.expect(test_id != null);
    try std.testing.expect(bar_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == test_id.? and e.target_id == bar_id.? and e.edge_type == .calls) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "test block with no local calls has no edges" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\test "standalone" {
        \\    const x: i32 = 42;
        \\    _ = x;
        \\}
    ;

    // Act
    try parse(source, &g);

    // Assert: the test_def node has zero outgoing edges
    var test_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .test_def) {
            test_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(test_id != null);

    var outgoing: usize = 0;
    for (g.edges.items) |e| {
        if (e.source_id == test_id.?) outgoing += 1;
    }
    try std.testing.expectEqual(@as(usize, 0), outgoing);
}

// --- Scope boundary: nested functions must not leak edges to parent ---

test "test block does not leak calls from nested function" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\fn helper() void {}
        \\test "outer" {
        \\    const S = struct {
        \\        fn inner() void { helper(); }
        \\    };
        \\    S.inner();
        \\}
    ;

    // Act
    try parse(source, &g);

    // Assert: the test_def "outer" must not have a calls edge to "helper".
    // Only the nested function inner() calls helper(); the test body does not.
    var test_id: ?NodeId = null;
    var helper_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .test_def and std.mem.eql(u8, n.name, "outer")) test_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "helper")) helper_id = @enumFromInt(idx);
    }
    try std.testing.expect(test_id != null);
    try std.testing.expect(helper_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == test_id.? and e.target_id == helper_id.? and e.edge_type == .calls) {
            found = true;
            break;
        }
    }
    try std.testing.expect(!found);
}

test "function does not leak calls from nested function" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\fn target() void {}
        \\fn outer() void {
        \\    const S = struct {
        \\        fn nested() void { target(); }
        \\    };
        \\    S.nested();
        \\}
    ;

    // Act
    try parse(source, &g);

    // Assert: function "outer" must not have a calls edge to "target".
    // Only the nested function nested() calls target(); outer does not.
    var outer_id: ?NodeId = null;
    var target_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "outer")) outer_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "target")) target_id = @enumFromInt(idx);
    }
    try std.testing.expect(outer_id != null);
    try std.testing.expect(target_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == outer_id.? and e.target_id == target_id.? and e.edge_type == .calls) {
            found = true;
            break;
        }
    }
    try std.testing.expect(!found);
}

test "nested function gets its own calls edge not parent's" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\fn alpha() void {}
        \\fn beta() void {}
        \\fn parent() void {
        \\    _ = alpha();
        \\    const S = struct {
        \\        fn child() void { beta(); }
        \\    };
        \\    S.child();
        \\}
    ;

    // Act
    try parse(source, &g);

    // Assert: "parent" has a calls edge to "alpha" (direct call)
    //          "parent" does not have a calls edge to "beta" (nested call)
    var parent_id: ?NodeId = null;
    var alpha_id: ?NodeId = null;
    var beta_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "parent")) parent_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "alpha")) alpha_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "beta")) beta_id = @enumFromInt(idx);
    }
    try std.testing.expect(parent_id != null);
    try std.testing.expect(alpha_id != null);
    try std.testing.expect(beta_id != null);

    var calls_alpha = false;
    var calls_beta = false;
    for (g.edges.items) |e| {
        if (e.source_id == parent_id.?) {
            if (e.target_id == alpha_id.? and e.edge_type == .calls) calls_alpha = true;
            if (e.target_id == beta_id.? and e.edge_type == .calls) calls_beta = true;
        }
    }
    try std.testing.expect(calls_alpha);
    try std.testing.expect(!calls_beta);
}

test "function does not leak uses_type from nested function" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const MyType = struct {};
        \\fn outer() void {
        \\    const S = struct {
        \\        fn nested(x: MyType) void { _ = x; }
        \\    };
        \\    _ = S.nested;
        \\}
    ;

    // Act
    try parse(source, &g);

    // Assert: function "outer" must not have a uses_type edge to "MyType".
    // The nested function's signature references MyType, but that should not
    // leak to the enclosing function.
    var outer_id: ?NodeId = null;
    var mytype_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "outer")) outer_id = @enumFromInt(idx);
        if (std.mem.eql(u8, n.name, "MyType")) mytype_id = @enumFromInt(idx);
    }
    try std.testing.expect(outer_id != null);
    try std.testing.expect(mytype_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == outer_id.? and e.target_id == mytype_id.? and e.edge_type == .uses_type) {
            found = true;
            break;
        }
    }
    try std.testing.expect(!found);
}

// --- Decl-reference test edge attribution ---

test "decl-reference test edges are attributed to correct node" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\fn alpha() i32 { return 1; }
        \\fn beta() i32 { return 2; }
        \\test alpha { _ = alpha(); }
        \\test beta { _ = beta(); }
    ;

    // Act
    try parse(source, &g);

    // Assert: locate node ids
    var test_alpha_id: ?NodeId = null;
    var test_beta_id: ?NodeId = null;
    var fn_alpha_id: ?NodeId = null;
    var fn_beta_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .test_def and std.mem.eql(u8, n.name, "alpha")) test_alpha_id = @enumFromInt(idx);
        if (n.kind == .test_def and std.mem.eql(u8, n.name, "beta")) test_beta_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "alpha")) fn_alpha_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "beta")) fn_beta_id = @enumFromInt(idx);
    }
    try std.testing.expect(test_alpha_id != null);
    try std.testing.expect(test_beta_id != null);
    try std.testing.expect(fn_alpha_id != null);
    try std.testing.expect(fn_beta_id != null);

    // Assert: test "alpha" calls function "alpha"
    var alpha_calls_alpha = false;
    var alpha_calls_beta = false;
    var beta_calls_beta = false;
    var beta_calls_alpha = false;
    for (g.edges.items) |e| {
        if (e.edge_type == .calls) {
            if (e.source_id == test_alpha_id.? and e.target_id == fn_alpha_id.?) alpha_calls_alpha = true;
            if (e.source_id == test_alpha_id.? and e.target_id == fn_beta_id.?) alpha_calls_beta = true;
            if (e.source_id == test_beta_id.? and e.target_id == fn_beta_id.?) beta_calls_beta = true;
            if (e.source_id == test_beta_id.? and e.target_id == fn_alpha_id.?) beta_calls_alpha = true;
        }
    }
    try std.testing.expect(alpha_calls_alpha);
    try std.testing.expect(!alpha_calls_beta);
    try std.testing.expect(beta_calls_beta);
    try std.testing.expect(!beta_calls_alpha);
}

// =========================================================================
// Type alias uses_type edge tests
// =========================================================================

test "uses_type edge created for type alias in parameter" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const Foo = struct { val: i32 };
        \\const Bar = Foo;
        \\fn useBar(b: Bar) void { _ = b; }
    ;

    // Act
    try parse(source, &g);

    // Assert: useBar has a uses_type edge to Bar.
    var useBar_id: ?NodeId = null;
    var bar_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "useBar")) {
            useBar_id = @enumFromInt(idx);
        }
        if (std.mem.eql(u8, n.name, "Bar")) {
            bar_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(useBar_id != null);
    try std.testing.expect(bar_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == useBar_id.? and e.target_id == bar_id.? and e.edge_type == .uses_type) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "uses_type edge created for type alias in return type" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const MyError = error{ Oops, Bad };
        \\const Err = MyError;
        \\fn doStuff() Err!void {}
    ;

    // Act
    try parse(source, &g);

    // Assert: doStuff has a uses_type edge to Err.
    var doStuff_id: ?NodeId = null;
    var err_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "doStuff")) {
            doStuff_id = @enumFromInt(idx);
        }
        if (std.mem.eql(u8, n.name, "Err")) {
            err_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(doStuff_id != null);
    try std.testing.expect(err_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == doStuff_id.? and e.target_id == err_id.? and e.edge_type == .uses_type) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "uses_type edge not created for non-type constant" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const Limit = struct {};
        \\const max = 100;
        \\fn process(l: Limit) void { _ = l; }
    ;

    // Act
    try parse(source, &g);

    // Assert: uses_type edge exists to Limit (type_def) but not to max
    var process_id: ?NodeId = null;
    var limit_id: ?NodeId = null;
    var max_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "process")) {
            process_id = @enumFromInt(idx);
        }
        if (std.mem.eql(u8, n.name, "Limit")) {
            limit_id = @enumFromInt(idx);
        }
        if (std.mem.eql(u8, n.name, "max")) {
            max_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(process_id != null);
    try std.testing.expect(limit_id != null);
    try std.testing.expect(max_id != null);

    var found_limit = false;
    var found_max = false;
    for (g.edges.items) |e| {
        if (e.source_id == process_id.? and e.edge_type == .uses_type) {
            if (e.target_id == limit_id.?) found_limit = true;
            if (e.target_id == max_id.?) found_max = true;
        }
    }
    try std.testing.expect(found_limit);
    try std.testing.expect(!found_max);
}

test "uses_type edge for aliased type works alongside direct type" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const Direct = struct {};
        \\const Other = struct {};
        \\const Alias = Other;
        \\fn both(d: Direct, a: Alias) void { _ = d; _ = a; }
    ;

    // Act
    try parse(source, &g);

    // Assert: both has uses_type edges to Direct and Alias
    var both_id: ?NodeId = null;
    var direct_id: ?NodeId = null;
    var alias_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "both")) {
            both_id = @enumFromInt(idx);
        }
        if (std.mem.eql(u8, n.name, "Direct")) {
            direct_id = @enumFromInt(idx);
        }
        if (std.mem.eql(u8, n.name, "Alias")) {
            alias_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(both_id != null);
    try std.testing.expect(direct_id != null);
    try std.testing.expect(alias_id != null);

    var found_direct = false;
    var found_alias = false;
    for (g.edges.items) |e| {
        if (e.source_id == both_id.? and e.edge_type == .uses_type) {
            if (e.target_id == direct_id.?) found_direct = true;
            if (e.target_id == alias_id.?) found_alias = true;
        }
    }
    try std.testing.expect(found_direct);
    try std.testing.expect(found_alias);
}

// =========================================================================
// Body type reference tests
// =========================================================================

test "uses_type edge for struct literal construction in body" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const Config = struct { max: i32 = 0 };
        \\fn makeDefault() void {
        \\    const c = Config{ .max = 42 };
        \\    _ = c;
        \\}
    ;

    // Act
    try parse(source, &g);

    // Assert: makeDefault has a uses_type edge to Config.
    var makeDefault_id: ?NodeId = null;
    var config_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "makeDefault")) {
            makeDefault_id = @enumFromInt(idx);
        }
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Config")) {
            config_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(makeDefault_id != null);
    try std.testing.expect(config_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == makeDefault_id.? and e.target_id == config_id.? and e.edge_type == .uses_type) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "uses_type edge for static method call in body" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const Builder = struct {
        \\    val: i32,
        \\    pub fn init(v: i32) Builder {
        \\        return Builder{ .val = v };
        \\    }
        \\};
        \\fn create() void {
        \\    const b = Builder.init(5);
        \\    _ = b;
        \\}
    ;

    // Act
    try parse(source, &g);

    // Assert: create has a uses_type edge to Builder.
    var create_id: ?NodeId = null;
    var builder_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "create")) {
            create_id = @enumFromInt(idx);
        }
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Builder")) {
            builder_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(create_id != null);
    try std.testing.expect(builder_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == create_id.? and e.target_id == builder_id.? and e.edge_type == .uses_type) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "no duplicate uses_type edge when type in both signature and body" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const Item = struct { id: i32 = 0 };
        \\fn process(item: Item) Item {
        \\    return Item{ .id = item.id + 1 };
        \\}
    ;

    // Act
    try parse(source, &g);

    // Assert: process has exactly one uses_type edge to Item.
    var process_id: ?NodeId = null;
    var item_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "process")) {
            process_id = @enumFromInt(idx);
        }
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Item")) {
            item_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(process_id != null);
    try std.testing.expect(item_id != null);

    var count: usize = 0;
    for (g.edges.items) |e| {
        if (e.source_id == process_id.? and e.target_id == item_id.? and e.edge_type == .uses_type) {
            count += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 1), count);
}

test "uses_type edge for type passed as comptime argument" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const Payload = struct { data: i32 = 0 };
        \\fn serialize(comptime T: type, val: T) void { _ = val; }
        \\fn doWork() void {
        \\    const p = Payload{ .data = 1 };
        \\    serialize(Payload, p);
        \\}
    ;

    // Act
    try parse(source, &g);

    // Assert: doWork has a uses_type edge to Payload.
    var doWork_id: ?NodeId = null;
    var payload_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "doWork")) {
            doWork_id = @enumFromInt(idx);
        }
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Payload")) {
            payload_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(doWork_id != null);
    try std.testing.expect(payload_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == doWork_id.? and e.target_id == payload_id.? and e.edge_type == .uses_type) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "uses_type edge for type in generic container instantiation" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const Element = struct { val: i32 = 0 };
        \\fn Container(comptime T: type) type {
        \\    return struct { item: T };
        \\}
        \\fn build() void {
        \\    _ = Container(Element);
        \\}
    ;

    // Act
    try parse(source, &g);

    // Assert: build has a uses_type edge to Element.
    var build_id: ?NodeId = null;
    var element_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "build")) {
            build_id = @enumFromInt(idx);
        }
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Element")) {
            element_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(build_id != null);
    try std.testing.expect(element_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == build_id.? and e.target_id == element_id.? and e.edge_type == .uses_type) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "no uses_type edge for non-type identifier argument" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const Config = struct {};
        \\const max = 100;
        \\fn helper(n: i32) void { _ = n; }
        \\fn run() void {
        \\    helper(max);
        \\}
    ;

    // Act
    try parse(source, &g);

    // Assert: run has no uses_type edge to max.
    var run_id: ?NodeId = null;
    var helper_id: ?NodeId = null;
    var max_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "run")) {
            run_id = @enumFromInt(idx);
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "helper")) {
            helper_id = @enumFromInt(idx);
        }
        if (n.kind == .constant and std.mem.eql(u8, n.name, "max")) {
            max_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(run_id != null);
    try std.testing.expect(helper_id != null);
    try std.testing.expect(max_id != null);

    var found_max_edge = false;
    for (g.edges.items) |e| {
        if (e.source_id == run_id.? and e.target_id == max_id.? and e.edge_type == .uses_type) {
            found_max_edge = true;
            break;
        }
    }
    try std.testing.expect(!found_max_edge);

    var found_calls = false;
    for (g.edges.items) |e| {
        if (e.source_id == run_id.? and e.target_id == helper_id.? and e.edge_type == .calls) {
            found_calls = true;
            break;
        }
    }
    try std.testing.expect(found_calls);
}

// =========================================================================
// Test blocks: body type scanning
// =========================================================================

test "test block has uses_type edge for struct literal in body" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert: test "point manhattan distance" has a uses_type edge to Point
    var test_id: ?NodeId = null;
    var point_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .test_def and std.mem.eql(u8, n.name, "point manhattan distance")) {
            test_id = @enumFromInt(idx);
        }
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Point")) {
            point_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(test_id != null);
    try std.testing.expect(point_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == test_id.? and e.target_id == point_id.? and e.edge_type == .uses_type) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "test block in file-struct has no uses_type to filtered Self" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.file_struct, &g);

    // Assert: Self constant is filtered; no Self node exists
    for (g.nodes.items) |n| {
        if (std.mem.eql(u8, n.name, "Self")) {
            try std.testing.expect(false); // Self should not exist
        }
    }

    // Assert: test "basic" still exists
    var test_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .test_def and std.mem.eql(u8, n.name, "basic")) {
            test_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(test_id != null);
}

// =========================================================================
// Spurious uses_type from type_def to its own inner declarations
// =========================================================================

test "generic type has no Self constant after filtering" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g);

    // Assert: no Self constants exist (all @This() aliases filtered)
    for (g.nodes.items) |n| {
        if (std.mem.eql(u8, n.name, "Self")) {
            try std.testing.expect(false); // Self should not exist
        }
    }
}

test "generic type_def does not have uses_type edge to own nested type" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g);

    var container_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Container")) {
            container_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(container_id != null);

    var entry_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Entry") and
            n.parent_id != null and n.parent_id.? == container_id.?)
        {
            entry_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(entry_id != null);

    var has_spurious_edge = false;
    for (g.edges.items) |e| {
        if (e.source_id == container_id.? and e.target_id == entry_id.? and e.edge_type == .uses_type) {
            has_spurious_edge = true;
            break;
        }
    }
    try std.testing.expect(!has_spurious_edge);
}

test "inner method of generic type has no uses_type edge to Self after filtering" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g);

    // Assert: Self is filtered, so no uses_type edges target a Self node
    for (g.nodes.items) |n| {
        if (std.mem.eql(u8, n.name, "Self")) {
            try std.testing.expect(false); // Self should not exist
        }
    }

    // Assert: Container.init still exists
    var container_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Container")) {
            container_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(container_id != null);

    var init_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "init") and
            n.parent_id != null and n.parent_id.? == container_id.?)
        {
            init_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(init_id != null);
}

// =========================================================================
// Local-type parameter tests
// =========================================================================

test "local-type param creates calls edge to method" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.local_type_param, &g);

    // Assert: processPoint should have a calls edge to manhattan
    var caller_id: ?NodeId = null;
    var callee_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "processPoint")) {
            caller_id = @enumFromInt(idx);
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "manhattan")) {
            callee_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(caller_id != null);
    try std.testing.expect(callee_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == caller_id.? and e.target_id == callee_id.? and e.edge_type == .calls) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "multi-param function creates calls edge via local-type param" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.local_type_param, &g);

    // Assert: multiParam should have a calls edge to manhattan
    var caller_id: ?NodeId = null;
    var callee_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "multiParam")) {
            caller_id = @enumFromInt(idx);
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "manhattan")) {
            callee_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(caller_id != null);
    try std.testing.expect(callee_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == caller_id.? and e.target_id == callee_id.? and e.edge_type == .calls) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "pointer-type param does not create local calls edge" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.local_type_param, &g);

    // Assert: pointerParam should NOT have a calls edge to scale
    var caller_id: ?NodeId = null;
    var callee_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "pointerParam")) {
            caller_id = @enumFromInt(idx);
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "scale")) {
            callee_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(caller_id != null);
    try std.testing.expect(callee_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == caller_id.? and e.target_id == callee_id.? and e.edge_type == .calls) {
            found = true;
            break;
        }
    }
    try std.testing.expect(!found);
}

test "external-type param does not create local calls edge" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.local_type_param, &g);

    // Assert: externalParam has no calls edges at all (allocator is external)
    var caller_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "externalParam")) {
            caller_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(caller_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == caller_id.? and e.edge_type == .calls) {
            found = true;
            break;
        }
    }
    try std.testing.expect(!found);
}
