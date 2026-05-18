const std = @import("std");
const ts = @import("tree-sitter");
const graph_mod = @import("../../core/graph.zig");
const types_mod = @import("../../core/types.zig");
const EdgeType = types_mod.EdgeType;
const NodeKind = types_mod.NodeKind;
const graph_index_mod = @import("../../core/graph_index.zig");
const phantom_mod = @import("../../core/phantom.zig");
const logging = @import("../../logging.zig");
const shared_types = @import("types.zig");
const shared_resolve = @import("resolve.zig");
const shared_lookup = @import("lookup.zig");
const type_env_mod = @import("type_env.zig");
const worklist_mod = @import("../../lsp/worklist.zig");

const Graph = graph_mod.Graph;
const NodeId = types_mod.NodeId;
const GraphIndex = graph_index_mod.GraphIndex;
const PhantomManager = phantom_mod.PhantomManager;
const Logger = logging.Logger;
const EdgeContext = shared_types.EdgeContext;
const SymbolOrigin = shared_types.SymbolOrigin;
const TypeEnv = type_env_mod.TypeEnv;
const LspWorklist = worklist_mod.LspWorklist;
const QueryKind = worklist_mod.QueryKind;

/// Common per-function/test scan state shared between all language edge builders.
/// Each language composes this via a `base` field and adds its own language-specific
/// extras (field_types for Zig, self_type_id for Rust).
pub const BaseScanContext = struct {
    graph: *Graph,
    source: []const u8,
    caller_id: NodeId,
    caller_parent_id: ?NodeId,
    fn_node: ts.Node,
    edge_ctx: *const EdgeContext,
    type_env: *const TypeEnv,
    graph_index: *const GraphIndex,
    phantom_mgr: *const PhantomManager,
    wl: *LspWorklist,
    io: std.Io,
    log: Logger,
    resolve: shared_resolve.ResolveContext,

    /// Safe parent lookup. Returns null rather than crashing when the caller
    /// node is missing (which can happen if caller_id is stale).
    pub fn parentOf(graph: *const Graph, caller_id: NodeId) ?NodeId {
        const node = graph.getNode(caller_id) orelse return null;
        return node.parent_id;
    }

    pub fn callerFilePath(self: *const BaseScanContext) []const u8 {
        const node = self.graph.getNode(self.caller_id) orelse return "";
        return node.file_path orelse "";
    }

    pub fn appendWorklist(
        self: *const BaseScanContext,
        allocator: std.mem.Allocator,
        pos: ts.Point,
        query_kind: QueryKind,
        hint_name: []const u8,
    ) !void {
        try self.wl.append(allocator, .{
            .source_node_id = self.caller_id,
            .file_path = self.callerFilePath(),
            .line = pos.row,
            .col = pos.column,
            .query_kind = query_kind,
            .hint_name = hint_name,
        });
    }

    pub fn addResolvedEdges(
        self: *const BaseScanContext,
        allocator: std.mem.Allocator,
        target_file_id: NodeId,
        chain: []const []const u8,
        terminal_edge: ?EdgeType,
    ) !bool {
        return shared_resolve.addResolvedEdges(
            allocator,
            self.graph,
            self.caller_id,
            target_file_id,
            chain,
            terminal_edge,
            &self.resolve,
        );
    }

    pub fn resolveOriginCall(
        self: *const BaseScanContext,
        allocator: std.mem.Allocator,
        origin: SymbolOrigin,
        call_chain: []const []const u8,
        terminal_edge: ?EdgeType,
    ) !bool {
        return shared_resolve.resolveOriginCall(
            allocator,
            self.graph,
            self.caller_id,
            origin,
            call_chain,
            terminal_edge,
            &self.resolve,
        );
    }

    /// True when an edge from `caller_id` to `target_id` is neither a
    /// self-reference nor a direct parent->child relation.
    pub fn shouldEmitEdgeTo(self: *const BaseScanContext, target_id: NodeId) bool {
        return edgeCarriesNewInformation(self.graph, self.caller_id, target_id);
    }

    /// Add an edge from `caller_id` to `target_id` of the given type, but only
    /// when `shouldEmitEdgeTo` accepts the pair. Returns whether the write was
    /// attempted (regardless of whether the edge was new in the graph).
    pub fn emitEdge(
        self: *const BaseScanContext,
        allocator: std.mem.Allocator,
        target_id: NodeId,
        edge_type: EdgeType,
    ) !bool {
        if (!self.shouldEmitEdgeTo(target_id)) return false;
        _ = try self.graph.addEdgeIfNew(allocator, .{
            .source_id = self.caller_id,
            .target_id = target_id,
            .edge_type = edge_type,
        });
        return true;
    }

    /// Resolve `name` to a function in scope (or via cross-file imports) and
    /// emit a `.uses_value` edge from the caller. `extra_kinds` is forwarded
    /// to scoped function lookup. Returns true if any edge was emitted.
    pub fn emitValueUse(
        self: *const BaseScanContext,
        allocator: std.mem.Allocator,
        name: []const u8,
        extra_kinds: []const NodeKind,
    ) !bool {
        if (shared_lookup.findFunctionByNameScoped(
            self.graph,
            name,
            self.edge_ctx.scope_start,
            self.edge_ctx.scope_end,
            self.caller_parent_id,
            &self.graph_index.scope,
            extra_kinds,
        )) |fn_id| {
            const fn_node = self.graph.getNode(fn_id) orelse return false;
            if (fn_node.kind != .function) return false;
            return try self.emitEdge(allocator, fn_id, .uses_value);
        }
        if (self.edge_ctx.findImportOrigin(name)) |origin| {
            if (origin.chain.len == 0) return false;
            return try self.resolveOriginCall(allocator, origin, &.{}, .uses_value);
        }
        return false;
    }
};

/// True when `node`'s parent has one of the given kind ids.
pub fn parentKindIsAny(node: ts.Node, kinds: []const u16) bool {
    const parent = node.parent() orelse return false;
    const pk = parent.kindId();
    for (kinds) |kid| if (pk == kid) return true;
    return false;
}

/// True when `parent.childByFieldName(field_name)` resolves to `child`.
/// When the grammar does not expose the named field, falls back to a
/// positional check: `child` appears after the first child whose kind name
/// ends with `=` (covering `=`, `+=`, `-=`, etc.).
pub fn matchesParentField(parent: ts.Node, child: ts.Node, field_name: []const u8) bool {
    if (parent.childByFieldName(field_name)) |value_child| {
        return value_child.startByte() == child.startByte() and value_child.endByte() == child.endByte();
    }
    var past_assign = false;
    var i: u32 = 0;
    while (i < parent.childCount()) : (i += 1) {
        const c = parent.child(i) orelse continue;
        if (!past_assign) {
            if (std.mem.endsWith(u8, c.kind(), "=")) past_assign = true;
            continue;
        }
        if (c.startByte() == child.startByte() and c.endByte() == child.endByte()) return true;
    }
    return false;
}

/// True when `source_id` and `target_id` are distinct nodes and `target_id`
/// is not a direct child of `source_id`.
pub fn edgeCarriesNewInformation(graph: *const graph_mod.Graph, source_id: NodeId, target_id: NodeId) bool {
    if (target_id == source_id) return false;
    const target_node = graph.getNode(target_id) orelse return false;
    if (target_node.parent_id) |pid| if (pid == source_id) return false;
    return true;
}

/// Generic post-order body walker with a depth cap.
///
/// The language's ScanContext type must provide two methods:
///   pub fn isBoundary(self: *const Ctx, node: ts.Node) bool
///   pub fn dispatch(self: *const Ctx, alloc: std.mem.Allocator, node: ts.Node, depth: u32) !void
///
/// walkBody checks the depth cap, returns early at boundary nodes (stopping
/// both dispatch and child recursion), then delegates per-node edge emission
/// to dispatch before recursing into children. The dispatch is comptime-resolved
/// with no vtable overhead.
pub fn walkBody(
    comptime Ctx: type,
    allocator: std.mem.Allocator,
    sctx: *const Ctx,
    node: ts.Node,
    depth: u32,
) !void {
    if (depth >= shared_types.max_ast_scan_depth) {
        sctx.base.log.trace("scan depth cap reached", &.{});
        return;
    }
    if (sctx.isBoundary(node)) return;
    try sctx.dispatch(allocator, node, depth);
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        try walkBody(Ctx, allocator, sctx, child, depth + 1);
    }
}
