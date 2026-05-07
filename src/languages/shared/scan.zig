const std = @import("std");
const ts = @import("tree-sitter");
const graph_mod = @import("../../core/graph.zig");
const types_mod = @import("../../core/types.zig");
const graph_index_mod = @import("../../core/graph_index.zig");
const phantom_mod = @import("../../core/phantom.zig");
const logging = @import("../../logging.zig");
const shared_types = @import("types.zig");
const shared_resolve = @import("resolve.zig");
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
        is_call: bool,
    ) !bool {
        return shared_resolve.addResolvedEdges(
            allocator,
            self.graph,
            self.caller_id,
            target_file_id,
            chain,
            is_call,
            &self.resolve,
        );
    }

    pub fn resolveOriginCall(
        self: *const BaseScanContext,
        allocator: std.mem.Allocator,
        origin: SymbolOrigin,
        call_chain: []const []const u8,
        is_call: bool,
    ) !bool {
        return shared_resolve.resolveOriginCall(
            allocator,
            self.graph,
            self.caller_id,
            origin,
            call_chain,
            is_call,
            &self.resolve,
        );
    }
};

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
