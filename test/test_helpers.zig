const std = @import("std");
const zcodeprism = @import("zcodeprism");

const Graph = zcodeprism.graph.Graph;
const Node = zcodeprism.node.Node;
const NodeId = zcodeprism.types.NodeId;
const NodeKind = zcodeprism.types.NodeKind;
const EdgeType = zcodeprism.types.EdgeType;

/// Count nodes of a given kind in the graph.
pub fn countNodesByKind(g: *const Graph, kind: NodeKind) usize {
    var count: usize = 0;
    for (g.nodes.items) |n| {
        if (n.kind == kind) count += 1;
    }
    return count;
}

/// Find the first node matching a name and kind.
pub fn findNode(g: *const Graph, name: []const u8, kind: NodeKind) ?*const Node {
    for (g.nodes.items) |*n| {
        if (n.kind == kind and std.mem.eql(u8, n.name, name)) return n;
    }
    return null;
}

/// Check if an edge exists between two node ids with the given type.
pub fn hasEdge(g: *const Graph, source: NodeId, target: NodeId, edge_type: EdgeType) bool {
    for (g.edges.items) |e| {
        if (e.source_id == source and e.target_id == target and e.edge_type == edge_type) return true;
    }
    return false;
}

/// Check if any edge with the given source has edge_source == .phantom.
pub fn hasPhantomEdge(g: *const Graph, source: NodeId) bool {
    for (g.edges.items) |e| {
        if (e.source_id == source and e.source == .phantom) return true;
    }
    return false;
}

/// Find a node matching name and kind that is a descendant of a given ancestor node.
/// Used to distinguish same-named nodes across different files.
pub fn findNodeInFile(g: *const Graph, name: []const u8, kind: NodeKind, ancestor_id: NodeId) ?NodeId {
    for (g.nodes.items, 0..) |n, i| {
        const nid: NodeId = @enumFromInt(i);
        if (n.kind == kind and std.mem.eql(u8, n.name, name) and isDescendantOf(g, nid, ancestor_id)) return nid;
    }
    return null;
}

/// Walk the parent chain to check if node_id is a descendant of ancestor_id.
fn isDescendantOf(g: *const Graph, node_id: NodeId, ancestor_id: NodeId) bool {
    var current = node_id;
    var hops: usize = 0;
    while (hops < 100) : (hops += 1) {
        const n = g.getNode(current) orelse return false;
        const pid = n.parent_id orelse return false;
        if (pid == ancestor_id) return true;
        current = pid;
    }
    return false;
}
