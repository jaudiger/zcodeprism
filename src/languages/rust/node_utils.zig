const node_mod = @import("../../core/node.zig");
const types = @import("../../core/types.zig");
const rust_meta = @import("meta.zig");

const Node = node_mod.Node;

/// Check whether a node is a type definition or alias (struct, enum, union, type alias),
/// excluding impl_blocks, traits, and associated types.
pub fn isTypeOrAliasNode(n: Node) bool {
    if (n.kind == .enum_def or n.kind == .union_def) return true;
    if (n.kind != .type_def) return false;
    if (rust_meta.metaOf(&n)) |m| {
        const sk = m.sub_kind;
        if (sk == .impl_block or sk == .trait_ or sk == .associated_type) return false;
    }
    return true;
}

/// Check whether a node is a trait definition.
pub fn isTraitNode(n: Node) bool {
    if (n.kind != .type_def) return false;
    const m = rust_meta.metaOf(&n) orelse return false;
    return m.sub_kind == .trait_;
}
