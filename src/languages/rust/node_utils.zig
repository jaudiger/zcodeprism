const node_mod = @import("../../core/node.zig");
const types = @import("../../core/types.zig");
const rust_meta = @import("../../core/lang_meta.zig");

const Node = node_mod.Node;

/// Check whether a node is a type definition or alias (struct, enum, union, type alias),
/// excluding impl_blocks, traits, and associated types.
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
