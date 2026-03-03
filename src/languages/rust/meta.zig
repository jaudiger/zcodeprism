const std = @import("std");

/// Sub-kind discriminator for Rust-specific node classification.
/// Refines the generic NodeKind (type_def, function, constant) into
/// Rust-specific concepts (trait, impl block, macro_rules, etc.).
pub const RustSubKind = enum(u4) {
    /// Regular struct, function, enum, union (no special sub-kind).
    none = 0,
    /// Trait definition (NodeKind = type_def).
    trait_ = 1,
    /// Impl block (NodeKind = type_def).
    impl_block = 2,
    /// macro_rules! definition (NodeKind = function).
    macro_rules = 3,
    /// Type alias (NodeKind = type_def).
    type_alias = 4,
    /// Static variable (NodeKind = constant).
    static_item = 5,
    /// Function signature in trait body (NodeKind = function).
    fn_signature = 6,
    /// Associated type declaration in a trait body (NodeKind = type_def).
    associated_type = 7,
};

/// Rust-specific metadata attached to nodes.
pub const RustMeta = struct {
    is_unsafe: bool = false,
    is_async: bool = false,
    is_const: bool = false,
    is_extern: bool = false,
    is_default: bool = false,
    sub_kind: RustSubKind = .none,
    /// ABI string for extern functions.
    abi: ?[]const u8 = null,
    /// Comma-separated derive trait names.
    derives: ?[]const u8 = null,
    /// Raw source text of outer attributes besides derive and test.
    attributes: ?[]const u8 = null,
    /// Raw source text of inner attributes (#![...]) on file or inline module nodes.
    inner_attributes: ?[]const u8 = null,
    /// Restriction scope for pub(...) visibility. Null for bare pub or private.
    visibility_scope: ?[]const u8 = null,
};

test "RustSubKind has exactly 7 variants" {
    comptime {
        const fields = @typeInfo(RustSubKind).@"enum".fields;
        std.debug.assert(fields.len == 8);
    }
}

test "RustMeta default values" {
    // Arrange
    const meta = RustMeta{};

    // Assert
    try std.testing.expect(!meta.is_unsafe);
    try std.testing.expect(!meta.is_async);
    try std.testing.expect(!meta.is_const);
    try std.testing.expect(!meta.is_extern);
    try std.testing.expect(!meta.is_default);
    try std.testing.expectEqual(RustSubKind.none, meta.sub_kind);
    try std.testing.expectEqual(@as(?[]const u8, null), meta.abi);
    try std.testing.expectEqual(@as(?[]const u8, null), meta.derives);
    try std.testing.expectEqual(@as(?[]const u8, null), meta.attributes);
    try std.testing.expectEqual(@as(?[]const u8, null), meta.inner_attributes);
    try std.testing.expectEqual(@as(?[]const u8, null), meta.visibility_scope);
}
