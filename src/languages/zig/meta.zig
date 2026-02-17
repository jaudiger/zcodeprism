/// Zig-specific metadata attached to nodes.
pub const ZigMeta = struct {
    is_comptime: bool = false,
    is_inline: bool = false,
    is_extern: bool = false,
    error_set_names: ?[]const []const u8 = null,
    inferred_errors: ?[]const []const u8 = null,
    comptime_conditional: bool = false,
};
