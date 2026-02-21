const std = @import("std");
const logging = @import("../logging.zig");
const ZigMeta = @import("zig/meta.zig").ZigMeta;

/// Language-specific metadata union. Each language variant carries
/// its own metadata struct. `.none` is used for nodes that need
/// no language-specific information.
pub const LangMeta = union(enum) {
    zig: ZigMeta,
    none: void,
};

/// Information about external (phantom) nodes, i.e. nodes that reference
/// code outside the project (stdlib, dependencies).
pub const ExternalInfo = union(enum) {
    none: void,
    stdlib: void,
    dependency: struct {
        version: ?[]const u8,
    },
};

/// Interface for language-specific support.
/// Uses `*anyopaque` for the graph parameter to avoid circular
/// dependencies (language.zig must not import graph.zig).
pub const LanguageSupport = struct {
    name: []const u8,
    extensions: []const []const u8,
    parseFn: ?*const fn (source: []const u8, graph: *anyopaque, file_path: ?[]const u8, logger: logging.Logger) anyerror!void,
    lsp_config: ?LspConfig,
};

/// Configuration for an LSP server that can enrich the graph.
pub const LspConfig = struct {
    server_name: []const u8,
    server_command: []const u8,
    init_options: ?[]const u8 = null,
};

// --- Tests ---

test "node stores ZigMeta with is_comptime true" {
    // Arrange
    const meta = LangMeta{ .zig = .{ .is_comptime = true } };

    // Assert
    try std.testing.expect(meta.zig.is_comptime);
}

test "node stores LangMeta.none" {
    // Arrange
    const meta = LangMeta{ .none = {} };

    // Assert
    try std.testing.expectEqual(LangMeta.none, meta);
}

test "ZigMeta default values are all false or null" {
    // Arrange
    const meta = ZigMeta{};

    // Assert
    try std.testing.expect(!meta.is_comptime);
    try std.testing.expect(!meta.is_inline);
    try std.testing.expect(!meta.is_extern);
    try std.testing.expect(!meta.is_packed);
    try std.testing.expectEqual(@as(?[]const []const u8, null), meta.error_set_names);
    try std.testing.expectEqual(@as(?[]const []const u8, null), meta.inferred_errors);
    try std.testing.expect(!meta.comptime_conditional);
}

test "node with external equals none" {
    // Arrange
    const ext = ExternalInfo{ .none = {} };

    // Assert
    try std.testing.expectEqual(ExternalInfo.none, ext);
}

test "node with external equals stdlib" {
    // Arrange
    const ext = ExternalInfo{ .stdlib = {} };

    // Assert
    try std.testing.expectEqual(ExternalInfo.stdlib, ext);
}

test "node with external equals dependency with version" {
    // Arrange
    const version_str = "1.2.3";
    const ext = ExternalInfo{ .dependency = .{ .version = version_str } };

    // Assert
    switch (ext) {
        .dependency => |dep| {
            try std.testing.expect(dep.version != null);
            try std.testing.expectEqualStrings("1.2.3", dep.version.?);
        },
        else => return error.UnexpectedVariant,
    }
}
