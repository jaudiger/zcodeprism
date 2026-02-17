const std = @import("std");
const lang = @import("language.zig");
const visitor = @import("zig/visitor.zig");

const LanguageSupport = lang.LanguageSupport;

const zig_support = LanguageSupport{
    .name = "zig",
    .extensions = &.{".zig"},
    .parseFn = &visitor.parse,
    .lsp_config = null,
};

/// Static registry for language support lookup by file extension.
pub const Registry = struct {
    /// Look up language support by file extension (e.g. ".zig", ".rs").
    /// Returns null if no language support is registered for the extension.
    pub fn getByExtension(ext: []const u8) ?*const LanguageSupport {
        if (std.mem.eql(u8, ext, ".zig")) return &zig_support;
        return null;
    }
};

// --- Tests ---

test "lookup by .zig extension returns zig support" {
    // Act
    const result = Registry.getByExtension(".zig");

    // Assert
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("zig", result.?.name);
}

test "lookup by unknown extension returns null" {
    // Act
    const result = Registry.getByExtension(".xyz");

    // Assert
    try std.testing.expectEqual(@as(?*const LanguageSupport, null), result);
}

test "lookup by .rs extension returns null" {
    // Act: Rust not yet registered
    const result = Registry.getByExtension(".rs");

    // Assert
    try std.testing.expectEqual(@as(?*const LanguageSupport, null), result);
}

test "zig language support has parseFn" {
    // Act
    const result = Registry.getByExtension(".zig");

    // Assert: after visitor is registered, parseFn must not be null
    try std.testing.expect(result != null);
    try std.testing.expect(result.?.parseFn != null);
}
