const std = @import("std");
const lang_support = @import("language_support.zig");
const visitor = @import("zig/visitor.zig");
const zig_hooks = @import("zig/indexer_hooks.zig");

const LanguageSupport = lang_support.LanguageSupport;

const zig_support = LanguageSupport{
    .name = "zig",
    .extensions = &.{".zig"},
    .parseFn = &visitor.parse,
    .lsp_config = null,
    .excluded_dirs = &.{ ".zig-cache", "zig-out" },
    .build_files = &.{ "build.zig.zon", "build.zig" },
    .import_granularity = .file,
    .extractImportsFn = &zig_hooks.extractImports,
    .resolveImportPathFn = &zig_hooks.resolveImportPath,
    .parseBuildConfigFn = &zig_hooks.parseBuildConfig,
    .resolvePhantomsFn = &zig_hooks.resolvePhantoms,
};

const all_languages = [_]*const LanguageSupport{&zig_support};

/// Static registry that maps file extensions to language support descriptors.
///
/// All entries are comptime constants; the registry requires no allocator
/// and no initialization at runtime.
pub const Registry = struct {
    /// Returns the language support descriptor for the given file extension.
    ///
    /// `ext` must include the leading dot (e.g. ".zig").
    /// Returns null when no language is registered for the extension.
    pub fn getByExtension(ext: []const u8) ?*const LanguageSupport {
        for (&all_languages) |ls| {
            for (ls.extensions) |e| {
                if (std.mem.eql(u8, ext, e)) return ls;
            }
        }
        return null;
    }

    /// Returns a slice of all registered language support descriptors.
    ///
    /// The indexer uses this to collect excluded directories across all languages.
    pub fn allLanguages() []const *const LanguageSupport {
        return &all_languages;
    }
};

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
