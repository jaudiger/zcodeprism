const std = @import("std");
const lang_support = @import("language_support.zig");
const rust_visitor = @import("rust/visitor.zig");
const rust_hooks = @import("rust/indexer_hooks.zig");
const zig_visitor = @import("zig/visitor.zig");
const zig_hooks = @import("zig/indexer_hooks.zig");
const ts_api = @import("../parser/tree_sitter_api.zig");

const LanguageSupport = lang_support.LanguageSupport;

const rust_support = LanguageSupport{
    .language = .rust,
    .extensions = &.{".rs"},
    .parseFn = &rust_visitor.parse,
    .lsp_config = .{
        .server_name = "rust-analyzer",
        .server_command = "rust-analyzer",
        .enrichFn = &rust_hooks.enrichWithLsp,
    },
    .excluded_dirs = &.{ "target", ".cargo" },
    .build_files = &.{"Cargo.toml"},
    .import_granularity = .file,
    .extractImportsFn = &rust_hooks.extractImports,
    .resolveImportPathFn = &rust_hooks.resolveImportPath,
    .parseBuildConfigFn = &rust_hooks.parseBuildConfig,
    .resolvePhantomsFn = &rust_hooks.resolvePhantoms,
    .buildEdgesFn = &rust_visitor.buildEdges,
    .grammarFn = &ts_api.tree_sitter_rust,
};

const zig_support = LanguageSupport{
    .language = .zig,
    .extensions = &.{".zig"},
    .parseFn = &zig_visitor.parse,
    .lsp_config = .{
        .server_name = "zls",
        .server_command = "zls",
        .enrichFn = &zig_hooks.enrichWithLsp,
    },
    .excluded_dirs = &.{ ".zig-cache", "zig-out" },
    .build_files = &.{ "build.zig.zon", "build.zig" },
    .import_granularity = .file,
    .extractImportsFn = &zig_hooks.extractImports,
    .resolveImportPathFn = &zig_hooks.resolveImportPath,
    .parseBuildConfigFn = &zig_hooks.parseBuildConfig,
    .resolvePhantomsFn = &zig_hooks.resolvePhantoms,
    .buildEdgesFn = &zig_visitor.buildEdges,
    .grammarFn = &ts_api.tree_sitter_zig,
};

const all_languages = [_]*const LanguageSupport{ &rust_support, &zig_support };

/// Static registry that maps file extensions to language support descriptors.
///
/// All entries are comptime constants; the registry requires no allocator
/// and no initialization at runtime.
pub const language_count = all_languages.len;

pub const Registry = struct {
    /// Returns the language support descriptor for the given file extension.
    ///
    /// `ext` must include the leading dot.
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

test "lookup by unknown extension returns null" {
    // Act
    const result = Registry.getByExtension(".xyz");

    // Assert
    try std.testing.expectEqual(@as(?*const LanguageSupport, null), result);
}

test "lookup by .rs extension returns rust support" {
    // Act
    const result = Registry.getByExtension(".rs");

    // Assert
    try std.testing.expect(result != null);
    try std.testing.expectEqual(lang_support.Language.rust, result.?.language);
}

test "lookup by .zig extension returns zig support" {
    // Act
    const result = Registry.getByExtension(".zig");

    // Assert
    try std.testing.expect(result != null);
    try std.testing.expectEqual(lang_support.Language.zig, result.?.language);
}
