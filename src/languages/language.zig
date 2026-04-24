const std = @import("std");
const logging = @import("../logging.zig");

pub const LangMeta = @import("../core/lang_meta.zig").LangMeta;
pub const ExternalInfo = @import("../core/lang_meta.zig").ExternalInfo;

/// Classification of an import statement for topo-sort ordering and
/// phantom node resolution.
pub const ImportKind = enum {
    /// Project-internal file resolved via a relative path.
    project_file,
    /// Standard library module.
    stdlib,
    /// External dependency declared in the build manifest.
    dependency,
    /// Cannot be classified without additional context.
    unknown,
};

/// A single import statement extracted from source text.
pub const ImportEntry = struct {
    /// Import path string -- a zero-copy slice into the source text.
    path: []const u8,
    /// Classification of this import (project file, stdlib, dependency, or unknown).
    kind: ImportKind,
};

/// Describes whether a language's import system resolves to individual
/// files or to entire packages (directories).
pub const ImportGranularity = enum {
    /// Each import resolves to a single source file.
    file,
    /// Each import resolves to a directory containing all package files.
    package,
};

/// Language-specific build configuration extracted from project manifests.
/// All slice data is allocator-owned; call `deinit` to release it.
pub const BuildConfig = struct {
    /// Module declarations extracted from the build script, or null if
    /// no build script was found or it contained no module declarations.
    build_modules: ?[]BuildModule = null,
    /// External dependencies extracted from the build manifest, or null
    /// if no dependencies were declared.
    build_dependencies: ?[]BuildDep = null,

    /// A module declaration from the build script.
    pub const BuildModule = struct {
        /// Module variable name from the build script.
        name: []u8,
        /// Root source file path relative to the project root, or null
        /// if the build script did not specify one.
        root_source_file: ?[]u8,
    };

    /// An external dependency declaration from the build manifest.
    pub const BuildDep = struct {
        /// Dependency name as declared in the build manifest.
        name: []u8,
        /// Version URL from the build manifest, or null if no URL was found.
        version: ?[]u8,
    };

    /// Free all allocator-owned memory held by this BuildConfig.
    pub fn deinit(self: BuildConfig, allocator: std.mem.Allocator) void {
        if (self.build_modules) |modules| {
            for (modules) |m| {
                allocator.free(m.name);
                if (m.root_source_file) |rsf| allocator.free(rsf);
            }
            allocator.free(modules);
        }
        if (self.build_dependencies) |deps| {
            for (deps) |d| {
                allocator.free(d.name);
                if (d.version) |v| allocator.free(v);
            }
            allocator.free(deps);
        }
    }
};

/// Callback that resolves an import path relative to the importing file.
/// Returns a slice into `buf` with the resolved absolute path, or null if
/// `candidate_idx` is out of range or resolution fails. Languages with
/// multiple resolution candidates increment `candidate_idx` across calls;
/// single-candidate languages return null for any `candidate_idx` > 0.
pub const ResolveImportPathFn = *const fn (buf: []u8, importer_path: []const u8, import_path: []const u8, candidate_idx: usize) ?[]const u8;

/// Callback that parses a build manifest file at the given project root
/// and returns extracted dependency names in a `BuildConfig`.
/// The returned config is allocator-owned; caller must call `deinit`.
pub const ParseBuildConfigFn = *const fn (allocator: std.mem.Allocator, io: std.Io, project_root: []const u8, logger: logging.Logger) error{OutOfMemory}!BuildConfig;
