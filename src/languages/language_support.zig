const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const phantom_mod = @import("../core/phantom.zig");
const lang = @import("language.zig");
const logging = @import("../logging.zig");

const Graph = graph_mod.Graph;
const PhantomManager = phantom_mod.PhantomManager;
const Logger = logging.Logger;

// Re-export data types from language.zig for convenience.

/// Discriminates between package-level and file-level imports.
pub const ImportKind = lang.ImportKind;
/// A single import declaration extracted from source code.
pub const ImportEntry = lang.ImportEntry;
/// Whether the language resolves imports at file or symbol granularity.
pub const ImportGranularity = lang.ImportGranularity;
/// Parsed build-system configuration (dependency paths, package roots).
pub const BuildConfig = lang.BuildConfig;
/// Extracts import entries from a source string.
pub const ExtractImportsFn = lang.ExtractImportsFn;
/// Resolves an import path to an absolute filesystem path.
pub const ResolveImportPathFn = lang.ResolveImportPathFn;
/// Parses a build-system file into a BuildConfig.
pub const ParseBuildConfigFn = lang.ParseBuildConfigFn;

/// Function pointer type for language-specific source parsers.
///
/// Parses `source` and populates `graph` with the resulting nodes and edges.
/// `allocator` is passed through to graph mutation methods.
/// `file_path`, when provided, is stored on each created node for location tracking.
/// Returns an error if the underlying tree-sitter parse or graph mutation fails.
pub const ParseFn = *const fn (allocator: std.mem.Allocator, source: []const u8, graph: *Graph, file_path: ?[]const u8, logger: Logger) anyerror!void;

/// Function pointer type for resolving phantom (external) references in a single file.
///
/// Scans nodes in the range `[file_idx, scope_end)` of `graph`, identifies unresolved
/// references, and registers them as phantom nodes via `phantom_mgr`.
/// `allocator` is passed through to graph and phantom mutation methods.
/// `build_config`, when provided, supplies dependency paths for import resolution.
/// Returns an error if graph access or phantom registration fails.
pub const ResolvePhantomsFn = *const fn (
    allocator: std.mem.Allocator,
    graph: *Graph,
    source: []const u8,
    file_idx: usize,
    scope_end: usize,
    phantom_mgr: *PhantomManager,
    build_config: ?*const BuildConfig,
    logger: Logger,
) anyerror!void;

/// Descriptor for a supported programming language.
///
/// Groups together all language-specific configuration and function pointers
/// needed by the indexer and cross-file resolver. Each registered language
/// provides one static instance of this struct.
/// All function pointer fields use concrete types for compile-time type safety.
pub const LanguageSupport = struct {
    /// Human-readable language name (e.g. "zig").
    name: []const u8,
    /// File extensions that identify this language (e.g. &.{".zig"}).
    extensions: []const []const u8,
    /// Parses source code into graph nodes and edges. Null when no parser is available yet.
    parseFn: ?ParseFn = null,
    /// LSP server configuration for graph enrichment. Null when LSP is not supported.
    lsp_config: ?LspConfig = null,
    /// Directory names to skip during recursive file discovery (e.g. ".zig-cache").
    excluded_dirs: []const []const u8 = &.{},
    /// Build-system file names used to detect project roots (e.g. "build.zig.zon").
    build_files: []const []const u8 = &.{},
    /// Granularity of import resolution for this language.
    import_granularity: ImportGranularity = .file,
    /// Extracts import entries from source code. Null when not implemented.
    extractImportsFn: ?ExtractImportsFn = null,
    /// Resolves an import path to an absolute file path. Null when not implemented.
    resolveImportPathFn: ?ResolveImportPathFn = null,
    /// Parses build configuration files for dependency information. Null when not implemented.
    parseBuildConfigFn: ?ParseBuildConfigFn = null,
    /// Resolves phantom references within a file's node range. Null when not implemented.
    resolvePhantomsFn: ?ResolvePhantomsFn = null,
};

/// Configuration for launching an LSP server to enrich the code graph.
pub const LspConfig = struct {
    /// Display name of the LSP server (e.g. "zls").
    server_name: []const u8,
    /// Shell command to start the LSP server process.
    server_command: []const u8,
    /// Optional JSON string passed as `initializationOptions` to the server.
    init_options: ?[]const u8 = null,
};
