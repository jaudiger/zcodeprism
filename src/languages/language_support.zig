const std = @import("std");
const ts = @import("tree-sitter");
const graph_mod = @import("../core/graph.zig");
const phantom_mod = @import("../core/phantom.zig");
const graph_index_mod = @import("../core/graph_index.zig");
const lang = @import("language.zig");
const logging = @import("../logging.zig");
const types = @import("../core/types.zig");

const Graph = graph_mod.Graph;
const PhantomManager = phantom_mod.PhantomManager;
const Logger = logging.Logger;
const LspClient = @import("../lsp/client.zig").LspClient;
const worklist_mod = @import("../lsp/worklist.zig");
const LspWorklist = worklist_mod.LspWorklist;

pub const GraphIndex = graph_index_mod.GraphIndex;
pub const ScopeIndex = graph_index_mod.ScopeIndex;
pub const NameIndex = graph_index_mod.NameIndex;

const NodeId = types.NodeId;

/// Shared map from node IDs (fields and parameters) to their resolved
/// type node IDs. Created by the indexer, populated across all files.
pub const NodeTypeMap = std.AutoHashMapUnmanaged(NodeId, NodeId);

// Re-export data types from language.zig for convenience.

/// Discriminates between package-level and file-level imports.
pub const ImportKind = lang.ImportKind;
/// A single import declaration extracted from source code.
pub const ImportEntry = lang.ImportEntry;
/// Whether the language resolves imports at file or symbol granularity.
pub const ImportGranularity = lang.ImportGranularity;
/// Parsed build-system configuration (dependency paths, package roots).
pub const BuildConfig = lang.BuildConfig;
/// Extracts import entries from source code by walking the tree-sitter AST.
pub const ExtractImportsFn = *const fn (source: []const u8, ts_language: *const ts.Language, out: []ImportEntry) usize;
/// Resolves an import path to an absolute filesystem path.
pub const ResolveImportPathFn = lang.ResolveImportPathFn;
/// Parses a build-system file into a BuildConfig.
pub const ParseBuildConfigFn = lang.ParseBuildConfigFn;

/// Language enum for tagging nodes.
pub const Language = types.Language;

/// Function pointer type for language-specific source parsers.
///
/// Parses `source` and populates `graph` with the resulting nodes and edges.
/// `allocator` is passed through to graph mutation methods.
/// `file_path`, when provided, is stored on each created node for location tracking.
/// Returns `error.OutOfMemory` if graph mutation fails.
pub const ParseFn = *const fn (allocator: std.mem.Allocator, io: std.Io, source: []const u8, graph: *Graph, file_path: ?[]const u8, logger: Logger) error{OutOfMemory}!void;

/// Resolves external references for a single file's node range.
///
/// Scans `graph` nodes in `[file_idx, scope_end)`, creates phantom nodes for
/// unresolved external symbols via `phantom_mgr`, and records a `UsageSite`
/// for each phantom through `PhantomManager.recordUsageSite`. The caller
/// (`indexDirectory`) transfers those sites into the worklist after all files
/// have been processed.
/// `graph_index` provides pre-built scope, name, and file indexes.
/// `build_config`, when non-null, supplies dependency paths for import resolution.
/// Returns `OutOfMemory` if graph or phantom registration fails.
pub const ResolvePhantomsFn = *const fn (
    allocator: std.mem.Allocator,
    graph: *Graph,
    source: []const u8,
    file_idx: usize,
    scope_end: usize,
    phantom_mgr: *PhantomManager,
    graph_index: *const GraphIndex,
    build_config: ?*const BuildConfig,
    logger: Logger,
) error{OutOfMemory}!void;

/// Re-parses source with tree-sitter to emit cross-file edges for a single
/// file's node range. `phantom_mgr` is the shared phantom registry for
/// external symbol lookups. `node_type_map` is shared across all files and
/// accumulates field/parameter-to-type mappings. `wl` collects unresolved
/// references.
pub const BuildEdgesFn = *const fn (
    allocator: std.mem.Allocator,
    io: std.Io,
    source: []const u8,
    graph: *Graph,
    file_idx: usize,
    scope_end: usize,
    file_path: ?[]const u8,
    graph_index: *const GraphIndex,
    phantom_mgr: *const PhantomManager,
    node_type_map: *NodeTypeMap,
    wl: *LspWorklist,
    logger: Logger,
) error{OutOfMemory}!void;

/// Descriptor for a supported programming language.
///
/// Groups together all language-specific configuration and function pointers
/// needed by the indexer and cross-file resolver. Each registered language
/// provides one static instance of this struct.
/// All function pointer fields use concrete types for compile-time type safety.
pub const LanguageSupport = struct {
    /// Language identifier used for tagging graph nodes.
    language: Language,
    /// File extensions that identify this language, including the leading dot.
    extensions: []const []const u8,
    /// Parses source code into graph nodes and edges.
    parseFn: ParseFn,
    /// LSP server configuration for graph enrichment. Null when LSP is not supported.
    lsp_config: ?LspConfig = null,
    /// Directory names to skip during recursive file discovery.
    excluded_dirs: []const []const u8 = &.{},
    /// Build-system file names used to detect project roots.
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
    /// Builds cross-file edges for a single file's node range. Called by the
    /// indexer after all files have been parsed, so the graph contains every
    /// file's nodes and the FileIndex/ScopeIndex are complete. Re-parses
    /// source with tree-sitter to walk the AST for edge patterns.
    buildEdgesFn: ?BuildEdgesFn = null,
    /// Returns the tree-sitter grammar for this language.
    grammarFn: *const fn () callconv(.c) *const ts.Language,
};

/// Accumulated counters from an LSP enrichment pass.
pub const EnrichResult = struct {
    worklist_total: usize = 0,
    worklist_resolved: usize = 0,
    definition_queries: usize = 0,
    definition_successes: usize = 0,
    type_definition_queries: usize = 0,
    type_definition_successes: usize = 0,
    hover_queries: usize = 0,
    hover_successes: usize = 0,
    reference_queries: usize = 0,
    reference_successes: usize = 0,
    edges_promoted: usize = 0,
    edges_added: usize = 0,
    errors_inferred: usize = 0,
    phantoms_enriched: usize = 0,
    phantoms_remaining: usize = 0,
    warmup_ms: u64 = 0,

    /// Merge counters from another result into this one.
    pub fn accumulate(self: *EnrichResult, other: EnrichResult) void {
        self.worklist_total += other.worklist_total;
        self.worklist_resolved += other.worklist_resolved;
        self.definition_queries += other.definition_queries;
        self.definition_successes += other.definition_successes;
        self.type_definition_queries += other.type_definition_queries;
        self.type_definition_successes += other.type_definition_successes;
        self.hover_queries += other.hover_queries;
        self.hover_successes += other.hover_successes;
        self.reference_queries += other.reference_queries;
        self.reference_successes += other.reference_successes;
        self.edges_promoted += other.edges_promoted;
        self.edges_added += other.edges_added;
        self.errors_inferred += other.errors_inferred;
        self.phantoms_enriched += other.phantoms_enriched;
        self.phantoms_remaining += other.phantoms_remaining;
        if (other.warmup_ms > self.warmup_ms) self.warmup_ms = other.warmup_ms;
    }
};

/// Language-specific callback that queries an LSP client and enriches
/// the graph with edges and metadata that tree-sitter alone cannot provide.
/// `wl` carries both unresolved AST references and phantom hover sites.
pub const EnrichFn = *const fn (
    allocator: std.mem.Allocator,
    io: std.Io,
    graph: *Graph,
    client: *LspClient,
    wl: *const LspWorklist,
    logger: Logger,
) error{OutOfMemory}!EnrichResult;

/// Configuration for launching an LSP server to enrich the code graph.
pub const LspConfig = struct {
    /// Display name of the LSP server.
    server_name: []const u8,
    /// Shell command to start the LSP server process.
    server_command: []const u8,
    /// Optional JSON string passed as `initializationOptions` to the server.
    init_options: ?[]const u8 = null,
    /// Language-specific enrichment callback. Null when not yet implemented.
    enrichFn: ?EnrichFn = null,
};
