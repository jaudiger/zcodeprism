//! ZCodePrism -- a Zig library that ingests a local codebase, builds a
//! semantic code graph, and exposes it for exploration by LLMs and humans.
//!
//! This root module re-exports every public sub-module so that downstream
//! consumers (CLI, MCP server, tests) can import a single `@import("lib.zig")`
//! and reach any component.

/// Adjacency-list index for fast edge lookups by node.
pub const adjacency = @import("core/adjacency.zig");
/// Project configuration loaded from `.zcodeprism.zon`.
pub const config = @import("core/config.zig");
/// Edge definition and helpers.
pub const edge = @import("core/edge.zig");
/// Generation-swap support for atomic graph replacement.
pub const generation = @import("core/generation.zig");
/// The central Graph container (nodes + edges + edge index).
pub const graph = @import("core/graph.zig");
/// Composite graph index (scope, name, file) built once after parsing.
pub const graph_index_mod = @import("core/graph_index.zig");
/// Node metrics (complexity, fan-in/out, etc.).
pub const metrics_mod = @import("core/metrics.zig");
/// Node definition and helpers.
pub const node = @import("core/node.zig");
/// Phantom node creation for external/stdlib references.
pub const phantom = @import("core/phantom.zig");
/// Query engine: search, path finding, stats, impact analysis.
pub const query = @import("core/query.zig");
/// Simple regex engine for pattern matching on node names.
pub const regex = @import("regex.zig");
/// Scope matching for restricting queries to subtrees.
pub const scope = @import("core/scope.zig");
/// Shared type definitions: NodeId, NodeKind, EdgeType, Visibility, etc.
pub const types = @import("core/types.zig");
/// Workspace mode: multi-project assembly under a virtual root.
pub const workspace = @import("core/workspace.zig");

/// External provenance (ExternalInfo) for phantom nodes.
pub const external = @import("core/external.zig");
/// Dispatch module: serialization and accessors that route to per-language meta modules.
pub const lang_meta = @import("languages/lang_meta.zig");
/// Per-language Rust metadata (RustMeta, RustSubKind) and accessors.
pub const rust_meta = @import("languages/rust/meta.zig");
/// Per-language Zig metadata (ZigMeta) and accessors.
pub const zig_meta = @import("languages/zig/meta.zig");
/// Language-agnostic import/build types.
pub const language = @import("languages/language.zig");
/// Language feature detection and capability queries.
pub const language_support = @import("languages/language_support.zig");
/// Language registry mapping file extensions to parsers.
pub const registry = @import("languages/registry.zig");

/// Rust AST analysis helpers.
pub const rust_ast_analysis = @import("languages/rust/ast_analysis.zig");
/// Rust Cargo.toml parser for dependency and target extraction.
pub const rust_cargo_parser = @import("languages/rust/cargo_parser.zig");
/// Rust intra-file edge builder (calls, uses_type, implements).
pub const rust_edge_builder = @import("languages/rust/edge_builder.zig");
/// Rust indexer hooks for multi-file processing.
pub const rust_indexer_hooks = @import("languages/rust/indexer_hooks.zig");
/// Rust tree-sitter parse context (KindIds, ScopeIndex, FileIndex).
pub const rust_parse_context = @import("languages/rust/parse_context.zig");
/// Rust tree-sitter visitor producing nodes and edges from a single file.
pub const rust_visitor = @import("languages/rust/visitor.zig");

/// Zig tree-sitter visitor producing nodes and edges from a single file.
pub const visitor = @import("languages/zig/visitor.zig");
/// Zig-specific AST analysis helpers.
pub const zig_ast_analysis = @import("languages/zig/ast_analysis.zig");
/// Zig build.zig / build.zig.zon parser for module and dependency extraction.
pub const zig_build_parser = @import("languages/zig/build_parser.zig");
/// Zig cross-file edge resolution.
pub const zig_cross_file = @import("languages/zig/cross_file.zig");
/// Zig intra-file edge builder (calls, uses_type, etc.).
pub const zig_edge_builder = @import("languages/zig/edge_builder.zig");
/// Zig indexer hooks for multi-file processing.
pub const zig_indexer_hooks = @import("languages/zig/indexer_hooks.zig");

/// Multi-file indexer orchestrating per-file visitors and cross-file linking.
pub const indexer = @import("parser/indexer.zig");
/// Source map for file-path to content-hash tracking.
pub const source_map = @import("parser/source_map.zig");
/// Low-level tree-sitter API wrappers.
pub const tree_sitter_api = @import("parser/tree_sitter_api.zig");

/// Compact Text Graph (CTG) renderer.
pub const ctg = @import("render/ctg.zig");
/// Mermaid flowchart renderer.
pub const mermaid = @import("render/mermaid.zig");
/// Shared rendering utilities (filtering, sorting, label helpers).
pub const render_common = @import("render/common.zig");

/// Code analysis tools (complexity, dead code, duplicates, impact, coupling, cycles).
pub const analyzer = @import("analyzer/analyzer.zig");

/// Metrics enrichment pipeline (source metrics, fan-in/out, error sets).
pub const enrichment = @import("enrichment/enrichment.zig");

/// Semantic diff engine for comparing code graph snapshots.
pub const diff = @import("diff/diff.zig");

/// Explorer cursor for interactive graph navigation.
pub const cursor = @import("explorer/cursor.zig");
/// Cursor lifecycle manager (create, lookup, close).
pub const cursor_manager = @import("explorer/cursor_manager.zig");

/// LSP client, protocol types, and enrichment orchestration.
pub const lsp = @import("lsp/lsp.zig");

/// MCP server: JSON-RPC transport, protocol types, dispatch.
pub const mcp = @import("mcp/mcp.zig");

/// High-level CLI command workflows (init, index, export, snapshot,
/// diff, serve, status). Each `run` returns an error union; the CLI
/// dispatcher translates results and errors to stdout/stderr/exit codes.
pub const commands = @import("commands/commands.zig");

/// File watcher, debouncer, and generation manager for watch mode.
pub const watcher = @import("watcher/watcher_mod.zig");

/// Persistent storage backends (binary and JSONL).
pub const storage = @import("storage/storage.zig");

/// Vtable-based structured logging (noop by default, TextStderrLogger for CLI).
pub const logging = @import("logging.zig");

/// Semantic kind of an edge (calls, uses_type, exports, etc.).
pub const EdgeType = types.EdgeType;
/// Immutable view of a frozen graph.
pub const FrozenGraph = graph.FrozenGraph;
/// The central graph container.
pub const Graph = graph.Graph;
/// Vtable-based logger interface.
pub const Logger = logging.Logger;
/// A single node in the code graph.
pub const Node = node.Node;
/// Semantic kind of a node (function, type_def, module, etc.).
pub const NodeKind = types.NodeKind;
/// Visibility of a declaration (public, private, etc.).
pub const Visibility = types.Visibility;

test {
    const std = @import("std");
    std.testing.refAllDecls(@This());
}
