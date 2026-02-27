const std = @import("std");
const types = @import("types.zig");
const lang = @import("../languages/language.zig");
const metrics_mod = @import("metrics.zig");

const NodeId = types.NodeId;
const NodeKind = types.NodeKind;
const Visibility = types.Visibility;
const Language = types.Language;
const LangMeta = lang.LangMeta;
const ExternalInfo = lang.ExternalInfo;

/// A semantic code element in the code graph (function, type, field, etc.).
///
/// Nodes are created by the visitor during single-file parsing and enriched
/// by the indexer during multi-file indexation. The `id` field is assigned
/// by `Graph.addNode`; callers should pass a placeholder (typically `.root`)
/// which the graph overwrites with the next sequential id.
///
/// String fields (`name`, `file_path`, `doc`, `signature`) are borrowed
/// slices whose backing memory is owned by the graph's `owned_buffers`.
/// They remain valid for the lifetime of the graph.
pub const Node = struct {
    /// Unique identifier for this node within the graph.
    /// Assigned by `Graph.addNode`; callers should not set this manually.
    id: NodeId,

    /// The declared name of this code element (e.g. function name, type name).
    /// Borrowed slice -- lifetime tied to the graph's owned buffers.
    name: []const u8,

    /// Semantic kind: function, type_def, field, file, etc.
    kind: NodeKind,

    /// Programming language this node was parsed from.
    language: Language,

    /// Absolute path to the source file containing this node.
    /// Null for phantom nodes (stdlib/dependency references) and the
    /// synthetic root node.
    file_path: ?[]const u8 = null,

    /// 0-based line number of the first line of this element in the source file.
    /// Null for phantom nodes and the root node.
    line_start: ?u32 = null,

    /// 0-based line number of the last line of this element in the source file.
    /// Null for phantom nodes and the root node.
    line_end: ?u32 = null,

    /// NodeId of the lexical parent (e.g. the struct containing a method).
    /// Null for top-level file nodes whose parent is the synthetic root.
    parent_id: ?NodeId = null,

    /// Whether this declaration is `pub` or private.
    /// Defaults to `.private`; the visitor sets `.public` when the `pub`
    /// keyword is present.
    visibility: Visibility = .private,

    /// Documentation comment text extracted from `///` lines above
    /// the declaration. Null when no doc comment is present.
    doc: ?[]const u8 = null,

    /// Human-readable signature string (e.g. `fn foo(a: u32) void`).
    /// Null when the visitor cannot extract a meaningful signature.
    signature: ?[]const u8 = null,

    /// Truncated hash of the node's source text, used for change detection
    /// and snapshot diffing. 12 bytes (96 bits). Null when the source text
    /// is not available (phantom nodes).
    content_hash: ?[12]u8 = null,

    /// Quantitative code metrics (complexity, lines, fan-in/out, etc.).
    /// Null until the metrics pass populates them.
    metrics: ?metrics_mod.Metrics = null,

    /// Language-specific metadata (e.g. Zig comptime/inline/extern flags).
    /// Defaults to `.none` for nodes that carry no language-specific info.
    lang_meta: LangMeta = .{ .none = {} },

    /// External origin info for phantom nodes (stdlib or dependency).
    /// Defaults to `.none` for project-internal nodes.
    external: ExternalInfo = .{ .none = {} },
};
