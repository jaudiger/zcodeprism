const std = @import("std");
const protocol = @import("protocol.zig");

const Tool = protocol.Tool;
const P = protocol.SchemaProperty;

const node_kinds = [_][]const u8{ "file", "module", "function", "type_def", "enum_def", "field", "constant", "test_def", "error_def", "import_decl", "union_def", "directory" };
const edge_types = [_][]const u8{ "calls", "imports", "uses_type", "similar_to", "exports", "implements" };
const languages = [_][]const u8{ "zig", "rust" };

const tool_defs = [_]Tool{
    // -- graph namespace --
    .{
        .name = "graph.stats",
        .title = "Codebase Statistics",
        .description = "Returns global statistics about the indexed codebase: node counts by type, edge counts by type, languages detected, external dependency counts, total lines of code, last index timestamp.",
        .properties = &.{
            P{ .name = "scope", .description = "Restrict stats to a subtree (module path or glob)." },
            P{ .name = "language", .enum_values = &languages },
            P{ .name = "include_tests", .type = "boolean", .default_bool = false, .description = "Include test_def nodes in counts." },
            P{ .name = "include_external_nodes", .type = "boolean", .default_bool = false, .description = "Include external (phantom) nodes in counts." },
        },
        .output_properties = &.{
            P{ .name = "project_root", .description = "Absolute path to the project root." },
            P{ .name = "languages", .type = "array", .items_type = "string", .description = "Languages detected." },
            P{ .name = "total_files", .type = "integer" },
            P{ .name = "total_lines", .type = "integer" },
            P{ .name = "source_hash", .description = "Hash of the indexed source." },
            P{ .name = "nodes", .type = "object", .description = "Node counts by kind." },
            P{ .name = "edges", .type = "object", .description = "Edge counts by type." },
            P{ .name = "externals", .type = "object", .description = "External dependency summary." },
        },
        .output_required = &.{ "project_root", "languages", "total_files", "total_lines", "nodes", "edges", "externals" },
    },
    .{
        .name = "graph.search",
        .title = "Search Nodes",
        .description = "Search nodes by name, kind, visibility, language, external status, or metric thresholds. Supports regex on names. Returns matching nodes with basic metadata and metrics.",
        .properties = &.{
            P{ .name = "query", .description = "Regex on node names. Supports: . * + ? ^ $ \\ | () [abc] [^abc] [a-z] \\d \\w \\s \\b and negated \\D \\W \\S. Unanchored by default." },
            P{ .name = "kind", .enum_values = &node_kinds },
            P{ .name = "visibility", .enum_values = &[_][]const u8{ "public", "private" }, .description = "Filter by visibility." },
            P{ .name = "external", .enum_values = &[_][]const u8{ "include", "exclude", "only" }, .default_str = "include", .description = "Control phantom node inclusion." },
            P{ .name = "scope" },
            P{ .name = "min_complexity", .type = "integer" },
            P{ .name = "min_lines", .type = "integer" },
            P{ .name = "has_edge", .enum_values = &edge_types },
            P{ .name = "language", .enum_values = &languages },
            P{ .name = "include_tests", .type = "boolean", .default_bool = false, .description = "Include test_def nodes in results." },
            P{ .name = "offset", .type = "integer", .default_int = 0 },
            P{ .name = "limit", .type = "integer", .default_int = 50, .maximum = 200 },
        },
        .required = &.{"query"},
        .output_properties = &.{
            P{ .name = "total_matches", .type = "integer" },
            P{ .name = "nodes", .type = "array", .description = "Matching nodes with metadata and metrics." },
        },
        .output_required = &.{ "total_matches", "nodes" },
    },
    .{
        .name = "graph.get_nodes",
        .title = "Get Node Details",
        .description = "Returns complete information about one or more nodes: metadata, metrics, parent, doc, language-specific metadata, external status, content_hash (file nodes only), edges summary, and optionally source code. Accepts a single ID or an array.",
        .properties = &.{
            P{ .name = "node_ids", .one_of_string_or_array = true, .max_items = 50, .description = "One node ID or an array of node IDs." },
            P{ .name = "include_source", .type = "boolean", .default_bool = false, .description = "If true, include the full source code of each node." },
            P{ .name = "include_edges", .type = "boolean", .default_bool = true, .description = "If true, include a summary of all connected edges for each node." },
        },
        .required = &.{"node_ids"},
        .output_properties = &.{
            P{ .name = "nodes", .type = "array", .description = "Full node details." },
        },
        .output_required = &.{"nodes"},
    },
    .{
        .name = "graph.get_source",
        .title = "Get Source Code",
        .description = "Returns the source code of one or more nodes, optionally with surrounding context lines. Returns null source for phantom (external) nodes.",
        .properties = &.{
            P{ .name = "node_ids", .one_of_string_or_array = true, .max_items = 20, .description = "One node ID or an array of node IDs." },
            P{ .name = "context_lines", .type = "integer", .default_int = 0, .description = "Number of lines of context above and below each node." },
            P{ .name = "part", .enum_values = &[_][]const u8{ "full", "signature", "body" }, .default_str = "full" },
        },
        .required = &.{"node_ids"},
    },
    .{
        .name = "graph.get_edges",
        .title = "Get Edges",
        .description = "Returns all edges connected to one or more nodes, optionally filtered by direction and type. Each edge includes the connected node's basic info.",
        .properties = &.{
            P{ .name = "node_ids", .one_of_string_or_array = true, .max_items = 50, .description = "One node ID or an array of node IDs." },
            P{ .name = "direction", .enum_values = &[_][]const u8{ "in", "out", "both" }, .default_str = "both" },
            P{ .name = "edge_type", .enum_values = &edge_types },
            P{ .name = "include_external_nodes", .type = "boolean", .default_bool = false, .description = "Include external (phantom) nodes and their edges." },
            P{ .name = "offset", .type = "integer", .default_int = 0 },
            P{ .name = "limit", .type = "integer", .default_int = 50, .maximum = 200 },
        },
        .required = &.{"node_ids"},
    },
    .{
        .name = "graph.path",
        .title = "Find Path",
        .description = "Find the shortest path(s) between two nodes in the graph, traversing edges of specified types.",
        .properties = &.{
            P{ .name = "from_id", .description = "Source node ID." },
            P{ .name = "to_id", .description = "Target node ID." },
            P{ .name = "edge_types", .type = "array", .items_type = "string", .items_enum = &edge_types, .description = "Restrict traversal to these edge types." },
            P{ .name = "max_depth", .type = "integer", .default_int = 10, .maximum = 20 },
            P{ .name = "max_paths", .type = "integer", .default_int = 3, .maximum = 10 },
        },
        .required = &.{ "from_id", "to_id" },
        .output_properties = &.{
            P{ .name = "paths", .type = "array", .description = "Shortest paths between the two nodes." },
        },
        .output_required = &.{"paths"},
    },

    // -- explorer namespace --
    .{
        .name = "explorer.cursor_create",
        .title = "Create Cursor",
        .description = "Creates a new exploration cursor, optionally positioned on a node or scoped to a subtree. Returns a cursor ID and an initial summary of the visible neighborhood.",
        .properties = &.{
            P{ .name = "start_node_id", .description = "Node to start on. If omitted, cursor starts at the root." },
            P{ .name = "scope", .description = "Restrict the cursor's visible graph to a subtree." },
            P{ .name = "include_tests", .type = "boolean", .default_bool = false, .description = "Include test_def nodes in the cursor's visible graph." },
            P{ .name = "include_external_nodes", .type = "boolean", .default_bool = false, .description = "Include external (phantom) nodes in the cursor's visible graph." },
        },
        .output_properties = &.{
            P{ .name = "cursor_id" },
            P{ .name = "position", .type = "object", .description = "Current cursor position." },
            P{ .name = "neighborhood", .type = "object", .description = "Children and stats for the position." },
            P{ .name = "expires_in_seconds", .type = "integer" },
        },
        .output_required = &.{ "cursor_id", "position", "neighborhood", "expires_in_seconds" },
    },
    .{
        .name = "explorer.cursor_move",
        .title = "Move Cursor",
        .description = "Moves the cursor to a specific node. Returns the node's full details and its immediate neighborhood.",
        .properties = &.{
            P{ .name = "cursor_id" },
            P{ .name = "node_id" },
        },
        .required = &.{ "cursor_id", "node_id" },
    },
    .{
        .name = "explorer.cursor_expand",
        .title = "Expand Cursor",
        .description = "From the cursor's current position, expand the subgraph along specific edge types up to a given depth. Returns the expanded subgraph.",
        .properties = &.{
            P{ .name = "cursor_id" },
            P{ .name = "edge_types", .type = "array", .items_type = "string", .items_enum = &edge_types },
            P{ .name = "direction", .enum_values = &[_][]const u8{ "in", "out", "both" }, .default_str = "out" },
            P{ .name = "depth", .type = "integer", .default_int = 2, .maximum = 5 },
        },
        .required = &.{"cursor_id"},
    },
    .{
        .name = "explorer.cursor_query",
        .title = "Query from Cursor",
        .description = "Run a structured query from the cursor's current position within the visible subgraph.",
        .properties = &.{
            P{ .name = "cursor_id" },
            P{ .name = "query", .description = "Regex on node names. Supports: . * + ? ^ $ \\ | () [abc] [^abc] [a-z] \\d \\w \\s \\b and negated \\D \\W \\S. Unanchored by default." },
            P{ .name = "kind", .enum_values = &node_kinds },
            P{ .name = "min_complexity", .type = "integer" },
            P{ .name = "max_depth_from_position", .type = "integer", .default_int = 5 },
            P{ .name = "limit", .type = "integer", .default_int = 20 },
        },
        .required = &.{"cursor_id"},
    },
    .{
        .name = "explorer.cursor_close",
        .title = "Close Cursor",
        .description = "Explicitly closes a cursor and frees its resources. Cursors also auto-expire after inactivity.",
        .properties = &.{
            P{ .name = "cursor_id" },
        },
        .required = &.{"cursor_id"},
    },
    .{
        .name = "explorer.diff",
        .title = "Structural Diff",
        .description = "Structural diff between two or more nodes. Compares AST structure (not text). When given 2 nodes, returns a pairwise diff. When given N nodes, returns a similarity matrix.",
        .properties = &.{
            P{ .name = "node_ids", .type = "array", .items_type = "string", .min_items = 2, .max_items = 20, .description = "Two or more node IDs to compare. Must be internal (non-phantom) nodes." },
            P{ .name = "diff_mode", .enum_values = &[_][]const u8{ "structural", "textual", "both" }, .default_str = "structural", .description = "structural = AST-level, textual = raw text diff, both = combined." },
        },
        .required = &.{"node_ids"},
    },
    .{
        .name = "explorer.annotate",
        .title = "Annotate Nodes",
        .description = "Attach a temporary annotation (tag + optional note) to one or more nodes during exploration. Annotations persist for the cursor's lifetime.",
        .properties = &.{
            P{ .name = "cursor_id" },
            P{ .name = "node_ids", .one_of_string_or_array = true, .max_items = 50, .description = "One node ID or an array of node IDs." },
            P{ .name = "tag", .description = "A short tag (e.g. 'candidate', 'reviewed', 'skip')." },
            P{ .name = "note", .description = "Optional free-text note." },
        },
        .required = &.{ "cursor_id", "node_ids", "tag" },
    },
    .{
        .name = "explorer.annotations",
        .title = "List Annotations",
        .description = "Retrieve all annotations created by this cursor.",
        .properties = &.{
            P{ .name = "cursor_id" },
            P{ .name = "tag", .description = "Filter by tag." },
        },
        .required = &.{"cursor_id"},
    },

    // -- analysis namespace --
    .{
        .name = "analysis.complexity",
        .title = "Complexity Ranking",
        .description = "Returns nodes ranked by cyclomatic complexity with metric breakdown. Facts only. Excludes phantom nodes.",
        .properties = &.{
            P{ .name = "top_n", .type = "integer", .default_int = 10, .maximum = 50 },
            P{ .name = "scope" },
            P{ .name = "kind", .enum_values = &[_][]const u8{ "function", "file" }, .default_str = "function" },
            P{ .name = "language", .enum_values = &languages },
        },
    },
    .{
        .name = "analysis.coupling",
        .title = "Module Coupling",
        .description = "Compute coupling metrics between modules. Returns pairs ranked by coupling strength.",
        .properties = &.{
            P{ .name = "scope" },
            P{ .name = "granularity", .enum_values = &[_][]const u8{ "file", "directory" }, .default_str = "directory" },
            P{ .name = "min_coupling", .type = "number", .default_float = 0.3 },
            P{ .name = "top_n", .type = "integer", .default_int = 10 },
            P{ .name = "external", .enum_values = &[_][]const u8{ "include", "exclude" }, .default_str = "exclude", .description = "Include coupling to external deps in the analysis." },
            P{ .name = "language", .enum_values = &languages },
        },
    },
    .{
        .name = "analysis.dead_code",
        .title = "Dead Code Detection",
        .description = "Find symbols defined but never referenced. Returns each symbol with its reference count (0) and context. Excludes phantom nodes.",
        .properties = &.{
            P{ .name = "scope" },
            P{ .name = "include_test_only", .type = "boolean", .default_bool = false },
            P{ .name = "include_public", .type = "boolean", .default_bool = false },
            P{ .name = "kind", .enum_values = &[_][]const u8{ "function", "type_def", "enum_def", "union_def", "constant", "all" }, .default_str = "all" },
            P{ .name = "language", .enum_values = &languages },
            P{ .name = "offset", .type = "integer", .default_int = 0 },
            P{ .name = "limit", .type = "integer", .default_int = 50 },
        },
    },
    .{
        .name = "analysis.dependency_cycles",
        .title = "Dependency Cycles",
        .description = "Detect circular dependencies in the graph. Returns each cycle as an ordered list of nodes forming the loop.",
        .properties = &.{
            P{ .name = "scope" },
            P{ .name = "edge_types", .type = "array", .items_type = "string", .items_enum = &[_][]const u8{ "calls", "imports", "uses_type" }, .default_str_array = &.{"imports"} },
            P{ .name = "max_cycle_length", .type = "integer", .default_int = 10 },
            P{ .name = "language", .enum_values = &languages },
        },
    },
    .{
        .name = "analysis.duplicates",
        .title = "Duplicate Detection",
        .description = "Find clusters of structurally similar functions based on AST hashing. Returns groups with similarity scores and member details. Facts only. Excludes phantom nodes.",
        .properties = &.{
            P{ .name = "threshold", .type = "number", .minimum = 0.5, .maximum = 1.0, .default_float = 0.75 },
            P{ .name = "scope" },
            P{ .name = "min_lines", .type = "integer", .default_int = 5 },
            P{ .name = "include_source", .type = "boolean", .default_bool = false },
            P{ .name = "language", .enum_values = &languages },
            P{ .name = "offset", .type = "integer", .default_int = 0 },
            P{ .name = "limit", .type = "integer", .default_int = 10 },
        },
    },
    .{
        .name = "analysis.impact",
        .title = "Change Impact Analysis",
        .description = "Compute the transitive set of nodes affected if one or more nodes are modified. Traverses reverse 'calls' and 'uses_type' edges plus parent chain.",
        .properties = &.{
            P{ .name = "node_ids", .one_of_string_or_array = true, .max_items = 20, .description = "Node(s) to analyze impact for. Can be internal or phantom (external) nodes." },
            P{ .name = "edge_types", .type = "array", .items_type = "string", .items_enum = &[_][]const u8{ "calls", "uses_type", "imports" }, .default_str_array = &.{ "calls", "uses_type" } },
            P{ .name = "max_depth", .type = "integer", .default_int = 10, .maximum = 20 },
            P{ .name = "include_parent_chain", .type = "boolean", .default_bool = true },
        },
        .required = &.{"node_ids"},
        .output_properties = &.{
            P{ .name = "source_nodes", .type = "array", .items_type = "string" },
            P{ .name = "total_impacted", .type = "integer" },
            P{ .name = "impacted", .type = "array", .description = "Impacted nodes with depth and traversal path." },
        },
        .output_required = &.{ "source_nodes", "total_impacted", "impacted" },
    },
};

/// Tool registry holding MCP tool descriptors.
pub const Dispatcher = struct {
    tools: []const Tool = &tool_defs,

    /// Create a dispatcher with the default tool definitions.
    pub fn init() Dispatcher {
        return .{};
    }

    /// Release dispatcher resources (currently a no-op).
    pub fn deinit(self: *Dispatcher) void {
        _ = self;
    }

    /// Return the full list of registered tools.
    pub fn listTools(self: *const Dispatcher) []const Tool {
        return self.tools;
    }
};
