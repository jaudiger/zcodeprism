# MCP Tool Reference

ZCodePrism exposes a read-only MCP server over stdio (JSON-RPC 2.0,
protocol version `2025-11-25`). It advertises a single capability:
`tools` (with `listChanged: false`).

The server handles `initialize`, `ping`, `tools/list`, and `tools/call`.
Notifications (messages without an `id` field) are silently ignored and
produce no response. Requests with an explicit `"id": null` are valid and
receive a response with `"id": null`. Parse errors and Invalid Request
errors return a response with `"id": null` per JSON-RPC 2.0 section 5.1.

**20 tools across 3 namespaces, all read-only.**

## Conventions

### IDs

All node, edge, and cursor IDs are opaque strings. In single-project
mode, node IDs are hex-encoded u64 values. In workspace mode, they are
prefixed with the project name: `"backend:a3f1"`.

### Pagination

Tools returning lists accept `offset` (default 0) and `limit` (default
50, max 200).

### Scoping

Most tools accept an optional `scope` parameter restricting results to a
path prefix. In workspace mode, the project name is the first path
component (e.g., `"backend/"`, `"backend/src/parser/"`).

### Pluralized node_ids

Parameters accepting a node ID use `node_ids` as a union type:
`string | string[]`. A single string is treated as a one-element array.

### Filtering

Two global filters reduce noise. Both default to false.

| Parameter | Default | Effect |
|-----------|---------|--------|
| `include_tests` | `false` | When false, test nodes are excluded |
| `include_external_nodes` | `false` | When false, phantom nodes are excluded |

### Node Kinds

13 universal kinds shared across all languages:

`file`, `module`, `function`, `type_def`, `enum_def`, `field`,
`parameter`, `constant`, `test_def`, `error_def`, `import_decl`,
`union_def`, `directory`.

Language-specific sub-kinds are in the `lang_meta` object.

### Edge Types

| Type | From | To | Meaning |
|------|------|----|---------|
| `accesses_field` | function/test | field | Reads a field |
| `calls` | function | function | Calls in body |
| `contains` | directory/module | file/directory | Contains child |
| `exports` | file | declaration | Publicly exposes |
| `implements` | type_def | type_def | Implements trait/interface |
| `imports` | file | file | Imports (`@import`, `use`) |
| `similar_to` | function | function | Structural similarity above threshold |
| `uses_type` | function/constant | type/enum/union/error/external | References the type |

### Edge Source (provenance)

| Source | Meaning |
|--------|---------|
| `tree_sitter` | Inferred by syntactic analysis |
| `lsp` | Confirmed by LSP semantic resolution |
| `phantom` | Edge to/from an external node |
| `workspace` | Cross-project edge from workspace assembly |

### Error Model

Standard MCP error responses with codes: `not_found`,
`invalid_cursor`, `scope_error`, `parse_error`.

### Phantom Nodes

Phantom nodes represent stdlib types and external dependencies. They
carry an `external` field:
- `{"source": "stdlib"}` for standard library symbols
- `{"source": "dependency"}` for external packages

They have no source, no metrics, and no documentation.

### Workspace Mode

| Mode | Launched by |
|------|-------------|
| Single-project | `zcodeprism serve` |
| Workspace | `zcodeprism serve --workspace <path>` |

In workspace mode, graphs are assembled under a virtual root node. The
tool schemas do not change. Differences:
- Node IDs are prefixed: `"backend:a3f1"`
- File paths are prefixed: `"backend/src/parser/tokenizer.zig"`
- `scope` naturally selects a project: `"backend/"`
- The virtual root has ID `"root"`

---

## Core Tools (`graph.*`)

Stateless tools for querying the code graph.

### `graph.stats`

Global statistics. Natural entry point for consumers.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `scope` | string | no | Restrict to a path prefix |
| `language` | `"zig"` or `"rust"` | no | Filter by language |
| `include_tests` | boolean | no | Include test nodes in counts |
| `include_external_nodes` | boolean | no | Include phantom nodes in counts |

**Response:**

```json
{
  "project_root": "/home/user/my-project",
  "languages": ["zig"],
  "total_files": 42,
  "total_lines": 12830,
  "source_hash": "a7f3b2c9e1d4",
  "last_indexed": "2026-02-14T10:30:00Z",
  "nodes": { "file": 42, "function": 318, "type_def": 56, "enum_def": 23 },
  "edges": { "calls": 612, "imports": 134, "uses_type": 287 },
  "externals": {
    "stdlib_symbols": 34,
    "dependency_symbols": 12,
    "dependencies": [
      { "name": "tree_sitter", "version": "0.24.0", "referenced_symbols": 5 }
    ]
  }
}
```

In workspace mode without a project-specific scope, `project_root` is
replaced by `workspace_root` and `workspace_name`, with a `projects`
array containing per-project breakdowns.

---

### `graph.search`

Search nodes by name, kind, visibility, or metrics.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `query` | string | yes | Regex on node names |
| `kind` | string | no | Filter by node kind |
| `visibility` | `"public"` or `"private"` | no | Filter by visibility |
| `external` | `"include"`, `"exclude"`, or `"only"` | no | Phantom node inclusion (default: `"include"`) |
| `scope` | string | no | Path prefix filter |
| `min_complexity` | integer | no | Minimum cyclomatic complexity |
| `min_lines` | integer | no | Minimum line count |
| `has_edge` | string | no | Filter to nodes with this edge type |
| `language` | string | no | Filter by language |
| `include_tests` | boolean | no | Include test nodes |
| `offset` | integer | no | Pagination offset |
| `limit` | integer | no | Pagination limit (max 200) |

**Response:**

```json
{
  "total_matches": 7,
  "nodes": [
    {
      "id": "a3f1",
      "name": "parseToken",
      "kind": "function",
      "language": "zig",
      "file": "src/parser/tokenizer.zig",
      "line_start": 42,
      "line_end": 98,
      "visibility": "public",
      "external": null,
      "signature": "fn parseToken(self: *Tokenizer) !Token",
      "metrics": { "complexity": 12, "lines": 56, "fan_in": 3, "fan_out": 5 }
    }
  ]
}
```

---

### `graph.get_nodes`

Full detail on one or more nodes.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `node_ids` | string or string[] | yes | Node ID(s), max 50 |
| `include_source` | boolean | no | Include source code (default: false) |
| `include_edges` | boolean | no | Include edge summary (default: true) |

**Response:** Array of node objects with full metadata including
`lang_meta`, `metrics`, `edges` (in/out), and optionally `source`.

---

### `graph.get_source`

Source code retrieval with optional context lines.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `node_ids` | string or string[] | yes | Node ID(s), max 20 |
| `context_lines` | integer | no | Lines of context above/below (default: 0) |
| `part` | `"full"`, `"signature"`, or `"body"` | no | Which part to return (default: `"full"`) |

Returns null source for phantom nodes.

---

### `graph.get_edges`

Relationship exploration for one or more nodes.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `node_ids` | string or string[] | yes | Node ID(s), max 50 |
| `direction` | `"in"`, `"out"`, or `"both"` | no | Default: `"both"` |
| `edge_type` | string | no | Filter by edge type |
| `include_external_nodes` | boolean | no | Include phantom edges (default: false) |
| `offset` | integer | no | Pagination offset |
| `limit` | integer | no | Pagination limit (max 200) |

---

### `graph.path`

Find shortest paths between two nodes.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `from_id` | string | yes | Source node |
| `to_id` | string | yes | Target node |
| `edge_types` | string[] | no | Restrict to these edge types |
| `max_depth` | integer | no | Maximum path length (default: 10, max: 20) |
| `max_paths` | integer | no | Maximum paths to return (default: 3, max: 10) |

**Response:**

```json
{
  "paths": [
    {
      "length": 3,
      "nodes": ["a3f1", "b7c2", "c8d3", "d9e4"],
      "edges": [
        { "from": "a3f1", "to": "b7c2", "type": "calls", "source": "lsp" }
      ]
    }
  ]
}
```

---

## Explorer Tools (`explorer.*`)

Stateful cursor-based graph traversal. Cursors auto-expire after 10
minutes of inactivity.

### `explorer.cursor_create`

Creates an exploration cursor.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `start_node_id` | string | no | Starting node (default: project root) |
| `scope` | string | no | Restrict visible graph |
| `include_tests` | boolean | no | Include test nodes |
| `include_external_nodes` | boolean | no | Include phantom nodes |

**Response:**

```json
{
  "cursor_id": "cur_8f2a",
  "position": { "id": "root", "kind": "module", "name": "zcodeprism" },
  "neighborhood": {
    "children": [{ "id": "m1", "kind": "file", "name": "src/main.zig" }],
    "stats": { "visible_nodes": 487, "visible_edges": 1203 }
  },
  "expires_in_seconds": 600
}
```

---

### `explorer.cursor_move`

Moves a cursor to a specific node.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `cursor_id` | string | yes | Cursor ID |
| `node_id` | string | yes | Target node |

Returns the node's full details and immediate neighborhood.

---

### `explorer.cursor_expand`

Expands the subgraph from the cursor's current position.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `cursor_id` | string | yes | Cursor ID |
| `edge_types` | string[] | no | Edge types to traverse |
| `direction` | `"in"`, `"out"`, or `"both"` | no | Default: `"out"` |
| `depth` | integer | no | Expansion depth (default: 2, max: 5) |

**Response:**

```json
{
  "cursor_id": "cur_8f2a",
  "position": { "id": "a3f1", "name": "parseToken" },
  "subgraph": {
    "nodes": [{ "id": "...", "name": "...", "kind": "..." }],
    "edges": [{ "from": "...", "to": "...", "type": "calls" }]
  },
  "truncated": false,
  "total_nodes_in_expansion": 23
}
```

---

### `explorer.cursor_query`

Scoped search from the cursor's current position.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `cursor_id` | string | yes | Cursor ID |
| `query` | string | no | Regex on node names |
| `kind` | string | no | Filter by node kind |
| `min_complexity` | integer | no | Minimum complexity |
| `max_depth_from_position` | integer | no | Max traversal depth (default: 5) |
| `limit` | integer | no | Result limit (default: 20) |

---

### `explorer.cursor_close`

Explicitly closes a cursor and frees its resources.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `cursor_id` | string | yes | Cursor ID |

---

### `explorer.diff`

Structural diff between two or more nodes.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `node_ids` | string[] | yes | 2 to 20 internal (non-phantom) node IDs |
| `diff_mode` | `"structural"`, `"textual"`, or `"both"` | no | Default: `"structural"` |

For 2 nodes, returns a pairwise diff with similarity score. For N
nodes, returns a similarity matrix.

---

### `explorer.annotate`

Attach a temporary annotation to nodes during exploration.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `cursor_id` | string | yes | Cursor ID |
| `node_ids` | string or string[] | yes | Node ID(s), max 50 |
| `tag` | string | yes | Short tag (e.g., `"candidate"`, `"reviewed"`) |
| `note` | string | no | Free-text note |

Annotations persist for the cursor's lifetime.

---

### `explorer.annotations`

Retrieve all annotations for a cursor.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `cursor_id` | string | yes | Cursor ID |
| `tag` | string | no | Filter by tag |

---

## Analysis Tools (`analysis.*`)

Return facts and metrics only. No suggestions or recommendations.

### `analysis.duplicates`

Find clusters of structurally similar functions.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `threshold` | number | no | Similarity threshold 0.5-1.0 (default: 0.75) |
| `scope` | string | no | Path prefix filter |
| `min_lines` | integer | no | Minimum function size (default: 5) |
| `include_source` | boolean | no | Include source in response |
| `language` | string | no | Filter by language |
| `offset` | integer | no | Pagination offset |
| `limit` | integer | no | Result limit (default: 10) |

---

### `analysis.complexity`

Nodes ranked by cyclomatic complexity.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `top_n` | integer | no | Number of results (default: 10, max: 50) |
| `scope` | string | no | Path prefix filter |
| `kind` | `"function"` or `"file"` | no | Granularity (default: `"function"`) |
| `language` | string | no | Filter by language |

---

### `analysis.dead_code`

Symbols defined but never referenced.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `scope` | string | no | Path prefix filter |
| `include_test_only` | boolean | no | Include symbols only referenced by tests |
| `include_public` | boolean | no | Include public symbols (default: false) |
| `kind` | string | no | Node kind or `"all"` (default: `"all"`, excludes structural kinds) |
| `language` | string | no | Filter by language |
| `offset` | integer | no | Pagination offset |
| `limit` | integer | no | Result limit (default: 50) |

---

### `analysis.dependency_cycles`

Detect circular dependencies.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `scope` | string | no | Path prefix filter |
| `edge_types` | string[] | no | Edge types to check (default: `["imports"]`) |
| `max_cycle_length` | integer | no | Maximum cycle length (default: 10) |
| `language` | string | no | Filter by language |

---

### `analysis.coupling`

Coupling metrics between modules.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `scope` | string | no | Path prefix filter |
| `granularity` | `"file"` or `"directory"` | no | Default: `"directory"` |
| `min_coupling` | number | no | Minimum coupling score (default: 0.3) |
| `top_n` | integer | no | Number of results (default: 10) |
| `external` | `"include"` or `"exclude"` | no | Include external deps (default: `"exclude"`) |
| `language` | string | no | Filter by language |

---

### `analysis.impact`

Transitive impact analysis: what is affected if nodes are modified.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `node_ids` | string or string[] | yes | Node ID(s), max 20. Can be internal or phantom. |
| `edge_types` | string[] | no | Edge types to traverse (default: `["calls", "uses_type"]`) |
| `max_depth` | integer | no | Maximum traversal depth (default: 10, max: 20) |
| `include_parent_chain` | boolean | no | Include parent chain (default: true) |

**Response:**

```json
{
  "source_nodes": ["x001"],
  "source_name": "std.mem.Allocator",
  "source_external": "stdlib",
  "total_impacted": 47,
  "impacted": [
    {
      "node_id": "a3f1",
      "name": "parseToken",
      "kind": "function",
      "file": "src/parser/tokenizer.zig",
      "depth": 1,
      "via_edge": "uses_type",
      "via_node": "x001"
    }
  ]
}
```

---

## Tool Summary

| Namespace | Tool | Pluralized IDs | Stateful |
|-----------|------|----------------|----------|
| `graph` | `graph.stats` | no | no |
| `graph` | `graph.search` | no | no |
| `graph` | `graph.get_nodes` | yes | no |
| `graph` | `graph.get_source` | yes | no |
| `graph` | `graph.get_edges` | yes | no |
| `graph` | `graph.path` | no | no |
| `explorer` | `explorer.cursor_create` | no | yes |
| `explorer` | `explorer.cursor_move` | no | yes |
| `explorer` | `explorer.cursor_expand` | no | yes |
| `explorer` | `explorer.cursor_query` | no | yes |
| `explorer` | `explorer.cursor_close` | no | yes |
| `explorer` | `explorer.diff` | yes (2-N) | no |
| `explorer` | `explorer.annotate` | yes | yes |
| `explorer` | `explorer.annotations` | no | yes |
| `analysis` | `analysis.duplicates` | no | no |
| `analysis` | `analysis.complexity` | no | no |
| `analysis` | `analysis.dead_code` | no | no |
| `analysis` | `analysis.dependency_cycles` | no | no |
| `analysis` | `analysis.coupling` | no | no |
| `analysis` | `analysis.impact` | yes | no |
