# JSONL Storage/Export Format Specification

Version: 1.0

A line-oriented JSON format where each line is a self-contained JSON
object. Used both as a storage format (alternative to binary) and as an
export format. The text-based nature makes it diffable with standard
tools.

Extension: `.jsonl`

## Overview

JSONL can serve two roles:

- **Storage format**: when `storage.format` is set to `jsonl` in
  `.zcodeprism.zon`, the graph is persisted as
  `.zcodeprism/graph.jsonl` instead of `graph.bin`.
- **Export format**: `zcodeprism export --jsonl` writes the current
  graph to stdout or a file.

## Record Types

Each line is a JSON object with a `_type` discriminator field.

### Node Records (`_type: "node"`)

```json
{
  "_type": "node",
  "id": 1,
  "name": "parseToken",
  "kind": "function",
  "language": "zig",
  "file_path": "src/parser/tokenizer.zig",
  "line_start": 42,
  "line_end": 98,
  "visibility": "public",
  "parent_id": 5,
  "doc": "Parses the next token.",
  "signature": "fn parseToken(self: *Tokenizer) !Token",
  "content_hash": "a7f3b2c9e1d4f5a6b7c8d9e0a1b2c3d4",
  "external": null,
  "lang_meta": { ... },
  "metrics": { ... }
}
```

#### Node Fields

| Field | JSON Type | Nullable | Description |
|-------|-----------|----------|-------------|
| `_type` | string | no | Always `"node"` |
| `id` | integer | no | Node ID |
| `name` | string | no | Declaration name |
| `kind` | string | no | Node kind tag name (see below) |
| `language` | string | yes | `"zig"`, `"rust"`, or null |
| `file_path` | string | yes | Relative path, null for phantom nodes |
| `line_start` | integer | yes | Start line, null for phantom nodes |
| `line_end` | integer | yes | End line, null for phantom nodes |
| `visibility` | string | no | `"public"` or `"private"` |
| `parent_id` | integer | yes | Parent node ID, null if root |
| `doc` | string | yes | Documentation comment |
| `signature` | string | yes | Function/type signature |
| `content_hash` | string | yes | Blake3 hash as hex string (file nodes only) |
| `external` | varies | yes | External info (see below) |
| `lang_meta` | object | yes | Language-specific metadata (see below) |
| `metrics` | object | yes | Code metrics (see below) |

Null fields are always present with an explicit `null` value (never
omitted).

#### Node Kind Values

`file`, `module`, `function`, `type_def`, `enum_def`, `field`,
`parameter`, `constant`, `test_def`, `error_def`, `import_decl`,
`union_def`, `directory`.

#### External Field

Three possible shapes:

- Internal node: `null`
- Standard library: `"stdlib"` (string)
- Dependency: `{"type": "dependency", "version": "1.0.0"}` (version
  may be null)

#### Language Metadata (`lang_meta`)

Null when no language metadata is available. Otherwise an object whose
`type` field indicates the language.

Zig metadata:

```json
{
  "type": "zig",
  "is_comptime": false,
  "is_mutable": false,
  "is_inline": false,
  "is_extern": false,
  "is_packed": false,
  "comptime_conditional": false,
  "calling_convention": null
}
```

Rust metadata:

```json
{
  "type": "rust",
  "is_unsafe": false,
  "is_async": false,
  "is_const": false,
  "is_extern": false,
  "is_default": false,
  "sub_kind": "none",
  "abi": null,
  "derives": null,
  "attributes": null,
  "inner_attributes": null,
  "visibility_scope": null
}
```

#### Metrics

Null for file nodes and phantom nodes. Otherwise:

```json
{
  "complexity": 12,
  "lines": 56,
  "fan_in": 3,
  "fan_out": 5,
  "branches": 4,
  "loops": 2,
  "error_paths": 3,
  "nesting_depth_max": 4,
  "structural_hash": "e4c9a1b700000000"
}
```

The `structural_hash` is a 64-bit value rendered as a 16-character
hex string.

### Edge Records (`_type: "edge"`)

```json
{
  "_type": "edge",
  "source_id": 2,
  "target_id": 3,
  "edge_type": "calls",
  "source": "tree_sitter"
}
```

#### Edge Fields

| Field | JSON Type | Nullable | Description |
|-------|-----------|----------|-------------|
| `_type` | string | no | Always `"edge"` |
| `source_id` | integer | no | Source node ID |
| `target_id` | integer | no | Target node ID |
| `edge_type` | string | no | Edge type tag name |
| `source` | string | no | Edge provenance tag name |

#### Edge Type Values

`accesses_field`, `calls`, `imports`, `uses_value`, `uses_type`,
`similar_to`, `exports`, `implements`, `contains`.

#### Edge Source Values

`tree_sitter`, `lsp`, `phantom`, `workspace`.

## Ordering

1. All node records first, sorted by `id` ascending
2. All edge records second, sorted by `edge_type` (alphabetical), then
   `source_id`, then `target_id`

The order is deterministic for a given graph.

## Examples

Minimal file node:

```json
{"_type":"node","id":1,"name":"main.zig","kind":"file","language":"zig","file_path":"src/main.zig","line_start":1,"line_end":245,"visibility":"public","parent_id":0,"doc":null,"signature":null,"content_hash":"a7f3b2c9e1d4f5a6b7c8d9e0a1b2c3d4","external":null,"lang_meta":null,"metrics":null}
```

Phantom node (stdlib):

```json
{"_type":"node","id":100,"name":"std.mem.Allocator","kind":"type_def","language":"zig","file_path":null,"line_start":null,"line_end":null,"visibility":"public","parent_id":99,"doc":null,"signature":null,"content_hash":null,"external":"stdlib","lang_meta":null,"metrics":null}
```

Edge:

```json
{"_type":"edge","source_id":2,"target_id":3,"edge_type":"calls","source":"tree_sitter"}
```
