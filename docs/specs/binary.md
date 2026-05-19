# Binary Storage Format Specification

Version: 1

A compact binary format for persisting the code graph. Designed for fast
loading via memory mapping. All multi-byte integers are little-endian.

Extension: `.bin`

## File Layout

```
+------------------------------+
|       Header (80 bytes)      |
+------------------------------+
|   Node Table (fixed records) |
+------------------------------+
|   Edge Table (fixed records) |
+------------------------------+
| Metrics Table (fixed records)|
+------------------------------+
|  String Table (variable)     |
+------------------------------+
```

Each table starts at an 8-byte aligned offset. Padding between tables
is filled with zeros.

## Header (80 bytes)

| Offset | Field | Type | Description |
|--------|-------|------|-------------|
| 0 | `magic` | `[8]u8` | `"ZCPRISM\x00"` |
| 8 | `version` | `u32` | Format version (currently `1`) |
| 12 | `flags` | `u32` | Reserved (currently `0`) |
| 16 | `node_count` | `u64` | Number of node records |
| 24 | `edge_count` | `u64` | Number of edge records |
| 32 | `node_table_offset` | `u64` | Byte offset to the node table |
| 40 | `edge_table_offset` | `u64` | Byte offset to the edge table |
| 48 | `metrics_table_offset` | `u64` | Byte offset to the metrics table |
| 56 | `string_table_offset` | `u64` | Byte offset to the string table |
| 64 | `string_table_size` | `u64` | String table size in bytes |
| 72 | `project_root` | `StringRef` | Project root path |

## StringRef

A reference into the string table. Used throughout the node and edge
tables to store variable-length strings without inline allocation.

| Offset | Field | Type | Description |
|--------|-------|------|-------------|
| 0 | `offset` | `u32` | Byte offset into the string table |
| 4 | `len` | `u32` | String length in bytes |

A StringRef with `len = 0` represents a null or empty value.

Total size: 8 bytes.

## Node Table

Fixed-size records of 128 bytes each. One record per graph node.

| Offset | Field | Type | Description |
|--------|-------|------|-------------|
| 0 | `id` | `u64` | Node ID |
| 8 | `parent_id` | `u64` | Parent node ID (valid when `FLAG_HAS_PARENT` set) |
| 16 | `line_start` | `u32` | Start line (valid when `FLAG_HAS_LINE_START` set) |
| 20 | `line_end` | `u32` | End line (valid when `FLAG_HAS_LINE_END` set) |
| 24 | `col_start` | `u32` | Start column (valid when `FLAG_HAS_COL_START` set) |
| 28 | `col_end` | `u32` | End column (valid when `FLAG_HAS_COL_END` set) |
| 32 | `external_kind` | `u8` | `0` = none, `1` = stdlib, `2` = dependency |
| 33 | `flags` | `u8` | Bitmask (see below) |
| 34 | (padding) | `[2]u8` | Alignment padding |
| 36 | `content_hash` | `[16]u8` | Blake3 hash (valid when `FLAG_HAS_CONTENT_HASH` set) |
| 52 | (padding) | `[4]u8` | Alignment padding |
| 56 | `kind` | `StringRef` | Node kind tag name |
| 64 | `language` | `StringRef` | Language tag name |
| 72 | `visibility` | `StringRef` | Visibility tag name |
| 80 | `name` | `StringRef` | Declaration name |
| 88 | `file_path` | `StringRef` | Relative source file path |
| 96 | `signature` | `StringRef` | Function/type signature |
| 104 | `doc` | `StringRef` | Documentation comment |
| 112 | `ext_version` | `StringRef` | External dependency version |
| 120 | `lang_meta` | `StringRef` | Language metadata (binary-encoded) |

### Node Flags

| Bit | Name | Meaning |
|-----|------|---------|
| `0x01` | `FLAG_HAS_CONTENT_HASH` | `content_hash` field is populated |
| `0x02` | `FLAG_HAS_METRICS` | Corresponding metrics record is populated |
| `0x04` | `FLAG_HAS_PARENT` | `parent_id` field is valid |
| `0x08` | `FLAG_HAS_LINE_START` | `line_start` field is valid |
| `0x10` | `FLAG_HAS_LINE_END` | `line_end` field is valid |
| `0x20` | `FLAG_HAS_COL_START` | `col_start` field is valid |
| `0x40` | `FLAG_HAS_COL_END` | `col_end` field is valid |

### Node Kind Values

Stored as string tag names via StringRef (not numeric):
`file`, `module`, `function`, `type_def`, `enum_def`, `field`,
`parameter`, `constant`, `test_def`, `error_def`, `import_decl`,
`union_def`, `directory`.

### Edge Type Values

Stored as string tag names via StringRef (not numeric):
`accesses_field`, `calls`, `imports`, `uses_value`, `uses_type`,
`similar_to`, `exports`, `implements`, `contains`.

## Edge Table

Fixed-size records of 32 bytes each.

| Offset | Field | Type | Description |
|--------|-------|------|-------------|
| 0 | `source_id` | `u64` | Source node ID |
| 8 | `target_id` | `u64` | Target node ID |
| 16 | `edge_type` | `StringRef` | Edge type tag name |
| 24 | `source` | `StringRef` | Edge source tag name |

### Edge Source Values

Stored as string tag names: `tree_sitter`, `lsp`, `phantom`,
`workspace`.

## Metrics Table

Fixed-size records of 28 bytes each. One record per node, at the same
index as the node table. All bytes are zero for nodes without metrics
(files, phantom nodes).

| Offset | Field | Type | Description |
|--------|-------|------|-------------|
| 0 | `complexity` | `u16` | Cyclomatic complexity |
| 2 | `lines` | `u32` | Line count |
| 6 | `fan_in` | `u16` | Number of callers |
| 8 | `fan_out` | `u16` | Number of outgoing calls |
| 10 | `branches` | `u16` | Branch count (if, switch) |
| 12 | `loops` | `u16` | Loop count (while, for) |
| 14 | `error_paths` | `u16` | Error path count (catch, orelse, try) |
| 16 | `nesting_depth_max` | `u8` | Maximum nesting depth |
| 17 | (padding) | `[1]u8` | Alignment padding |
| 18 | `structural_hash` | `u64` | Normalized AST structure hash |
| 26 | (padding) | `[2]u8` | Alignment to 28 bytes |

## String Table

A contiguous byte buffer of UTF-8 text. Strings are not null-terminated;
their length is carried by the `len` field of each StringRef.

Contents stored in the string table:
- Node names
- File paths (relative to project root)
- Function/type signatures
- Documentation comments
- External dependency versions
- Node kind, edge type, edge source, language, and visibility tag names
- Language metadata (binary-encoded, structure depends on language)

Duplicate strings are deduplicated during serialization: identical byte
sequences share the same offset/length pair.

## Table Alignment

Each table begins at an 8-byte aligned offset. The alignment formula is:
`(offset + 7) & ~7`. Gaps between tables are zero-filled.

The node table starts immediately after the header at offset 80. The
remaining table offsets are recorded in the header.
