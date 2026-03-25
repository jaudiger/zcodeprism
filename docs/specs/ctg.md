# Compact Text Graph (CTG) Format Specification

Version: 1.0

A compact, deterministic text format for representing a code graph. Used
for exports and human-readable inspection. Optimized for small size
(suitable for LLM context windows) and stable `diff` output between
snapshots.

Extension: `.ctg`

## Structure

A CTG file consists of a header (comment lines), followed by ordered
sections. Each section begins with a `[tag]` line. Empty sections are
omitted.

```
# zcodeprism graph -- {project_name}
# {stats_summary}
# languages: {lang1}, {lang2}
# generated {ISO-8601}
# snapshot: {name} | source_hash: {hash}

[files]
...

[types]
...

[unions]
...

[enums]
...

[functions]
...

[constants]
...

[errors]
...

[tests]
...

[externals]
...

[edges]
...
```

## Header

Lines beginning with `#` are metadata. They are informational and not
part of the parseable graph. The order is fixed.

| Line | Format | Example |
|------|--------|---------|
| 1 | `# zcodeprism graph -- {project_name}` | `# zcodeprism graph -- myproject` |
| 2 | `# {N} files, {N} functions, {N} types, ...` | `# 42 files, 318 functions, 56 types, 23 enums, 89 constants, 44 tests, 46 externals` |
| 3 | `# languages: {csv}` | `# languages: zig, rust` |
| 4 | `# generated {ISO-8601}` | `# generated 2026-02-14T10:30:00Z` |
| 5 | `# snapshot: {name} \| source_hash: {hash}` | `# snapshot: main-baseline \| source_hash: a7f3b2c9e1d4` |

Line 5 (snapshot) is optional. It is absent for a plain export
(`zcodeprism export --ctg`) and present for a snapshot.

## Short IDs

Each entity receives a short ID prefixed by its kind. IDs are assigned
sequentially and are stable for a given graph (deterministic).

| Prefix | Kind | Example |
|--------|------|---------|
| `f:` | file | `f:1`, `f:42` |
| `ty:` | type_def (struct, trait, ...) | `ty:1`, `ty:15` |
| `un:` | union_def | `un:1`, `un:3` |
| `en:` | enum_def | `en:1`, `en:8` |
| `fn:` | function (including methods) | `fn:1`, `fn:318` |
| `c:` | constant | `c:1`, `c:89` |
| `err:` | error_def | `err:1`, `err:5` |
| `t:` | test_def | `t:1`, `t:44` |
| `m:` | module | `m:1`, `m:3` |
| `x:` | external (phantom) | `x:1`, `x:12` |

IDs are assigned in parsing order: files sorted alphabetically by path,
then entities within each file sorted by line number. External IDs
(`x:`) are assigned in order of first encounter.

## Node Sections

### `[files]`

One file per line.

```
f:{id} {relative_path} {lines}L
```

Example:

```
[files]
f:1 src/main.zig 245L
f:2 src/parser/tokenizer.zig 312L
f:3 src/parser/visitor.zig 189L
```

Sorted by path (alphabetical).

### `[types]`

A type definition (struct, trait, opaque) occupies 1 to 3 lines: the
header, then optionally a fields line (names only), then optionally a
methods line (names only). Fields and methods lines are indented with 2
spaces.

```
ty:{id} {Name} f:{file_id}:{line} {visibility}
  .{field1} .{field2} ...
  fn {method1}() fn {method2}() ...
```

The fields line is omitted when the type has no fields. The methods line
is omitted when the type has no methods. Field types, method parameters,
and return types are not shown.

Example:

```
[types]
ty:1 Tokenizer f:2:5 pub
  .source .index .line
  fn init() fn next() fn peek()

ty:2 Graph f:4:12 pub
  .nodes .edges
  fn addNode() fn getNode()
```

Rules:
- Sorted by `ty:{id}` ascending
- Fields listed before methods
- Fields prefixed with `.` (name only), separated by spaces
- Methods prefixed with `fn` (name only), separated by spaces
- Visibility: `pub` or omitted (private by default)

### `[unions]`

Same format as `[types]`. Unions can contain fields and methods.

```
un:{id} {Name} f:{file_id}:{line} {visibility}
  .{field1} .{field2} ...
  fn {method1}() fn {method2}() ...
```

### `[enums]`

One enum per line. Variants are omitted.

```
en:{id} {Name} f:{file_id}:{line} {visibility}
```

### `[functions]`

Top-level functions only (methods are shown under their parent type in
`[types]` or `[unions]`). Name only, no parameters or return type.

```
fn:{id} {name}() f:{file_id}:{line} {visibility}
```

Rules:
- Sorted by `fn:{id}` ascending
- Visibility: `pub` or omitted

### `[constants]`

Top-level constants and static variables. Name only.

```
c:{id} {name} f:{file_id}:{line} {visibility}
```

### `[errors]`

Error sets and error definitions. Name only.

```
err:{id} {Name} f:{file_id}:{line}
```

### `[tests]`

Test blocks. Included only when the `include_test_nodes` filter is
enabled (CLI flag: `--test-nodes`).

```
t:{id} "{test_name}" f:{file_id}:{line} {lines}L
```

### `[externals]`

External dependencies and stdlib types referenced by the project. These
are phantom nodes: they are not parsed from source but created when an
edge targets a symbol outside the project. Included only when the
`include_external_nodes` filter is enabled (CLI flag:
`--external-nodes`).

```
x:{id} {qualified_name} ({source}) [{version}]
  {symbol_path}
  {symbol_path}
```

Where `{source}` is `stdlib` or `dependency`.

Example:

```
[externals]
x:1 std (stdlib)
  mem.Allocator
  fs.Dir
  ArrayList

x:2 tree_sitter (dependency) v0.24.0
  Parser
  Tree
  Node
```

Rules:
- Sorted by `x:{id}` ascending
- Symbols indented, sorted alphabetically
- Version is optional (absent for stdlib)

## Edge Section

### `[edges]`

All relationships in a single section. Edges are grouped by (source,
type): one line lists all targets for the same source and type.

```
{source_id} {edge_type} {target1} [{target2} ...]
```

Edge types:

| Type | From | To | Meaning |
|------|------|----|---------|
| `accesses_field` | function/test | field | Reads a field on a typed value |
| `calls` | function | function | Calls in body |
| `contains` | directory/module | file/directory | Contains child |
| `exports` | file | function/type/... | Publicly exposes |
| `implements` | type_def | type_def | Implements trait/interface |
| `imports` | file | file | Imports via `@import` or `use` |
| `similar_to` | function | function | Structural similarity above threshold |
| `uses_type` | function/constant | type/enum/union/error/external | References the type |

Example:

```
[edges]
fn:1 calls fn:2
fn:2 calls fn:3 fn:5
f:1 imports f:2 f:3
fn:2 uses_type ty:1 ty:2 en:1 x:1:mem.Allocator
```

Rules:
- Sorted by edge type (alphabetical), then by source ID, then targets
  sorted by destination ID
- Function IDs (`fn:`) refer to both top-level functions and methods
- External targets use the form `x:{id}:{symbol_path}`

## Filtering

Two boolean filters control which nodes appear. Both default to false.

| Filter | Default | CLI flag | Effect when false |
|--------|---------|----------|-------------------|
| `include_test_nodes` | false | `--test-nodes` | `[tests]` section omitted, test edges omitted |
| `include_external_nodes` | false | `--external-nodes` | `[externals]` section omitted, external edges omitted |

An optional depth limit restricts how many nesting levels below file
nodes are rendered. Null means unlimited.

## Multi-language

All entities from all languages share a single CTG file. The language is
implicit from the file extension of the referenced source file. Language-
specific concepts (Rust traits, Zig comptime) appear in the appropriate
section based on their core kind mapping.

## Sorting Rules (Determinism)

1. Files: sorted by path (alphabetical)
2. Types, unions, enums, functions, constants, errors, tests: sorted by
   numeric ID ascending
3. Externals: sorted by `x:{id}` ascending, symbols sorted alphabetically
4. Edges: sorted by edge type (alphabetical), then source ID, then
   target IDs within a group

IDs are assigned during parsing: files sorted by path, then entities
within each file sorted by line number.

## Full Example

```
# zcodeprism graph -- zcodeprism
# 3 files, 5 functions, 2 types, 0 unions, 1 enums, 2 constants, 1 tests, 2 externals
# languages: zig
# generated 2026-02-14T10:30:00Z
# snapshot: main-baseline | source_hash: a7f3b2c9e1d4

[files]
f:1 src/main.zig 45L
f:2 src/parser/tokenizer.zig 312L
f:3 src/core/graph.zig 189L

[types]
ty:1 Tokenizer f:2:5 pub
  .source .index .line
  fn init() fn next()

ty:2 Graph f:3:12 pub
  .nodes .edges
  fn addNode() fn getNode()

[enums]
en:1 TokenKind f:2:30 pub

[functions]
fn:1 main() f:1:10 pub
fn:2 parseFile() f:1:20 pub

[constants]
c:1 max_node_capacity f:3:1 pub
c:2 version f:1:1 pub

[errors]
err:1 ParseError f:2:90

[tests]
t:1 "tokenizer handles empty input" f:2:280 12L

[externals]
x:1 std (stdlib)
  mem.Allocator
  fs.Dir
  ArrayList

x:2 tree_sitter (dependency) v0.24.0
  Parser
  Tree

[edges]
fn:1 calls fn:2
fn:2 calls fn:3 fn:4
f:1 imports f:2 f:3
fn:2 uses_type ty:1 ty:2 en:1 err:1 x:1:mem.Allocator x:2:Parser
```
