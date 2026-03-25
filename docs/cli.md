# CLI Reference

ZCodePrism is a command-line tool for indexing codebases, building
semantic code graphs, and serving them via MCP.

```
zcodeprism <command> [options]
```

## Principles

- The CLI is a thin wrapper over the ZCodePrism library
- All graph mutations (indexing, snapshots, diffs) go through the CLI
- The MCP server is read-only
- Exit codes: `0` = success, `1` = error, `2` = invalid usage
- Errors go to stderr, results go to stdout
- `-v` / `-vv` / `-vvv` increases log verbosity on stderr

## Commands

### `zcodeprism init`

Initializes a ZCodePrism project in the current directory.

Creates:
- `.zcodeprism.zon` (configuration file with defaults)
- `.zcodeprism/` (data directory)

| Flag | Description |
|------|-------------|
| `--force` | Overwrite existing configuration |
| `--workspace <path>` | Also write a workspace config template |

```
zcodeprism init
zcodeprism init --force
```

### `zcodeprism index`

Indexes the project and builds the semantic code graph. Always performs
a full re-index from scratch.

| Flag | Description |
|------|-------------|
| `-v`, `-vv`, `-vvv` | Increase log verbosity |

Output (stdout):
```
indexed 42 files (318 functions, 56 types, 23 enums, 44 tests)
languages: zig
externals: 34 stdlib symbols, 12 dependency symbols (3 packages)
source_hash: a7f3b2c9e1d4
duration: 1.2s
```

```
zcodeprism index
zcodeprism index -v
```

### `zcodeprism status`

Shows the current graph state and statistics.

| Flag | Description |
|------|-------------|
| `--workspace <path>` | Show aggregated status for a workspace |

```
zcodeprism status
zcodeprism status --workspace zcodeprism-workspace.zon
```

### `zcodeprism serve`

Starts the MCP server (JSON-RPC 2.0 over stdio, read-only).

| Flag | Description |
|------|-------------|
| `--watch` | Re-index automatically when source files change |
| `--budget <MB>` | Override memory budget |
| `--workspace <path>` | Load a multi-project workspace |

In single-project mode, the server loads the graph from `.zcodeprism/`
at startup. With `--watch`, it re-indexes on file changes (500ms
debounce).

In workspace mode, the server loads each project's graph and assembles
them under a virtual root node. Node IDs are prefixed with the project
name.

```
zcodeprism serve
zcodeprism serve --watch
zcodeprism serve --workspace zcodeprism-workspace.zon --watch
```

### `zcodeprism export`

Exports the graph in a specified format.

| Flag | Description |
|------|-------------|
| `--ctg` | Compact Text Graph format |
| `--mermaid` | Mermaid flowchart format |
| `--jsonl` | JSON Lines format |
| `--scope <path>` | Restrict export to a path prefix |
| `--output <path>` | Write to file instead of stdout |
| `--snapshot <tag>` | Export a saved snapshot instead of the current graph |
| `--test-nodes` | Include test nodes (excluded by default) |
| `--external-nodes` | Include external/phantom nodes (excluded by default) |
| `-v`, `-vv`, `-vvv` | Increase log verbosity |

Exactly one format flag is required.

```
zcodeprism export --ctg
zcodeprism export --mermaid --scope src/parser/ --output parser.mmd
zcodeprism export --jsonl --output graph.jsonl
zcodeprism export --ctg --snapshot main-baseline
```

### `zcodeprism snapshot`

Saves a named snapshot of the current graph in binary format.

| Flag | Description |
|------|-------------|
| `--name <tag>` | Snapshot tag name (required) |

Tag names: alphanumeric, hyphens, and underscores only. Maximum 64
characters.

Snapshots are stored in `.zcodeprism/snapshots/<tag>.bin`. To export a
snapshot in a readable format, use
`zcodeprism export --ctg --snapshot <tag>`.

```
zcodeprism snapshot --name main-baseline
zcodeprism snapshot --name before-refactor
```

### `zcodeprism diff`

Semantic diff between two snapshots.

Arguments: two snapshot tag names.

| Flag | Description |
|------|-------------|
| `--output <path>` | Write to file instead of stdout |

The diff operates on the binary graph data, comparing entities by
identity key (kind, name, file path). It detects additions, removals,
modifications, and renames (same structural hash, different name).

```
zcodeprism diff before-refactor after-refactor
```

### `zcodeprism --version`

Prints the version string.

```
zcodeprism --version
```

### `zcodeprism --help`

Prints usage information.

```
zcodeprism --help
```

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | Error (indexing failed, file not found, etc.) |
| 2 | Invalid usage (unknown command, missing argument) |
