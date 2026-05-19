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

## Global Flags

| Flag | Description |
|------|-------------|
| `--project-root <path>` | Run as if invoked from this directory (chdir before dispatch) |
| `-v`, `-vv`, `-vvv` | Increase log verbosity on stderr |

## Commands

### `zcodeprism init`

Initializes a ZCodePrism project in the current directory.

Without `--workspace`, creates:
- `.zcodeprism.zon` (configuration file with defaults)
- `.zcodeprism/` (empty data directory)

With `--workspace`, writes `zcodeprism-workspace.zon` only. The two
modes are mutually exclusive: workspace-template init does not create
`.zcodeprism.zon` or the data directory.

| Flag | Description |
|------|-------------|
| `--force` | Overwrite the file that would be created |
| `--workspace <path>` | Write a workspace template instead of a project config (argument is accepted but currently ignored) |

```
zcodeprism init
zcodeprism init --force
zcodeprism init --workspace .
```

### `zcodeprism index`

Indexes the project and builds the semantic code graph. Always performs
a full re-index from scratch.

| Flag | Description |
|------|-------------|
| `-v`, `-vv`, `-vvv` | Increase log verbosity |

Output (stdout):
```
indexed 42 files (4218 nodes, 1033 edges)
```

A trailing summary from the LSP enricher is printed when LSP is
enabled.

```
zcodeprism index
zcodeprism index -v
```

### `zcodeprism status`

Loads the saved graph and prints summary counters.

| Flag | Description |
|------|-------------|
| `--workspace <path>` | Show aggregated status for a workspace |

Output (stdout):
```
nodes: 4218 (42 files, 318 functions, 79 types)
edges: 1033
source_hash: a7f3b2c9e1d400112233445566778899
```

```
zcodeprism status
zcodeprism status --workspace zcodeprism-workspace.zon
```

### `zcodeprism serve`

Starts the MCP server (JSON-RPC 2.0 over stdio, read-only).

| Flag | Description |
|------|-------------|
| `--workspace <path>` | Load a multi-project workspace |

The stdio loop opens immediately so the MCP `initialize` handshake
completes without waiting for indexing. The initial graph is empty;
indexing runs in a background thread and a `graph/updated` notification
fires when it lands. Tools called before the first index completes
return empty results.

A file watcher re-indexes automatically on source changes (500ms
debounce). Each re-index emits another `graph/updated` notification.

In single-project mode, the background thread runs the indexer plus
LSP enrichment over the project root.

In workspace mode, the background thread loads each project's saved
graph and assembles them under a virtual root node. Node IDs are
prefixed with the project name.

```
zcodeprism serve
zcodeprism serve --workspace zcodeprism-workspace.zon
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

Arguments: two snapshot tag names. Output is written to stdout.

The diff operates on the binary graph data, comparing entities by
identity key (kind, name, file path). It detects additions, removals,
modifications, and renames (same structural hash, different name).

```
zcodeprism diff before-refactor after-refactor
```

### `zcodeprism --version`

Prints the version string. Also accepted as a subcommand: `zcodeprism version`.

```
zcodeprism --version
```

### `zcodeprism --help`

Prints usage information. `-h` is an accepted alias, and `zcodeprism help`
works as a subcommand. Running `zcodeprism` with no arguments also
prints usage.

```
zcodeprism --help
```

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | Error (indexing failed, file not found, etc.) |
| 2 | Invalid usage (unknown command, missing argument) |
