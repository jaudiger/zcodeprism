# ZCodePrism

A [Zig](https://ziglang.org) library, CLI, and MCP server that ingests a local codebase, builds a semantic code graph, enriches it via LSP, and exposes it for exploration by LLMs and humans.

The library is the core. The CLI and MCP server are thin consumers. The MCP server is read-only, exposing only read queries on the graph. All mutations (indexing, snapshots, diffs) go through the CLI.

## Getting Started

### Build

```sh
zig build
```

### Test

```sh
zig build test
```

To generate test coverage with [kcov](https://github.com/SimonKagworths/kcov):

```sh
zig build test -Dcoverage=true
```

Results are written to `kcov-output/`.

## CLI

The `zcodeprism` binary is the main entry point for indexing and inspecting a codebase.

```
Usage: zcodeprism <command> [options]
```

### Commands

**`zcodeprism init`** creates a `.zcodeprism.zon` configuration file and a
`.zcodeprism/` data directory in the current working directory. Use `--force`
to overwrite an existing configuration. Pass `--workspace` to create a
`zcodeprism-workspace.zon` template instead.

```sh
cd my-project
zcodeprism init

# or for a workspace
zcodeprism init --workspace
```

**`zcodeprism index`** parses all supported source files (`.zig`, `.rs`) under
the current directory, builds the semantic code graph, and saves it to
`.zcodeprism/graph.bin` (and optionally `.zcodeprism/graph.jsonl` depending on
the `storage.format` setting). Pass `--full` for a complete re-index.

```sh
zcodeprism index --full
```

**`zcodeprism export`** renders the persisted graph in a chosen format and
writes it to stdout (or to a file with `--output`).

```sh
# Compact Text Graph to stdout
zcodeprism export --ctg

# Mermaid flowchart restricted to the parser subtree
zcodeprism export --mermaid --scope src/parser/ --output parser.mmd

# JSONL (one JSON object per line, diffable with git)
zcodeprism export --jsonl --output graph.jsonl
```

| Flag | Description |
|------|-------------|
| `--ctg` | Compact Text Graph format |
| `--mermaid` | Mermaid flowchart format |
| `--jsonl` | One JSON line per node/edge |
| `--scope <path>` | Restrict output to nodes whose file path starts with the given prefix |
| `--output <path>` | Write to a file instead of stdout |
| `--test-nodes` | Include test nodes (excluded by default) |
| `--external-nodes` | Include external/phantom nodes (excluded by default) |

**`zcodeprism snapshot`** saves a named snapshot of the current graph as a
binary file under `.zcodeprism/snapshots/<tag>.bin`. Tags must be
alphanumeric, hyphens, or underscores (max 64 characters).

```sh
zcodeprism snapshot --name v1
```

To export a snapshot in any format, use `export --snapshot`:

```sh
zcodeprism export --ctg --snapshot v1
```

**`zcodeprism diff`** computes a semantic diff between two named snapshots.
It detects added, removed, modified (changed structural hash), and renamed
(same structural hash, different name) entities.

```sh
zcodeprism diff v1 v2
```

Example output:

```
summary: +1 added, -1 removed, ~1 modified, >0 renamed
  + function newFunc (main.zig:16)
  - function oldFunc (main.zig:10)
  ~ function parse (parser.zig:5)
```

**`zcodeprism serve`** starts the MCP server (read-only, JSON-RPC 2.0 over
stdio). The server exposes tools across the `graph.*`, `explorer.*`, and
`analysis.*` namespaces. Pass `--workspace` to serve a unified graph assembled
from multiple projects.

```sh
zcodeprism serve

# serve a workspace (multiple projects as one graph)
zcodeprism serve --workspace zcodeprism-workspace.zon
```

**`zcodeprism status`** loads the persisted graph and prints statistics: node
counts by kind, edge count, and a `source_hash` fingerprint derived from file
content hashes. Pass `--workspace` for workspace-level stats.

```sh
zcodeprism status

# workspace stats
zcodeprism status --workspace zcodeprism-workspace.zon
```

### Global options

| Option | Description |
|--------|-------------|
| `--version` | Print version and exit |
| `--help` | Show usage help |
| `--project-root <path>` | Set the project root directory |
| `--name <tag>` | Snapshot tag name (with `snapshot`) |
| `--snapshot <tag>` | Load a snapshot instead of the current graph (with `export`) |
| `--workspace <path>` | Workspace config file (with `init`, `serve`, `status`) |
| `-v`, `-vv`, `-vvv` | Increase log verbosity (info, debug, trace) |

### Configuration

Running `zcodeprism init` generates a `.zcodeprism.zon` file with sensible
defaults. All fields are optional.

```zig
.{
    .exclude_paths = .{ "zig-cache", "zig-out", ".git", "target" },
    .storage = .{
        .path = ".zcodeprism/",
        .format = .binary,   // .binary or .jsonl
    },
    .memory = .{
        .budget_mb = 512,
    },
}
```

### Workspace Configuration

A workspace groups multiple independently-indexed projects into a single
unified graph. Running `zcodeprism init --workspace` generates a
`zcodeprism-workspace.zon` file. Each referenced project must have its own
`.zcodeprism.zon` and be indexed separately.

```zig
.{
    .name = "my-workspace",
    .projects = .{
        .{ .name = "frontend", .path = "frontend/" },
        .{ .name = "backend", .path = "backend/" },
    },
}
```

Project names must be unique, at most 64 characters, and must not contain `:`.
The `.path` field is relative to the workspace file directory. Use `"."` to
reference the same directory.

### Debug Tools

| Command | Description |
|---------|-------------|
| `zig build analyze-graph -- <dir> <cmd>` | Run analysis algorithms on an indexed directory |
| `zig build dump-ast -- <file>` | Dump the raw tree-sitter AST for a source file |
| `zig build parse-directory -- <dir>` | Index all supported source files and dump the full code graph |
| `zig build parse-file -- <file>` | Parse a source file and dump the semantic graph |
| `zig build query-graph -- <dir> <cmd>` | Exercise the query engine on an indexed directory |
| `zig build render-graph -- <path>` | Render the code graph in CTG or Mermaid format |
