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

### Debug Tools

| Command | Description |
|---------|-------------|
| `zig build dump-ast -- <file>` | Dump the raw tree-sitter AST for a source file |
| `zig build parse-file -- <file>` | Parse a source file and dump the semantic graph |
| `zig build parse-directory -- <dir>` | Index all `.zig` files and dump the full code graph |
| `zig build render-graph -- <dir>` | Render the code graph in CTG or Mermaid format |
