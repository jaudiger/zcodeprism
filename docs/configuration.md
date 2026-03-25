# Configuration

ZCodePrism uses two optional configuration files, both in Zig Object
Notation (ZON) format.

## Project Configuration (`.zcodeprism.zon`)

Created by `zcodeprism init` at the project root. All fields are
optional. The file itself is optional: ZCodePrism uses sensible defaults
without it.

This file should be committed to version control.

### Format

```zig
.{
    .exclude_paths = .{ "zig-cache", "zig-out", ".git", "target" },
    .storage = .{
        .path = ".zcodeprism/",
        .format = .binary,
    },
    .memory = .{
        .budget_mb = 512,
    },
}
```

### Fields

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `exclude_paths` | tuple of strings | `("zig-cache", "zig-out", ".git", "target")` | Paths excluded from indexing |
| `languages` | tuple of enums | auto-detect by file extension | Languages to enable: `.zig`, `.rust` |
| `lsp.zls_path` | optional string | `null` (search in PATH) | Path to the ZLS binary |
| `lsp.rust_analyzer_path` | optional string | `null` (search in PATH) | Path to the rust-analyzer binary |
| `storage.path` | string | `".zcodeprism/"` | Data directory (relative to project root) |
| `storage.format` | enum | `.binary` | Storage format: `.binary` or `.jsonl` |
| `memory.budget_mb` | integer | `512` | Memory budget in megabytes |

### Storage Format

| Value | File Created | Characteristics |
|-------|-------------|-----------------|
| `.binary` | `graph.bin` | Fast loading via memory mapping |
| `.jsonl` | `graph.jsonl` | Text-based, diffable with git |

## Workspace Configuration (`zcodeprism-workspace.zon`)

An optional file that references multiple projects for unified querying.
Created manually by the user. Should be committed to version control.

When absent, ZCodePrism operates in single-project mode.

### Format

```zig
.{
    .name = "my-platform",
    .projects = .{
        .{ .name = "frontend", .path = "frontend/" },
        .{ .name = "backend", .path = "backend-api/" },
        .{ .name = "shared", .path = "shared-lib/" },
    },
}
```

### Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `name` | optional string | no | Workspace display name (defaults to directory name) |
| `projects` | tuple of project entries | yes | Projects in the workspace |

### Project Entry Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `name` | string | yes | Unique project identifier, used as ID prefix and scope value |
| `path` | string | yes | Path to project directory, relative to the workspace file |

### Constraints

- Each project `name` must be unique within the workspace
- Names may contain `[a-zA-Z0-9_-]`, max 32 characters
- Names must not contain `:` (reserved as ID separator)
- Two projects cannot share the same resolved path
- `path = "."` is valid (project colocated with workspace file)
- Each project is indexed independently; the workspace only assembles
  graphs at serve time

### Colocation

The workspace file and a project config can coexist in the same
directory. This is natural when a project wants to include itself in a
workspace alongside its dependencies.

```
my-project/
  .zcodeprism.zon
  zcodeprism-workspace.zon
  .zcodeprism/
  src/
```

```zig
.{
    .name = "my-project",
    .projects = .{
        .{ .name = "my-project", .path = "." },
    },
}
```

| File | Role | Used by |
|------|------|---------|
| `.zcodeprism.zon` | Project indexing configuration | `zcodeprism index` |
| `zcodeprism-workspace.zon` | Multi-project assembly | `zcodeprism serve --workspace` |
