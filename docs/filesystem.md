# Filesystem Layout

ZCodePrism creates files and directories at the project root. This
document describes what is created, by which command, and how it should
be managed.

## Overview

ZCodePrism operates in two modes:

| Mode | Description |
|------|-------------|
| Single-project | One project indexed. Default mode. |
| Workspace | Multiple projects assembled into a unified graph at serve time. |

## Single-Project Layout

```
{project_root}/
  .zcodeprism.zon                  <- project configuration
  .zcodeprism/                     <- data directory
    graph.bin                      <- binary graph (when format = binary)
    graph.jsonl                    <- JSONL graph (when format = jsonl)
    snapshots/                     <- named snapshots
      {tag}.bin                    <- snapshot in binary format
      ...
```

| Artifact | Type | Created by | Versioned |
|----------|------|------------|-----------|
| `.zcodeprism.zon` | file | `zcodeprism init` | yes |
| `.zcodeprism/` | directory | `zcodeprism init` | no |
| `graph.bin` | file | `zcodeprism index` | no |
| `graph.jsonl` | file | `zcodeprism index` | no |
| `snapshots/{tag}.bin` | file | `zcodeprism snapshot` | no |

Only one of `graph.bin` or `graph.jsonl` is created, depending on the
`storage.format` setting.

## Workspace Layout

```
{workspace_root}/
  zcodeprism-workspace.zon         <- workspace configuration
  frontend/                        <- project 1
    .zcodeprism.zon
    .zcodeprism/
    src/
  backend-api/                     <- project 2
    .zcodeprism.zon
    .zcodeprism/
    src/
  shared-lib/                      <- project 3
    .zcodeprism.zon
    .zcodeprism/
    src/
```

Each project keeps its own configuration and data. The workspace file
only references them. Indexing is per-project; workspace assembly
happens at serve time.

## File Lifecycle

### Creation

| Command | Creates |
|---------|---------|
| `zcodeprism init` | `.zcodeprism.zon` + `.zcodeprism/` (empty directory) |
| `zcodeprism init --workspace` | `zcodeprism-workspace.zon` only (no project config, no data dir) |
| `zcodeprism index` | `graph.bin` or `graph.jsonl` |
| `zcodeprism snapshot --name X` | `.zcodeprism/snapshots/X.bin` |

### Updates

| Command | Behavior |
|---------|----------|
| `zcodeprism index` | Overwrites the graph file |
| `zcodeprism serve` | Re-indexes automatically on file changes (built-in watcher) |

### Deletion

ZCodePrism never deletes user-created files. Snapshots, exports, and
configuration files are never removed automatically.

## Exports

Export commands write to stdout by default, or to a file with
`--output`. Exported files are not managed by ZCodePrism.

| Command | Extension | Format |
|---------|-----------|--------|
| `zcodeprism export --ctg` | `.ctg` | Compact Text Graph |
| `zcodeprism export --mermaid` | `.mmd` | Mermaid flowchart |
| `zcodeprism export --jsonl` | `.jsonl` | JSON Lines |

## Snapshots

Snapshots are saved in binary format under
`.zcodeprism/snapshots/{tag}.bin`.

### Tag Rules

- Allowed characters: `[a-zA-Z0-9_-]`
- Maximum length: 64 characters
- Case-sensitive

### Retention

Snapshots are never deleted automatically. The user manages them
manually.

### Exporting a Snapshot

To view a snapshot in a readable format:

```
zcodeprism export --ctg --snapshot main-baseline
zcodeprism export --mermaid --snapshot before-refactor
```

## Recommended `.gitignore`

```gitignore
# ZCodePrism data (regenerable via zcodeprism index)
.zcodeprism/
```

The configuration files `.zcodeprism.zon` and
`zcodeprism-workspace.zon` should NOT be gitignored. They are shared
project settings.

## Indicative Sizes

| Project Size | Files | Nodes | `graph.bin` | `graph.jsonl` |
|-------------|-------|-------|-------------|---------------|
| Small (< 50 files) | ~50 | ~500 | ~100 KB | ~200 KB |
| Medium (~200 files) | ~200 | ~5,000 | ~1 MB | ~3 MB |
| Large (~1,000 files) | ~1,000 | ~30,000 | ~8 MB | ~20 MB |

The binary format is roughly 3-5x more compact than JSONL thanks to
the shared string table and fixed-size records.
