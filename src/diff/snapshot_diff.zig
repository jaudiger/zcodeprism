const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const types = @import("../core/types.zig");
const node_mod = @import("../core/node.zig");

const Graph = graph_mod.Graph;
const Node = node_mod.Node;
const NodeKind = types.NodeKind;

/// Classification of how an entity changed between two snapshots.
pub const ChangeKind = enum {
    added,
    removed,
    modified,
    renamed,
};

/// Broad category of the changed entity.
pub const EntityKind = enum {
    file,
    function,
    type_def,
    union_def,
    enum_def,
    constant,
    error_def,
    test_def,
};

/// A single change detected between two snapshots.
/// Borrows name/file_path/old_name slices from the input graphs.
pub const DiffEntry = struct {
    change: ChangeKind,
    entity_kind: EntityKind,
    name: []const u8,
    file_path: ?[]const u8,
    line: ?u32,
    old_name: ?[]const u8,
};

/// Aggregate counts by change kind.
pub const DiffSummary = struct {
    added: usize,
    removed: usize,
    modified: usize,
    renamed: usize,
};

/// Complete diff result between two snapshots.
pub const DiffReport = struct {
    summary: DiffSummary,
    entries: []const DiffEntry,
    snapshot_a_name: ?[]const u8,
    snapshot_b_name: ?[]const u8,

    /// Free all memory owned by this report.
    pub fn deinit(self: *DiffReport, allocator: std.mem.Allocator) void {
        allocator.free(self.entries);
    }
};

/// Semantic diff between two graphs. Matches entities by (kind, name, file_path).
/// Detects renames via structural_hash and modifications via hash changes.
/// Both graphs must outlive the returned report (string fields are borrowed).
pub fn diffGraphs(
    allocator: std.mem.Allocator,
    graph_a: *const Graph,
    graph_b: *const Graph,
) !DiffReport {
    var entries: std.ArrayList(DiffEntry) = .{};
    errdefer entries.deinit(allocator);

    var a_entities = try collectEntities(allocator, graph_a);
    defer a_entities.deinit(allocator);

    var b_entities = try collectEntities(allocator, graph_b);
    defer b_entities.deinit(allocator);

    const b_matched = try allocator.alloc(bool, b_entities.items.len);
    defer allocator.free(b_matched);
    @memset(b_matched, false);

    const a_matched = try allocator.alloc(bool, a_entities.items.len);
    defer allocator.free(a_matched);
    @memset(a_matched, false);

    // Match by identity key (kind, name, file_path). Detect modifications.
    for (a_entities.items, 0..) |a, ai| {
        for (b_entities.items, 0..) |b, bi| {
            if (b_matched[bi]) continue;
            if (!entityKeysMatch(a, b)) continue;

            a_matched[ai] = true;
            b_matched[bi] = true;

            if (a.structural_hash != 0 and b.structural_hash != 0 and
                a.structural_hash != b.structural_hash)
            {
                try entries.append(allocator, .{
                    .change = .modified,
                    .entity_kind = a.entity_kind,
                    .name = b.name,
                    .file_path = b.file_path,
                    .line = b.line,
                    .old_name = null,
                });
            }
            break;
        }
    }

    // Rename detection: unmatched pairs with same structural_hash in same file.
    for (a_entities.items, 0..) |a, ai| {
        if (a_matched[ai]) continue;
        if (a.structural_hash == 0) continue;

        for (b_entities.items, 0..) |b, bi| {
            if (b_matched[bi]) continue;
            if (b.structural_hash == 0) continue;
            if (a.entity_kind != b.entity_kind) continue;
            if (a.structural_hash != b.structural_hash) continue;
            if (!optionalStrEql(a.file_path, b.file_path)) continue;

            a_matched[ai] = true;
            b_matched[bi] = true;

            try entries.append(allocator, .{
                .change = .renamed,
                .entity_kind = a.entity_kind,
                .name = b.name,
                .file_path = b.file_path,
                .line = b.line,
                .old_name = a.name,
            });
            break;
        }
    }

    // Remaining unmatched in A are removed, in B are added.
    for (a_entities.items, 0..) |a, ai| {
        if (a_matched[ai]) continue;
        try entries.append(allocator, .{
            .change = .removed,
            .entity_kind = a.entity_kind,
            .name = a.name,
            .file_path = a.file_path,
            .line = a.line,
            .old_name = null,
        });
    }
    for (b_entities.items, 0..) |b, bi| {
        if (b_matched[bi]) continue;
        try entries.append(allocator, .{
            .change = .added,
            .entity_kind = b.entity_kind,
            .name = b.name,
            .file_path = b.file_path,
            .line = b.line,
            .old_name = null,
        });
    }

    // Sort for determinism: change kind, then entity kind, then name.
    std.mem.sort(DiffEntry, entries.items, {}, struct {
        fn lessThan(_: void, x: DiffEntry, y: DiffEntry) bool {
            const xc = @intFromEnum(x.change);
            const yc = @intFromEnum(y.change);
            if (xc != yc) return xc < yc;
            const xk = @intFromEnum(x.entity_kind);
            const yk = @intFromEnum(y.entity_kind);
            if (xk != yk) return xk < yk;
            return std.mem.order(u8, x.name, y.name) == .lt;
        }
    }.lessThan);

    var summary = DiffSummary{ .added = 0, .removed = 0, .modified = 0, .renamed = 0 };
    for (entries.items) |e| {
        switch (e.change) {
            .added => summary.added += 1,
            .removed => summary.removed += 1,
            .modified => summary.modified += 1,
            .renamed => summary.renamed += 1,
        }
    }

    return .{
        .summary = summary,
        .entries = try entries.toOwnedSlice(allocator),
        .snapshot_a_name = null,
        .snapshot_b_name = null,
    };
}

/// Render a diff report as deterministic human-readable text.
pub fn renderDiffReport(
    allocator: std.mem.Allocator,
    report: *const DiffReport,
    out: *std.ArrayList(u8),
) !void {
    try out.appendSlice(allocator, "summary: ");
    try appendCount(allocator, out, "+", report.summary.added);
    try out.appendSlice(allocator, " added, ");
    try appendCount(allocator, out, "-", report.summary.removed);
    try out.appendSlice(allocator, " removed, ");
    try appendCount(allocator, out, "~", report.summary.modified);
    try out.appendSlice(allocator, " modified, ");
    try appendCount(allocator, out, ">", report.summary.renamed);
    try out.appendSlice(allocator, " renamed\n");

    for (report.entries) |e| {
        try out.appendSlice(allocator, switch (e.change) {
            .added => "  + ",
            .removed => "  - ",
            .modified => "  ~ ",
            .renamed => "  > ",
        });
        try out.appendSlice(allocator, @tagName(e.entity_kind));
        try out.append(allocator, ' ');

        if (e.change == .renamed) {
            if (e.old_name) |old| {
                try out.appendSlice(allocator, old);
                try out.appendSlice(allocator, " -> ");
            }
        }

        try out.appendSlice(allocator, e.name);

        if (e.file_path) |fp| {
            try out.appendSlice(allocator, " (");
            try out.appendSlice(allocator, fp);
            if (e.line) |l| {
                try out.append(allocator, ':');
                var num_buf: [20]u8 = undefined;
                const s = std.fmt.bufPrint(&num_buf, "{d}", .{l}) catch unreachable;
                try out.appendSlice(allocator, s);
            }
            try out.append(allocator, ')');
        }

        try out.append(allocator, '\n');
    }
}

// --- internal helpers ---

const CollectedEntity = struct {
    entity_kind: EntityKind,
    name: []const u8,
    file_path: ?[]const u8,
    line: ?u32,
    structural_hash: u64,
};

/// Map NodeKind to EntityKind, returning null for structural nodes not diffed.
fn toEntityKind(kind: NodeKind) ?EntityKind {
    return switch (kind) {
        .file => .file,
        .function => .function,
        .type_def => .type_def,
        .union_def => .union_def,
        .enum_def => .enum_def,
        .constant => .constant,
        .error_def => .error_def,
        .test_def => .test_def,
        .module, .field, .import_decl, .directory, .parameter => null,
    };
}

/// Extract all diffable entities from a graph, skipping phantom/structural nodes.
fn collectEntities(allocator: std.mem.Allocator, g: *const Graph) !std.ArrayList(CollectedEntity) {
    var list: std.ArrayList(CollectedEntity) = .{};
    errdefer list.deinit(allocator);

    for (g.nodes.items) |n| {
        if (n.external != .none) continue;
        const ek = toEntityKind(n.kind) orelse continue;
        const sh: u64 = if (n.metrics) |m| m.structural_hash else 0;
        try list.append(allocator, .{
            .entity_kind = ek,
            .name = n.name,
            .file_path = n.file_path,
            .line = n.line_start,
            .structural_hash = sh,
        });
    }

    return list;
}

/// True when both entities have the same (kind, name, file_path) identity.
fn entityKeysMatch(a: CollectedEntity, b: CollectedEntity) bool {
    if (a.entity_kind != b.entity_kind) return false;
    if (!std.mem.eql(u8, a.name, b.name)) return false;
    return optionalStrEql(a.file_path, b.file_path);
}

/// Null-safe string equality: two nulls are equal, null != non-null.
fn optionalStrEql(a: ?[]const u8, b: ?[]const u8) bool {
    const sa = a orelse return b == null;
    const sb = b orelse return false;
    return std.mem.eql(u8, sa, sb);
}

/// Append a prefixed decimal count like "+3" or "~0" to the output buffer.
fn appendCount(allocator: std.mem.Allocator, out: *std.ArrayList(u8), prefix: []const u8, count: usize) !void {
    try out.appendSlice(allocator, prefix);
    var num_buf: [20]u8 = undefined;
    const s = std.fmt.bufPrint(&num_buf, "{d}", .{count}) catch unreachable;
    try out.appendSlice(allocator, s);
}
