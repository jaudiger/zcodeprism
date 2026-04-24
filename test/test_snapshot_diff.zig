const std = @import("std");
const zcodeprism = @import("zcodeprism");

const FrozenGraph = zcodeprism.FrozenGraph;
const Graph = zcodeprism.graph.Graph;
const snapshot_diff = zcodeprism.diff.snapshot_diff;
const metrics_mod = zcodeprism.metrics_mod;

const DiffReport = snapshot_diff.DiffReport;
const ChangeKind = snapshot_diff.ChangeKind;

/// Count entries matching a specific change kind.
fn countByKind(report: *const DiffReport, kind: ChangeKind) usize {
    var count: usize = 0;
    for (report.entries) |e| {
        if (e.change == kind) count += 1;
    }
    return count;
}

/// Find the first entry matching name and change kind.
fn findEntry(report: *const DiffReport, name: []const u8, kind: ChangeKind) ?*const snapshot_diff.DiffEntry {
    for (report.entries) |*e| {
        if (e.change == kind and std.mem.eql(u8, e.name, name)) return e;
    }
    return null;
}

test "detects added and removed functions" {
    // Arrange
    const allocator = std.testing.allocator;

    var ga = Graph.init("proj");
    defer ga.deinit(allocator);
    _ = try ga.addNode(allocator, .{ .id = .root, .name = "main.zig", .kind = .file, .file_path = "main.zig", .content_hash = "aaaaaaaaaaaaaaaa".* });
    _ = try ga.addNode(allocator, .{ .id = .root, .name = "a", .kind = .function, .file_path = "main.zig" });
    _ = try ga.addNode(allocator, .{ .id = .root, .name = "b", .kind = .function, .file_path = "main.zig" });
    _ = try ga.addNode(allocator, .{ .id = .root, .name = "c", .kind = .function, .file_path = "main.zig" });

    var gb = Graph.init("proj");
    defer gb.deinit(allocator);
    _ = try gb.addNode(allocator, .{ .id = .root, .name = "main.zig", .kind = .file, .file_path = "main.zig", .content_hash = "aaaaaaaaaaaaaaaa".* });
    _ = try gb.addNode(allocator, .{ .id = .root, .name = "b", .kind = .function, .file_path = "main.zig" });
    _ = try gb.addNode(allocator, .{ .id = .root, .name = "c", .kind = .function, .file_path = "main.zig" });
    _ = try gb.addNode(allocator, .{ .id = .root, .name = "d", .kind = .function, .file_path = "main.zig" });

    // Act
    const fga = FrozenGraph{ .graph = &ga };
    const fgb = FrozenGraph{ .graph = &gb };
    var report = try snapshot_diff.diffGraphs(allocator, fga, fgb);
    defer report.deinit(allocator);

    // Assert
    try std.testing.expectEqual(@as(usize, 1), report.summary.added);
    try std.testing.expectEqual(@as(usize, 1), report.summary.removed);
    try std.testing.expect(findEntry(&report, "d", .added) != null);
    try std.testing.expect(findEntry(&report, "a", .removed) != null);
}

test "detects modified and renamed functions" {
    // Arrange
    const allocator = std.testing.allocator;

    var ga = Graph.init("proj");
    defer ga.deinit(allocator);
    _ = try ga.addNode(allocator, .{ .id = .root, .name = "main.zig", .kind = .file, .file_path = "main.zig", .content_hash = "aaaaaaaaaaaaaaaa".* });
    _ = try ga.addNode(allocator, .{
        .id = .root,
        .name = "scanChar",
        .kind = .function,
        .file_path = "main.zig",
        .metrics = metrics_mod.Metrics{ .structural_hash = 42 },
    });
    _ = try ga.addNode(allocator, .{
        .id = .root,
        .name = "parse",
        .kind = .function,
        .file_path = "main.zig",
        .line_start = 10,
        .metrics = metrics_mod.Metrics{ .structural_hash = 99 },
    });

    var gb = Graph.init("proj");
    defer gb.deinit(allocator);
    _ = try gb.addNode(allocator, .{ .id = .root, .name = "main.zig", .kind = .file, .file_path = "main.zig", .content_hash = "aaaaaaaaaaaaaaaa".* });
    _ = try gb.addNode(allocator, .{
        .id = .root,
        .name = "scanCharacter",
        .kind = .function,
        .file_path = "main.zig",
        .metrics = metrics_mod.Metrics{ .structural_hash = 42 },
    });
    _ = try gb.addNode(allocator, .{
        .id = .root,
        .name = "parse",
        .kind = .function,
        .file_path = "main.zig",
        .line_start = 10,
        .metrics = metrics_mod.Metrics{ .structural_hash = 77 },
    });

    // Act
    const fga = FrozenGraph{ .graph = &ga };
    const fgb = FrozenGraph{ .graph = &gb };
    var report = try snapshot_diff.diffGraphs(allocator, fga, fgb);
    defer report.deinit(allocator);

    // Assert
    try std.testing.expectEqual(@as(usize, 1), report.summary.renamed);
    try std.testing.expectEqual(@as(usize, 1), report.summary.modified);

    const rename_entry = findEntry(&report, "scanCharacter", .renamed);
    try std.testing.expect(rename_entry != null);
    try std.testing.expectEqualSlices(u8, "scanChar", rename_entry.?.old_name.?);

    try std.testing.expect(findEntry(&report, "parse", .modified) != null);
}

test "detects added and removed files" {
    // Arrange
    const allocator = std.testing.allocator;

    var ga = Graph.init("proj");
    defer ga.deinit(allocator);
    _ = try ga.addNode(allocator, .{ .id = .root, .name = "a.zig", .kind = .file, .file_path = "a.zig", .content_hash = "aaaaaaaaaaaaaaaa".* });
    _ = try ga.addNode(allocator, .{ .id = .root, .name = "b.zig", .kind = .file, .file_path = "b.zig", .content_hash = "bbbbbbbbbbbbbbbb".* });

    var gb = Graph.init("proj");
    defer gb.deinit(allocator);
    _ = try gb.addNode(allocator, .{ .id = .root, .name = "b.zig", .kind = .file, .file_path = "b.zig", .content_hash = "bbbbbbbbbbbbbbbb".* });
    _ = try gb.addNode(allocator, .{ .id = .root, .name = "c.zig", .kind = .file, .file_path = "c.zig", .content_hash = "cccccccccccccccc".* });

    // Act
    const fga = FrozenGraph{ .graph = &ga };
    const fgb = FrozenGraph{ .graph = &gb };
    var report = try snapshot_diff.diffGraphs(allocator, fga, fgb);
    defer report.deinit(allocator);

    // Assert
    try std.testing.expect(findEntry(&report, "c.zig", .added) != null);
    try std.testing.expect(findEntry(&report, "a.zig", .removed) != null);
}

test "diff identical graphs produces zero changes" {
    // Arrange
    const allocator = std.testing.allocator;

    var g = Graph.init("proj");
    defer g.deinit(allocator);
    _ = try g.addNode(allocator, .{ .id = .root, .name = "a.zig", .kind = .file, .file_path = "a.zig", .content_hash = "aaaaaaaaaaaaaaaa".* });
    _ = try g.addNode(allocator, .{ .id = .root, .name = "foo", .kind = .function, .file_path = "a.zig" });

    // Act
    const fg_self = FrozenGraph{ .graph = &g };
    var report = try snapshot_diff.diffGraphs(allocator, fg_self, fg_self);
    defer report.deinit(allocator);

    // Assert
    try std.testing.expectEqual(@as(usize, 0), report.summary.added);
    try std.testing.expectEqual(@as(usize, 0), report.summary.removed);
    try std.testing.expectEqual(@as(usize, 0), report.summary.modified);
    try std.testing.expectEqual(@as(usize, 0), report.summary.renamed);
    try std.testing.expectEqual(@as(usize, 0), report.entries.len);
}

test "diff empty vs populated and populated vs empty" {
    // Arrange
    const allocator = std.testing.allocator;

    var empty = Graph.init("proj");
    defer empty.deinit(allocator);

    var populated = Graph.init("proj");
    defer populated.deinit(allocator);
    _ = try populated.addNode(allocator, .{ .id = .root, .name = "a.zig", .kind = .file, .file_path = "a.zig", .content_hash = "aaaaaaaaaaaaaaaa".* });
    _ = try populated.addNode(allocator, .{ .id = .root, .name = "foo", .kind = .function, .file_path = "a.zig" });

    // Act: empty -> populated = everything added
    const fg_empty = FrozenGraph{ .graph = &empty };
    const fg_pop = FrozenGraph{ .graph = &populated };
    var report1 = try snapshot_diff.diffGraphs(allocator, fg_empty, fg_pop);
    defer report1.deinit(allocator);

    try std.testing.expect(report1.summary.added > 0);
    try std.testing.expectEqual(@as(usize, 0), report1.summary.removed);

    // Act: populated -> empty = everything removed
    var report2 = try snapshot_diff.diffGraphs(allocator, fg_pop, fg_empty);
    defer report2.deinit(allocator);

    try std.testing.expect(report2.summary.removed > 0);
    try std.testing.expectEqual(@as(usize, 0), report2.summary.added);

    // Act: empty -> empty = zero changes
    var report3 = try snapshot_diff.diffGraphs(allocator, fg_empty, fg_empty);
    defer report3.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 0), report3.summary.added);
    try std.testing.expectEqual(@as(usize, 0), report3.summary.removed);
}

test "diff output is deterministic" {
    // Arrange
    const allocator = std.testing.allocator;

    var ga = Graph.init("proj");
    defer ga.deinit(allocator);
    _ = try ga.addNode(allocator, .{ .id = .root, .name = "a.zig", .kind = .file, .file_path = "a.zig", .content_hash = "aaaaaaaaaaaaaaaa".* });
    _ = try ga.addNode(allocator, .{ .id = .root, .name = "foo", .kind = .function, .file_path = "a.zig" });

    var gb = Graph.init("proj");
    defer gb.deinit(allocator);
    _ = try gb.addNode(allocator, .{ .id = .root, .name = "a.zig", .kind = .file, .file_path = "a.zig", .content_hash = "aaaaaaaaaaaaaaaa".* });
    _ = try gb.addNode(allocator, .{ .id = .root, .name = "bar", .kind = .function, .file_path = "a.zig" });

    // Act: diff twice
    const fga = FrozenGraph{ .graph = &ga };
    const fgb = FrozenGraph{ .graph = &gb };
    var report1 = try snapshot_diff.diffGraphs(allocator, fga, fgb);
    defer report1.deinit(allocator);
    var report2 = try snapshot_diff.diffGraphs(allocator, fga, fgb);
    defer report2.deinit(allocator);

    // Render both
    var out1: std.ArrayList(u8) = .empty;
    defer out1.deinit(allocator);
    try snapshot_diff.renderDiffReport(allocator, &report1, &out1);

    var out2: std.ArrayList(u8) = .empty;
    defer out2.deinit(allocator);
    try snapshot_diff.renderDiffReport(allocator, &report2, &out2);

    // Assert: identical output
    try std.testing.expectEqualSlices(u8, out1.items, out2.items);
}
