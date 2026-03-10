const std = @import("std");
const zcodeprism = @import("zcodeprism");

const Graph = zcodeprism.graph.Graph;
const snapshot = zcodeprism.storage.snapshot;

test "snapshot save and load round-trip" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    var g = Graph.init("test-project");
    defer g.deinit(allocator);

    try g.addOwnedBuffer(allocator, try allocator.dupe(u8, "fake source a"));
    _ = try g.addNode(allocator, .{
        .id = .root,
        .name = "a.zig",
        .kind = .file,
        .file_path = "src/a.zig",
        .content_hash = "abcdef123456".*,
    });

    const storage_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(storage_path);

    // Act
    try snapshot.saveSnapshot(allocator, &g, "v1", storage_path);

    var loaded = try snapshot.loadSnapshotGraph(allocator, "v1", storage_path);
    defer loaded.deinit(allocator);

    // Assert
    try std.testing.expectEqual(g.nodeCount(), loaded.nodeCount());
    try std.testing.expectEqual(g.edgeCount(), loaded.edgeCount());

    const snap_dir = try tmp.dir.openDir("snapshots", .{});
    defer @constCast(&snap_dir).close();
    const stat = try snap_dir.statFile("v1.bin");
    try std.testing.expect(stat.size > 0);
}

test "snapshot rejects invalid tag and loads missing tag" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    var g = Graph.init("test-project");
    defer g.deinit(allocator);

    const storage_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(storage_path);

    // Act / Assert: invalid tags
    try std.testing.expectError(
        error.InvalidTagName,
        snapshot.saveSnapshot(allocator, &g, "has/slash", storage_path),
    );
    try std.testing.expectError(
        error.InvalidTagName,
        snapshot.saveSnapshot(allocator, &g, "has space", storage_path),
    );

    // Act / Assert: load nonexistent tag
    try std.testing.expectError(
        error.SnapshotNotFound,
        snapshot.loadSnapshotGraph(allocator, "nonexistent", storage_path),
    );
}

test "computeSourceHash is deterministic and content-sensitive" {
    // Arrange
    const allocator = std.testing.allocator;

    var g1 = Graph.init("proj");
    defer g1.deinit(allocator);
    try g1.addOwnedBuffer(allocator, try allocator.dupe(u8, "src a"));
    try g1.addOwnedBuffer(allocator, try allocator.dupe(u8, "src b"));
    _ = try g1.addNode(allocator, .{
        .id = .root,
        .name = "a.zig",
        .kind = .file,
        .file_path = "src/a.zig",
        .content_hash = "aaaaaaaaaaaa".*,
    });
    _ = try g1.addNode(allocator, .{
        .id = .root,
        .name = "b.zig",
        .kind = .file,
        .file_path = "src/b.zig",
        .content_hash = "bbbbbbbbbbbb".*,
    });

    // Act
    const hash1a = snapshot.computeSourceHash(&g1);
    const hash1b = snapshot.computeSourceHash(&g1);

    // Assert: deterministic
    try std.testing.expectEqualSlices(u8, &hash1a, &hash1b);

    // Assert: all hex characters
    for (&hash1a) |c| {
        try std.testing.expect(std.ascii.isHex(c));
    }

    // Arrange: second graph with different content hash
    var g2 = Graph.init("proj");
    defer g2.deinit(allocator);
    try g2.addOwnedBuffer(allocator, try allocator.dupe(u8, "src a2"));
    try g2.addOwnedBuffer(allocator, try allocator.dupe(u8, "src b2"));
    _ = try g2.addNode(allocator, .{
        .id = .root,
        .name = "a.zig",
        .kind = .file,
        .file_path = "src/a.zig",
        .content_hash = "cccccccccccc".*,
    });
    _ = try g2.addNode(allocator, .{
        .id = .root,
        .name = "b.zig",
        .kind = .file,
        .file_path = "src/b.zig",
        .content_hash = "bbbbbbbbbbbb".*,
    });

    const hash2 = snapshot.computeSourceHash(&g2);

    // Assert: different content -> different hash
    try std.testing.expect(!std.mem.eql(u8, &hash1a, &hash2));

    // Arrange: third graph with same hashes but different file path
    var g3 = Graph.init("proj");
    defer g3.deinit(allocator);
    try g3.addOwnedBuffer(allocator, try allocator.dupe(u8, "src a3"));
    try g3.addOwnedBuffer(allocator, try allocator.dupe(u8, "src b3"));
    _ = try g3.addNode(allocator, .{
        .id = .root,
        .name = "a.zig",
        .kind = .file,
        .file_path = "lib/a.zig",
        .content_hash = "aaaaaaaaaaaa".*,
    });
    _ = try g3.addNode(allocator, .{
        .id = .root,
        .name = "b.zig",
        .kind = .file,
        .file_path = "src/b.zig",
        .content_hash = "bbbbbbbbbbbb".*,
    });

    const hash3 = snapshot.computeSourceHash(&g3);

    // Assert: different file path -> different hash
    try std.testing.expect(!std.mem.eql(u8, &hash1a, &hash3));
}

test "computeSourceHash handles empty graph" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = Graph.init("empty");
    defer g.deinit(allocator);

    // Act
    const hash = snapshot.computeSourceHash(&g);

    // Assert: valid 12-char hex string
    try std.testing.expectEqual(@as(usize, 12), hash.len);
    for (&hash) |c| {
        try std.testing.expect(std.ascii.isHex(c));
    }
}
