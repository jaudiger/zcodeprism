const std = @import("std");
const zcodeprism = @import("zcodeprism");

const FrozenGraph = zcodeprism.FrozenGraph;
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
        .content_hash = "abcdef1234567890".*,
    });

    const storage_path = try tmp.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(storage_path);

    // Act
    const fg = try g.freeze(allocator);
    try snapshot.saveSnapshot(allocator, std.testing.io, fg, "v1", storage_path);

    var loaded = try snapshot.loadSnapshotGraph(allocator, std.testing.io, "v1", storage_path);
    defer loaded.deinit(allocator);

    // Assert
    try std.testing.expectEqual(g.nodeCount(), loaded.nodeCount());
    try std.testing.expectEqual(g.edgeCount(), loaded.edgeCount());

    var snap_dir = try tmp.dir.openDir(std.testing.io, "snapshots", .{});
    defer snap_dir.close(std.testing.io);
    const stat = try snap_dir.statFile(std.testing.io, "v1.bin", .{});
    try std.testing.expect(stat.size > 0);
}

test "snapshot rejects invalid tag and loads missing tag" {
    // Arrange
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    var g = Graph.init("test-project");
    defer g.deinit(allocator);

    const storage_path = try tmp.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(storage_path);

    // Act / Assert: invalid tags
    const fg2 = FrozenGraph{ .graph = &g };
    try std.testing.expectError(
        error.InvalidTagName,
        snapshot.saveSnapshot(allocator, std.testing.io, fg2, "has/slash", storage_path),
    );
    try std.testing.expectError(
        error.InvalidTagName,
        snapshot.saveSnapshot(allocator, std.testing.io, fg2, "has space", storage_path),
    );

    // Act / Assert: load nonexistent tag
    try std.testing.expectError(
        error.SnapshotNotFound,
        snapshot.loadSnapshotGraph(allocator, std.testing.io, "nonexistent", storage_path),
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
        .content_hash = "aaaaaaaaaaaaaaaa".*,
    });
    _ = try g1.addNode(allocator, .{
        .id = .root,
        .name = "b.zig",
        .kind = .file,
        .file_path = "src/b.zig",
        .content_hash = "bbbbbbbbbbbbbbbb".*,
    });

    // Act
    const fg1 = FrozenGraph{ .graph = &g1 };
    const hash1a = snapshot.computeSourceHash(fg1);
    const hash1b = snapshot.computeSourceHash(fg1);

    // Assert: deterministic
    try std.testing.expectEqualSlices(u8, &hash1a, &hash1b);

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
        .content_hash = "cccccccccccccccc".*,
    });
    _ = try g2.addNode(allocator, .{
        .id = .root,
        .name = "b.zig",
        .kind = .file,
        .file_path = "src/b.zig",
        .content_hash = "bbbbbbbbbbbbbbbb".*,
    });

    const fg_g2 = FrozenGraph{ .graph = &g2 };
    const hash2 = snapshot.computeSourceHash(fg_g2);

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
        .content_hash = "aaaaaaaaaaaaaaaa".*,
    });
    _ = try g3.addNode(allocator, .{
        .id = .root,
        .name = "b.zig",
        .kind = .file,
        .file_path = "src/b.zig",
        .content_hash = "bbbbbbbbbbbbbbbb".*,
    });

    const fg_g3 = FrozenGraph{ .graph = &g3 };
    const hash3 = snapshot.computeSourceHash(fg_g3);

    // Assert: different file path -> different hash
    try std.testing.expect(!std.mem.eql(u8, &hash1a, &hash3));
}

test "computeSourceHash handles empty graph" {
    // Arrange
    const allocator = std.testing.allocator;
    var g = Graph.init("empty");
    defer g.deinit(allocator);

    // Act
    const fg_empty = FrozenGraph{ .graph = &g };
    const hash = snapshot.computeSourceHash(fg_empty);

    // Assert: returns a 16-byte hash
    try std.testing.expectEqual(@as(usize, 16), hash.len);
}
