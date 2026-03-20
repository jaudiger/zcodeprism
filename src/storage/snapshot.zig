const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const binary = @import("binary.zig");

const Graph = graph_mod.Graph;
const FrozenGraph = graph_mod.FrozenGraph;

pub const SnapshotError = error{
    InvalidTagName,
    TagTooLong,
    SnapshotNotFound,
};

pub const MAX_TAG_LENGTH: usize = 64;

/// Only alphanumeric, hyphen, underscore. Non-empty, at most MAX_TAG_LENGTH.
pub fn validateTag(tag: []const u8) SnapshotError!void {
    if (tag.len == 0) return error.InvalidTagName;
    if (tag.len > MAX_TAG_LENGTH) return error.TagTooLong;
    for (tag) |c| {
        switch (c) {
            'a'...'z', 'A'...'Z', '0'...'9', '-', '_' => {},
            else => return error.InvalidTagName,
        }
    }
}

/// Save graph as binary under storage_path/snapshots/<tag>.bin.
pub fn saveSnapshot(
    allocator: std.mem.Allocator,
    fg: FrozenGraph,
    tag: []const u8,
    storage_path: []const u8,
) !void {
    try validateTag(tag);

    var base = std.fs.cwd().openDir(storage_path, .{}) catch |err| return err;
    defer base.close();
    base.makeDir("snapshots") catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    var path_buf: std.ArrayList(u8) = .{};
    defer path_buf.deinit(allocator);
    try path_buf.appendSlice(allocator, storage_path);
    try path_buf.append(allocator, '/');
    try path_buf.appendSlice(allocator, "snapshots/");
    try path_buf.appendSlice(allocator, tag);
    try path_buf.appendSlice(allocator, ".bin");

    try binary.save(allocator, fg, path_buf.items);
}

/// Load a snapshot graph by tag from storage_path/snapshots/<tag>.bin.
pub fn loadSnapshotGraph(
    allocator: std.mem.Allocator,
    tag: []const u8,
    storage_path: []const u8,
) !Graph {
    try validateTag(tag);

    var path_buf: std.ArrayList(u8) = .{};
    defer path_buf.deinit(allocator);
    try path_buf.appendSlice(allocator, storage_path);
    try path_buf.append(allocator, '/');
    try path_buf.appendSlice(allocator, "snapshots/");
    try path_buf.appendSlice(allocator, tag);
    try path_buf.appendSlice(allocator, ".bin");

    return binary.load(allocator, path_buf.items) catch |err| switch (err) {
        error.FileNotFound => return error.SnapshotNotFound,
        else => return err,
    };
}

/// 12-hex-char hash of all file content_hashes in the graph. Order-independent.
pub fn computeSourceHash(fg: FrozenGraph) [12]u8 {
    const g = fg.graph;
    var combined: u64 = 0;
    for (g.nodes.items) |n| {
        if (n.kind != .file) continue;
        var h = std.hash.XxHash3.init(0x7a636f6465707269);
        h.update(n.file_path orelse "");
        h.update(&[_]u8{0});
        if (n.content_hash) |ch| h.update(&ch);
        combined +%= h.final();
    }

    const hex = "0123456789abcdef";
    var result: [12]u8 = undefined;
    var val = combined;
    var i: usize = 12;
    while (i > 0) {
        i -= 1;
        result[i] = hex[@intCast(val & 0xf)];
        val >>= 4;
    }
    return result;
}

test "validateTag accepts valid tags and rejects invalid ones" {
    // Arrange
    const valid_alphanum = "my-snapshot_v2";
    const valid_single = "a";
    const valid_max_len = "a" ** 64;
    const invalid_empty = "";
    const invalid_slash = "foo/bar";
    const invalid_dot = "v1.0";
    const invalid_space = "has space";
    const invalid_too_long = "a" ** 65;

    // Act / Assert: valid tags succeed
    try validateTag(valid_alphanum);
    try validateTag(valid_single);
    try validateTag(valid_max_len);

    // Act / Assert: invalid tags produce expected errors
    try std.testing.expectError(error.InvalidTagName, validateTag(invalid_empty));
    try std.testing.expectError(error.InvalidTagName, validateTag(invalid_slash));
    try std.testing.expectError(error.InvalidTagName, validateTag(invalid_dot));
    try std.testing.expectError(error.InvalidTagName, validateTag(invalid_space));
    try std.testing.expectError(error.TagTooLong, validateTag(invalid_too_long));
}
