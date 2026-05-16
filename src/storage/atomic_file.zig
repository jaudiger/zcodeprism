const std = @import("std");

fn syncParentDir(io: std.Io, dir: std.Io.Dir, sub_path: []const u8) !void {
    const parent = std.fs.path.dirname(sub_path) orelse ".";
    var parent_dir = try dir.openDir(io, parent, .{ .iterate = true });
    defer parent_dir.close(io);
    const dir_file = std.Io.File{
        .handle = parent_dir.handle,
        .flags = .{ .nonblocking = false },
    };
    try dir_file.sync(io);
}

/// Wraps an atomic file replacement with a buffered writer interface.
///
/// The caller supplies the buffer. `commit` flushes the writer, fsyncs the
/// temp file, renames it into place, then fsyncs the parent directory.
/// `deinit` cleans up the temp on error paths.
pub const AtomicWriter = struct {
    af: std.Io.File.Atomic,
    file_writer: std.Io.File.Writer,
    dir: std.Io.Dir,
    sub_path: []const u8,

    pub fn init(io: std.Io, dir: std.Io.Dir, sub_path: []const u8, buffer: []u8) !AtomicWriter {
        const af = try dir.createFileAtomic(io, sub_path, .{ .replace = true });
        return .{
            .af = af,
            .file_writer = af.file.writer(io, buffer),
            .dir = dir,
            .sub_path = sub_path,
        };
    }

    pub fn deinit(self: *AtomicWriter, io: std.Io) void {
        self.af.deinit(io);
    }

    pub fn writer(self: *AtomicWriter) *std.Io.Writer {
        return &self.file_writer.interface;
    }

    pub fn commit(self: *AtomicWriter, io: std.Io) !void {
        try self.file_writer.interface.flush();
        try self.af.file.sync(io);
        try self.af.replace(io);
        try syncParentDir(io, self.dir, self.sub_path);
    }
};

/// Write `data` to `sub_path` within `dir` atomically and durably.
///
/// Opens a temp file, writes all bytes, fsyncs the file, renames it into place,
/// then fsyncs the parent directory.
pub fn writeAtomic(io: std.Io, dir: std.Io.Dir, sub_path: []const u8, data: []const u8) !void {
    var af = try dir.createFileAtomic(io, sub_path, .{ .replace = true });
    defer af.deinit(io);
    try af.file.writeStreamingAll(io, data);
    try af.file.sync(io);
    try af.replace(io);
    try syncParentDir(io, dir, sub_path);
}

test "writeAtomic creates new file with exact contents" {
    // Arrange
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const payload = "hello atomic";

    // Act
    try writeAtomic(std.testing.io, tmp.dir, "out.txt", payload);

    // Assert
    var buf: [64]u8 = undefined;
    const got = try tmp.dir.readFile(std.testing.io, "out.txt", &buf);
    try std.testing.expectEqualStrings(payload, got);
}

test "writeAtomic replaces existing file" {
    // Arrange
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try writeAtomic(std.testing.io, tmp.dir, "out.txt", "first");

    // Act
    try writeAtomic(std.testing.io, tmp.dir, "out.txt", "second");

    // Assert
    var buf: [64]u8 = undefined;
    const got = try tmp.dir.readFile(std.testing.io, "out.txt", &buf);
    try std.testing.expectEqualStrings("second", got);
}

test "AtomicWriter.commit produces destination file" {
    // Arrange
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var write_buf: [256]u8 = undefined;

    // Act
    var aw = try AtomicWriter.init(std.testing.io, tmp.dir, "streamed.txt", &write_buf);
    defer aw.deinit(std.testing.io);
    try aw.writer().writeAll("streamed content");
    try aw.commit(std.testing.io);

    // Assert
    var read_buf: [64]u8 = undefined;
    const got = try tmp.dir.readFile(std.testing.io, "streamed.txt", &read_buf);
    try std.testing.expectEqualStrings("streamed content", got);
}

test "AtomicWriter.deinit without commit leaves no file" {
    // Arrange
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var write_buf: [256]u8 = undefined;

    // Act
    {
        var aw = try AtomicWriter.init(std.testing.io, tmp.dir, "ghost.txt", &write_buf);
        aw.deinit(std.testing.io);
    }

    // Assert
    const result = tmp.dir.access(std.testing.io, "ghost.txt", .{});
    try std.testing.expectError(error.FileNotFound, result);
}
