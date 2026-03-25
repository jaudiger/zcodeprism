const std = @import("std");

// Resolve the absolute path to the installed CLI binary.
fn exePath(allocator: std.mem.Allocator) ![]const u8 {
    return std.fs.cwd().realpathAlloc(allocator, "zig-out/bin/zcodeprism");
}

const CliResult = struct {
    stdout_buf: []u8,
    stderr_buf: []u8,
    exit_code: u8,

    fn stdout(self: CliResult) []const u8 {
        return self.stdout_buf;
    }

    fn stderr(self: CliResult) []const u8 {
        return self.stderr_buf;
    }

    fn deinit(self: CliResult, allocator: std.mem.Allocator) void {
        allocator.free(self.stdout_buf);
        allocator.free(self.stderr_buf);
    }
};

// Spawn the CLI binary with the given args, optionally in a specific directory.
fn runCli(
    allocator: std.mem.Allocator,
    bin: []const u8,
    args: []const []const u8,
    cwd_dir: ?std.fs.Dir,
) !CliResult {
    // Arrange argv: binary path followed by user args.
    var argv: std.ArrayList([]const u8) = .{};
    defer argv.deinit(allocator);
    try argv.ensureTotalCapacity(allocator, 1 + args.len);
    argv.appendAssumeCapacity(bin);
    argv.appendSliceAssumeCapacity(args);

    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = argv.items,
        .cwd_dir = cwd_dir,
    });

    const exit_code: u8 = switch (result.term) {
        .Exited => |code| code,
        else => 255,
    };

    return .{
        .stdout_buf = result.stdout,
        .stderr_buf = result.stderr,
        .exit_code = exit_code,
    };
}

test "--version outputs version string" {
    // Arrange
    const allocator = std.testing.allocator;
    const bin = try exePath(allocator);
    defer allocator.free(bin);

    // Act
    const result = try runCli(allocator, bin, &.{"--version"}, null);
    defer result.deinit(allocator);

    // Assert
    try std.testing.expectEqual(@as(u8, 0), result.exit_code);
    try std.testing.expect(std.mem.startsWith(u8, result.stdout(), "zcodeprism "));
}

test "--help exits with code 0" {
    // Arrange
    const allocator = std.testing.allocator;
    const bin = try exePath(allocator);
    defer allocator.free(bin);

    // Act
    const result = try runCli(allocator, bin, &.{"--help"}, null);
    defer result.deinit(allocator);

    // Assert
    try std.testing.expectEqual(@as(u8, 0), result.exit_code);
    try std.testing.expect(std.mem.indexOf(u8, result.stdout(), "Usage") != null);
}

test "unknown command exits with code 2" {
    // Arrange
    const allocator = std.testing.allocator;
    const bin = try exePath(allocator);
    defer allocator.free(bin);

    // Act
    const result = try runCli(allocator, bin, &.{"foobar"}, null);
    defer result.deinit(allocator);

    // Assert
    try std.testing.expectEqual(@as(u8, 2), result.exit_code);
}

test "init creates config file" {
    // Arrange
    const allocator = std.testing.allocator;
    const bin = try exePath(allocator);
    defer allocator.free(bin);
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    // Act
    const result = try runCli(allocator, bin, &.{"init"}, tmp.dir);
    defer result.deinit(allocator);

    // Assert
    try std.testing.expectEqual(@as(u8, 0), result.exit_code);
    const stat = tmp.dir.statFile(".zcodeprism.zon") catch |err| {
        std.debug.print("expected .zcodeprism.zon to exist, got {}\n", .{err});
        return error.TestExpectedEqual;
    };
    try std.testing.expect(stat.size > 0);
}

test "init creates data directory" {
    // Arrange
    const allocator = std.testing.allocator;
    const bin = try exePath(allocator);
    defer allocator.free(bin);
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    // Act
    const result = try runCli(allocator, bin, &.{"init"}, tmp.dir);
    defer result.deinit(allocator);

    // Assert
    try std.testing.expectEqual(@as(u8, 0), result.exit_code);
    var data_dir = tmp.dir.openDir(".zcodeprism", .{}) catch |err| {
        std.debug.print("expected .zcodeprism/ to exist, got {}\n", .{err});
        return error.TestExpectedEqual;
    };
    data_dir.close();
}

test "init --force overwrites existing" {
    // Arrange
    const allocator = std.testing.allocator;
    const bin = try exePath(allocator);
    defer allocator.free(bin);
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const first = try runCli(allocator, bin, &.{"init"}, tmp.dir);
    first.deinit(allocator);

    // Tamper with the config so we can detect overwrite.
    const file = try tmp.dir.createFile(".zcodeprism.zon", .{});
    try file.writeAll("modified");
    file.close();

    // Act
    const result = try runCli(allocator, bin, &.{ "init", "--force" }, tmp.dir);
    defer result.deinit(allocator);

    // Assert
    try std.testing.expectEqual(@as(u8, 0), result.exit_code);
    const content = try tmp.dir.readFileAlloc(allocator, ".zcodeprism.zon", 4096);
    defer allocator.free(content);
    try std.testing.expect(!std.mem.eql(u8, content, "modified"));
}

test "init on already initialized project" {
    // Arrange
    const allocator = std.testing.allocator;
    const bin = try exePath(allocator);
    defer allocator.free(bin);
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const first = try runCli(allocator, bin, &.{"init"}, tmp.dir);
    first.deinit(allocator);

    // Act
    const result = try runCli(allocator, bin, &.{"init"}, tmp.dir);
    defer result.deinit(allocator);

    // Assert
    try std.testing.expect(result.exit_code != 0);
}

test "index on fixture produces output" {
    // Arrange
    const allocator = std.testing.allocator;
    const bin = try exePath(allocator);
    defer allocator.free(bin);
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const init_result = try runCli(allocator, bin, &.{"init"}, tmp.dir);
    init_result.deinit(allocator);

    // Write a minimal .zig file for the indexer.
    const src = try tmp.dir.createFile("hello.zig", .{});
    try src.writeAll("pub fn hello() void {}");
    src.close();

    // Act
    const result = try runCli(allocator, bin, &.{"index"}, tmp.dir);
    defer result.deinit(allocator);

    // Assert
    try std.testing.expectEqual(@as(u8, 0), result.exit_code);
    try std.testing.expect(std.mem.indexOf(u8, result.stdout(), "indexed") != null);
}

test "status on indexed project" {
    // Arrange
    const allocator = std.testing.allocator;
    const bin = try exePath(allocator);
    defer allocator.free(bin);
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const init_result = try runCli(allocator, bin, &.{"init"}, tmp.dir);
    init_result.deinit(allocator);

    const src = try tmp.dir.createFile("hello.zig", .{});
    try src.writeAll("pub fn hello() void {}");
    src.close();

    const idx_result = try runCli(allocator, bin, &.{"index"}, tmp.dir);
    idx_result.deinit(allocator);

    // Act
    const result = try runCli(allocator, bin, &.{"status"}, tmp.dir);
    defer result.deinit(allocator);

    // Assert
    try std.testing.expectEqual(@as(u8, 0), result.exit_code);
    try std.testing.expect(std.mem.indexOf(u8, result.stdout(), "source_hash") != null);
}

test "status on uninitialized project" {
    // Arrange
    const allocator = std.testing.allocator;
    const bin = try exePath(allocator);
    defer allocator.free(bin);
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    // Act
    const result = try runCli(allocator, bin, &.{"status"}, tmp.dir);
    defer result.deinit(allocator);

    // Assert
    try std.testing.expectEqual(@as(u8, 1), result.exit_code);
}
