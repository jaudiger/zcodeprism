//! LSP connection pool: manages long-lived LSP server connections,
//! one per Language, for reuse across enrichment passes.
//!
//! Not thread-safe. Single-threaded usage only.

const std = @import("std");
const client_mod = @import("client.zig");
const lang_support = @import("../languages/language_support.zig");
const graph_mod = @import("../core/graph.zig");
const logging = @import("../logging.zig");
const types = @import("../core/types.zig");

const LspClient = client_mod.LspClient;
const LspConfig = lang_support.LspConfig;
const Graph = graph_mod.Graph;
const Language = types.Language;
const Logger = logging.Logger;
const Field = logging.Field;

pub const State = enum { ready, idle };

/// A single persistent connection to an LSP server.
/// Tracks which files have been opened on the server side
/// so subsequent passes can skip the didOpen/warmup cycle.
pub const LspConnection = struct {
    client: LspClient,
    language: Language,
    state: State,
    /// URI -> version counter. Keys are owned (duped) strings.
    opened_files: std.StringHashMapUnmanaged(i32) = .{},
    last_activity_ns: i128,
    /// Borrowed from the caller; must outlive the connection.
    project_root: []const u8,

    pub fn openFile(self: *LspConnection, allocator: std.mem.Allocator, io: std.Io, uri: []const u8, text: []const u8, language_id: []const u8) !void {
        if (self.opened_files.get(uri)) |version| {
            const new_version = version + 1;
            try self.client.textDocumentDidChange(allocator, io, uri, new_version, text);
            // Key already exists so put only updates the value in-place.
            self.opened_files.put(allocator, uri, new_version) catch {};
        } else {
            const owned_uri = try allocator.dupe(u8, uri);
            errdefer allocator.free(owned_uri);
            try self.client.textDocumentDidOpen(allocator, io, owned_uri, text, language_id);
            try self.opened_files.put(allocator, owned_uri, 1);
        }
        self.touch(io);
    }

    pub fn closeFile(self: *LspConnection, allocator: std.mem.Allocator, io: std.Io, uri: []const u8) void {
        if (self.opened_files.fetchRemove(uri)) |kv| {
            self.client.textDocumentDidClose(allocator, io, uri) catch {};
            allocator.free(kv.key);
        }
    }

    /// Walk file nodes in the graph, read their content, and open each on the server.
    pub fn openAllFiles(self: *LspConnection, allocator: std.mem.Allocator, io: std.Io, graph: *Graph, language_id: []const u8, project_root: []const u8, log: Logger) void {
        for (graph.nodes.items) |n| {
            if (n.kind != .file) continue;
            const file_path = n.file_path orelse continue;

            const abs_path = std.fs.path.join(allocator, &.{ project_root, file_path }) catch continue;
            defer allocator.free(abs_path);

            const source = std.Io.Dir.openFileAbsolute(io, abs_path, .{}) catch {
                log.debug(io, "could not open file", &.{Field.string("path", abs_path)});
                continue;
            };
            defer source.close(io);
            var read_buf: [4096]u8 = undefined;
            var src_reader = source.reader(io, &read_buf);
            const text = src_reader.interface.allocRemaining(allocator, .limited(10 * 1024 * 1024)) catch continue;
            defer allocator.free(text);

            const uri = client_mod.pathToUri(allocator, abs_path) catch continue;
            defer allocator.free(uri);

            self.openFile(allocator, io, uri, text, language_id) catch continue;
        }
    }

    pub fn openedFileCount(self: *const LspConnection) u32 {
        return self.opened_files.count();
    }

    pub fn touch(self: *LspConnection, io: std.Io) void {
        self.last_activity_ns = std.Io.Timestamp.now(io, .awake).nanoseconds;
    }

    pub fn deinit(self: *LspConnection, allocator: std.mem.Allocator, io: std.Io) void {
        var it = self.opened_files.iterator();
        while (it.next()) |entry| {
            allocator.free(entry.key_ptr.*);
        }
        self.opened_files.deinit(allocator);
        self.client.deinit(io);
    }
};

pub const PoolOptions = struct {
    idle_timeout_ns: u64 = 60 * std.time.ns_per_s,
};

/// Manages long-lived LSP server connections, one per Language.
/// Not thread-safe. Single-threaded usage only.
pub const LspPool = struct {
    connections: [lang_count]?*LspConnection = .{null} ** lang_count,
    idle_timeout_ns: u64,

    const lang_count = @typeInfo(Language).@"enum".fields.len;

    pub fn init(options: PoolOptions) LspPool {
        return .{
            .idle_timeout_ns = options.idle_timeout_ns,
        };
    }

    pub fn deinit(self: *LspPool, allocator: std.mem.Allocator, io: std.Io) void {
        for (&self.connections) |*slot| {
            if (slot.*) |conn| {
                conn.deinit(allocator, io);
                allocator.destroy(conn);
                slot.* = null;
            }
        }
    }

    pub fn acquire(
        self: *LspPool,
        allocator: std.mem.Allocator,
        io: std.Io,
        language: Language,
        lsp_config: *const LspConfig,
        project_root: []const u8,
        logger: Logger,
    ) !*LspConnection {
        self.reapIdle(allocator, io);

        const idx = @intFromEnum(language);
        if (self.connections[idx]) |conn| {
            conn.state = .ready;
            conn.touch(io);
            return conn;
        }

        const conn = try allocator.create(LspConnection);
        conn.* = .{
            .client = LspClient.init(logger),
            .language = language,
            .state = .ready,
            .last_activity_ns = std.Io.Timestamp.now(io, .awake).nanoseconds,
            .project_root = project_root,
        };
        errdefer {
            conn.deinit(allocator, io);
            allocator.destroy(conn);
        }

        try conn.client.start(io, lsp_config.server_command);
        try conn.client.initialize(allocator, io, project_root, lsp_config.init_options);
        errdefer comptime unreachable;

        self.connections[idx] = conn;
        return conn;
    }

    pub fn release(self: *LspPool, io: std.Io, language: Language) void {
        const idx = @intFromEnum(language);
        if (self.connections[idx]) |conn| {
            conn.state = .idle;
            conn.touch(io);
        }
    }

    pub fn reapIdle(self: *LspPool, allocator: std.mem.Allocator, io: std.Io) void {
        const now = std.Io.Timestamp.now(io, .awake).nanoseconds;
        for (&self.connections) |*slot| {
            const conn = slot.* orelse continue;
            if (conn.state != .idle) continue;
            const elapsed: u64 = @intCast(@max(0, now - conn.last_activity_ns));
            if (elapsed > self.idle_timeout_ns) {
                conn.deinit(allocator, io);
                allocator.destroy(conn);
                slot.* = null;
            }
        }
    }

    pub fn notifyFileChanged(
        self: *LspPool,
        language: Language,
        allocator: std.mem.Allocator,
        io: std.Io,
        uri: []const u8,
        new_text: []const u8,
        language_id: []const u8,
    ) void {
        const idx = @intFromEnum(language);
        const conn = self.connections[idx] orelse return;
        conn.openFile(allocator, io, uri, new_text, language_id) catch {};
    }

    pub fn notifyFileClosed(
        self: *LspPool,
        language: Language,
        allocator: std.mem.Allocator,
        io: std.Io,
        uri: []const u8,
    ) void {
        const idx = @intFromEnum(language);
        const conn = self.connections[idx] orelse return;
        conn.closeFile(allocator, io, uri);
    }

    pub fn connectionCount(self: *const LspPool) u32 {
        var count: u32 = 0;
        for (self.connections) |slot| {
            if (slot != null) count += 1;
        }
        return count;
    }
};

// -- Tests --

test "pool init has zero connections" {
    // Arrange / Act
    var pool = LspPool.init(.{});
    defer pool.deinit(std.testing.allocator, std.testing.io);

    // Assert
    try std.testing.expectEqual(@as(u32, 0), pool.connectionCount());
}

test "reapIdle on empty pool is no-op" {
    // Arrange
    var pool = LspPool.init(.{});
    defer pool.deinit(std.testing.allocator, std.testing.io);

    // Act
    pool.reapIdle(std.testing.allocator, std.testing.io);

    // Assert
    try std.testing.expectEqual(@as(u32, 0), pool.connectionCount());
}

test "pool deinit with no connections is clean" {
    // Arrange / Act
    var pool = LspPool.init(.{});

    // Assert: deinit does not leak (std.testing.allocator checks)
    pool.deinit(std.testing.allocator, std.testing.io);
}

test "LspConnection.touch updates last_activity" {
    // Arrange
    const allocator = std.testing.allocator;
    var conn = LspConnection{
        .client = LspClient.init(Logger.noop),
        .language = .zig,
        .state = .ready,
        .last_activity_ns = 0,
        .project_root = ".",
    };
    defer conn.deinit(allocator, std.testing.io);

    // Act
    conn.touch(std.testing.io);

    // Assert
    try std.testing.expect(conn.last_activity_ns > 0);
}

test "LspConnection tracks opened files" {
    // Arrange
    const allocator = std.testing.allocator;
    var conn = LspConnection{
        .client = LspClient.init(Logger.noop),
        .language = .zig,
        .state = .ready,
        .last_activity_ns = 0,
        .project_root = ".",
    };
    defer conn.deinit(allocator, std.testing.io);

    // Act: open a file
    try conn.openFile(allocator, std.testing.io, "file:///test.zig", "const x = 1;", "zig");

    // Assert: tracked at version 1
    try std.testing.expectEqual(@as(u32, 1), conn.openedFileCount());
    try std.testing.expectEqual(@as(i32, 1), conn.opened_files.get("file:///test.zig").?);

    // Act: re-open same file (triggers didChange, increments version)
    try conn.openFile(allocator, std.testing.io, "file:///test.zig", "const x = 2;", "zig");

    // Assert: still one entry, version bumped to 2
    try std.testing.expectEqual(@as(u32, 1), conn.openedFileCount());
    try std.testing.expectEqual(@as(i32, 2), conn.opened_files.get("file:///test.zig").?);

    // Act: close the file
    conn.closeFile(allocator, std.testing.io, "file:///test.zig");

    // Assert: entry removed
    try std.testing.expectEqual(@as(u32, 0), conn.openedFileCount());
    try std.testing.expect(conn.opened_files.get("file:///test.zig") == null);
}

test "LspConnection closeFile on unknown uri is no-op" {
    // Arrange
    const allocator = std.testing.allocator;
    var conn = LspConnection{
        .client = LspClient.init(Logger.noop),
        .language = .zig,
        .state = .ready,
        .last_activity_ns = 0,
        .project_root = ".",
    };
    defer conn.deinit(allocator, std.testing.io);

    // Act / Assert: no crash, no leak
    conn.closeFile(allocator, std.testing.io, "file:///nonexistent.zig");
    try std.testing.expectEqual(@as(u32, 0), conn.openedFileCount());
}

test "idle timeout computation" {
    // Arrange
    var pool = LspPool.init(.{ .idle_timeout_ns = 60 * std.time.ns_per_s });
    defer pool.deinit(std.testing.allocator, std.testing.io);

    const now: i128 = std.Io.Timestamp.now(std.testing.io, .awake).nanoseconds;
    const expired_activity = now - @as(i128, 61 * std.time.ns_per_s);
    const fresh_activity = now - @as(i128, 30 * std.time.ns_per_s);

    // Act / Assert
    const elapsed_expired: u64 = @intCast(@max(0, now - expired_activity));
    const elapsed_fresh: u64 = @intCast(@max(0, now - fresh_activity));

    try std.testing.expect(elapsed_expired > pool.idle_timeout_ns);
    try std.testing.expect(elapsed_fresh <= pool.idle_timeout_ns);
}

test "pool options default idle timeout is 60 seconds" {
    // Arrange / Act
    const pool = LspPool.init(.{});

    // Assert
    try std.testing.expectEqual(@as(u64, 60 * std.time.ns_per_s), pool.idle_timeout_ns);
}
