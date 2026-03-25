const std = @import("std");
const builtin = @import("builtin");

/// Platform-abstracted directory watcher. Uses kqueue on macOS, inotify on
/// Linux. Blocks on waitForEvents until file changes are detected or stop
/// is called from another thread (self-pipe trick).
pub const FileWatcher = struct {
    backend: Backend,
    stop_pipe: [2]std.posix.fd_t,

    const Backend = switch (builtin.os.tag) {
        .macos => KqueueBackend,
        .linux => InotifyBackend,
        else => @compileError("FileWatcher: unsupported platform"),
    };

    pub fn init(
        project_root: []const u8,
        allocator: std.mem.Allocator,
        exclude_paths: []const []const u8,
    ) !FileWatcher {
        const pipe = try std.posix.pipe();
        errdefer {
            std.posix.close(pipe[0]);
            std.posix.close(pipe[1]);
        }

        const backend = try Backend.init(project_root, allocator, exclude_paths, pipe[0]);
        return .{
            .backend = backend,
            .stop_pipe = pipe,
        };
    }

    pub fn deinit(self: *FileWatcher, allocator: std.mem.Allocator) void {
        self.backend.deinit(allocator);
        std.posix.close(self.stop_pipe[0]);
        std.posix.close(self.stop_pipe[1]);
    }

    /// Blocks until at least one file event is ready or the watcher is stopped.
    /// Returns true if events were detected, false if stopped/interrupted.
    pub fn waitForEvents(self: *FileWatcher) bool {
        return self.backend.waitForEvents(self.stop_pipe[0]);
    }

    /// Signal the watcher to stop (from another thread).
    pub fn stop(self: *FileWatcher) void {
        _ = std.posix.write(self.stop_pipe[1], &.{1}) catch {};
    }
};

/// kqueue-based watcher for macOS.
const KqueueBackend = struct {
    kq: std.posix.fd_t,
    dir_fds: std.ArrayList(DirEntry),

    const DirEntry = struct {
        fd: std.posix.fd_t,
    };

    const vnode_flags: u32 = std.c.NOTE.WRITE | std.c.NOTE.DELETE | std.c.NOTE.RENAME | std.c.NOTE.EXTEND;

    fn init(
        project_root: []const u8,
        allocator: std.mem.Allocator,
        exclude_paths: []const []const u8,
        pipe_read_fd: std.posix.fd_t,
    ) !KqueueBackend {
        const kq = try std.posix.kqueue();
        errdefer std.posix.close(kq);

        var self = KqueueBackend{
            .kq = kq,
            .dir_fds = .{},
        };

        // Register the pipe read-end for stop signaling.
        var pipe_event: [1]std.c.Kevent = .{makeReadEvent(pipe_read_fd)};
        _ = try std.posix.kevent(kq, &pipe_event, &.{}, null);

        // Walk the project root recursively and register all directories.
        try self.registerDirectory(allocator, project_root, exclude_paths);

        return self;
    }

    fn registerDirectory(
        self: *KqueueBackend,
        allocator: std.mem.Allocator,
        path: []const u8,
        exclude_paths: []const []const u8,
    ) !void {
        var dir = std.fs.openDirAbsolute(path, .{ .iterate = true }) catch return;
        defer dir.close();

        const fd = dir.fd;
        // dup the fd so it survives the dir.close() above.
        const duped = std.posix.dup(fd) catch return;
        errdefer std.posix.close(duped);

        try self.dir_fds.append(allocator, .{ .fd = duped });

        var ev: [1]std.c.Kevent = .{makeVnodeEvent(duped)};
        _ = std.posix.kevent(self.kq, &ev, &.{}, null) catch {
            return;
        };

        var it = dir.iterate();
        while (it.next() catch null) |entry| {
            if (entry.kind != .directory) continue;
            if (isExcluded(entry.name, exclude_paths)) continue;
            if (std.mem.startsWith(u8, entry.name, ".")) continue;

            var child_buf: [std.fs.max_path_bytes]u8 = undefined;
            const child_path = std.fmt.bufPrint(&child_buf, "{s}/{s}", .{ path, entry.name }) catch continue;
            self.registerDirectory(allocator, child_path, exclude_paths) catch continue;
        }
    }

    fn makeVnodeEvent(fd: std.posix.fd_t) std.c.Kevent {
        return .{
            .ident = @intCast(fd),
            .filter = std.c.EVFILT.VNODE,
            .flags = std.c.EV.ADD | std.c.EV.CLEAR,
            .fflags = vnode_flags,
            .data = 0,
            .udata = 0,
        };
    }

    fn makeReadEvent(fd: std.posix.fd_t) std.c.Kevent {
        return .{
            .ident = @intCast(fd),
            .filter = std.c.EVFILT.READ,
            .flags = std.c.EV.ADD,
            .fflags = 0,
            .data = 0,
            .udata = 0,
        };
    }

    fn waitForEvents(self: *KqueueBackend, pipe_read_fd: std.posix.fd_t) bool {
        var events: [16]std.c.Kevent = undefined;
        const n = std.posix.kevent(self.kq, &.{}, &events, null) catch return false;
        if (n == 0) return false;

        for (events[0..@intCast(n)]) |ev| {
            if (ev.filter == std.c.EVFILT.READ and ev.ident == @as(usize, @intCast(pipe_read_fd))) {
                return false;
            }
        }
        return true;
    }

    fn deinit(self: *KqueueBackend, allocator: std.mem.Allocator) void {
        for (self.dir_fds.items) |entry| {
            std.posix.close(entry.fd);
        }
        self.dir_fds.deinit(allocator);
        std.posix.close(self.kq);
    }
};

/// inotify-based watcher for Linux.
const InotifyBackend = struct {
    inotify_fd: std.posix.fd_t,
    watch_count: u32,

    fn init(
        project_root: []const u8,
        allocator: std.mem.Allocator,
        exclude_paths: []const []const u8,
        _: std.posix.fd_t,
    ) !InotifyBackend {
        const fd = try std.posix.inotify_init1(std.os.linux.IN.NONBLOCK);
        errdefer std.posix.close(fd);

        var self = InotifyBackend{
            .inotify_fd = fd,
            .watch_count = 0,
        };
        self.registerDirectory(allocator, project_root, exclude_paths);
        return self;
    }

    fn registerDirectory(
        self: *InotifyBackend,
        allocator: std.mem.Allocator,
        path: []const u8,
        exclude_paths: []const []const u8,
    ) void {
        const mask: u32 = std.os.linux.IN.MODIFY | std.os.linux.IN.CREATE | std.os.linux.IN.DELETE | std.os.linux.IN.MOVE;
        const path_z = allocator.dupeZ(u8, path) catch return;
        defer allocator.free(path_z);
        _ = std.posix.inotify_add_watch(self.inotify_fd, path_z, mask) catch return;
        self.watch_count += 1;

        var dir = std.fs.openDirAbsolute(path, .{ .iterate = true }) catch return;
        defer dir.close();

        var it = dir.iterate();
        while (it.next() catch null) |entry| {
            if (entry.kind != .directory) continue;
            if (isExcluded(entry.name, exclude_paths)) continue;
            if (std.mem.startsWith(u8, entry.name, ".")) continue;

            var child_buf: [std.fs.max_path_bytes]u8 = undefined;
            const child_path = std.fmt.bufPrint(&child_buf, "{s}/{s}", .{ path, entry.name }) catch continue;
            self.registerDirectory(allocator, child_path, exclude_paths);
        }
    }

    fn waitForEvents(self: *InotifyBackend, pipe_read_fd: std.posix.fd_t) bool {
        var fds = [2]std.posix.pollfd{
            .{ .fd = self.inotify_fd, .events = std.posix.POLL.IN, .revents = 0 },
            .{ .fd = pipe_read_fd, .events = std.posix.POLL.IN, .revents = 0 },
        };
        const n = std.posix.poll(&fds, -1) catch return false;
        if (n == 0) return false;
        if (fds[1].revents & std.posix.POLL.IN != 0) return false;
        if (fds[0].revents & std.posix.POLL.IN != 0) {
            // Drain the inotify buffer.
            var buf: [4096]u8 = undefined;
            while (true) {
                const read_result = std.posix.read(self.inotify_fd, &buf);
                if (read_result) |bytes_read| {
                    if (bytes_read == 0) break;
                } else |_| break;
            }
            return true;
        }
        return false;
    }

    fn deinit(self: *InotifyBackend, _: std.mem.Allocator) void {
        std.posix.close(self.inotify_fd);
        self.* = undefined;
    }
};

fn isExcluded(name: []const u8, exclude_paths: []const []const u8) bool {
    for (exclude_paths) |excl| {
        if (std.mem.eql(u8, name, excl)) return true;
        if (std.mem.eql(u8, name, std.fs.path.basename(excl))) return true;
    }
    return false;
}

test "isExcluded matches exact name" {
    // Arrange
    const excludes: []const []const u8 = &.{ "node_modules", ".git" };

    // Act / Assert
    try std.testing.expect(isExcluded("node_modules", excludes));
    try std.testing.expect(isExcluded(".git", excludes));
    try std.testing.expect(!isExcluded("src", excludes));
}

test "FileWatcher struct has expected fields" {
    // Compile-time structural check.
    comptime {
        std.debug.assert(@hasField(FileWatcher, "backend"));
        std.debug.assert(@hasField(FileWatcher, "stop_pipe"));
    }
}
