const std = @import("std");
const builtin = @import("builtin");

/// Platform-abstracted directory watcher. Uses kqueue on macOS, inotify on
/// Linux. Blocks on waitForEvents until file changes are detected or stop
/// is called from another thread (self-pipe trick).
pub const FileWatcher = struct {
    backend: Backend,
    stop_pipe: [2]std.c.fd_t,

    const Backend = switch (builtin.os.tag) {
        .macos => KqueueBackend,
        .linux => InotifyBackend,
        else => @compileError("FileWatcher: unsupported platform"),
    };

    pub fn init(
        allocator: std.mem.Allocator,
        io: std.Io,
        project_root: []const u8,
        exclude_paths: []const []const u8,
    ) !FileWatcher {
        var pipe_fds: [2]std.c.fd_t = undefined;
        const pipe_rc = std.c.pipe(&pipe_fds);
        if (pipe_rc != 0) return switch (std.c.errno(pipe_rc)) {
            .MFILE => error.ProcessFdQuotaExceeded,
            .NFILE => error.SystemFdQuotaExceeded,
            else => |e| std.posix.unexpectedErrno(e),
        };
        errdefer {
            _ = std.c.close(pipe_fds[0]);
            _ = std.c.close(pipe_fds[1]);
        }

        const backend = try Backend.init(allocator, io, project_root, exclude_paths, pipe_fds[0]);
        return .{
            .backend = backend,
            .stop_pipe = pipe_fds,
        };
    }

    pub fn deinit(self: *FileWatcher, allocator: std.mem.Allocator) void {
        self.backend.deinit(allocator);
        _ = std.c.close(self.stop_pipe[0]);
        _ = std.c.close(self.stop_pipe[1]);
    }

    /// Blocks until at least one file event is ready or the watcher is stopped.
    /// Returns true if events were detected, false if stopped/interrupted.
    pub fn waitForEvents(self: *FileWatcher) bool {
        return self.backend.waitForEvents(self.stop_pipe[0]);
    }

    /// Signal the watcher to stop (from another thread).
    pub fn stop(self: *FileWatcher) void {
        const byte: u8 = 1;
        _ = std.c.write(self.stop_pipe[1], @ptrCast(&byte), 1);
    }
};

/// kqueue-based watcher for macOS.
const KqueueBackend = struct {
    kq: std.c.fd_t,
    dir_fds: std.ArrayList(DirEntry),

    const DirEntry = struct {
        fd: std.c.fd_t,
    };

    const vnode_flags: u32 = std.c.NOTE.WRITE | std.c.NOTE.DELETE | std.c.NOTE.RENAME | std.c.NOTE.EXTEND;

    fn init(
        allocator: std.mem.Allocator,
        io: std.Io,
        project_root: []const u8,
        exclude_paths: []const []const u8,
        pipe_read_fd: std.c.fd_t,
    ) !KqueueBackend {
        const kq = std.c.kqueue();
        if (kq < 0) return switch (std.c.errno(kq)) {
            .MFILE => error.ProcessFdQuotaExceeded,
            .NFILE => error.SystemFdQuotaExceeded,
            else => |e| std.posix.unexpectedErrno(e),
        };
        errdefer _ = std.c.close(kq);

        var self = KqueueBackend{
            .kq = kq,
            .dir_fds = .empty,
        };

        var pipe_event: [1]std.c.Kevent = .{makeReadEvent(pipe_read_fd)};
        var no_events: [1]std.c.Kevent = undefined;
        const ev_rc = std.c.kevent(kq, &pipe_event, 1, &no_events, 0, null);
        if (ev_rc < 0) return switch (std.c.errno(ev_rc)) {
            .ACCES => error.AccessDenied,
            .NOMEM => error.SystemResources,
            .BADF, .INVAL => error.Unexpected,
            else => |e| std.posix.unexpectedErrno(e),
        };

        try self.registerOne(allocator, project_root);

        var root_dir = std.Io.Dir.cwd().openDir(io, project_root, .{ .iterate = true }) catch return self;
        defer root_dir.close(io);
        var walker = root_dir.walk(allocator) catch return self;
        defer walker.deinit();

        while (walker.next(io) catch null) |entry| {
            if (entry.kind != .directory) continue;
            // Skip excludes and hidden directories.
            if (isExcluded(entry.basename, exclude_paths) or std.mem.startsWith(u8, entry.basename, ".")) {
                walker.leave(io);
                continue;
            }
            const abs = std.fs.path.join(allocator, &.{ project_root, entry.path }) catch continue;
            defer allocator.free(abs);
            self.registerOne(allocator, abs) catch continue;
        }

        return self;
    }

    fn registerOne(self: *KqueueBackend, allocator: std.mem.Allocator, path: []const u8) !void {
        const path_z = try allocator.dupeZ(u8, path);
        defer allocator.free(path_z);
        const fd = std.c.open(path_z, .{ .DIRECTORY = true });
        if (fd < 0) return;
        defer _ = std.c.close(fd);

        const duped = std.c.dup(fd);
        if (duped < 0) return;
        errdefer _ = std.c.close(duped);

        try self.dir_fds.append(allocator, .{ .fd = duped });

        var ev: [1]std.c.Kevent = .{makeVnodeEvent(duped)};
        var no_out: [1]std.c.Kevent = undefined;
        if (std.c.kevent(self.kq, &ev, 1, &no_out, 0, null) < 0) return;
    }

    fn makeVnodeEvent(fd: std.c.fd_t) std.c.Kevent {
        return .{
            .ident = @intCast(fd),
            .filter = std.c.EVFILT.VNODE,
            .flags = std.c.EV.ADD | std.c.EV.CLEAR,
            .fflags = vnode_flags,
            .data = 0,
            .udata = 0,
        };
    }

    fn makeReadEvent(fd: std.c.fd_t) std.c.Kevent {
        return .{
            .ident = @intCast(fd),
            .filter = std.c.EVFILT.READ,
            .flags = std.c.EV.ADD,
            .fflags = 0,
            .data = 0,
            .udata = 0,
        };
    }

    fn waitForEvents(self: *KqueueBackend, pipe_read_fd: std.c.fd_t) bool {
        var events: [16]std.c.Kevent = undefined;
        var no_changes: [1]std.c.Kevent = undefined;
        const n = std.c.kevent(self.kq, &no_changes, 0, &events, events.len, null);
        if (n <= 0) return false;

        for (events[0..@intCast(n)]) |ev| {
            if (ev.filter == std.c.EVFILT.READ and ev.ident == @as(usize, @intCast(pipe_read_fd))) {
                return false;
            }
        }
        return true;
    }

    fn deinit(self: *KqueueBackend, allocator: std.mem.Allocator) void {
        for (self.dir_fds.items) |entry| {
            _ = std.c.close(entry.fd);
        }
        self.dir_fds.deinit(allocator);
        _ = std.c.close(self.kq);
    }
};

/// inotify-based watcher for Linux.
const InotifyBackend = struct {
    inotify_fd: std.c.fd_t,
    watch_count: u32,

    fn init(
        allocator: std.mem.Allocator,
        io: std.Io,
        project_root: []const u8,
        exclude_paths: []const []const u8,
        _: std.c.fd_t,
    ) !InotifyBackend {
        const fd = std.c.inotify_init1(std.os.linux.IN.NONBLOCK);
        if (fd < 0) return switch (std.c.errno(fd)) {
            .MFILE => error.ProcessFdQuotaExceeded,
            .NFILE => error.SystemFdQuotaExceeded,
            .NOMEM => error.SystemResources,
            else => |e| std.posix.unexpectedErrno(e),
        };
        errdefer _ = std.c.close(fd);

        var self = InotifyBackend{
            .inotify_fd = fd,
            .watch_count = 0,
        };
        self.registerOne(allocator, project_root);

        var root_dir = std.Io.Dir.cwd().openDir(io, project_root, .{ .iterate = true }) catch return self;
        defer root_dir.close(io);
        var walker = root_dir.walk(allocator) catch return self;
        defer walker.deinit();

        while (walker.next(io) catch null) |entry| {
            if (entry.kind != .directory) continue;
            // Skip excludes and hidden directories.
            if (isExcluded(entry.basename, exclude_paths) or std.mem.startsWith(u8, entry.basename, ".")) {
                walker.leave(io);
                continue;
            }
            const abs = std.fs.path.join(allocator, &.{ project_root, entry.path }) catch continue;
            defer allocator.free(abs);
            self.registerOne(allocator, abs);
        }

        return self;
    }

    fn registerOne(self: *InotifyBackend, allocator: std.mem.Allocator, path: []const u8) void {
        const mask: u32 = std.os.linux.IN.MODIFY | std.os.linux.IN.CREATE | std.os.linux.IN.DELETE | std.os.linux.IN.MOVE;
        const path_z = allocator.dupeZ(u8, path) catch return;
        defer allocator.free(path_z);
        _ = std.c.inotify_add_watch(self.inotify_fd, path_z, mask);
        self.watch_count += 1;
    }

    fn waitForEvents(self: *InotifyBackend, pipe_read_fd: std.c.fd_t) bool {
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
        _ = std.c.close(self.inotify_fd);
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
