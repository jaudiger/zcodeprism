const std = @import("std");
const logging = @import("../../logging.zig");

const Allocator = std.mem.Allocator;
const Logger = logging.Logger;

/// Structured result of parsing a Cargo.toml manifest.
///
/// All slice fields are allocator-owned. Call `deinit` to release.
pub const CargoInfo = struct {
    package_name: ?[]u8 = null,
    package_version: ?[]u8 = null,
    dependencies: ?[]DepEntry = null,
    dev_dependencies: ?[]DepEntry = null,
    bin_targets: ?[]TargetEntry = null,
    lib_target: ?LibTarget = null,
    workspace_members: ?[][]u8 = null,

    pub const DepEntry = struct {
        name: []u8,
        version: ?[]u8,
    };

    pub const TargetEntry = struct {
        name: []u8,
        path: ?[]u8,
    };

    pub const LibTarget = struct {
        name: ?[]u8,
        path: ?[]u8,
    };

    pub fn deinit(self: CargoInfo, allocator: Allocator) void {
        if (self.package_name) |n| allocator.free(n);
        if (self.package_version) |v| allocator.free(v);
        if (self.dependencies) |deps| {
            for (deps) |d| {
                allocator.free(d.name);
                if (d.version) |v| allocator.free(v);
            }
            allocator.free(deps);
        }
        if (self.dev_dependencies) |deps| {
            for (deps) |d| {
                allocator.free(d.name);
                if (d.version) |v| allocator.free(v);
            }
            allocator.free(deps);
        }
        if (self.bin_targets) |targets| {
            for (targets) |t| {
                allocator.free(t.name);
                if (t.path) |p| allocator.free(p);
            }
            allocator.free(targets);
        }
        if (self.lib_target) |lib| {
            if (lib.name) |n| allocator.free(n);
            if (lib.path) |p| allocator.free(p);
        }
        if (self.workspace_members) |members| {
            for (members) |m| allocator.free(m);
            allocator.free(members);
        }
    }
};

/// Parse a Cargo.toml content string and extract package, dependency, target,
/// and workspace information.
pub fn parseCargoToml(allocator: Allocator, content: []const u8, log: Logger) !CargoInfo {
    var result: CargoInfo = .{};
    errdefer result.deinit(allocator);

    var deps = std.ArrayList(CargoInfo.DepEntry).empty;
    defer deps.deinit(allocator);
    var dev_deps = std.ArrayList(CargoInfo.DepEntry).empty;
    defer dev_deps.deinit(allocator);
    var bin_targets = std.ArrayList(CargoInfo.TargetEntry).empty;
    defer bin_targets.deinit(allocator);
    var workspace_members = std.ArrayList([]u8).empty;
    defer workspace_members.deinit(allocator);

    var section: Section = .none;
    var in_members_array = false;

    // Per-section accumulators for [[bin]] and [lib].
    var bin_name: ?[]u8 = null;
    var bin_path: ?[]u8 = null;
    var lib_name: ?[]u8 = null;
    var lib_path: ?[]u8 = null;

    var line_iter = std.mem.splitScalar(u8, content, '\n');
    while (line_iter.next()) |raw_line| {
        const line = std.mem.trimEnd(u8, raw_line, "\r");
        const trimmed = std.mem.trimStart(u8, line, " \t");

        if (trimmed.len == 0) continue;
        if (trimmed[0] == '#') continue;

        // Check for section headers.
        if (trimmed[0] == '[') {
            // Flush any pending [[bin]] section.
            if (section == .bin and bin_name != null) {
                try bin_targets.append(allocator, .{ .name = bin_name.?, .path = bin_path });
                bin_name = null;
                bin_path = null;
            }

            in_members_array = false;
            section = parseSection(trimmed);
            continue;
        }

        // Inside members = [...] array, collect quoted strings.
        if (in_members_array) {
            if (std.mem.indexOfScalar(u8, trimmed, ']') != null) {
                // Collect any member on the closing line, then end.
                if (extractQuotedValue(trimmed)) |val| {
                    const m = try allocator.dupe(u8, val);
                    errdefer allocator.free(m);
                    try workspace_members.append(allocator, m);
                }
                in_members_array = false;
                continue;
            }
            if (extractQuotedValue(trimmed)) |val| {
                const m = try allocator.dupe(u8, val);
                errdefer allocator.free(m);
                try workspace_members.append(allocator, m);
            }
            continue;
        }

        switch (section) {
            .package => {
                if (parseKeyValue(trimmed)) |kv| {
                    if (std.mem.eql(u8, kv.key, "name")) {
                        if (result.package_name != null) allocator.free(result.package_name.?);
                        result.package_name = try allocator.dupe(u8, kv.value);
                    } else if (std.mem.eql(u8, kv.key, "version")) {
                        if (result.package_version != null) allocator.free(result.package_version.?);
                        result.package_version = try allocator.dupe(u8, kv.value);
                    }
                }
            },
            .dependencies => try parseDep(allocator, trimmed, &deps),
            .dev_dependencies => try parseDep(allocator, trimmed, &dev_deps),
            .bin => {
                if (parseKeyValue(trimmed)) |kv| {
                    if (std.mem.eql(u8, kv.key, "name")) {
                        if (bin_name) |old| allocator.free(old);
                        bin_name = try allocator.dupe(u8, kv.value);
                    } else if (std.mem.eql(u8, kv.key, "path")) {
                        if (bin_path) |old| allocator.free(old);
                        bin_path = try allocator.dupe(u8, kv.value);
                    }
                }
            },
            .lib => {
                if (parseKeyValue(trimmed)) |kv| {
                    if (std.mem.eql(u8, kv.key, "name")) {
                        if (lib_name) |old| allocator.free(old);
                        lib_name = try allocator.dupe(u8, kv.value);
                    } else if (std.mem.eql(u8, kv.key, "path")) {
                        if (lib_path) |old| allocator.free(old);
                        lib_path = try allocator.dupe(u8, kv.value);
                    }
                }
            },
            .workspace => {
                // Detect "members = [" (possibly with entries on same line).
                if (std.mem.startsWith(u8, trimmed, "members")) {
                    if (std.mem.indexOfScalar(u8, trimmed, '[')) |bracket| {
                        const rest = trimmed[bracket + 1 ..];
                        if (std.mem.indexOfScalar(u8, rest, ']')) |close| {
                            // Single-line array.
                            try collectArrayMembers(allocator, rest[0..close], &workspace_members);
                        } else {
                            // Multi-line array starts here.
                            try collectArrayMembers(allocator, rest, &workspace_members);
                            in_members_array = true;
                        }
                    }
                }
            },
            .none => {},
        }
    }

    // Flush trailing [[bin]] section.
    if (section == .bin and bin_name != null) {
        try bin_targets.append(allocator, .{ .name = bin_name.?, .path = bin_path });
        bin_name = null;
        bin_path = null;
    }

    // Clean up any unflushed bin/lib fragments on early returns above.
    errdefer {
        if (bin_name) |n| allocator.free(n);
        if (bin_path) |p| allocator.free(p);
        if (lib_name) |n| allocator.free(n);
        if (lib_path) |p| allocator.free(p);
    }

    if (deps.items.len > 0) {
        result.dependencies = try deps.toOwnedSlice(allocator);
    }
    if (dev_deps.items.len > 0) {
        result.dev_dependencies = try dev_deps.toOwnedSlice(allocator);
    }
    if (bin_targets.items.len > 0) {
        result.bin_targets = try bin_targets.toOwnedSlice(allocator);
    }
    if (workspace_members.items.len > 0) {
        result.workspace_members = try workspace_members.toOwnedSlice(allocator);
    }
    if (lib_name != null or lib_path != null) {
        result.lib_target = .{ .name = lib_name, .path = lib_path };
    }

    log.debug("parsed Cargo.toml", &.{
        logging.Field.string("package", result.package_name orelse "(none)"),
        logging.Field.uint("deps", if (result.dependencies) |d| d.len else 0),
    });

    errdefer comptime unreachable;

    return result;
}

const Section = enum { none, package, dependencies, dev_dependencies, bin, lib, workspace };

fn parseSection(header: []const u8) Section {
    if (std.mem.startsWith(u8, header, "[[bin]]")) return .bin;
    if (header.len < 2 or header[0] != '[') return .none;
    const close = std.mem.indexOfScalar(u8, header, ']') orelse return .none;
    const name = header[1..close];
    if (std.mem.eql(u8, name, "package")) return .package;
    if (std.mem.eql(u8, name, "dependencies")) return .dependencies;
    if (std.mem.eql(u8, name, "dev-dependencies")) return .dev_dependencies;
    if (std.mem.eql(u8, name, "lib")) return .lib;
    if (std.mem.eql(u8, name, "workspace")) return .workspace;
    return .none;
}

const KV = struct { key: []const u8, value: []const u8 };

/// Extract a simple key = "value" pair. Returns null if the value is not quoted.
fn parseKeyValue(line: []const u8) ?KV {
    const eq = std.mem.indexOfScalar(u8, line, '=') orelse return null;
    const key = std.mem.trimEnd(u8, line[0..eq], " \t");
    const after_eq = std.mem.trimStart(u8, line[eq + 1 ..], " \t");
    if (after_eq.len < 2 or after_eq[0] != '"') return null;
    const close_quote = std.mem.indexOfScalarPos(u8, after_eq, 1, '"') orelse return null;
    return .{ .key = key, .value = after_eq[1..close_quote] };
}

/// Extract the first quoted string from a line.
fn extractQuotedValue(line: []const u8) ?[]const u8 {
    const open = std.mem.indexOfScalar(u8, line, '"') orelse return null;
    const close = std.mem.indexOfScalarPos(u8, line, open + 1, '"') orelse return null;
    return line[open + 1 .. close];
}

/// Parse a dependency line: either `name = "version"` or `name = { version = "...", ... }`.
fn parseDep(allocator: Allocator, line: []const u8, list: *std.ArrayList(CargoInfo.DepEntry)) !void {
    const eq = std.mem.indexOfScalar(u8, line, '=') orelse return;
    const name_raw = std.mem.trimEnd(u8, line[0..eq], " \t");
    if (name_raw.len == 0) return;

    const after_eq = std.mem.trimStart(u8, line[eq + 1 ..], " \t");

    const dep_name = try allocator.dupe(u8, name_raw);
    errdefer allocator.free(dep_name);

    if (after_eq.len > 0 and after_eq[0] == '"') {
        // Simple string form: name = "version"
        const close = std.mem.indexOfScalarPos(u8, after_eq, 1, '"') orelse {
            try list.append(allocator, .{ .name = dep_name, .version = null });
            return;
        };
        const ver = try allocator.dupe(u8, after_eq[1..close]);
        errdefer allocator.free(ver);
        try list.append(allocator, .{ .name = dep_name, .version = ver });
    } else if (after_eq.len > 0 and after_eq[0] == '{') {
        // Inline table form: name = { version = "...", ... }
        const ver = extractVersionFromInlineTable(after_eq);
        const owned_ver: ?[]u8 = if (ver) |v| try allocator.dupe(u8, v) else null;
        errdefer if (owned_ver) |v| allocator.free(v);
        try list.append(allocator, .{ .name = dep_name, .version = owned_ver });
    } else {
        try list.append(allocator, .{ .name = dep_name, .version = null });
    }
}

/// Extract the version value from an inline table like `{ version = "1.0", features = [...] }`.
fn extractVersionFromInlineTable(table: []const u8) ?[]const u8 {
    const needle = "version";
    const pos = std.mem.indexOf(u8, table, needle) orelse return null;
    const after_key = table[pos + needle.len ..];
    const eq = std.mem.indexOfScalar(u8, after_key, '=') orelse return null;
    const after_eq = std.mem.trimStart(u8, after_key[eq + 1 ..], " \t");
    if (after_eq.len < 2 or after_eq[0] != '"') return null;
    const close = std.mem.indexOfScalarPos(u8, after_eq, 1, '"') orelse return null;
    return after_eq[1..close];
}

/// Collect all quoted strings from a partial array line.
fn collectArrayMembers(allocator: Allocator, text: []const u8, list: *std.ArrayList([]u8)) !void {
    var pos: usize = 0;
    while (pos < text.len) {
        const open = std.mem.indexOfScalarPos(u8, text, pos, '"') orelse break;
        const close = std.mem.indexOfScalarPos(u8, text, open + 1, '"') orelse break;
        const m = try allocator.dupe(u8, text[open + 1 .. close]);
        errdefer allocator.free(m);
        try list.append(allocator, m);
        pos = close + 1;
    }
}

/// Read and parse the Cargo.toml file at the given project root directory.
pub fn parseCargoManifest(allocator: Allocator, io: std.Io, project_root: []const u8, log: Logger) !CargoInfo {
    const max_manifest_bytes: usize = 1024 * 1024;

    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const joined = std.fmt.bufPrint(&path_buf, "{s}/Cargo.toml", .{project_root}) catch return .{};

    const file = std.Io.Dir.openFileAbsolute(io, joined, .{}) catch return .{};
    defer file.close(io);

    var rbuf: [4096]u8 = undefined;
    var reader = file.reader(io, &rbuf);
    const content = reader.interface.allocRemaining(allocator, .limited(max_manifest_bytes)) catch return .{};
    defer allocator.free(content);

    return parseCargoToml(allocator, content, log);
}

test "parseCargoToml extracts package and dependencies" {
    // Arrange
    const allocator = std.testing.allocator;
    const content =
        \\[package]
        \\name = "my-crate"
        \\version = "0.2.0"
        \\
        \\[dependencies]
        \\serde = "1.0"
        \\tokio = { version = "1.28", features = ["full"] }
        \\
        \\[dev-dependencies]
        \\tempfile = "3.5"
    ;

    // Act
    const info = try parseCargoToml(allocator, content, Logger.noop);
    defer info.deinit(allocator);

    // Assert
    try std.testing.expectEqualStrings("my-crate", info.package_name.?);
    try std.testing.expectEqualStrings("0.2.0", info.package_version.?);

    const deps = info.dependencies.?;
    try std.testing.expectEqual(@as(usize, 2), deps.len);

    const dev_deps = info.dev_dependencies.?;
    try std.testing.expectEqual(@as(usize, 1), dev_deps.len);
    try std.testing.expectEqualStrings("tempfile", dev_deps[0].name);
}

test "parseCargoToml handles empty and missing sections" {
    // Arrange
    const allocator = std.testing.allocator;

    // Act: empty content
    const empty_info = try parseCargoToml(allocator, "", Logger.noop);
    defer empty_info.deinit(allocator);

    // Assert: all fields null
    try std.testing.expectEqual(@as(?[]u8, null), empty_info.package_name);
    try std.testing.expectEqual(@as(?[]CargoInfo.DepEntry, null), empty_info.dependencies);

    // Act: package only, no dependencies section
    const pkg_only =
        \\[package]
        \\name = "bare"
        \\version = "1.0.0"
    ;
    const pkg_info = try parseCargoToml(allocator, pkg_only, Logger.noop);
    defer pkg_info.deinit(allocator);

    // Assert
    try std.testing.expectEqualStrings("bare", pkg_info.package_name.?);
    try std.testing.expectEqual(@as(?[]CargoInfo.DepEntry, null), pkg_info.dependencies);
}
