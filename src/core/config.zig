const std = @import("std");

/// Persistent storage format for graph snapshots.
pub const StorageFormat = enum {
    binary,
    jsonl,
};

/// Programming language that ZCodePrism can index.
pub const LanguageOption = enum {
    zig,
    rust,
};

/// Paths to external LSP servers for enrichment.
pub const LspConfig = struct {
    zls_path: ?[]const u8 = null,
    rust_analyzer_path: ?[]const u8 = null,
};

/// Where and how to persist the graph.
pub const StorageConfig = struct {
    path: ?[]const u8 = null,
    format: ?StorageFormat = null,
};

/// Memory budget knobs.
pub const MemoryConfig = struct {
    budget_mb: ?u32 = null,
};

/// Project-level configuration loaded from `.zcodeprism.zon`.
pub const Config = struct {
    exclude_paths: ?[]const []const u8 = null,
    languages: ?[]const LanguageOption = null,
    lsp: ?LspConfig = null,
    storage: ?StorageConfig = null,
    memory: ?MemoryConfig = null,
};

pub const ParseError = error{
    InvalidConfig,
    OutOfMemory,
};

/// Parse a `.zcodeprism.zon` file from its raw text content.
pub fn parseFromSlice(allocator: std.mem.Allocator, source: [:0]const u8) ParseError!Config {
    return std.zon.parse.fromSlice(Config, allocator, source, null, .{
        .ignore_unknown_fields = true,
    }) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ParseZon => return error.InvalidConfig,
    };
}

/// Free all memory owned by a parsed Config.
pub fn deinit(cfg: Config, allocator: std.mem.Allocator) void {
    std.zon.parse.free(allocator, cfg);
}

/// Fill in default values for any field left null.
pub fn withDefaults(cfg: Config) Config {
    return .{
        .exclude_paths = cfg.exclude_paths orelse defaultExcludePaths(),
        .languages = cfg.languages,
        .lsp = cfg.lsp,
        .storage = .{
            .path = if (cfg.storage) |s| s.path orelse ".zcodeprism/" else ".zcodeprism/",
            .format = if (cfg.storage) |s| s.format orelse .binary else .binary,
        },
        .memory = .{
            .budget_mb = if (cfg.memory) |m| m.budget_mb orelse 512 else 512,
        },
    };
}

/// The five paths excluded by default when no config is present.
pub fn defaultExcludePaths() []const []const u8 {
    return &.{ ".git", ".zcodeprism", "zig-out", "zig-cache", "target" };
}

const default_config_content = @embedFile("default_config.zon");

/// Write the default `.zcodeprism.zon` config file into `dir`.
pub fn writeDefaultConfig(dir: std.fs.Dir) !void {
    const file = try dir.createFile(".zcodeprism.zon", .{ .exclusive = true });
    defer file.close();
    try file.writeAll(default_config_content);
}

/// Create the `.zcodeprism/` data directory inside `dir`.
pub fn createDataDir(dir: std.fs.Dir) !void {
    dir.makeDir(".zcodeprism") catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "parses valid config" {
    const source: [:0]const u8 =
        \\.{
        \\    .exclude_paths = .{ ".git", "vendor" },
        \\    .languages = .{ .zig, .rust },
        \\    .lsp = .{ .zls_path = "/usr/bin/zls" },
        \\    .storage = .{ .path = ".zcodeprism/data", .format = .jsonl },
        \\    .memory = .{ .budget_mb = 512 },
        \\}
    ;

    const cfg = try parseFromSlice(std.testing.allocator, source);
    defer deinit(cfg, std.testing.allocator);

    try std.testing.expect(cfg.exclude_paths != null);
    try std.testing.expectEqual(2, cfg.exclude_paths.?.len);
    try std.testing.expect(cfg.languages != null);
    try std.testing.expectEqual(2, cfg.languages.?.len);
    try std.testing.expect(cfg.lsp != null);
    try std.testing.expect(cfg.lsp.?.zls_path != null);
    try std.testing.expect(cfg.storage != null);
    try std.testing.expectEqual(StorageFormat.jsonl, cfg.storage.?.format.?);
    try std.testing.expect(cfg.memory != null);
    try std.testing.expectEqual(@as(u32, 512), cfg.memory.?.budget_mb.?);
}

test "applies defaults for missing fields" {
    const source: [:0]const u8 =
        \\.{
        \\    .languages = .{ .zig },
        \\}
    ;

    const cfg = try parseFromSlice(std.testing.allocator, source);
    defer deinit(cfg, std.testing.allocator);

    const full = withDefaults(cfg);

    try std.testing.expect(full.exclude_paths != null);
    try std.testing.expect(full.storage != null);
    try std.testing.expectEqual(StorageFormat.binary, full.storage.?.format.?);
}

test "exclude_paths default" {
    const paths = defaultExcludePaths();
    try std.testing.expectEqual(@as(usize, 5), paths.len);
    try std.testing.expectEqualStrings(".git", paths[0]);
    try std.testing.expectEqualStrings(".zcodeprism", paths[1]);
    try std.testing.expectEqualStrings("zig-out", paths[2]);
    try std.testing.expectEqualStrings("zig-cache", paths[3]);
    try std.testing.expectEqualStrings("target", paths[4]);
}

test "empty config file" {
    const source: [:0]const u8 = ".{}";

    const cfg = try parseFromSlice(std.testing.allocator, source);
    defer deinit(cfg, std.testing.allocator);

    try std.testing.expectEqual(@as(?[]const []const u8, null), cfg.exclude_paths);
    try std.testing.expectEqual(@as(?[]const LanguageOption, null), cfg.languages);
    try std.testing.expectEqual(@as(?LspConfig, null), cfg.lsp);
    try std.testing.expectEqual(@as(?StorageConfig, null), cfg.storage);
    try std.testing.expectEqual(@as(?MemoryConfig, null), cfg.memory);
}

test "config with unknown fields" {
    const source: [:0]const u8 =
        \\.{
        \\    .languages = .{ .zig },
        \\    .unknown_field = true,
        \\}
    ;

    const cfg = try parseFromSlice(std.testing.allocator, source);
    defer deinit(cfg, std.testing.allocator);

    try std.testing.expect(cfg.languages != null);
}

test "all fields are optional" {
    comptime {
        const info = @typeInfo(Config);
        for (info.@"struct".fields) |f| {
            std.debug.assert(@typeInfo(f.type) == .optional);
        }
    }

    const cfg = Config{};
    try std.testing.expectEqual(@as(?[]const []const u8, null), cfg.exclude_paths);
    try std.testing.expectEqual(@as(?[]const LanguageOption, null), cfg.languages);
    try std.testing.expectEqual(@as(?LspConfig, null), cfg.lsp);
    try std.testing.expectEqual(@as(?StorageConfig, null), cfg.storage);
    try std.testing.expectEqual(@as(?MemoryConfig, null), cfg.memory);
}
