const std = @import("std");
const config = @import("../core/config.zig");

/// Options for `init`.
pub const Options = struct {
    /// When true, delete any existing config or data dir before writing fresh defaults.
    force: bool = false,
    /// When true, write `zcodeprism-workspace.zon` instead of `.zcodeprism.zon`,
    /// and skip the data dir.
    workspace_template: bool = false,
};

/// Outcome of an `init` call.
pub const Result = enum {
    project_initialized,
    workspace_initialized,
};

/// Errors that originate inside the command layer for `init`.
pub const InitError = error{
    AlreadyInitialized,
};

/// Initialize a project or a workspace in the current working directory.
pub fn run(io: std.Io, options: Options) !Result {
    const cwd = std.Io.Dir.cwd();

    if (options.workspace_template) {
        if (options.force) {
            cwd.deleteFile(io, "zcodeprism-workspace.zon") catch {};
        }
        config.writeDefaultWorkspaceConfig(io, cwd) catch |err| switch (err) {
            error.PathAlreadyExists => return error.AlreadyInitialized,
            else => return err,
        };
        return .workspace_initialized;
    }

    if (options.force) {
        cwd.deleteFile(io, ".zcodeprism.zon") catch {};
        cwd.deleteTree(io, ".zcodeprism") catch {};
    }

    config.writeDefaultConfig(io, cwd) catch |err| switch (err) {
        error.PathAlreadyExists => return error.AlreadyInitialized,
        else => return err,
    };

    try config.createDataDir(io, cwd);
    return .project_initialized;
}
