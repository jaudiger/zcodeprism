const std = @import("std");

/// Provenance tag for external (phantom) nodes that reference code outside
/// the indexed project: stdlib or third-party dependencies.
pub const ExternalInfo = union(enum) {
    /// The node is project-internal (not external).
    none: void,
    /// The node references a standard library symbol.
    stdlib: void,
    /// The node references a third-party dependency.
    dependency: struct {
        /// Semver version string, or null if the version is unknown.
        version: ?[]const u8,
    },
};

test "node with external equals none" {
    // Arrange
    const ext = ExternalInfo{ .none = {} };

    // Assert
    try std.testing.expectEqual(ExternalInfo.none, ext);
}

test "node with external equals stdlib" {
    // Arrange
    const ext = ExternalInfo{ .stdlib = {} };

    // Assert
    try std.testing.expectEqual(ExternalInfo.stdlib, ext);
}

test "node with external equals dependency with version" {
    // Arrange
    const version_str = "1.2.3";
    const ext = ExternalInfo{ .dependency = .{ .version = version_str } };

    // Assert
    switch (ext) {
        .dependency => |dep| {
            try std.testing.expect(dep.version != null);
            try std.testing.expectEqualStrings("1.2.3", dep.version.?);
        },
        else => return error.UnexpectedVariant,
    }
}
