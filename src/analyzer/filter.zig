const std = @import("std");
const scope_mod = @import("../core/scope.zig");
const Scope = scope_mod.Scope;

/// Whether a node should pass the optional scope restriction.
/// A null scope passes all nodes. A null file_path is transparent to scope
/// filtering: the node has no known path, so scope cannot restrict it.
pub fn passesScope(scope: ?Scope, file_path: ?[]const u8) bool {
    const sf = scope orelse return true;
    const fp = file_path orelse return true;
    return sf.matches(fp);
}

test "passesScope with null scope passes all" {
    // Arrange / Act / Assert
    try std.testing.expect(passesScope(null, "src/foo.zig"));
    try std.testing.expect(passesScope(null, null));
}

test "passesScope with null file_path passes through" {
    // Arrange
    const sf = Scope.parse("src/");
    // Act / Assert
    try std.testing.expect(passesScope(sf, null));
}

test "passesScope applies scope to path" {
    // Arrange
    const sf = Scope.parse("src/");
    // Act / Assert
    try std.testing.expect(passesScope(sf, "src/foo.zig"));
    try std.testing.expect(!passesScope(sf, "test/foo.zig"));
}
