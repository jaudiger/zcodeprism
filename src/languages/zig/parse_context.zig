const std = @import("std");
const types = @import("../../core/types.zig");
const ts = @import("tree-sitter");

const NodeId = types.NodeId;

/// Pre-resolved tree-sitter node kind IDs. Built once per parse() call from
/// `Language.idForNodeKind()`. Replaces all `std.mem.eql(u8, child.kind(), "...")`
/// patterns with `child.kindId() == k.identifier`, using integer compare
/// instead of string compare.
pub const KindIds = struct {
    identifier: u16,
    field_identifier: u16,
    property_identifier: u16,
    field_expression: u16,
    call_expression: u16,
    function_declaration: u16,
    variable_declaration: u16,
    test_declaration: u16,
    container_field: u16,
    block: u16,
    comment: u16,
    string: u16,
    string_content: u16,
    builtin_function: u16,
    builtin_identifier: u16,
    builtin_type: u16,
    parameters: u16,
    parameter: u16,
    return_expression: u16,
    expression_statement: u16,
    struct_declaration: u16,
    union_declaration: u16,
    enum_declaration: u16,
    error_set_declaration: u16,
    try_expression: u16,
    if_statement: u16,
    if_expression: u16,
    for_statement: u16,
    while_statement: u16,
    switch_expression: u16,
    switch_case: u16,
    arguments: u16,
    defer_statement: u16,
    errdefer_statement: u16,
    catch_expression: u16,
    labeled_statement: u16,
    binary_expression: u16,
    unary_expression: u16,
    assignment_expression: u16,
    amp_op: u16,
    orelse_kw: u16,
    pointer_type: u16,
    nullable_type: u16,
    optional_type: u16,
    error_union_type: u16,
    payload: u16,
    payload_identifier: u16,
    pub_kw: u16,
    var_kw: u16,
    extern_kw: u16,
    inline_kw: u16,
    packed_kw: u16,
    comptime_declaration: u16,
    colon: u16,

    /// Resolve all node kind names to their numeric IDs from the given tree-sitter language.
    pub fn init(lang: *const ts.Language) KindIds {
        return .{
            .identifier = lang.idForNodeKind("identifier", true),
            .field_identifier = lang.idForNodeKind("field_identifier", true),
            .property_identifier = lang.idForNodeKind("property_identifier", true),
            .field_expression = lang.idForNodeKind("field_expression", true),
            .call_expression = lang.idForNodeKind("call_expression", true),
            .function_declaration = lang.idForNodeKind("function_declaration", true),
            .variable_declaration = lang.idForNodeKind("variable_declaration", true),
            .test_declaration = lang.idForNodeKind("test_declaration", true),
            .container_field = lang.idForNodeKind("container_field", true),
            .block = lang.idForNodeKind("block", true),
            .comment = lang.idForNodeKind("comment", true),
            .string = lang.idForNodeKind("string", true),
            .string_content = lang.idForNodeKind("string_content", true),
            .builtin_function = lang.idForNodeKind("builtin_function", true),
            .builtin_identifier = lang.idForNodeKind("builtin_identifier", true),
            .builtin_type = lang.idForNodeKind("builtin_type", true),
            .parameters = lang.idForNodeKind("parameters", true),
            .parameter = lang.idForNodeKind("parameter", true),
            .return_expression = lang.idForNodeKind("return_expression", true),
            .expression_statement = lang.idForNodeKind("expression_statement", true),
            .struct_declaration = lang.idForNodeKind("struct_declaration", true),
            .union_declaration = lang.idForNodeKind("union_declaration", true),
            .enum_declaration = lang.idForNodeKind("enum_declaration", true),
            .error_set_declaration = lang.idForNodeKind("error_set_declaration", true),
            .try_expression = lang.idForNodeKind("try_expression", true),
            .if_statement = lang.idForNodeKind("if_statement", true),
            .if_expression = lang.idForNodeKind("if_expression", true),
            .for_statement = lang.idForNodeKind("for_statement", true),
            .while_statement = lang.idForNodeKind("while_statement", true),
            .switch_expression = lang.idForNodeKind("switch_expression", true),
            .switch_case = lang.idForNodeKind("switch_case", true),
            .arguments = lang.idForNodeKind("arguments", true),
            .defer_statement = lang.idForNodeKind("defer_statement", true),
            .errdefer_statement = lang.idForNodeKind("errdefer_statement", true),
            .catch_expression = lang.idForNodeKind("catch_expression", true),
            .labeled_statement = lang.idForNodeKind("labeled_statement", true),
            .binary_expression = lang.idForNodeKind("binary_expression", true),
            .unary_expression = lang.idForNodeKind("unary_expression", true),
            .assignment_expression = lang.idForNodeKind("assignment_expression", true),
            .amp_op = lang.idForNodeKind("&", false),
            .orelse_kw = lang.idForNodeKind("orelse", false),
            .pointer_type = lang.idForNodeKind("pointer_type", true),
            .nullable_type = lang.idForNodeKind("nullable_type", true),
            .optional_type = lang.idForNodeKind("optional_type", true),
            .error_union_type = lang.idForNodeKind("error_union_type", true),
            .payload = lang.idForNodeKind("payload", true),
            .payload_identifier = lang.idForNodeKind("payload_identifier", true),
            .pub_kw = lang.idForNodeKind("pub", false),
            .var_kw = lang.idForNodeKind("var", false),
            .extern_kw = lang.idForNodeKind("extern", false),
            .inline_kw = lang.idForNodeKind("inline", false),
            .packed_kw = lang.idForNodeKind("packed", false),
            .comptime_declaration = lang.idForNodeKind("comptime_declaration", true),
            .colon = lang.idForNodeKind(":", false),
        };
    }
};

pub const ScopeIndex = @import("../../core/scope_index.zig").ScopeIndex;
pub const FileIndex = @import("../../core/file_index.zig").FileIndex;

/// Resolve an import path relative to the importing file's directory.
/// Joins the directory part of `importer_path` with `import_path`,
/// normalizing `.` and `..` segments. Returns a slice into `buf`,
/// or null if the path escapes above the project root.
pub fn resolveImportPath(buf: []u8, importer_path: []const u8, import_path: []const u8) ?[]const u8 {
    // Split importer_path into directory segments (drop the filename).
    var segments: [64][]const u8 = undefined;
    var seg_count: usize = 0;

    // Extract directory part of importer_path.
    var it = std.mem.splitScalar(u8, importer_path, '/');
    // Collect all segments first, then drop the last one (filename).
    var all_count: usize = 0;
    var all_segments: [64][]const u8 = undefined;
    while (it.next()) |seg| {
        if (all_count < 64) {
            all_segments[all_count] = seg;
            all_count += 1;
        }
    }
    // Copy directory segments (all except last).
    if (all_count > 1) {
        for (all_segments[0 .. all_count - 1]) |seg| {
            if (seg.len > 0 and seg_count < 64) {
                segments[seg_count] = seg;
                seg_count += 1;
            }
        }
    }

    // Process import_path segments.
    var imp_it = std.mem.splitScalar(u8, import_path, '/');
    while (imp_it.next()) |seg| {
        if (seg.len == 0 or std.mem.eql(u8, seg, ".")) {
            // Skip empty and `.` segments.
            continue;
        } else if (std.mem.eql(u8, seg, "..")) {
            // Go up one directory.
            if (seg_count == 0) return null; // Can't go above project root.
            seg_count -= 1;
        } else {
            // Append segment.
            if (seg_count >= 64) return null; // Too many segments.
            segments[seg_count] = seg;
            seg_count += 1;
        }
    }

    if (seg_count == 0) return null; // No path left.

    // Join segments into buf with '/' separators.
    var pos: usize = 0;
    for (segments[0..seg_count], 0..) |seg, i| {
        if (i > 0) {
            if (pos >= buf.len) return null;
            buf[pos] = '/';
            pos += 1;
        }
        if (pos + seg.len > buf.len) return null;
        @memcpy(buf[pos .. pos + seg.len], seg);
        pos += seg.len;
    }

    return buf[0..pos];
}

/// Resolve an import path to a file NodeId using directory-relative resolution.
/// Falls back to direct lookup when importer_path is null or resolution fails.
pub fn resolveFileImport(file_index: *const FileIndex, importer_path: ?[]const u8, import_path: []const u8) ?NodeId {
    if (importer_path) |ip| {
        var buf: [std.fs.max_path_bytes]u8 = undefined;
        if (resolveImportPath(&buf, ip, import_path)) |resolved| {
            if (file_index.findByName(resolved)) |id| return id;
        }
    }
    return file_index.findByName(import_path);
}

test "resolveImportPath resolves same-directory import" {
    // Arrange
    var buf: [std.fs.max_path_bytes]u8 = undefined;

    // Act
    const result = resolveImportPath(&buf, "crypto/aegis.zig", "helpers.zig");

    // Assert
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("crypto/helpers.zig", result.?);
}

test "resolveImportPath strips dot-slash prefix" {
    // Arrange
    var buf: [std.fs.max_path_bytes]u8 = undefined;

    // Act
    const result = resolveImportPath(&buf, "json/dynamic.zig", "./static.zig");

    // Assert
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("json/static.zig", result.?);
}

test "resolveImportPath resolves root-level import" {
    // Arrange
    var buf: [std.fs.max_path_bytes]u8 = undefined;

    // Act
    const result = resolveImportPath(&buf, "main.zig", "utils.zig");

    // Assert
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("utils.zig", result.?);
}

test "resolveImportPath ascends parent directory" {
    // Arrange
    var buf: [std.fs.max_path_bytes]u8 = undefined;

    // Act
    const result = resolveImportPath(&buf, "crypto/sub/inner.zig", "../helpers.zig");

    // Assert
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("crypto/helpers.zig", result.?);
}

test "resolveImportPath descends into subdirectory" {
    // Arrange
    var buf: [std.fs.max_path_bytes]u8 = undefined;

    // Act
    const result = resolveImportPath(&buf, "main.zig", "sub/mod.zig");

    // Assert
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("sub/mod.zig", result.?);
}

test "resolveImportPath ascends two levels with double dot-dot" {
    // Arrange
    var buf: [std.fs.max_path_bytes]u8 = undefined;

    // Act
    const result = resolveImportPath(&buf, "a/b/c/file.zig", "../../root.zig");

    // Assert
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("a/root.zig", result.?);
}

test "resolveImportPath strips dot-slash at root" {
    // Arrange
    var buf: [std.fs.max_path_bytes]u8 = undefined;

    // Act
    const result = resolveImportPath(&buf, "main.zig", "./utils.zig");

    // Assert
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("utils.zig", result.?);
}

test "resolveImportPath returns null when escaping above project root" {
    // Arrange
    var buf: [std.fs.max_path_bytes]u8 = undefined;

    // Act
    const result = resolveImportPath(&buf, "file.zig", "../outside.zig");

    // Assert
    try std.testing.expectEqual(@as(?[]const u8, null), result);
}
