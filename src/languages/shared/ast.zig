const std = @import("std");
const ts = @import("tree-sitter");
const ts_api = @import("../../parser/tree_sitter_api.zig");

/// Return the text of the first named child of `node` whose tree-sitter kind id
/// equals `identifier_kind`, or null if no such child exists. Strips Zig's
/// `@"..."` quoting from the returned text.
pub fn getIdentifierName(source: []const u8, node: ts.Node, identifier_kind: u16) ?[]const u8 {
    var i: u32 = 0;
    while (i < node.namedChildCount()) : (i += 1) {
        const child = node.namedChild(i) orelse continue;
        if (child.kindId() == identifier_kind) {
            return stripQuotedIdentifier(ts_api.nodeText(source, child));
        }
    }
    return null;
}

/// Strip the `@"..."` quoting from a Zig identifier, returning the inner name.
/// Returns `raw` unchanged when it is not quoted.
pub fn stripQuotedIdentifier(raw: []const u8) []const u8 {
    if (raw.len >= 3 and raw[0] == '@' and raw[1] == '"' and raw[raw.len - 1] == '"') {
        return raw[2 .. raw.len - 1];
    }
    return raw;
}
