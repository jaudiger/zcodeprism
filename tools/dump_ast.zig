//! Dump the raw tree-sitter AST for any supported source file.
//! Usage: zig build dump-ast -- <path-to-source-file>
//!
//! Outputs the full tree-sitter node tree with kinds, text excerpts,
//! and line/column positions. Useful for understanding what the visitor
//! receives from tree-sitter.

const std = @import("std");
const zcodeprism = @import("zcodeprism");

const ts_api = zcodeprism.tree_sitter_api;
const source_map = zcodeprism.source_map;

pub fn main() !void {
    var stdout_buffer: [8192]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    // Get file path from args.
    var args = std.process.args();
    _ = args.next(); // skip program name
    const file_path = args.next() orelse {
        try stdout.print("Usage: dump-ast <path-to-source-file>\n", .{});
        try stdout.flush();
        return;
    };

    // Determine language from extension.
    const ext = std.fs.path.extension(file_path);
    const language = ts_api.getLanguageByExtension(ext) orelse {
        try stdout.print("Unsupported file extension: '{s}'\n", .{ext});
        try stdout.flush();
        return;
    };

    // Read the file.
    const source = source_map.mmapFile(file_path) catch |err| {
        try stdout.print("Error opening file '{s}': {}\n", .{ file_path, err });
        try stdout.flush();
        return;
    };
    defer source_map.unmapFile(source);

    // Parse.
    const tree = ts_api.parseSource(language, source) orelse {
        try stdout.print("Failed to parse '{s}'\n", .{file_path});
        try stdout.flush();
        return;
    };
    defer tree.destroy();

    try stdout.print("=== AST: {s} ({} bytes) ===\n\n", .{ file_path, source.len });

    // Dump the tree recursively.
    const root = tree.rootNode();
    try dumpNode(stdout, source, root, 0);

    try stdout.print("\n", .{});
    try stdout.flush();
}

fn dumpNode(stdout: *std.Io.Writer, source: []const u8, node: @import("tree-sitter").Node, depth: u32) !void {
    // Indent.
    var d: u32 = 0;
    while (d < depth) : (d += 1) {
        try stdout.print("  ", .{});
    }

    const kind = node.kind();
    const start = node.startPoint();
    const end = node.endPoint();

    // Show text excerpt for leaf/small nodes, truncated.
    const text = ts_api.nodeText(source, node);
    if (text.len <= 60 and !containsNewline(text)) {
        try stdout.print("{s}  [{d}:{d}-{d}:{d}]  \"{s}\"\n", .{
            kind, start.row + 1, start.column, end.row + 1, end.column, text,
        });
    } else {
        try stdout.print("{s}  [{d}:{d}-{d}:{d}]  ({d} bytes)\n", .{
            kind, start.row + 1, start.column, end.row + 1, end.column, text.len,
        });
    }

    // Recurse into children.
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        try dumpNode(stdout, source, child, depth + 1);
    }
}

fn containsNewline(s: []const u8) bool {
    for (s) |c| {
        if (c == '\n') return true;
    }
    return false;
}
