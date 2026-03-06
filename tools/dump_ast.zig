//! Dump the raw tree-sitter AST for any supported source file.
//! Usage: zig build dump-ast -- <path-to-source-file> [--help]
//!
//! Outputs the full tree-sitter node tree with kinds, text excerpts,
//! and line/column positions. Useful for understanding what the visitor
//! receives from tree-sitter.

const std = @import("std");
const ts = @import("tree-sitter");
const zcodeprism = @import("zcodeprism");
const tool_utils = @import("tool-utils");

const Registry = zcodeprism.registry.Registry;
const ts_api = zcodeprism.tree_sitter_api;
const source_map = zcodeprism.source_map;

pub fn main() !void {
    var stdout_buffer: [tool_utils.stdout_buffer_size]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    // Get file path from args.
    var args = std.process.args();
    _ = args.next(); // skip program name

    var file_path: ?[]const u8 = null;

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            try printHelp(stdout);
            return;
        } else {
            file_path = arg;
        }
    }

    const path = file_path orelse {
        try printHelp(stdout);
        return;
    };

    // Determine language from extension via Registry.
    const ext = std.fs.path.extension(path);
    const lang_support = Registry.getByExtension(ext) orelse {
        try stdout.print("Unsupported file extension: '{s}'\n", .{ext});
        try stdout.flush();
        std.process.exit(1);
    };
    const language = lang_support.grammarFn();

    // Read the file.
    const source = source_map.mmapFile(path) catch |err| {
        try stdout.print("Error opening file '{s}': {}\n", .{ path, err });
        try stdout.flush();
        std.process.exit(1);
    };
    defer source_map.unmapFile(source);

    // Parse.
    const tree = ts_api.parseSource(language, source) orelse {
        try stdout.print("Failed to parse '{s}'\n", .{path});
        try stdout.flush();
        std.process.exit(1);
    };
    defer tree.destroy();

    try stdout.print("=== AST: {s} ({} bytes) ===\n\n", .{ path, source.len });

    // Dump the tree recursively.
    const root = tree.rootNode();
    try dumpNode(stdout, source, root, 0);

    try stdout.print("\n", .{});
    try stdout.flush();
}

fn printHelp(stdout: *std.Io.Writer) !void {
    try stdout.print(
        \\dump-ast - Dump the raw tree-sitter AST for a source file.
        \\
        \\USAGE:
        \\    zig build dump-ast -- <path-to-source-file>
        \\
        \\ARGUMENTS:
        \\    <path-to-source-file>    Path to the source file to parse
        \\
        \\OPTIONS:
        \\    -h, --help               Show this help message
        \\
        \\Shows every node kind, line/column positions, and text excerpts.
        \\Useful for understanding what tree-sitter gives us before the
        \\visitor interprets it.
        \\
    , .{});
    try stdout.flush();
}

const max_depth: u32 = 256;

fn dumpNode(stdout: *std.Io.Writer, source: []const u8, node: ts.Node, depth: u32) !void {
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
    if (text.len <= 60 and std.mem.indexOfScalar(u8, text, '\n') == null) {
        try stdout.print("{s}  [{d}:{d}-{d}:{d}]  \"{s}\"\n", .{
            kind, start.row + 1, start.column, end.row + 1, end.column, text,
        });
    } else {
        try stdout.print("{s}  [{d}:{d}-{d}:{d}]  ({d} bytes)\n", .{
            kind, start.row + 1, start.column, end.row + 1, end.column, text.len,
        });
    }

    // Stop recursion at max depth to prevent stack overflow.
    if (depth >= max_depth) {
        var d2: u32 = 0;
        while (d2 < depth + 1) : (d2 += 1) {
            try stdout.print("  ", .{});
        }
        try stdout.print("... (truncated at depth {})\n", .{max_depth});
        return;
    }

    // Recurse into children.
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        try dumpNode(stdout, source, child, depth + 1);
    }
}
