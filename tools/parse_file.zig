//! Quick standalone tool to test the single-file Zig visitor.
//! Usage: zig build run-parse-file -- <path-to-zig-file>
//!
//! Reads a .zig file, parses it with the visitor, and dumps all
//! nodes and edges to stdout.

const std = @import("std");
const zcodeprism = @import("zcodeprism");

const Graph = zcodeprism.Graph;
const Node = zcodeprism.Node;
const NodeKind = zcodeprism.NodeKind;
const EdgeType = zcodeprism.EdgeType;
const Visibility = zcodeprism.Visibility;
const visitor = zcodeprism.visitor;
const source_map = zcodeprism.source_map;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var stdout_buffer: [8192]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    // Get file path from args.
    var args = std.process.args();
    _ = args.next(); // skip program name
    const file_path = args.next() orelse {
        try stdout.print("Usage: parse_file <path-to-zig-file>\n", .{});
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
    var graph = Graph.init(allocator, ".");
    defer graph.deinit();

    visitor.parse(source, &graph) catch |err| {
        try stdout.print("Parse error: {}\n", .{err});
        try stdout.flush();
        return;
    };

    // Dump results.
    try stdout.print("=== File: {s} ===\n", .{file_path});
    try stdout.print("Source size: {} bytes\n", .{source.len});
    try stdout.print("Nodes: {}\n", .{graph.nodes.items.len});
    try stdout.print("Edges: {}\n\n", .{graph.edges.items.len});

    // Count by kind.
    var kind_counts: [@typeInfo(NodeKind).@"enum".fields.len]u32 = .{0} ** @typeInfo(NodeKind).@"enum".fields.len;
    for (graph.nodes.items) |n| {
        kind_counts[@intFromEnum(n.kind)] += 1;
    }
    try stdout.print("--- Node counts by kind ---\n", .{});
    inline for (@typeInfo(NodeKind).@"enum".fields, 0..) |f, i| {
        if (kind_counts[i] > 0) {
            try stdout.print("  {s}: {}\n", .{ f.name, kind_counts[i] });
        }
    }

    // Count by edge type.
    var edge_counts: [@typeInfo(EdgeType).@"enum".fields.len]u32 = .{0} ** @typeInfo(EdgeType).@"enum".fields.len;
    for (graph.edges.items) |e| {
        edge_counts[@intFromEnum(e.edge_type)] += 1;
    }
    try stdout.print("\n--- Edge counts by type ---\n", .{});
    inline for (@typeInfo(EdgeType).@"enum".fields, 0..) |f, i| {
        if (edge_counts[i] > 0) {
            try stdout.print("  {s}: {}\n", .{ f.name, edge_counts[i] });
        }
    }

    // List all nodes.
    try stdout.print("\n--- All nodes ---\n", .{});
    for (graph.nodes.items) |n| {
        const vis_str: []const u8 = if (n.visibility == .public) "pub" else "prv";
        try stdout.print("  [{d:>3}] {s:<12} {s} \"{s}\"", .{
            @intFromEnum(n.id),
            @tagName(n.kind),
            vis_str,
            n.name,
        });
        if (n.line_start) |ls| {
            if (n.line_end) |le| {
                try stdout.print("  L{}-{}", .{ ls, le });
            } else {
                try stdout.print("  L{}", .{ls});
            }
        }
        if (n.parent_id) |pid| {
            try stdout.print("  parent={}", .{@intFromEnum(pid)});
        }
        if (n.doc != null) {
            try stdout.print("  [has doc]", .{});
        }
        if (n.signature) |sig| {
            try stdout.print("  sig=\"{s}\"", .{sig[0..@min(sig.len, 60)]});
        }
        // Check lang_meta for comptime
        switch (n.lang_meta) {
            .zig => |zm| {
                if (zm.is_comptime) try stdout.print("  [comptime]", .{});
            },
            else => {},
        }
        try stdout.print("\n", .{});
    }

    // List all edges.
    if (graph.edges.items.len > 0) {
        try stdout.print("\n--- All edges ---\n", .{});
        for (graph.edges.items) |e| {
            // Get node names for readability.
            const src_name = if (graph.getNode(e.source_id)) |n| n.name else "?";
            const tgt_name = if (graph.getNode(e.target_id)) |n| n.name else "?";
            try stdout.print("  {d} ({s}) --[{s}]--> {d} ({s})\n", .{
                @intFromEnum(e.source_id),
                src_name,
                @tagName(e.edge_type),
                @intFromEnum(e.target_id),
                tgt_name,
            });
        }
    }

    try stdout.print("\n", .{});
    try stdout.flush();
}
