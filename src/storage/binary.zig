const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const node_mod = @import("../core/node.zig");
const edge_mod = @import("../core/edge.zig");
const types = @import("../core/types.zig");
const metrics_mod = @import("../core/metrics.zig");
const lang = @import("../languages/language.zig");

const Graph = graph_mod.Graph;
const Node = node_mod.Node;
const Edge = edge_mod.Edge;
const NodeId = types.NodeId;
const EdgeType = types.EdgeType;
const EdgeSource = types.EdgeSource;
const NodeKind = types.NodeKind;
const Visibility = types.Visibility;
const Metrics = metrics_mod.Metrics;
const LangMeta = lang.LangMeta;
const ExternalInfo = lang.ExternalInfo;

/// Magic bytes identifying a ZCodePrism binary file ("ZCPRISM\0").
pub const MAGIC: [8]u8 = "ZCPRISM\x00".*;

/// Current binary format version.
pub const VERSION: u32 = 1;

/// Fixed-size header at the start of a binary graph file.
/// Contains magic, version, counts, and byte offsets for each table section.
pub const BinaryHeader = struct {
    magic: [8]u8 = MAGIC,
    version: u32 = VERSION,
    flags: u32 = 0,
    node_count: u64 = 0,
    edge_count: u64 = 0,
    node_table_offset: u64 = 0,
    edge_table_offset: u64 = 0,
    metrics_table_offset: u64 = 0,
    string_table_offset: u64 = 0,
    string_table_size: u64 = 0,
};

const HEADER_SIZE: usize = 72;
const NODE_RECORD_SIZE: usize = 96;
const EDGE_RECORD_SIZE: usize = 24;
const METRICS_RECORD_SIZE: usize = 24;

// Node flags bitmask
const FLAG_HAS_CONTENT_HASH: u8 = 0x01;
const FLAG_HAS_METRICS: u8 = 0x02;
const FLAG_HAS_PARENT: u8 = 0x04;
const FLAG_HAS_LINE_START: u8 = 0x08;
const FLAG_HAS_LINE_END: u8 = 0x10;

const StringRef = struct {
    offset: u32,
    len: u32,
};

const NodeRefs = struct {
    name: StringRef,
    file_path: StringRef,
    signature: StringRef,
    doc: StringRef,
    ext_version: StringRef,
    lang_meta: StringRef,
};

const StringTableBuilder = struct {
    bytes: std.ArrayListUnmanaged(u8) = .{},
    index: std.StringHashMapUnmanaged(StringRef) = .{},

    fn deinit(self: *StringTableBuilder, allocator: std.mem.Allocator) void {
        self.index.deinit(allocator);
        self.bytes.deinit(allocator);
    }

    /// Pre-allocate bytes capacity so that intern() never reallocates the byte
    /// buffer, keeping StringHashMap keys (slices into bytes.items) stable.
    fn ensureBytesCapacity(self: *StringTableBuilder, allocator: std.mem.Allocator, capacity: usize) !void {
        try self.bytes.ensureTotalCapacity(allocator, capacity);
    }

    fn intern(self: *StringTableBuilder, allocator: std.mem.Allocator, str: []const u8) !StringRef {
        if (self.index.get(str)) |cached| return cached;
        const offset: u32 = @intCast(self.bytes.items.len);
        self.bytes.appendSliceAssumeCapacity(str);
        const ref = StringRef{ .offset = offset, .len = @intCast(str.len) };
        try self.index.put(allocator, self.bytes.items[offset..][0..str.len], ref);
        return ref;
    }

    fn internOptional(self: *StringTableBuilder, allocator: std.mem.Allocator, str: ?[]const u8) !StringRef {
        if (str) |s| return self.intern(allocator, s);
        return .{ .offset = 0, .len = 0 };
    }
};

fn alignTo8(offset: usize) usize {
    return (offset + 7) & ~@as(usize, 7);
}

fn writeStringRef(buf: []u8, offset: usize, ref: StringRef) void {
    std.mem.writeInt(u32, buf[offset..][0..4], ref.offset, .little);
    std.mem.writeInt(u32, buf[offset + 4 ..][0..4], ref.len, .little);
}

fn readStringRef(buf: []const u8, offset: usize) StringRef {
    return .{
        .offset = std.mem.readInt(u32, buf[offset..][0..4], .little),
        .len = std.mem.readInt(u32, buf[offset + 4 ..][0..4], .little),
    };
}

fn resolveStr(st_data: []const u8, ref: StringRef) error{InvalidFormat}![]const u8 {
    if (ref.len == 0) return "";
    const end: usize = @as(usize, ref.offset) + @as(usize, ref.len);
    if (end > st_data.len) return error.InvalidFormat;
    return st_data[ref.offset..][0..ref.len];
}

fn resolveOptStr(st_data: []const u8, ref: StringRef) error{InvalidFormat}!?[]const u8 {
    if (ref.len == 0) return null;
    const end: usize = @as(usize, ref.offset) + @as(usize, ref.len);
    if (end > st_data.len) return error.InvalidFormat;
    return st_data[ref.offset..][0..ref.len];
}

/// Validate that a StringRef's range falls within the string table.
fn validateStringRef(st_data: []const u8, ref: StringRef) error{InvalidFormat}!void {
    if (ref.len == 0) return;
    const end: usize = @as(usize, ref.offset) + @as(usize, ref.len);
    if (end > st_data.len) return error.InvalidFormat;
}

/// Serialize a graph to the binary storage format and write it to `path`.
///
/// Uses the MAF pattern: measures string table sizes, allocates a single
/// output buffer, fills all table sections, then writes the file atomically.
/// The caller owns `g`; this function does not modify it.
pub fn save(allocator: std.mem.Allocator, g: *const Graph, path: []const u8) !void {
    // Measure: build string table, compute refs per node
    var stb = StringTableBuilder{};
    defer stb.deinit(allocator);

    const nc = g.nodeCount();
    const ec = g.edgeCount();

    const node_refs = try allocator.alloc(NodeRefs, if (nc > 0) nc else 1);
    defer allocator.free(node_refs);

    // Pre-compute upper bound on string table bytes for stable dedup pointers
    var total_string_bytes: usize = 0;
    for (g.nodes.items) |n| {
        total_string_bytes += n.name.len;
        if (n.file_path) |fp| total_string_bytes += fp.len;
        if (n.signature) |s| total_string_bytes += s.len;
        if (n.doc) |d| total_string_bytes += d.len;
        switch (n.external) {
            .dependency => |d| if (d.version) |v| {
                total_string_bytes += v.len;
            },
            else => {},
        }
        total_string_bytes += n.lang_meta.binarySize();
    }
    try stb.ensureBytesCapacity(allocator, total_string_bytes);

    for (g.nodes.items, 0..) |n, i| {
        const ext_version: ?[]const u8 = switch (n.external) {
            .dependency => |d| d.version,
            else => null,
        };
        node_refs[i] = .{
            .name = try stb.intern(allocator, n.name),
            .file_path = try stb.internOptional(allocator, n.file_path),
            .signature = try stb.internOptional(allocator, n.signature),
            .doc = try stb.internOptional(allocator, n.doc),
            .ext_version = try stb.internOptional(allocator, ext_version),
            .lang_meta = blk: {
                var meta_buf: [256]u8 = undefined;
                const meta_len = n.lang_meta.encodeBinary(&meta_buf);
                break :blk if (meta_len > 0) try stb.intern(allocator, meta_buf[0..meta_len]) else .{ .offset = 0, .len = 0 };
            },
        };
    }

    // Compute table offsets with 8-byte alignment
    const node_table_offset = HEADER_SIZE;
    const node_table_size = nc * NODE_RECORD_SIZE;
    const edge_table_offset = alignTo8(node_table_offset + node_table_size);
    const edge_table_size = ec * EDGE_RECORD_SIZE;
    const metrics_table_offset = alignTo8(edge_table_offset + edge_table_size);
    const metrics_table_size = nc * METRICS_RECORD_SIZE;
    const string_table_offset = alignTo8(metrics_table_offset + metrics_table_size);
    const string_table_size = stb.bytes.items.len;
    const total_size = string_table_offset + string_table_size;

    // Allocate
    const buf = try allocator.alloc(u8, if (total_size > 0) total_size else HEADER_SIZE);
    defer allocator.free(buf);
    // Zero only table regions (header is fully written, string table is fully copied)
    if (string_table_offset > node_table_offset) {
        @memset(buf[node_table_offset..string_table_offset], 0);
    }

    // Fill

    // Header
    @memcpy(buf[0..8], &MAGIC);
    std.mem.writeInt(u32, buf[8..12], VERSION, .little);
    std.mem.writeInt(u32, buf[12..16], 0, .little); // flags
    std.mem.writeInt(u64, buf[16..24], @intCast(nc), .little);
    std.mem.writeInt(u64, buf[24..32], @intCast(ec), .little);
    std.mem.writeInt(u64, buf[32..40], @intCast(node_table_offset), .little);
    std.mem.writeInt(u64, buf[40..48], @intCast(edge_table_offset), .little);
    std.mem.writeInt(u64, buf[48..56], @intCast(metrics_table_offset), .little);
    std.mem.writeInt(u64, buf[56..64], @intCast(string_table_offset), .little);
    std.mem.writeInt(u64, buf[64..72], @intCast(string_table_size), .little);

    // Node records
    for (g.nodes.items, 0..) |n, i| {
        const base = node_table_offset + i * NODE_RECORD_SIZE;

        // id (u64)
        std.mem.writeInt(u64, buf[base..][0..8], @intFromEnum(n.id), .little);
        // kind (u8)
        buf[base + 8] = @intFromEnum(n.kind);
        // language (u8)
        buf[base + 9] = @intFromEnum(n.language);
        // visibility (u8)
        buf[base + 10] = @intFromEnum(n.visibility);
        // external_kind (u8)
        buf[base + 11] = switch (n.external) {
            .none => 0,
            .stdlib => 1,
            .dependency => 2,
        };
        // flags (u8)
        var flags: u8 = 0;
        if (n.content_hash != null) flags |= FLAG_HAS_CONTENT_HASH;
        if (n.metrics != null) flags |= FLAG_HAS_METRICS;
        if (n.parent_id != null) flags |= FLAG_HAS_PARENT;
        if (n.line_start != null) flags |= FLAG_HAS_LINE_START;
        if (n.line_end != null) flags |= FLAG_HAS_LINE_END;
        buf[base + 12] = flags;
        // padding [13..16] already zero
        // parent_id (u64) at offset 16
        if (n.parent_id) |pid| {
            std.mem.writeInt(u64, buf[base + 16 ..][0..8], @intFromEnum(pid), .little);
        }
        // line_start (u32) at offset 24
        if (n.line_start) |ls| {
            std.mem.writeInt(u32, buf[base + 24 ..][0..4], ls, .little);
        }
        // line_end (u32) at offset 28
        if (n.line_end) |le| {
            std.mem.writeInt(u32, buf[base + 28 ..][0..4], le, .little);
        }
        // content_hash ([12]u8) at offset 32
        if (n.content_hash) |ch| {
            @memcpy(buf[base + 32 ..][0..12], &ch);
        }
        // String refs: 6 refs x 8 bytes each starting at offset 44
        const refs = node_refs[i];
        writeStringRef(buf, base + 44, refs.name);
        writeStringRef(buf, base + 52, refs.file_path);
        writeStringRef(buf, base + 60, refs.signature);
        writeStringRef(buf, base + 68, refs.doc);
        writeStringRef(buf, base + 76, refs.ext_version);
        writeStringRef(buf, base + 84, refs.lang_meta);
        // padding [92..96] already zero
    }

    // Edge records
    for (g.edges.items, 0..) |e, i| {
        const base = edge_table_offset + i * EDGE_RECORD_SIZE;
        std.mem.writeInt(u64, buf[base..][0..8], @intFromEnum(e.source_id), .little);
        std.mem.writeInt(u64, buf[base + 8 ..][0..8], @intFromEnum(e.target_id), .little);
        buf[base + 16] = @intFromEnum(e.edge_type);
        buf[base + 17] = @intFromEnum(e.source);
        // padding [18..24] already zero
    }

    // Metrics records (one per node, same index)
    for (g.nodes.items, 0..) |n, i| {
        if (n.metrics) |m| {
            const base = metrics_table_offset + i * METRICS_RECORD_SIZE;
            std.mem.writeInt(u16, buf[base..][0..2], m.complexity, .little);
            std.mem.writeInt(u32, buf[base + 2 ..][0..4], m.lines, .little);
            std.mem.writeInt(u16, buf[base + 6 ..][0..2], m.fan_in, .little);
            std.mem.writeInt(u16, buf[base + 8 ..][0..2], m.fan_out, .little);
            std.mem.writeInt(u16, buf[base + 10 ..][0..2], m.branches, .little);
            std.mem.writeInt(u16, buf[base + 12 ..][0..2], m.loops, .little);
            std.mem.writeInt(u16, buf[base + 14 ..][0..2], m.error_paths, .little);
            buf[base + 16] = m.nesting_depth_max;
            // padding at 17 already zero
            std.mem.writeInt(u32, buf[base + 18 ..][0..4], m.structural_hash, .little);
            // padding [22..24] already zero
        }
    }

    // String table
    if (string_table_size > 0) {
        @memcpy(buf[string_table_offset..][0..string_table_size], stb.bytes.items);
    }

    // Write to file
    const file = try std.fs.cwd().createFile(path, .{});
    defer file.close();
    try file.writeAll(buf);
}

/// Deserialize a graph from a binary file at `path`.
///
/// Validates the header magic, version, and table bounds before parsing.
/// Returns `error.InvalidMagic` or `error.UnsupportedVersion` for header
/// mismatches, and `error.InvalidFormat` for truncated or corrupt data.
/// The caller owns the returned Graph and must call `deinit()` on it.
pub fn load(allocator: std.mem.Allocator, path: []const u8) !Graph {
    // Read file
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const stat = try file.stat();
    const file_size: usize = @intCast(stat.size);
    if (file_size < HEADER_SIZE) return error.InvalidFormat;

    const buf = try allocator.alloc(u8, file_size);
    defer allocator.free(buf);
    const bytes_read = try file.readAll(buf);
    if (bytes_read < HEADER_SIZE) return error.InvalidFormat;

    // Validate header
    if (!std.mem.eql(u8, buf[0..8], &MAGIC)) return error.InvalidMagic;
    const version = std.mem.readInt(u32, buf[8..12], .little);
    if (version != VERSION) return error.UnsupportedVersion;

    // Parse header fields
    const nc: usize = @intCast(std.mem.readInt(u64, buf[16..24], .little));
    const ec: usize = @intCast(std.mem.readInt(u64, buf[24..32], .little));
    const nto: usize = @intCast(std.mem.readInt(u64, buf[32..40], .little));
    const eto: usize = @intCast(std.mem.readInt(u64, buf[40..48], .little));
    const mto: usize = @intCast(std.mem.readInt(u64, buf[48..56], .little));
    const sto: usize = @intCast(std.mem.readInt(u64, buf[56..64], .little));
    const st_size: usize = @intCast(std.mem.readInt(u64, buf[64..72], .little));

    // Validate that all table regions fit within the file buffer.
    // Use overflow-safe arithmetic since counts are read from untrusted data.
    const node_table_end = std.math.add(usize, nto, std.math.mul(usize, nc, NODE_RECORD_SIZE) catch return error.InvalidFormat) catch return error.InvalidFormat;
    const edge_table_end = std.math.add(usize, eto, std.math.mul(usize, ec, EDGE_RECORD_SIZE) catch return error.InvalidFormat) catch return error.InvalidFormat;
    const metrics_table_end = std.math.add(usize, mto, std.math.mul(usize, nc, METRICS_RECORD_SIZE) catch return error.InvalidFormat) catch return error.InvalidFormat;
    const string_table_end = std.math.add(usize, sto, st_size) catch return error.InvalidFormat;

    if (node_table_end > bytes_read or
        edge_table_end > bytes_read or
        metrics_table_end > bytes_read or
        string_table_end > bytes_read) return error.InvalidFormat;

    var g = Graph.init(allocator, "");
    errdefer g.deinit();

    try g.nodes.ensureTotalCapacity(allocator, nc);
    try g.edges.ensureTotalCapacity(allocator, ec);

    // Single dupe of string table region; all node strings resolve into this buffer
    const st_data: []const u8 = if (st_size > 0) blk: {
        const data = try g.allocator.dupe(u8, buf[sto..][0..st_size]);
        errdefer g.allocator.free(data);
        try g.addOwnedBuffer(data);
        break :blk data;
    } else "";

    // Parse nodes
    for (0..nc) |i| {
        const base = nto + i * NODE_RECORD_SIZE;

        const flags = buf[base + 12];
        const has_content_hash = flags & FLAG_HAS_CONTENT_HASH != 0;
        const has_metrics = flags & FLAG_HAS_METRICS != 0;
        const has_parent = flags & FLAG_HAS_PARENT != 0;
        const has_line_start = flags & FLAG_HAS_LINE_START != 0;
        const has_line_end = flags & FLAG_HAS_LINE_END != 0;

        // String refs
        const name_ref = readStringRef(buf, base + 44);
        const file_path_ref = readStringRef(buf, base + 52);
        const sig_ref = readStringRef(buf, base + 60);
        const doc_ref = readStringRef(buf, base + 68);
        const ext_ver_ref = readStringRef(buf, base + 76);
        const lang_meta_ref = readStringRef(buf, base + 84);

        // Resolve strings from duped string table (zero additional allocations)
        const name = try resolveStr(st_data, name_ref);
        const file_path = try resolveOptStr(st_data, file_path_ref);
        const signature = try resolveOptStr(st_data, sig_ref);
        const doc = try resolveOptStr(st_data, doc_ref);

        // External info
        const external_kind = buf[base + 11];
        const external: ExternalInfo = switch (external_kind) {
            0 => .{ .none = {} },
            1 => .{ .stdlib = {} },
            2 => .{ .dependency = .{ .version = try resolveOptStr(st_data, ext_ver_ref) } },
            else => .{ .none = {} },
        };

        // LangMeta
        const lang_meta: LangMeta = if (lang_meta_ref.len > 0) blk: {
            try validateStringRef(st_data, lang_meta_ref);
            break :blk LangMeta.decodeBinary(st_data[lang_meta_ref.offset..][0..lang_meta_ref.len]);
        } else .{ .none = {} };

        // Metrics
        const metrics: ?Metrics = if (has_metrics) blk: {
            const mbase = mto + i * METRICS_RECORD_SIZE;
            break :blk .{
                .complexity = std.mem.readInt(u16, buf[mbase..][0..2], .little),
                .lines = std.mem.readInt(u32, buf[mbase + 2 ..][0..4], .little),
                .fan_in = std.mem.readInt(u16, buf[mbase + 6 ..][0..2], .little),
                .fan_out = std.mem.readInt(u16, buf[mbase + 8 ..][0..2], .little),
                .branches = std.mem.readInt(u16, buf[mbase + 10 ..][0..2], .little),
                .loops = std.mem.readInt(u16, buf[mbase + 12 ..][0..2], .little),
                .error_paths = std.mem.readInt(u16, buf[mbase + 14 ..][0..2], .little),
                .nesting_depth_max = buf[mbase + 16],
                .structural_hash = std.mem.readInt(u32, buf[mbase + 18 ..][0..4], .little),
            };
        } else null;

        g.nodes.appendAssumeCapacity(.{
            .id = @enumFromInt(std.mem.readInt(u64, buf[base..][0..8], .little)),
            .name = name,
            .kind = std.meta.intToEnum(NodeKind, buf[base + 8]) catch return error.InvalidFormat,
            .language = std.meta.intToEnum(types.Language, buf[base + 9]) catch return error.InvalidFormat,
            .file_path = file_path,
            .line_start = if (has_line_start) std.mem.readInt(u32, buf[base + 24 ..][0..4], .little) else null,
            .line_end = if (has_line_end) std.mem.readInt(u32, buf[base + 28 ..][0..4], .little) else null,
            .parent_id = if (has_parent) @as(NodeId, @enumFromInt(std.mem.readInt(u64, buf[base + 16 ..][0..8], .little))) else null,
            .visibility = std.meta.intToEnum(Visibility, buf[base + 10]) catch return error.InvalidFormat,
            .doc = doc,
            .signature = signature,
            .content_hash = if (has_content_hash) buf[base + 32 ..][0..12].* else null,
            .metrics = metrics,
            .lang_meta = lang_meta,
            .external = external,
        });
    }

    // Parse edges, skipping any that reference out-of-bounds node IDs.
    for (0..ec) |i| {
        const base = eto + i * EDGE_RECORD_SIZE;
        const src_id = std.mem.readInt(u64, buf[base..][0..8], .little);
        const tgt_id = std.mem.readInt(u64, buf[base + 8 ..][0..8], .little);
        if (src_id >= nc or tgt_id >= nc) continue;
        g.edges.appendAssumeCapacity(.{
            .source_id = @enumFromInt(src_id),
            .target_id = @enumFromInt(tgt_id),
            .edge_type = std.meta.intToEnum(EdgeType, buf[base + 16]) catch return error.InvalidFormat,
            .source = std.meta.intToEnum(EdgeSource, buf[base + 17]) catch return error.InvalidFormat,
        });
    }

    try g.rebuildEdgeIndex();
    try g.freeze();
    return g;
}

/// Append new nodes and edges to an existing binary file at `path`.
///
/// Loads the current file, merges in the nodes and edges from `g`,
/// then performs a full save (compaction) back to the same path.
pub fn append(allocator: std.mem.Allocator, g: *const Graph, path: []const u8) !void {
    // Load existing graph
    var existing = try load(allocator, path);
    defer existing.deinit();

    // Merge new nodes
    for (g.nodes.items) |n| {
        _ = try existing.addNode(n);
    }

    // Merge new edges
    for (g.edges.items) |e| {
        _ = try existing.addEdge(e);
    }

    // Full save (compaction)
    try save(allocator, &existing, path);
}

/// Build a test graph with 3 diverse nodes and 2 edges for use in tests.
fn createTestGraph(allocator: std.mem.Allocator) !Graph {
    var g = Graph.init(allocator, "/tmp/test-project");

    // Node 0: file node (minimal fields)
    _ = try g.addNode(.{
        .id = .root,
        .name = "main.zig",
        .kind = .file,
        .language = .zig,
        .visibility = .public,
        .file_path = "src/main.zig",
        .line_start = 1,
        .line_end = 100,
    });

    // Node 1: function with metrics, doc, signature, and ZigMeta
    _ = try g.addNode(.{
        .id = .root,
        .name = "process",
        .kind = .function,
        .language = .zig,
        .visibility = .public,
        .file_path = "src/main.zig",
        .line_start = 10,
        .line_end = 50,
        .parent_id = @enumFromInt(0),
        .doc = "/// Process the input data.",
        .signature = "pub fn process(data: []const u8) !void",
        .content_hash = "abcdefghijkl".*,
        .metrics = .{
            .complexity = 5,
            .lines = 40,
            .fan_in = 2,
            .fan_out = 3,
            .branches = 4,
            .loops = 1,
            .error_paths = 2,
            .nesting_depth_max = 3,
            .structural_hash = 0xCAFEBABE,
        },
        .lang_meta = .{ .zig = .{ .is_comptime = false, .is_inline = true } },
    });

    // Node 2: type_def with external=none
    _ = try g.addNode(.{
        .id = .root,
        .name = "Config",
        .kind = .type_def,
        .language = .zig,
        .visibility = .private,
        .file_path = "src/main.zig",
        .line_start = 55,
        .line_end = 70,
        .parent_id = @enumFromInt(0),
    });

    // Edge 0: function uses type
    _ = try g.addEdge(.{
        .source_id = @enumFromInt(1),
        .target_id = @enumFromInt(2),
        .edge_type = .uses_type,
        .source = .tree_sitter,
    });

    // Edge 1: file exports function
    _ = try g.addEdge(.{
        .source_id = @enumFromInt(0),
        .target_id = @enumFromInt(1),
        .edge_type = .exports,
        .source = .tree_sitter,
    });

    return g;
}

// Nominal tests

test "binary round-trip preserves nodes, edges, and metrics" {
    // Arrange
    var g = try createTestGraph(std.testing.allocator);
    defer g.deinit();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Act
    try save(std.testing.allocator, &g, file_path);
    var loaded = try load(std.testing.allocator, file_path);
    defer loaded.deinit();

    // Assert: nodes
    try std.testing.expectEqual(g.nodeCount(), loaded.nodeCount());
    for (g.nodes.items, loaded.nodes.items) |original, restored| {
        try std.testing.expectEqualStrings(original.name, restored.name);
        try std.testing.expectEqual(original.kind, restored.kind);
        try std.testing.expectEqual(original.language, restored.language);
        try std.testing.expectEqual(original.visibility, restored.visibility);
        try std.testing.expectEqual(original.parent_id, restored.parent_id);
        try std.testing.expectEqual(original.line_start, restored.line_start);
        try std.testing.expectEqual(original.line_end, restored.line_end);
    }

    // Assert: edges
    try std.testing.expectEqual(g.edgeCount(), loaded.edgeCount());
    for (g.edges.items, loaded.edges.items) |original, restored| {
        try std.testing.expectEqual(original.source_id, restored.source_id);
        try std.testing.expectEqual(original.target_id, restored.target_id);
        try std.testing.expectEqual(original.edge_type, restored.edge_type);
        try std.testing.expectEqual(original.source, restored.source);
    }

    // Assert: metrics (node 1 has metrics)
    const original_metrics = g.getNode(@enumFromInt(1)).?.metrics.?;
    const loaded_metrics = loaded.getNode(@enumFromInt(1)).?.metrics.?;
    try std.testing.expectEqual(original_metrics.complexity, loaded_metrics.complexity);
    try std.testing.expectEqual(original_metrics.lines, loaded_metrics.lines);
    try std.testing.expectEqual(original_metrics.fan_in, loaded_metrics.fan_in);
    try std.testing.expectEqual(original_metrics.fan_out, loaded_metrics.fan_out);
    try std.testing.expectEqual(original_metrics.branches, loaded_metrics.branches);
    try std.testing.expectEqual(original_metrics.loops, loaded_metrics.loops);
    try std.testing.expectEqual(original_metrics.error_paths, loaded_metrics.error_paths);
    try std.testing.expectEqual(original_metrics.nesting_depth_max, loaded_metrics.nesting_depth_max);
    try std.testing.expectEqual(original_metrics.structural_hash, loaded_metrics.structural_hash);
}

test "binary header has correct magic, version, and counts" {
    // Arrange
    var g = try createTestGraph(std.testing.allocator);
    defer g.deinit();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Act
    try save(std.testing.allocator, &g, file_path);

    // Assert: magic bytes
    const file = try tmp.dir.openFile("test.bin", .{});
    defer file.close();
    var header_buf: [12]u8 = undefined;
    const bytes_read = try file.readAll(&header_buf);
    try std.testing.expectEqual(@as(usize, 12), bytes_read);
    try std.testing.expectEqualSlices(u8, &MAGIC, header_buf[0..8]);

    // Assert: version
    const version = std.mem.readInt(u32, header_buf[8..12], .little);
    try std.testing.expectEqual(VERSION, version);

    // Assert: counts
    var loaded = try load(std.testing.allocator, file_path);
    defer loaded.deinit();
    try std.testing.expectEqual(@as(usize, 3), loaded.nodeCount());
    try std.testing.expectEqual(@as(usize, 2), loaded.edgeCount());
}

// Edge case tests

test "binary save/load empty graph" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/test-project");
    defer g.deinit();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Act
    try save(std.testing.allocator, &g, file_path);
    var loaded = try load(std.testing.allocator, file_path);
    defer loaded.deinit();

    // Assert
    try std.testing.expectEqual(@as(usize, 0), loaded.nodeCount());
    try std.testing.expectEqual(@as(usize, 0), loaded.edgeCount());
}

test "binary save/load single node" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/test-project");
    defer g.deinit();

    _ = try g.addNode(.{
        .id = .root,
        .name = "only_node",
        .kind = .file,
        .language = .zig,
    });

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Act
    try save(std.testing.allocator, &g, file_path);
    var loaded = try load(std.testing.allocator, file_path);
    defer loaded.deinit();

    // Assert
    try std.testing.expectEqual(@as(usize, 1), loaded.nodeCount());
    try std.testing.expectEqual(@as(usize, 0), loaded.edgeCount());
    try std.testing.expectEqualStrings("only_node", loaded.getNode(.root).?.name);
}

test "binary preserves phantom nodes" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/test-project");
    defer g.deinit();

    _ = try g.addNode(.{
        .id = .root,
        .name = "std",
        .kind = .module,
        .language = .zig,
        .external = .{ .stdlib = {} },
    });

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Act
    try save(std.testing.allocator, &g, file_path);
    var loaded = try load(std.testing.allocator, file_path);
    defer loaded.deinit();

    // Assert
    const loaded_node = loaded.getNode(.root).?;
    try std.testing.expectEqual(ExternalInfo.stdlib, loaded_node.external);
}

test "binary preserves null optional fields" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/test-project");
    defer g.deinit();

    _ = try g.addNode(.{
        .id = .root,
        .name = "bare",
        .kind = .function,
        .language = .zig,
        // All optional fields left at default null
    });

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Act
    try save(std.testing.allocator, &g, file_path);
    var loaded = try load(std.testing.allocator, file_path);
    defer loaded.deinit();

    // Assert
    const n = loaded.getNode(.root).?;
    try std.testing.expectEqual(@as(?[]const u8, null), n.doc);
    try std.testing.expectEqual(@as(?[]const u8, null), n.signature);
    try std.testing.expectEqual(@as(?[12]u8, null), n.content_hash);
    try std.testing.expectEqual(@as(?Metrics, null), n.metrics);
}

test "binary preserves long strings" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/test-project");
    defer g.deinit();

    // Create a 10KB doc comment
    const long_doc = try std.testing.allocator.alloc(u8, 10 * 1024);
    defer std.testing.allocator.free(long_doc);
    @memset(long_doc, 'A');

    _ = try g.addNode(.{
        .id = .root,
        .name = "verbose",
        .kind = .function,
        .language = .zig,
        .doc = long_doc,
    });

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Act
    try save(std.testing.allocator, &g, file_path);
    var loaded = try load(std.testing.allocator, file_path);
    defer loaded.deinit();

    // Assert
    const loaded_doc = loaded.getNode(.root).?.doc.?;
    try std.testing.expectEqual(@as(usize, 10 * 1024), loaded_doc.len);
    try std.testing.expectEqualSlices(u8, long_doc, loaded_doc);
}

test "binary preserves ZigMeta" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/test-project");
    defer g.deinit();

    _ = try g.addNode(.{
        .id = .root,
        .name = "comptime_fn",
        .kind = .function,
        .language = .zig,
        .lang_meta = .{ .zig = .{ .is_comptime = true } },
    });

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Act
    try save(std.testing.allocator, &g, file_path);
    var loaded = try load(std.testing.allocator, file_path);
    defer loaded.deinit();

    // Assert
    const meta = loaded.getNode(.root).?.lang_meta;
    try std.testing.expect(meta.zig.is_comptime);
}

test "binary preserves LangMeta.none" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/test-project");
    defer g.deinit();

    _ = try g.addNode(.{
        .id = .root,
        .name = "plain",
        .kind = .function,
        .language = .zig,
        .lang_meta = .{ .none = {} },
    });

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Act
    try save(std.testing.allocator, &g, file_path);
    var loaded = try load(std.testing.allocator, file_path);
    defer loaded.deinit();

    // Assert
    try std.testing.expectEqual(LangMeta.none, loaded.getNode(.root).?.lang_meta);
}

test "binary round-trip preserves union_def kind" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/test-project");
    defer g.deinit();

    _ = try g.addNode(.{
        .id = .root,
        .name = "MyUnion",
        .kind = .union_def,
        .language = .zig,
        .visibility = .public,
        .file_path = "src/main.zig",
    });

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Act
    try save(std.testing.allocator, &g, file_path);
    var loaded = try load(std.testing.allocator, file_path);
    defer loaded.deinit();

    // Assert
    try std.testing.expectEqual(NodeKind.union_def, loaded.getNode(.root).?.kind);
}

test "binary round-trip preserves is_packed metadata" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/test-project");
    defer g.deinit();

    _ = try g.addNode(.{
        .id = .root,
        .name = "PackedStruct",
        .kind = .type_def,
        .language = .zig,
        .lang_meta = .{ .zig = .{ .is_packed = true } },
    });

    _ = try g.addNode(.{
        .id = .root,
        .name = "ExternStruct",
        .kind = .type_def,
        .language = .zig,
        .lang_meta = .{ .zig = .{ .is_extern = true } },
    });

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Act
    try save(std.testing.allocator, &g, file_path);
    var loaded = try load(std.testing.allocator, file_path);
    defer loaded.deinit();

    // Assert
    const packed_meta = loaded.getNode(@enumFromInt(0)).?.lang_meta;
    try std.testing.expect(packed_meta.zig.is_packed);
    try std.testing.expect(!packed_meta.zig.is_extern);

    const extern_meta = loaded.getNode(@enumFromInt(1)).?.lang_meta;
    try std.testing.expect(extern_meta.zig.is_extern);
    try std.testing.expect(!extern_meta.zig.is_packed);
}

// Append tests

test "append adds new nodes" {
    // Arrange
    var g = try createTestGraph(std.testing.allocator);
    defer g.deinit();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Save initial 3 nodes
    try save(std.testing.allocator, &g, file_path);

    // Build a graph with 2 additional nodes
    var extra = Graph.init(std.testing.allocator, "/tmp/test-project");
    defer extra.deinit();
    _ = try extra.addNode(.{
        .id = .root,
        .name = "helper",
        .kind = .function,
        .language = .zig,
    });
    _ = try extra.addNode(.{
        .id = .root,
        .name = "util",
        .kind = .function,
        .language = .zig,
    });

    // Act
    try append(std.testing.allocator, &extra, file_path);

    // Assert
    var loaded = try load(std.testing.allocator, file_path);
    defer loaded.deinit();
    try std.testing.expectEqual(@as(usize, 5), loaded.nodeCount());
}

test "compaction after append" {
    // Arrange
    var g = try createTestGraph(std.testing.allocator);
    defer g.deinit();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Save initial graph
    try save(std.testing.allocator, &g, file_path);

    // Append multiple small graphs
    var extra1 = Graph.init(std.testing.allocator, "/tmp/test-project");
    defer extra1.deinit();
    _ = try extra1.addNode(.{ .id = .root, .name = "a1", .kind = .function, .language = .zig });
    try append(std.testing.allocator, &extra1, file_path);

    var extra2 = Graph.init(std.testing.allocator, "/tmp/test-project");
    defer extra2.deinit();
    _ = try extra2.addNode(.{ .id = .root, .name = "a2", .kind = .function, .language = .zig });
    try append(std.testing.allocator, &extra2, file_path);

    // Act: full save (compaction) after appends
    var loaded_pre = try load(std.testing.allocator, file_path);
    defer loaded_pre.deinit();
    try save(std.testing.allocator, &loaded_pre, file_path);

    // Assert: data preserved after compaction
    var loaded_post = try load(std.testing.allocator, file_path);
    defer loaded_post.deinit();
    try std.testing.expectEqual(@as(usize, 5), loaded_post.nodeCount());
}

test "load rejects truncated file with table regions past EOF" {
    // Arrange: valid header claiming 1 node, but file is only the header
    var header: [HEADER_SIZE]u8 = undefined;
    @memset(&header, 0);
    @memcpy(header[0..8], &MAGIC);
    std.mem.writeInt(u32, header[8..12], VERSION, .little);
    std.mem.writeInt(u64, header[16..24], 1, .little); // nc = 1
    std.mem.writeInt(u64, header[32..40], HEADER_SIZE, .little); // nto
    std.mem.writeInt(u64, header[40..48], HEADER_SIZE, .little); // eto
    std.mem.writeInt(u64, header[48..56], HEADER_SIZE, .little); // mto
    std.mem.writeInt(u64, header[56..64], HEADER_SIZE, .little); // sto

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/malformed.bin", .{path});
    defer std.testing.allocator.free(file_path);

    const file = try tmp.dir.createFile("malformed.bin", .{});
    defer file.close();
    try file.writeAll(&header);

    // Act / Assert: node table (72 + 96 = 168) exceeds 72-byte file
    const result = load(std.testing.allocator, file_path);
    try std.testing.expectError(error.InvalidFormat, result);
}

test "load rejects corrupt string ref past string table" {
    // Arrange: save a valid graph, then corrupt a name StringRef
    var g = Graph.init(std.testing.allocator, "/tmp/test-project");
    defer g.deinit();
    _ = try g.addNode(.{ .id = .root, .name = "n", .kind = .file, .language = .zig });

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/malformed.bin", .{path});
    defer std.testing.allocator.free(file_path);

    try save(std.testing.allocator, &g, file_path);

    // Overwrite name StringRef (node base + 44) with out-of-bounds offset
    const raw_file = try std.fs.cwd().openFile(file_path, .{ .mode = .read_write });
    defer raw_file.close();
    try raw_file.seekTo(HEADER_SIZE + 44);
    var ref_buf: [8]u8 = undefined;
    std.mem.writeInt(u32, ref_buf[0..4], 0xFFFF, .little);
    std.mem.writeInt(u32, ref_buf[4..8], 10, .little);
    try raw_file.writeAll(&ref_buf);

    // Act / Assert
    const result = load(std.testing.allocator, file_path);
    try std.testing.expectError(error.InvalidFormat, result);
}

test "load rejects invalid enum discriminant" {
    // Arrange: save a valid graph, then corrupt the NodeKind byte
    var g = Graph.init(std.testing.allocator, "/tmp/test-project");
    defer g.deinit();
    _ = try g.addNode(.{ .id = .root, .name = "n", .kind = .file, .language = .zig });

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/malformed.bin", .{path});
    defer std.testing.allocator.free(file_path);

    try save(std.testing.allocator, &g, file_path);

    // Overwrite NodeKind byte (node base + 8) with invalid value
    const raw_file = try std.fs.cwd().openFile(file_path, .{ .mode = .read_write });
    defer raw_file.close();
    try raw_file.seekTo(HEADER_SIZE + 8);
    try raw_file.writeAll(&[_]u8{0xFF});

    // Act / Assert
    const result = load(std.testing.allocator, file_path);
    try std.testing.expectError(error.InvalidFormat, result);
}
