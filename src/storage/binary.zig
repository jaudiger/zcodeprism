const std = @import("std");
const atomic_file = @import("atomic_file.zig");
const graph_mod = @import("../core/graph.zig");
const node_mod = @import("../core/node.zig");
const edge_mod = @import("../core/edge.zig");
const types = @import("../core/types.zig");
const metrics_mod = @import("../core/metrics.zig");
const lang_meta_mod = @import("../languages/lang_meta.zig");
const zig_meta_mod = @import("../languages/zig/meta.zig");
const external_mod = @import("../core/external.zig");

const Graph = graph_mod.Graph;
const FrozenGraph = graph_mod.FrozenGraph;
const Node = node_mod.Node;
const Edge = edge_mod.Edge;
const NodeId = types.NodeId;
const EdgeType = types.EdgeType;
const EdgeSource = types.EdgeSource;
const NodeKind = types.NodeKind;
const Visibility = types.Visibility;
const Metrics = metrics_mod.Metrics;
const ExternalInfo = external_mod.ExternalInfo;

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
    project_root: StringRef = .{ .offset = 0, .len = 0 },
};

const HEADER_SIZE: usize = 80;
const NODE_RECORD_SIZE: usize = 128;
const EDGE_RECORD_SIZE: usize = 32;
const METRICS_RECORD_SIZE: usize = 28;

// Node record layout (128 bytes):
//   [0..8]   id (u64)
//   [8..16]  parent_id (u64, present when FLAG_HAS_PARENT)
//   [16..20] line_start (u32, present when FLAG_HAS_LINE_START)
//   [20..24] line_end   (u32, present when FLAG_HAS_LINE_END)
//   [24..28] col_start  (u32, present when FLAG_HAS_COL_START)
//   [28..32] col_end    (u32, present when FLAG_HAS_COL_END)
//   [32]     external_kind (u8)
//   [33]     flags (u8)
//   [34..36] padding
//   [36..52] content_hash (16 bytes, present when FLAG_HAS_CONTENT_HASH)
//   [52..56] padding
//   [56..64] kind StringRef
//   [64..72] language StringRef
//   [72..80] visibility StringRef
//   [80..88] name StringRef
//   [88..96] file_path StringRef
//   [96..104] signature StringRef
//   [104..112] doc StringRef
//   [112..120] ext_version StringRef
//   [120..128] lang_meta StringRef

// Node flags bitmask
const FLAG_HAS_CONTENT_HASH: u8 = 0x01;
const FLAG_HAS_METRICS: u8 = 0x02;
const FLAG_HAS_PARENT: u8 = 0x04;
const FLAG_HAS_LINE_START: u8 = 0x08;
const FLAG_HAS_LINE_END: u8 = 0x10;
const FLAG_HAS_COL_START: u8 = 0x20;
const FLAG_HAS_COL_END: u8 = 0x40;

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
    kind: StringRef,
    language: StringRef,
    visibility: StringRef,
};

const StringTable = struct {
    bytes: std.ArrayList(u8),
    index: std.array_hash_map.String(StringRef),

    fn init(allocator: std.mem.Allocator, capacity: usize) !StringTable {
        var bytes: std.ArrayList(u8) = .empty;
        try bytes.ensureTotalCapacity(allocator, capacity);
        return .{
            .bytes = bytes,
            .index = .empty,
        };
    }

    fn deinit(self: *StringTable, allocator: std.mem.Allocator) void {
        self.index.deinit(allocator);
        self.bytes.deinit(allocator);
    }

    fn intern(self: *StringTable, allocator: std.mem.Allocator, str: []const u8) !StringRef {
        if (self.index.get(str)) |cached| return cached;
        std.debug.assert(self.bytes.items.len + str.len <= self.bytes.capacity);
        const offset: u32 = @intCast(self.bytes.items.len);
        self.bytes.appendSliceAssumeCapacity(str);
        const ref = StringRef{ .offset = offset, .len = @intCast(str.len) };
        try self.index.put(allocator, self.bytes.items[offset..][0..str.len], ref);
        return ref;
    }

    fn internOptional(self: *StringTable, allocator: std.mem.Allocator, str: ?[]const u8) !StringRef {
        if (str) |s| return self.intern(allocator, s);
        return .{ .offset = 0, .len = 0 };
    }
};

fn writeHeader(buf: []u8, h: BinaryHeader) void {
    @memcpy(buf[0..8], &h.magic);
    std.mem.writeInt(u32, buf[8..12], h.version, .little);
    std.mem.writeInt(u32, buf[12..16], h.flags, .little);
    std.mem.writeInt(u64, buf[16..24], h.node_count, .little);
    std.mem.writeInt(u64, buf[24..32], h.edge_count, .little);
    std.mem.writeInt(u64, buf[32..40], h.node_table_offset, .little);
    std.mem.writeInt(u64, buf[40..48], h.edge_table_offset, .little);
    std.mem.writeInt(u64, buf[48..56], h.metrics_table_offset, .little);
    std.mem.writeInt(u64, buf[56..64], h.string_table_offset, .little);
    std.mem.writeInt(u64, buf[64..72], h.string_table_size, .little);
    writeStringRef(buf, 72, h.project_root);
}

fn readHeader(buf: []const u8) !BinaryHeader {
    if (!std.mem.eql(u8, buf[0..8], &MAGIC)) return error.InvalidMagic;
    const version = std.mem.readInt(u32, buf[8..12], .little);
    if (version != VERSION) return error.UnsupportedVersion;

    return .{
        .version = version,
        .flags = std.mem.readInt(u32, buf[12..16], .little),
        .node_count = std.mem.readInt(u64, buf[16..24], .little),
        .edge_count = std.mem.readInt(u64, buf[24..32], .little),
        .node_table_offset = std.mem.readInt(u64, buf[32..40], .little),
        .edge_table_offset = std.mem.readInt(u64, buf[40..48], .little),
        .metrics_table_offset = std.mem.readInt(u64, buf[48..56], .little),
        .string_table_offset = std.mem.readInt(u64, buf[56..64], .little),
        .string_table_size = std.mem.readInt(u64, buf[64..72], .little),
        .project_root = readStringRef(buf, 72),
    };
}

fn validateTableBounds(h: BinaryHeader, file_size: usize) !void {
    const nc: usize = @intCast(h.node_count);
    const ec: usize = @intCast(h.edge_count);
    const nto: usize = @intCast(h.node_table_offset);
    const eto: usize = @intCast(h.edge_table_offset);
    const mto: usize = @intCast(h.metrics_table_offset);
    const sto: usize = @intCast(h.string_table_offset);
    const st_size: usize = @intCast(h.string_table_size);

    const node_table_end = std.math.add(usize, nto, std.math.mul(usize, nc, NODE_RECORD_SIZE) catch return error.InvalidFormat) catch return error.InvalidFormat;
    const edge_table_end = std.math.add(usize, eto, std.math.mul(usize, ec, EDGE_RECORD_SIZE) catch return error.InvalidFormat) catch return error.InvalidFormat;
    const metrics_table_end = std.math.add(usize, mto, std.math.mul(usize, nc, METRICS_RECORD_SIZE) catch return error.InvalidFormat) catch return error.InvalidFormat;
    const string_table_end = std.math.add(usize, sto, st_size) catch return error.InvalidFormat;

    if (node_table_end > file_size or
        edge_table_end > file_size or
        metrics_table_end > file_size or
        string_table_end > file_size) return error.InvalidFormat;
}

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

const Layout = struct {
    node_count: usize,
    edge_count: usize,
    string_table_size: usize,
    node_table_offset: usize,
    edge_table_offset: usize,
    metrics_table_offset: usize,
    string_table_offset: usize,
    total_size: usize,

    fn compute(node_count: usize, edge_count: usize, string_table_size: usize) Layout {
        const nto = HEADER_SIZE;
        const eto = alignTo8(nto + node_count * NODE_RECORD_SIZE);
        const mto = alignTo8(eto + edge_count * EDGE_RECORD_SIZE);
        const sto = alignTo8(mto + node_count * METRICS_RECORD_SIZE);
        return .{
            .node_count = node_count,
            .edge_count = edge_count,
            .string_table_size = string_table_size,
            .node_table_offset = nto,
            .edge_table_offset = eto,
            .metrics_table_offset = mto,
            .string_table_offset = sto,
            .total_size = sto + string_table_size,
        };
    }

    fn verify(self: Layout, buf_len: usize) void {
        std.debug.assert(self.total_size == buf_len);
        std.debug.assert(self.node_table_offset + self.node_count * NODE_RECORD_SIZE <= self.edge_table_offset);
        std.debug.assert(self.edge_table_offset + self.edge_count * EDGE_RECORD_SIZE <= self.metrics_table_offset);
        std.debug.assert(self.metrics_table_offset + self.node_count * METRICS_RECORD_SIZE <= self.string_table_offset);
        std.debug.assert(self.string_table_offset + self.string_table_size == self.total_size);
    }
};

/// Serialize a graph to the binary storage format and write it to `path`.
/// The caller owns `g`; this function does not modify it.
pub fn save(allocator: std.mem.Allocator, io: std.Io, fg: FrozenGraph, path: []const u8) !void {
    const g = fg.graph;
    const nc = g.nodeCount();
    const ec = g.edgeCount();

    const node_refs = try allocator.alloc(NodeRefs, if (nc > 0) nc else 1);
    defer allocator.free(node_refs);

    // Measure: upper bound on string table bytes for stable dedup pointers.
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
        total_string_bytes += lang_meta_mod.binarySize(n);
        total_string_bytes += @tagName(n.kind).len;
        total_string_bytes += @tagName(n.visibility).len;
        if (n.language) |l| total_string_bytes += @tagName(l).len;
    }
    for (g.edges.items) |e| {
        total_string_bytes += @tagName(e.edge_type).len;
        total_string_bytes += @tagName(e.source).len;
    }
    total_string_bytes += g.project_root.len;

    var st = try StringTable.init(allocator, total_string_bytes);
    defer st.deinit(allocator);

    const project_root_ref = if (g.project_root.len > 0)
        try st.intern(allocator, g.project_root)
    else
        StringRef{ .offset = 0, .len = 0 };

    for (g.nodes.items, 0..) |n, i| {
        const ext_version: ?[]const u8 = switch (n.external) {
            .dependency => |d| d.version,
            else => null,
        };
        node_refs[i] = .{
            .name = try st.intern(allocator, n.name),
            .file_path = try st.internOptional(allocator, n.file_path),
            .signature = try st.internOptional(allocator, n.signature),
            .doc = try st.internOptional(allocator, n.doc),
            .ext_version = try st.internOptional(allocator, ext_version),
            .lang_meta = blk: {
                var meta_buf: [256]u8 = undefined;
                const meta_len = lang_meta_mod.encodeBinary(n, &meta_buf);
                break :blk if (meta_len > 0) try st.intern(allocator, meta_buf[0..meta_len]) else .{ .offset = 0, .len = 0 };
            },
            .kind = try st.intern(allocator, @tagName(n.kind)),
            .language = if (n.language) |l| try st.intern(allocator, @tagName(l)) else .{ .offset = 0, .len = 0 },
            .visibility = try st.intern(allocator, @tagName(n.visibility)),
        };
    }

    const edge_refs = try allocator.alloc([2]StringRef, if (ec > 0) ec else 1);
    defer allocator.free(edge_refs);
    for (g.edges.items, 0..) |e, i| {
        edge_refs[i] = .{
            try st.intern(allocator, @tagName(e.edge_type)),
            try st.intern(allocator, @tagName(e.source)),
        };
    }

    const layout = Layout.compute(nc, ec, st.bytes.items.len);

    // Allocate
    const buf = try allocator.alloc(u8, layout.total_size);
    defer allocator.free(buf);

    // Zero the alignment-padding gaps between table sections.
    @memset(buf[layout.node_table_offset + nc * NODE_RECORD_SIZE .. layout.edge_table_offset], 0);
    @memset(buf[layout.edge_table_offset + ec * EDGE_RECORD_SIZE .. layout.metrics_table_offset], 0);
    @memset(buf[layout.metrics_table_offset + nc * METRICS_RECORD_SIZE .. layout.string_table_offset], 0);

    // Fill: header
    writeHeader(buf, .{
        .node_count = @intCast(nc),
        .edge_count = @intCast(ec),
        .node_table_offset = @intCast(layout.node_table_offset),
        .edge_table_offset = @intCast(layout.edge_table_offset),
        .metrics_table_offset = @intCast(layout.metrics_table_offset),
        .string_table_offset = @intCast(layout.string_table_offset),
        .string_table_size = @intCast(layout.string_table_size),
        .project_root = project_root_ref,
    });

    // Fill: node records
    for (g.nodes.items, 0..) |n, i| {
        const base = layout.node_table_offset + i * NODE_RECORD_SIZE;
        const refs = node_refs[i];
        @memset(buf[base..][0..NODE_RECORD_SIZE], 0);

        // [0..8] id
        std.mem.writeInt(u64, buf[base..][0..8], @intFromEnum(n.id), .little);
        // [8..16] parent_id
        if (n.parent_id) |pid| {
            std.mem.writeInt(u64, buf[base + 8 ..][0..8], @intFromEnum(pid), .little);
        }
        // [16..20] line_start
        if (n.line_start) |ls| {
            std.mem.writeInt(u32, buf[base + 16 ..][0..4], ls, .little);
        }
        // [20..24] line_end
        if (n.line_end) |le| {
            std.mem.writeInt(u32, buf[base + 20 ..][0..4], le, .little);
        }
        // [24..28] col_start
        if (n.col_start) |cs| {
            std.mem.writeInt(u32, buf[base + 24 ..][0..4], cs, .little);
        }
        // [28..32] col_end
        if (n.col_end) |ce| {
            std.mem.writeInt(u32, buf[base + 28 ..][0..4], ce, .little);
        }
        // [32] external_kind
        buf[base + 32] = switch (n.external) {
            .none => 0,
            .stdlib => 1,
            .dependency => 2,
        };
        // [33] flags
        var flags: u8 = 0;
        if (n.content_hash != null) flags |= FLAG_HAS_CONTENT_HASH;
        if (n.metrics != null) flags |= FLAG_HAS_METRICS;
        if (n.parent_id != null) flags |= FLAG_HAS_PARENT;
        if (n.line_start != null) flags |= FLAG_HAS_LINE_START;
        if (n.line_end != null) flags |= FLAG_HAS_LINE_END;
        if (n.col_start != null) flags |= FLAG_HAS_COL_START;
        if (n.col_end != null) flags |= FLAG_HAS_COL_END;
        buf[base + 33] = flags;
        // [36..52] content_hash
        if (n.content_hash) |ch| {
            @memcpy(buf[base + 36 ..][0..types.hash_len], &ch);
        }
        // [56..128] 9 string refs
        writeStringRef(buf, base + 56, refs.kind);
        writeStringRef(buf, base + 64, refs.language);
        writeStringRef(buf, base + 72, refs.visibility);
        writeStringRef(buf, base + 80, refs.name);
        writeStringRef(buf, base + 88, refs.file_path);
        writeStringRef(buf, base + 96, refs.signature);
        writeStringRef(buf, base + 104, refs.doc);
        writeStringRef(buf, base + 112, refs.ext_version);
        writeStringRef(buf, base + 120, refs.lang_meta);
    }

    // Fill: edge records
    for (g.edges.items, 0..) |e, i| {
        const base = layout.edge_table_offset + i * EDGE_RECORD_SIZE;
        std.mem.writeInt(u64, buf[base..][0..8], @intFromEnum(e.source_id), .little);
        std.mem.writeInt(u64, buf[base + 8 ..][0..8], @intFromEnum(e.target_id), .little);
        writeStringRef(buf, base + 16, edge_refs[i][0]);
        writeStringRef(buf, base + 24, edge_refs[i][1]);
    }

    // Fill: metrics records (zero for nodes without metrics)
    for (g.nodes.items, 0..) |n, i| {
        const base = layout.metrics_table_offset + i * METRICS_RECORD_SIZE;
        if (n.metrics) |m| {
            m.encodeBinary(buf[base..][0..Metrics.BINARY_SIZE]);
        } else {
            @memset(buf[base..][0..METRICS_RECORD_SIZE], 0);
        }
    }

    // Fill: string table
    if (layout.string_table_size > 0) {
        @memcpy(buf[layout.string_table_offset..][0..layout.string_table_size], st.bytes.items);
    }

    layout.verify(buf.len);

    try atomic_file.writeAtomic(io, std.Io.Dir.cwd(), path, buf);
}

/// Deserialize a graph from a binary file at `path`.
///
/// Validates the header magic, version, and table bounds before parsing.
/// Returns `error.InvalidMagic` or `error.UnsupportedVersion` for header
/// mismatches, and `error.InvalidFormat` for truncated or corrupt data.
/// The caller owns the returned Graph and must call `deinit()` on it.
pub fn load(allocator: std.mem.Allocator, io: std.Io, path: []const u8) !Graph {
    // Read file
    const file = try std.Io.Dir.cwd().openFile(io, path, .{});
    defer file.close(io);

    const file_len = try file.length(io);
    const file_size: usize = @intCast(file_len);
    if (file_size < HEADER_SIZE) return error.InvalidFormat;

    const buf = try allocator.alloc(u8, file_size);
    defer allocator.free(buf);
    const bytes_read = try file.readPositionalAll(io, buf, 0);
    if (bytes_read < HEADER_SIZE) return error.InvalidFormat;

    // Validate and parse header
    const h = try readHeader(buf);
    try validateTableBounds(h, bytes_read);

    const nc: usize = @intCast(h.node_count);
    const ec: usize = @intCast(h.edge_count);
    const nto: usize = @intCast(h.node_table_offset);
    const eto: usize = @intCast(h.edge_table_offset);
    const mto: usize = @intCast(h.metrics_table_offset);
    const sto: usize = @intCast(h.string_table_offset);
    const st_size: usize = @intCast(h.string_table_size);

    var g = Graph.init("");
    errdefer g.deinit(allocator);

    try g.nodes.ensureTotalCapacity(allocator, nc);
    try g.edges.ensureTotalCapacity(allocator, ec);

    // Single dupe of string table region; all node strings resolve into this buffer
    const st_data: []const u8 = if (st_size > 0) blk: {
        const data = try allocator.dupe(u8, buf[sto..][0..st_size]);
        errdefer allocator.free(data);
        try g.addOwnedBuffer(allocator, data);
        break :blk data;
    } else "";

    g.project_root = try resolveStr(st_data, h.project_root);

    // Parse nodes
    for (0..nc) |i| {
        const base = nto + i * NODE_RECORD_SIZE;

        const flags = buf[base + 33];
        const has_content_hash = flags & FLAG_HAS_CONTENT_HASH != 0;
        const has_metrics = flags & FLAG_HAS_METRICS != 0;
        const has_parent = flags & FLAG_HAS_PARENT != 0;
        const has_line_start = flags & FLAG_HAS_LINE_START != 0;
        const has_line_end = flags & FLAG_HAS_LINE_END != 0;
        const has_col_start = flags & FLAG_HAS_COL_START != 0;
        const has_col_end = flags & FLAG_HAS_COL_END != 0;

        // Enum StringRefs
        const kind_ref = readStringRef(buf, base + 56);
        const lang_ref = readStringRef(buf, base + 64);
        const vis_ref = readStringRef(buf, base + 72);

        const kind_str = try resolveStr(st_data, kind_ref);
        const kind = std.meta.stringToEnum(NodeKind, kind_str) orelse return error.InvalidFormat;
        const language: ?types.Language = if (lang_ref.len == 0) null else blk: {
            const ls = try resolveStr(st_data, lang_ref);
            break :blk std.meta.stringToEnum(types.Language, ls) orelse return error.InvalidFormat;
        };
        const vis_str = try resolveStr(st_data, vis_ref);
        const visibility = std.meta.stringToEnum(Visibility, vis_str) orelse return error.InvalidFormat;

        // Data StringRefs
        const name_ref = readStringRef(buf, base + 80);
        const file_path_ref = readStringRef(buf, base + 88);
        const sig_ref = readStringRef(buf, base + 96);
        const doc_ref = readStringRef(buf, base + 104);
        const ext_ver_ref = readStringRef(buf, base + 112);
        const lang_meta_ref = readStringRef(buf, base + 120);

        const name = try resolveStr(st_data, name_ref);
        const file_path = try resolveOptStr(st_data, file_path_ref);
        const signature = try resolveOptStr(st_data, sig_ref);
        const doc = try resolveOptStr(st_data, doc_ref);

        // External info
        const external_kind = buf[base + 32];
        const external: ExternalInfo = switch (external_kind) {
            0 => .{ .none = {} },
            1 => .{ .stdlib = {} },
            2 => .{ .dependency = .{ .version = try resolveOptStr(st_data, ext_ver_ref) } },
            else => .{ .none = {} },
        };

        const lang_meta: ?*const anyopaque = if (lang_meta_ref.len > 0 and language != null) blk: {
            try validateStringRef(st_data, lang_meta_ref);
            break :blk try lang_meta_mod.decodeBinaryAndAttach(allocator, &g, language.?, st_data[lang_meta_ref.offset..][0..lang_meta_ref.len]);
        } else null;

        // Metrics
        const metrics: ?Metrics = if (has_metrics) blk: {
            const mbase = mto + i * METRICS_RECORD_SIZE;
            break :blk Metrics.decodeBinary(buf[mbase..][0..Metrics.BINARY_SIZE]);
        } else null;

        g.nodes.appendAssumeCapacity(.{
            .id = @enumFromInt(std.mem.readInt(u64, buf[base..][0..8], .little)),
            .name = name,
            .kind = kind,
            .language = language,
            .file_path = file_path,
            .parent_id = if (has_parent) @as(NodeId, @enumFromInt(std.mem.readInt(u64, buf[base + 8 ..][0..8], .little))) else null,
            .line_start = if (has_line_start) std.mem.readInt(u32, buf[base + 16 ..][0..4], .little) else null,
            .line_end = if (has_line_end) std.mem.readInt(u32, buf[base + 20 ..][0..4], .little) else null,
            .col_start = if (has_col_start) std.mem.readInt(u32, buf[base + 24 ..][0..4], .little) else null,
            .col_end = if (has_col_end) std.mem.readInt(u32, buf[base + 28 ..][0..4], .little) else null,
            .visibility = visibility,
            .doc = doc,
            .signature = signature,
            .content_hash = if (has_content_hash) buf[base + 36 ..][0..types.hash_len].* else null,
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

        const et_ref = readStringRef(buf, base + 16);
        const es_ref = readStringRef(buf, base + 24);
        const et_str = try resolveStr(st_data, et_ref);
        const es_str = try resolveStr(st_data, es_ref);

        g.edges.appendAssumeCapacity(.{
            .source_id = @enumFromInt(src_id),
            .target_id = @enumFromInt(tgt_id),
            .edge_type = std.meta.stringToEnum(EdgeType, et_str) orelse return error.InvalidFormat,
            .source = std.meta.stringToEnum(EdgeSource, es_str) orelse return error.InvalidFormat,
        });
    }

    try g.rebuildEdgeIndex(allocator);
    _ = try g.freeze(allocator);
    return g;
}

/// Append new nodes and edges to an existing binary file at `path`.
///
/// Loads the current file, merges in the nodes and edges from `g`,
/// then performs a full save (compaction) back to the same path.
pub fn append(allocator: std.mem.Allocator, io: std.Io, fg: FrozenGraph, path: []const u8) !void {
    const g = fg.graph;

    // Load existing graph
    var existing = try load(allocator, io, path);
    defer existing.deinit(allocator);

    // Merge new nodes
    for (g.nodes.items) |n| {
        _ = try existing.addNode(allocator, n);
    }

    // Merge new edges (dedup against existing graph)
    for (g.edges.items) |e| {
        _ = try existing.addEdgeIfNew(allocator, e);
    }

    // Full save (compaction)
    const existing_fg = try existing.freeze(allocator);
    try save(allocator, io, existing_fg, path);
}

test "Layout.compute produces deterministic offsets" {
    // Arrange / Act
    const a = Layout.compute(3, 2, 100);
    const b = Layout.compute(3, 2, 100);

    // Assert
    try std.testing.expectEqual(a.node_table_offset, b.node_table_offset);
    try std.testing.expectEqual(a.edge_table_offset, b.edge_table_offset);
    try std.testing.expectEqual(a.total_size, b.total_size);
}

test "Layout.compute aligns each table to 8 bytes" {
    // Arrange / Act
    const layout = Layout.compute(1, 1, 7);

    // Assert
    try std.testing.expectEqual(@as(usize, 0), layout.node_table_offset % 8);
    try std.testing.expectEqual(@as(usize, 0), layout.edge_table_offset % 8);
    try std.testing.expectEqual(@as(usize, 0), layout.metrics_table_offset % 8);
    try std.testing.expectEqual(@as(usize, 0), layout.string_table_offset % 8);
}

test "Layout.verify passes for a correctly sized buffer" {
    // Arrange
    const layout = Layout.compute(2, 1, 50);
    const buf = try std.testing.allocator.alloc(u8, layout.total_size);
    defer std.testing.allocator.free(buf);

    // Act / Assert
    layout.verify(buf.len);
}

test "StringTable.intern deduplicates strings" {
    // Arrange
    var st = try StringTable.init(std.testing.allocator, 32);
    defer st.deinit(std.testing.allocator);

    // Act
    const r1 = try st.intern(std.testing.allocator, "hello");
    const r2 = try st.intern(std.testing.allocator, "hello");

    // Assert
    try std.testing.expectEqual(r1.offset, r2.offset);
    try std.testing.expectEqual(r1.len, r2.len);
    try std.testing.expectEqual(@as(usize, 5), st.bytes.items.len);
}

test "StringTable iteration order matches insertion order" {
    // Arrange
    var st = try StringTable.init(std.testing.allocator, 9);
    defer st.deinit(std.testing.allocator);

    _ = try st.intern(std.testing.allocator, "aaa");
    _ = try st.intern(std.testing.allocator, "bbb");
    _ = try st.intern(std.testing.allocator, "ccc");

    // Act
    var it = st.index.iterator();
    const first = it.next().?.key_ptr.*;
    const second = it.next().?.key_ptr.*;
    const third = it.next().?.key_ptr.*;
    try std.testing.expect(it.next() == null);

    // Assert
    try std.testing.expectEqualStrings("aaa", first);
    try std.testing.expectEqualStrings("bbb", second);
    try std.testing.expectEqualStrings("ccc", third);
}

/// Build a test graph with 3 diverse nodes and 2 edges for use in tests.
fn createTestGraph(allocator: std.mem.Allocator) !Graph {
    var g = Graph.init("/tmp/test-project");

    // Node 0: file node (minimal fields)
    _ = try g.addNode(allocator, .{
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
    _ = try g.addNode(allocator, .{
        .id = .root,
        .name = "process",
        .kind = .function,
        .language = .zig,
        .visibility = .public,
        .file_path = "src/main.zig",
        .line_start = 10,
        .line_end = 50,
        .col_start = 7,
        .col_end = 14,
        .parent_id = @enumFromInt(0),
        .doc = "/// Process the input data.",
        .signature = "pub fn process(data: []const u8) !void",
        .content_hash = "abcdefghijklmnop".*,
        .metrics = .{
            .complexity = 5,
            .lines = 40,
            .fan_in = 2,
            .fan_out = 3,
            .branches = 4,
            .loops = 1,
            .error_paths = 2,
            .nesting_depth_max = 3,
            .structural_hash = 0xCAFEBABE12345678,
        },
        .lang_meta = try zig_meta_mod.allocAndAttach(allocator, &g, .{ .is_comptime = false, .is_inline = true }),
    });

    // Node 2: type_def with external=none
    _ = try g.addNode(allocator, .{
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
    _ = try g.addEdgeIfNew(allocator, .{
        .source_id = @enumFromInt(1),
        .target_id = @enumFromInt(2),
        .edge_type = .uses_type,
        .source = .tree_sitter,
    });

    // Edge 1: file exports function
    _ = try g.addEdgeIfNew(allocator, .{
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
    defer g.deinit(std.testing.allocator);

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Act
    const fg = try g.freeze(std.testing.allocator);
    try save(std.testing.allocator, std.testing.io, fg, file_path);
    var loaded = try load(std.testing.allocator, std.testing.io, file_path);
    defer loaded.deinit(std.testing.allocator);

    // Assert
    try std.testing.expectEqual(g.nodeCount(), loaded.nodeCount());
    for (g.nodes.items, loaded.nodes.items) |original, restored| {
        try std.testing.expectEqualStrings(original.name, restored.name);
        try std.testing.expectEqual(original.kind, restored.kind);
        try std.testing.expectEqual(original.language, restored.language);
        try std.testing.expectEqual(original.visibility, restored.visibility);
        try std.testing.expectEqual(original.parent_id, restored.parent_id);
        try std.testing.expectEqual(original.line_start, restored.line_start);
        try std.testing.expectEqual(original.line_end, restored.line_end);
        try std.testing.expectEqual(original.col_start, restored.col_start);
        try std.testing.expectEqual(original.col_end, restored.col_end);
    }

    // Assert
    try std.testing.expectEqual(g.edgeCount(), loaded.edgeCount());
    for (g.edges.items, loaded.edges.items) |original, restored| {
        try std.testing.expectEqual(original.source_id, restored.source_id);
        try std.testing.expectEqual(original.target_id, restored.target_id);
        try std.testing.expectEqual(original.edge_type, restored.edge_type);
        try std.testing.expectEqual(original.source, restored.source);
    }

    // Assert
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
    defer g.deinit(std.testing.allocator);

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Act
    const fg = try g.freeze(std.testing.allocator);
    try save(std.testing.allocator, std.testing.io, fg, file_path);

    // Assert
    const file = try tmp.dir.openFile(std.testing.io, "test.bin", .{});
    defer file.close(std.testing.io);
    var header_buf: [12]u8 = undefined;
    var read_buf: [128]u8 = undefined;
    var f_reader = file.reader(std.testing.io, &read_buf);
    try f_reader.interface.readSliceAll(&header_buf);
    const bytes_read: usize = 12;
    try std.testing.expectEqual(@as(usize, 12), bytes_read);
    try std.testing.expectEqualSlices(u8, &MAGIC, header_buf[0..8]);

    // Assert
    const version = std.mem.readInt(u32, header_buf[8..12], .little);
    try std.testing.expectEqual(VERSION, version);

    // Assert
    var loaded = try load(std.testing.allocator, std.testing.io, file_path);
    defer loaded.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 3), loaded.nodeCount());
    try std.testing.expectEqual(@as(usize, 2), loaded.edgeCount());
}

// Edge case tests

test "binary save/load empty graph" {
    // Arrange
    var g = Graph.init("/tmp/test-project");
    defer g.deinit(std.testing.allocator);

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Act
    const fg = try g.freeze(std.testing.allocator);
    try save(std.testing.allocator, std.testing.io, fg, file_path);
    var loaded = try load(std.testing.allocator, std.testing.io, file_path);
    defer loaded.deinit(std.testing.allocator);

    // Assert
    try std.testing.expectEqual(@as(usize, 0), loaded.nodeCount());
    try std.testing.expectEqual(@as(usize, 0), loaded.edgeCount());
}

test "binary save/load single node" {
    // Arrange
    var g = Graph.init("/tmp/test-project");
    defer g.deinit(std.testing.allocator);

    _ = try g.addNode(std.testing.allocator, .{
        .id = .root,
        .name = "only_node",
        .kind = .file,
        .language = .zig,
    });

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Act
    const fg = try g.freeze(std.testing.allocator);
    try save(std.testing.allocator, std.testing.io, fg, file_path);
    var loaded = try load(std.testing.allocator, std.testing.io, file_path);
    defer loaded.deinit(std.testing.allocator);

    // Assert
    try std.testing.expectEqual(@as(usize, 1), loaded.nodeCount());
    try std.testing.expectEqual(@as(usize, 0), loaded.edgeCount());
    try std.testing.expectEqualStrings("only_node", loaded.getNode(.root).?.name);
}

test "binary preserves phantom nodes" {
    // Arrange
    var g = Graph.init("/tmp/test-project");
    defer g.deinit(std.testing.allocator);

    _ = try g.addNode(std.testing.allocator, .{
        .id = .root,
        .name = "std",
        .kind = .module,
        .language = .zig,
        .external = .{ .stdlib = {} },
    });

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Act
    const fg = try g.freeze(std.testing.allocator);
    try save(std.testing.allocator, std.testing.io, fg, file_path);
    var loaded = try load(std.testing.allocator, std.testing.io, file_path);
    defer loaded.deinit(std.testing.allocator);

    // Assert
    const loaded_node = loaded.getNode(.root).?;
    try std.testing.expectEqual(ExternalInfo.stdlib, loaded_node.external);
}

test "binary preserves null optional fields" {
    // Arrange
    var g = Graph.init("/tmp/test-project");
    defer g.deinit(std.testing.allocator);

    _ = try g.addNode(std.testing.allocator, .{
        .id = .root,
        .name = "bare",
        .kind = .function,
        .language = .zig,
        // All optional fields left at default null
    });

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Act
    const fg = try g.freeze(std.testing.allocator);
    try save(std.testing.allocator, std.testing.io, fg, file_path);
    var loaded = try load(std.testing.allocator, std.testing.io, file_path);
    defer loaded.deinit(std.testing.allocator);

    // Assert
    const n = loaded.getNode(.root).?;
    try std.testing.expectEqual(@as(?[]const u8, null), n.doc);
    try std.testing.expectEqual(@as(?[]const u8, null), n.signature);
    try std.testing.expectEqual(@as(?types.ContentHash, null), n.content_hash);
    try std.testing.expectEqual(@as(?Metrics, null), n.metrics);
}

test "binary preserves long strings" {
    // Arrange
    var g = Graph.init("/tmp/test-project");
    defer g.deinit(std.testing.allocator);

    // Create a 10KB doc comment
    const long_doc = try std.testing.allocator.alloc(u8, 10 * 1024);
    defer std.testing.allocator.free(long_doc);
    @memset(long_doc, 'A');

    _ = try g.addNode(std.testing.allocator, .{
        .id = .root,
        .name = "verbose",
        .kind = .function,
        .language = .zig,
        .doc = long_doc,
    });

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Act
    const fg = try g.freeze(std.testing.allocator);
    try save(std.testing.allocator, std.testing.io, fg, file_path);
    var loaded = try load(std.testing.allocator, std.testing.io, file_path);
    defer loaded.deinit(std.testing.allocator);

    // Assert
    const loaded_doc = loaded.getNode(.root).?.doc.?;
    try std.testing.expectEqual(@as(usize, 10 * 1024), loaded_doc.len);
    try std.testing.expectEqualSlices(u8, long_doc, loaded_doc);
}

test "binary preserves ZigMeta" {
    // Arrange
    var g = Graph.init("/tmp/test-project");
    defer g.deinit(std.testing.allocator);

    _ = try g.addNode(std.testing.allocator, .{
        .id = .root,
        .name = "comptime_fn",
        .kind = .function,
        .language = .zig,
        .lang_meta = try zig_meta_mod.allocAndAttach(std.testing.allocator, &g, .{ .is_comptime = true }),
    });

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Act
    const fg = try g.freeze(std.testing.allocator);
    try save(std.testing.allocator, std.testing.io, fg, file_path);
    var loaded = try load(std.testing.allocator, std.testing.io, file_path);
    defer loaded.deinit(std.testing.allocator);

    // Assert
    const meta = zig_meta_mod.metaOf(loaded.getNode(.root).?).?;
    try std.testing.expect(meta.is_comptime);
}

test "binary preserves null lang_meta" {
    // Arrange
    var g = Graph.init("/tmp/test-project");
    defer g.deinit(std.testing.allocator);

    _ = try g.addNode(std.testing.allocator, .{
        .id = .root,
        .name = "plain",
        .kind = .function,
        .language = .zig,
    });

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Act
    const fg = try g.freeze(std.testing.allocator);
    try save(std.testing.allocator, std.testing.io, fg, file_path);
    var loaded = try load(std.testing.allocator, std.testing.io, file_path);
    defer loaded.deinit(std.testing.allocator);

    // Assert
    try std.testing.expect(loaded.getNode(.root).?.lang_meta == null);
}

test "binary round-trip preserves union_def kind" {
    // Arrange
    var g = Graph.init("/tmp/test-project");
    defer g.deinit(std.testing.allocator);

    _ = try g.addNode(std.testing.allocator, .{
        .id = .root,
        .name = "MyUnion",
        .kind = .union_def,
        .language = .zig,
        .visibility = .public,
        .file_path = "src/main.zig",
    });

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Act
    const fg = try g.freeze(std.testing.allocator);
    try save(std.testing.allocator, std.testing.io, fg, file_path);
    var loaded = try load(std.testing.allocator, std.testing.io, file_path);
    defer loaded.deinit(std.testing.allocator);

    // Assert
    try std.testing.expectEqual(NodeKind.union_def, loaded.getNode(.root).?.kind);
}

test "binary round-trip preserves is_packed metadata" {
    // Arrange
    var g = Graph.init("/tmp/test-project");
    defer g.deinit(std.testing.allocator);

    _ = try g.addNode(std.testing.allocator, .{
        .id = .root,
        .name = "PackedStruct",
        .kind = .type_def,
        .language = .zig,
        .lang_meta = try zig_meta_mod.allocAndAttach(std.testing.allocator, &g, .{ .is_packed = true }),
    });

    _ = try g.addNode(std.testing.allocator, .{
        .id = .root,
        .name = "ExternStruct",
        .kind = .type_def,
        .language = .zig,
        .lang_meta = try zig_meta_mod.allocAndAttach(std.testing.allocator, &g, .{ .is_extern = true }),
    });

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Act
    const fg = try g.freeze(std.testing.allocator);
    try save(std.testing.allocator, std.testing.io, fg, file_path);
    var loaded = try load(std.testing.allocator, std.testing.io, file_path);
    defer loaded.deinit(std.testing.allocator);

    // Assert
    const packed_meta = zig_meta_mod.metaOf(loaded.getNode(@enumFromInt(0)).?).?;
    try std.testing.expect(packed_meta.is_packed);
    try std.testing.expect(!packed_meta.is_extern);

    const extern_meta = zig_meta_mod.metaOf(loaded.getNode(@enumFromInt(1)).?).?;
    try std.testing.expect(extern_meta.is_extern);
    try std.testing.expect(!extern_meta.is_packed);
}

// Append tests

test "append adds new nodes" {
    // Arrange
    var g = try createTestGraph(std.testing.allocator);
    defer g.deinit(std.testing.allocator);

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Save initial 3 nodes
    const fg = try g.freeze(std.testing.allocator);
    try save(std.testing.allocator, std.testing.io, fg, file_path);

    // Build a graph with 2 additional nodes
    var extra = Graph.init("/tmp/test-project");
    defer extra.deinit(std.testing.allocator);
    _ = try extra.addNode(std.testing.allocator, .{
        .id = .root,
        .name = "helper",
        .kind = .function,
        .language = .zig,
    });
    _ = try extra.addNode(std.testing.allocator, .{
        .id = .root,
        .name = "util",
        .kind = .function,
        .language = .zig,
    });

    // Act
    const extra_fg = try extra.freeze(std.testing.allocator);
    try append(std.testing.allocator, std.testing.io, extra_fg, file_path);

    // Assert
    var loaded = try load(std.testing.allocator, std.testing.io, file_path);
    defer loaded.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 5), loaded.nodeCount());
}

test "compaction after append" {
    // Arrange
    var g = try createTestGraph(std.testing.allocator);
    defer g.deinit(std.testing.allocator);

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/test.bin", .{path});
    defer std.testing.allocator.free(file_path);

    // Save initial graph
    const fg = try g.freeze(std.testing.allocator);
    try save(std.testing.allocator, std.testing.io, fg, file_path);

    // Append multiple small graphs
    var extra1 = Graph.init("/tmp/test-project");
    defer extra1.deinit(std.testing.allocator);
    _ = try extra1.addNode(std.testing.allocator, .{ .id = .root, .name = "a1", .kind = .function, .language = .zig });
    const fg1 = try extra1.freeze(std.testing.allocator);
    try append(std.testing.allocator, std.testing.io, fg1, file_path);

    var extra2 = Graph.init("/tmp/test-project");
    defer extra2.deinit(std.testing.allocator);
    _ = try extra2.addNode(std.testing.allocator, .{ .id = .root, .name = "a2", .kind = .function, .language = .zig });
    const fg2 = try extra2.freeze(std.testing.allocator);
    try append(std.testing.allocator, std.testing.io, fg2, file_path);

    // Act
    var loaded_pre = try load(std.testing.allocator, std.testing.io, file_path);
    defer loaded_pre.deinit(std.testing.allocator);
    const pre_fg = try loaded_pre.freeze(std.testing.allocator);
    try save(std.testing.allocator, std.testing.io, pre_fg, file_path);

    // Assert
    var loaded_post = try load(std.testing.allocator, std.testing.io, file_path);
    defer loaded_post.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 5), loaded_post.nodeCount());
}

test "load rejects truncated file with table regions past EOF" {
    // Arrange
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
    const path = try tmp.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/malformed.bin", .{path});
    defer std.testing.allocator.free(file_path);

    const file = try tmp.dir.createFile(std.testing.io, "malformed.bin", .{});
    defer file.close(std.testing.io);
    try file.writeStreamingAll(std.testing.io, &header);

    // Act / Assert
    const result = load(std.testing.allocator, std.testing.io, file_path);
    try std.testing.expectError(error.InvalidFormat, result);
}

test "load rejects corrupt string ref past string table" {
    // Arrange
    var g = Graph.init("/tmp/test-project");
    defer g.deinit(std.testing.allocator);
    _ = try g.addNode(std.testing.allocator, .{ .id = .root, .name = "n", .kind = .file, .language = .zig });

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/malformed.bin", .{path});
    defer std.testing.allocator.free(file_path);

    const fg = try g.freeze(std.testing.allocator);
    try save(std.testing.allocator, std.testing.io, fg, file_path);

    // Overwrite name StringRef (node base + 72) with out-of-bounds offset
    const raw_file = try std.Io.Dir.cwd().openFile(std.testing.io, file_path, .{ .mode = .read_write });
    defer raw_file.close(std.testing.io);
    var ref_buf: [8]u8 = undefined;
    std.mem.writeInt(u32, ref_buf[0..4], 0xFFFF, .little);
    std.mem.writeInt(u32, ref_buf[4..8], 10, .little);
    try raw_file.writePositionalAll(std.testing.io, &ref_buf, HEADER_SIZE + 72);

    // Act / Assert
    const result = load(std.testing.allocator, std.testing.io, file_path);
    try std.testing.expectError(error.InvalidFormat, result);
}

test "load rejects invalid enum string" {
    // Arrange
    // to point to an out-of-bounds offset so resolveStr fails
    var g = Graph.init("/tmp/test-project");
    defer g.deinit(std.testing.allocator);
    _ = try g.addNode(std.testing.allocator, .{ .id = .root, .name = "n", .kind = .file, .language = .zig });

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
    defer std.testing.allocator.free(path);
    const file_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/malformed.bin", .{path});
    defer std.testing.allocator.free(file_path);

    const fg = try g.freeze(std.testing.allocator);
    try save(std.testing.allocator, std.testing.io, fg, file_path);

    // Overwrite kind StringRef (node base + 56) with out-of-bounds offset
    const raw_file = try std.Io.Dir.cwd().openFile(std.testing.io, file_path, .{ .mode = .read_write });
    defer raw_file.close(std.testing.io);
    var ref_buf: [8]u8 = undefined;
    std.mem.writeInt(u32, ref_buf[0..4], 0xFFFF, .little);
    std.mem.writeInt(u32, ref_buf[4..8], 10, .little);
    try raw_file.writePositionalAll(std.testing.io, &ref_buf, HEADER_SIZE + 56);

    // Act / Assert
    const result = load(std.testing.allocator, std.testing.io, file_path);
    try std.testing.expectError(error.InvalidFormat, result);
}
