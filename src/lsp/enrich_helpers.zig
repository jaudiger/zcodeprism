//! Shared helpers for language-specific LSP enrichment callbacks.
//! Used by both Zig and Rust enrichWithLsp implementations.

const std = @import("std");
const graph_mod = @import("../core/graph.zig");
const types = @import("../core/types.zig");
const client_mod = @import("client.zig");
const protocol = @import("protocol.zig");
const worklist_mod = @import("worklist.zig");
const logging = @import("../logging.zig");
const lang_support = @import("../languages/language_support.zig");

pub const Graph = graph_mod.Graph;
pub const NodeId = types.NodeId;
pub const LspClient = client_mod.LspClient;
pub const WorklistEntry = worklist_mod.WorklistEntry;
pub const EnrichResult = lang_support.EnrichResult;
pub const Logger = logging.Logger;
pub const Field = logging.Field;

/// Build a map from relative file paths to file node IDs.
pub fn buildFileNodeMap(allocator: std.mem.Allocator, graph: *const Graph) error{OutOfMemory}!std.StringHashMapUnmanaged(NodeId) {
    var map: std.StringHashMapUnmanaged(NodeId) = .{};
    errdefer map.deinit(allocator);
    for (graph.nodes.items, 0..) |n, i| {
        if (n.kind != .file) continue;
        const fp = n.file_path orelse continue;
        try map.put(allocator, fp, @enumFromInt(i));
    }
    return map;
}

/// Resolve a definition URI to a relative path within the project, or null
/// if the definition points outside the project.
pub fn resolveDefinitionToRelPath(uri: []const u8, project_root: []const u8) ?[]const u8 {
    const abs = client_mod.uriToPath(uri) orelse return null;
    if (!std.mem.startsWith(u8, abs, project_root)) return null;
    var rel = abs[project_root.len..];
    if (rel.len > 0 and rel[0] == '/') rel = rel[1..];
    return rel;
}

/// Find the most specific (narrowest line span) declaration node within the
/// file at `file_node_id` that contains `def_line` (0-based, from LSP).
/// Returns the file node itself when no declaration spans the line.
pub fn findDeclarationAtLine(graph: *const Graph, file_node_id: NodeId, def_line: u32) NodeId {
    const graph_line: u32 = def_line + 1;
    const file_idx = @intFromEnum(file_node_id);
    const file_path = graph.nodes.items[file_idx].file_path orelse return file_node_id;

    var best: NodeId = file_node_id;
    var best_span: u32 = std.math.maxInt(u32);

    for (graph.nodes.items[file_idx + 1 ..]) |candidate| {
        const fp = candidate.file_path orelse break;
        if (fp.ptr != file_path.ptr or fp.len != file_path.len) break;
        const ls = candidate.line_start orelse continue;
        const le = candidate.line_end orelse continue;
        if (le < ls) continue;
        if (ls <= graph_line and graph_line <= le) {
            const span = le - ls;
            if (span < best_span) {
                best_span = span;
                best = candidate.id;
            }
        }
    }

    // When the best match is a parameter, prefer its parent function.
    const best_node = graph.getNode(best) orelse return best;
    if (best_node.kind == .parameter) {
        return best_node.parent_id orelse best;
    }
    return best;
}

/// Hover text split into signature and doc sub-slices of the input.
/// Both fields borrow from the input text and carry no owned memory.
pub const HoverContents = struct {
    signature: ?[]const u8,
    doc: ?[]const u8,
};

/// Extract signature and doc from markdown hover text. Signature is the
/// content of the first code fence; doc is the text between the closing
/// fence and any subsequent fence.
pub fn parseHoverContents(text: []const u8) HoverContents {
    const fence = "```";

    const open = std.mem.indexOf(u8, text, fence) orelse {
        const sig = std.mem.trim(u8, text, " \t\n\r");
        return .{ .signature = if (sig.len > 0) sig else null, .doc = null };
    };

    const after_fence = open + fence.len;
    const line_end = std.mem.indexOfScalarPos(u8, text, after_fence, '\n') orelse text.len;
    const code_start = if (line_end < text.len) line_end + 1 else line_end;

    const close = std.mem.indexOfPos(u8, text, code_start, fence) orelse {
        const sig = std.mem.trim(u8, text[code_start..], " \t\n\r");
        return .{ .signature = if (sig.len > 0) sig else null, .doc = null };
    };

    const sig_raw = std.mem.trim(u8, text[code_start..close], " \t\n\r");
    const after_close = close + fence.len;

    const doc_region = if (after_close < text.len) text[after_close..] else "";
    const doc_trimmed = if (std.mem.indexOf(u8, doc_region, fence)) |next_fence|
        std.mem.trim(u8, doc_region[0..next_fence], " \t\n\r")
    else
        std.mem.trim(u8, doc_region, " \t\n\r");

    return .{
        .signature = if (sig_raw.len > 0) sig_raw else null,
        .doc = if (doc_trimmed.len > 0) doc_trimmed else null,
    };
}

/// Map an LSP location to a graph declaration node within the project. Returns
/// null when the location falls outside the project or onto a file node.
fn locationToDeclaration(
    graph: *const Graph,
    file_map: *const std.StringHashMapUnmanaged(NodeId),
    loc: protocol.Location,
) ?NodeId {
    const rel = resolveDefinitionToRelPath(loc.uri, graph.project_root) orelse return null;
    const file_node_id = file_map.get(rel) orelse return null;
    const target_id = findDeclarationAtLine(graph, file_node_id, loc.range.start.line);
    if (target_id == file_node_id) return null;
    return target_id;
}

fn processDefinitionEntry(
    allocator: std.mem.Allocator,
    io: std.Io,
    graph: *Graph,
    client: *LspClient,
    file_map: *const std.StringHashMapUnmanaged(NodeId),
    uri: []const u8,
    entry: WorklistEntry,
    result: *EnrichResult,
    logger: Logger,
) error{OutOfMemory}!void {
    result.definition_queries += 1;
    const locs = (client.textDocumentDefinition(allocator, io, uri, entry.line, entry.col) catch return) orelse return;
    defer protocol.freeLocationArray(allocator, locs);
    for (locs) |loc| {
        const target_id = locationToDeclaration(graph, file_map, loc) orelse continue;
        const added = graph.addEdgeIfNew(allocator, .{
            .source_id = entry.source_node_id,
            .target_id = target_id,
            .edge_type = .calls,
            .source = .lsp,
        }) catch return error.OutOfMemory;
        if (added) {
            result.edges_promoted += 1;
            result.definition_successes += 1;
            result.worklist_resolved += 1;
            const target_name = if (graph.getNode(target_id)) |n| n.name else "?";
            logger.debug("promoted call edge via definition", &.{
                Field.string("hint", entry.hint_name orelse "?"),
                Field.string("target", target_name),
            });
        }
        break;
    }
}

fn processTypeDefinitionEntry(
    allocator: std.mem.Allocator,
    io: std.Io,
    graph: *Graph,
    client: *LspClient,
    file_map: *const std.StringHashMapUnmanaged(NodeId),
    uri: []const u8,
    entry: WorklistEntry,
    result: *EnrichResult,
) error{OutOfMemory}!void {
    result.type_definition_queries += 1;
    const locs = (client.textDocumentTypeDefinition(allocator, io, uri, entry.line, entry.col) catch return) orelse return;
    defer protocol.freeLocationArray(allocator, locs);
    for (locs) |loc| {
        const target_id = locationToDeclaration(graph, file_map, loc) orelse continue;
        const added = graph.addEdgeIfNew(allocator, .{
            .source_id = entry.source_node_id,
            .target_id = target_id,
            .edge_type = .uses_type,
            .source = .lsp,
        }) catch return error.OutOfMemory;
        if (added) {
            result.edges_added += 1;
            result.type_definition_successes += 1;
            result.worklist_resolved += 1;
        }
        break;
    }
}

fn processHoverEntry(
    allocator: std.mem.Allocator,
    io: std.Io,
    graph: *Graph,
    client: *LspClient,
    uri: []const u8,
    entry: WorklistEntry,
    result: *EnrichResult,
    handleHover: ?*const fn (std.mem.Allocator, *Graph, usize, protocol.Hover, *EnrichResult) error{OutOfMemory}!void,
    logger: Logger,
) error{OutOfMemory}!void {
    const src_idx = @intFromEnum(entry.source_node_id);
    if (src_idx >= graph.nodes.items.len) return;
    result.hover_queries += 1;
    const hover = (client.textDocumentHover(allocator, io, uri, entry.line, entry.col) catch {
        logger.debug("hover query failed", &.{Field.string("hint", entry.hint_name orelse "?")});
        return;
    }) orelse return;
    defer protocol.freeHover(allocator, hover);
    if (handleHover) |handler| {
        try handler(allocator, graph, src_idx, hover, result);
    }
}

fn processReferencesEntry(
    allocator: std.mem.Allocator,
    io: std.Io,
    graph: *Graph,
    client: *LspClient,
    file_map: *const std.StringHashMapUnmanaged(NodeId),
    uri: []const u8,
    entry: WorklistEntry,
    result: *EnrichResult,
) error{OutOfMemory}!void {
    result.reference_queries += 1;
    const locs = (client.textDocumentReferences(allocator, io, uri, entry.line, entry.col, false) catch return) orelse return;
    defer protocol.freeLocationArray(allocator, locs);
    var resolved_any = false;
    for (locs) |loc| {
        const ref_id = locationToDeclaration(graph, file_map, loc) orelse continue;
        const added = graph.addEdgeIfNew(allocator, .{
            .source_id = ref_id,
            .target_id = entry.source_node_id,
            .edge_type = .calls,
            .source = .lsp,
        }) catch return error.OutOfMemory;
        if (added) {
            result.edges_added += 1;
            resolved_any = true;
        }
    }
    if (resolved_any) {
        result.reference_successes += 1;
        result.worklist_resolved += 1;
    }
}

/// Send each worklist entry to the appropriate LSP method and integrate
/// the result as a new graph edge or node metadata. The `handleHover`
/// callback processes hover results in a language-specific way; pass null
/// to skip hover entries entirely.
pub fn dispatchWorklist(
    allocator: std.mem.Allocator,
    io: std.Io,
    graph: *Graph,
    client: *LspClient,
    worklist: []const WorklistEntry,
    file_map: *const std.StringHashMapUnmanaged(NodeId),
    result: *EnrichResult,
    handleHover: ?*const fn (std.mem.Allocator, *Graph, usize, protocol.Hover, *EnrichResult) error{OutOfMemory}!void,
    logger: Logger,
) error{OutOfMemory}!void {
    for (worklist) |entry| {
        const abs_path = std.fs.path.join(allocator, &.{ graph.project_root, entry.file_path }) catch continue;
        defer allocator.free(abs_path);
        const uri = client_mod.pathToUri(allocator, abs_path) catch continue;
        defer allocator.free(uri);

        switch (entry.query_kind) {
            .definition => try processDefinitionEntry(allocator, io, graph, client, file_map, uri, entry, result, logger),
            .type_definition => try processTypeDefinitionEntry(allocator, io, graph, client, file_map, uri, entry, result),
            .hover => try processHoverEntry(allocator, io, graph, client, uri, entry, result, handleHover, logger),
            .references => try processReferencesEntry(allocator, io, graph, client, file_map, uri, entry, result),
        }
    }
}

/// Query textDocument/references for every function, constant, and type node
/// with no inbound edges. Adds a calls or uses_type edge for each reference
/// site that maps to a known graph node.
pub fn runDeadCodeReferencesPass(
    allocator: std.mem.Allocator,
    io: std.Io,
    graph: *Graph,
    client: *LspClient,
    file_map: *const std.StringHashMapUnmanaged(NodeId),
    result: *EnrichResult,
    logger: Logger,
) error{OutOfMemory}!void {
    var inbound: std.AutoHashMapUnmanaged(NodeId, u32) = .{};
    defer inbound.deinit(allocator);
    for (graph.edges.items) |e| {
        const gop = try inbound.getOrPut(allocator, e.target_id);
        if (!gop.found_existing) gop.value_ptr.* = 0;
        gop.value_ptr.* += 1;
    }

    for (graph.nodes.items) |node| {
        switch (node.kind) {
            .function, .constant, .type_def, .enum_def, .union_def => {},
            else => continue,
        }
        if (node.external != .none) continue;
        const line_start = node.line_start orelse continue;
        const col_start = node.col_start orelse 0;
        const file_path = node.file_path orelse continue;
        if ((inbound.get(node.id) orelse 0) > 0) continue;

        const abs_path = std.fs.path.join(allocator, &.{ graph.project_root, file_path }) catch continue;
        defer allocator.free(abs_path);
        const uri_val = client_mod.pathToUri(allocator, abs_path) catch continue;
        defer allocator.free(uri_val);

        result.reference_queries += 1;
        const locs = (client.textDocumentReferences(allocator, io, uri_val, line_start - 1, col_start, false) catch continue) orelse continue;
        defer protocol.freeLocationArray(allocator, locs);

        const edge_type: types.EdgeType = if (node.kind == .function) .calls else .uses_type;

        var resolved_any = false;
        for (locs) |loc| {
            const rel = resolveDefinitionToRelPath(loc.uri, graph.project_root) orelse continue;
            const file_node_id = file_map.get(rel) orelse continue;
            const ref_id = findDeclarationAtLine(graph, file_node_id, loc.range.start.line);
            if (ref_id == file_node_id) continue;
            if (ref_id == node.id) continue;
            const added = graph.addEdgeIfNew(allocator, .{
                .source_id = ref_id,
                .target_id = node.id,
                .edge_type = edge_type,
                .source = .lsp,
            }) catch return error.OutOfMemory;
            if (added) {
                result.edges_added += 1;
                resolved_any = true;
            }
        }
        if (resolved_any) {
            result.reference_successes += 1;
            logger.debug("confirmed live node via references", &.{
                Field.string("name", node.name),
            });
        }
    }
}

/// Query hover for each phantom node and store extracted signature and doc
/// on the corresponding graph node.
pub fn enrichPhantoms(
    allocator: std.mem.Allocator,
    io: std.Io,
    graph: *Graph,
    client: *LspClient,
    phantom_hovers: []const WorklistEntry,
    result: *EnrichResult,
    logger: Logger,
) error{OutOfMemory}!void {
    for (phantom_hovers) |entry| {
        const src_idx = @intFromEnum(entry.source_node_id);
        if (src_idx >= graph.nodes.items.len) continue;
        if (graph.nodes.items[src_idx].signature != null) continue;

        const abs_path = std.fs.path.join(
            allocator,
            &.{ graph.project_root, entry.file_path },
        ) catch continue;
        defer allocator.free(abs_path);
        const uri = client_mod.pathToUri(allocator, abs_path) catch continue;
        defer allocator.free(uri);

        result.hover_queries += 1;
        const hover = (client.textDocumentHover(
            allocator,
            io,
            uri,
            entry.line,
            entry.col,
        ) catch {
            logger.debug("phantom hover failed", &.{
                Field.string("hint", entry.hint_name orelse "?"),
            });
            result.phantoms_remaining += 1;
            continue;
        }) orelse {
            result.phantoms_remaining += 1;
            continue;
        };
        defer protocol.freeHover(allocator, hover);

        const hover_text = switch (hover.contents) {
            .markup => |m| m.value,
            .plain_string => |s| s,
        };
        const extracted = parseHoverContents(hover_text);
        var enriched = false;

        if (extracted.signature) |sig| {
            const d = try allocator.dupe(u8, sig);
            errdefer allocator.free(d);
            try graph.addOwnedBuffer(allocator, d);
            graph.nodes.items[src_idx].signature = d;
            enriched = true;
        }
        if (extracted.doc) |doc_text| {
            const d = try allocator.dupe(u8, doc_text);
            errdefer allocator.free(d);
            try graph.addOwnedBuffer(allocator, d);
            graph.nodes.items[src_idx].doc = d;
            enriched = true;
        }

        if (enriched) {
            result.phantoms_enriched += 1;
            result.hover_successes += 1;
            logger.debug("enriched phantom", &.{
                Field.string("hint", entry.hint_name orelse "?"),
            });
        } else {
            result.phantoms_remaining += 1;
        }
    }
}
