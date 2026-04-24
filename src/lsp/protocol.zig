//! Minimal LSP 3.17 type definitions with JSON parsing and serialization.
//! Covers definition, references, type-definition, hover, and document symbol queries.

const std = @import("std");

/// Zero-indexed line/character position in a text document.
pub const Position = struct {
    line: u32,
    character: u32,
};

/// A range in a text document between two positions.
pub const Range = struct {
    start: Position,
    end: Position,
};

/// A location in a text document identified by URI and range.
pub const Location = struct {
    uri: []const u8,
    range: Range,
};

/// Identifies a text document by its URI.
pub const TextDocumentIdentifier = struct {
    uri: []const u8,
};

/// A position in a specific text document.
pub const TextDocumentPositionParams = struct {
    textDocument: TextDocumentIdentifier,
    position: Position,
};

/// Content type of hover/markup results.
pub const MarkupKind = enum {
    plaintext,
    markdown,
};

/// Structured content returned by hover requests.
pub const MarkupContent = struct {
    kind: MarkupKind,
    value: []const u8,
};

/// Result of a textDocument/hover request.
pub const Hover = struct {
    contents: HoverContents,

    pub const HoverContents = union(enum) {
        plain_string: []const u8,
        markup: MarkupContent,
    };
};

/// Symbol classification used by textDocument/documentSymbol responses.
/// Integer values 1-26 match the protocol definition; any value outside
/// that range is stored as-is in the non-exhaustive enum.
pub const SymbolKind = enum(u8) {
    file = 1,
    module = 2,
    namespace = 3,
    package = 4,
    class = 5,
    method = 6,
    property = 7,
    field = 8,
    constructor = 9,
    @"enum" = 10,
    interface = 11,
    function = 12,
    variable = 13,
    constant = 14,
    string = 15,
    number = 16,
    boolean = 17,
    array = 18,
    object = 19,
    key = 20,
    null = 21,
    enum_member = 22,
    @"struct" = 23,
    event = 24,
    operator = 25,
    type_parameter = 26,
    _,

    pub fn fromInt(n: i64) SymbolKind {
        if (n < 1 or n > 255) return @enumFromInt(0);
        return @enumFromInt(@as(u8, @intCast(n)));
    }
};

/// Error payload from an LSP JSON-RPC error response.
pub const ResponseError = struct {
    code: i32,
    message: []const u8,
};

/// Parsed JSON-RPC response envelope from an LSP server.
pub const Response = struct {
    id: ?i64 = null,
    result_raw: ?[]const u8 = null,
    @"error": ?ResponseError = null,

    /// Free all allocator-owned slices in this response.
    pub fn deinit(self: *const Response, allocator: std.mem.Allocator) void {
        if (self.@"error") |e| {
            if (e.message.len > 0) allocator.free(e.message);
        }
        if (self.result_raw) |r| allocator.free(r);
    }
};

/// Parse a JSON-RPC response from raw JSON bytes.
/// Caller owns the returned Response and must call deinit() to free it.
pub fn parseResponse(allocator: std.mem.Allocator, json: []const u8) !Response {
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, json, .{});
    defer parsed.deinit();

    const root = parsed.value;
    if (root != .object) return error.UnexpectedToken;
    const obj = root.object;

    var resp = Response{};

    if (obj.get("id")) |id_val| {
        if (id_val == .integer) resp.id = id_val.integer;
    }

    if (obj.get("error")) |err_val| {
        if (err_val == .object) {
            const err_obj = err_val.object;
            const code: i32 = if (err_obj.get("code")) |c| blk: {
                if (c == .integer) break :blk @intCast(c.integer);
                break :blk 0;
            } else 0;

            var message: []const u8 = "";
            if (err_obj.get("message")) |m| {
                if (m == .string) {
                    message = try allocator.dupe(u8, m.string);
                }
            }
            resp.@"error" = .{ .code = code, .message = message };
        }
    }
    errdefer if (resp.@"error") |e| {
        if (e.message.len > 0) allocator.free(e.message);
    };

    if (obj.get("result")) |result_val| {
        if (result_val != .null) {
            var aw: std.Io.Writer.Allocating = .init(allocator);
            errdefer aw.deinit();
            var stream: std.json.Stringify = .{ .writer = &aw.writer };
            stream.write(result_val) catch return error.OutOfMemory;
            resp.result_raw = aw.toOwnedSlice() catch return error.OutOfMemory;
        }
    }

    return resp;
}

/// Parse a single Location from a JSON object string.
pub fn parseLocation(allocator: std.mem.Allocator, json: []const u8) !Location {
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, json, .{});
    defer parsed.deinit();

    const root = parsed.value;
    if (root != .object) return error.UnexpectedToken;
    return parseLocationFromValue(allocator, root.object);
}

fn parseLocationFromValue(allocator: std.mem.Allocator, obj: std.json.ObjectMap) !Location {
    const uri_val = obj.get("uri") orelse return error.UnexpectedToken;
    if (uri_val != .string) return error.UnexpectedToken;
    const uri = try allocator.dupe(u8, uri_val.string);
    errdefer allocator.free(uri);

    const range_val = obj.get("range") orelse return error.UnexpectedToken;
    if (range_val != .object) return error.UnexpectedToken;
    const range_obj = range_val.object;

    const start_val = range_obj.get("start") orelse return error.UnexpectedToken;
    if (start_val != .object) return error.UnexpectedToken;
    const start = try parsePositionFromValue(start_val.object);

    const end_val = range_obj.get("end") orelse return error.UnexpectedToken;
    if (end_val != .object) return error.UnexpectedToken;
    const end = try parsePositionFromValue(end_val.object);

    return .{
        .uri = uri,
        .range = .{ .start = start, .end = end },
    };
}

fn parsePositionFromValue(obj: std.json.ObjectMap) !Position {
    const line_val = obj.get("line") orelse return error.UnexpectedToken;
    if (line_val != .integer) return error.UnexpectedToken;
    const char_val = obj.get("character") orelse return error.UnexpectedToken;
    if (char_val != .integer) return error.UnexpectedToken;
    return .{
        .line = @intCast(line_val.integer),
        .character = @intCast(char_val.integer),
    };
}

/// Parse an array of Locations from a JSON array string.
pub fn parseLocationArray(allocator: std.mem.Allocator, json: []const u8) ![]Location {
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, json, .{});
    defer parsed.deinit();

    const root = parsed.value;
    if (root != .array) return error.UnexpectedToken;

    const items = root.array.items;
    if (items.len == 0) {
        return try allocator.alloc(Location, 0);
    }

    const locs = try allocator.alloc(Location, items.len);
    var filled: usize = 0;
    errdefer {
        for (locs[0..filled]) |loc| allocator.free(loc.uri);
        allocator.free(locs);
    }

    for (items) |item| {
        if (item != .object) return error.UnexpectedToken;
        locs[filled] = try parseLocationFromValue(allocator, item.object);
        filled += 1;
    }

    return locs;
}

/// Free an allocator-owned Location slice returned by any textDocument location query.
pub fn freeLocationArray(allocator: std.mem.Allocator, locs: []Location) void {
    for (locs) |loc| allocator.free(loc.uri);
    allocator.free(locs);
}

/// Parse a Hover result from a JSON object string.
pub fn parseHover(allocator: std.mem.Allocator, json: []const u8) !Hover {
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, json, .{});
    defer parsed.deinit();

    const root = parsed.value;
    if (root != .object) return error.UnexpectedToken;
    const obj = root.object;

    const contents_val = obj.get("contents") orelse return error.UnexpectedToken;

    // MarkupContent has "kind" and "value" string fields.
    if (contents_val == .object) {
        const contents_obj = contents_val.object;
        if (contents_obj.get("kind")) |kind_val| {
            if (kind_val == .string) {
                if (contents_obj.get("value")) |val_val| {
                    if (val_val == .string) {
                        const kind: MarkupKind = if (std.mem.eql(u8, kind_val.string, "markdown"))
                            .markdown
                        else
                            .plaintext;
                        const value = try allocator.dupe(u8, val_val.string);
                        return .{
                            .contents = .{ .markup = .{ .kind = kind, .value = value } },
                        };
                    }
                }
            }
        }
    }

    // Plain string variant.
    if (contents_val == .string) {
        const value = try allocator.dupe(u8, contents_val.string);
        return .{
            .contents = .{ .plain_string = value },
        };
    }

    return error.UnexpectedToken;
}

/// Free a Hover's owned string content.
pub fn freeHover(allocator: std.mem.Allocator, hover: Hover) void {
    switch (hover.contents) {
        .markup => |m| allocator.free(m.value),
        .plain_string => |s| allocator.free(s),
    }
}

test "parse LSP responses" {
    // Arrange
    const allocator = std.testing.allocator;

    // Act: single Location (definition response)
    const def_json =
        \\{"uri":"file:///src/main.zig","range":{"start":{"line":10,"character":4},"end":{"line":10,"character":14}}}
    ;
    const loc = try parseLocation(allocator, def_json);
    defer allocator.free(loc.uri);
    try std.testing.expectEqualStrings("file:///src/main.zig", loc.uri);
    try std.testing.expectEqual(@as(u32, 10), loc.range.start.line);
    try std.testing.expectEqual(@as(u32, 4), loc.range.start.character);

    // Act: empty references array
    const empty_json = "[]";
    const empty_locs = try parseLocationArray(allocator, empty_json);
    defer allocator.free(empty_locs);
    try std.testing.expectEqual(@as(usize, 0), empty_locs.len);

    // Act: hover with MarkupContent
    const hover_json =
        \\{"contents":{"kind":"markdown","value":"fn readConfig() ![]const u8"}}
    ;
    const hover = try parseHover(allocator, hover_json);
    defer freeHover(allocator, hover);
    try std.testing.expectEqual(MarkupKind.markdown, hover.contents.markup.kind);

    // Act: error response (tested via parseResponse)
    const err_json =
        \\{"jsonrpc":"2.0","id":1,"error":{"code":-32601,"message":"method not found"}}
    ;
    const resp = try parseResponse(allocator, err_json);
    defer resp.deinit(allocator);
    try std.testing.expect(resp.@"error" != null);
    try std.testing.expectEqual(@as(i32, -32601), resp.@"error".?.code);
    try std.testing.expectEqualStrings("method not found", resp.@"error".?.message);
}

test "protocol types are correctly sized" {
    comptime {
        std.debug.assert(@sizeOf(Position) == 8);
        std.debug.assert(@hasField(ResponseError, "code"));
        std.debug.assert(@hasField(ResponseError, "message"));
        std.debug.assert(@hasField(Location, "uri"));
        std.debug.assert(@hasField(Location, "range"));
    }
}

test "parse response id" {
    // Arrange
    const allocator = std.testing.allocator;

    // Act
    const json =
        \\{"jsonrpc":"2.0","id":42,"result":null}
    ;
    const resp = try parseResponse(allocator, json);
    defer resp.deinit(allocator);

    // Assert
    try std.testing.expectEqual(@as(i64, 42), resp.id.?);
}

test "freeLocationArray releases all URIs" {
    // Arrange
    const allocator = std.testing.allocator;
    const json =
        \\[{"uri":"file:///a.zig","range":{"start":{"line":0,"character":0},"end":{"line":0,"character":1}}},{"uri":"file:///b.zig","range":{"start":{"line":5,"character":2},"end":{"line":5,"character":9}}}]
    ;

    // Act
    const locs = try parseLocationArray(allocator, json);
    freeLocationArray(allocator, locs);

    // Assert: no leak (checked by std.testing.allocator)
}

test "SymbolKind fromInt maps known values" {
    // Arrange / Act / Assert
    try std.testing.expectEqual(SymbolKind.file, SymbolKind.fromInt(1));
    try std.testing.expectEqual(SymbolKind.function, SymbolKind.fromInt(12));
    try std.testing.expectEqual(SymbolKind.type_parameter, SymbolKind.fromInt(26));
    // Out-of-range values map to the zero sentinel.
    try std.testing.expectEqual(@as(u8, 0), @intFromEnum(SymbolKind.fromInt(0)));
    try std.testing.expectEqual(@as(u8, 0), @intFromEnum(SymbolKind.fromInt(-1)));
    try std.testing.expectEqual(@as(u8, 0), @intFromEnum(SymbolKind.fromInt(300)));
}
