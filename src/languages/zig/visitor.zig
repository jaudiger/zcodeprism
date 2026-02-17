const std = @import("std");
const graph_mod = @import("../../core/graph.zig");
const node_mod = @import("../../core/node.zig");
const edge_mod = @import("../../core/edge.zig");
const types = @import("../../core/types.zig");
const lang = @import("../language.zig");
const ts = @import("tree-sitter");
const ts_api = @import("../../parser/tree_sitter_api.zig");

const Graph = graph_mod.Graph;
const Node = node_mod.Node;
const Edge = edge_mod.Edge;
const NodeId = types.NodeId;
const NodeKind = types.NodeKind;
const EdgeType = types.EdgeType;
const EdgeSource = types.EdgeSource;
const Visibility = types.Visibility;
const Language = types.Language;
const LangMeta = lang.LangMeta;

/// Parse Zig source code and populate the graph with nodes and edges.
/// This is the entry point used by the LanguageSupport registry.
pub fn parse(source: []const u8, graph: *anyopaque) anyerror!void {
    const g: *Graph = @ptrCast(@alignCast(graph));

    const line_count = ts_api.countLines(source);

    // Parse source with tree-sitter first so we can collect module doc comments.
    const tree = ts_api.parseSource(ts_api.zigLanguage(), source) orelse {
        // If tree-sitter parsing fails, create a bare file node.
        _ = try g.addNode(.{
            .id = .root,
            .name = "",
            .kind = .file,
            .language = .zig,
            .visibility = .public,
            .line_start = 1,
            .line_end = if (line_count > 0) line_count else null,
        });
        return;
    };
    defer tree.destroy();

    const root = tree.rootNode();

    // Collect module doc comments (//!) from the beginning of the file.
    const module_doc = collectModuleDocComment(source, root);

    // Create file node (always the first node).
    const file_id = try g.addNode(.{
        .id = .root,
        .name = "",
        .kind = .file,
        .language = .zig,
        .visibility = .public,
        .line_start = 1,
        .line_end = if (line_count > 0) line_count else null,
        .doc = module_doc,
    });

    // Pass 1: create nodes from declarations.
    var i: u32 = 0;
    while (i < root.childCount()) : (i += 1) {
        const child = root.child(i) orelse continue;
        if (!child.isNamed()) continue;
        processDeclaration(g, source, child, file_id) catch {};
    }

    // Pass 2: create edges (calls, uses_type).
    walkForEdges(g, source, root);
}

// =========================================================================
// Pass 1 — Node creation
// =========================================================================

fn processDeclaration(g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId) anyerror!void {
    const kind_str = ts_node.kind();

    if (std.mem.eql(u8, kind_str, "variable_declaration")) {
        try processVariableDecl(g, source, ts_node, parent_id);
    } else if (std.mem.eql(u8, kind_str, "function_declaration")) {
        try processFunctionDecl(g, source, ts_node, parent_id);
    } else if (std.mem.eql(u8, kind_str, "test_declaration")) {
        try processTestDecl(g, source, ts_node, parent_id);
    } else if (std.mem.eql(u8, kind_str, "container_field")) {
        try processContainerField(g, source, ts_node, parent_id);
    }
}

fn processVariableDecl(g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId) anyerror!void {
    const name = getIdentifierName(source, ts_node) orelse return;
    const visibility = detectVisibility(ts_node);
    const doc = collectDocComment(source, ts_node);

    // Classify the value: struct, enum, error set, import, or plain constant.
    const classification = classifyVariableValue(source, ts_node);
    const kind = classification.kind;

    // Comptime detection: constants with explicit type annotation are comptime-known.
    var lang_meta: LangMeta = .{ .none = {} };
    if (kind == .constant and hasTypeAnnotation(ts_node)) {
        lang_meta = .{ .zig = .{ .is_comptime = true } };
    }

    const node_id = try g.addNode(.{
        .id = .root,
        .name = name,
        .kind = kind,
        .language = .zig,
        .parent_id = parent_id,
        .visibility = visibility,
        .doc = doc,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
        .lang_meta = lang_meta,
    });

    // Recurse into struct body for nested declarations (methods, nested types).
    if (classification.struct_body) |body| {
        var i: u32 = 0;
        while (i < body.childCount()) : (i += 1) {
            const child = body.child(i) orelse continue;
            if (!child.isNamed()) continue;
            processDeclaration(g, source, child, node_id) catch {};
        }
    }
}

fn processFunctionDecl(g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId) anyerror!void {
    const name = getIdentifierName(source, ts_node) orelse return;
    const visibility = detectVisibility(ts_node);
    const doc = collectDocComment(source, ts_node);

    // Detect type-returning generic functions: `fn Foo(comptime T: type) type { return struct { ... }; }`
    if (returnsType(source, ts_node)) {
        if (findReturnedTypeBody(ts_node)) |body_info| {
            const kind: NodeKind = if (body_info.is_enum) .enum_def else .type_def;
            const node_id = try g.addNode(.{
                .id = .root,
                .name = name,
                .kind = kind,
                .language = .zig,
                .parent_id = parent_id,
                .visibility = visibility,
                .doc = doc,
                .line_start = ts_node.startPoint().row + 1,
                .line_end = ts_node.endPoint().row + 1,
            });

            // Recurse into the returned struct/union/enum body for inner declarations.
            var i: u32 = 0;
            while (i < body_info.body.childCount()) : (i += 1) {
                const child = body_info.body.child(i) orelse continue;
                if (!child.isNamed()) continue;
                processDeclaration(g, source, child, node_id) catch {};
            }
            return;
        }
    }

    _ = try g.addNode(.{
        .id = .root,
        .name = name,
        .kind = .function,
        .language = .zig,
        .parent_id = parent_id,
        .visibility = visibility,
        .doc = doc,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
    });
}

fn processTestDecl(g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId) anyerror!void {
    const name = getTestName(source, ts_node);

    _ = try g.addNode(.{
        .id = .root,
        .name = name,
        .kind = .test_def,
        .language = .zig,
        .parent_id = parent_id,
        .visibility = .private,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
    });
}

fn processContainerField(g: *Graph, source: []const u8, ts_node: ts.Node, parent_id: NodeId) anyerror!void {
    const name = getIdentifierName(source, ts_node) orelse return;
    const visibility = detectVisibility(ts_node);
    const doc = collectDocComment(source, ts_node);

    _ = try g.addNode(.{
        .id = .root,
        .name = name,
        .kind = .field,
        .language = .zig,
        .parent_id = parent_id,
        .visibility = visibility,
        .doc = doc,
        .line_start = ts_node.startPoint().row + 1,
        .line_end = ts_node.endPoint().row + 1,
    });
}

// =========================================================================
// Pass 2 — Edge creation
// =========================================================================

fn walkForEdges(g: *Graph, source: []const u8, ts_node: ts.Node) void {
    const kind_str = ts_node.kind();

    if (std.mem.eql(u8, kind_str, "function_declaration")) {
        if (getIdentifierName(source, ts_node)) |name| {
            if (findFunctionByName(g, name)) |fn_id| {
                // Scan function body for call expressions.
                scanForCalls(g, source, fn_id, ts_node);

                // Scan function signature (excluding body) for type references.
                scanSignatureForTypes(g, source, fn_id, ts_node);

                // Scan function body for type references (struct literals, static
                // method calls, comptime type arguments).
                scanBodyForTypes(g, source, fn_id, ts_node);
            }
        }
    } else if (std.mem.eql(u8, kind_str, "test_declaration")) {
        const test_name = getTestName(source, ts_node);
        if (findTestByName(g, test_name)) |test_id| {
            scanForCalls(g, source, test_id, ts_node);
        }
    }

    // Recurse into all named children to find nested function declarations.
    var i: u32 = 0;
    while (i < ts_node.namedChildCount()) : (i += 1) {
        const child = ts_node.namedChild(i) orelse continue;
        walkForEdges(g, source, child);
    }
}

fn scanForCalls(g: *Graph, source: []const u8, caller_id: NodeId, ts_node: ts.Node) void {
    if (std.mem.eql(u8, ts_node.kind(), "call_expression")) {
        // The first named child of a call_expression is the function reference.
        if (ts_node.namedChild(0)) |fn_ref| {
            const callee_name = extractCallTarget(source, fn_ref);
            if (callee_name) |name| {
                if (findFunctionByName(g, name)) |callee_id| {
                    if (@intFromEnum(caller_id) != @intFromEnum(callee_id)) {
                        addEdgeIfNew(g, caller_id, callee_id, .calls);
                    }
                }
            }
        }
    }

    // Recurse into all children, but stop at scope boundaries.
    var i: u32 = 0;
    while (i < ts_node.childCount()) : (i += 1) {
        const child = ts_node.child(i) orelse continue;
        const child_kind = child.kind();
        if (std.mem.eql(u8, child_kind, "function_declaration") or
            std.mem.eql(u8, child_kind, "test_declaration")) continue;
        scanForCalls(g, source, caller_id, child);
    }
}

/// Extract the function name from a call target node.
/// Handles both bare identifiers (`foo(...)`) and field access (`self.foo(...)`).
fn extractCallTarget(source: []const u8, fn_ref: ts.Node) ?[]const u8 {
    const kind = fn_ref.kind();
    if (std.mem.eql(u8, kind, "identifier")) {
        return ts_api.nodeText(source, fn_ref);
    }
    if (std.mem.eql(u8, kind, "field_expression")) {
        // The last named child is the field/method name.
        const count = fn_ref.namedChildCount();
        if (count >= 1) {
            if (fn_ref.namedChild(count - 1)) |field_name| {
                return ts_api.nodeText(source, field_name);
            }
        }
    }
    return null;
}

fn scanSignatureForTypes(g: *Graph, source: []const u8, fn_id: NodeId, fn_node: ts.Node) void {
    // Scan all children except the body block for type identifier references.
    var i: u32 = 0;
    while (i < fn_node.childCount()) : (i += 1) {
        const child = fn_node.child(i) orelse continue;
        if (std.mem.eql(u8, child.kind(), "block")) continue;
        scanForTypeIdentifiers(g, source, fn_id, child);
    }
}

fn scanBodyForTypes(g: *Graph, source: []const u8, fn_id: NodeId, fn_node: ts.Node) void {
    // Scan only the body block for type identifier references.
    var i: u32 = 0;
    while (i < fn_node.childCount()) : (i += 1) {
        const child = fn_node.child(i) orelse continue;
        if (std.mem.eql(u8, child.kind(), "block")) {
            scanForTypeIdentifiers(g, source, fn_id, child);
            return;
        }
    }
}

fn scanForTypeIdentifiers(g: *Graph, source: []const u8, fn_id: NodeId, ts_node: ts.Node) void {
    if (std.mem.eql(u8, ts_node.kind(), "identifier")) {
        const name = ts_api.nodeText(source, ts_node);
        for (g.nodes.items, 0..) |n, idx| {
            const is_type = n.kind == .type_def or n.kind == .enum_def;
            // Match PascalCase constants as type aliases (e.g. `const Bar = Foo;`).
            // Lowercase constants like `const max = 100` are excluded.
            const is_type_alias = n.kind == .constant and
                n.name.len > 0 and n.name[0] >= 'A' and n.name[0] <= 'Z';
            if ((is_type or is_type_alias) and std.mem.eql(u8, n.name, name)) {
                addEdgeIfNew(g, fn_id, @enumFromInt(idx), .uses_type);
                break;
            }
        }
    }

    // Recurse into all children, but stop at scope boundaries.
    var i: u32 = 0;
    while (i < ts_node.childCount()) : (i += 1) {
        const child = ts_node.child(i) orelse continue;
        const child_kind = child.kind();
        if (std.mem.eql(u8, child_kind, "function_declaration") or
            std.mem.eql(u8, child_kind, "test_declaration")) continue;
        scanForTypeIdentifiers(g, source, fn_id, child);
    }
}

fn findFunctionByName(g: *const Graph, name: []const u8) ?NodeId {
    for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .function and std.mem.eql(u8, n.name, name)) {
            return @enumFromInt(i);
        }
    }
    return null;
}

fn findTestByName(g: *const Graph, name: []const u8) ?NodeId {
    for (g.nodes.items, 0..) |n, i| {
        if (n.kind == .test_def and std.mem.eql(u8, n.name, name)) {
            return @enumFromInt(i);
        }
    }
    return null;
}

fn addEdgeIfNew(g: *Graph, source_id: NodeId, target_id: NodeId, edge_type: EdgeType) void {
    for (g.edges.items) |e| {
        if (e.source_id == source_id and e.target_id == target_id and e.edge_type == edge_type) {
            return;
        }
    }
    _ = g.addEdge(.{
        .source_id = source_id,
        .target_id = target_id,
        .edge_type = edge_type,
        .source = .tree_sitter,
    }) catch {};
}

// =========================================================================
// Helpers
// =========================================================================

/// Check if a function_declaration has `type` as its return type.
fn returnsType(source: []const u8, fn_node: ts.Node) bool {
    // The return type appears as a `builtin_type` child with text "type",
    // positioned before the block. We check non-named children too since
    // `builtin_type` is a named child.
    var i: u32 = 0;
    while (i < fn_node.childCount()) : (i += 1) {
        const child = fn_node.child(i) orelse continue;
        const kind = child.kind();
        if (std.mem.eql(u8, kind, "block")) break;
        if (std.mem.eql(u8, kind, "builtin_type")) {
            const text = ts_api.nodeText(source, child);
            if (std.mem.eql(u8, text, "type")) return true;
        }
    }
    return false;
}

const ReturnedTypeBody = struct {
    body: ts.Node,
    is_enum: bool,
};

/// Search a function body for `return struct { ... }`, `return union { ... }`, or `return enum { ... }`.
/// Returns the struct/union/enum body node if found.
fn findReturnedTypeBody(fn_node: ts.Node) ?ReturnedTypeBody {
    // Path: function_declaration → block → expression_statement → return_expression → struct/union/enum_declaration
    var i: u32 = 0;
    while (i < fn_node.childCount()) : (i += 1) {
        const child = fn_node.child(i) orelse continue;
        if (!std.mem.eql(u8, child.kind(), "block")) continue;

        // Search block children for expression_statement containing a return.
        var j: u32 = 0;
        while (j < child.childCount()) : (j += 1) {
            const stmt = child.child(j) orelse continue;
            if (!std.mem.eql(u8, stmt.kind(), "expression_statement")) continue;

            // Look for return_expression inside the expression_statement.
            var k: u32 = 0;
            while (k < stmt.childCount()) : (k += 1) {
                const ret = stmt.child(k) orelse continue;
                if (!std.mem.eql(u8, ret.kind(), "return_expression")) continue;

                // The returned value is a named child of return_expression (after "return").
                var l: u32 = 0;
                while (l < ret.namedChildCount()) : (l += 1) {
                    const val = ret.namedChild(l) orelse continue;
                    const val_kind = val.kind();
                    if (std.mem.eql(u8, val_kind, "struct_declaration")) {
                        return .{ .body = val, .is_enum = false };
                    }
                    if (std.mem.eql(u8, val_kind, "union_declaration")) {
                        return .{ .body = val, .is_enum = false };
                    }
                    if (std.mem.eql(u8, val_kind, "enum_declaration")) {
                        return .{ .body = val, .is_enum = true };
                    }
                }
            }
        }
    }
    return null;
}

fn getIdentifierName(source: []const u8, ts_node: ts.Node) ?[]const u8 {
    var i: u32 = 0;
    while (i < ts_node.namedChildCount()) : (i += 1) {
        const child = ts_node.namedChild(i) orelse continue;
        if (std.mem.eql(u8, child.kind(), "identifier")) {
            const raw = ts_api.nodeText(source, child);
            return stripQuotedIdentifier(raw);
        }
    }
    return null;
}

fn stripQuotedIdentifier(raw: []const u8) []const u8 {
    // Handle @"name" syntax for unicode or keyword identifiers.
    if (raw.len >= 3 and raw[0] == '@' and raw[1] == '"' and raw[raw.len - 1] == '"') {
        return raw[2 .. raw.len - 1];
    }
    return raw;
}

fn getTestName(source: []const u8, ts_node: ts.Node) []const u8 {
    var i: u32 = 0;
    while (i < ts_node.namedChildCount()) : (i += 1) {
        const child = ts_node.namedChild(i) orelse continue;
        if (std.mem.eql(u8, child.kind(), "string")) {
            // String-literal test name: test "name" { }
            // Look for string_content inside the string node.
            var j: u32 = 0;
            while (j < child.namedChildCount()) : (j += 1) {
                const sc = child.namedChild(j) orelse continue;
                if (std.mem.eql(u8, sc.kind(), "string_content")) {
                    return ts_api.nodeText(source, sc);
                }
            }
            // Fallback: strip quotes.
            const text = ts_api.nodeText(source, child);
            if (text.len >= 2 and text[0] == '"' and text[text.len - 1] == '"') {
                return text[1 .. text.len - 1];
            }
            return text;
        }
        if (std.mem.eql(u8, child.kind(), "identifier")) {
            // Decl-reference test name: test Value { } or test @"edge case" { }
            const text = ts_api.nodeText(source, child);
            return stripQuotedIdentifier(text);
        }
    }
    return "test";
}

fn detectVisibility(ts_node: ts.Node) Visibility {
    // Check all children (including anonymous) for "pub" keyword.
    var i: u32 = 0;
    while (i < ts_node.childCount()) : (i += 1) {
        const child = ts_node.child(i) orelse continue;
        if (std.mem.eql(u8, child.kind(), "pub")) return .public;
    }
    return .private;
}

/// Collect module-level doc comments (//!) from the beginning of a source file.
/// Scans direct children of the root node for consecutive comment nodes whose
/// text starts with "//!". Stops at the first non-//! child.
fn collectModuleDocComment(source: []const u8, root: ts.Node) ?[]const u8 {
    var first_start: ?u32 = null;
    var last_end: ?u32 = null;

    var i: u32 = 0;
    while (i < root.childCount()) : (i += 1) {
        const child = root.child(i) orelse continue;
        if (!std.mem.eql(u8, child.kind(), "comment")) {
            // Skip anonymous non-comment tokens (e.g. punctuation).
            if (!child.isNamed()) continue;
            // Hit a declaration — stop.
            break;
        }
        const text = source[child.startByte()..child.endByte()];
        if (text.len >= 3 and text[0] == '/' and text[1] == '/' and text[2] == '!') {
            if (first_start == null) first_start = child.startByte();
            last_end = child.endByte();
        } else {
            // Non-//! comment (e.g. /// or //) — stop collecting.
            break;
        }
    }

    if (first_start) |start| {
        return source[start..last_end.?];
    }
    return null;
}

fn collectDocComment(source: []const u8, ts_node: ts.Node) ?[]const u8 {
    // Walk previous siblings to collect consecutive doc comment lines.
    var first_doc_start: ?u32 = null;
    var last_doc_end: ?u32 = null;
    var current = ts_node.prevSibling();
    while (current) |sib| {
        if (std.mem.eql(u8, sib.kind(), "comment")) {
            const text = source[sib.startByte()..sib.endByte()];
            if (text.len >= 3 and text[0] == '/' and text[1] == '/' and text[2] == '/') {
                first_doc_start = sib.startByte();
                if (last_doc_end == null) last_doc_end = sib.endByte();
                current = sib.prevSibling();
                continue;
            }
        }
        break;
    }

    if (first_doc_start) |start| {
        return source[start..last_doc_end.?];
    }
    return null;
}

fn hasTypeAnnotation(ts_node: ts.Node) bool {
    // Check for ":" anonymous child (indicates explicit type annotation).
    var i: u32 = 0;
    while (i < ts_node.childCount()) : (i += 1) {
        const child = ts_node.child(i) orelse continue;
        if (std.mem.eql(u8, child.kind(), ":")) return true;
    }
    return false;
}

const Classification = struct {
    kind: NodeKind,
    struct_body: ?ts.Node,
};

fn classifyVariableValue(source: []const u8, ts_node: ts.Node) Classification {
    var result = Classification{ .kind = .constant, .struct_body = null };
    classifyRecursive(source, ts_node, &result, 0);
    return result;
}

fn classifyRecursive(source: []const u8, ts_node: ts.Node, result: *Classification, depth: u32) void {
    if (depth > 5) return;
    var i: u32 = 0;
    while (i < ts_node.namedChildCount()) : (i += 1) {
        const child = ts_node.namedChild(i) orelse continue;
        const kind = child.kind();
        if (std.mem.eql(u8, kind, "struct_declaration")) {
            result.kind = .type_def;
            result.struct_body = child;
            return;
        }
        if (std.mem.eql(u8, kind, "union_declaration")) {
            result.kind = .type_def;
            result.struct_body = child;
            return;
        }
        if (std.mem.eql(u8, kind, "enum_declaration")) {
            result.kind = .enum_def;
            result.struct_body = child;
            return;
        }
        if (std.mem.eql(u8, kind, "error_set_declaration")) {
            result.kind = .error_def;
            return;
        }
        if (std.mem.eql(u8, kind, "builtin_identifier")) {
            const text = ts_api.nodeText(source, child);
            if (std.mem.eql(u8, text, "@import")) {
                result.kind = .import_decl;
                return;
            }
        }
        classifyRecursive(source, child, result, depth + 1);
        if (result.kind != .constant) return;
    }
}

// =========================================================================
// Nominal tests — parse tests/fixtures/simple.zig
// =========================================================================

const fixtures = @import("test-fixtures");

test "parses public function" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — at least one node with kind=function and visibility=public
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and n.visibility == .public) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "parses private function" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — at least one node with kind=function and visibility=private
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and n.visibility == .private) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "parses struct" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — at least one node with kind=type_def
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .type_def) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "parses enum" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — at least one node with kind=enum_def
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .enum_def) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "parses constant" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — at least one node with kind=constant
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .constant) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "parses test block" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — at least one node with kind=test_def
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .test_def) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "parses error set" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — at least one node with kind=error_def
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .error_def) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "attaches doc comment" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — the public function "defaultPoint" has doc != null
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and n.visibility == .public and
            std.mem.eql(u8, n.name, "defaultPoint"))
        {
            try std.testing.expect(n.doc != null);
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "sets parent_id for method" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — method "manhattan" has parent_id pointing to the struct "Point"
    var method_node: ?*const Node = null;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and std.mem.eql(u8, n.name, "manhattan")) {
            method_node = n;
            break;
        }
    }
    try std.testing.expect(method_node != null);
    try std.testing.expect(method_node.?.parent_id != null);

    const parent = g.getNode(method_node.?.parent_id.?);
    try std.testing.expect(parent != null);
    try std.testing.expectEqual(NodeKind.type_def, parent.?.kind);
    try std.testing.expectEqualStrings("Point", parent.?.name);
}

test "sets parent_id for struct" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — struct "Point" has parent_id pointing to the file node
    var struct_node: ?*const Node = null;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Point")) {
            struct_node = n;
            break;
        }
    }
    try std.testing.expect(struct_node != null);
    try std.testing.expect(struct_node.?.parent_id != null);

    const parent = g.getNode(struct_node.?.parent_id.?);
    try std.testing.expect(parent != null);
    try std.testing.expectEqual(NodeKind.file, parent.?.kind);
}

test "sets parent_id for top-level function" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — top-level function "defaultPoint" has parent_id pointing to file node
    var fn_node: ?*const Node = null;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and std.mem.eql(u8, n.name, "defaultPoint")) {
            fn_node = n;
            break;
        }
    }
    try std.testing.expect(fn_node != null);
    try std.testing.expect(fn_node.?.parent_id != null);

    const parent = g.getNode(fn_node.?.parent_id.?);
    try std.testing.expect(parent != null);
    try std.testing.expectEqual(NodeKind.file, parent.?.kind);
}

test "populates ZigMeta for comptime" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — constant "buffer_size" has lang_meta.zig.is_comptime == true
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .constant and std.mem.eql(u8, n.name, "buffer_size")) {
            switch (n.lang_meta) {
                .zig => |zm| {
                    try std.testing.expect(zm.is_comptime);
                    found = true;
                },
                .none => return error.ExpectedZigMeta,
            }
            break;
        }
    }
    try std.testing.expect(found);
}

test "creates calls edge" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — at least one edge with edge_type=calls exists
    //          (manhattan calls abs in the fixture)
    var found = false;
    for (g.edges.items) |e| {
        if (e.edge_type == .calls) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "creates calls edge for method call via self" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — isWithinRadius calls manhattan via self.manhattan()
    var caller_id: ?NodeId = null;
    var callee_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "isWithinRadius")) {
            caller_id = @enumFromInt(idx);
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "manhattan")) {
            callee_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(caller_id != null);
    try std.testing.expect(callee_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == caller_id.? and e.target_id == callee_id.? and e.edge_type == .calls) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "creates uses_type edge" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — at least one edge with edge_type=uses_type exists
    //          (defaultPoint uses/returns Point in the fixture)
    var found = false;
    for (g.edges.items) |e| {
        if (e.edge_type == .uses_type) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "all edges have source tree_sitter" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — every edge has source == .tree_sitter
    try std.testing.expect(g.edgeCount() > 0);
    for (g.edges.items) |e| {
        try std.testing.expectEqual(EdgeSource.tree_sitter, e.source);
    }
}

test "all nodes have language zig" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — every node has language == .zig
    try std.testing.expect(g.nodeCount() > 0);
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        try std.testing.expectEqual(Language.zig, n.language);
    }
}

test "file node has correct line count" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — file node (first node, kind=file) has line_end == 75
    //          (simple.zig has 75 lines of content)
    const file_node = g.getNode(@enumFromInt(0));
    try std.testing.expect(file_node != null);
    try std.testing.expectEqual(NodeKind.file, file_node.?.kind);
    try std.testing.expect(file_node.?.line_end != null);
    try std.testing.expectEqual(@as(u32, 75), file_node.?.line_end.?);
}

// =========================================================================
// Edge case tests
// =========================================================================

test "empty file produces only file node" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.empty, &g);

    // Assert — exactly 1 node (kind=file), zero edges
    try std.testing.expectEqual(@as(usize, 1), g.nodeCount());
    try std.testing.expectEqual(@as(usize, 0), g.edgeCount());
    const file_node = g.getNode(@enumFromInt(0));
    try std.testing.expect(file_node != null);
    try std.testing.expectEqual(NodeKind.file, file_node.?.kind);
}

test "only comments produces only file node" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.only_comments, &g);

    // Assert — exactly 1 node (kind=file), no declarations parsed
    try std.testing.expectEqual(@as(usize, 1), g.nodeCount());
    const file_node = g.getNode(@enumFromInt(0));
    try std.testing.expect(file_node != null);
    try std.testing.expectEqual(NodeKind.file, file_node.?.kind);
}

test "no pub file has all private nodes" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.no_pub, &g);

    // Assert — all function nodes have visibility=private
    var fn_count: usize = 0;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function) {
            try std.testing.expectEqual(Visibility.private, n.visibility);
            fn_count += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 3), fn_count);
}

test "deeply nested sets correct parent chain" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.deeply_nested, &g);

    // Assert — innerMethod → Inner → Middle → Outer → file
    // Find "innerMethod"
    var inner_method: ?*const Node = null;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and std.mem.eql(u8, n.name, "innerMethod")) {
            inner_method = n;
            break;
        }
    }
    try std.testing.expect(inner_method != null);

    // innerMethod → Inner (type_def)
    try std.testing.expect(inner_method.?.parent_id != null);
    const inner_struct = g.getNode(inner_method.?.parent_id.?);
    try std.testing.expect(inner_struct != null);
    try std.testing.expectEqual(NodeKind.type_def, inner_struct.?.kind);
    try std.testing.expectEqualStrings("Inner", inner_struct.?.name);

    // Inner → Middle (type_def)
    try std.testing.expect(inner_struct.?.parent_id != null);
    const middle_struct = g.getNode(inner_struct.?.parent_id.?);
    try std.testing.expect(middle_struct != null);
    try std.testing.expectEqual(NodeKind.type_def, middle_struct.?.kind);
    try std.testing.expectEqualStrings("Middle", middle_struct.?.name);

    // Middle → Outer (type_def)
    try std.testing.expect(middle_struct.?.parent_id != null);
    const outer_struct = g.getNode(middle_struct.?.parent_id.?);
    try std.testing.expect(outer_struct != null);
    try std.testing.expectEqual(NodeKind.type_def, outer_struct.?.kind);
    try std.testing.expectEqualStrings("Outer", outer_struct.?.name);

    // Outer → file
    try std.testing.expect(outer_struct.?.parent_id != null);
    const file_node = g.getNode(outer_struct.?.parent_id.?);
    try std.testing.expect(file_node != null);
    try std.testing.expectEqual(NodeKind.file, file_node.?.kind);
}

test "many params function is parsed" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.many_params, &g);

    // Assert — a function node named "manyParams" exists
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and std.mem.eql(u8, n.name, "manyParams")) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "unicode names are preserved" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.unicode_names, &g);

    // Assert — a node named "café" or "résumé" exists (unicode identifiers preserved)
    var found_cafe = false;
    var found_resume = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "café")) found_cafe = true;
        if (std.mem.eql(u8, n.name, "résumé")) found_resume = true;
    }
    try std.testing.expect(found_cafe or found_resume);
}

// =========================================================================
// Generic type-returning function tests — parse tests/fixtures/generic_type.zig
// =========================================================================

test "generic type-returning function produces a type_def node" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g);

    // Assert — a node named "Container" with kind=type_def exists
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Container")) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "generic type-returning function has file as parent" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g);

    // Assert — Container's parent_id points to the file node
    var container_node: ?*const Node = null;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Container")) {
            container_node = n;
            break;
        }
    }
    try std.testing.expect(container_node != null);
    try std.testing.expect(container_node.?.parent_id != null);

    const parent = g.getNode(container_node.?.parent_id.?);
    try std.testing.expect(parent != null);
    try std.testing.expectEqual(NodeKind.file, parent.?.kind);
}

test "generic type-returning function exposes inner public methods" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g);

    // Assert — public methods init, deinit, count, isEmpty exist as function nodes
    var found_init = false;
    var found_deinit = false;
    var found_count = false;
    var found_isEmpty = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and n.visibility == .public) {
            if (std.mem.eql(u8, n.name, "init")) found_init = true;
            if (std.mem.eql(u8, n.name, "deinit")) found_deinit = true;
            if (std.mem.eql(u8, n.name, "count")) found_count = true;
            if (std.mem.eql(u8, n.name, "isEmpty")) found_isEmpty = true;
        }
    }
    try std.testing.expect(found_init);
    try std.testing.expect(found_deinit);
    try std.testing.expect(found_count);
    try std.testing.expect(found_isEmpty);
}

test "generic type-returning function exposes inner private helper" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g);

    // Assert — private function "validate" exists with visibility=private
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and n.visibility == .private and
            std.mem.eql(u8, n.name, "validate"))
        {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "generic type-returning function inner method has correct parent" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g);

    // Assert — method "init" has parent_id pointing to Container (type_def)
    var init_node: ?*const Node = null;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and std.mem.eql(u8, n.name, "init")) {
            init_node = n;
            break;
        }
    }
    try std.testing.expect(init_node != null);
    try std.testing.expect(init_node.?.parent_id != null);

    const parent = g.getNode(init_node.?.parent_id.?);
    try std.testing.expect(parent != null);
    try std.testing.expectEqual(NodeKind.type_def, parent.?.kind);
    try std.testing.expectEqualStrings("Container", parent.?.name);
}

test "generic type-returning function inner type has correct parent" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g);

    // Assert — inner type "Entry" has parent_id pointing to Container (type_def)
    var entry_node: ?*const Node = null;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Entry")) {
            entry_node = n;
            break;
        }
    }
    try std.testing.expect(entry_node != null);
    try std.testing.expect(entry_node.?.parent_id != null);

    const parent = g.getNode(entry_node.?.parent_id.?);
    try std.testing.expect(parent != null);
    try std.testing.expectEqual(NodeKind.type_def, parent.?.kind);
    try std.testing.expectEqualStrings("Container", parent.?.name);
}

test "generic type inner method call edge is detected" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g);

    // Assert — isEmpty calls count (via self.count())
    var caller_id: ?NodeId = null;
    var callee_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "isEmpty")) {
            caller_id = @enumFromInt(idx);
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "count")) {
            callee_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(caller_id != null);
    try std.testing.expect(callee_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == caller_id.? and e.target_id == callee_id.? and e.edge_type == .calls) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "generic type-returning function doc comment on inner method is captured" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g);

    // Assert — public method "count" inside Container has doc != null
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .function and std.mem.eql(u8, n.name, "count")) {
            try std.testing.expect(n.doc != null);
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "generic union type-returning function produces nodes" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g);

    // Assert — a node named "Result" exists (type_def or enum_def for the union)
    //          and a method "isOk" exists inside it
    var found_result = false;
    var found_isOk = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "Result") and
            (n.kind == .type_def or n.kind == .enum_def))
        {
            found_result = true;
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "isOk")) {
            found_isOk = true;
        }
    }
    try std.testing.expect(found_result);
    try std.testing.expect(found_isOk);
}

test "non-generic struct still works alongside generic type" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g);

    // Assert — Config exists as type_def with file as parent, defaults method present
    var config_node: ?*const Node = null;
    var found_defaults = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Config")) {
            config_node = n;
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "defaults")) {
            found_defaults = true;
        }
    }
    try std.testing.expect(config_node != null);
    try std.testing.expect(found_defaults);

    // Config's parent should be the file node
    try std.testing.expect(config_node.?.parent_id != null);
    const parent = g.getNode(config_node.?.parent_id.?);
    try std.testing.expect(parent != null);
    try std.testing.expectEqual(NodeKind.file, parent.?.kind);
}

test "generic type-returning function doc comment on outer function is captured" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g);

    // Assert — Container node has doc != null
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Container")) {
            try std.testing.expect(n.doc != null);
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

// =========================================================================
// Union recognition tests — simple.zig unions
// =========================================================================

test "tagged union is classified as type_def" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — Shape node exists with kind=type_def, visibility=public, doc != null
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "Shape")) {
            try std.testing.expectEqual(NodeKind.type_def, n.kind);
            try std.testing.expectEqual(Visibility.public, n.visibility);
            try std.testing.expect(n.doc != null);
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "plain union is classified as type_def" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — RawValue node exists with kind=type_def, visibility=private
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "RawValue")) {
            try std.testing.expectEqual(NodeKind.type_def, n.kind);
            try std.testing.expectEqual(Visibility.private, n.visibility);
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "tagged union with nested struct is still type_def" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — Shape (which has an anonymous struct in its rect variant)
    //          is classified as type_def because it IS a union, not
    //          accidentally via the nested struct
    var shape_node: ?*const Node = null;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "Shape")) {
            shape_node = n;
            break;
        }
    }
    try std.testing.expect(shape_node != null);
    try std.testing.expectEqual(NodeKind.type_def, shape_node.?.kind);

    // Parent should be the file node, not some intermediate
    try std.testing.expect(shape_node.?.parent_id != null);
    const parent = g.getNode(shape_node.?.parent_id.?);
    try std.testing.expect(parent != null);
    try std.testing.expectEqual(NodeKind.file, parent.?.kind);
}

// =========================================================================
// Enum method extraction tests — simple.zig Color enum
// =========================================================================

test "enum method is extracted as function child" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — Color exists as enum_def, isWarm exists as function with parent == Color
    var color_id: ?NodeId = null;
    var iswarm_node: ?*const Node = null;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .enum_def and std.mem.eql(u8, n.name, "Color")) {
            color_id = n.id;
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "isWarm")) {
            iswarm_node = n;
        }
    }
    try std.testing.expect(color_id != null);
    try std.testing.expect(iswarm_node != null);
    try std.testing.expectEqual(Visibility.public, iswarm_node.?.visibility);
    try std.testing.expect(iswarm_node.?.parent_id != null);
    try std.testing.expectEqual(color_id.?, iswarm_node.?.parent_id.?);
}

test "enum method has uses_type edge to parent enum" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — isWarm has a uses_type edge pointing to Color
    //          (its signature contains `self: Color`)
    var color_id: ?NodeId = null;
    var iswarm_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .enum_def and std.mem.eql(u8, n.name, "Color")) {
            color_id = @enumFromInt(idx);
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "isWarm")) {
            iswarm_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(color_id != null);
    try std.testing.expect(iswarm_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == iswarm_id.? and e.target_id == color_id.? and e.edge_type == .uses_type) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

// =========================================================================
// @import detection tests — simple.zig imports
// =========================================================================

test "import declaration is classified as import_decl" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — node named "std" has kind=import_decl and visibility=private
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "std")) {
            try std.testing.expectEqual(NodeKind.import_decl, n.kind);
            try std.testing.expectEqual(Visibility.private, n.visibility);
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "import with field access is classified as import_decl" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — node named "ZigMeta" (from `@import("zig/meta.zig").ZigMeta`)
    //          has kind=import_decl
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "ZigMeta")) {
            try std.testing.expectEqual(NodeKind.import_decl, n.kind);
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "non-import constant is still constant" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — node named "max_iterations" has kind=constant (regression check)
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "max_iterations")) {
            try std.testing.expectEqual(NodeKind.constant, n.kind);
            try std.testing.expectEqual(Visibility.private, n.visibility);
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

// =========================================================================
// @This() file-struct tests — file_struct.zig
// =========================================================================

test "file-struct methods are extracted as function nodes" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.file_struct, &g);

    // Assert — init, getValue, validate, isValid exist as function nodes
    //          with parent_id pointing to the file node
    var found_init = false;
    var found_getValue = false;
    var found_validate = false;
    var found_isValid = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind != .function) continue;

        // Verify parent is the file node (id 0, kind file)
        if (n.parent_id) |pid| {
            const parent = g.getNode(pid) orelse continue;
            if (parent.kind != .file) continue;
        } else continue;

        if (std.mem.eql(u8, n.name, "init")) found_init = true;
        if (std.mem.eql(u8, n.name, "getValue")) found_getValue = true;
        if (std.mem.eql(u8, n.name, "validate")) found_validate = true;
        if (std.mem.eql(u8, n.name, "isValid")) found_isValid = true;
    }
    try std.testing.expect(found_init);
    try std.testing.expect(found_getValue);
    try std.testing.expect(found_validate);
    try std.testing.expect(found_isValid);
}

test "file-struct nested type is extracted" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.file_struct, &g);

    // Assert — Config struct is detected as type_def with parent = file node
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Config")) {
            try std.testing.expect(n.parent_id != null);
            const parent = g.getNode(n.parent_id.?);
            try std.testing.expect(parent != null);
            try std.testing.expectEqual(NodeKind.file, parent.?.kind);
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "file-struct call edge between methods" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.file_struct, &g);

    // Assert — isValid has a calls edge to validate (via self.validate())
    var isValid_id: ?NodeId = null;
    var validate_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "isValid")) {
            isValid_id = @enumFromInt(idx);
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "validate")) {
            validate_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(isValid_id != null);
    try std.testing.expect(validate_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == isValid_id.? and e.target_id == validate_id.? and e.edge_type == .calls) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "file-struct Self constant is detected" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.file_struct, &g);

    // Assert — const Self = @This() is detected as a constant node
    //          (@This() is not an import, it's a builtin that returns the
    //          enclosing type — semantically a constant binding)
    var found = false;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "Self")) {
            try std.testing.expectEqual(NodeKind.constant, n.kind);
            try std.testing.expectEqual(Visibility.private, n.visibility);
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

// --- Test edge scanning (test blocks should produce calls/uses_type edges) ---

test "test block creates calls edge to local function" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\fn helper() i32 { return 42; }
        \\test "uses helper" { _ = helper(); }
    ;

    // Act
    try parse(source, &g);

    // Assert — the test_def node should have a calls edge to helper
    var test_id: ?NodeId = null;
    var helper_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .test_def) test_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "helper")) helper_id = @enumFromInt(idx);
    }
    try std.testing.expect(test_id != null);
    try std.testing.expect(helper_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == test_id.? and e.target_id == helper_id.? and e.edge_type == .calls) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "test block creates calls edge to method via dot syntax" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const Foo = struct {
        \\    pub fn bar() void {}
        \\};
        \\test "calls bar" { Foo.bar(); }
    ;

    // Act
    try parse(source, &g);

    // Assert — the test_def node should have a calls edge to bar
    var test_id: ?NodeId = null;
    var bar_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .test_def) test_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "bar")) bar_id = @enumFromInt(idx);
    }
    try std.testing.expect(test_id != null);
    try std.testing.expect(bar_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == test_id.? and e.target_id == bar_id.? and e.edge_type == .calls) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "test block with no local calls has no edges" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\test "standalone" {
        \\    const x: i32 = 42;
        \\    _ = x;
        \\}
    ;

    // Act
    try parse(source, &g);

    // Assert — the test_def node has zero outgoing edges
    var test_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .test_def) {
            test_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(test_id != null);

    var outgoing: usize = 0;
    for (g.edges.items) |e| {
        if (e.source_id == test_id.?) outgoing += 1;
    }
    try std.testing.expectEqual(@as(usize, 0), outgoing);
}

// --- Scope boundary: nested functions must not leak edges to parent ---

test "test block does not leak calls from nested function" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\fn helper() void {}
        \\test "outer" {
        \\    const S = struct {
        \\        fn inner() void { helper(); }
        \\    };
        \\    S.inner();
        \\}
    ;

    // Act
    try parse(source, &g);

    // Assert — the test_def "outer" must NOT have a calls edge to "helper".
    // Only the nested function inner() calls helper(); the test body does not.
    var test_id: ?NodeId = null;
    var helper_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .test_def and std.mem.eql(u8, n.name, "outer")) test_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "helper")) helper_id = @enumFromInt(idx);
    }
    try std.testing.expect(test_id != null);
    try std.testing.expect(helper_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == test_id.? and e.target_id == helper_id.? and e.edge_type == .calls) {
            found = true;
            break;
        }
    }
    try std.testing.expect(!found);
}

test "function does not leak calls from nested function" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\fn target() void {}
        \\fn outer() void {
        \\    const S = struct {
        \\        fn nested() void { target(); }
        \\    };
        \\    S.nested();
        \\}
    ;

    // Act
    try parse(source, &g);

    // Assert — function "outer" must NOT have a calls edge to "target".
    // Only the nested function nested() calls target(); outer does not.
    var outer_id: ?NodeId = null;
    var target_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "outer")) outer_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "target")) target_id = @enumFromInt(idx);
    }
    try std.testing.expect(outer_id != null);
    try std.testing.expect(target_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == outer_id.? and e.target_id == target_id.? and e.edge_type == .calls) {
            found = true;
            break;
        }
    }
    try std.testing.expect(!found);
}

test "nested function gets its own calls edge not parent's" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\fn alpha() void {}
        \\fn beta() void {}
        \\fn parent() void {
        \\    _ = alpha();
        \\    const S = struct {
        \\        fn child() void { beta(); }
        \\    };
        \\    S.child();
        \\}
    ;

    // Act
    try parse(source, &g);

    // Assert — "parent" HAS a calls edge to "alpha" (direct call)
    //          "parent" does NOT have a calls edge to "beta" (nested call)
    var parent_id: ?NodeId = null;
    var alpha_id: ?NodeId = null;
    var beta_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "parent")) parent_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "alpha")) alpha_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "beta")) beta_id = @enumFromInt(idx);
    }
    try std.testing.expect(parent_id != null);
    try std.testing.expect(alpha_id != null);
    try std.testing.expect(beta_id != null);

    var calls_alpha = false;
    var calls_beta = false;
    for (g.edges.items) |e| {
        if (e.source_id == parent_id.?) {
            if (e.target_id == alpha_id.? and e.edge_type == .calls) calls_alpha = true;
            if (e.target_id == beta_id.? and e.edge_type == .calls) calls_beta = true;
        }
    }
    try std.testing.expect(calls_alpha);
    try std.testing.expect(!calls_beta);
}

test "function does not leak uses_type from nested function" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const MyType = struct {};
        \\fn outer() void {
        \\    const S = struct {
        \\        fn nested(x: MyType) void { _ = x; }
        \\    };
        \\    _ = S.nested;
        \\}
    ;

    // Act
    try parse(source, &g);

    // Assert — function "outer" must NOT have a uses_type edge to "MyType".
    // The nested function's signature references MyType, but that should not
    // leak to the enclosing function.
    var outer_id: ?NodeId = null;
    var mytype_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "outer")) outer_id = @enumFromInt(idx);
        if (std.mem.eql(u8, n.name, "MyType")) mytype_id = @enumFromInt(idx);
    }
    try std.testing.expect(outer_id != null);
    try std.testing.expect(mytype_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == outer_id.? and e.target_id == mytype_id.? and e.edge_type == .uses_type) {
            found = true;
            break;
        }
    }
    try std.testing.expect(!found);
}

// --- Decl-reference test names (identifier-based test declarations) ---

test "decl-reference test name is captured as identifier" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\fn helper() i32 { return 42; }
        \\test helper { _ = helper(); }
    ;

    // Act
    try parse(source, &g);

    // Assert — the test_def node's name should be "helper", not "test"
    var found_test = false;
    for (g.nodes.items) |n| {
        if (n.kind == .test_def) {
            try std.testing.expectEqualStrings("helper", n.name);
            found_test = true;
            break;
        }
    }
    try std.testing.expect(found_test);
}

test "decl-reference test name for type is captured" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const Foo = struct {
        \\    pub fn bar() void {}
        \\};
        \\test Foo { Foo.bar(); }
    ;

    // Act
    try parse(source, &g);

    // Assert — the test_def node's name should be "Foo", not "test"
    var found_test = false;
    for (g.nodes.items) |n| {
        if (n.kind == .test_def) {
            try std.testing.expectEqualStrings("Foo", n.name);
            found_test = true;
            break;
        }
    }
    try std.testing.expect(found_test);
}

test "two decl-reference tests get distinct names" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\fn alpha() i32 { return 1; }
        \\fn beta() i32 { return 2; }
        \\test alpha { _ = alpha(); }
        \\test beta { _ = beta(); }
    ;

    // Act
    try parse(source, &g);

    // Assert — exactly 2 test_def nodes, named "alpha" and "beta"
    var test_count: usize = 0;
    var found_alpha = false;
    var found_beta = false;
    for (g.nodes.items) |n| {
        if (n.kind == .test_def) {
            test_count += 1;
            if (std.mem.eql(u8, n.name, "alpha")) found_alpha = true;
            if (std.mem.eql(u8, n.name, "beta")) found_beta = true;
            // Neither should be named "test"
            try std.testing.expect(!std.mem.eql(u8, n.name, "test"));
        }
    }
    try std.testing.expectEqual(@as(usize, 2), test_count);
    try std.testing.expect(found_alpha);
    try std.testing.expect(found_beta);
}

test "decl-reference test edges are attributed to correct node" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\fn alpha() i32 { return 1; }
        \\fn beta() i32 { return 2; }
        \\test alpha { _ = alpha(); }
        \\test beta { _ = beta(); }
    ;

    // Act
    try parse(source, &g);

    // Assert — locate node ids
    var test_alpha_id: ?NodeId = null;
    var test_beta_id: ?NodeId = null;
    var fn_alpha_id: ?NodeId = null;
    var fn_beta_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .test_def and std.mem.eql(u8, n.name, "alpha")) test_alpha_id = @enumFromInt(idx);
        if (n.kind == .test_def and std.mem.eql(u8, n.name, "beta")) test_beta_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "alpha")) fn_alpha_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "beta")) fn_beta_id = @enumFromInt(idx);
    }
    try std.testing.expect(test_alpha_id != null);
    try std.testing.expect(test_beta_id != null);
    try std.testing.expect(fn_alpha_id != null);
    try std.testing.expect(fn_beta_id != null);

    // Assert — test "alpha" calls function "alpha"
    var alpha_calls_alpha = false;
    var alpha_calls_beta = false;
    var beta_calls_beta = false;
    var beta_calls_alpha = false;
    for (g.edges.items) |e| {
        if (e.edge_type == .calls) {
            if (e.source_id == test_alpha_id.? and e.target_id == fn_alpha_id.?) alpha_calls_alpha = true;
            if (e.source_id == test_alpha_id.? and e.target_id == fn_beta_id.?) alpha_calls_beta = true;
            if (e.source_id == test_beta_id.? and e.target_id == fn_beta_id.?) beta_calls_beta = true;
            if (e.source_id == test_beta_id.? and e.target_id == fn_alpha_id.?) beta_calls_alpha = true;
        }
    }
    try std.testing.expect(alpha_calls_alpha);
    try std.testing.expect(!alpha_calls_beta);
    try std.testing.expect(beta_calls_beta);
    try std.testing.expect(!beta_calls_alpha);
}

test "quoted identifier test name is stripped" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\test @"edge case name" {}
    ;

    // Act
    try parse(source, &g);

    // Assert — the test_def node's name should be "edge case name" (@ and quotes stripped)
    var found_test = false;
    for (g.nodes.items) |n| {
        if (n.kind == .test_def) {
            try std.testing.expectEqualStrings("edge case name", n.name);
            found_test = true;
            break;
        }
    }
    try std.testing.expect(found_test);
}

test "string-literal test names still work" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\fn foo() void {}
        \\test "calls foo" { _ = foo(); }
    ;

    // Act
    try parse(source, &g);

    // Assert — the test_def node's name should be "calls foo"
    var test_id: ?NodeId = null;
    var foo_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .test_def) {
            try std.testing.expectEqualStrings("calls foo", n.name);
            test_id = @enumFromInt(idx);
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "foo")) foo_id = @enumFromInt(idx);
    }
    try std.testing.expect(test_id != null);
    try std.testing.expect(foo_id != null);

    // Assert — there is a calls edge from the test to foo
    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == test_id.? and e.target_id == foo_id.? and e.edge_type == .calls) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

// --- Struct/enum field extraction (container_field → NodeKind.field) ---

test "struct fields are emitted as field nodes" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — Point struct has two field children (x and y)
    var point_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Point")) {
            point_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(point_id != null);

    var field_count: usize = 0;
    for (g.nodes.items) |n| {
        if (n.kind == .field and n.parent_id != null and n.parent_id.? == point_id.?) {
            field_count += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 2), field_count);
}

test "field node has correct name" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — Point's field nodes are named "x" and "y"
    var point_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Point")) {
            point_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(point_id != null);

    var found_x = false;
    var found_y = false;
    for (g.nodes.items) |n| {
        if (n.kind == .field and n.parent_id != null and n.parent_id.? == point_id.?) {
            if (std.mem.eql(u8, n.name, "x")) found_x = true;
            if (std.mem.eql(u8, n.name, "y")) found_y = true;
        }
    }
    try std.testing.expect(found_x);
    try std.testing.expect(found_y);
}

test "field node visibility is private by default" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — Point's x and y fields have visibility == .private
    //          (Zig struct fields do not support the pub modifier)
    var point_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Point")) {
            point_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(point_id != null);

    var checked: usize = 0;
    for (g.nodes.items) |n| {
        if (n.kind == .field and n.parent_id != null and n.parent_id.? == point_id.?) {
            try std.testing.expectEqual(Visibility.private, n.visibility);
            checked += 1;
        }
    }
    // Guard against vacuous pass — we must have checked at least 2 fields
    try std.testing.expect(checked >= 2);
}

test "enum variants are emitted as field nodes" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — Direction enum has three field children (north, south, east)
    var dir_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .enum_def and std.mem.eql(u8, n.name, "Direction")) {
            dir_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(dir_id != null);

    var found_north = false;
    var found_south = false;
    var found_east = false;
    for (g.nodes.items) |n| {
        if (n.kind == .field and n.parent_id != null and n.parent_id.? == dir_id.?) {
            if (std.mem.eql(u8, n.name, "north")) found_north = true;
            if (std.mem.eql(u8, n.name, "south")) found_south = true;
            if (std.mem.eql(u8, n.name, "east")) found_east = true;
        }
    }
    try std.testing.expect(found_north);
    try std.testing.expect(found_south);
    try std.testing.expect(found_east);
}

// =========================================================================
// Module doc comment tests (//!)
// =========================================================================

test "file node captures module doc comment" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g);

    // Assert — file node (node 0) has doc != null containing module doc text
    const file_node = g.getNode(@enumFromInt(0));
    try std.testing.expect(file_node != null);
    try std.testing.expectEqual(NodeKind.file, file_node.?.kind);
    try std.testing.expect(file_node.?.doc != null);

    const doc = file_node.?.doc.?;
    // Doc should contain the //! content
    try std.testing.expect(std.mem.indexOf(u8, doc, "Simple fixture") != null);
    // Doc should NOT contain /// item doc comments from declarations
    try std.testing.expect(std.mem.indexOf(u8, doc, "Compute the Manhattan") == null);
}

test "file node doc is null when no module doc comment" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source = "const x = 42;\n";

    // Act
    try parse(source, &g);

    // Assert — file node has doc == null
    const file_node = g.getNode(@enumFromInt(0));
    try std.testing.expect(file_node != null);
    try std.testing.expectEqual(NodeKind.file, file_node.?.kind);
    try std.testing.expectEqual(@as(?[]const u8, null), file_node.?.doc);
}

test "module doc is separate from item doc" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\//! Module doc.
        \\/// Item doc.
        \\const x = 42;
    ;

    // Act
    try parse(source, &g);

    // Assert — file node doc contains "Module doc." but NOT "Item doc."
    const file_node = g.getNode(@enumFromInt(0));
    try std.testing.expect(file_node != null);
    try std.testing.expect(file_node.?.doc != null);
    const file_doc = file_node.?.doc.?;
    try std.testing.expect(std.mem.indexOf(u8, file_doc, "Module doc.") != null);
    try std.testing.expect(std.mem.indexOf(u8, file_doc, "Item doc.") == null);

    // Assert — x node doc contains "Item doc." but NOT "Module doc."
    var x_node: ?*const Node = null;
    var i: usize = 0;
    while (i < g.nodeCount()) : (i += 1) {
        const n = g.getNode(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, n.name, "x")) {
            x_node = n;
            break;
        }
    }
    try std.testing.expect(x_node != null);
    try std.testing.expect(x_node.?.doc != null);
    const item_doc = x_node.?.doc.?;
    try std.testing.expect(std.mem.indexOf(u8, item_doc, "Item doc.") != null);
    try std.testing.expect(std.mem.indexOf(u8, item_doc, "Module doc.") == null);
}

// =========================================================================
// Type alias uses_type edge tests
// =========================================================================

test "uses_type edge created for type alias in parameter" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const Foo = struct { val: i32 };
        \\const Bar = Foo;
        \\fn useBar(b: Bar) void { _ = b; }
    ;

    // Act
    try parse(source, &g);

    // Assert — useBar has a uses_type edge to Bar.
    // Rationale: Bar appears in the function signature as a type. Even though
    // Bar is classified as "constant" (it's an alias, not a struct literal),
    // the visitor should recognize that Bar is used as a type and create a
    // uses_type edge to it.
    var useBar_id: ?NodeId = null;
    var bar_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "useBar")) {
            useBar_id = @enumFromInt(idx);
        }
        if (std.mem.eql(u8, n.name, "Bar")) {
            bar_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(useBar_id != null);
    try std.testing.expect(bar_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == useBar_id.? and e.target_id == bar_id.? and e.edge_type == .uses_type) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "uses_type edge created for type alias in return type" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const MyError = error{ Oops, Bad };
        \\const Err = MyError;
        \\fn doStuff() Err!void {}
    ;

    // Act
    try parse(source, &g);

    // Assert — doStuff has a uses_type edge to Err.
    // Rationale: Err appears in the return type position. Even though Err is
    // classified as "constant" (alias to an error set), the visitor should
    // detect it as a type reference in the signature.
    var doStuff_id: ?NodeId = null;
    var err_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "doStuff")) {
            doStuff_id = @enumFromInt(idx);
        }
        if (std.mem.eql(u8, n.name, "Err")) {
            err_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(doStuff_id != null);
    try std.testing.expect(err_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == doStuff_id.? and e.target_id == err_id.? and e.edge_type == .uses_type) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "uses_type edge not created for non-type constant" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const Limit = struct {};
        \\const max = 100;
        \\fn process(l: Limit) void { _ = l; }
    ;

    // Act
    try parse(source, &g);

    // Assert — uses_type edge exists to Limit (type_def) but NOT to max
    //          (numeric constant, not a type)
    var process_id: ?NodeId = null;
    var limit_id: ?NodeId = null;
    var max_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "process")) {
            process_id = @enumFromInt(idx);
        }
        if (std.mem.eql(u8, n.name, "Limit")) {
            limit_id = @enumFromInt(idx);
        }
        if (std.mem.eql(u8, n.name, "max")) {
            max_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(process_id != null);
    try std.testing.expect(limit_id != null);
    try std.testing.expect(max_id != null);

    var found_limit = false;
    var found_max = false;
    for (g.edges.items) |e| {
        if (e.source_id == process_id.? and e.edge_type == .uses_type) {
            if (e.target_id == limit_id.?) found_limit = true;
            if (e.target_id == max_id.?) found_max = true;
        }
    }
    try std.testing.expect(found_limit);
    try std.testing.expect(!found_max);
}

test "uses_type edge for aliased type works alongside direct type" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const Direct = struct {};
        \\const Other = struct {};
        \\const Alias = Other;
        \\fn both(d: Direct, a: Alias) void { _ = d; _ = a; }
    ;

    // Act
    try parse(source, &g);

    // Assert — both has uses_type edges to Direct (via type_def match) and to
    // Alias (via alias match). Both types appear in the function signature.
    var both_id: ?NodeId = null;
    var direct_id: ?NodeId = null;
    var alias_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "both")) {
            both_id = @enumFromInt(idx);
        }
        if (std.mem.eql(u8, n.name, "Direct")) {
            direct_id = @enumFromInt(idx);
        }
        if (std.mem.eql(u8, n.name, "Alias")) {
            alias_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(both_id != null);
    try std.testing.expect(direct_id != null);
    try std.testing.expect(alias_id != null);

    var found_direct = false;
    var found_alias = false;
    for (g.edges.items) |e| {
        if (e.source_id == both_id.? and e.edge_type == .uses_type) {
            if (e.target_id == direct_id.?) found_direct = true;
            if (e.target_id == alias_id.?) found_alias = true;
        }
    }
    try std.testing.expect(found_direct);
    try std.testing.expect(found_alias);
}

// =========================================================================
// Body type reference tests — struct literals, static method calls, comptime args
// =========================================================================

test "uses_type edge for struct literal construction in body" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const Config = struct { max: i32 = 0 };
        \\fn makeDefault() void {
        \\    const c = Config{ .max = 42 };
        \\    _ = c;
        \\}
    ;

    // Act
    try parse(source, &g);

    // Assert — makeDefault has a uses_type edge to Config.
    // Config appears only in the function body (as a struct literal), not in
    // the signature (return type is void). The visitor must detect type
    // references inside function bodies, not just signatures.
    var makeDefault_id: ?NodeId = null;
    var config_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "makeDefault")) {
            makeDefault_id = @enumFromInt(idx);
        }
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Config")) {
            config_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(makeDefault_id != null);
    try std.testing.expect(config_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == makeDefault_id.? and e.target_id == config_id.? and e.edge_type == .uses_type) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "uses_type edge for static method call in body" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const Builder = struct {
        \\    val: i32,
        \\    pub fn init(v: i32) Builder {
        \\        return Builder{ .val = v };
        \\    }
        \\};
        \\fn create() void {
        \\    const b = Builder.init(5);
        \\    _ = b;
        \\}
    ;

    // Act
    try parse(source, &g);

    // Assert — create has a uses_type edge to Builder.
    // The function body references Builder via `Builder.init(5)`. This is a
    // field_expression where the object is the type name. The visitor must
    // detect this as a type reference even though it also creates a calls edge
    // to init.
    var create_id: ?NodeId = null;
    var builder_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "create")) {
            create_id = @enumFromInt(idx);
        }
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Builder")) {
            builder_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(create_id != null);
    try std.testing.expect(builder_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == create_id.? and e.target_id == builder_id.? and e.edge_type == .uses_type) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "no duplicate uses_type edge when type in both signature and body" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const Item = struct { id: i32 = 0 };
        \\fn process(item: Item) Item {
        \\    return Item{ .id = item.id + 1 };
        \\}
    ;

    // Act
    try parse(source, &g);

    // Assert — process has exactly one uses_type edge to Item.
    // Item appears both in the signature (parameter type and return type) and
    // in the body (struct literal). addEdgeIfNew must deduplicate so that only
    // one uses_type edge exists.
    var process_id: ?NodeId = null;
    var item_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "process")) {
            process_id = @enumFromInt(idx);
        }
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Item")) {
            item_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(process_id != null);
    try std.testing.expect(item_id != null);

    var count: usize = 0;
    for (g.edges.items) |e| {
        if (e.source_id == process_id.? and e.target_id == item_id.? and e.edge_type == .uses_type) {
            count += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 1), count);
}

test "uses_type edge for type passed as comptime argument" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const Payload = struct { data: i32 = 0 };
        \\fn serialize(comptime T: type, val: T) void { _ = val; }
        \\fn doWork() void {
        \\    const p = Payload{ .data = 1 };
        \\    serialize(Payload, p);
        \\}
    ;

    // Act
    try parse(source, &g);

    // Assert — doWork has a uses_type edge to Payload.
    // Payload is passed as a comptime type parameter in `serialize(Payload, p)`.
    // Even though it's a call argument (not a type annotation in the signature),
    // the identifier refers to a locally-defined struct and the visitor must
    // detect it as a type reference.
    var doWork_id: ?NodeId = null;
    var payload_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "doWork")) {
            doWork_id = @enumFromInt(idx);
        }
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Payload")) {
            payload_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(doWork_id != null);
    try std.testing.expect(payload_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == doWork_id.? and e.target_id == payload_id.? and e.edge_type == .uses_type) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "uses_type edge for type in generic container instantiation" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const Element = struct { val: i32 = 0 };
        \\fn Container(comptime T: type) type {
        \\    return struct { item: T };
        \\}
        \\fn build() void {
        \\    _ = Container(Element);
        \\}
    ;

    // Act
    try parse(source, &g);

    // Assert — build has a uses_type edge to Element.
    // Element is passed as a comptime argument in `Container(Element)`. The
    // visitor must detect identifiers in call arguments that refer to
    // locally-defined types, not just identifiers in type annotation positions.
    var build_id: ?NodeId = null;
    var element_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "build")) {
            build_id = @enumFromInt(idx);
        }
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Element")) {
            element_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(build_id != null);
    try std.testing.expect(element_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == build_id.? and e.target_id == element_id.? and e.edge_type == .uses_type) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "no uses_type edge for non-type identifier argument" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();
    const source =
        \\const Config = struct {};
        \\const max = 100;
        \\fn helper(n: i32) void { _ = n; }
        \\fn run() void {
        \\    helper(max);
        \\}
    ;

    // Act
    try parse(source, &g);

    // Assert — run has NO uses_type edge to max. max is a numeric constant,
    // not a type. The body scanner must not create uses_type edges for every
    // identifier argument — only for identifiers that refer to type_def or
    // enum_def nodes.
    // Also assert — run DOES have a calls edge to helper. This validates that
    // call edges still work correctly even when identifier arguments are present.
    var run_id: ?NodeId = null;
    var helper_id: ?NodeId = null;
    var max_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "run")) {
            run_id = @enumFromInt(idx);
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "helper")) {
            helper_id = @enumFromInt(idx);
        }
        if (n.kind == .constant and std.mem.eql(u8, n.name, "max")) {
            max_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(run_id != null);
    try std.testing.expect(helper_id != null);
    try std.testing.expect(max_id != null);

    // No uses_type edge from run to max (max is not a type)
    var found_max_edge = false;
    for (g.edges.items) |e| {
        if (e.source_id == run_id.? and e.target_id == max_id.? and e.edge_type == .uses_type) {
            found_max_edge = true;
            break;
        }
    }
    try std.testing.expect(!found_max_edge);

    // calls edge from run to helper exists
    var found_calls = false;
    for (g.edges.items) |e| {
        if (e.source_id == run_id.? and e.target_id == helper_id.? and e.edge_type == .calls) {
            found_calls = true;
            break;
        }
    }
    try std.testing.expect(found_calls);
}
