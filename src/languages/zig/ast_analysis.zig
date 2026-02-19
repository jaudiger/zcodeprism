const std = @import("std");
const types = @import("../../core/types.zig");
const ts = @import("tree-sitter");
const ts_api = @import("../../parser/tree_sitter_api.zig");
const pc = @import("parse_context.zig");

const NodeKind = types.NodeKind;
const Visibility = types.Visibility;
const KindIds = pc.KindIds;

// =========================================================================
// Return type analysis
// =========================================================================

/// Check if a function_declaration has `type` as its return type.
pub fn returnsType(source: []const u8, fn_node: ts.Node, k: *const KindIds) bool {
    // The return type appears as a `builtin_type` child with text "type",
    // positioned before the block. We check non-named children too since
    // `builtin_type` is a named child.
    var i: u32 = 0;
    while (i < fn_node.childCount()) : (i += 1) {
        const child = fn_node.child(i) orelse continue;
        const kid = child.kindId();
        if (kid == k.block) break;
        if (kid == k.builtin_type) {
            const text = ts_api.nodeText(source, child);
            if (std.mem.eql(u8, text, "type")) return true;
        }
    }
    return false;
}

pub const ReturnedTypeBody = struct {
    body: ts.Node,
    is_enum: bool,
};

/// Search a function body for `return struct { ... }`, `return union { ... }`, or `return enum { ... }`.
/// Returns the struct/union/enum body node if found.
pub fn findReturnedTypeBody(fn_node: ts.Node, k: *const KindIds) ?ReturnedTypeBody {
    // Path: function_declaration -> block -> expression_statement -> return_expression -> struct/union/enum_declaration
    var i: u32 = 0;
    while (i < fn_node.childCount()) : (i += 1) {
        const child = fn_node.child(i) orelse continue;
        if (child.kindId() != k.block) continue;

        // Search block children for expression_statement containing a return.
        var j: u32 = 0;
        while (j < child.childCount()) : (j += 1) {
            const stmt = child.child(j) orelse continue;
            if (stmt.kindId() != k.expression_statement) continue;

            // Look for return_expression inside the expression_statement.
            var ki: u32 = 0;
            while (ki < stmt.childCount()) : (ki += 1) {
                const ret = stmt.child(ki) orelse continue;
                if (ret.kindId() != k.return_expression) continue;

                // The returned value is a named child of return_expression (after "return").
                var l: u32 = 0;
                while (l < ret.namedChildCount()) : (l += 1) {
                    const val = ret.namedChild(l) orelse continue;
                    const val_kid = val.kindId();
                    if (val_kid == k.struct_declaration) {
                        return .{ .body = val, .is_enum = false };
                    }
                    if (val_kid == k.union_declaration) {
                        return .{ .body = val, .is_enum = false };
                    }
                    if (val_kid == k.enum_declaration) {
                        return .{ .body = val, .is_enum = true };
                    }
                }
            }
        }
    }
    return null;
}

// =========================================================================
// Identifier and name extraction
// =========================================================================

pub fn getIdentifierName(source: []const u8, ts_node: ts.Node, k: *const KindIds) ?[]const u8 {
    var i: u32 = 0;
    while (i < ts_node.namedChildCount()) : (i += 1) {
        const child = ts_node.namedChild(i) orelse continue;
        if (child.kindId() == k.identifier) {
            const raw = ts_api.nodeText(source, child);
            return stripQuotedIdentifier(raw);
        }
    }
    return null;
}

pub fn stripQuotedIdentifier(raw: []const u8) []const u8 {
    // Handle @"name" syntax for unicode or keyword identifiers.
    if (raw.len >= 3 and raw[0] == '@' and raw[1] == '"' and raw[raw.len - 1] == '"') {
        return raw[2 .. raw.len - 1];
    }
    return raw;
}

pub fn getTestName(source: []const u8, ts_node: ts.Node, k: *const KindIds) []const u8 {
    var i: u32 = 0;
    while (i < ts_node.namedChildCount()) : (i += 1) {
        const child = ts_node.namedChild(i) orelse continue;
        const kid = child.kindId();
        if (kid == k.string) {
            // String-literal test name: test "name" { }
            // Look for string_content inside the string node.
            var j: u32 = 0;
            while (j < child.namedChildCount()) : (j += 1) {
                const sc = child.namedChild(j) orelse continue;
                if (sc.kindId() == k.string_content) {
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
        if (kid == k.identifier) {
            // Decl-reference test name: test Value { } or test @"edge case" { }
            const text = ts_api.nodeText(source, child);
            return stripQuotedIdentifier(text);
        }
    }
    return "test";
}

// =========================================================================
// Visibility and doc comments
// =========================================================================

pub fn detectVisibility(ts_node: ts.Node, k: *const KindIds) Visibility {
    // Check all children (including anonymous) for "pub" keyword.
    var i: u32 = 0;
    while (i < ts_node.childCount()) : (i += 1) {
        const child = ts_node.child(i) orelse continue;
        if (child.kindId() == k.pub_kw) return .public;
    }
    return .private;
}

/// Collect module-level doc comments (//!) from the beginning of a source file.
/// Scans direct children of the root node for consecutive comment nodes whose
/// text starts with "//!". Stops at the first non-//! child.
pub fn collectModuleDocComment(source: []const u8, root: ts.Node, k: *const KindIds) ?[]const u8 {
    var first_start: ?u32 = null;
    var last_end: ?u32 = null;

    var i: u32 = 0;
    while (i < root.childCount()) : (i += 1) {
        const child = root.child(i) orelse continue;
        if (child.kindId() != k.comment) {
            // Skip anonymous non-comment tokens (e.g. punctuation).
            if (!child.isNamed()) continue;
            // Hit a declaration, stop.
            break;
        }
        const text = source[child.startByte()..child.endByte()];
        if (text.len >= 3 and text[0] == '/' and text[1] == '/' and text[2] == '!') {
            if (first_start == null) first_start = child.startByte();
            last_end = child.endByte();
        } else {
            // Non-//! comment (e.g. /// or //), stop collecting.
            break;
        }
    }

    if (first_start) |start| {
        return source[start..last_end.?];
    }
    return null;
}

pub fn collectDocComment(source: []const u8, ts_node: ts.Node, k: *const KindIds) ?[]const u8 {
    // Walk previous siblings to collect consecutive doc comment lines.
    var first_doc_start: ?u32 = null;
    var last_doc_end: ?u32 = null;
    var current = ts_node.prevSibling();
    while (current) |sib| {
        if (sib.kindId() == k.comment) {
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

// =========================================================================
// @This() and field_expression detection
// =========================================================================

/// Check if a variable_declaration's value contains `@This()`.
/// Detects all aliases regardless of name (not just "Self").
pub fn isThisBuiltin(source: []const u8, var_decl: ts.Node, k: *const KindIds) bool {
    return isThisRecursive(source, var_decl, k, 0);
}

fn isThisRecursive(source: []const u8, node: ts.Node, k: *const KindIds, depth: u32) bool {
    if (depth > 5) return false;
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        if (child.kindId() == k.builtin_identifier) {
            const text = ts_api.nodeText(source, child);
            if (std.mem.eql(u8, text, "@This")) return true;
        }
        if (isThisRecursive(source, child, k, depth + 1)) return true;
    }
    return false;
}

pub const FieldExprParts = struct {
    root: []const u8,
    leaf: []const u8,
};

/// For a variable_declaration whose value is a field_expression, extract
/// the root (leftmost identifier) and leaf (rightmost field_identifier).
/// Returns null if the value is not a field_expression.
/// Handles nested chains like `std.mem.Allocator` (root="std", leaf="Allocator").
pub fn getFieldExprRootAndLeaf(source: []const u8, var_decl: ts.Node, k: *const KindIds) ?FieldExprParts {
    // Find the value expression in the variable_declaration.
    // Scan named children for a field_expression (skipping identifier, type, etc.)
    var i: u32 = 0;
    while (i < var_decl.namedChildCount()) : (i += 1) {
        const child = var_decl.namedChild(i) orelse continue;
        if (child.kindId() == k.field_expression) {
            const root = getLeftmostIdent(source, child, k) orelse return null;
            const leaf = getRightmostField(source, child, k) orelse return null;
            return .{ .root = root, .leaf = leaf };
        }
    }
    return null;
}

fn getLeftmostIdent(source: []const u8, node: ts.Node, k: *const KindIds) ?[]const u8 {
    const kid = node.kindId();
    if (kid == k.identifier) {
        return ts_api.nodeText(source, node);
    }
    if (kid == k.field_expression) {
        if (node.namedChild(0)) |child| {
            return getLeftmostIdent(source, child, k);
        }
    }
    return null;
}

fn getRightmostField(source: []const u8, node: ts.Node, k: *const KindIds) ?[]const u8 {
    if (node.kindId() != k.field_expression) return null;
    const count = node.namedChildCount();
    if (count < 2) return null;
    const last = node.namedChild(count - 1) orelse return null;
    const last_kid = last.kindId();
    // tree-sitter Zig uses field_identifier for the .field part
    if (last_kid == k.field_identifier or last_kid == k.property_identifier or last_kid == k.identifier) {
        return ts_api.nodeText(source, last);
    }
    return null;
}

// =========================================================================
// Type annotation and classification
// =========================================================================

pub fn hasTypeAnnotation(ts_node: ts.Node, k: *const KindIds) bool {
    // Check for ":" anonymous child (indicates explicit type annotation).
    var i: u32 = 0;
    while (i < ts_node.childCount()) : (i += 1) {
        const child = ts_node.child(i) orelse continue;
        if (child.kindId() == k.colon) return true;
    }
    return false;
}

pub const Classification = struct {
    kind: NodeKind,
    struct_body: ?ts.Node,
};

pub fn classifyVariableValue(source: []const u8, ts_node: ts.Node, k: *const KindIds) Classification {
    var result = Classification{ .kind = .constant, .struct_body = null };
    classifyRecursive(source, ts_node, k, &result, 0);
    return result;
}

pub fn classifyRecursive(source: []const u8, ts_node: ts.Node, k: *const KindIds, result: *Classification, depth: u32) void {
    if (depth > 5) return;
    var i: u32 = 0;
    while (i < ts_node.namedChildCount()) : (i += 1) {
        const child = ts_node.namedChild(i) orelse continue;
        const kid = child.kindId();
        if (kid == k.struct_declaration) {
            result.kind = .type_def;
            result.struct_body = child;
            return;
        }
        if (kid == k.union_declaration) {
            result.kind = .type_def;
            result.struct_body = child;
            return;
        }
        if (kid == k.enum_declaration) {
            result.kind = .enum_def;
            result.struct_body = child;
            return;
        }
        if (kid == k.error_set_declaration) {
            result.kind = .error_def;
            return;
        }
        if (kid == k.builtin_identifier) {
            const text = ts_api.nodeText(source, child);
            if (std.mem.eql(u8, text, "@import")) {
                result.kind = .import_decl;
                return;
            }
        }
        classifyRecursive(source, child, k, result, depth + 1);
        if (result.kind != .constant) return;
    }
}
