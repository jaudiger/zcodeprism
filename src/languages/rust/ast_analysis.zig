const std = @import("std");
const ts = @import("tree-sitter");
const ts_api = @import("../../parser/tree_sitter_api.zig");
const pc = @import("parse_context.zig");
const types = @import("../../core/types.zig");

const KindIds = pc.KindIds;
const Visibility = types.Visibility;

/// Extract the `identifier` child text from a declaration node.
pub fn getIdentifierName(source: []const u8, node: ts.Node, k: *const KindIds) ?[]const u8 {
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        if (child.kindId() == k.identifier) {
            return ts_api.nodeText(source, child);
        }
    }
    return null;
}

/// Extract the `type_identifier` child text from a declaration node.
pub fn getTypeIdentifierName(source: []const u8, node: ts.Node, k: *const KindIds) ?[]const u8 {
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        if (child.kindId() == k.type_identifier) {
            return ts_api.nodeText(source, child);
        }
    }
    return null;
}

/// Visibility plus optional restriction scope for pub(crate/super/in ...) forms.
pub const VisibilityInfo = struct {
    visibility: Visibility,
    scope: ?[]const u8,
};

/// Detect visibility by checking for a visibility_modifier child.
/// Returns .public for bare pub, .private with scope for restricted forms,
/// .private with null scope when no modifier is present.
pub fn detectVisibility(source: []const u8, node: ts.Node, k: *const KindIds) VisibilityInfo {
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        if (child.kindId() == k.visibility_modifier) {
            // Bare `pub` has exactly 1 child (the pub keyword).
            // Restricted forms have additional children: (, scope, ).
            if (child.childCount() <= 1) return .{ .visibility = .public, .scope = null };
            const scope = extractVisibilityScope(source, child);
            return .{ .visibility = .private, .scope = scope };
        }
    }
    return .{ .visibility = .private, .scope = null };
}

/// Extract the text between parens from a restricted visibility_modifier.
fn extractVisibilityScope(source: []const u8, vis_node: ts.Node) ?[]const u8 {
    const text = ts_api.nodeText(source, vis_node);
    const open = std.mem.indexOfScalar(u8, text, '(') orelse return null;
    const close = std.mem.lastIndexOfScalar(u8, text, ')') orelse return null;
    if (close <= open + 1) return null;
    return text[open + 1 .. close];
}

/// Check if any child has the given kind ID.
pub fn hasChildKind(node: ts.Node, kind_id: u16) bool {
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        if (child.kindId() == kind_id) return true;
    }
    return false;
}

/// Check if a function_item has a `function_modifiers` child containing a specific modifier.
pub fn hasFunctionModifier(node: ts.Node, modifier_kind: u16, k: *const KindIds) bool {
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        if (child.kindId() == k.function_modifiers) {
            var j: u32 = 0;
            while (j < child.childCount()) : (j += 1) {
                const mod_child = child.child(j) orelse continue;
                if (mod_child.kindId() == modifier_kind) return true;
                // Check inside extern_modifier for the extern keyword
                if (mod_child.kindId() == k.extern_modifier) {
                    var m: u32 = 0;
                    while (m < mod_child.childCount()) : (m += 1) {
                        const inner = mod_child.child(m) orelse continue;
                        if (inner.kindId() == modifier_kind) return true;
                    }
                }
            }
        }
    }
    return false;
}

/// Check if a function_item has an extern modifier (possibly with ABI).
pub fn hasExternModifier(node: ts.Node, k: *const KindIds) bool {
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        if (child.kindId() == k.function_modifiers) {
            var j: u32 = 0;
            while (j < child.childCount()) : (j += 1) {
                const mod_child = child.child(j) orelse continue;
                if (mod_child.kindId() == k.extern_modifier) return true;
            }
        }
    }
    return false;
}

/// Extract the ABI string from an extern modifier.
pub fn extractExternAbi(source: []const u8, node: ts.Node, k: *const KindIds) ?[]const u8 {
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        if (child.kindId() == k.function_modifiers) {
            var j: u32 = 0;
            while (j < child.childCount()) : (j += 1) {
                const mod_child = child.child(j) orelse continue;
                if (mod_child.kindId() == k.extern_modifier) {
                    // Look for string_literal > string_content inside extern_modifier
                    var m: u32 = 0;
                    while (m < mod_child.childCount()) : (m += 1) {
                        const inner = mod_child.child(m) orelse continue;
                        if (inner.kindId() == k.string_literal) {
                            return extractStringContent(source, inner, k);
                        }
                    }
                }
            }
        }
    }
    return null;
}

/// Extract the string_content from a string_literal node.
fn extractStringContent(source: []const u8, str_node: ts.Node, k: *const KindIds) ?[]const u8 {
    var i: u32 = 0;
    while (i < str_node.childCount()) : (i += 1) {
        const child = str_node.child(i) orelse continue;
        if (child.kindId() == k.string_content) {
            return ts_api.nodeText(source, child);
        }
    }
    return null;
}

/// Collect outer doc comments (///) before a declaration node.
/// Returns the raw text span covering all consecutive /// comments
/// immediately preceding the node (or its attribute).
pub fn collectOuterDocComment(source: []const u8, node: ts.Node, k: *const KindIds) ?[]const u8 {
    // Walk backward from node to find consecutive doc comment lines.
    // First, skip any attribute_item siblings to find the first doc comment.
    var target = node;

    // If the previous sibling is an attribute_item, we want to look for doc
    // comments before the attribute.
    while (true) {
        const prev = target.prevSibling() orelse break;
        if (prev.kindId() == k.attribute_item) {
            target = prev;
        } else {
            break;
        }
    }

    // Now collect consecutive /// comment siblings before `target`.
    var first_doc_start: ?u32 = null;
    var last_doc_end: ?u32 = null;
    var current = target;

    while (true) {
        const prev = current.prevSibling() orelse break;
        if (prev.kindId() != k.line_comment) break;

        // Check if this is a /// (outer doc) comment
        if (!isOuterDocComment(prev, k)) break;

        first_doc_start = prev.startByte();
        if (last_doc_end == null) {
            last_doc_end = prev.endByte();
        }
        current = prev;
    }

    if (first_doc_start) |start| {
        if (last_doc_end) |end| {
            return source[start..end];
        }
    }
    return null;
}

/// Collect inner doc comments (//!) from the start of the file.
pub fn collectInnerDocComment(source: []const u8, root: ts.Node, k: *const KindIds) ?[]const u8 {
    var first_start: ?u32 = null;
    var last_end: ?u32 = null;

    var i: u32 = 0;
    while (i < root.childCount()) : (i += 1) {
        const child = root.child(i) orelse continue;
        if (child.kindId() != k.line_comment) break;
        if (!isInnerDocComment(child, k)) break;

        if (first_start == null) {
            first_start = child.startByte();
        }
        last_end = child.endByte();
    }

    if (first_start) |start| {
        if (last_end) |end| {
            return source[start..end];
        }
    }
    return null;
}

/// Check if a line_comment node is an outer doc comment (///).
fn isOuterDocComment(comment: ts.Node, k: *const KindIds) bool {
    var i: u32 = 0;
    while (i < comment.childCount()) : (i += 1) {
        const child = comment.child(i) orelse continue;
        if (child.kindId() == k.outer_doc_comment_marker) return true;
    }
    return false;
}

/// Check if a line_comment node is an inner doc comment (//!).
fn isInnerDocComment(comment: ts.Node, k: *const KindIds) bool {
    var i: u32 = 0;
    while (i < comment.childCount()) : (i += 1) {
        const child = comment.child(i) orelse continue;
        if (child.kindId() == k.inner_doc_comment_marker) return true;
    }
    return false;
}

/// Check if a declaration has a specific attribute, regardless of doc comment ordering.
pub fn hasAttribute(source: []const u8, node: ts.Node, k: *const KindIds, name: []const u8) bool {
    var current = node;
    while (true) {
        const prev = current.prevSibling() orelse return false;
        if (prev.kindId() == k.line_comment) {
            current = prev;
            continue;
        }
        if (prev.kindId() == k.attribute_item) {
            if (attributeHasName(source, prev, k, name)) return true;
            current = prev;
        } else {
            return false;
        }
    }
}

/// Check if an attribute_item has a specific attribute name.
fn attributeHasName(source: []const u8, attr_node: ts.Node, k: *const KindIds, name: []const u8) bool {
    var i: u32 = 0;
    while (i < attr_node.childCount()) : (i += 1) {
        const child = attr_node.child(i) orelse continue;
        if (child.kindId() == k.attribute) {
            var j: u32 = 0;
            while (j < child.childCount()) : (j += 1) {
                const attr_child = child.child(j) orelse continue;
                if (attr_child.kindId() == k.identifier) {
                    return std.mem.eql(u8, ts_api.nodeText(source, attr_child), name);
                }
            }
        }
    }
    return false;
}

/// Extract derive list from preceding #[derive(...)] attribute(s),
/// regardless of doc comment ordering.
pub fn extractDerives(source: []const u8, node: ts.Node, k: *const KindIds) ?[]const u8 {
    var current = node;
    while (true) {
        const prev = current.prevSibling() orelse break;
        if (prev.kindId() == k.line_comment) {
            current = prev;
            continue;
        }
        if (prev.kindId() != k.attribute_item) break;

        if (attributeHasName(source, prev, k, "derive")) {
            return extractDeriveContent(source, prev, k);
        }
        current = prev;
    }
    return null;
}

/// Extract the content of a derive attribute's token_tree.
/// Returns the text inside the parentheses with whitespace trimmed.
fn extractDeriveContent(source: []const u8, attr_node: ts.Node, k: *const KindIds) ?[]const u8 {
    var i: u32 = 0;
    while (i < attr_node.childCount()) : (i += 1) {
        const child = attr_node.child(i) orelse continue;
        if (child.kindId() == k.attribute) {
            var j: u32 = 0;
            while (j < child.childCount()) : (j += 1) {
                const attr_child = child.child(j) orelse continue;
                if (attr_child.kindId() == k.token_tree) {
                    // Get the raw text between parens.
                    const text = ts_api.nodeText(source, attr_child);
                    if (text.len >= 2) {
                        // Strip parentheses
                        const inner = std.mem.trim(u8, text[1 .. text.len - 1], " ");
                        if (inner.len > 0) return inner;
                    }
                }
            }
        }
    }
    return null;
}

/// Collect outer attributes besides derive and test, regardless of doc
/// comment ordering. Returns an allocated string joining the kept
/// attribute texts with newlines, or null when no kept attributes exist.
/// Caller owns the returned memory.
pub fn extractAttributes(allocator: std.mem.Allocator, source: []const u8, node: ts.Node, k: *const KindIds) !?[]const u8 {
    const skip_names = [_][]const u8{ "derive", "test" };

    // Walk backward and record byte ranges of kept attributes.
    // Stored in reverse source order (closest-to-declaration first).
    var ranges: [16][2]u32 = undefined;
    var count: usize = 0;
    var current = node;

    while (true) {
        const prev = current.prevSibling() orelse break;
        if (prev.kindId() == k.line_comment) {
            current = prev;
            continue;
        }
        if (prev.kindId() != k.attribute_item) break;

        var skip = false;
        for (skip_names) |name| {
            if (attributeHasName(source, prev, k, name)) {
                skip = true;
                break;
            }
        }
        if (!skip and count < ranges.len) {
            ranges[count] = .{ prev.startByte(), prev.endByte() };
            count += 1;
        }
        current = prev;
    }

    if (count == 0) return null;

    // MAF: measure total size, allocate once, fill.
    var total: usize = 0;
    for (0..count) |i| {
        if (i > 0) total += 1;
        const idx = count - 1 - i;
        total += ranges[idx][1] - ranges[idx][0];
    }

    const buf = try allocator.alloc(u8, total);
    var pos: usize = 0;
    for (0..count) |i| {
        if (i > 0) {
            buf[pos] = '\n';
            pos += 1;
        }
        const idx = count - 1 - i;
        const text = source[ranges[idx][0]..ranges[idx][1]];
        @memcpy(buf[pos..][0..text.len], text);
        pos += text.len;
    }
    std.debug.assert(pos == total);
    return buf;
}

/// Extract function signature text (everything before the block body).
pub fn extractFunctionSignature(source: []const u8, node: ts.Node, k: *const KindIds) ?[]const u8 {
    const start = node.startByte();

    // Find the block child to determine where the signature ends.
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        if (child.kindId() == k.block) {
            const sig_end = child.startByte();
            if (sig_end > start) {
                const sig = std.mem.trimRight(u8, source[start..sig_end], " \t\n\r");
                if (sig.len > 0) return sig;
            }
            return null;
        }
    }

    // No block found (trait method signature without body): use full text minus trailing semicolon.
    const end = node.endByte();
    if (end > start) {
        const text = std.mem.trimRight(u8, source[start..end], " \t\n\r;");
        if (text.len > 0) return text;
    }
    return null;
}

/// Extract the declaration header text for structs, enums, unions, traits,
/// impl blocks, and type aliases. Returns everything from the node start
/// to the opening body delimiter, preserving generic parameters, lifetime
/// parameters, trait bounds, and where clauses.
pub fn extractDeclarationSignature(source: []const u8, node: ts.Node, k: *const KindIds) ?[]const u8 {
    const start = node.startByte();

    // Find the body child to determine where the header ends.
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        const kid = child.kindId();
        if (kid == k.field_declaration_list or kid == k.enum_variant_list or
            kid == k.declaration_list or kid == k.ordered_field_declaration_list)
        {
            const sig_end = child.startByte();
            if (sig_end > start) {
                const sig = std.mem.trimRight(u8, source[start..sig_end], " \t\n\r");
                if (sig.len > 0) return sig;
            }
            return null;
        }
    }

    // No body (unit struct, type alias, external mod declaration).
    const end = node.endByte();
    if (end > start) {
        const text = std.mem.trimRight(u8, source[start..end], " \t\n\r;");
        if (text.len > 0) return text;
    }
    return null;
}

/// Get impl target information: the type being implemented and optional trait.
pub const ImplInfo = struct {
    type_name: []const u8,
    trait_name: ?[]const u8 = null,
    has_for: bool = false,
};

pub fn getImplInfo(source: []const u8, node: ts.Node, k: *const KindIds) ?ImplInfo {
    var first_type: ?[]const u8 = null;
    var second_type: ?[]const u8 = null;
    var has_for = false;

    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        const kid = child.kindId();

        if (kid == k.kw_for) {
            has_for = true;
        } else if (extractImplTypeName(source, child, k)) |name| {
            if (first_type == null) {
                first_type = name;
            } else if (second_type == null) {
                second_type = name;
            }
        }
    }

    if (has_for) {
        // impl Trait for Type
        if (first_type) |trait_name| {
            if (second_type) |type_name| {
                return ImplInfo{
                    .type_name = type_name,
                    .trait_name = trait_name,
                    .has_for = true,
                };
            }
        }
        // Target type unresolvable (exotic type after `for`). Return null
        // rather than falling through to the `impl Type` path, which would
        // silently use the trait name as the type name.
        return null;
    }

    // impl Type (inherent impl, no trait)
    if (first_type) |type_name| {
        return ImplInfo{ .type_name = type_name };
    }
    return null;
}

/// Extract the base type name from any type expression in an impl_item.
/// Recurses through type wrappers, skipping containers that hold
/// parameter names rather than the base type.
fn extractImplTypeName(source: []const u8, node: ts.Node, k: *const KindIds) ?[]const u8 {
    const kid = node.kindId();

    // Terminals
    if (kid == k.type_identifier) return ts_api.nodeText(source, node);
    if (kid == k.scoped_type_identifier) return ts_api.nodeText(source, node);
    if (kid == k.primitive_type) return ts_api.nodeText(source, node);

    // Containers that hold parameter names, not the base type.
    if (kid == k.type_arguments or kid == k.type_parameters or
        kid == k.declaration_list or kid == k.where_clause) return null;

    // Recurse through any type wrapper to find the base type_identifier.
    var i: u32 = 0;
    while (i < node.childCount()) : (i += 1) {
        const child = node.child(i) orelse continue;
        if (extractImplTypeName(source, child, k)) |name| return name;
    }
    return null;
}

/// Collect inner attributes (#![...]) from the children of a container node
/// (source_file root or declaration_list inside an inline module).
/// Returns the combined source text span of all consecutive inner_attribute_item
/// nodes, or null if none are found.
pub fn collectInnerAttributes(source: []const u8, container: ts.Node, k: *const KindIds) ?[]const u8 {
    var first_start: ?u32 = null;
    var last_end: u32 = 0;

    var i: u32 = 0;
    while (i < container.childCount()) : (i += 1) {
        const child = container.child(i) orelse continue;
        if (child.kindId() == k.inner_attribute_item) {
            if (first_start == null) first_start = child.startByte();
            last_end = child.endByte();
        }
    }

    if (first_start) |start| {
        if (last_end > start) return source[start..last_end];
    }
    return null;
}

/// Collect inner attributes from an inline module's declaration_list.
pub fn collectInnerAttributesFromMod(source: []const u8, mod_node: ts.Node, k: *const KindIds) ?[]const u8 {
    var i: u32 = 0;
    while (i < mod_node.childCount()) : (i += 1) {
        const child = mod_node.child(i) orelse continue;
        if (child.kindId() == k.declaration_list) {
            return collectInnerAttributes(source, child, k);
        }
    }
    return null;
}

/// Check if a mod_item is an inline module (has a declaration_list body).
pub fn isInlineMod(node: ts.Node, k: *const KindIds) bool {
    return hasChildKind(node, k.declaration_list);
}
