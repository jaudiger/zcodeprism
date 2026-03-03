const std = @import("std");
const types = @import("../../core/types.zig");
const ts = @import("tree-sitter");

const NodeId = types.NodeId;

pub const FileIndex = @import("../../core/file_index.zig").FileIndex;

/// Pre-resolved tree-sitter node kind IDs for the Rust grammar.
/// Built once per parse() call. Uses integer compare instead of string compare.
pub const KindIds = struct {
    // Top-level declarations
    source_file: u16,
    use_declaration: u16,
    struct_item: u16,
    enum_item: u16,
    union_item: u16,
    trait_item: u16,
    impl_item: u16,
    function_item: u16,
    function_signature_item: u16,
    const_item: u16,
    static_item: u16,
    type_item: u16,
    macro_definition: u16,
    mod_item: u16,

    // Attributes
    attribute_item: u16,
    inner_attribute_item: u16,
    attribute: u16,

    // Type wrappers
    generic_type: u16,
    reference_type: u16,
    primitive_type: u16,

    // Type containers (skipped during base-type extraction)
    type_parameters: u16,
    type_arguments: u16,
    where_clause: u16,

    // Identifiers
    identifier: u16,
    type_identifier: u16,
    field_identifier: u16,
    scoped_identifier: u16,
    scoped_type_identifier: u16,

    // Modifiers
    visibility_modifier: u16,
    function_modifiers: u16,
    extern_modifier: u16,
    mutable_specifier: u16,

    // Bodies
    declaration_list: u16,
    field_declaration_list: u16,
    ordered_field_declaration_list: u16,
    enum_variant_list: u16,
    field_declaration: u16,
    enum_variant: u16,
    associated_type: u16,

    // Expressions
    call_expression: u16,
    field_expression: u16,
    generic_function: u16,
    arguments: u16,
    block: u16,
    let_declaration: u16,

    // Literals and strings
    string_literal: u16,
    string_content: u16,
    token_tree: u16,

    // Comments
    line_comment: u16,
    inner_doc_comment_marker: u16,
    outer_doc_comment_marker: u16,
    doc_comment: u16,

    // Function parts
    parameters: u16,
    parameter: u16,
    self_parameter: u16,

    // Self expression (named node)
    self_expr: u16,

    // Use-declaration structure
    scoped_use_list: u16,
    use_list: u16,
    use_as_clause: u16,
    use_wildcard: u16,

    // Path keywords (named nodes)
    kw_crate: u16,
    kw_super: u16,

    // Keywords (anonymous nodes, named=false)
    kw_pub: u16,
    kw_unsafe: u16,
    kw_async: u16,
    kw_const: u16,
    kw_extern: u16,
    kw_for: u16,
    kw_impl: u16,
    kw_mod: u16,
    kw_fn: u16,

    /// Resolve all node kind names to numeric IDs from the Rust grammar.
    pub fn init(lang: *const ts.Language) KindIds {
        return .{
            .source_file = lang.idForNodeKind("source_file", true),
            .use_declaration = lang.idForNodeKind("use_declaration", true),
            .struct_item = lang.idForNodeKind("struct_item", true),
            .enum_item = lang.idForNodeKind("enum_item", true),
            .union_item = lang.idForNodeKind("union_item", true),
            .trait_item = lang.idForNodeKind("trait_item", true),
            .impl_item = lang.idForNodeKind("impl_item", true),
            .function_item = lang.idForNodeKind("function_item", true),
            .function_signature_item = lang.idForNodeKind("function_signature_item", true),
            .const_item = lang.idForNodeKind("const_item", true),
            .static_item = lang.idForNodeKind("static_item", true),
            .type_item = lang.idForNodeKind("type_item", true),
            .macro_definition = lang.idForNodeKind("macro_definition", true),
            .mod_item = lang.idForNodeKind("mod_item", true),

            .attribute_item = lang.idForNodeKind("attribute_item", true),
            .inner_attribute_item = lang.idForNodeKind("inner_attribute_item", true),
            .attribute = lang.idForNodeKind("attribute", true),

            .generic_type = lang.idForNodeKind("generic_type", true),
            .reference_type = lang.idForNodeKind("reference_type", true),
            .primitive_type = lang.idForNodeKind("primitive_type", true),

            .type_parameters = lang.idForNodeKind("type_parameters", true),
            .type_arguments = lang.idForNodeKind("type_arguments", true),
            .where_clause = lang.idForNodeKind("where_clause", true),

            .identifier = lang.idForNodeKind("identifier", true),
            .type_identifier = lang.idForNodeKind("type_identifier", true),
            .field_identifier = lang.idForNodeKind("field_identifier", true),
            .scoped_identifier = lang.idForNodeKind("scoped_identifier", true),
            .scoped_type_identifier = lang.idForNodeKind("scoped_type_identifier", true),

            .visibility_modifier = lang.idForNodeKind("visibility_modifier", true),
            .function_modifiers = lang.idForNodeKind("function_modifiers", true),
            .extern_modifier = lang.idForNodeKind("extern_modifier", true),
            .mutable_specifier = lang.idForNodeKind("mutable_specifier", true),

            .declaration_list = lang.idForNodeKind("declaration_list", true),
            .field_declaration_list = lang.idForNodeKind("field_declaration_list", true),
            .ordered_field_declaration_list = lang.idForNodeKind("ordered_field_declaration_list", true),
            .enum_variant_list = lang.idForNodeKind("enum_variant_list", true),
            .field_declaration = lang.idForNodeKind("field_declaration", true),
            .enum_variant = lang.idForNodeKind("enum_variant", true),
            .associated_type = lang.idForNodeKind("associated_type", true),

            .call_expression = lang.idForNodeKind("call_expression", true),
            .field_expression = lang.idForNodeKind("field_expression", true),
            .generic_function = lang.idForNodeKind("generic_function", true),
            .arguments = lang.idForNodeKind("arguments", true),
            .block = lang.idForNodeKind("block", true),
            .let_declaration = lang.idForNodeKind("let_declaration", true),

            .string_literal = lang.idForNodeKind("string_literal", true),
            .string_content = lang.idForNodeKind("string_content", true),
            .token_tree = lang.idForNodeKind("token_tree", true),

            .line_comment = lang.idForNodeKind("line_comment", true),
            .inner_doc_comment_marker = lang.idForNodeKind("inner_doc_comment_marker", true),
            .outer_doc_comment_marker = lang.idForNodeKind("outer_doc_comment_marker", true),
            .doc_comment = lang.idForNodeKind("doc_comment", true),

            .parameters = lang.idForNodeKind("parameters", true),
            .parameter = lang.idForNodeKind("parameter", true),
            .self_parameter = lang.idForNodeKind("self_parameter", true),

            .scoped_use_list = lang.idForNodeKind("scoped_use_list", true),
            .use_list = lang.idForNodeKind("use_list", true),
            .use_as_clause = lang.idForNodeKind("use_as_clause", true),
            .use_wildcard = lang.idForNodeKind("use_wildcard", true),

            .self_expr = lang.idForNodeKind("self", true),

            .kw_crate = lang.idForNodeKind("crate", true),
            .kw_super = lang.idForNodeKind("super", true),

            .kw_pub = lang.idForNodeKind("pub", false),
            .kw_unsafe = lang.idForNodeKind("unsafe", false),
            .kw_async = lang.idForNodeKind("async", false),
            .kw_const = lang.idForNodeKind("const", false),
            .kw_extern = lang.idForNodeKind("extern", false),
            .kw_for = lang.idForNodeKind("for", false),
            .kw_impl = lang.idForNodeKind("impl", false),
            .kw_mod = lang.idForNodeKind("mod", false),
            .kw_fn = lang.idForNodeKind("fn", false),
        };
    }
};

pub const ScopeIndex = @import("../../core/scope_index.zig").ScopeIndex;
