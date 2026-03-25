//! Compile-time access to test fixture files.
//! Organized by language. Each language gets its own namespace struct.
//! This module is rooted in `test/`, so @embedFile can reach `test/fixtures/`.

pub const mixed_project = struct {
    pub const lib_rs = @embedFile("fixtures/mixed_project/src/lib.rs");
    pub const main_zig = @embedFile("fixtures/mixed_project/src/main.zig");
};

pub const rust = struct {
    pub const simple = @embedFile("fixtures/rust/simple.rs");

    pub const analysis = struct {
        pub const dead_code = @embedFile("fixtures/rust/analysis/dead_code.rs");
    };

    pub const edge_cases = struct {
        pub const aliased_trait = @embedFile("fixtures/rust/edge_cases/aliased_trait.rs");
        pub const deeply_nested = @embedFile("fixtures/rust/edge_cases/deeply_nested.rs");
        pub const empty = @embedFile("fixtures/rust/edge_cases/empty.rs");
        pub const many_attrs = @embedFile("fixtures/rust/edge_cases/many_attrs.rs");
        pub const no_pub = @embedFile("fixtures/rust/edge_cases/no_pub.rs");
        pub const only_comments = @embedFile("fixtures/rust/edge_cases/only_comments.rs");
        pub const tuple_field_attrs = @embedFile("fixtures/rust/edge_cases/tuple_field_attrs.rs");
    };

    pub const glob_import = struct {
        pub const lib_rs = @embedFile("fixtures/rust/glob_import/lib.rs");
        pub const sub_rs = @embedFile("fixtures/rust/glob_import/sub.rs");
        pub const utils_rs = @embedFile("fixtures/rust/glob_import/utils.rs");
    };

    pub const project = struct {
        pub const lib_rs = @embedFile("fixtures/rust/project/lib.rs");
        pub const parser_rs = @embedFile("fixtures/rust/project/parser.rs");
        pub const parser_helpers_rs = @embedFile("fixtures/rust/project/parser/helpers.rs");
        pub const utils_rs = @embedFile("fixtures/rust/project/utils.rs");
    };

    pub const reexport_chain = struct {
        pub const deep_rs = @embedFile("fixtures/rust/reexport_chain/mid/deep.rs");
        pub const lib_rs = @embedFile("fixtures/rust/reexport_chain/lib.rs");
        pub const mid_rs = @embedFile("fixtures/rust/reexport_chain/mid.rs");
    };
};

pub const zig = struct {
    pub const file_struct = @embedFile("fixtures/zig/file_struct.zig");
    pub const generic_type = @embedFile("fixtures/zig/generic_type.zig");
    pub const simple = @embedFile("fixtures/zig/simple.zig");

    pub const lsp = struct {
        pub const inferred_errors = @embedFile("fixtures/zig/lsp/inferred_errors.zig");
    };

    pub const analysis = struct {
        pub const circular = struct {
            pub const a_zig = @embedFile("fixtures/zig/analysis/circular/a.zig");
            pub const b_zig = @embedFile("fixtures/zig/analysis/circular/b.zig");
            pub const c_zig = @embedFile("fixtures/zig/analysis/circular/c.zig");
        };
        pub const complex = @embedFile("fixtures/zig/analysis/complex.zig");
        pub const dead_code = @embedFile("fixtures/zig/analysis/dead_code.zig");
        pub const duplicates = @embedFile("fixtures/zig/analysis/duplicates.zig");
    };

    pub const build_no_deps = struct {
        pub const build_zig = @embedFile("fixtures/zig/build_no_deps/build.zig");
        pub const build_zig_zon = @embedFile("fixtures/zig/build_no_deps/build.zig.zon");
        pub const src_main_zig = @embedFile("fixtures/zig/build_no_deps/src/main.zig");
    };

    pub const build_parsing = struct {
        pub const build_zig = @embedFile("fixtures/zig/build_parsing/build.zig");
        pub const build_zig_zon = @embedFile("fixtures/zig/build_parsing/build.zig.zon");
        pub const src_lib_zig = @embedFile("fixtures/zig/build_parsing/src/lib.zig");
        pub const src_main_zig = @embedFile("fixtures/zig/build_parsing/src/main.zig");
    };

    pub const direct_extraction = struct {
        pub const fn_consumer_zig = @embedFile("fixtures/zig/direct_extraction/fn_consumer.zig");
        pub const provider_zig = @embedFile("fixtures/zig/direct_extraction/provider.zig");
        pub const type_consumer_zig = @embedFile("fixtures/zig/direct_extraction/type_consumer.zig");
    };

    pub const dir_imports = struct {
        pub const compress_flate_inner_zig = @embedFile("fixtures/zig/dir_imports/compress/flate/inner.zig");
        pub const compress_flate_zig = @embedFile("fixtures/zig/dir_imports/compress/flate.zig");
        pub const crypto_aegis_zig = @embedFile("fixtures/zig/dir_imports/crypto/aegis.zig");
        pub const crypto_helpers_zig = @embedFile("fixtures/zig/dir_imports/crypto/helpers.zig");
        pub const crypto_hmac_zig = @embedFile("fixtures/zig/dir_imports/crypto/hmac.zig");
        pub const root_zig = @embedFile("fixtures/zig/dir_imports/root.zig");
        pub const tar_helpers_zig = @embedFile("fixtures/zig/dir_imports/tar/helpers.zig");
        pub const tar_reader_zig = @embedFile("fixtures/zig/dir_imports/tar/reader.zig");
    };

    pub const edge_cases = struct {
        pub const deeply_nested = @embedFile("fixtures/zig/edge_cases/deeply_nested.zig");
        pub const duplicate_method_names = @embedFile("fixtures/zig/edge_cases/duplicate_method_names.zig");
        pub const empty = @embedFile("fixtures/zig/edge_cases/empty.zig");
        pub const extern_functions = @embedFile("fixtures/zig/edge_cases/extern_functions.zig");
        pub const external_method_collision = @embedFile("fixtures/zig/edge_cases/external_method_collision.zig");
        pub const generic_dual_self = @embedFile("fixtures/zig/edge_cases/generic_dual_self.zig");
        pub const local_type_param = @embedFile("fixtures/zig/edge_cases/local_type_param.zig");
        pub const many_params = @embedFile("fixtures/zig/edge_cases/many_params.zig");
        pub const mutability = @embedFile("fixtures/zig/edge_cases/mutability.zig");
        pub const no_pub = @embedFile("fixtures/zig/edge_cases/no_pub.zig");
        pub const only_comments = @embedFile("fixtures/zig/edge_cases/only_comments.zig");
        pub const project_single_file = @embedFile("fixtures/zig/edge_cases/single_file_project/single.zig");
        pub const unicode_names = @embedFile("fixtures/zig/edge_cases/unicode_names.zig");
    };

    pub const inner_struct_call = struct {
        pub const consumer_zig = @embedFile("fixtures/zig/inner_struct_call/consumer.zig");
        pub const provider_zig = @embedFile("fixtures/zig/inner_struct_call/provider.zig");
    };

    pub const name_collision = struct {
        pub const alpha_zig = @embedFile("fixtures/zig/name_collision/alpha.zig");
        pub const beta_zig = @embedFile("fixtures/zig/name_collision/beta.zig");
        pub const consumer_zig = @embedFile("fixtures/zig/name_collision/consumer.zig");
    };

    pub const param_method_call = struct {
        pub const chained_zig = @embedFile("fixtures/zig/param_method_call/chained.zig");
        pub const client_zig = @embedFile("fixtures/zig/param_method_call/client.zig");
        pub const consumer_zig = @embedFile("fixtures/zig/param_method_call/consumer.zig");
        pub const factory_zig = @embedFile("fixtures/zig/param_method_call/factory.zig");
        pub const multi_param_zig = @embedFile("fixtures/zig/param_method_call/multi_param.zig");
        pub const no_calls_zig = @embedFile("fixtures/zig/param_method_call/no_calls.zig");
        pub const optional_param_zig = @embedFile("fixtures/zig/param_method_call/optional_param.zig");
        pub const pointer_param_zig = @embedFile("fixtures/zig/param_method_call/pointer_param.zig");
        pub const return_value_zig = @embedFile("fixtures/zig/param_method_call/return_value.zig");
        pub const self_calls_param_zig = @embedFile("fixtures/zig/param_method_call/self_calls_param.zig");
        pub const service_zig = @embedFile("fixtures/zig/param_method_call/service.zig");
        pub const service_with_client_zig = @embedFile("fixtures/zig/param_method_call/service_with_client.zig");
    };

    pub const project = struct {
        pub const main_zig = @embedFile("fixtures/zig/project/main.zig");
        pub const parser_zig = @embedFile("fixtures/zig/project/parser.zig");
        pub const utils_zig = @embedFile("fixtures/zig/project/utils.zig");
    };

    pub const test_import_call = struct {
        pub const consumer_zig = @embedFile("fixtures/zig/test_import_call/consumer.zig");
        pub const provider_zig = @embedFile("fixtures/zig/test_import_call/provider.zig");
    };
};
