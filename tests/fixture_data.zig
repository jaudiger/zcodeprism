//! Compile-time access to test fixture files.
//! Organized by language. Each language gets its own namespace struct.
//! This module is rooted in `tests/`, so @embedFile can reach `tests/fixtures/`.

pub const zig = struct {
    pub const file_struct = @embedFile("fixtures/zig/file_struct.zig");
    pub const generic_type = @embedFile("fixtures/zig/generic_type.zig");
    pub const simple = @embedFile("fixtures/zig/simple.zig");

    pub const edge_cases = struct {
        pub const deeply_nested = @embedFile("fixtures/zig/edge_cases/deeply_nested.zig");
        pub const duplicate_method_names = @embedFile("fixtures/zig/edge_cases/duplicate_method_names.zig");
        pub const empty = @embedFile("fixtures/zig/edge_cases/empty.zig");
        pub const external_method_collision = @embedFile("fixtures/zig/edge_cases/external_method_collision.zig");
        pub const generic_dual_self = @embedFile("fixtures/zig/edge_cases/generic_dual_self.zig");
        pub const local_type_param = @embedFile("fixtures/zig/edge_cases/local_type_param.zig");
        pub const many_params = @embedFile("fixtures/zig/edge_cases/many_params.zig");
        pub const no_pub = @embedFile("fixtures/zig/edge_cases/no_pub.zig");
        pub const only_comments = @embedFile("fixtures/zig/edge_cases/only_comments.zig");
        pub const project_single_file = @embedFile("fixtures/zig/edge_cases/single_file_project/single.zig");
        pub const unicode_names = @embedFile("fixtures/zig/edge_cases/unicode_names.zig");
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
