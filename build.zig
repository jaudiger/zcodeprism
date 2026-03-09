const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const coverage = b.option(bool, "coverage", "Generate test coverage with kcov") orelse false;

    // --- Dependencies ---

    // zig-tree-sitter bindings (provides the Zig API: Parser, Language, Node, etc.)
    const ts_dep = b.dependency("tree-sitter", .{
        .target = target,
        .optimize = optimize,
    });

    // tree-sitter-rust grammar (compiled from C sources directly)
    const ts_rust_dep = b.dependency("tree-sitter-rust", .{});
    const ts_rust_c_mod = b.createModule(.{
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    ts_rust_c_mod.addCSourceFile(.{ .file = ts_rust_dep.path("src/parser.c") });
    ts_rust_c_mod.addCSourceFile(.{ .file = ts_rust_dep.path("src/scanner.c") });
    ts_rust_c_mod.addIncludePath(ts_rust_dep.path("src"));
    const ts_rust_lib = b.addLibrary(.{
        .name = "tree-sitter-rust",
        .root_module = ts_rust_c_mod,
    });

    // tree-sitter-zig grammar (compiled C parser library)
    const ts_zig_dep = b.dependency("tree-sitter-zig", .{
        .target = target,
        .optimize = optimize,
        .@"build-shared" = false,
    });

    // --- Library module ---

    const lib_mod = b.createModule(.{
        .root_source_file = b.path("src/lib.zig"),
        .target = target,
        .optimize = optimize,
    });
    lib_mod.addImport("tree-sitter", ts_dep.module("tree_sitter"));
    lib_mod.linkLibrary(ts_rust_lib);
    lib_mod.linkLibrary(ts_zig_dep.artifact("tree-sitter-zig"));

    const lib = b.addLibrary(.{
        .name = "zcodeprism",
        .root_module = lib_mod,
    });
    b.installArtifact(lib);

    // --- Executable ---

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe_mod.addImport("zcodeprism", lib_mod);
    exe_mod.linkLibrary(ts_rust_lib);
    exe_mod.linkLibrary(ts_zig_dep.artifact("tree-sitter-zig"));

    const exe = b.addExecutable(.{
        .name = "zcodeprism",
        .root_module = exe_mod,
    });
    b.installArtifact(exe);

    // Run step
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| run_cmd.addArgs(args);
    const run_step = b.step("run", "Run the CLI");
    run_step.dependOn(&run_cmd.step);

    // --- Shared tool utilities module ---

    const tool_utils_mod = b.createModule(.{
        .root_source_file = b.path("tools/tool_utils.zig"),
        .target = target,
        .optimize = optimize,
    });
    tool_utils_mod.addImport("zcodeprism", lib_mod);

    // --- Debug tools ---

    const dump_ast_mod = addTool(b, "dump-ast", "tools/dump_ast.zig", "Dump the raw tree-sitter AST for a source file", target, optimize, lib_mod, tool_utils_mod);
    dump_ast_mod.addImport("tree-sitter", ts_dep.module("tree_sitter"));

    _ = addTool(b, "query-graph", "tools/query_graph.zig", "Exercise the query engine on an indexed directory", target, optimize, lib_mod, tool_utils_mod);
    _ = addTool(b, "parse-directory", "tools/parse_directory.zig", "Index a directory and dump the full code graph", target, optimize, lib_mod, tool_utils_mod);
    _ = addTool(b, "parse-file", "tools/parse_file.zig", "Parse a single source file and dump the graph", target, optimize, lib_mod, tool_utils_mod);
    _ = addTool(b, "render-graph", "tools/render_graph.zig", "Render a code graph (directory or file) as CTG or Mermaid", target, optimize, lib_mod, tool_utils_mod);

    // --- Tests ---

    const test_step = b.step("test", "Run unit tests");

    // kcov arguments for coverage mode
    const kcov_args: []const ?[]const u8 = &.{
        "kcov", "--include-pattern=src/", "kcov-output", null,
    };

    // Test fixtures module
    const fixture_mod = b.createModule(.{
        .root_source_file = b.path("test/fixture_data.zig"),
    });

    // Library unit tests (inline tests in src/)
    const test_mod = b.createModule(.{
        .root_source_file = b.path("src/lib.zig"),
        .target = target,
        .optimize = optimize,
    });
    test_mod.addImport("tree-sitter", ts_dep.module("tree_sitter"));
    test_mod.linkLibrary(ts_rust_lib);
    test_mod.linkLibrary(ts_zig_dep.artifact("tree-sitter-zig"));
    test_mod.addImport("test-fixtures", fixture_mod);

    addTestStep(b, test_step, b.addTest(.{
        .root_module = test_mod,
    }), coverage, kcov_args);

    // --- Integration tests ---

    // Test helpers module (shared by integration tests)
    const helpers_mod = b.createModule(.{
        .root_source_file = b.path("test/test_helpers.zig"),
    });
    helpers_mod.addImport("zcodeprism", lib_mod);

    // Rust parsing integration tests (single-file parsing)
    const rust_parsing_test_mod = b.createModule(.{
        .root_source_file = b.path("test/rust/test_parsing.zig"),
        .target = target,
        .optimize = optimize,
    });
    rust_parsing_test_mod.addImport("zcodeprism", lib_mod);
    rust_parsing_test_mod.addImport("test-fixtures", fixture_mod);
    rust_parsing_test_mod.addImport("test-helpers", helpers_mod);
    rust_parsing_test_mod.linkLibrary(ts_rust_lib);
    rust_parsing_test_mod.linkLibrary(ts_zig_dep.artifact("tree-sitter-zig"));

    addTestStep(b, test_step, b.addTest(.{
        .root_module = rust_parsing_test_mod,
    }), coverage, kcov_args);

    // Rust indexer integration tests (multi-file indexing)
    const rust_indexer_test_mod = b.createModule(.{
        .root_source_file = b.path("test/rust/test_indexer.zig"),
        .target = target,
        .optimize = optimize,
    });
    rust_indexer_test_mod.addImport("zcodeprism", lib_mod);
    rust_indexer_test_mod.addImport("test-fixtures", fixture_mod);
    rust_indexer_test_mod.addImport("test-helpers", helpers_mod);
    rust_indexer_test_mod.linkLibrary(ts_rust_lib);
    rust_indexer_test_mod.linkLibrary(ts_zig_dep.artifact("tree-sitter-zig"));

    addTestStep(b, test_step, b.addTest(.{
        .root_module = rust_indexer_test_mod,
    }), coverage, kcov_args);

    // Tree-sitter binding integration tests
    const ts_test_mod = b.createModule(.{
        .root_source_file = b.path("test/test_tree_sitter.zig"),
        .target = target,
        .optimize = optimize,
    });
    ts_test_mod.addImport("zcodeprism", lib_mod);
    ts_test_mod.addImport("tree-sitter", ts_dep.module("tree_sitter"));
    ts_test_mod.linkLibrary(ts_rust_lib);
    ts_test_mod.linkLibrary(ts_zig_dep.artifact("tree-sitter-zig"));

    addTestStep(b, test_step, b.addTest(.{
        .root_module = ts_test_mod,
    }), coverage, kcov_args);

    // Zig parsing integration tests (single-file parsing: edge_builder + cross_file tests)
    const parsing_test_mod = b.createModule(.{
        .root_source_file = b.path("test/zig/test_parsing.zig"),
        .target = target,
        .optimize = optimize,
    });
    parsing_test_mod.addImport("zcodeprism", lib_mod);
    parsing_test_mod.addImport("test-fixtures", fixture_mod);
    parsing_test_mod.addImport("test-helpers", helpers_mod);
    parsing_test_mod.linkLibrary(ts_rust_lib);
    parsing_test_mod.linkLibrary(ts_zig_dep.artifact("tree-sitter-zig"));

    addTestStep(b, test_step, b.addTest(.{
        .root_module = parsing_test_mod,
    }), coverage, kcov_args);

    // Zig indexer integration tests (multi-file indexing)
    const indexer_test_mod = b.createModule(.{
        .root_source_file = b.path("test/zig/test_indexer.zig"),
        .target = target,
        .optimize = optimize,
    });
    indexer_test_mod.addImport("zcodeprism", lib_mod);
    indexer_test_mod.addImport("test-fixtures", fixture_mod);
    indexer_test_mod.addImport("test-helpers", helpers_mod);
    indexer_test_mod.linkLibrary(ts_rust_lib);
    indexer_test_mod.linkLibrary(ts_zig_dep.artifact("tree-sitter-zig"));

    addTestStep(b, test_step, b.addTest(.{
        .root_module = indexer_test_mod,
    }), coverage, kcov_args);

    // Zig build parsing integration tests (build.zig module/dependency extraction)
    const build_parsing_test_mod = b.createModule(.{
        .root_source_file = b.path("test/zig/test_build_parsing.zig"),
        .target = target,
        .optimize = optimize,
    });
    build_parsing_test_mod.addImport("zcodeprism", lib_mod);
    build_parsing_test_mod.addImport("test-fixtures", fixture_mod);
    build_parsing_test_mod.addImport("test-helpers", helpers_mod);
    build_parsing_test_mod.linkLibrary(ts_rust_lib);
    build_parsing_test_mod.linkLibrary(ts_zig_dep.artifact("tree-sitter-zig"));

    addTestStep(b, test_step, b.addTest(.{
        .root_module = build_parsing_test_mod,
    }), coverage, kcov_args);

    // MCP transport integration tests
    const mcp_test_mod = b.createModule(.{
        .root_source_file = b.path("test/test_mcp_transport.zig"),
        .target = target,
        .optimize = optimize,
    });
    mcp_test_mod.addImport("zcodeprism", lib_mod);
    mcp_test_mod.linkLibrary(ts_rust_lib);
    mcp_test_mod.linkLibrary(ts_zig_dep.artifact("tree-sitter-zig"));

    addTestStep(b, test_step, b.addTest(.{
        .root_module = mcp_test_mod,
    }), coverage, kcov_args);

    // CLI integration tests (spawn the zcodeprism binary)
    const cli_test_mod = b.createModule(.{
        .root_source_file = b.path("test/test_cli.zig"),
        .target = target,
        .optimize = optimize,
    });
    const cli_test = b.addTest(.{ .root_module = cli_test_mod });
    cli_test.step.dependOn(b.getInstallStep());
    addTestStep(b, test_step, cli_test, coverage, kcov_args);
}

fn addTestStep(
    b: *std.Build,
    test_step: *std.Build.Step,
    test_artifact: *std.Build.Step.Compile,
    cov: bool,
    kcov_args: []const ?[]const u8,
) void {
    if (cov) test_artifact.setExecCmd(kcov_args);
    test_step.dependOn(&b.addRunArtifact(test_artifact).step);
}

fn addTool(
    b: *std.Build,
    name: []const u8,
    source: []const u8,
    description: []const u8,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    lib_mod: *std.Build.Module,
    tool_utils_mod: *std.Build.Module,
) *std.Build.Module {
    const mod = b.createModule(.{
        .root_source_file = b.path(source),
        .target = target,
        .optimize = optimize,
    });
    mod.addImport("zcodeprism", lib_mod);
    mod.addImport("tool-utils", tool_utils_mod);
    const exe = b.addExecutable(.{ .name = name, .root_module = mod });
    b.installArtifact(exe);
    const run = b.addRunArtifact(exe);
    run.step.dependOn(b.getInstallStep());
    if (b.args) |args| run.addArgs(args);
    const step = b.step(name, description);
    step.dependOn(&run.step);
    return mod;
}
