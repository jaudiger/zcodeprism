const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // --- Dependencies ---

    // zig-tree-sitter bindings (provides the Zig API: Parser, Language, Node, etc.)
    const ts_dep = b.dependency("tree-sitter", .{
        .target = target,
        .optimize = optimize,
    });

    // tree-sitter-zig grammar (compiled C parser library)
    // The grammar package compiles parser.c internally.
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

    // --- Dump AST tool ---

    const dump_ast_mod = b.createModule(.{
        .root_source_file = b.path("tools/dump_ast.zig"),
        .target = target,
        .optimize = optimize,
    });
    dump_ast_mod.addImport("zcodeprism", lib_mod);
    dump_ast_mod.addImport("tree-sitter", ts_dep.module("tree_sitter"));

    const dump_ast_exe = b.addExecutable(.{
        .name = "dump-ast",
        .root_module = dump_ast_mod,
    });
    b.installArtifact(dump_ast_exe);

    const run_dump_ast = b.addRunArtifact(dump_ast_exe);
    run_dump_ast.step.dependOn(b.getInstallStep());
    if (b.args) |args| run_dump_ast.addArgs(args);
    const run_dump_ast_step = b.step("dump-ast", "Dump the raw tree-sitter AST for a source file");
    run_dump_ast_step.dependOn(&run_dump_ast.step);

    // --- Parse directory tool ---

    const parse_dir_mod = b.createModule(.{
        .root_source_file = b.path("tools/parse_directory.zig"),
        .target = target,
        .optimize = optimize,
    });
    parse_dir_mod.addImport("zcodeprism", lib_mod);

    const parse_dir_exe = b.addExecutable(.{
        .name = "parse-directory",
        .root_module = parse_dir_mod,
    });
    b.installArtifact(parse_dir_exe);

    const run_parse_dir = b.addRunArtifact(parse_dir_exe);
    run_parse_dir.step.dependOn(b.getInstallStep());
    if (b.args) |args| run_parse_dir.addArgs(args);
    const run_parse_dir_step = b.step("parse-directory", "Index a directory and dump the full code graph");
    run_parse_dir_step.dependOn(&run_parse_dir.step);

    // --- Parse file tool ---

    const parse_file_mod = b.createModule(.{
        .root_source_file = b.path("tools/parse_file.zig"),
        .target = target,
        .optimize = optimize,
    });
    parse_file_mod.addImport("zcodeprism", lib_mod);

    const parse_file_exe = b.addExecutable(.{
        .name = "parse-file",
        .root_module = parse_file_mod,
    });
    b.installArtifact(parse_file_exe);

    const run_parse_file = b.addRunArtifact(parse_file_exe);
    run_parse_file.step.dependOn(b.getInstallStep());
    if (b.args) |args| run_parse_file.addArgs(args);
    const run_parse_file_step = b.step("parse-file", "Parse a single .zig file and dump the graph");
    run_parse_file_step.dependOn(&run_parse_file.step);

    // --- Tests ---

    const test_mod = b.createModule(.{
        .root_source_file = b.path("src/lib.zig"),
        .target = target,
        .optimize = optimize,
    });
    test_mod.addImport("tree-sitter", ts_dep.module("tree_sitter"));
    test_mod.linkLibrary(ts_zig_dep.artifact("tree-sitter-zig"));

    // Test fixtures module
    const fixture_mod = b.createModule(.{
        .root_source_file = b.path("tests/fixture_data.zig"),
    });
    test_mod.addImport("test-fixtures", fixture_mod);

    const lib_unit_tests = b.addTest(.{
        .root_module = test_mod,
    });
    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
}
