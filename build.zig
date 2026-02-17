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
