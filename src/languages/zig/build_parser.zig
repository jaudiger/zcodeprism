const std = @import("std");
const ts = @import("tree-sitter");
const ts_api = @import("../../parser/tree_sitter_api.zig");
const logging = @import("../../logging.zig");

const Logger = logging.Logger;

/// Target kind extracted from build script calls.
pub const TargetKind = enum {
    executable,
    library,
    test_target,
};

/// Structured result of parsing a build.zig file.
///
/// Slice fields use optionals: null means "not present", non-null means
/// heap-allocated by the caller's allocator. Call `deinit` to release.
pub const BuildInfo = struct {
    modules: ?[]ModuleEntry = null,
    targets: ?[]TargetEntry = null,
    dependencies: ?[]DependencyEntry = null,
    dependency_urls: ?[]DependencyUrl = null,

    pub const ModuleEntry = struct {
        /// Variable name assigned to the module in the build script.
        name: []u8,
        /// Root source file path as written in the build script, or null.
        root_source_file: ?[]u8,
        /// Names of imports added to this module via addImport.
        import_names: ?[][]u8 = null,
    };

    pub const TargetEntry = struct {
        /// Name string passed to the target constructor.
        name: []u8,
        /// Whether this is an executable, library, or test target.
        kind: TargetKind,
        /// Variable name of the root module assigned to this target, or null.
        root_module_var: ?[]u8,
    };

    pub const DependencyEntry = struct {
        /// Dependency name string passed to b.dependency().
        name: []u8,
        /// Variable name assigned to the dependency result.
        var_name: ?[]u8,
    };

    pub const DependencyUrl = struct {
        /// Dependency name from the .zon manifest.
        name: []u8,
        /// URL string from the .url field.
        url: []u8,
    };

    pub fn deinit(self: BuildInfo, allocator: std.mem.Allocator) void {
        if (self.modules) |modules| {
            for (modules) |m| {
                allocator.free(m.name);
                if (m.root_source_file) |rsf| allocator.free(rsf);
                if (m.import_names) |imports| {
                    for (imports) |imp| allocator.free(imp);
                    allocator.free(imports);
                }
            }
            allocator.free(modules);
        }
        if (self.targets) |targets| {
            for (targets) |t| {
                allocator.free(t.name);
                if (t.root_module_var) |rmv| allocator.free(rmv);
            }
            allocator.free(targets);
        }
        if (self.dependencies) |dependencies| {
            for (dependencies) |d| {
                allocator.free(d.name);
                if (d.var_name) |vn| allocator.free(vn);
            }
            allocator.free(dependencies);
        }
        if (self.dependency_urls) |urls| {
            for (urls) |du| {
                allocator.free(du.name);
                allocator.free(du.url);
            }
            allocator.free(urls);
        }
    }
};

/// Parse build.zig source text via tree-sitter and extract module,
/// target, and dependency declarations.
pub fn parseBuildSource(allocator: std.mem.Allocator, source: []const u8, log: Logger) !BuildInfo {
    _ = log;
    if (source.len == 0) return emptyBuildInfo();

    const tree = ts_api.parseSource(ts_api.zigLanguage(), source) orelse {
        return emptyBuildInfo();
    };
    defer tree.destroy();

    const root = tree.rootNode();

    var modules = std.ArrayList(BuildInfo.ModuleEntry){};
    errdefer {
        for (modules.items) |m| {
            allocator.free(m.name);
            if (m.root_source_file) |rsf| allocator.free(rsf);
            if (m.import_names) |imports| {
                for (imports) |imp| allocator.free(imp);
                allocator.free(imports);
            }
        }
        modules.deinit(allocator);
    }

    var targets = std.ArrayList(BuildInfo.TargetEntry){};
    errdefer {
        for (targets.items) |t| {
            allocator.free(t.name);
            if (t.root_module_var) |rmv| allocator.free(rmv);
        }
        targets.deinit(allocator);
    }

    var deps = std.ArrayList(BuildInfo.DependencyEntry){};
    errdefer {
        for (deps.items) |d| {
            allocator.free(d.name);
            if (d.var_name) |vn| allocator.free(vn);
        }
        deps.deinit(allocator);
    }

    // Find the `build` function and walk its body block.
    const build_block = findBuildFunctionBlock(source, root) orelse return .{};

    // Walk variable declarations inside the build function body
    // for createModule, addExecutable, addLibrary, addTest, and dependency calls.
    var i: u32 = 0;
    while (i < build_block.namedChildCount()) : (i += 1) {
        const child = build_block.namedChild(i) orelse continue;
        if (!std.mem.eql(u8, child.kind(), "variable_declaration")) continue;

        const var_name = extractVarName(source, child) orelse continue;
        const value_node = findValueExpression(child) orelse continue;

        if (!std.mem.eql(u8, value_node.kind(), "call_expression")) continue;

        const fn_name = extractCallFunctionName(source, value_node) orelse continue;

        if (std.mem.eql(u8, fn_name, "createModule")) {
            const rsf = extractRootSourceFile(source, value_node);
            const name_dup = try allocator.dupe(u8, var_name);
            errdefer allocator.free(name_dup);
            const rsf_dup = if (rsf) |r| try allocator.dupe(u8, r) else null;
            errdefer if (rsf_dup) |rd| allocator.free(rd);
            try modules.append(allocator, .{
                .name = name_dup,
                .root_source_file = rsf_dup,
            });
        } else if (std.mem.eql(u8, fn_name, "addExecutable")) {
            const target_name = extractNameField(source, value_node) orelse continue;
            const rmv = extractRootModuleVar(source, value_node);
            const name_dup = try allocator.dupe(u8, target_name);
            errdefer allocator.free(name_dup);
            const rmv_dup = if (rmv) |r| try allocator.dupe(u8, r) else null;
            errdefer if (rmv_dup) |rd| allocator.free(rd);
            try targets.append(allocator, .{
                .name = name_dup,
                .kind = .executable,
                .root_module_var = rmv_dup,
            });
        } else if (std.mem.eql(u8, fn_name, "addLibrary")) {
            const target_name = extractNameField(source, value_node) orelse continue;
            const rmv = extractRootModuleVar(source, value_node);
            const name_dup = try allocator.dupe(u8, target_name);
            errdefer allocator.free(name_dup);
            const rmv_dup = if (rmv) |r| try allocator.dupe(u8, r) else null;
            errdefer if (rmv_dup) |rd| allocator.free(rd);
            try targets.append(allocator, .{
                .name = name_dup,
                .kind = .library,
                .root_module_var = rmv_dup,
            });
        } else if (std.mem.eql(u8, fn_name, "addTest")) {
            const target_name = extractNameField(source, value_node) orelse var_name;
            const rmv = extractRootModuleVar(source, value_node);
            const name_dup = try allocator.dupe(u8, target_name);
            errdefer allocator.free(name_dup);
            const rmv_dup = if (rmv) |r| try allocator.dupe(u8, r) else null;
            errdefer if (rmv_dup) |rd| allocator.free(rd);
            try targets.append(allocator, .{
                .name = name_dup,
                .kind = .test_target,
                .root_module_var = rmv_dup,
            });
        } else if (std.mem.eql(u8, fn_name, "dependency")) {
            const dep_name = extractFirstStringArg(source, value_node) orelse continue;
            const name_dup = try allocator.dupe(u8, dep_name);
            errdefer allocator.free(name_dup);
            const vn_dup = try allocator.dupe(u8, var_name);
            errdefer allocator.free(vn_dup);
            try deps.append(allocator, .{
                .name = name_dup,
                .var_name = vn_dup,
            });
        }
    }

    // Collect addImport calls and associate them with modules.
    i = 0;
    while (i < build_block.namedChildCount()) : (i += 1) {
        const child = build_block.namedChild(i) orelse continue;
        try collectAddImportCalls(allocator, source, child, modules.items);
    }

    return .{
        .modules = if (modules.items.len > 0) try modules.toOwnedSlice(allocator) else null,
        .targets = if (targets.items.len > 0) try targets.toOwnedSlice(allocator) else null,
        .dependencies = if (deps.items.len > 0) try deps.toOwnedSlice(allocator) else null,
    };
}

/// Extract dependency name-URL pairs from build.zig.zon content.
/// Uses text scanning (not tree-sitter) since .zon is a data format.
/// Returns null when no dependencies section is found.
pub fn extractDependencyUrls(allocator: std.mem.Allocator, content: []const u8) !?[]BuildInfo.DependencyUrl {
    const deps_start = std.mem.indexOf(u8, content, ".dependencies") orelse return null;
    var pos = deps_start + ".dependencies".len;

    // Find opening brace of the dependencies block.
    while (pos < content.len and content[pos] != '{') : (pos += 1) {}
    if (pos >= content.len) return null;
    pos += 1;

    var results = std.ArrayList(BuildInfo.DependencyUrl){};
    errdefer {
        for (results.items) |r| {
            allocator.free(r.name);
            allocator.free(r.url);
        }
        results.deinit(allocator);
    }

    var depth: usize = 1;
    while (pos < content.len and depth > 0) {
        switch (content[pos]) {
            '"' => {
                // Skip string literals so braces/dots inside them
                // do not confuse the depth tracker.
                pos += 1;
                while (pos < content.len and content[pos] != '"') {
                    if (content[pos] == '\\') pos += 1; // skip escaped char
                    pos += 1;
                }
                if (pos < content.len) pos += 1; // skip closing quote
            },
            '{' => {
                depth += 1;
                pos += 1;
            },
            '}' => {
                depth -= 1;
                pos += 1;
            },
            '.' => {
                if (depth == 1) {
                    // At depth 1, this is a dependency field name.
                    if (extractFieldName(content, pos + 1)) |field_name| {
                        // Find the URL inside this dependency block.
                        const block_start = std.mem.indexOf(u8, content[pos..], "{");
                        if (block_start) |bs| {
                            const abs_bs = pos + bs;
                            if (findUrlInBlock(content, abs_bs)) |url| {
                                const name_dup = try allocator.dupe(u8, field_name);
                                errdefer allocator.free(name_dup);
                                const url_dup = try allocator.dupe(u8, url);
                                errdefer allocator.free(url_dup);
                                try results.append(allocator, .{
                                    .name = name_dup,
                                    .url = url_dup,
                                });
                            }
                        }
                    }
                }
                pos += 1;
            },
            else => pos += 1,
        }
    }

    if (results.items.len == 0) return null;
    return try results.toOwnedSlice(allocator);
}

/// Read build.zig and build.zig.zon from project_root and return
/// combined build information. Missing files produce empty results.
pub fn parseBuildFiles(allocator: std.mem.Allocator, project_root: []const u8, log: Logger) !BuildInfo {
    var dir = std.fs.openDirAbsolute(project_root, .{}) catch return .{};
    defer dir.close();

    // Parse build.zig via tree-sitter.
    var info = blk: {
        const build_file = dir.openFile("build.zig", .{}) catch break :blk BuildInfo{};
        defer build_file.close();
        const build_source = build_file.readToEndAlloc(allocator, 1 * 1024 * 1024) catch break :blk BuildInfo{};
        defer allocator.free(build_source);
        break :blk try parseBuildSource(allocator, build_source, log);
    };
    errdefer info.deinit(allocator);

    // Enrich with dependency URLs from the .zon manifest.
    const zon_file = dir.openFile("build.zig.zon", .{}) catch return info;
    defer zon_file.close();
    const zon_content = zon_file.readToEndAlloc(allocator, 1 * 1024 * 1024) catch return info;
    defer allocator.free(zon_content);

    info.dependency_urls = try extractDependencyUrls(allocator, zon_content);

    return info;
}

fn emptyBuildInfo() BuildInfo {
    return .{};
}

// --- Tree-sitter AST helpers ---

/// Find the body block of the top-level `build` function declaration.
fn findBuildFunctionBlock(source: []const u8, root: ts.Node) ?ts.Node {
    var i: u32 = 0;
    while (i < root.namedChildCount()) : (i += 1) {
        const child = root.namedChild(i) orelse continue;
        if (!std.mem.eql(u8, child.kind(), "function_declaration")) continue;
        var j: u32 = 0;
        while (j < child.namedChildCount()) : (j += 1) {
            const fc = child.namedChild(j) orelse continue;
            if (std.mem.eql(u8, fc.kind(), "identifier")) {
                const name = ts_api.nodeText(source, fc);
                if (std.mem.eql(u8, name, "build")) {
                    var k: u32 = 0;
                    while (k < child.namedChildCount()) : (k += 1) {
                        const bc = child.namedChild(k) orelse continue;
                        if (std.mem.eql(u8, bc.kind(), "block")) return bc;
                    }
                }
                break;
            }
        }
    }
    return null;
}

fn extractVarName(source: []const u8, var_decl: ts.Node) ?[]const u8 {
    var ci: u32 = 0;
    while (ci < var_decl.namedChildCount()) : (ci += 1) {
        const c = var_decl.namedChild(ci) orelse continue;
        if (std.mem.eql(u8, c.kind(), "identifier")) {
            return ts_api.nodeText(source, c);
        }
    }
    return null;
}

fn findValueExpression(var_decl: ts.Node) ?ts.Node {
    // In the tree-sitter Zig grammar, a variable_declaration's named
    // children are [identifier, (type)?, value_expression]. The value
    // is always the last named child.
    var last: ?ts.Node = null;
    var ci: u32 = 0;
    while (ci < var_decl.namedChildCount()) : (ci += 1) {
        last = var_decl.namedChild(ci);
    }
    return last;
}

fn extractCallFunctionName(source: []const u8, call: ts.Node) ?[]const u8 {
    // call_expression has a function child. For method calls like b.createModule,
    // we want just the last component.
    const func = call.namedChild(0) orelse return null;
    const text = ts_api.nodeText(source, func);

    // Handle field_expression (b.createModule).
    if (std.mem.eql(u8, func.kind(), "field_expression")) {
        if (std.mem.lastIndexOfScalar(u8, text, '.')) |dot| {
            return text[dot + 1 ..];
        }
    }
    return text;
}

fn extractRootSourceFile(source: []const u8, call: ts.Node) ?[]const u8 {
    // Look through the anonymous struct init argument for root_source_file.
    const args = call.namedChild(1) orelse return null;
    return findStructFieldStringValue(source, args, "root_source_file");
}

fn extractNameField(source: []const u8, call: ts.Node) ?[]const u8 {
    const args = call.namedChild(1) orelse return null;
    return findStructFieldStringValue(source, args, "name");
}

fn extractRootModuleVar(source: []const u8, call: ts.Node) ?[]const u8 {
    // Look for .root_module = <identifier> in the struct init.
    const args = call.namedChild(1) orelse return null;
    return findStructFieldIdentValue(source, args, "root_module");
}

fn findStructFieldStringValue(source: []const u8, node: ts.Node, field_name: []const u8) ?[]const u8 {
    // Walk children looking for struct_field_init nodes.
    var ci: u32 = 0;
    while (ci < node.childCount()) : (ci += 1) {
        const child = node.child(ci) orelse continue;
        const child_text = ts_api.nodeText(source, child);

        // Look for the pattern: .field_name = b.path("value")
        // or .field_name = "value"
        if (child_text.len > 0 and child_text[0] == '.') {
            const rest = child_text[1..];
            if (matchesFieldName(rest, field_name)) {
                const after_name = rest[field_name.len..];
                return extractQuotedString(after_name);
            }
        }
    }
    // Fallback: scan the full text for the field pattern.
    const full_text = ts_api.nodeText(source, node);
    return findFieldInText(full_text, field_name);
}

/// Look for `.field_name = identifier` in the text representation of a struct init.
/// Field names longer than 63 characters are not supported (stack buffer limit).
fn findStructFieldIdentValue(source: []const u8, node: ts.Node, field_name: []const u8) ?[]const u8 {
    const full_text = ts_api.nodeText(source, node);
    var search_buf: [64]u8 = undefined;
    if (field_name.len + 1 > search_buf.len) return null;
    search_buf[0] = '.';
    @memcpy(search_buf[1..][0..field_name.len], field_name);
    const needle = search_buf[0 .. field_name.len + 1];

    const idx = std.mem.indexOf(u8, full_text, needle) orelse return null;
    // Word boundary: character after the field name must not be an ident char.
    const end_pos = idx + needle.len;
    if (end_pos < full_text.len and isIdentChar(full_text[end_pos])) return null;

    const after = full_text[end_pos..];
    // Skip whitespace and '='
    var pos: usize = 0;
    while (pos < after.len and (after[pos] == ' ' or after[pos] == '=' or after[pos] == '\n' or after[pos] == '\r' or after[pos] == '\t')) : (pos += 1) {}
    if (pos >= after.len) return null;
    // Read identifier
    const start = pos;
    while (pos < after.len and isIdentChar(after[pos])) : (pos += 1) {}
    if (pos == start) return null;
    return after[start..pos];
}

fn findFieldInText(text: []const u8, field_name: []const u8) ?[]const u8 {
    // Search for .field_name followed by = and a quoted string (possibly inside b.path()).
    var search_buf: [64]u8 = undefined;
    if (field_name.len + 1 > search_buf.len) return null;
    search_buf[0] = '.';
    @memcpy(search_buf[1..][0..field_name.len], field_name);
    const needle = search_buf[0 .. field_name.len + 1];

    const idx = std.mem.indexOf(u8, text, needle) orelse return null;
    // Word boundary: next character must not continue an identifier.
    const end_pos = idx + needle.len;
    if (end_pos < text.len and isIdentChar(text[end_pos])) return null;

    return extractQuotedString(text[end_pos..]);
}

fn extractQuotedString(text: []const u8) ?[]const u8 {
    const q1 = std.mem.indexOfScalar(u8, text, '"') orelse return null;
    const rest = text[q1 + 1 ..];
    // Scan for unescaped closing quote.
    var i: usize = 0;
    while (i < rest.len) : (i += 1) {
        if (rest[i] == '\\') {
            i += 1; // skip escaped character
            continue;
        }
        if (rest[i] == '"') return rest[0..i];
    }
    return null;
}

fn extractFirstStringArg(source: []const u8, call: ts.Node) ?[]const u8 {
    // The first argument in a call like b.dependency("name", ...).
    const text = ts_api.nodeText(source, call);
    // Find opening paren of the call arguments.
    const paren = std.mem.indexOfScalar(u8, text, '(') orelse return null;
    const after = text[paren + 1 ..];
    return extractQuotedString(after);
}

fn collectAddImportCalls(allocator: std.mem.Allocator, source: []const u8, node: ts.Node, modules: []BuildInfo.ModuleEntry) !void {
    // Look for all .addImport("name", ...) calls in this statement and
    // associate each with the matching module entry.
    const text = ts_api.nodeText(source, node);
    const pattern = ".addImport(\"";

    var search_pos: usize = 0;
    while (search_pos < text.len) {
        const add_idx = std.mem.indexOf(u8, text[search_pos..], pattern) orelse break;
        const abs_idx = search_pos + add_idx;

        const receiver = text[0..abs_idx];
        const recv_name = lastIdentifier(receiver);
        if (recv_name.len == 0) {
            search_pos = abs_idx + pattern.len;
            continue;
        }

        const after = text[abs_idx + pattern.len ..];
        const end_quote = std.mem.indexOfScalar(u8, after, '"') orelse break;
        const import_name = after[0..end_quote];

        for (modules) |*m| {
            if (std.mem.eql(u8, m.name, recv_name)) {
                const duped = try allocator.dupe(u8, import_name);
                errdefer allocator.free(duped);
                // Build the import list via ArrayList to avoid quadratic realloc.
                var list = std.ArrayList([]u8).fromOwnedSlice(m.import_names orelse &.{});
                try list.append(allocator, duped);
                m.import_names = try list.toOwnedSlice(allocator);
                break;
            }
        }

        search_pos = abs_idx + pattern.len + end_quote + 1;
    }
}

fn lastIdentifier(text: []const u8) []const u8 {
    var end = text.len;
    while (end > 0 and !isIdentChar(text[end - 1])) : (end -= 1) {}
    var start = end;
    while (start > 0 and isIdentChar(text[start - 1])) : (start -= 1) {}
    return text[start..end];
}

fn isIdentChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_';
}

fn isIdentStart(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

/// Check that `text` starts with `name` and the next character (if any) is
/// not an identifier character, preventing `.name` from matching `.namespace`.
fn matchesFieldName(text: []const u8, name: []const u8) bool {
    if (!std.mem.startsWith(u8, text, name)) return false;
    if (text.len > name.len and isIdentChar(text[name.len])) return false;
    return true;
}

fn extractFieldName(content: []const u8, pos: usize) ?[]const u8 {
    if (pos >= content.len) return null;
    if (content[pos] == '@' and pos + 1 < content.len and content[pos + 1] == '"') {
        // .@"name" pattern. Scan for unescaped closing quote.
        const start = pos + 2;
        var i: usize = start;
        while (i < content.len) : (i += 1) {
            if (content[i] == '\\') {
                i += 1;
                continue;
            }
            if (content[i] == '"') return content[start..i];
        }
        return null;
    }
    if (!isIdentStart(content[pos])) return null;
    var end = pos;
    while (end < content.len and isIdentChar(content[end])) : (end += 1) {}
    return content[pos..end];
}

fn findUrlInBlock(content: []const u8, block_start: usize) ?[]const u8 {
    // Find .url = "..." inside a dependency block starting at block_start.
    var pos = block_start;
    var depth: usize = 0;
    while (pos < content.len) : (pos += 1) {
        switch (content[pos]) {
            '{' => depth += 1,
            '}' => {
                if (depth <= 1) return null;
                depth -= 1;
            },
            '.' => {
                if (depth == 1) {
                    const remaining = content[pos + 1 ..];
                    if (matchesFieldName(remaining, "url")) {
                        const after_url = remaining["url".len..];
                        return extractQuotedString(after_url);
                    }
                }
            },
            else => {},
        }
    }
    return null;
}

// --- Tests ---

test "parseBuildSource returns empty for empty source" {
    // Arrange
    const allocator = std.testing.allocator;

    // Act
    const info = try parseBuildSource(allocator, "", Logger.noop);
    defer info.deinit(allocator);

    // Assert
    try std.testing.expect(info.modules == null);
    try std.testing.expect(info.targets == null);
    try std.testing.expect(info.dependencies == null);
    try std.testing.expect(info.dependency_urls == null);
}

test "parseBuildSource extracts createModule" {
    // Arrange
    const allocator = std.testing.allocator;
    const source =
        \\const std = @import("std");
        \\pub fn build(b: *std.Build) void {
        \\    const lib_mod = b.createModule(.{
        \\        .root_source_file = b.path("src/lib.zig"),
        \\    });
        \\    _ = lib_mod;
        \\}
    ;

    // Act
    const info = try parseBuildSource(allocator, source, Logger.noop);
    defer info.deinit(allocator);

    // Assert
    const modules = info.modules orelse return error.TestExpectedEqual;
    try std.testing.expectEqual(@as(usize, 1), modules.len);
    try std.testing.expectEqualStrings("lib_mod", modules[0].name);
    try std.testing.expect(modules[0].root_source_file != null);
    try std.testing.expectEqualStrings("src/lib.zig", modules[0].root_source_file.?);
}

test "parseBuildSource extracts multiple modules" {
    // Arrange
    const allocator = std.testing.allocator;
    const source =
        \\const std = @import("std");
        \\pub fn build(b: *std.Build) void {
        \\    const mod_a = b.createModule(.{
        \\        .root_source_file = b.path("src/a.zig"),
        \\    });
        \\    const mod_b = b.createModule(.{
        \\        .root_source_file = b.path("src/b.zig"),
        \\    });
        \\    _ = mod_a;
        \\    _ = mod_b;
        \\}
    ;

    // Act
    const info = try parseBuildSource(allocator, source, Logger.noop);
    defer info.deinit(allocator);

    // Assert
    const modules = info.modules orelse return error.TestExpectedEqual;
    try std.testing.expectEqual(@as(usize, 2), modules.len);
}

test "parseBuildSource extracts addExecutable" {
    // Arrange
    const allocator = std.testing.allocator;
    const source =
        \\const std = @import("std");
        \\pub fn build(b: *std.Build) void {
        \\    const exe = b.addExecutable(.{
        \\        .name = "myapp",
        \\        .root_module = exe_mod,
        \\    });
        \\    _ = exe;
        \\}
    ;

    // Act
    const info = try parseBuildSource(allocator, source, Logger.noop);
    defer info.deinit(allocator);

    // Assert
    const tgts = info.targets orelse return error.TestExpectedEqual;
    try std.testing.expectEqual(@as(usize, 1), tgts.len);
    try std.testing.expectEqualStrings("myapp", tgts[0].name);
    try std.testing.expectEqual(TargetKind.executable, tgts[0].kind);
}

test "parseBuildSource extracts addLibrary" {
    // Arrange
    const allocator = std.testing.allocator;
    const source =
        \\const std = @import("std");
        \\pub fn build(b: *std.Build) void {
        \\    const lib = b.addLibrary(.{
        \\        .name = "mylib",
        \\        .root_module = lib_mod,
        \\    });
        \\    _ = lib;
        \\}
    ;

    // Act
    const info = try parseBuildSource(allocator, source, Logger.noop);
    defer info.deinit(allocator);

    // Assert
    const tgts = info.targets orelse return error.TestExpectedEqual;
    try std.testing.expectEqual(@as(usize, 1), tgts.len);
    try std.testing.expectEqualStrings("mylib", tgts[0].name);
    try std.testing.expectEqual(TargetKind.library, tgts[0].kind);
}

test "parseBuildSource extracts addTest" {
    // Arrange
    const allocator = std.testing.allocator;
    const source =
        \\const std = @import("std");
        \\pub fn build(b: *std.Build) void {
        \\    const t = b.addTest(.{
        \\        .name = "unit_tests",
        \\        .root_module = test_mod,
        \\    });
        \\    _ = t;
        \\}
    ;

    // Act
    const info = try parseBuildSource(allocator, source, Logger.noop);
    defer info.deinit(allocator);

    // Assert
    const tgts = info.targets orelse return error.TestExpectedEqual;
    try std.testing.expectEqual(@as(usize, 1), tgts.len);
    try std.testing.expectEqualStrings("unit_tests", tgts[0].name);
    try std.testing.expectEqual(TargetKind.test_target, tgts[0].kind);
}

test "parseBuildSource extracts dependency" {
    // Arrange
    const allocator = std.testing.allocator;
    const source =
        \\const std = @import("std");
        \\pub fn build(b: *std.Build) void {
        \\    const ts_dep = b.dependency("tree-sitter", .{});
        \\    _ = ts_dep;
        \\}
    ;

    // Act
    const info = try parseBuildSource(allocator, source, Logger.noop);
    defer info.deinit(allocator);

    // Assert
    const dependencies = info.dependencies orelse return error.TestExpectedEqual;
    try std.testing.expectEqual(@as(usize, 1), dependencies.len);
    try std.testing.expectEqualStrings("tree-sitter", dependencies[0].name);
}

test "parseBuildSource extracts addImport" {
    // Arrange
    const allocator = std.testing.allocator;
    const source =
        \\const std = @import("std");
        \\pub fn build(b: *std.Build) void {
        \\    const lib_mod = b.createModule(.{
        \\        .root_source_file = b.path("src/lib.zig"),
        \\    });
        \\    lib_mod.addImport("tree-sitter", ts_dep.module("tree_sitter"));
        \\}
    ;

    // Act
    const info = try parseBuildSource(allocator, source, Logger.noop);
    defer info.deinit(allocator);

    // Assert
    const modules = info.modules orelse return error.TestExpectedEqual;
    try std.testing.expectEqual(@as(usize, 1), modules.len);
    const imports = modules[0].import_names orelse return error.TestExpectedEqual;
    try std.testing.expectEqual(@as(usize, 1), imports.len);
    try std.testing.expectEqualStrings("tree-sitter", imports[0]);
}

test "extractDependencyUrls returns urls" {
    // Arrange
    const allocator = std.testing.allocator;
    const content =
        \\.{
        \\    .name = "test",
        \\    .dependencies = .{
        \\        .zlib = .{
        \\            .url = "https://example.com/zlib-0.1.0.tar.gz",
        \\            .hash = "abc",
        \\        },
        \\    },
        \\}
    ;

    // Act
    const urls = (try extractDependencyUrls(allocator, content)) orelse return error.TestExpectedEqual;
    defer {
        for (urls) |u| {
            allocator.free(u.name);
            allocator.free(u.url);
        }
        allocator.free(urls);
    }

    // Assert
    try std.testing.expectEqual(@as(usize, 1), urls.len);
    try std.testing.expectEqualStrings("zlib", urls[0].name);
    try std.testing.expectEqualStrings("https://example.com/zlib-0.1.0.tar.gz", urls[0].url);
}

test "extractDependencyUrls handles empty deps" {
    // Arrange
    const allocator = std.testing.allocator;
    const content =
        \\.{
        \\    .name = "test",
        \\    .dependencies = .{},
        \\}
    ;

    // Act
    const result = try extractDependencyUrls(allocator, content);

    // Assert
    try std.testing.expect(result == null);
}

test "extractDependencyUrls handles quoted names" {
    // Arrange
    const allocator = std.testing.allocator;
    const content =
        \\.{
        \\    .dependencies = .{
        \\        .@"tree-sitter" = .{
        \\            .url = "https://example.com/ts.tar.gz",
        \\            .hash = "xyz",
        \\        },
        \\    },
        \\}
    ;

    // Act
    const urls = (try extractDependencyUrls(allocator, content)) orelse return error.TestExpectedEqual;
    defer {
        for (urls) |u| {
            allocator.free(u.name);
            allocator.free(u.url);
        }
        allocator.free(urls);
    }

    // Assert
    try std.testing.expectEqual(@as(usize, 1), urls.len);
    try std.testing.expectEqualStrings("tree-sitter", urls[0].name);
}

test "parseBuildSource ignores commented-out calls" {
    // Arrange
    const allocator = std.testing.allocator;
    const source =
        \\const std = @import("std");
        \\pub fn build(b: *std.Build) void {
        \\    // const old = b.createModule(.{
        \\    //     .root_source_file = b.path("src/old.zig"),
        \\    // });
        \\    const real = b.createModule(.{
        \\        .root_source_file = b.path("src/real.zig"),
        \\    });
        \\    _ = real;
        \\}
    ;

    // Act
    const info = try parseBuildSource(allocator, source, Logger.noop);
    defer info.deinit(allocator);

    // Assert
    const modules = info.modules orelse return error.TestExpectedEqual;
    try std.testing.expectEqual(@as(usize, 1), modules.len);
    try std.testing.expectEqualStrings("real", modules[0].name);
}

test "BuildInfo.deinit frees all memory" {
    // Arrange
    const allocator = std.testing.allocator;
    const source =
        \\const std = @import("std");
        \\pub fn build(b: *std.Build) void {
        \\    const lib_mod = b.createModule(.{
        \\        .root_source_file = b.path("src/lib.zig"),
        \\    });
        \\    lib_mod.addImport("dep", ts_dep.module("ts"));
        \\    const ts_dep = b.dependency("tree-sitter", .{});
        \\    const exe = b.addExecutable(.{
        \\        .name = "app",
        \\        .root_module = lib_mod,
        \\    });
        \\    _ = exe;
        \\    _ = ts_dep;
        \\}
    ;

    // Act
    const info = try parseBuildSource(allocator, source, Logger.noop);

    // Assert: deinit must free everything; leak detector will catch any missed frees.
    info.deinit(allocator);
}
