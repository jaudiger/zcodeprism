const std = @import("std");
const zcodeprism = @import("zcodeprism");
const fixtures = @import("test-fixtures");

const Graph = zcodeprism.graph.Graph;
const Node = zcodeprism.node.Node;
const NodeId = zcodeprism.types.NodeId;
const NodeKind = zcodeprism.types.NodeKind;
const EdgeType = zcodeprism.types.EdgeType;
const EdgeSource = zcodeprism.types.EdgeSource;
const Visibility = zcodeprism.types.Visibility;
const Logger = zcodeprism.logging.Logger;
const parse = zcodeprism.visitor.parse;

// =========================================================================
// simple.zig fixture: edge creation
// =========================================================================

test "simple fixture: edge creation" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.simple, &g, Logger.noop);

    // Assert: at least one calls edge exists (manhattan calls abs)
    var found_calls = false;
    for (g.edges.items) |e| {
        if (e.edge_type == .calls) {
            found_calls = true;
            break;
        }
    }
    try std.testing.expect(found_calls);

    // Assert: isWithinRadius calls manhattan via self.manhattan()
    var iwr_id: ?NodeId = null;
    var manhattan_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "isWithinRadius")) {
            iwr_id = @enumFromInt(idx);
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "manhattan")) {
            manhattan_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(iwr_id != null);
    try std.testing.expect(manhattan_id != null);

    var found_self_call = false;
    for (g.edges.items) |e| {
        if (e.source_id == iwr_id.? and e.target_id == manhattan_id.? and e.edge_type == .calls) {
            found_self_call = true;
            break;
        }
    }
    try std.testing.expect(found_self_call);

    // Assert: at least one uses_type edge exists (defaultPoint uses Point)
    var found_uses_type = false;
    for (g.edges.items) |e| {
        if (e.edge_type == .uses_type) {
            found_uses_type = true;
            break;
        }
    }
    try std.testing.expect(found_uses_type);

    // Assert: every edge has source == .tree_sitter
    try std.testing.expect(g.edgeCount() > 0);
    for (g.edges.items) |e| {
        try std.testing.expectEqual(EdgeSource.tree_sitter, e.source);
    }

    // Assert: isWarm has a uses_type edge pointing to Color (enum method → parent enum)
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

    var found_enum_edge = false;
    for (g.edges.items) |e| {
        if (e.source_id == iswarm_id.? and e.target_id == color_id.? and e.edge_type == .uses_type) {
            found_enum_edge = true;
            break;
        }
    }
    try std.testing.expect(found_enum_edge);

    // Assert: test "point manhattan distance" has a uses_type edge to Point
    var test_id: ?NodeId = null;
    var point_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .test_def and std.mem.eql(u8, n.name, "point manhattan distance")) {
            test_id = @enumFromInt(idx);
        }
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Point")) {
            point_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(test_id != null);
    try std.testing.expect(point_id != null);

    var found_test_uses_type = false;
    for (g.edges.items) |e| {
        if (e.source_id == test_id.? and e.target_id == point_id.? and e.edge_type == .uses_type) {
            found_test_uses_type = true;
            break;
        }
    }
    try std.testing.expect(found_test_uses_type);
}

// =========================================================================
// file_struct.zig fixture: edge creation
// =========================================================================

test "file struct fixture: edge creation" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.file_struct, &g, Logger.noop);

    // Assert: isValid has a calls edge to validate (via self.validate())
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

    // Assert: Self constant is filtered; no Self node exists
    for (g.nodes.items) |n| {
        if (std.mem.eql(u8, n.name, "Self")) {
            try std.testing.expect(false); // Self should not exist
        }
    }

    // Assert: test "basic" still exists
    var basic_test_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .test_def and std.mem.eql(u8, n.name, "basic")) {
            basic_test_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(basic_test_id != null);
}

// =========================================================================
// generic_type.zig fixture: edge creation
// =========================================================================

test "generic type fixture: edge creation" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g, Logger.noop);

    // Assert: isEmpty calls count (via self.count())
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

    // Assert: no Self constants exist (all @This() aliases filtered)
    for (g.nodes.items) |n| {
        if (std.mem.eql(u8, n.name, "Self")) {
            try std.testing.expect(false); // Self should not exist
        }
    }

    // Assert: Container.init still exists
    var container_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Container")) {
            container_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(container_id != null);

    var init_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "init") and
            n.parent_id != null and n.parent_id.? == container_id.?)
        {
            init_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(init_id != null);

    // Assert: Container type_def does not have uses_type edge to own nested type Entry
    var entry_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Entry") and
            n.parent_id != null and n.parent_id.? == container_id.?)
        {
            entry_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(entry_id != null);

    var has_spurious_edge = false;
    for (g.edges.items) |e| {
        if (e.source_id == container_id.? and e.target_id == entry_id.? and e.edge_type == .uses_type) {
            has_spurious_edge = true;
            break;
        }
    }
    try std.testing.expect(!has_spurious_edge);
}

// =========================================================================
// Test block edge scanning
// =========================================================================

test "test block edges" {
    // --- test block creates calls edge to local function ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        const source =
            \\fn helper() i32 { return 42; }
            \\test "uses helper" { _ = helper(); }
        ;
        try parse(source, &g, Logger.noop);

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

    // --- test block creates calls edge to method via dot syntax ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        const source =
            \\const Foo = struct {
            \\    pub fn bar() void {}
            \\};
            \\test "calls bar" { Foo.bar(); }
        ;
        try parse(source, &g, Logger.noop);

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

    // --- test block with no local calls has no edges ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        const source =
            \\test "standalone" {
            \\    const x: i32 = 42;
            \\    _ = x;
            \\}
        ;
        try parse(source, &g, Logger.noop);

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

    // --- decl-reference test edges are attributed to correct node ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        const source =
            \\fn alpha() i32 { return 1; }
            \\fn beta() i32 { return 2; }
            \\test alpha { _ = alpha(); }
            \\test beta { _ = beta(); }
        ;
        try parse(source, &g, Logger.noop);

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
}

// =========================================================================
// Nested scope isolation
// =========================================================================

test "nested scope isolation" {
    // --- test block does not leak calls from nested function ---
    {
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
        try parse(source, &g, Logger.noop);

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

    // --- function does not leak calls from nested function ---
    {
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
        try parse(source, &g, Logger.noop);

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

    // --- nested function gets its own calls edge not parent's ---
    {
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
        try parse(source, &g, Logger.noop);

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

    // --- function does not leak uses_type from nested function ---
    {
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
        try parse(source, &g, Logger.noop);

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
}

// =========================================================================
// Type alias uses_type edges
// =========================================================================

test "type alias edges" {
    // --- uses_type edge for type alias in parameter ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        const source =
            \\const Foo = struct { val: i32 };
            \\const Bar = Foo;
            \\fn useBar(b: Bar) void { _ = b; }
        ;
        try parse(source, &g, Logger.noop);

        var useBar_id: ?NodeId = null;
        var bar_id: ?NodeId = null;
        for (g.nodes.items, 0..) |n, idx| {
            if (n.kind == .function and std.mem.eql(u8, n.name, "useBar")) useBar_id = @enumFromInt(idx);
            if (std.mem.eql(u8, n.name, "Bar")) bar_id = @enumFromInt(idx);
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

    // --- uses_type edge for type alias in return type ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        const source =
            \\const MyError = error{ Oops, Bad };
            \\const Err = MyError;
            \\fn doStuff() Err!void {}
        ;
        try parse(source, &g, Logger.noop);

        var doStuff_id: ?NodeId = null;
        var err_id: ?NodeId = null;
        for (g.nodes.items, 0..) |n, idx| {
            if (n.kind == .function and std.mem.eql(u8, n.name, "doStuff")) doStuff_id = @enumFromInt(idx);
            if (std.mem.eql(u8, n.name, "Err")) err_id = @enumFromInt(idx);
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

    // --- uses_type edge not created for non-type constant ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        const source =
            \\const Limit = struct {};
            \\const max = 100;
            \\fn process(l: Limit) void { _ = l; }
        ;
        try parse(source, &g, Logger.noop);

        var process_id: ?NodeId = null;
        var limit_id: ?NodeId = null;
        var max_id: ?NodeId = null;
        for (g.nodes.items, 0..) |n, idx| {
            if (n.kind == .function and std.mem.eql(u8, n.name, "process")) process_id = @enumFromInt(idx);
            if (std.mem.eql(u8, n.name, "Limit")) limit_id = @enumFromInt(idx);
            if (std.mem.eql(u8, n.name, "max")) max_id = @enumFromInt(idx);
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

    // --- uses_type edge for aliased type works alongside direct type ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        const source =
            \\const Direct = struct {};
            \\const Other = struct {};
            \\const Alias = Other;
            \\fn both(d: Direct, a: Alias) void { _ = d; _ = a; }
        ;
        try parse(source, &g, Logger.noop);

        var both_id: ?NodeId = null;
        var direct_id: ?NodeId = null;
        var alias_id: ?NodeId = null;
        for (g.nodes.items, 0..) |n, idx| {
            if (n.kind == .function and std.mem.eql(u8, n.name, "both")) both_id = @enumFromInt(idx);
            if (std.mem.eql(u8, n.name, "Direct")) direct_id = @enumFromInt(idx);
            if (std.mem.eql(u8, n.name, "Alias")) alias_id = @enumFromInt(idx);
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
}

// =========================================================================
// Advanced uses_type edges (body-level type references)
// =========================================================================

test "advanced uses_type edges" {
    // --- struct literal construction in body ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        const source =
            \\const Config = struct { max: i32 = 0 };
            \\fn makeDefault() void {
            \\    const c = Config{ .max = 42 };
            \\    _ = c;
            \\}
        ;
        try parse(source, &g, Logger.noop);

        var makeDefault_id: ?NodeId = null;
        var config_id: ?NodeId = null;
        for (g.nodes.items, 0..) |n, idx| {
            if (n.kind == .function and std.mem.eql(u8, n.name, "makeDefault")) makeDefault_id = @enumFromInt(idx);
            if (n.kind == .type_def and std.mem.eql(u8, n.name, "Config")) config_id = @enumFromInt(idx);
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

    // --- static method call in body ---
    {
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
        try parse(source, &g, Logger.noop);

        var create_id: ?NodeId = null;
        var builder_id: ?NodeId = null;
        for (g.nodes.items, 0..) |n, idx| {
            if (n.kind == .function and std.mem.eql(u8, n.name, "create")) create_id = @enumFromInt(idx);
            if (n.kind == .type_def and std.mem.eql(u8, n.name, "Builder")) builder_id = @enumFromInt(idx);
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

    // --- no duplicate uses_type edge when type in both signature and body ---
    {
        var g = Graph.init(std.testing.allocator, "/tmp/project");
        defer g.deinit();
        const source =
            \\const Item = struct { id: i32 = 0 };
            \\fn process(item: Item) Item {
            \\    return Item{ .id = item.id + 1 };
            \\}
        ;
        try parse(source, &g, Logger.noop);

        var process_id: ?NodeId = null;
        var item_id: ?NodeId = null;
        for (g.nodes.items, 0..) |n, idx| {
            if (n.kind == .function and std.mem.eql(u8, n.name, "process")) process_id = @enumFromInt(idx);
            if (n.kind == .type_def and std.mem.eql(u8, n.name, "Item")) item_id = @enumFromInt(idx);
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

    // --- type passed as comptime argument ---
    {
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
        try parse(source, &g, Logger.noop);

        var doWork_id: ?NodeId = null;
        var payload_id: ?NodeId = null;
        for (g.nodes.items, 0..) |n, idx| {
            if (n.kind == .function and std.mem.eql(u8, n.name, "doWork")) doWork_id = @enumFromInt(idx);
            if (n.kind == .type_def and std.mem.eql(u8, n.name, "Payload")) payload_id = @enumFromInt(idx);
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

    // --- type in generic container instantiation ---
    {
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
        try parse(source, &g, Logger.noop);

        var build_id: ?NodeId = null;
        var element_id: ?NodeId = null;
        for (g.nodes.items, 0..) |n, idx| {
            if (n.kind == .function and std.mem.eql(u8, n.name, "build")) build_id = @enumFromInt(idx);
            if (n.kind == .type_def and std.mem.eql(u8, n.name, "Element")) element_id = @enumFromInt(idx);
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

    // --- no uses_type edge for non-type identifier argument ---
    {
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
        try parse(source, &g, Logger.noop);

        var run_id: ?NodeId = null;
        var helper_id: ?NodeId = null;
        var max_id: ?NodeId = null;
        for (g.nodes.items, 0..) |n, idx| {
            if (n.kind == .function and std.mem.eql(u8, n.name, "run")) run_id = @enumFromInt(idx);
            if (n.kind == .function and std.mem.eql(u8, n.name, "helper")) helper_id = @enumFromInt(idx);
            if (n.kind == .constant and std.mem.eql(u8, n.name, "max")) max_id = @enumFromInt(idx);
        }
        try std.testing.expect(run_id != null);
        try std.testing.expect(helper_id != null);
        try std.testing.expect(max_id != null);

        var found_max_edge = false;
        for (g.edges.items) |e| {
            if (e.source_id == run_id.? and e.target_id == max_id.? and e.edge_type == .uses_type) {
                found_max_edge = true;
                break;
            }
        }
        try std.testing.expect(!found_max_edge);

        var found_calls = false;
        for (g.edges.items) |e| {
            if (e.source_id == run_id.? and e.target_id == helper_id.? and e.edge_type == .calls) {
                found_calls = true;
                break;
            }
        }
        try std.testing.expect(found_calls);
    }
}

// =========================================================================
// local_type_param.zig fixture: local-type parameter edges
// =========================================================================

test "local-type parameter edges" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.local_type_param, &g, Logger.noop);

    // Assert: processPoint has a calls edge to manhattan
    var processPoint_id: ?NodeId = null;
    var manhattan_id: ?NodeId = null;
    var multiParam_id: ?NodeId = null;
    var pointerParam_id: ?NodeId = null;
    var externalParam_id: ?NodeId = null;
    var scale_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "processPoint")) processPoint_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "manhattan")) manhattan_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "multiParam")) multiParam_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "pointerParam")) pointerParam_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "externalParam")) externalParam_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "scale")) scale_id = @enumFromInt(idx);
    }
    try std.testing.expect(processPoint_id != null);
    try std.testing.expect(manhattan_id != null);
    try std.testing.expect(multiParam_id != null);
    try std.testing.expect(pointerParam_id != null);
    try std.testing.expect(externalParam_id != null);
    try std.testing.expect(scale_id != null);

    // processPoint calls manhattan
    var found_pp = false;
    for (g.edges.items) |e| {
        if (e.source_id == processPoint_id.? and e.target_id == manhattan_id.? and e.edge_type == .calls) {
            found_pp = true;
            break;
        }
    }
    try std.testing.expect(found_pp);

    // multiParam calls manhattan
    var found_mp = false;
    for (g.edges.items) |e| {
        if (e.source_id == multiParam_id.? and e.target_id == manhattan_id.? and e.edge_type == .calls) {
            found_mp = true;
            break;
        }
    }
    try std.testing.expect(found_mp);

    // pointerParam does NOT call scale (pointer type not resolved locally)
    var found_ptr = false;
    for (g.edges.items) |e| {
        if (e.source_id == pointerParam_id.? and e.target_id == scale_id.? and e.edge_type == .calls) {
            found_ptr = true;
            break;
        }
    }
    try std.testing.expect(!found_ptr);

    // externalParam has no calls edges at all
    var found_ext = false;
    for (g.edges.items) |e| {
        if (e.source_id == externalParam_id.? and e.edge_type == .calls) {
            found_ext = true;
            break;
        }
    }
    try std.testing.expect(!found_ext);
}

// =========================================================================
// duplicate_method_names.zig fixture: scope resolution
// =========================================================================

test "duplicate method names: scope resolution" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.duplicate_method_names, &g, Logger.noop);

    // Assert: @This() aliases are filtered -- no Self constants exist
    var self_count: usize = 0;
    for (g.nodes.items) |n| {
        if (n.kind == .constant and std.mem.eql(u8, n.name, "Self")) {
            self_count += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 0), self_count);

    // Find Alpha and Beta type_defs
    var alpha_id: ?NodeId = null;
    var beta_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Alpha")) alpha_id = @enumFromInt(idx);
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Beta")) beta_id = @enumFromInt(idx);
    }
    try std.testing.expect(alpha_id != null);
    try std.testing.expect(beta_id != null);

    // No Self constant child in Alpha
    var found_alpha_self = false;
    for (g.nodes.items) |n| {
        if (std.mem.eql(u8, n.name, "Self") and n.parent_id != null and n.parent_id.? == alpha_id.?) {
            found_alpha_self = true;
            break;
        }
    }
    try std.testing.expect(!found_alpha_self);

    // No Self constant child in Beta
    var found_beta_self = false;
    for (g.nodes.items) |n| {
        if (std.mem.eql(u8, n.name, "Self") and n.parent_id != null and n.parent_id.? == beta_id.?) {
            found_beta_self = true;
            break;
        }
    }
    try std.testing.expect(!found_beta_self);

    // Find deinit methods and reset/clear
    var alpha_deinit_id: ?NodeId = null;
    var beta_deinit_id: ?NodeId = null;
    var reset_id: ?NodeId = null;
    var clear_id: ?NodeId = null;
    var alpha_init_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "deinit")) {
            if (n.parent_id != null and n.parent_id.? == alpha_id.?) alpha_deinit_id = @enumFromInt(idx);
            if (n.parent_id != null and n.parent_id.? == beta_id.?) beta_deinit_id = @enumFromInt(idx);
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "reset")) reset_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "clear")) clear_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "init") and
            n.parent_id != null and n.parent_id.? == alpha_id.?)
        {
            alpha_init_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(alpha_deinit_id != null);
    try std.testing.expect(beta_deinit_id != null);
    try std.testing.expect(reset_id != null);
    try std.testing.expect(clear_id != null);
    try std.testing.expect(alpha_init_id != null);

    // Alpha.reset calls Alpha.deinit not Beta.deinit
    var reset_calls_alpha = false;
    var reset_calls_beta = false;
    for (g.edges.items) |e| {
        if (e.source_id == reset_id.? and e.edge_type == .calls) {
            if (e.target_id == alpha_deinit_id.?) reset_calls_alpha = true;
            if (e.target_id == beta_deinit_id.?) reset_calls_beta = true;
        }
    }
    try std.testing.expect(reset_calls_alpha);
    try std.testing.expect(!reset_calls_beta);

    // Beta.clear calls Beta.deinit not Alpha.deinit
    var clear_calls_alpha = false;
    var clear_calls_beta = false;
    for (g.edges.items) |e| {
        if (e.source_id == clear_id.? and e.edge_type == .calls) {
            if (e.target_id == alpha_deinit_id.?) clear_calls_alpha = true;
            if (e.target_id == beta_deinit_id.?) clear_calls_beta = true;
        }
    }
    try std.testing.expect(clear_calls_beta);
    try std.testing.expect(!clear_calls_alpha);

    // Alpha.init's uses_type edges should not target any Beta-scoped node
    for (g.edges.items) |e| {
        if (e.source_id == alpha_init_id.? and e.edge_type == .uses_type) {
            try std.testing.expect(e.target_id != beta_id.?);
            const target = g.getNode(e.target_id);
            if (target) |t| {
                if (t.parent_id) |pid| {
                    try std.testing.expect(pid != beta_id.?);
                }
            }
        }
    }
}

// =========================================================================
// external_method_collision.zig fixture: no false edges
// =========================================================================

test "external method collision: no false edges" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.external_method_collision, &g, Logger.noop);

    // Find Resource struct and its methods
    var resource_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Resource")) {
            resource_id = @enumFromInt(idx);
            break;
        }
    }
    try std.testing.expect(resource_id != null);

    var resource_init_id: ?NodeId = null;
    var resource_deinit_id: ?NodeId = null;
    var external_init_id: ?NodeId = null;
    var external_deinit_id: ?NodeId = null;
    var create_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "init") and
            n.parent_id != null and n.parent_id.? == resource_id.?)
        {
            resource_init_id = @enumFromInt(idx);
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "deinit") and
            n.parent_id != null and n.parent_id.? == resource_id.?)
        {
            resource_deinit_id = @enumFromInt(idx);
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "externalInit")) external_init_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "externalDeinit")) external_deinit_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "createLocalResource")) create_id = @enumFromInt(idx);
    }
    try std.testing.expect(resource_init_id != null);
    try std.testing.expect(resource_deinit_id != null);
    try std.testing.expect(external_init_id != null);
    try std.testing.expect(external_deinit_id != null);
    try std.testing.expect(create_id != null);

    // externalInit should NOT have a calls edge to Resource.init
    var found_false_init = false;
    for (g.edges.items) |e| {
        if (e.source_id == external_init_id.? and e.target_id == resource_init_id.? and e.edge_type == .calls) {
            found_false_init = true;
            break;
        }
    }
    try std.testing.expect(!found_false_init);

    // externalDeinit should NOT have a calls edge to Resource.deinit
    var found_false_deinit = false;
    for (g.edges.items) |e| {
        if (e.source_id == external_deinit_id.? and e.target_id == resource_deinit_id.? and e.edge_type == .calls) {
            found_false_deinit = true;
            break;
        }
    }
    try std.testing.expect(!found_false_deinit);

    // createLocalResource SHOULD have a calls edge to Resource.init
    var found_genuine = false;
    for (g.edges.items) |e| {
        if (e.source_id == create_id.? and e.target_id == resource_init_id.? and e.edge_type == .calls) {
            found_genuine = true;
            break;
        }
    }
    try std.testing.expect(found_genuine);
}

// =========================================================================
// generic_dual_self.zig fixture: Self filtering and resolution
// =========================================================================

test "generic dual self: Self filtering and resolution" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.edge_cases.generic_dual_self, &g, Logger.noop);

    // Assert: no Self constants exist
    var self_count: usize = 0;
    for (g.nodes.items) |n| {
        if (n.kind == .constant and std.mem.eql(u8, n.name, "Self")) {
            self_count += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 0), self_count);

    // Find Stack and Queue type_defs
    var stack_id: ?NodeId = null;
    var queue_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Stack")) stack_id = @enumFromInt(idx);
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Queue")) queue_id = @enumFromInt(idx);
    }
    try std.testing.expect(stack_id != null);
    try std.testing.expect(queue_id != null);

    var stack_deinit_id: ?NodeId = null;
    var queue_deinit_id: ?NodeId = null;
    var clear_id: ?NodeId = null;
    var flush_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "deinit")) {
            if (n.parent_id != null and n.parent_id.? == stack_id.?) stack_deinit_id = @enumFromInt(idx);
            if (n.parent_id != null and n.parent_id.? == queue_id.?) queue_deinit_id = @enumFromInt(idx);
        }
        if (n.kind == .function and std.mem.eql(u8, n.name, "clear")) clear_id = @enumFromInt(idx);
        if (n.kind == .function and std.mem.eql(u8, n.name, "flush")) flush_id = @enumFromInt(idx);
    }
    try std.testing.expect(stack_deinit_id != null);
    try std.testing.expect(queue_deinit_id != null);
    try std.testing.expect(clear_id != null);
    try std.testing.expect(flush_id != null);

    // Stack.clear calls Stack.deinit not Queue.deinit
    var clear_calls_stack = false;
    var clear_calls_queue = false;
    for (g.edges.items) |e| {
        if (e.source_id == clear_id.? and e.edge_type == .calls) {
            if (e.target_id == stack_deinit_id.?) clear_calls_stack = true;
            if (e.target_id == queue_deinit_id.?) clear_calls_queue = true;
        }
    }
    try std.testing.expect(clear_calls_stack);
    try std.testing.expect(!clear_calls_queue);

    // Queue.flush calls Queue.deinit not Stack.deinit
    var flush_calls_stack = false;
    var flush_calls_queue = false;
    for (g.edges.items) |e| {
        if (e.source_id == flush_id.? and e.edge_type == .calls) {
            if (e.target_id == stack_deinit_id.?) flush_calls_stack = true;
            if (e.target_id == queue_deinit_id.?) flush_calls_queue = true;
        }
    }
    try std.testing.expect(flush_calls_queue);
    try std.testing.expect(!flush_calls_stack);

    // No self-referencing uses_type edges for Stack or Queue
    var stack_self_loop = false;
    var queue_self_loop = false;
    for (g.edges.items) |e| {
        if (e.edge_type == .uses_type) {
            if (e.source_id == stack_id.? and e.target_id == stack_id.?) stack_self_loop = true;
            if (e.source_id == queue_id.? and e.target_id == queue_id.?) queue_self_loop = true;
        }
    }
    try std.testing.expect(!stack_self_loop);
    try std.testing.expect(!queue_self_loop);
}

// =========================================================================
// generic_type.zig fixture: self-reference prevention
// =========================================================================

test "generic type: self-reference prevention" {
    // Arrange
    var g = Graph.init(std.testing.allocator, "/tmp/project");
    defer g.deinit();

    // Act
    try parse(fixtures.zig.generic_type, &g, Logger.noop);

    // Assert: no Self constants exist
    for (g.nodes.items) |n| {
        if (std.mem.eql(u8, n.name, "Self")) {
            try std.testing.expect(false);
        }
    }

    // Assert: Container has no self-referencing uses_type edge
    var container_id: ?NodeId = null;
    var result_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Container")) container_id = @enumFromInt(idx);
        if (n.kind == .type_def and std.mem.eql(u8, n.name, "Result")) result_id = @enumFromInt(idx);
    }
    try std.testing.expect(container_id != null);
    try std.testing.expect(result_id != null);

    var container_self_loop = false;
    var result_self_loop = false;
    for (g.edges.items) |e| {
        if (e.edge_type == .uses_type) {
            if (e.source_id == container_id.? and e.target_id == container_id.?) container_self_loop = true;
            if (e.source_id == result_id.? and e.target_id == result_id.?) result_self_loop = true;
        }
    }
    try std.testing.expect(!container_self_loop);
    try std.testing.expect(!result_self_loop);
}
