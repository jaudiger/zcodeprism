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
const LangMeta = zcodeprism.language.LangMeta;
const Logger = zcodeprism.logging.Logger;
const parse = zcodeprism.visitor.parse;

test "simple fixture: edge creation" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.simple, &g, null, Logger.noop);

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

    // Assert: isWarm has a uses_type edge pointing to Color (enum method -> parent enum)
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

test "file struct fixture: edge creation" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.file_struct, &g, null, Logger.noop);

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

    // Assert: test "basic" still exists
    var basic_test_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .test_def and std.mem.eql(u8, n.name, "basic")) {
            basic_test_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(basic_test_id != null);
}

test "generic type fixture: edge creation" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.generic_type, &g, null, Logger.noop);

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

test "test block edges" {
    // test block creates calls edge to local function
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\fn helper() i32 { return 42; }
            \\test "uses helper" { _ = helper(); }
        ;
        try parse(std.testing.allocator, source, &g, null, Logger.noop);

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

    // test block creates calls edge to method via dot syntax
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\const Foo = struct {
            \\    pub fn bar() void {}
            \\};
            \\test "calls bar" { Foo.bar(); }
        ;
        try parse(std.testing.allocator, source, &g, null, Logger.noop);

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

    // test block with no local calls has no edges
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\test "standalone" {
            \\    const x: i32 = 42;
            \\    _ = x;
            \\}
        ;
        try parse(std.testing.allocator, source, &g, null, Logger.noop);

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

    // decl-reference test edges are attributed to correct node
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\fn alpha() i32 { return 1; }
            \\fn beta() i32 { return 2; }
            \\test alpha { _ = alpha(); }
            \\test beta { _ = beta(); }
        ;
        try parse(std.testing.allocator, source, &g, null, Logger.noop);

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

test "nested scope isolation" {
    // test block does not leak calls from nested function
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\fn helper() void {}
            \\test "outer" {
            \\    const S = struct {
            \\        fn inner() void { helper(); }
            \\    };
            \\    S.inner();
            \\}
        ;
        try parse(std.testing.allocator, source, &g, null, Logger.noop);

        var test_id: ?NodeId = null;
        var helper_id: ?NodeId = null;
        var inner_id: ?NodeId = null;
        for (g.nodes.items, 0..) |n, idx| {
            if (n.kind == .test_def and std.mem.eql(u8, n.name, "outer")) test_id = @enumFromInt(idx);
            if (n.kind == .function and std.mem.eql(u8, n.name, "helper")) helper_id = @enumFromInt(idx);
            if (n.kind == .function and std.mem.eql(u8, n.name, "inner")) inner_id = @enumFromInt(idx);
        }
        try std.testing.expect(test_id != null);
        try std.testing.expect(helper_id != null);
        try std.testing.expect(inner_id != null);

        // test "outer" does NOT have a leaked calls edge to helper
        var found_leak = false;
        for (g.edges.items) |e| {
            if (e.source_id == test_id.? and e.target_id == helper_id.? and e.edge_type == .calls) {
                found_leak = true;
                break;
            }
        }
        try std.testing.expect(!found_leak);

        // inner -> helper calls edge exists (inner function's own edge)
        var found_inner_calls = false;
        for (g.edges.items) |e| {
            if (e.source_id == inner_id.? and e.target_id == helper_id.? and e.edge_type == .calls) {
                found_inner_calls = true;
                break;
            }
        }
        try std.testing.expect(found_inner_calls);

        // S type_def node exists with parent = test_def
        var found_s = false;
        for (g.nodes.items) |n| {
            if (n.kind == .type_def and std.mem.eql(u8, n.name, "S") and
                n.parent_id != null and n.parent_id.? == test_id.?)
            {
                found_s = true;
                break;
            }
        }
        try std.testing.expect(found_s);
    }

    // function does not leak calls from nested function
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\fn target() void {}
            \\fn outer() void {
            \\    const S = struct {
            \\        fn nested() void { target(); }
            \\    };
            \\    S.nested();
            \\}
        ;
        try parse(std.testing.allocator, source, &g, null, Logger.noop);

        var outer_id: ?NodeId = null;
        var target_id: ?NodeId = null;
        var nested_id: ?NodeId = null;
        for (g.nodes.items, 0..) |n, idx| {
            if (n.kind == .function and std.mem.eql(u8, n.name, "outer")) outer_id = @enumFromInt(idx);
            if (n.kind == .function and std.mem.eql(u8, n.name, "target")) target_id = @enumFromInt(idx);
            if (n.kind == .function and std.mem.eql(u8, n.name, "nested")) nested_id = @enumFromInt(idx);
        }
        try std.testing.expect(outer_id != null);
        try std.testing.expect(target_id != null);
        try std.testing.expect(nested_id != null);

        // outer does NOT have a leaked calls edge to target
        var found_leak = false;
        for (g.edges.items) |e| {
            if (e.source_id == outer_id.? and e.target_id == target_id.? and e.edge_type == .calls) {
                found_leak = true;
                break;
            }
        }
        try std.testing.expect(!found_leak);

        // nested -> target calls edge exists
        var found_nested_calls = false;
        for (g.edges.items) |e| {
            if (e.source_id == nested_id.? and e.target_id == target_id.? and e.edge_type == .calls) {
                found_nested_calls = true;
                break;
            }
        }
        try std.testing.expect(found_nested_calls);
    }

    // nested function gets its own calls edge not parent's
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
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
        try parse(std.testing.allocator, source, &g, null, Logger.noop);

        var parent_id: ?NodeId = null;
        var child_id: ?NodeId = null;
        var alpha_id: ?NodeId = null;
        var beta_id: ?NodeId = null;
        for (g.nodes.items, 0..) |n, idx| {
            if (n.kind == .function and std.mem.eql(u8, n.name, "parent")) parent_id = @enumFromInt(idx);
            if (n.kind == .function and std.mem.eql(u8, n.name, "child")) child_id = @enumFromInt(idx);
            if (n.kind == .function and std.mem.eql(u8, n.name, "alpha")) alpha_id = @enumFromInt(idx);
            if (n.kind == .function and std.mem.eql(u8, n.name, "beta")) beta_id = @enumFromInt(idx);
        }
        try std.testing.expect(parent_id != null);
        try std.testing.expect(child_id != null);
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

        // child -> beta calls edge exists
        var found_child_calls = false;
        for (g.edges.items) |e| {
            if (e.source_id == child_id.? and e.target_id == beta_id.? and e.edge_type == .calls) {
                found_child_calls = true;
                break;
            }
        }
        try std.testing.expect(found_child_calls);
    }

    // function does not leak uses_type from nested function
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\const MyType = struct {};
            \\fn outer() void {
            \\    const S = struct {
            \\        fn nested(x: MyType) void { _ = x; }
            \\    };
            \\    _ = S.nested;
            \\}
        ;
        try parse(std.testing.allocator, source, &g, null, Logger.noop);

        var outer_id: ?NodeId = null;
        var nested_id: ?NodeId = null;
        var mytype_id: ?NodeId = null;
        for (g.nodes.items, 0..) |n, idx| {
            if (n.kind == .function and std.mem.eql(u8, n.name, "outer")) outer_id = @enumFromInt(idx);
            if (n.kind == .function and std.mem.eql(u8, n.name, "nested")) nested_id = @enumFromInt(idx);
            if (std.mem.eql(u8, n.name, "MyType")) mytype_id = @enumFromInt(idx);
        }
        try std.testing.expect(outer_id != null);
        try std.testing.expect(nested_id != null);
        try std.testing.expect(mytype_id != null);

        // outer does NOT have a leaked uses_type edge to MyType
        var found_leak = false;
        for (g.edges.items) |e| {
            if (e.source_id == outer_id.? and e.target_id == mytype_id.? and e.edge_type == .uses_type) {
                found_leak = true;
                break;
            }
        }
        try std.testing.expect(!found_leak);

        // nested -> MyType uses_type edge exists (inner function's own type reference)
        var found_nested_uses = false;
        for (g.edges.items) |e| {
            if (e.source_id == nested_id.? and e.target_id == mytype_id.? and e.edge_type == .uses_type) {
                found_nested_uses = true;
                break;
            }
        }
        try std.testing.expect(found_nested_uses);
    }
}

test "type alias edges" {
    // uses_type edge for type alias in parameter
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\const Foo = struct { val: i32 };
            \\const Bar = Foo;
            \\fn useBar(b: Bar) void { _ = b; }
        ;
        try parse(std.testing.allocator, source, &g, null, Logger.noop);

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

    // uses_type edge for type alias in return type
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\const MyError = error{ Oops, Bad };
            \\const Err = MyError;
            \\fn doStuff() Err!void {}
        ;
        try parse(std.testing.allocator, source, &g, null, Logger.noop);

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

    // uses_type edge not created for non-type constant
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\const Limit = struct {};
            \\const max = 100;
            \\fn process(l: Limit) void { _ = l; }
        ;
        try parse(std.testing.allocator, source, &g, null, Logger.noop);

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

    // uses_type edge for aliased type works alongside direct type
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\const Direct = struct {};
            \\const Other = struct {};
            \\const Alias = Other;
            \\fn both(d: Direct, a: Alias) void { _ = d; _ = a; }
        ;
        try parse(std.testing.allocator, source, &g, null, Logger.noop);

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

test "advanced uses_type edges" {
    // struct literal construction in body
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\const Config = struct { max: i32 = 0 };
            \\fn makeDefault() void {
            \\    const c = Config{ .max = 42 };
            \\    _ = c;
            \\}
        ;
        try parse(std.testing.allocator, source, &g, null, Logger.noop);

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

    // static method call in body
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
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
        try parse(std.testing.allocator, source, &g, null, Logger.noop);

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

    // no duplicate uses_type edge when type in both signature and body
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\const Item = struct { id: i32 = 0 };
            \\fn process(item: Item) Item {
            \\    return Item{ .id = item.id + 1 };
            \\}
        ;
        try parse(std.testing.allocator, source, &g, null, Logger.noop);

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

    // type passed as comptime argument
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\const Payload = struct { data: i32 = 0 };
            \\fn serialize(comptime T: type, val: T) void { _ = val; }
            \\fn doWork() void {
            \\    const p = Payload{ .data = 1 };
            \\    serialize(Payload, p);
            \\}
        ;
        try parse(std.testing.allocator, source, &g, null, Logger.noop);

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

    // type in generic container instantiation
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\const Element = struct { val: i32 = 0 };
            \\fn Container(comptime T: type) type {
            \\    return struct { item: T };
            \\}
            \\fn build() void {
            \\    _ = Container(Element);
            \\}
        ;
        try parse(std.testing.allocator, source, &g, null, Logger.noop);

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

    // no uses_type edge for non-type identifier argument
    {
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\const Config = struct {};
            \\const max = 100;
            \\fn helper(n: i32) void { _ = n; }
            \\fn run() void {
            \\    helper(max);
            \\}
        ;
        try parse(std.testing.allocator, source, &g, null, Logger.noop);

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

test "local-type parameter edges" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.edge_cases.local_type_param, &g, null, Logger.noop);

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

test "duplicate method names: scope resolution" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.edge_cases.duplicate_method_names, &g, null, Logger.noop);

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

test "external method collision: no false edges" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.edge_cases.external_method_collision, &g, null, Logger.noop);

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

test "generic dual self: Self filtering and resolution" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.edge_cases.generic_dual_self, &g, null, Logger.noop);

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

test "generic type: self-reference prevention" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);

    // Act
    try parse(std.testing.allocator, fixtures.zig.generic_type, &g, null, Logger.noop);

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
        if (n.kind == .union_def and std.mem.eql(u8, n.name, "Result")) result_id = @enumFromInt(idx);
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

test "union classification: union_def distinct from type_def" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);
    const source =
        \\const MyStruct = struct { x: i32 };
        \\const TaggedUnion = union(enum) { int: i32, none };
        \\const PlainUnion = union { int: i32, float: f64 };
        \\const MyEnum = enum { a, b };
    ;

    // Act
    try parse(std.testing.allocator, source, &g, null, Logger.noop);

    // Assert: struct -> type_def, union -> union_def, enum -> enum_def
    var struct_node: ?*const Node = null;
    var tagged_node: ?*const Node = null;
    var plain_node: ?*const Node = null;
    var enum_node: ?*const Node = null;
    for (g.nodes.items) |*n| {
        if (std.mem.eql(u8, n.name, "MyStruct")) struct_node = n;
        if (std.mem.eql(u8, n.name, "TaggedUnion")) tagged_node = n;
        if (std.mem.eql(u8, n.name, "PlainUnion")) plain_node = n;
        if (std.mem.eql(u8, n.name, "MyEnum")) enum_node = n;
    }
    try std.testing.expect(struct_node != null);
    try std.testing.expect(tagged_node != null);
    try std.testing.expect(plain_node != null);
    try std.testing.expect(enum_node != null);

    try std.testing.expectEqual(NodeKind.type_def, struct_node.?.kind);
    try std.testing.expectEqual(NodeKind.union_def, tagged_node.?.kind);
    try std.testing.expectEqual(NodeKind.union_def, plain_node.?.kind);
    try std.testing.expectEqual(NodeKind.enum_def, enum_node.?.kind);

    // Assert: union fields are children of the union_def node
    var tagged_field_count: usize = 0;
    for (g.nodes.items) |n| {
        if (n.kind == .field and n.parent_id != null and n.parent_id.? == tagged_node.?.id) {
            tagged_field_count += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 2), tagged_field_count);
}

test "uses_type edges can target union_def nodes" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);
    const source =
        \\const Value = union(enum) { int: i32, none };
        \\fn process(v: Value) void { _ = v; }
    ;

    // Act
    try parse(std.testing.allocator, source, &g, null, Logger.noop);

    // Assert
    var fn_id: ?NodeId = null;
    var union_id: ?NodeId = null;
    for (g.nodes.items, 0..) |n, idx| {
        if (n.kind == .function and std.mem.eql(u8, n.name, "process")) fn_id = @enumFromInt(idx);
        if (n.kind == .union_def and std.mem.eql(u8, n.name, "Value")) union_id = @enumFromInt(idx);
    }
    try std.testing.expect(fn_id != null);
    try std.testing.expect(union_id != null);

    var found = false;
    for (g.edges.items) |e| {
        if (e.source_id == fn_id.? and e.target_id == union_id.? and e.edge_type == .uses_type) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "container layout qualifiers: packed and extern detected on structs and unions" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);
    const source =
        \\const Normal = struct { x: i32 };
        \\const Packed = packed struct { x: i32, y: u8 };
        \\const PackedBacked = packed struct(u32) { a: u16, b: u16 };
        \\const Ext = extern struct { x: c_int };
        \\const PackedUnion = packed union { int: i32, float: f32 };
        \\const ExternUnion = extern union { int: c_int, float: c_int };
        \\const NormalUnion = union { int: i32, float: f64 };
        \\pub fn GenPacked(comptime T: type) type {
        \\    return packed struct { val: T };
        \\}
    ;

    // Act
    try parse(std.testing.allocator, source, &g, null, Logger.noop);

    // Assert: collect all named nodes
    var normal: ?*const Node = null;
    var packed_s: ?*const Node = null;
    var packed_backed: ?*const Node = null;
    var ext_s: ?*const Node = null;
    var packed_u: ?*const Node = null;
    var ext_u: ?*const Node = null;
    var normal_u: ?*const Node = null;
    var gen_packed: ?*const Node = null;
    for (g.nodes.items) |*n| {
        if (std.mem.eql(u8, n.name, "Normal")) normal = n;
        if (std.mem.eql(u8, n.name, "Packed")) packed_s = n;
        if (std.mem.eql(u8, n.name, "PackedBacked")) packed_backed = n;
        if (std.mem.eql(u8, n.name, "Ext")) ext_s = n;
        if (std.mem.eql(u8, n.name, "PackedUnion")) packed_u = n;
        if (std.mem.eql(u8, n.name, "ExternUnion")) ext_u = n;
        if (std.mem.eql(u8, n.name, "NormalUnion")) normal_u = n;
        if (std.mem.eql(u8, n.name, "GenPacked")) gen_packed = n;
    }

    // All nodes found
    try std.testing.expect(normal != null);
    try std.testing.expect(packed_s != null);
    try std.testing.expect(packed_backed != null);
    try std.testing.expect(ext_s != null);
    try std.testing.expect(packed_u != null);
    try std.testing.expect(ext_u != null);
    try std.testing.expect(normal_u != null);
    try std.testing.expect(gen_packed != null);

    // Normal struct: no zig meta (neither packed nor extern)
    try std.testing.expectEqual(LangMeta{ .none = {} }, normal.?.lang_meta);

    // Packed struct: is_packed=true, is_extern=false
    switch (packed_s.?.lang_meta) {
        .zig => |zm| {
            try std.testing.expect(zm.is_packed);
            try std.testing.expect(!zm.is_extern);
        },
        .none => return error.ExpectedZigMeta,
    }

    // Packed struct with backing type: same as packed
    switch (packed_backed.?.lang_meta) {
        .zig => |zm| try std.testing.expect(zm.is_packed),
        .none => return error.ExpectedZigMeta,
    }

    // Extern struct: is_extern=true, is_packed=false
    switch (ext_s.?.lang_meta) {
        .zig => |zm| {
            try std.testing.expect(zm.is_extern);
            try std.testing.expect(!zm.is_packed);
        },
        .none => return error.ExpectedZigMeta,
    }

    // Packed union: is_packed=true
    try std.testing.expectEqual(NodeKind.union_def, packed_u.?.kind);
    switch (packed_u.?.lang_meta) {
        .zig => |zm| try std.testing.expect(zm.is_packed),
        .none => return error.ExpectedZigMeta,
    }

    // Extern union: is_extern=true
    try std.testing.expectEqual(NodeKind.union_def, ext_u.?.kind);
    switch (ext_u.?.lang_meta) {
        .zig => |zm| try std.testing.expect(zm.is_extern),
        .none => return error.ExpectedZigMeta,
    }

    // Normal union: no zig meta
    try std.testing.expectEqual(LangMeta{ .none = {} }, normal_u.?.lang_meta);

    // Type-returning function returning packed struct: is_packed=true
    try std.testing.expectEqual(NodeKind.type_def, gen_packed.?.kind);
    switch (gen_packed.?.lang_meta) {
        .zig => |zm| try std.testing.expect(zm.is_packed),
        .none => return error.ExpectedZigMeta,
    }
}

test "type-returning function signature preserved" {
    // generic_type.zig fixture: Container, Result, Config
    {
        // Arrange
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);

        // Act
        try parse(std.testing.allocator, fixtures.zig.generic_type, &g, null, Logger.noop);

        // Assert: find Container, Result, Config nodes
        var container_node: ?*const Node = null;
        var result_node: ?*const Node = null;
        var config_node: ?*const Node = null;
        for (g.nodes.items) |*n| {
            if (std.mem.eql(u8, n.name, "Container")) container_node = n;
            if (std.mem.eql(u8, n.name, "Result")) result_node = n;
            if (std.mem.eql(u8, n.name, "Config")) config_node = n;
        }
        try std.testing.expect(container_node != null);
        try std.testing.expect(result_node != null);
        try std.testing.expect(config_node != null);

        // Assert: Container has kind type_def and signature != null
        try std.testing.expectEqual(NodeKind.type_def, container_node.?.kind);
        try std.testing.expect(container_node.?.signature != null);
        const container_sig = container_node.?.signature.?;
        // Signature contains function name and generic parameter
        try std.testing.expect(std.mem.indexOf(u8, container_sig, "Container") != null);
        try std.testing.expect(std.mem.indexOf(u8, container_sig, "comptime T") != null);

        // Assert: Result has kind union_def and signature != null
        try std.testing.expectEqual(NodeKind.union_def, result_node.?.kind);
        try std.testing.expect(result_node.?.signature != null);
        const result_sig = result_node.?.signature.?;
        // Signature contains function name and both generic parameters
        try std.testing.expect(std.mem.indexOf(u8, result_sig, "Result") != null);
        try std.testing.expect(std.mem.indexOf(u8, result_sig, "comptime T") != null);
        try std.testing.expect(std.mem.indexOf(u8, result_sig, "comptime E") != null);

        // Assert: Config has kind type_def and signature == null (non-generic, not a function)
        try std.testing.expectEqual(NodeKind.type_def, config_node.?.kind);
        try std.testing.expectEqual(@as(?[]const u8, null), config_node.?.signature);
    }

    // inline type-returning function: is_inline and signature
    {
        // Arrange
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\inline fn Wrapper(comptime T: type) type {
            \\    return struct { val: T };
            \\}
        ;

        // Act
        try parse(std.testing.allocator, source, &g, null, Logger.noop);

        // Assert: find Wrapper node
        var wrapper_node: ?*const Node = null;
        for (g.nodes.items) |*n| {
            if (std.mem.eql(u8, n.name, "Wrapper")) wrapper_node = n;
        }
        try std.testing.expect(wrapper_node != null);

        // Assert: kind is type_def
        try std.testing.expectEqual(NodeKind.type_def, wrapper_node.?.kind);

        // Assert: signature is present
        try std.testing.expect(wrapper_node.?.signature != null);

        // Assert: is_inline is true in lang_meta
        switch (wrapper_node.?.lang_meta) {
            .zig => |zm| try std.testing.expect(zm.is_inline),
            .none => return error.ExpectedZigMeta,
        }
    }
}

test "conditional expressions classified as constant with comptime_conditional" {
    // Sub-test 1: if-expression with inline structs
    {
        // Arrange
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\const flag = true;
            \\pub const system = if (flag)
            \\    struct { pub fn read() void {} }
            \\else
            \\    struct { pub fn write() void {} };
        ;

        // Act
        try parse(std.testing.allocator, source, &g, null, Logger.noop);

        // Assert: system has kind .constant (NOT .type_def)
        var system_node: ?*const Node = null;
        for (g.nodes.items) |*n| {
            if (std.mem.eql(u8, n.name, "system")) {
                system_node = n;
                break;
            }
        }
        try std.testing.expect(system_node != null);
        try std.testing.expectEqual(NodeKind.constant, system_node.?.kind);

        // Assert: system has lang_meta.zig.comptime_conditional == true
        switch (system_node.?.lang_meta) {
            .zig => |zm| try std.testing.expect(zm.comptime_conditional),
            .none => return error.ExpectedZigMeta,
        }

        // Assert: system has visibility .public
        try std.testing.expectEqual(Visibility.public, system_node.?.visibility);

        // Assert: no child nodes with parent system exist (no read, no write leaked)
        var system_id: ?NodeId = null;
        for (g.nodes.items, 0..) |n, idx| {
            if (std.mem.eql(u8, n.name, "system")) {
                system_id = @enumFromInt(idx);
                break;
            }
        }
        try std.testing.expect(system_id != null);
        var child_count: usize = 0;
        for (g.nodes.items) |n| {
            if (n.parent_id != null and n.parent_id.? == system_id.?) {
                child_count += 1;
            }
        }
        try std.testing.expectEqual(@as(usize, 0), child_count);
    }

    // Sub-test 2: switch-expression with inline structs
    {
        // Arrange
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\const Mode = enum { fast, safe };
            \\const mode: Mode = .fast;
            \\pub const backend = switch (mode) {
            \\    .fast => struct { pub fn init() void {} },
            \\    .safe => struct { pub fn init() void {} },
            \\};
        ;

        // Act
        try parse(std.testing.allocator, source, &g, null, Logger.noop);

        // Assert: backend has kind .constant (NOT .type_def)
        var backend_node: ?*const Node = null;
        for (g.nodes.items) |*n| {
            if (std.mem.eql(u8, n.name, "backend")) {
                backend_node = n;
                break;
            }
        }
        try std.testing.expect(backend_node != null);
        try std.testing.expectEqual(NodeKind.constant, backend_node.?.kind);

        // Assert: backend has lang_meta.zig.comptime_conditional == true
        switch (backend_node.?.lang_meta) {
            .zig => |zm| try std.testing.expect(zm.comptime_conditional),
            .none => return error.ExpectedZigMeta,
        }

        // Assert: no child function init has parent backend
        var backend_id: ?NodeId = null;
        for (g.nodes.items, 0..) |n, idx| {
            if (std.mem.eql(u8, n.name, "backend")) {
                backend_id = @enumFromInt(idx);
                break;
            }
        }
        try std.testing.expect(backend_id != null);
        var child_count: usize = 0;
        for (g.nodes.items) |n| {
            if (n.parent_id != null and n.parent_id.? == backend_id.?) {
                child_count += 1;
            }
        }
        try std.testing.expectEqual(@as(usize, 0), child_count);
    }

    // Sub-test 3: plain struct is unaffected
    {
        // Arrange
        var g = Graph.init("/tmp/project");
        defer g.deinit(std.testing.allocator);
        const source =
            \\pub const Config = struct {
            \\    name: []const u8,
            \\    pub fn defaults() void {}
            \\};
        ;

        // Act
        try parse(std.testing.allocator, source, &g, null, Logger.noop);

        // Assert: Config has kind .type_def (unchanged, no conditional)
        var config_node: ?*const Node = null;
        for (g.nodes.items) |*n| {
            if (std.mem.eql(u8, n.name, "Config")) {
                config_node = n;
                break;
            }
        }
        try std.testing.expect(config_node != null);
        try std.testing.expectEqual(NodeKind.type_def, config_node.?.kind);

        // Assert: defaults function has parent Config (children still captured)
        var config_id: ?NodeId = null;
        for (g.nodes.items, 0..) |n, idx| {
            if (std.mem.eql(u8, n.name, "Config")) {
                config_id = @enumFromInt(idx);
                break;
            }
        }
        try std.testing.expect(config_id != null);
        var found_defaults = false;
        for (g.nodes.items) |n| {
            if (n.kind == .function and std.mem.eql(u8, n.name, "defaults") and
                n.parent_id != null and n.parent_id.? == config_id.?)
            {
                found_defaults = true;
                break;
            }
        }
        try std.testing.expect(found_defaults);

        // Assert: Config does NOT have comptime_conditional == true
        switch (config_node.?.lang_meta) {
            .zig => |zm| try std.testing.expect(!zm.comptime_conditional),
            .none => {}, // no zig meta is also acceptable (means no flags set)
        }
    }
}

test "error set variant names captured in signature" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);
    const source =
        \\/// Multi-member error set.
        \\pub const FileError = error{
        \\    NotFound,
        \\    PermissionDenied,
        \\    IsDir,
        \\};
        \\
        \\/// Single-member error set.
        \\pub const TimeoutError = error{Timeout};
        \\
        \\/// Error set merge (inline part only).
        \\pub const IoError = FileError || error{BrokenPipe};
        \\
        \\/// Empty error set.
        \\const E = error{};
        \\
        \\/// Not an error set (for negative check).
        \\pub fn open() void {}
    ;

    // Act
    try parse(std.testing.allocator, source, &g, null, Logger.noop);

    // Assert 1: FileError has kind .error_def
    var file_error_node: ?*const Node = null;
    var file_error_id: ?NodeId = null;
    for (g.nodes.items, 0..) |*n, idx| {
        if (std.mem.eql(u8, n.name, "FileError")) {
            file_error_node = n;
            file_error_id = @enumFromInt(idx);
        }
    }
    try std.testing.expect(file_error_node != null);
    try std.testing.expectEqual(NodeKind.error_def, file_error_node.?.kind);

    // Assert 2: FileError has signature != null
    try std.testing.expect(file_error_node.?.signature != null);

    // Assert 3: FileError's signature contains variant names
    const fe_sig = file_error_node.?.signature.?;
    try std.testing.expect(std.mem.indexOf(u8, fe_sig, "NotFound") != null);
    try std.testing.expect(std.mem.indexOf(u8, fe_sig, "PermissionDenied") != null);
    try std.testing.expect(std.mem.indexOf(u8, fe_sig, "IsDir") != null);

    // Assert 4: TimeoutError has kind .error_def and signature containing "Timeout"
    var timeout_node: ?*const Node = null;
    for (g.nodes.items) |*n| {
        if (std.mem.eql(u8, n.name, "TimeoutError")) timeout_node = n;
    }
    try std.testing.expect(timeout_node != null);
    try std.testing.expectEqual(NodeKind.error_def, timeout_node.?.kind);
    try std.testing.expect(timeout_node.?.signature != null);
    try std.testing.expect(std.mem.indexOf(u8, timeout_node.?.signature.?, "Timeout") != null);

    // Assert 5: IoError has kind .error_def and signature containing "BrokenPipe"
    var io_error_node: ?*const Node = null;
    for (g.nodes.items) |*n| {
        if (std.mem.eql(u8, n.name, "IoError")) io_error_node = n;
    }
    try std.testing.expect(io_error_node != null);
    try std.testing.expectEqual(NodeKind.error_def, io_error_node.?.kind);
    try std.testing.expect(io_error_node.?.signature != null);
    try std.testing.expect(std.mem.indexOf(u8, io_error_node.?.signature.?, "BrokenPipe") != null);

    // Assert 6: E has kind .error_def (empty error set)
    var e_node: ?*const Node = null;
    for (g.nodes.items) |*n| {
        if (std.mem.eql(u8, n.name, "E")) e_node = n;
    }
    try std.testing.expect(e_node != null);
    try std.testing.expectEqual(NodeKind.error_def, e_node.?.kind);

    // Assert 7: open has kind .function and signature does NOT contain "error"
    var open_node: ?*const Node = null;
    for (g.nodes.items) |*n| {
        if (std.mem.eql(u8, n.name, "open")) open_node = n;
    }
    try std.testing.expect(open_node != null);
    try std.testing.expectEqual(NodeKind.function, open_node.?.kind);
    if (open_node.?.signature) |sig| {
        try std.testing.expect(std.mem.indexOf(u8, sig, "error") == null);
    }

    // Assert 8: No child nodes exist with parent FileError
    var child_count: usize = 0;
    for (g.nodes.items) |n| {
        if (n.parent_id != null and n.parent_id.? == file_error_id.?) {
            child_count += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 0), child_count);
}

test "re-export filtering: public re-exports preserved, private aliases filtered" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);
    const source =
        \\const std = @import("std");
        \\const utils = @import("utils.zig");
        \\
        \\/// Public re-export from std: intentional API surface.
        \\pub const iovec = std.posix.iovec;
        \\
        \\/// Public re-export from local import.
        \\pub const Helper = utils.Helper;
        \\
        \\/// Private same-name alias: noise, should be filtered.
        \\const Allocator = std.mem.Allocator;
        \\
        \\/// Private same-name alias from local import.
        \\const InternalHelper = utils.InternalHelper;
        \\
        \\/// Different name: NOT a same-name re-export, always kept.
        \\pub const MyAlloc = std.mem.Allocator;
        \\
        \\/// Private different name: also kept (not same-name).
        \\const LocalAlloc = std.mem.Allocator;
        \\
        \\pub fn doWork() void {}
    ;

    // Act
    try parse(std.testing.allocator, source, &g, null, Logger.noop);

    // Assert: collect all nodes by name
    var found_iovec = false;
    var found_helper = false;
    var found_allocator = false;
    var found_internal_helper = false;
    var found_my_alloc = false;
    var found_local_alloc = false;
    var found_do_work = false;
    for (g.nodes.items) |n| {
        if (std.mem.eql(u8, n.name, "iovec")) found_iovec = true;
        if (std.mem.eql(u8, n.name, "Helper")) found_helper = true;
        if (std.mem.eql(u8, n.name, "Allocator")) found_allocator = true;
        if (std.mem.eql(u8, n.name, "InternalHelper")) found_internal_helper = true;
        if (std.mem.eql(u8, n.name, "MyAlloc")) found_my_alloc = true;
        if (std.mem.eql(u8, n.name, "LocalAlloc")) found_local_alloc = true;
        if (std.mem.eql(u8, n.name, "doWork")) found_do_work = true;
    }

    // Public same-name re-exports: KEPT (intentional API)
    try std.testing.expect(found_iovec);
    try std.testing.expect(found_helper);

    // Private same-name aliases: FILTERED (noise)
    try std.testing.expect(!found_allocator);
    try std.testing.expect(!found_internal_helper);

    // Different-name aliases: KEPT regardless of visibility
    try std.testing.expect(found_my_alloc);
    try std.testing.expect(found_local_alloc);

    // Sanity: function and imports are unaffected
    try std.testing.expect(found_do_work);

    // Assert: public re-exports have correct visibility
    for (g.nodes.items) |n| {
        if (std.mem.eql(u8, n.name, "iovec")) {
            try std.testing.expectEqual(Visibility.public, n.visibility);
            try std.testing.expectEqual(NodeKind.constant, n.kind);
        }
        if (std.mem.eql(u8, n.name, "Helper")) {
            try std.testing.expectEqual(Visibility.public, n.visibility);
            try std.testing.expectEqual(NodeKind.constant, n.kind);
        }
    }
}

test "top-level comptime blocks produce no nodes" {
    // Arrange
    var g = Graph.init("/tmp/project");
    defer g.deinit(std.testing.allocator);
    const source =
        \\const std = @import("std");
        \\
        \\comptime {
        \\    @export(&std, .{ .name = "main" });
        \\}
        \\
        \\comptime {
        \\    if (@import("builtin").is_test) {
        \\        _ = std;
        \\    }
        \\}
        \\
        \\pub const Config = struct { name: []const u8 };
        \\pub fn init() void {}
    ;

    // Act
    try parse(std.testing.allocator, source, &g, null, Logger.noop);

    // Assert: no comptime_block nodes exist (comptime blocks produce zero nodes)
    for (g.nodes.items) |n| {
        try std.testing.expect(!std.mem.eql(u8, n.name, "comptime"));
    }

    // Assert: Config still has kind .type_def
    var found_config = false;
    for (g.nodes.items) |n| {
        if (std.mem.eql(u8, n.name, "Config")) {
            try std.testing.expectEqual(NodeKind.type_def, n.kind);
            found_config = true;
        }
    }
    try std.testing.expect(found_config);

    // Assert: init still has kind .function
    var found_init = false;
    for (g.nodes.items) |n| {
        if (std.mem.eql(u8, n.name, "init")) {
            try std.testing.expectEqual(NodeKind.function, n.kind);
            found_init = true;
        }
    }
    try std.testing.expect(found_init);

    // Assert: only file + std import + Config (type_def) + name (field) + init (function) = 5 nodes
    try std.testing.expectEqual(@as(usize, 5), g.nodeCount());
}
