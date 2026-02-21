const provider = @import("provider.zig");

test "inner struct call" {
    const Test = struct {
        fn do() void {
            provider.someFunction();
        }
    };
    Test.do();
}

test "direct call" {
    provider.anotherFunction();
}
