/// Factory module: provides a create() function that returns a Service.
/// Used by return_value.zig to test method calls on cross-file return values.
const svc_mod = @import("service.zig");

pub fn create() svc_mod.Service {
    return .{ .state = 0 };
}
