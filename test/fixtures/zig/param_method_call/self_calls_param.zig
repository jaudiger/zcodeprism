/// Tests a struct method that receives a parameter typed from another file
/// and calls methods on it. The caller is a method (has self), not a free function.
///
/// Expected edges:
///   Handler.execute -> Service.process  (calls)
///   Handler.execute -> Service          (uses_type)
const svc_mod = @import("service.zig");

pub const Handler = struct {
    name: []const u8,

    pub fn execute(self: Handler, svc: svc_mod.Service) u32 {
        _ = self;
        return svc.process();
    }
};
