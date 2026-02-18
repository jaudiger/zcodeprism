/// Tests basic parameter method call: function receives a struct from another
/// file and calls a method on it.
///
/// Expected edges:
///   handle -> Service.process  (calls)
///   handle -> Service           (uses_type)
const svc_mod = @import("service.zig");

pub fn handle(svc: svc_mod.Service) u32 {
    return svc.process();
}
