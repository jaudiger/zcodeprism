/// Tests optional parameter method call: function receives a ?Service from
/// another file, unwraps it, and calls a method on the payload.
///
/// Expected edges:
///   handleOpt -> Service.process  (calls)
///   handleOpt -> Service           (uses_type)
const svc_mod = @import("service.zig");

pub fn handleOpt(svc: ?svc_mod.Service) u32 {
    if (svc) |s| {
        return s.process();
    }
    return 0;
}
