/// Tests pointer parameter method call: function receives a *Service from
/// another file and calls a method on it.
///
/// Expected edges:
///   handlePtr -> Service.reset  (calls)
///   handlePtr -> Service        (uses_type)
const svc_mod = @import("service.zig");

pub fn handlePtr(svc: *svc_mod.Service) void {
    svc.reset();
}
