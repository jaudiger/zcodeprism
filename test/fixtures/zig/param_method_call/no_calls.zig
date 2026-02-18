/// Negative fixture: imports Service but does NOT call any methods on it.
/// Used to verify no false-positive calls edges are created.
///
/// Expected edges:
///   noMethodCalls -> Service  (uses_type only, no calls)
const svc_mod = @import("service.zig");

pub fn noMethodCalls(svc: svc_mod.Service) u32 {
    _ = svc;
    return 42;
}
