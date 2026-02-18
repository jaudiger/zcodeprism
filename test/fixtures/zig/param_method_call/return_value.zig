/// Tests calling a method on the return value of an imported function.
/// factory.zig provides a create() function that returns a Service.
/// This file calls create() and then calls process() on the result.
///
/// Expected edges:
///   useFactory -> factory.create   (calls)
///   useFactory -> Service.process  (calls, requires return type tracking)
const factory_mod = @import("factory.zig");

pub fn useFactory() u32 {
    var svc = factory_mod.create();
    return svc.process();
}
