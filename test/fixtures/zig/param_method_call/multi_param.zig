/// Tests multiple parameters from different files: function receives a Service
/// from service.zig and a Client from client.zig, and calls methods on both.
///
/// Expected edges:
///   handleMulti -> Service.process    (calls)
///   handleMulti -> Client.send        (calls)
///   handleMulti -> Service            (uses_type)
///   handleMulti -> Client             (uses_type)
const svc_mod = @import("service.zig");
const client_mod = @import("client.zig");

pub fn handleMulti(svc: svc_mod.Service, cli: client_mod.Client) u32 {
    cli.send();
    return svc.process();
}
