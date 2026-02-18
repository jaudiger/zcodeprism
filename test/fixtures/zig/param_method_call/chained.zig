/// Tests chained cross-file method call: Service has a getClient() method that
/// returns a Client from yet another file. The consumer calls svc.getClient().send().
///
/// Expected edges:
///   handleChained -> Service.getClient  (calls)
///   handleChained -> Client.send        (calls, transitive — requires return type tracking)
///   handleChained -> Service            (uses_type)
const svc_mod = @import("service_with_client.zig");

pub fn handleChained(svc: svc_mod.ServiceWithClient) void {
    svc.getClient().send();
}
