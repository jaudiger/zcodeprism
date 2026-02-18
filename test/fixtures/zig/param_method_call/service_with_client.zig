/// Bridge module: defines ServiceWithClient that returns a Client from client.zig.
/// Used by chained.zig to test chained cross-file method call resolution.
const client_mod = @import("client.zig");

pub const ServiceWithClient = struct {
    client: client_mod.Client,

    pub fn getClient(self: ServiceWithClient) client_mod.Client {
        return self.client;
    }
};
