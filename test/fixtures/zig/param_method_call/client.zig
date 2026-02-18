/// Provider module: defines Client struct with methods.
/// Used by chained.zig and multi_param.zig to test cross-file parameter calls
/// involving a second imported type.
pub const Client = struct {
    id: u64,

    pub fn send(self: *Client) void {
        _ = self;
    }

    pub fn disconnect(self: *Client) void {
        _ = self;
    }
};
