/// Provider module: defines Service struct with methods.
/// Used by consumer.zig to test parameter method call resolution.
pub const Service = struct {
    state: u32,

    pub fn process(self: *Service) u32 {
        _ = self;
        return 0;
    }

    pub fn reset(self: *Service) void {
        self.state = 0;
    }
};
