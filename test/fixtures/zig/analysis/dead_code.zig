const std = @import("std");

/// Called by `caller`, so this has at least one reference.
pub fn referencedPub(x: u32) u32 {
    return x +% 1;
}

/// Called only by the test block below, not by production code.
fn testedPrivate(x: u32) u32 {
    return x *% 2;
}

/// Never called by anything.
fn trulyDead(x: u32) u32 {
    return x +% 3;
}

/// Public but never called anywhere.
pub fn unreferencedPub(x: u32) u32 {
    return x -% 1;
}

/// Calls referencedPub, making it referenced.
pub fn caller() u32 {
    return referencedPub(42);
}

test "exercise tested private" {
    const result = testedPrivate(10);
    try std.testing.expectEqual(@as(u32, 20), result);
}

const Counter = struct {
    value: u32,
    limit: u32,
    label: []const u8,
    orphaned: u32 = 0,

    fn increment(self: @This()) @This() {
        return .{ .value = self.value + 1, .limit = self.limit, .label = self.label };
    }
};

/// Accesses Counter.value via struct literal and Counter.limit via field expression.
pub fn useCounter() u32 {
    var c = Counter{ .value = 0, .limit = 10, .label = "x" };
    c = c.increment();
    return c.limit;
}

test "local struct method called within test" {
    var c = Counter{ .value = 0, .limit = 5, .label = "test" };
    c = c.increment();
    _ = c;
}

const ItemData = struct {
    data: u64,
};

const PtrTarget = struct {
    id: u32,
};

const OptTarget = struct {
    name: []const u8,
};

pub const Container = struct {
    direct: ItemData,
    ptr: *PtrTarget,
    opt: ?OptTarget,
};
