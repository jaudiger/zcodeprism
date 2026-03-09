const std = @import("std");

/// Function with many branches contributing to high cyclomatic complexity.
pub fn highComplexity(data: []const u8, mode: u8) usize {
    var count: usize = 0;

    for (data) |byte| {
        if (byte == 0) {
            count += 1;
        } else if (byte < 0x20) {
            count += 2;
        } else if (byte < 0x40) {
            count += 3;
        } else if (byte < 0x60) {
            count += 4;
        } else {
            count += 5;
        }

        switch (mode) {
            0 => count += 10,
            1 => count += 20,
            2 => count += 30,
            3 => count += 40,
            else => count += 50,
        }

        if (byte == '\n' or byte == '\r') {
            count += 100;
        }

        if (count > 1000) break;
    }

    if (count == 0) return 0;
    if (count < 10) return 1;
    if (count < 100) return 2;
    return 3;
}

/// Trivial function with no branches.
pub fn trivial() usize {
    return 42;
}
