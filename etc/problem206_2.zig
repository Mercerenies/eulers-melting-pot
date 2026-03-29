// Why do anything intelligent when I can just brute force? That's the
// motto of Euler's Melting Pot! ^.^

const std = @import("std");

const print = std.debug.print;

pub fn main() void {
    for (0..10000000000) |value| {
        if ((value * value) % 10 != 0) {
            continue;
        }
        if (((value * value) / 100) % 10 != 9) {
            continue;
        }
        if (((value * value) / 10000) % 10 != 8) {
            continue;
        }
        if (((value * value) / 1000000) % 10 != 7) {
            continue;
        }
        if (((value * value) / 100000000) % 10 != 6) {
            continue;
        }
        if (((value * value) / 10000000000) % 10 != 5) {
            continue;
        }
        if (((value * value) / 1000000000000) % 10 != 4) {
            continue;
        }
        if (((value * value) / 100000000000000) % 10 != 3) {
            continue;
        }
        if (((value * value) / 10000000000000000) % 10 != 2) {
            continue;
        }
        if (((value * value) / 1000000000000000000) != 1) {
            continue;
        }
        print("{}\n", .{value});
        break;
    }
}
