// Dumbing it down :)

const std = @import("std");

const print = std.debug.print;

pub fn main() void {
    var value: u128 = 0;
    loop: for (0..10) |j| {
        value = j;
        if ((value * value) % 10 != 0) {
            continue;
        }
        for (0..10) |i| {
            for (0..10) |h| {
                value = j + 10 * (i + 10 * h);
                if (((value * value) / 100) % 10 != 9) {
                    continue;
                }
                for (0..10) |g| {
                    for (0..10) |f| {
                        value = j + 10 * (i + 10 * (h + 10 * (g + 10 * f)));
                        if (((value * value) / 10000) % 10 != 8) {
                            continue;
                        }
                        for (0..10) |e| {
                            value = j + 10 * (i + 10 * (h + 10 * (g + 10 * (f + 10 * e))));
                            if (((value * value) / 1000000) % 10 != 7) {
                                continue;
                            }
                            for (0..10) |d| {
                                for (0..10) |c| {
                                    value = j + 10 * (i + 10 * (h + 10 * (g + 10 * (f + 10 * (e + 10 * (d + 10 * c))))));
                                    if (((value * value) / 100000000) % 10 != 6) {
                                        continue;
                                    }
                                    for (0..10) |b| {
                                        for (0..10) |a| {
                                            value = j + 10 * (i + 10 * (h + 10 * (g + 10 * (f + 10 * (e + 10 * (d + 10 * (c + 10 * (b + 10 * a))))))));
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
                                            break :loop;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
