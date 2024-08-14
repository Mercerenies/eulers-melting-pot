// Zig implementation of Problem 188 naively. I want to see how my
// implementation of powermod compares to Julia's.

const std = @import("std");
const print = std.debug.print;

pub fn powermod(original_a: i64, original_b: i64, m: i64) i64 {
    // Note: For our purposes, we can assume b will never be zero. Our
    // starting base is 1777 which is coprime to 10^8, so no amount of
    // multiplying 1777 by itself can get zero mod 10^8.
    var a = original_a;
    var b = original_b;
    var result: i64 = 1;
    while (b > 0) {
        if (@mod(b, 2) == 0) {
            a *= a;
            a = @mod(a, m);
            b = @divExact(b, 2);
        } else {
            result *= a;
            result = @mod(result, m);
            b -= 1;
        }
    }
    return result;
}

pub fn tetration(a: i64, b: i64, m: i64) i64 {
    var result: i64 = 1;
    for (0..@intCast(b)) |_| {
        result = powermod(a, result, m);
    }
    return result;
}

pub fn main() void {
    print("{}\n", .{tetration(1777, 1855, 100_000_000)});
}
