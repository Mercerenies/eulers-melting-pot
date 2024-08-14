// Same naive approach as problem188.jl and problem188_1.zig, but with
// all the function calls removed so we can more easily translate into
// a 2D language.

const std = @import("std");
const print = std.debug.print;

pub fn main() void {
    var a: i64 = 1777;
    var b: i64 = 1855;
    const m: i64 = 100_000_000;
    var result: i64 = 1;

    for (0..@intCast(b)) |_| {
        var inner_a = a;
        var inner_b = result;
        var pow_result: i64 = 1;
        while (inner_b > 0) {
            if (@mod(inner_b, 2) == 0) {
                inner_a *= inner_a;
                inner_a = @mod(inner_a, m);
                inner_b = @divExact(inner_b, 2);
            } else {
                pow_result *= inner_a;
                pow_result = @mod(pow_result, m);
                inner_b -= 1;
            }
        }
        result = pow_result;
    }
    print("{}\n", .{result});
}
