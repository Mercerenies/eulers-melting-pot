const std = @import("std");

const print = std.debug.print;

pub fn main() void {
    const answer: u64 = enumerate(0, 1, 0);
    print("{}\n", .{answer});
}

pub fn enumerate(square_root: u64, current_place: u64, current_place_index: u64) u64 {
    if (current_place_index >= 10) {
        return square_root;
    } else {
        for (0..10) |digit| {
            const new_square_root = square_root + current_place * digit;
            if (!check_last_digits(current_place_index, new_square_root)) {
                continue;
            }
            const result = enumerate(new_square_root, current_place * 10, current_place_index + 1);
            if (result != std.math.maxInt(u64)) {
                return result;
            }
        }
        return std.math.maxInt(u64);
    }
}

pub fn check_last_digits(current_place_index: u64, square_root: u64) bool {
    if (square_root >= 4294967296) {
        // Squaring it will overflow a u64, clearly too large.
        return false;
    }
    const square = square_root * square_root;
    if (square % 10 != 0) {
        return false;
    }
    if ((current_place_index >= 2) and ((square / 100) % 10 != 9)) {
        return false;
    }
    if ((current_place_index >= 4) and ((square / 10000) % 10 != 8)) {
        return false;
    }
    if ((current_place_index >= 6) and ((square / 1000000) % 10 != 7)) {
        return false;
    }
    if ((current_place_index >= 8) and ((square / 100000000) % 10 != 6)) {
        return false;
    }
    if ((current_place_index >= 9) and ((square / 10000000000) % 10 != 5)) {
        return false;
    }
    if ((current_place_index >= 9) and ((square / 1000000000000) % 10 != 4)) {
        return false;
    }
    if ((current_place_index >= 9) and ((square / 100000000000000) % 10 != 3)) {
        return false;
    }
    if ((current_place_index >= 9) and ((square / 10000000000000000) % 10 != 2)) {
        return false;
    }
    if ((current_place_index >= 9) and ((square / 1000000000000000000) != 1)) {
        return false;
    }
    return true;
}
