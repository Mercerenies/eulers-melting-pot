
// Ulam sequence generator using []i64 and complicated bit arithmetic. See https://math.stackexchange.com/questions/2165222/generating-ulam-sequences-using-bit-manipulation (specifically the answer that changed the function slightly)
//
// STRONGLY recommend running this with -OReleaseFast

const std = @import("std");

const AllocationError = error {
    OutOfMemory,
};

const SIZEOF_U64 = @bitSizeOf(u64);

// Periods and fundamental differences from "On the Regularity of
// Certain 1-Additive Sequences (Steven R. Finch)".
const KNOWN_PERIODS =
    [_]u64{0, 0, 0, 0, 0, 32, 0, 26, 0, 444, 0, 1628, 0, 5906, 0, 80, 0, 126960, 0, 380882, 0, 2097152};
const KNOWN_DIFFERENCES =
    [_]u64{0, 0, 0, 0, 0, 126, 0, 126, 0, 1778, 0, 6510, 0, 23622, 0, 510, 0, 507842, 0, 1523526, 0, 8388606};

inline fn bitOf(value: u64, bit_number: u6) u1 {
    return @truncate(value >> bit_number);
}

inline fn setBitOf(value: *u64, bit_number: u6, new_bit_value: u1) void {
    const current_value = value.* & (@as(u64, 1) << bit_number);
    const flip_value = current_value ^ (@as(u64, new_bit_value) << bit_number);
    value.* = value.* ^ flip_value;
}

inline fn bitOfArray(values: []u64, index: u64) u1 {
    return bitOf(values[index / SIZEOF_U64], @truncate(index % SIZEOF_U64));
}

inline fn setBitOfArray(values: []u64, index: u64, value: u1) void {
    setBitOf(&values[index / SIZEOF_U64], @truncate(index % SIZEOF_U64), value);
}

inline fn firstNOnes(count: u6) u64 {
    return (@as(u64, 1) << count) - 1;
}

inline fn doUlamStepSlow(ulam_array: []u64, non_ulam_array: []u64, k: u64, i: u64) void {
    const j = k + i;
    const src_bit = bitOfArray(ulam_array, i);
    const dest_bit = bitOfArray(ulam_array, j);
    var non_ulam_bit = bitOfArray(non_ulam_array, j);
    non_ulam_bit |= (src_bit & dest_bit);
    setBitOfArray(non_ulam_array, j, non_ulam_bit);
    setBitOfArray(ulam_array, j, (dest_bit | src_bit) & ~non_ulam_bit);
}

inline fn doUlamSumSlow(ulam_array: []u64, non_ulam_array: []u64, k: u64) void {
    // The slow version; do each bit individually.
    const upper_bound = @min(k, ulam_array.len * SIZEOF_U64 - k);
    for (1..upper_bound) |i| {
        doUlamStepSlow(ulam_array, non_ulam_array, k, i);
    }
}

inline fn doUlamSum(ulam_array: []u64, non_ulam_array: []u64, k: u64) void {
    const upper_bound = @min(k, ulam_array.len * SIZEOF_U64 - k);
    // Avoid weird corner cases; do it the slow way for small inputs.
    if (upper_bound < 2 * SIZEOF_U64) {
        doUlamSumSlow(ulam_array, non_ulam_array, k);
        return;
    }

    if (k % SIZEOF_U64 == 0) {
        // Easy case: We're axis aligned
        for (0..(upper_bound/SIZEOF_U64)) |i| {
            const j = k / SIZEOF_U64 + i;
            const src_value = ulam_array[i];
            const dest_value = ulam_array[j];
            var non_ulam_value = non_ulam_array[j];
            non_ulam_value |= src_value & dest_value;
            non_ulam_array[j] = non_ulam_value;
            ulam_array[j] = (dest_value | src_value) & ~non_ulam_value;
        }
    } else {
        // Hard case: Not axis-aligned
        const dest_shift_amount = @as(u6, @truncate(k % SIZEOF_U64));
        const lower_dest_mask = firstNOnes(dest_shift_amount);
        const upper_dest_mask = ~lower_dest_mask;
        const src_shift_amount = @as(u6, @truncate(SIZEOF_U64 - (k % SIZEOF_U64)));
        const lower_src_mask = firstNOnes(src_shift_amount);
        const upper_src_mask = ~lower_src_mask;
        for (0..(upper_bound/SIZEOF_U64)) |i| {
            // Map lower of src onto upper of dest
            {
                const j = k / SIZEOF_U64 + i;
                const src_value = (ulam_array[i] & lower_src_mask) << dest_shift_amount;
                const dest_value = ulam_array[j] & upper_dest_mask;
                var non_ulam_value = non_ulam_array[j] & upper_dest_mask;
                non_ulam_value |= src_value & dest_value;
                non_ulam_array[j] |= src_value & dest_value;
                ulam_array[j] |= src_value;
                ulam_array[j] &= ~non_ulam_value;
            }
            // Map upper of src onto lower of dest
            {
                const j = k / SIZEOF_U64 + i + 1;
                const src_value = (ulam_array[i] & upper_src_mask) >> src_shift_amount;
                const dest_value = ulam_array[j] & lower_dest_mask;
                var non_ulam_value = non_ulam_array[j] & lower_dest_mask;
                non_ulam_value |= src_value & dest_value;
                non_ulam_array[j] |= src_value & dest_value;
                ulam_array[j] |= src_value;
                ulam_array[j] &= ~non_ulam_value;
            }
        }
    }
    // Now finish the last few bits the slow way.
    for (((upper_bound/SIZEOF_U64) * SIZEOF_U64) .. upper_bound) |i| {
        doUlamStepSlow(ulam_array, non_ulam_array, k, i);
    }
}

fn generateUlamBits(allocator: std.mem.Allocator, a: u64, b: u64, length: usize) AllocationError![]u64 {
    // Zero-initialize the array
    const ulam_array = try allocator.alloc(u64, length);
    for (ulam_array) |*item| {
        item.* = 0;
    }

    // Zero-initialize the "false" array
    const non_ulam_array = try allocator.alloc(u64, length);
    for (non_ulam_array) |*item| {
        item.* = 0;
    }

    var k = b;
    setBitOfArray(ulam_array, a, 1);
    setBitOfArray(ulam_array, b, 1);

    while (k < length * SIZEOF_U64) {
        doUlamSum(ulam_array, non_ulam_array, k);
        // Calculate new k.
        for (k + 1 .. length * SIZEOF_U64) |new_k| {
            if (bitOfArray(ulam_array, new_k) != 0) {
                k = new_k;
                break;
            }
        } else {
            break;
        }
    }

    return ulam_array;
}

fn compileUlamValues(allocator: std.mem.Allocator, bits: []u64) AllocationError!std.ArrayList(u64) {
    var list = std.ArrayList(u64).init(allocator);
    for (bits, 0..) |value, index| {
        for (0..SIZEOF_U64) |bit| {
            const bit_value = bitOf(value, @truncate(bit));
            if (bit_value != 0) {
                try list.append(index * SIZEOF_U64 + bit);
            }
        }
    }
    return list;
}

fn requiredUlamBitVectorLength(b: u64) u64 {
    const fundamental_difference = KNOWN_DIFFERENCES[b];
    // The linear function y = f(x) where x is length of bit vector in
    // bits and y is number of Ulam values produced has slope = period
    // / fundamental_difference, but we want the inverse function, so
    // our slope is fundamental_difference / period. And the number of
    // Ulam values we want is equal to period. So our final value is
    // approximately fundamental_difference. Divide by 64 to convert
    // to u64s rather than bits.
    return fundamental_difference / 64;
}

fn getUlamTerm(allocator: std.mem.Allocator, b: u64, term_number: u64) AllocationError!u64 {
    const a = 2;
    const period = KNOWN_PERIODS[b];
    const fundamental_difference = KNOWN_DIFFERENCES[b];
    const bitvector_length = requiredUlamBitVectorLength(b) + 200; // Plus some padding :)
    const bits = try generateUlamBits(allocator, a, b, bitvector_length);
    const values = try compileUlamValues(allocator, bits);
    var shifted_term_number = term_number % period;
    var shift_count = term_number / period;
    if (shifted_term_number < 100) {
        shifted_term_number += period;
        shift_count -= 1;
    }
    const base_term = values.items[shifted_term_number];
    return base_term + shift_count * fundamental_difference;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const target_index = 100000000000; // 1-based index, per problem description

    var sum: u64 = 0;
    for (2..11) |n| {
        const b = 2 * n + 1;
        const term = try getUlamTerm(allocator, b, target_index - 1);
        sum += term;
    }
    std.debug.print("{}\n", .{sum});
}
