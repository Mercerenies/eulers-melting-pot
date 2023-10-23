
// Ulam sequence generator using []i64 and complicated bit arithmetic. See https://math.stackexchange.com/questions/2165222/generating-ulam-sequences-using-bit-manipulation (specifically the answer that changed the function slightly)

const std = @import("std");

const AllocationError = error {
    OutOfMemory,
};

const SIZEOF_U64 = @sizeOf(u64);

inline fn bitOf(value: u64, bit_number: u6) u1 {
    return @truncate(value >> bit_number);
}

inline fn setBitOf(value: *u64, bit_number: u6, new_bit_value: u1) void {
    const current_value = value.* & (@as(u64, 1) << bit_number);
    const flip_value = current_value ^ (@as(u64, new_bit_value) << bit_number);
    value.* = value.* ^ flip_value;
}

inline fn doUlamSumSlow(ulam_array: []u64, non_ulam_array: []u64, k: u64) void {
    // The slow version; do each bit individually.
    const upper_bound = @min(k, ulam_array.len - k);
    for (1..upper_bound) |i| {
        const j = k + i;
        const src_bit = bitOf(ulam_array[i / SIZEOF_U64], @truncate(i % SIZEOF_U64));
        const dest_bit = bitOf(ulam_array[j / SIZEOF_U64], @truncate(j % SIZEOF_U64));
        var non_ulam_bit = bitOf(non_ulam_array[j / SIZEOF_U64], @truncate(j % SIZEOF_U64));
        non_ulam_bit |= (src_bit & dest_bit);
        setBitOf(&non_ulam_array[j / SIZEOF_U64], @truncate(j % SIZEOF_U64), non_ulam_bit);
        setBitOf(&ulam_array[j / SIZEOF_U64], @truncate(j % SIZEOF_U64), (dest_bit | src_bit) & ~non_ulam_bit);
    }
}

inline fn doUlamSum(ulam_array: []u64, non_ulam_array: []u64, k: u64) void {
    const upper_bound = @min(k, ulam_array.len - k);
    _ = upper_bound;

    // I want to go from 1 up to (exclusive) upper_bound. Start by
    // getting aligned to a u64.
    //const init_span = SIZEOF_U64 - ((k + 1) % SIZEOF_U64);

    //for (1..upper_bound) |i| {
    //    const j = k + i;
    //    non_ulam_array[j] = non_ulam_array[j] or (ulam_array[i] and ulam_array[j]);
    //    ulam_array[j] = (ulam_array[j] or ulam_array[i]) and !non_ulam_array[j];
    //}
    doUlamSumSlow(ulam_array, non_ulam_array, k);
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
    setBitOf(&ulam_array[a / SIZEOF_U64], @truncate(a), 1);
    setBitOf(&ulam_array[b / SIZEOF_U64], @truncate(b), 1);

    while (k < length) {
        doUlamSum(ulam_array, non_ulam_array, k);
        // Calculate new k.
        for (k+1..length) |new_k| {
            if (bitOf(ulam_array[new_k / SIZEOF_U64], @truncate(new_k % SIZEOF_U64)) != 0) {
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

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const bits = try generateUlamBits(allocator, 1, 2, 200);
    const values = try compileUlamValues(allocator, bits);
    std.debug.print("{any}\n", .{values});
}
