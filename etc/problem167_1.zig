
// Ulam sequence generator using []bool and bit arithmetic. See https://math.stackexchange.com/questions/2165222/generating-ulam-sequences-using-bit-manipulation (specifically the answer that changed the function slightly)

const std = @import("std");

const AllocationError = error {
    OutOfMemory,
};

fn generateUlamBits(allocator: std.mem.Allocator, a: u64, b: u64, length: usize) AllocationError![]bool {
    // Zero-initialize the array
    const ulam_array = try allocator.alloc(bool, length);
    for (ulam_array) |*item| {
        item.* = false;
    }

    // Zero-initialize the "false" array
    const non_ulam_array = try allocator.alloc(bool, length);
    for (non_ulam_array) |*item| {
        item.* = false;
    }

    var k = b;
    ulam_array[a] = true;
    ulam_array[b] = true;

    while (k < length) {
        // Go from 1 up to (exclusive) k, but don't let k + i
        // run off the end of the array.
        for (1..@min(k, length - k)) |i| {
            const j = k + i;
            non_ulam_array[j] = non_ulam_array[j] or (ulam_array[i] and ulam_array[j]);
            ulam_array[j] = (ulam_array[j] or ulam_array[i]) and !non_ulam_array[j];
        }
        // Calculate new k.
        for (k+1..length) |new_k| {
            if (ulam_array[new_k]) {
                k = new_k;
                break;
            }
        } else {
            break;
        }
    }

    return ulam_array;
}

fn compileUlamValues(allocator: std.mem.Allocator, bits: []bool) AllocationError!std.ArrayList(u64) {
    var list = std.ArrayList(u64).init(allocator);
    for (bits, 0..) |bit_value, index| {
        if (bit_value) {
            try list.append(index);
        }
    }
    return list;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const bits = try generateUlamBits(allocator, 2, 5, 100000);
    const values = try compileUlamValues(allocator, bits);
    std.debug.print("{any}\n", .{values.items.len});
}
