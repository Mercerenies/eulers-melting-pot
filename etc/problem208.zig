// Simplified version of 208_analysis.hs
//
// 5 seconds, runs correctly. Need 64-bit ints.
//
// 2.5 seconds after removing "impossible scenarios"

const std = @import("std");

const print = std.debug.print;

const XCoord = struct {
    const Self = @This();

    c: i64,
    c2: i64,

    pub fn add(self: *const Self, other: *const Self) Self {
        return .{ .c = self.c + other.c, .c2 = self.c2 + other.c2 };
    }
};

const YCoord = struct {
    const Self = @This();

    s: i64,
    sc: i64,

    pub fn add(self: *const Self, other: *const Self) Self {
      return .{ .s = self.s + other.s, .sc = self.sc + other.sc };
    }
};

const RobotPosition = struct {
    dir: i16,
    x: XCoord,
    y: YCoord,

    pub fn is_start(self: *const RobotPosition) bool {
      return self.x.c == 0 and self.x.c2 == 0 and self.y.s == 0 and self.y.sc == 0 and self.dir == 0;
    }
};

fn cos36n(n: i16) XCoord {
    return switch (n) {
        0 => XCoord{ .c = -2, .c2 = 4 },
        1 => XCoord{ .c = 1, .c2 = 0 },
        2 => XCoord{ .c = 2, .c2 = -2 },
        3 => XCoord{ .c = -2, .c2 = 2 },
        4 => XCoord{ .c = -1, .c2 = 0 },
        5 => XCoord{ .c = 2, .c2 = -4 },
        6 => XCoord{ .c = -1, .c2 = 0 },
        7 => XCoord{ .c = -2, .c2 = 2 },
        8 => XCoord{ .c = 2, .c2 = -2 },
        9 => XCoord{ .c = 1, .c2 = 0 },
        else => unreachable,
    };
}

fn sin36n(n: i16) YCoord {
    return switch (n) {
        0 => YCoord{ .s = 0, .sc = 0 },
        1 => YCoord{ .s = 1, .sc = 0 },
        2 => YCoord{ .s = 0, .sc = 2 },
        3 => YCoord{ .s = 0, .sc = 2 },
        4 => YCoord{ .s = 1, .sc = 0 },
        5 => YCoord{ .s = 0, .sc = 0 },
        6 => YCoord{ .s = -1, .sc = 0 },
        7 => YCoord{ .s = 0, .sc = -2 },
        8 => YCoord{ .s = 0, .sc = -2 },
        9 => YCoord{ .s = -1, .sc = 0 },
        else => unreachable,
    };
}

const HashKey = struct {
    pos: RobotPosition,
    remaining: i32,
};

// move_dir = 4 is clockwise
// move_dir = 1 is anticlockwise
fn apply_move(dir: i16, pos: *const RobotPosition) RobotPosition {
    const move_angle = if (dir == 4) @mod(2 * pos.dir + 9, 10) else @mod(2 * pos.dir + 1, 10);
    return .{
        .dir = @mod(pos.dir + dir, 5),
        .x = pos.x.add(&cos36n(move_angle)),
        .y = pos.y.add(&sin36n(move_angle)),
    };
}

fn count_paths(memo: *std.AutoHashMap(HashKey, i64), pos: RobotPosition, remaining: i32) !i64 {
    const key: HashKey = .{ .pos = pos, .remaining = remaining };

    if (remaining == 0) {
        return if (pos.is_start()) 1 else 0;
    } else if (memo.get(key)) |value| {
        return value;
    } else {
        // Impossible scenarios
        if (pos.x.c < - 2 * remaining or pos.x.c > 2 * remaining) {
            return 0;
        }
        if (pos.x.c2 < - 4 * remaining or pos.x.c2 > 4 * remaining) {
            return 0;
        }
        if (pos.y.s < - remaining or pos.y.s > remaining) {
            return 0;
        }
        if (pos.y.sc < - 2 * remaining or pos.y.sc > 2 * remaining) {
            return 0;
        }

        const clockwise = try count_paths(memo, apply_move(4, &pos), remaining - 1);
        const anticlockwise = try count_paths(memo, apply_move(1, &pos), remaining - 1);
        try memo.put(key, clockwise + anticlockwise);
        return clockwise + anticlockwise;
    }
}

pub fn main() !void {
    var memo: std.AutoHashMap(HashKey, i64) = .init(std.heap.page_allocator);
    defer memo.deinit();

    const start_position = RobotPosition{ .dir = 0, .x = .{ .c = 0, .c2 = 0 }, .y = .{ .s = 0, .sc = 0 } };
    const answer: i64 = try count_paths(&memo, start_position, 70);
    print("{}\n", .{answer});

    // Collect statistics
    print("Memo keys: {}\n", .{memo.count()});
    var min_c: i64 = 0;
    var max_c: i64 = 0;
    var min_c2: i64 = 0;
    var max_c2: i64 = 0;
    var min_s: i64 = 0;
    var max_s: i64 = 0;
    var min_sc: i64 = 0;
    var max_sc: i64 = 0;
    var iter = memo.keyIterator();
    while (iter.next()) |key| {
        min_c = min(min_c, key.pos.x.c);
        max_c = max(max_c, key.pos.x.c);
        min_c2 = min(min_c2, key.pos.x.c2);
        max_c2 = max(max_c2, key.pos.x.c2);
        min_s = min(min_s, key.pos.y.s);
        max_s = max(max_s, key.pos.y.s);
        min_sc = min(min_sc, key.pos.y.sc);
        max_sc = max(max_sc, key.pos.y.sc);
    }
    print("min_c = {}\n", .{min_c});
    print("max_c = {}\n", .{max_c});
    print("min_c2 = {}\n", .{min_c2});
    print("max_c2 = {}\n", .{max_c2});
    print("min_s = {}\n", .{min_s});
    print("max_s = {}\n", .{max_s});
    print("min_sc = {}\n", .{min_sc});
    print("max_sc = {}\n", .{max_sc});
}

fn min(a: i64, b: i64) i64 {
    if (a < b) return a else return b;
}

fn max(a: i64, b: i64) i64 {
    if (a > b) return a else return b;
}
