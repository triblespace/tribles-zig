const std = @import("std");
const testing = std.testing;
const time = std.time;

const coz = @import("./coz.zig");

const Trible = @import("Trible.zig").Trible;
const PACT = @import("./PACT.zig").PACT;
const keyHash = @import("./PACT.zig").keyHash;

const benchmark_size: usize = 100000;
const change_prob = 0.1;

pub fn main() !void {
    try benchmark(); 
    try benchmark_hashing();
    try benchmark_std();
}

pub fn benchmark() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = general_purpose_allocator.deinit();
    const gpa = general_purpose_allocator.allocator();

    var timer = try time.Timer.start();
    var t_total: u64 = 0;

    var rnd = std.rand.DefaultPrng.init(0).random();

    var tree = PACT.Tree.init(gpa);
    defer tree.deinit();

    std.debug.print("Inserting {d} tribles into PACT.\n", .{benchmark_size});

    var i: u64 = 0;
    var t = Trible.initAribitrary(rnd);

    coz.begin("insert");
    while (i < benchmark_size) : (i += 1) {
        t = Trible.initAribitraryLike(rnd, change_prob, t);
        //const value = rnd.int(usize);

        timer.reset();

        try tree.put(&t.data, null);
        coz.progress();

        t_total += timer.lap();
    }
    coz.end("insert");

    std.debug.print("Inserted {d} in {d}ns\n", .{ benchmark_size, t_total });

    std.debug.print("{s}\n", .{tree});

    // var node_iter = tree.nodes();
    // while(node_iter.next()) |res| {
    //      std.debug.print("Depth: {d}..{d}\n{s}\n", .{res.start_depth, res.node.depth(), res.node});
    // }

}

pub fn benchmark_hashing() !void {
    var timer = try time.Timer.start();
    var t_total: u64 = 0;

    var rnd = std.rand.DefaultPrng.init(0).random();

    std.debug.print("Hashing {d} tribles.\n", .{benchmark_size});

    var i: u64 = 0;
    var t = Trible.initAribitrary(rnd);
    while (i < benchmark_size) : (i += 1) {
        t = Trible.initAribitraryLike(rnd, change_prob, t);

        timer.reset();

        _ = keyHash(&t.data);

        t_total += timer.lap();
    }

    std.debug.print("Hashed {d} in {d}ns\n", .{ benchmark_size, t_total });
}

pub fn benchmark_std() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = general_purpose_allocator.allocator();

    var timer = try time.Timer.start();
    var t_total: u64 = 0;

    var rnd = std.rand.DefaultPrng.init(0).random();

    var map = std.hash_map.AutoHashMap(Trible, ?usize).init(gpa);
    defer map.deinit();

    std.debug.print("Inserting {d} tribles into AutoHashMap.\n", .{benchmark_size});

    var i: u64 = 0;
    var t = Trible.initAribitrary(rnd);
    while (i < benchmark_size) : (i += 1) {
        t = Trible.initAribitraryLike(rnd, change_prob, t);
        //const value = rnd.int(usize);

        timer.reset();

        try map.put(t, null);

        t_total += timer.lap();
    }

    std.debug.print("Inserted {d} in {d}ns\n", .{ benchmark_size, t_total });
}