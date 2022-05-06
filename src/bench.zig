const std = @import("std");
const testing = std.testing;
const time = std.time;

const coz = @import("./coz.zig");

const Trible = @import("Trible.zig").Trible;
const PACT = @import("./PACT.zig");
const TribleSet = @import("./TribleSet.zig").TribleSet;
const keyHash = @import("./PACT.zig").keyHash;

const sample_size: usize = 1;
const data_size: usize = 1000000;
const change_prob = 0.1;

pub fn main() !void {
    PACT.init();
    var i: u64 = 0;
    while (i < sample_size) : (i += 1) {
        try benchmark_tribleset_write();
    }
    //try benchmark_hashing();
    //try benchmark_std();
}

pub fn benchmark_tribleset_write() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer _ = arena.deinit();
    //var gp = std.heap.GeneralPurposeAllocator(.{}){};
    //defer _ = gp.deinit();

    var timer = try time.Timer.start();
    var t_total: u64 = 0;

    var rnd = std.rand.DefaultPrng.init(0).random();

    var set = TribleSet.init(arena.allocator());
    defer set.deinit();

    std.debug.print("Inserting {d} tribles into TribleSet.\n", .{data_size});

    var i: u64 = 0;
    var t = Trible.initAribitrary(rnd);

    coz.begin("insert");
    while (i < data_size) : (i += 1) {
        t = Trible.initAribitraryLike(rnd, change_prob, t);
        //const value = rnd.int(usize);

        timer.reset();

        try set.put(&t);
        coz.progress("put");

        t_total += timer.lap();
    }
    coz.end("insert");

    std.debug.print("Inserted {d} in {d}ns\n", .{ i, t_total });

    std.debug.print("{s}\n", .{set.mem_info()});
}

pub fn benchmark_pact_write() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer _ = arena.deinit();
    //var gp = std.heap.GeneralPurposeAllocator(.{}){};
    //defer _ = gp.deinit();

    var timer = try time.Timer.start();
    var t_total: u64 = 0;

    var rnd = std.rand.DefaultPrng.init(0).random();

    var tree = PACT.Tree.init(arena.allocator());
    //var tree = PACT.Tree.init(gp.allocator());
    defer tree.deinit();

    //std.debug.print("Inserting {d} tribles into PACT.\n", .{data_size});

    var i: u64 = 0;
    var t = Trible.initAribitrary(rnd);

    coz.begin("insert");
    while (i < data_size) : (i += 1) {
        t = Trible.initAribitraryLike(rnd, change_prob, t);
        //const value = rnd.int(usize);

        timer.reset();

        try tree.put(t.data, null);
        coz.progress("put");

        t_total += timer.lap();
    }
    coz.end("insert");

    std.debug.print("Inserted {d} in {d}ns\n", .{ i, t_total });

    std.debug.print("{s}\n", .{tree});

    // var node_iter = tree.nodes();
    // while(node_iter.next()) |res| {
    //     std.debug.print("Depth: {d}\n{s}\n", .{res.start_depth, res.node});
    // }

}

pub fn benchmark_pact_iterate() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer _ = arena.deinit();
    //var gp = std.heap.GeneralPurposeAllocator(.{}){};
    //defer _ = gp.deinit();

    var timer = try time.Timer.start();
    var t_total: u64 = 0;

    var rnd = std.rand.DefaultPrng.init(0).random();

    var tree = PACT.Tree.init(arena.allocator());
    //var tree = PACT.Tree.init(gp.allocator());
    defer tree.deinit();

    var t = Trible.initAribitrary(rnd);

    var i: u64 = 0;
    while (i < data_size) : (i += 1) {
        t = Trible.initAribitraryLike(rnd, change_prob, t);
        try tree.put(&t.data, null);
    }

    std.debug.print("Iterating nodes of PACT with {d} tribles.\n", .{data_size});
    timer.reset();
    coz.begin("iterate");
    var j: u64 = 0;
    var node_iter = tree.nodes();
    while(node_iter.next()) |_| {
        j += 1;
        coz.progress("next");
    }
    coz.end("iterate");
    t_total += timer.lap();

    std.debug.print("Iterated {d} in {d}ns\n", .{ j, t_total });
}

pub fn benchmark_hashing() !void {
    var timer = try time.Timer.start();
    var t_total: u64 = 0;

    var rnd = std.rand.DefaultPrng.init(0).random();

    std.debug.print("Hashing {d} tribles.\n", .{data_size});

    var i: u64 = 0;
    var t = Trible.initAribitrary(rnd);
    while (i < data_size) : (i += 1) {
        t = Trible.initAribitraryLike(rnd, change_prob, t);

        timer.reset();

        _ = keyHash(&t.data);

        t_total += timer.lap();
    }

    std.debug.print("Hashed {d} in {d}ns\n", .{ data_size, t_total });
}

pub fn benchmark_std() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = general_purpose_allocator.allocator();

    var timer = try time.Timer.start();
    var t_total: u64 = 0;

    var rnd = std.rand.DefaultPrng.init(0).random();

    var map = std.hash_map.AutoHashMap(Trible, ?usize).init(gpa);
    defer map.deinit();

    std.debug.print("Inserting {d} tribles into AutoHashMap.\n", .{data_size});

    var i: u64 = 0;
    var t = Trible.initAribitrary(rnd);
    while (i < data_size) : (i += 1) {
        t = Trible.initAribitraryLike(rnd, change_prob, t);
        //const value = rnd.int(usize);

        timer.reset();

        try map.put(t, null);

        t_total += timer.lap();
    }

    std.debug.print("Inserted {d} in {d}ns\n", .{ data_size, t_total });
}
