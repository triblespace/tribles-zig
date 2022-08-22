const std = @import("std");

const Card = @import("../Card.zig").Card;
const PACT = @import("../PACT.zig");

pub fn treeCard(self: PACT.Tree) Card {
    var card = Card.from(
\\┌────────────────────────────────────────────────────────────────────────────────┐
\\│ Tree                                                                           │
\\│━━━━━━                                                                          │
\\│        Count: ░░░░░░░░░░░░░░░░      Memory (keys): ░░░░░░░░░░░░░░░░            │
\\│   Node Count: ░░░░░░░░░░░░░░░░    Memory (actual): ░░░░░░░░░░░░░░░░            │
\\│  Alloc Count: ░░░░░░░░░░░░░░░░   Overhead (ratio): ░░░░░░░░░░░░░░░░            │
\\│                                                                                │
\\│  Node Distribution                                                             │
\\│ ═══════════════════                                                            │
\\│                                                                                │
\\│                                                      infix8 ░░░░░░░░░░░░░░░░   │
\\│                           branch1 ░░░░░░░░░░░░░░░░  infix16 ░░░░░░░░░░░░░░░░   │
\\│                           branch2 ░░░░░░░░░░░░░░░░  infix24 ░░░░░░░░░░░░░░░░   │
\\│                           branch4 ░░░░░░░░░░░░░░░░  infix32 ░░░░░░░░░░░░░░░░   │
\\│                           branch8 ░░░░░░░░░░░░░░░░  infix40 ░░░░░░░░░░░░░░░░   │
\\│   none ░░░░░░░░░░░░░░░░  branch16 ░░░░░░░░░░░░░░░░  infix48 ░░░░░░░░░░░░░░░░   │
\\│   leaf ░░░░░░░░░░░░░░░░  branch32 ░░░░░░░░░░░░░░░░  infix56 ░░░░░░░░░░░░░░░░   │
\\│   twig ░░░░░░░░░░░░░░░░  branch64 ░░░░░░░░░░░░░░░░  infix64 ░░░░░░░░░░░░░░░░   │
\\│                                                                                │
\\│  Density                                                                       │
\\│ ═════════                                                                      │
\\│                                                                                │
\\│       ┐░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░        │
\\│       │░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░        │
\\│       │░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░        │
\\│       │░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░        │
\\│       │░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░        │
\\│       │░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░        │
\\│       │░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░        │
\\│       ┘░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░        │
\\│       0┌──────────────┬───────────────┬───────────────┬───────────────┐63      │
\\└────────────────────────────────────────────────────────────────────────────────┘
    ) catch unreachable;

    const item_count = self.count();

    var node_count: u64 = 0;

    var mem_keys: u64 = item_count * PACT.key_length;

    var none_count: u64 = 0;
    var twig_count: u64 = 0;
    var leaf_count: u64 = 0;
    var branch_1_count: u64 = 0;
    var branch_2_count: u64 = 0;
    var branch_4_count: u64 = 0;
    var branch_8_count: u64 = 0;
    var branch_16_count: u64 = 0;
    var branch_32_count: u64 = 0;
    var branch_64_count: u64 = 0;
    var infix_8_count: u64 = 0;
    var infix_16_count: u64 = 0;
    var infix_24_count: u64 = 0;
    var infix_32_count: u64 = 0;
    var infix_40_count: u64 = 0;
    var infix_48_count: u64 = 0;
    var infix_56_count: u64 = 0;
    var infix_64_count: u64 = 0;

    var density_at_depth: [PACT.key_length]u64 = [_]u64{0} ** PACT.key_length;

    var node_iter = self.nodes();
    while (node_iter.next()) |res| {
        node_count += 1;
        density_at_depth[res.start_depth] += 1;
        switch (res.node.unknown.tag) {
            .none => none_count += 1,
            .leaf => leaf_count += 1,
            .twig => twig_count += 1,
            .branch1 => {
                branch_1_count += 1;},
            .branch2 => {
                branch_2_count += 1;},
            .branch4 => {
                branch_4_count += 1;},
            .branch8 => {
                branch_8_count += 1;},
            .branch16 => {
                branch_16_count += 1;},
            .branch32 => {
                branch_32_count += 1;},
            .branch64 => {
                branch_64_count += 1;},
            .infix8 => {
                infix_8_count += 1;},
            .infix16 => {
                infix_16_count += 1;},
            .infix24 => {
                infix_24_count += 1;},
            .infix32 => {
                infix_32_count += 1;},
            .infix40 => {
                infix_40_count += 1;},
            .infix48 => {
                infix_48_count += 1;},
            .infix56 => {
                infix_56_count += 1;},
            .infix64 => {
                infix_64_count += 1;},
        }
    }

    var max_density: u64 = 0;
    for (density_at_depth) |density| {
        max_density = std.math.max(max_density, density);
    }

    const mem_info = self.mem_info(); 

    const mem_overhead: f64 = (@intToFloat(f64, mem_info.active_memory)
                             - @intToFloat(f64, mem_keys))
                             / @intToFloat(f64, mem_keys);

    const string = try std.fmt.allocPrint(allocator, "{d:_>16}", .{ item_count });
    defer test_allocator.free(string);
    _ = std.fmt.bufPrint(&count_data, ) catch unreachable;

    var node_count_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&node_count_data, "{d:_>16}", .{node_count}) catch unreachable;
    var node_count_iter = (std.unicode.Utf8View.init(&node_count_data) catch unreachable).iterator();

    var alloc_count_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&alloc_count_data, "{d:_>16}", .{mem_info.allocation_count}) catch unreachable;
    var alloc_count_iter = (std.unicode.Utf8View.init(&alloc_count_data) catch unreachable).iterator();

    var mem_keys_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&mem_keys_data, "{d:_>16}", .{mem_keys}) catch unreachable;
    var mem_keys_iter = (std.unicode.Utf8View.init(&mem_keys_data) catch unreachable).iterator();

    var mem_actual_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&mem_actual_data, "{d:_>16}", .{mem_info.active_memory}) catch unreachable;
    var mem_actual_iter = (std.unicode.Utf8View.init(&mem_actual_data) catch unreachable).iterator();

    var mem_overhead_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&mem_overhead_data, "{d:_>16}", .{mem_overhead}) catch unreachable;
    var mem_overhead_iter = (std.unicode.Utf8View.init(&mem_overhead_data) catch unreachable).iterator();

    var none_count_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&none_count_data, "{d:_>16}", .{none_count}) catch unreachable;
    var none_count_iter = (std.unicode.Utf8View.init(&none_count_data) catch unreachable).iterator();

    var leaf_count_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&leaf_count_data, "{d:_>16}", .{leaf_count}) catch unreachable;
    var leaf_count_iter = (std.unicode.Utf8View.init(&leaf_count_data) catch unreachable).iterator();

    var twig_count_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&twig_count_data, "{d:_>16}", .{twig_count}) catch unreachable;
    var twig_count_iter = (std.unicode.Utf8View.init(&twig_count_data) catch unreachable).iterator();

    var branch_1_count_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&branch_1_count_data, "{d:_>16}", .{branch_1_count}) catch unreachable;
    var branch_1_count_iter = (std.unicode.Utf8View.init(&branch_1_count_data) catch unreachable).iterator();

    var branch_2_count_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&branch_2_count_data, "{d:_>16}", .{branch_2_count}) catch unreachable;
    var branch_2_count_iter = (std.unicode.Utf8View.init(&branch_2_count_data) catch unreachable).iterator();

    var branch_4_count_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&branch_4_count_data, "{d:_>16}", .{branch_4_count}) catch unreachable;
    var branch_4_count_iter = (std.unicode.Utf8View.init(&branch_4_count_data) catch unreachable).iterator();

    var branch_8_count_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&branch_8_count_data, "{d:_>16}", .{branch_8_count}) catch unreachable;
    var branch_8_count_iter = (std.unicode.Utf8View.init(&branch_8_count_data) catch unreachable).iterator();

    var branch_16_count_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&branch_16_count_data, "{d:_>16}", .{branch_16_count}) catch unreachable;
    var branch_16_count_iter = (std.unicode.Utf8View.init(&branch_16_count_data) catch unreachable).iterator();

    var branch_32_count_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&branch_32_count_data, "{d:_>16}", .{branch_32_count}) catch unreachable;
    var branch_32_count_iter = (std.unicode.Utf8View.init(&branch_32_count_data) catch unreachable).iterator();

    var branch_64_count_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&branch_64_count_data, "{d:_>16}", .{branch_64_count}) catch unreachable;
    var branch_64_count_iter = (std.unicode.Utf8View.init(&branch_64_count_data) catch unreachable).iterator();

    var infix_8_count_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&infix_8_count_data, "{d:_>16}", .{infix_8_count}) catch unreachable;
    var infix_8_count_iter = (std.unicode.Utf8View.init(&infix_8_count_data) catch unreachable).iterator();

    var infix_16_count_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&infix_16_count_data, "{d:_>16}", .{infix_16_count}) catch unreachable;
    var infix_16_count_iter = (std.unicode.Utf8View.init(&infix_16_count_data) catch unreachable).iterator();

    var infix_24_count_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&infix_24_count_data, "{d:_>16}", .{infix_24_count}) catch unreachable;
    var infix_24_count_iter = (std.unicode.Utf8View.init(&infix_24_count_data) catch unreachable).iterator();

    var infix_32_count_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&infix_32_count_data, "{d:_>16}", .{infix_32_count}) catch unreachable;
    var infix_32_count_iter = (std.unicode.Utf8View.init(&infix_32_count_data) catch unreachable).iterator();

    var infix_40_count_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&infix_40_count_data, "{d:_>16}", .{infix_40_count}) catch unreachable;
    var infix_40_count_iter = (std.unicode.Utf8View.init(&infix_40_count_data) catch unreachable).iterator();

    var infix_48_count_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&infix_48_count_data, "{d:_>16}", .{infix_48_count}) catch unreachable;
    var infix_48_count_iter = (std.unicode.Utf8View.init(&infix_48_count_data) catch unreachable).iterator();

    var infix_56_count_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&infix_56_count_data, "{d:_>16}", .{infix_56_count}) catch unreachable;
    var infix_56_count_iter = (std.unicode.Utf8View.init(&infix_56_count_data) catch unreachable).iterator();

    var infix_64_count_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&infix_64_count_data, "{d:_>16}", .{infix_64_count}) catch unreachable;
    var infix_64_count_iter = (std.unicode.Utf8View.init(&infix_64_count_data) catch unreachable).iterator();

    for (card.grid) |*row, global_y| {
        for (row.*) |*cell, global_x| {
                    const x: u64 = global_x - density_pos.x;
                    const y: u64 = global_y - density_pos.y;

                    const density = @intToFloat(f64, density_at_depth[x]);
                    const norm_density = density / @intToFloat(f64, max_density);

                    const s: u21 = if (norm_density > (@intToFloat(f64, (7 - y)) * (1.0 / 8.0))) '█' else ' ';
                    break :blk s;
                },
                else => cell.*,
            };
        }
    }

    return card;
}

// pub fn branchNodeCard(self: PACT.Node, bucket_count: usize) Card {
//             var card = Card.from(
// \\┌────────────────────────────────────────────────────────────────────────────────┐
// \\│ Branch Node @󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀                                                  │
// \\│━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━                                                  │
// \\│                                                                                │
// \\│ Metadata                                                                       │
// \\│ ═════════                                                                      │
// \\│                                                                                │
// \\│   Hash: 󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁    Leafs: 󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂        │
// \\│   Ref#: 󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃                Depth: 󰀇󰀇                          │
// \\│                                                                                │
// \\│ Infix                                                                          │
// \\│ ══════                                                                         │
// \\│                                                                                │
// \\│   Head: 󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅                                                           │
// \\│                                                                                │
// \\│   Body: 󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆       │
// \\│         󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆       │
// \\│         ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔         │
// \\│ Children                                                                       │
// \\│ ══════════                                                                     │
// \\│                         TODO add %      0123456789ABCDEF     0123456789ABCDEF  │
// \\│  ▼                                     ┌────────────────┐   ┌────────────────┐ │
// \\│  ┌                  ● Seq Hash       0_│󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏│ 8_│󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐│ │
// \\│  │󰀈                 ◆ Rand Hash      1_│󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏│ 9_│󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐│ │
// \\│  │󰀉                 ○ Seq Missing    2_│󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏│ A_│󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐│ │
// \\│  │󰀊󰀊                ◇ Rand Missing   3_│󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏│ B_│󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐│ │
// \\│  │󰀋󰀋󰀋󰀋                               4_│󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏│ C_│󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐│ │
// \\│  │󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌                           5_│󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏│ D_│󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐│ │
// \\│  │󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍                   6_│󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏│ E_│󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐│ │
// \\│  │󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎   7_│󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏│ F_│󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐│ │
// \\│  └                                     └────────────────┘   └────────────────┘ │
// \\└────────────────────────────────────────────────────────────────────────────────┘
//             ) catch unreachable;

//             var addr_data: [16:0]u8 = undefined;
//             _ = std.fmt.bufPrint(&addr_data, "{x:0>16}", .{@ptrToInt(self.body)}) catch unreachable;
//             var addr_iter = (std.unicode.Utf8View.init(&addr_data) catch unreachable).iterator();

//             var hash_data: [32:0]u8 = undefined;
//             _ = std.fmt.bufPrint(&hash_data, "{s:_>32}", .{std.fmt.fmtSliceHexUpper(&self.body.child_sum_hash.data)}) catch unreachable;
//             var hash_iter = (std.unicode.Utf8View.init(&hash_data) catch unreachable).iterator();

//             var leaf_count_data: [20:0]u8 = undefined;
//             _ = std.fmt.bufPrint(&leaf_count_data, "{d:_>20}", .{self.body.leaf_count}) catch unreachable;
//             var leaf_count_iter = (std.unicode.Utf8View.init(&leaf_count_data) catch unreachable).iterator();

//             var ref_count_data: [20:0]u8 = undefined;
//             _ = std.fmt.bufPrint(&ref_count_data, "{d:_>20}", .{self.body.ref_count}) catch unreachable;
//             var ref_count_iter = (std.unicode.Utf8View.init(&ref_count_data) catch unreachable).iterator();

//             var head_infix_data: [12:0]u8 = undefined;
//             _ = std.fmt.bufPrint(&head_infix_data, "{s:_>12}", .{std.fmt.fmtSliceHexUpper(&self.infix)}) catch unreachable;
//             var head_infix_iter = (std.unicode.Utf8View.init(&head_infix_data) catch unreachable).iterator();

//             var branch_depth_data: [2:0]u8 = undefined;
//             _ = std.fmt.bufPrint(&branch_depth_data, "{d:_>2}", .{self.branch_depth}) catch unreachable;
//             var branch_depth_iter = (std.unicode.Utf8View.init(&branch_depth_data) catch unreachable).iterator();

//             const lower_childset_pos = card.findTopLeft('\u{F000F}').?;
//             const upper_childset_pos = card.findTopLeft('\u{F0010}').?;

//             for (card.grid) |*row, y| {
//                 for (row.*) |*cell, x| {
//                     cell.* = switch (cell.*) {
//                         '\u{F0000}' => addr_iter.nextCodepoint().?,
//                         '\u{F0001}' => hash_iter.nextCodepoint().?,
//                         '\u{F0002}' => leaf_count_iter.nextCodepoint() orelse unreachable,
//                         '\u{F0003}' => ref_count_iter.nextCodepoint() orelse unreachable,
//                         '\u{F0005}' => head_infix_iter.nextCodepoint() orelse unreachable,
//                         '\u{F0006}' => '_',
//                         '\u{F0007}' => branch_depth_iter.nextCodepoint() orelse unreachable,
//                         '\u{F0008}' => if (bucket_count >= 1) '█' else '░',
//                         '\u{F0009}' => if (bucket_count >= 2) '█' else '░',
//                         '\u{F000A}' => if (bucket_count >= 4) '█' else '░',
//                         '\u{F000B}' => if (bucket_count >= 8) '█' else '░',
//                         '\u{F000C}' => if (bucket_count >= 16) '█' else '░',
//                         '\u{F000D}' => if (bucket_count >= 32) '█' else '░',
//                         '\u{F000E}' => if (bucket_count >= 64) '█' else '░',
//                         '\u{F000F}' => blk: {
//                             const lx: u8 = @intCast(u8, x) - lower_childset_pos.x;
//                             const ly: u8 = @intCast(u8, y) - lower_childset_pos.y;
//                             const byte_key: u8 = @as(u8, lx + (ly * 16));

//                             if (!self.body.child_set.isSet(byte_key)) break :blk ' ';

//                             var s: u21 = undefined;
//                             const rand_hash_used = self.body.rand_hash_used.isSet(byte_key);

//                             const bucket_index = hashByteKey(rand_hash_used, bucket_count, byte_key);
//                             if (!self.body.buckets[bucket_index].get(self.branch_depth, byte_key).isNone()) {
//                                 s = if (rand_hash_used) '◆' else '●';
//                             } else {
//                                 s = if (rand_hash_used) '◇' else '○';
//                             }

//                             break :blk s;
//                         },
//                         '\u{F0010}' => blk: {
//                             const lx: u8 = @intCast(u8, x) - upper_childset_pos.x;
//                             const ly: u8 = @intCast(u8, y) - upper_childset_pos.y;
//                             const byte: u8 = @as(u8, 128 + lx + (ly * 16));
//                             if (!self.body.child_set.isSet(byte)) break :blk ' ';
//                             const s: u21 = if (self.body.rand_hash_used.isSet(byte)) '◆' else '●';
//                             break :blk s;
//                         },
//                         else => cell.*,
//                     };
//                 }
//             }

//             return card;
//             }