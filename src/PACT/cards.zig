const std = @import("std");

const Card = @import("../Card.zig").Card;
const PACT = @import("../PACT.zig");

pub fn treeCard(self: PACT.Tree) Card {
    var card = Card.from(
\\┌────────────────────────────────────────────────────────────────────────────────┐
\\│ Tree                                                                           │
\\│━━━━━━                                                                          │
\\│        Count: 󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀      Memory (keys): 󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃            │
\\│   Node Count: 󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁    Memory (actual): 󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄            │
\\│  Alloc Count: 󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂   Overhead (ratio): 󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅            │
\\│                                                                                │
\\│  Node Distribution                                                             │
\\│ ═══════════════════                                                            │
\\│                                                                                │
\\│                                                      infix8 󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐   │
\\│                           branch1 󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉  infix16 󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑   │
\\│                           branch2 󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊  infix24 󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒   │
\\│                           branch4 󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋  infix32 󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓   │
\\│                           branch8 󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌  infix40 󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔   │
\\│   none 󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆  branch16 󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍  infix48 󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕   │
\\│   leaf 󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇  branch32 󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎  infix56 󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖   │
\\│   twig 󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈  branch64 󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏  infix64 󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗   │
\\│                                                                                │
\\│  Density                                                                       │
\\│ ═════════                                                                      │
\\│                                                                                │
\\│       ┐󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘        │
\\│       │󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘        │
\\│       │󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘        │
\\│       │󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘        │
\\│       │󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘        │
\\│       │󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘        │
\\│       │󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘        │
\\│       ┘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘        │
\\│       0┌──────────────┬───────────────┬───────────────┬───────────────┐63      │
\\└────────────────────────────────────────────────────────────────────────────────┘
    ) catch unreachable;

    const item_count = self.count();

    var node_count: u64 = 0;

    var alloc_count: u64 = 0;

    var mem_keys: u64 = item_count * PACT.key_length;

    var mem_actual: u64 = 0;

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
                branch_1_count += 1;
                alloc_count += 1;},
            .branch2 => {
                branch_2_count += 1;
                alloc_count += 1;},
            .branch4 => {
                branch_4_count += 1;
                alloc_count += 1;},
            .branch8 => {
                branch_8_count += 1;
                alloc_count += 1;},
            .branch16 => {
                branch_16_count += 1;
                alloc_count += 1;},
            .branch32 => {
                branch_32_count += 1;
                alloc_count += 1;},
            .branch64 => {
                branch_64_count += 1;
                alloc_count += 1;},
            .infix8 => {
                infix_8_count += 1;
                alloc_count += 1;},
            .infix16 => {
                infix_16_count += 1;
                alloc_count += 1;},
            .infix24 => {
                infix_24_count += 1;
                alloc_count += 1;},
            .infix32 => {
                infix_32_count += 1;
                alloc_count += 1;},
            .infix40 => {
                infix_40_count += 1;
                alloc_count += 1;},
            .infix48 => {
                infix_48_count += 1;
                alloc_count += 1;},
            .infix56 => {
                infix_56_count += 1;
                alloc_count += 1;},
            .infix64 => {
                infix_64_count += 1;
                alloc_count += 1;},
        }
    }

    var max_density: u64 = 0;
    for (density_at_depth) |density| {
        max_density = std.math.max(max_density, density);
    }

    mem_actual =   branch_1_count * @sizeOf(PACT.BranchNodeBase.Body)         //
                    + branch_2_count * @sizeOf(PACT.BranchNode(2).Body)         //
                    + branch_4_count * @sizeOf(PACT.BranchNode(4).Body)         //
                    + branch_8_count * @sizeOf(PACT.BranchNode(8).Body)         //
                    + branch_16_count * @sizeOf(PACT.BranchNode(16).Body)       //
                    + branch_32_count * @sizeOf(PACT.BranchNode(32).Body)       //
                    + branch_64_count * @sizeOf(PACT.BranchNode(64).Body)       //
                    + infix_8_count * @sizeOf(PACT.InfixNode(8).Body)   //
                    + infix_16_count * @sizeOf(PACT.InfixNode(16).Body) //
                    + infix_24_count * @sizeOf(PACT.InfixNode(24).Body) //
                    + infix_32_count * @sizeOf(PACT.InfixNode(32).Body) //
                    + infix_40_count * @sizeOf(PACT.InfixNode(40).Body) //
                    + infix_48_count * @sizeOf(PACT.InfixNode(48).Body) //
                    + infix_56_count * @sizeOf(PACT.InfixNode(56).Body) //
                    + infix_64_count * @sizeOf(PACT.InfixNode(64).Body);

    const mem_overhead: f64 = @intToFloat(f64, mem_actual) / @intToFloat(f64, mem_keys);

    var count_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&count_data, "{d:_>16}", .{ item_count }) catch unreachable;
    var count_iter = (std.unicode.Utf8View.init(&count_data) catch unreachable).iterator();

    var node_count_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&node_count_data, "{d:_>16}", .{node_count}) catch unreachable;
    var node_count_iter = (std.unicode.Utf8View.init(&node_count_data) catch unreachable).iterator();

    var alloc_count_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&alloc_count_data, "{d:_>16}", .{alloc_count}) catch unreachable;
    var alloc_count_iter = (std.unicode.Utf8View.init(&alloc_count_data) catch unreachable).iterator();

    var mem_keys_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&mem_keys_data, "{d:_>16}", .{mem_keys}) catch unreachable;
    var mem_keys_iter = (std.unicode.Utf8View.init(&mem_keys_data) catch unreachable).iterator();

    var mem_actual_data: [16:0]u8 = undefined;
    _ = std.fmt.bufPrint(&mem_actual_data, "{d:_>16}", .{mem_actual}) catch unreachable;
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

    const density_pos = card.findTopLeft('\u{F0018}').?;

    for (card.grid) |*row, global_y| {
        for (row.*) |*cell, global_x| {
            cell.* = switch (cell.*) {
                '\u{F0000}' => count_iter.nextCodepoint().?,
                '\u{F0001}' => node_count_iter.nextCodepoint().?,
                '\u{F0002}' => alloc_count_iter.nextCodepoint().?,
                '\u{F0003}' => mem_keys_iter.nextCodepoint().?,
                '\u{F0004}' => mem_actual_iter.nextCodepoint().?,
                '\u{F0005}' => mem_overhead_iter.nextCodepoint().?,
                '\u{F0006}' => none_count_iter.nextCodepoint().?,
                '\u{F0007}' => leaf_count_iter.nextCodepoint().?,
                '\u{F0008}' => twig_count_iter.nextCodepoint().?,
                '\u{F0009}' => branch_1_count_iter.nextCodepoint().?,
                '\u{F000A}' => branch_2_count_iter.nextCodepoint().?,
                '\u{F000B}' => branch_4_count_iter.nextCodepoint().?,
                '\u{F000C}' => branch_8_count_iter.nextCodepoint().?,
                '\u{F000D}' => branch_16_count_iter.nextCodepoint().?,
                '\u{F000E}' => branch_32_count_iter.nextCodepoint().?,
                '\u{F000F}' => branch_64_count_iter.nextCodepoint().?,
                '\u{F0010}' => infix_8_count_iter.nextCodepoint().?,
                '\u{F0011}' => infix_16_count_iter.nextCodepoint().?,
                '\u{F0012}' => infix_24_count_iter.nextCodepoint().?,
                '\u{F0013}' => infix_32_count_iter.nextCodepoint().?,
                '\u{F0014}' => infix_40_count_iter.nextCodepoint().?,
                '\u{F0015}' => infix_48_count_iter.nextCodepoint().?,
                '\u{F0016}' => infix_56_count_iter.nextCodepoint().?,
                '\u{F0017}' => infix_64_count_iter.nextCodepoint().?,
                '\u{F0018}' => blk: {
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