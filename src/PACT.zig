const std = @import("std");
const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;
const ByteBitset = @import("ByteBitset.zig").ByteBitset;
const Card = @import("Card.zig").Card;
const MemInfo = @import("./MemInfo.zig").MemInfo;
const cards = @import("./PACT/cards.zig");
const hash = @import("./PACT/Hash.zig");
const Hash = hash.Hash;
const MinHash = hash.MinHash;

const mem = std.mem;

pub fn init() void {
    hash.init();
}

/// The Tries branching factor, fixed to the number of elements
/// that can be represented by a byte/8bit.
const BRANCH_FACTOR = 256;

/// The number of hashes used in the cuckoo table.
const HASH_COUNT = 2;

/// The maximum number of cuckoo displacements atempted during
/// insert before the size of the table is increased.
const MAX_ATTEMPTS = 4;

/// A byte -> byte lookup table used in hashes as permutations.
const Byte_LUT = [256]u8;

/// The permutation LUTs of multiple hashes arranged for better
/// memory locality.
const Hash_LUT = [256][HASH_COUNT]u8;

/// Checks if a LUT is a permutation
fn is_permutation(lut: *const Byte_LUT) bool {
    var seen = ByteBitset.initEmpty();
    for (lut) |x| {
        seen.set(x);
    }

    return std.meta.eql(seen, ByteBitset.initFull());
}

/// Choose a random element from a bitset.
/// Generate a LUT where each input maps to itself.
fn generate_identity_LUT() Byte_LUT {
    var lut: Byte_LUT = undefined;
    for (lut) |*element, i| {
        element.* = @intCast(u8, i);
    }
    return lut;
}

/// Generate a LUT where each input maps to the input in reverse
/// bit order, e.g. 0b00110101 -> 0b10101100
fn generate_bitReverse_LUT() Byte_LUT {
    var lut: Byte_LUT = undefined;
    for (lut) |*element, i| {
        element.* = @bitReverse(u8, @intCast(u8, i));
    }
    assert(is_permutation(&lut));
    return lut;
}

fn random_choice(rng: std.rand.Random, set: ByteBitset) ?u8 {
    if (set.isEmpty()) return null;

    var possible_values: [256]u8 = undefined;
    var possible_values_len: usize = 0;

    var set_iterator = set;
    while (set_iterator.drainNext(true)) |b| {
        possible_values[possible_values_len] = @intCast(u8, b);
        possible_values_len += 1;
    }

    const rand_index: u8 = @intCast(u8, rng.uintLessThan(usize, possible_values_len));
    return possible_values[rand_index];
}

fn generate_rand_LUT_helper(rng: std.rand.Random, dependencies: []const Byte_LUT, i: usize, remaining: ByteBitset, mask: u8, lut: *Byte_LUT) bool {
    if (i == 256) return true;

    var candidates = remaining;
    var iter = remaining;
    while (iter.drainNext(true)) |candidate| {
        for (dependencies) |d| {
            if ((d[i] & mask) == (candidate & mask)) {
                candidates.unset(candidate);
            }
        }
    }
    while (random_choice(rng, candidates)) |candidate| {
        var new_remaining = remaining;
        new_remaining.unset(candidate);
        candidates.unset(candidate);
        lut[i] = candidate;
        if (generate_rand_LUT_helper(rng, dependencies, i + 1, new_remaining, mask, lut)) {
            return true;
        }
    } else {
        return false;
    }
}

fn generate_rand_LUT(
    rng: std.rand.Random,
    dependencies: []const Byte_LUT,
    mask: u8,
) Byte_LUT {
    var lut: Byte_LUT = undefined;
    if (!generate_rand_LUT_helper(rng, dependencies, 0, ByteBitset.initFull(), mask, &lut)) unreachable;
    return lut;
}

fn generate_hash_LUTs(comptime rng: std.rand.Random) Hash_LUT {
    var luts: [HASH_COUNT]Byte_LUT = undefined;

    luts[0] = generate_bitReverse_LUT();
    for (luts[1..]) |*lut, i| {
        lut.* = generate_rand_LUT(rng, luts[0..i], HASH_COUNT - 1);
    }

    var hash_lut: Hash_LUT = undefined;

    for (luts) |lut, i| {
        for (lut) |h, j| {
            hash_lut[j][i] = h;
        }
    }

    return hash_lut;
}

/// Generates a byte -> byte lookup table for pearson hashing.
fn generate_pearson_LUT(comptime rng: std.rand.Random) Byte_LUT {
    const no_deps = [0]Byte_LUT{};
    const lut = generate_rand_LUT(rng, no_deps[0..], 0b11111111);
    assert(is_permutation(&lut));
    return lut;
}

fn depth_to_infix(comptime infix_len: u8, infix_end: u8, depth: u8) u8 {
    return (depth + infix_len) - infix_end;
}

const allocError = std.mem.Allocator.Error;

const NodeTag = enum(u8) {
    none,
    branch1,
    branch2,
    branch4,
    branch8,
    branch16,
    branch32,
    branch64,
    infix8,
    infix16,
    infix24,
    infix32,
    infix40,
    infix48,
    infix56,
    infix64,
    leaf,
    twig,
};

pub fn PACT(comptime segments: []const u8, comptime segment_size: u8, T: type) type {

    return struct {
        pub const key_length = blk: {
            var segment_sum = 0;
            for (segments) |segment| {
                segment_sum += segment;
            }
            break :blk segment_sum;
        };
        pub const value_type = T;

        const segment_lut: [key_length + 1]u8 = blk: {
            var lut: [key_length + 1]u8 = undefined;

            var s = 0;
            var i = 0;
            for (segments) |segment| {
                var j = 0;
                while (j < segment) : ({
                    i += 1;
                    j += 1;
                }) {
                    lut[i] = s;
                }
                s += 1;
            }
            lut[key_length] = lut[key_length - 1];

            break :blk lut;
        };

        pub const Node = extern union {
            unknown: extern struct {
                tag: NodeTag,
                padding: [15]u8 = undefined,
            },
            none: extern struct {
                tag: NodeTag = .none,
                padding: [15]u8 = undefined,
            },
            branch1: BranchNodeBase,
            branch2: BranchNode(2),
            branch4: BranchNode(4),
            branch8: BranchNode(8),
            branch16: BranchNode(16),
            branch32: BranchNode(32),
            branch64: BranchNode(64),
            infix8: InfixNode(8),
            infix16: InfixNode(16),
            infix24: InfixNode(24),
            infix32: InfixNode(32),
            infix40: InfixNode(40),
            infix48: InfixNode(48),
            infix56: InfixNode(56),
            infix64: InfixNode(64),
            leaf: LeafNode,
            twig: TwigNode,

            const none = Node{ .none = .{ .tag = .none } };

            fn branchNodeTag(comptime bucket_count: u8) NodeTag {
                return switch (bucket_count) {
                    1 => NodeTag.branch1,
                    2 => NodeTag.branch2,
                    4 => NodeTag.branch4,
                    8 => NodeTag.branch8,
                    16 => NodeTag.branch16,
                    32 => NodeTag.branch32,
                    64 => NodeTag.branch64,
                    else => @panic("Bad bucket count for tag."),
                };
            }

            fn infixNodeTag(comptime suffix_len: u8) NodeTag {
                return switch (suffix_len) {
                    8 => NodeTag.infix8,
                    16 => NodeTag.infix16,
                    24 => NodeTag.infix24,
                    32 => NodeTag.infix32,
                    40 => NodeTag.infix40,
                    48 => NodeTag.infix48,
                    56 => NodeTag.infix56,
                    64 => NodeTag.infix64,
                    else => @panic("Bad infix count for infix tag."),
                };
            }

            pub fn format(
                self: Node,
                comptime fmt: []const u8,
                options: std.fmt.FormatOptions,
                writer: anytype,
            ) !void {
                _ = fmt;
                _ = options;

                switch (self.unknown.tag) {
                    .none => try writer.print("none", .{}),
                    .branch1 => try writer.print("{s}", .{self.branch1}),
                    .branch2 => try writer.print("{s}", .{self.branch2}),
                    .branch4 => try writer.print("{s}", .{self.branch4}),
                    .branch8 => try writer.print("{s}", .{self.branch8}),
                    .branch16 => try writer.print("{s}", .{self.branch16}),
                    .branch32 => try writer.print("{s}", .{self.branch32}),
                    .branch64 => try writer.print("{s}", .{self.branch64}),
                    .infix8 => try writer.print("{s}", .{self.infix8}),
                    .infix16 => try writer.print("{s}", .{self.infix16}),
                    .infix24 => try writer.print("{s}", .{self.infix24}),
                    .infix32 => try writer.print("{s}", .{self.infix32}),
                    .infix40 => try writer.print("{s}", .{self.infix40}),
                    .infix48 => try writer.print("{s}", .{self.infix48}),
                    .infix56 => try writer.print("{s}", .{self.infix56}),
                    .infix64 => try writer.print("{s}", .{self.infix64}),
                    .leaf => try writer.print("{s}", .{self.leaf}),
                    .twig => try writer.print("{s}", .{self.twig}),
                }
                try writer.writeAll("");
            }

            pub fn isNone(self: Node) bool {
                return self.unknown.tag == .none;
            }

            pub fn ref(self: Node, allocator: std.mem.Allocator) allocError!?Node {
                return switch (self.unknown.tag) {
                    .none => Node{ .none = .{} },
                    .branch1 => self.branch1.ref(allocator),
                    .branch2 => self.branch2.ref(allocator),
                    .branch4 => self.branch4.ref(allocator),
                    .branch8 => self.branch8.ref(allocator),
                    .branch16 => self.branch16.ref(allocator),
                    .branch32 => self.branch32.ref(allocator),
                    .branch64 => self.branch64.ref(allocator),
                    .infix8 => self.infix8.ref(allocator),
                    .infix16 => self.infix16.ref(allocator),
                    .infix24 => self.infix24.ref(allocator),
                    .infix32 => self.infix32.ref(allocator),
                    .infix40 => self.infix40.ref(allocator),
                    .infix48 => self.infix48.ref(allocator),
                    .infix56 => self.infix56.ref(allocator),
                    .infix64 => self.infix64.ref(allocator),
                    .leaf => self.leaf.ref(allocator),
                    .twig => self.twig.ref(allocator),
                };
            }

            pub fn rel(self: Node, allocator: std.mem.Allocator) void {
                switch (self.unknown.tag) {
                    .none => {},
                    .branch1 => self.branch1.rel(allocator),
                    .branch2 => self.branch2.rel(allocator),
                    .branch4 => self.branch4.rel(allocator),
                    .branch8 => self.branch8.rel(allocator),
                    .branch16 => self.branch16.rel(allocator),
                    .branch32 => self.branch32.rel(allocator),
                    .branch64 => self.branch64.rel(allocator),
                    .infix8 => self.infix8.rel(allocator),
                    .infix16 => self.infix16.rel(allocator),
                    .infix24 => self.infix24.rel(allocator),
                    .infix32 => self.infix32.rel(allocator),
                    .infix40 => self.infix40.rel(allocator),
                    .infix48 => self.infix48.rel(allocator),
                    .infix56 => self.infix56.rel(allocator),
                    .infix64 => self.infix64.rel(allocator),
                    .leaf => self.leaf.rel(allocator),
                    .twig => self.twig.rel(allocator),
                }
            }

            pub fn count(self: Node) u64 {
                return switch (self.unknown.tag) {
                    .none => 0,
                    .branch1 => self.branch1.count(),
                    .branch2 => self.branch2.count(),
                    .branch4 => self.branch4.count(),
                    .branch8 => self.branch8.count(),
                    .branch16 => self.branch16.count(),
                    .branch32 => self.branch32.count(),
                    .branch64 => self.branch64.count(),
                    .infix8 => self.infix8.count(),
                    .infix16 => self.infix16.count(),
                    .infix24 => self.infix24.count(),
                    .infix32 => self.infix32.count(),
                    .infix40 => self.infix40.count(),
                    .infix48 => self.infix48.count(),
                    .infix56 => self.infix56.count(),
                    .infix64 => self.infix64.count(),
                    .leaf => self.leaf.count(),
                    .twig => self.twig.count(),
                };
            }

            pub fn segmentCount(self: Node, depth: u8) u32 {
                return switch (self.unknown.tag) {
                    .none => 0,
                    .branch1 => self.branch1.segmentCount(depth),
                    .branch2 => self.branch2.segmentCount(depth),
                    .branch4 => self.branch4.segmentCount(depth),
                    .branch8 => self.branch8.segmentCount(depth),
                    .branch16 => self.branch16.segmentCount(depth),
                    .branch32 => self.branch32.segmentCount(depth),
                    .branch64 => self.branch64.segmentCount(depth),
                    .infix8 => self.infix8.segmentCount(depth),
                    .infix16 => self.infix16.segmentCount(depth),
                    .infix24 => self.infix24.segmentCount(depth),
                    .infix32 => self.infix32.segmentCount(depth),
                    .infix40 => self.infix40.segmentCount(depth),
                    .infix48 => self.infix48.segmentCount(depth),
                    .infix56 => self.infix56.segmentCount(depth),
                    .infix64 => self.infix64.segmentCount(depth),
                    .leaf => self.leaf.segmentCount(depth),
                    .twig => self.twig.segmentCount(depth),
                };
            }

            pub fn hash(self: Node, prefix: [key_length]u8) Hash {
                return switch (self.unknown.tag) {
                    .none => Hash{},
                    .branch1 => self.branch1.hash(prefix),
                    .branch2 => self.branch2.hash(prefix),
                    .branch4 => self.branch4.hash(prefix),
                    .branch8 => self.branch8.hash(prefix),
                    .branch16 => self.branch16.hash(prefix),
                    .branch32 => self.branch32.hash(prefix),
                    .branch64 => self.branch64.hash(prefix),
                    .infix8 => self.infix8.hash(prefix),
                    .infix16 => self.infix16.hash(prefix),
                    .infix24 => self.infix24.hash(prefix),
                    .infix32 => self.infix32.hash(prefix),
                    .infix40 => self.infix40.hash(prefix),
                    .infix48 => self.infix48.hash(prefix),
                    .infix56 => self.infix56.hash(prefix),
                    .infix64 => self.infix64.hash(prefix),
                    .leaf => self.leaf.hash(prefix),
                    .twig => self.twig.hash(prefix),
                };
            }

            pub fn minhash(self: Node, prefix: [key_length]u8, depth: u8) Hash {
                return switch (self.unknown.tag) {
                    .none => Hash{},
                    .branch1 => self.branch1.minhash(prefix, depth),
                    .branch2 => self.branch2.minhash(prefix, depth),
                    .branch4 => self.branch4.minhash(prefix, depth),
                    .branch8 => self.branch8.minhash(prefix, depth),
                    .branch16 => self.branch16.minhash(prefix, depth),
                    .branch32 => self.branch32.minhash(prefix, depth),
                    .branch64 => self.branch64.minhash(prefix, depth),
                    .infix8 => self.infix8.minhash(prefix, depth),
                    .infix16 => self.infix16.minhash(prefix, depth),
                    .infix24 => self.infix24.minhash(prefix, depth),
                    .infix32 => self.infix32.minhash(prefix, depth),
                    .infix40 => self.infix40.minhash(prefix, depth),
                    .infix48 => self.infix48.minhash(prefix, depth),
                    .infix56 => self.infix56.minhash(prefix, depth),
                    .infix64 => self.infix64.minhash(prefix, depth),
                    .leaf => self.leaf.minhash(prefix, depth),
                    .twig => self.twig.minhash(prefix, depth),
                };
            }

            pub fn range(self: Node) u8 {
                return switch (self.unknown.tag) {
                    .none => @panic("Called `range` on none."),
                    .branch1 => self.branch1.range(),
                    .branch2 => self.branch2.range(),
                    .branch4 => self.branch4.range(),
                    .branch8 => self.branch8.range(),
                    .branch16 => self.branch16.range(),
                    .branch32 => self.branch32.range(),
                    .branch64 => self.branch64.range(),
                    .infix8 => self.infix8.range(),
                    .infix16 => self.infix16.range(),
                    .infix24 => self.infix24.range(),
                    .infix32 => self.infix32.range(),
                    .infix40 => self.infix40.range(),
                    .infix48 => self.infix48.range(),
                    .infix56 => self.infix56.range(),
                    .infix64 => self.infix64.range(),
                    .leaf => self.leaf.range(),
                    .twig => self.twig.range(),
                };
            }

            pub fn peek(self: Node, at_depth: u8) ?u8 {
                return switch (self.unknown.tag) {
                    .none => null,
                    .branch1 => self.branch1.peek(at_depth),
                    .branch2 => self.branch2.peek(at_depth),
                    .branch4 => self.branch4.peek(at_depth),
                    .branch8 => self.branch8.peek(at_depth),
                    .branch16 => self.branch16.peek(at_depth),
                    .branch32 => self.branch32.peek(at_depth),
                    .branch64 => self.branch64.peek(at_depth),
                    .infix8 => self.infix8.peek(at_depth),
                    .infix16 => self.infix16.peek(at_depth),
                    .infix24 => self.infix24.peek(at_depth),
                    .infix32 => self.infix32.peek(at_depth),
                    .infix40 => self.infix40.peek(at_depth),
                    .infix48 => self.infix48.peek(at_depth),
                    .infix56 => self.infix56.peek(at_depth),
                    .infix64 => self.infix64.peek(at_depth),
                    .leaf => self.leaf.peek(at_depth),
                    .twig => self.twig.peek(at_depth),
                };
            }

            pub fn propose(self: Node, at_depth: u8, result_set: *ByteBitset) void {
                return switch (self.unknown.tag) {
                    .none => result_set.unsetAll(),
                    .branch1 => self.branch1.propose(at_depth, result_set),
                    .branch2 => self.branch2.propose(at_depth, result_set),
                    .branch4 => self.branch4.propose(at_depth, result_set),
                    .branch8 => self.branch8.propose(at_depth, result_set),
                    .branch16 => self.branch16.propose(at_depth, result_set),
                    .branch32 => self.branch32.propose(at_depth, result_set),
                    .branch64 => self.branch64.propose(at_depth, result_set),
                    .infix8 => self.infix8.propose(at_depth, result_set),
                    .infix16 => self.infix16.propose(at_depth, result_set),
                    .infix24 => self.infix24.propose(at_depth, result_set),
                    .infix32 => self.infix32.propose(at_depth, result_set),
                    .infix40 => self.infix40.propose(at_depth, result_set),
                    .infix48 => self.infix48.propose(at_depth, result_set),
                    .infix56 => self.infix56.propose(at_depth, result_set),
                    .infix64 => self.infix64.propose(at_depth, result_set),
                    .leaf => self.leaf.propose(at_depth, result_set),
                    .twig => self.twig.propose(at_depth, result_set),
                };
            }

            pub fn get(self: Node, at_depth: u8, byte_key: u8) Node {
                return switch (self.unknown.tag) {
                    .none => self,
                    .branch1 => self.branch1.get(at_depth, byte_key),
                    .branch2 => self.branch2.get(at_depth, byte_key),
                    .branch4 => self.branch4.get(at_depth, byte_key),
                    .branch8 => self.branch8.get(at_depth, byte_key),
                    .branch16 => self.branch16.get(at_depth, byte_key),
                    .branch32 => self.branch32.get(at_depth, byte_key),
                    .branch64 => self.branch64.get(at_depth, byte_key),
                    .infix8 => self.infix8.get(at_depth, byte_key),
                    .infix16 => self.infix16.get(at_depth, byte_key),
                    .infix24 => self.infix24.get(at_depth, byte_key),
                    .infix32 => self.infix32.get(at_depth, byte_key),
                    .infix40 => self.infix40.get(at_depth, byte_key),
                    .infix48 => self.infix48.get(at_depth, byte_key),
                    .infix56 => self.infix56.get(at_depth, byte_key),
                    .infix64 => self.infix64.get(at_depth, byte_key),
                    .leaf => self.leaf.get(at_depth, byte_key),
                    .twig => self.twig.get(at_depth, byte_key),
                };
            }

            pub fn put(self: Node, start_depth: u8, key: [key_length]u8, value: ?T, single_owner: bool, allocator: std.mem.Allocator) allocError!Node {
                return switch (self.unknown.tag) {
                    .none => @panic("Called `put` on none."),
                    .branch1 => self.branch1.put(start_depth, key, value, single_owner, allocator),
                    .branch2 => self.branch2.put(start_depth, key, value, single_owner, allocator),
                    .branch4 => self.branch4.put(start_depth, key, value, single_owner, allocator),
                    .branch8 => self.branch8.put(start_depth, key, value, single_owner, allocator),
                    .branch16 => self.branch16.put(start_depth, key, value, single_owner, allocator),
                    .branch32 => self.branch32.put(start_depth, key, value, single_owner, allocator),
                    .branch64 => self.branch64.put(start_depth, key, value, single_owner, allocator),
                    .infix8 => self.infix8.put(start_depth, key, value, single_owner, allocator),
                    .infix16 => self.infix16.put(start_depth, key, value, single_owner, allocator),
                    .infix24 => self.infix24.put(start_depth, key, value, single_owner, allocator),
                    .infix32 => self.infix32.put(start_depth, key, value, single_owner, allocator),
                    .infix40 => self.infix40.put(start_depth, key, value, single_owner, allocator),
                    .infix48 => self.infix48.put(start_depth, key, value, single_owner, allocator),
                    .infix56 => self.infix56.put(start_depth, key, value, single_owner, allocator),
                    .infix64 => self.infix64.put(start_depth, key, value, single_owner, allocator),
                    .leaf => self.leaf.put(start_depth, key, value, single_owner, allocator),
                    .twig => self.twig.put(start_depth, key, value, single_owner, allocator),
                };
            }

            fn createBranch(self: Node, child: Node, at_depth: u8, prefix: [key_length]u8) ?Node {
                return switch (self.unknown.tag) {
                    .branch1 => self.branch1.createBranch(child, at_depth, prefix),
                    .branch2 => self.branch2.createBranch(child, at_depth, prefix),
                    .branch4 => self.branch4.createBranch(child, at_depth, prefix),
                    .branch8 => self.branch8.createBranch(child, at_depth, prefix),
                    .branch16 => self.branch16.createBranch(child, at_depth, prefix),
                    .branch32 => self.branch32.createBranch(child, at_depth, prefix),
                    .branch64 => self.branch64.createBranch(child, at_depth, prefix),
                    .none => @panic("Called `createBranch` on none."),
                    else => @panic("Called `createBranch` on non-branch node."),
                };
            }

            fn reinsertBranch(self: Node, node: Node) ?Node {
                return switch (self.unknown.tag) {
                    .branch1 => self.branch1.reinsertBranch(node),
                    .branch2 => self.branch2.reinsertBranch(node),
                    .branch4 => self.branch4.reinsertBranch(node),
                    .branch8 => self.branch8.reinsertBranch(node),
                    .branch16 => self.branch16.reinsertBranch(node),
                    .branch32 => self.branch32.reinsertBranch(node),
                    .branch64 => self.branch64.reinsertBranch(node),
                    .none => @panic("Called `reinsertBranch` on none."),
                    else => @panic("Called `reinsertBranch` on non-branch node."),
                };
            }

            fn grow(self: Node, allocator: std.mem.Allocator) allocError!Node {
                return switch (self.unknown.tag) {
                    .branch1 => self.branch1.grow(allocator),
                    .branch2 => self.branch2.grow(allocator),
                    .branch4 => self.branch4.grow(allocator),
                    .branch8 => self.branch8.grow(allocator),
                    .branch16 => self.branch16.grow(allocator),
                    .branch32 => self.branch32.grow(allocator),
                    .branch64 => self.branch64.grow(allocator),
                    .none => @panic("Called `grow` on none."),
                    else => @panic("Called `grow` on non-branch node."),
                };
            }

            pub fn getValue(self: Node) ?T {
                return switch (self.unknown.tag) {
                    .none => null,
                    .twig => null,
                    .leaf => self.leaf.value,
                    else => @panic("Called `value` on non-terminal node."),
                };
            }

            pub fn coveredDepth(self: Node) u8 {
                return switch (self.unknown.tag) {
                    .none => @panic("Called `depth` on none."),
                    .branch1 => self.branch1.branch_depth,
                    .branch2 => self.branch2.branch_depth,
                    .branch4 => self.branch4.branch_depth,
                    .branch8 => self.branch8.branch_depth,
                    .branch16 => self.branch16.branch_depth,
                    .branch32 => self.branch32.branch_depth,
                    .branch64 => self.branch64.branch_depth,
                    .infix8 => self.infix8.body.child.coveredDepth(),
                    .infix16 => self.infix16.body.child.coveredDepth(),
                    .infix24 => self.infix24.body.child.coveredDepth(),
                    .infix32 => self.infix32.body.child.coveredDepth(),
                    .infix40 => self.infix40.body.child.coveredDepth(),
                    .infix48 => self.infix48.body.child.coveredDepth(),
                    .infix56 => self.infix56.body.child.coveredDepth(),
                    .infix64 => self.infix64.body.child.coveredDepth(),
                    .leaf => key_length,
                    .twig => key_length,
                };
            }

            pub fn mem_info(self: Node) MemInfo {
                return switch (self.unknown.tag) {
                    .none => self.branch1.mem_info(),
                    .branch1 => self.branch1.mem_info(),
                    .branch2 => self.branch2.mem_info(),
                    .branch4 => self.branch4.mem_info(),
                    .branch8 => self.branch8.mem_info(),
                    .branch16 => self.branch16.mem_info(),
                    .branch32 => self.branch32.mem_info(),
                    .branch64 => self.branch64.mem_info(),
                    .infix8 => self.infix8.mem_info(),
                    .infix16 => self.infix16.mem_info(),
                    .infix24 => self.infix24.mem_info(),
                    .infix32 => self.infix32.mem_info(),
                    .infix40 => self.infix40.mem_info(),
                    .infix48 => self.infix48.mem_info(),
                    .infix56 => self.infix56.mem_info(),
                    .infix64 => self.infix64.mem_info(),
                    .leaf => self.leaf.mem_info(),
                    .twig => self.twig.mem_info(),
                };
            }
        };

        const rand_lut = blk: {
            @setEvalBranchQuota(1000000);
            var rand_state = std.rand.Xoroshiro128.init(0);
            break :blk generate_pearson_LUT(rand_state.random());
        };
        var random: u8 = 4; // Chosen by fair dice roll.

        /// Hashes the value provided with the selected permuation and provided compression.
        fn hashByteKey(
            // / Use alternative permuation.
            p: bool,
            // / Bucket count to parameterize the compression used to pigeonhole the items. Must be a power of 2.
            c: u8,
            // / The value to hash.
            v: u8,
        ) u8 {
            assert(@popCount(u8, c) == 1);
            @setEvalBranchQuota(1000000);
            const luts = comptime blk: {
                @setEvalBranchQuota(1000000);
                var rand_state = std.rand.Xoroshiro128.init(0);
                break :blk generate_hash_LUTs(rand_state.random());
            };
            const mask = c - 1;
            const pi: u8 = if (p) 1 else 0;
            return mask & luts[v][pi];
        }

        const max_bucket_count = BRANCH_FACTOR / Bucket.slot_count; //64

        const Bucket = extern struct {
            const slot_count = 4;

            slots: [slot_count]Node = [_]Node{Node{ .none = .{} }} ** slot_count,

            pub fn get(self: *const Bucket, depth: u8, byte_key: u8) Node {
                for (self.slots) |slot| {
                    if (slot.unknown.tag != .none and ((slot.peek(depth).?) == byte_key)) {
                        return slot;
                    }
                }
                return Node.none;
            }

            /// Attempt to store a new node in this bucket,
            /// the key must not exist in this bucket beforehand.
            /// If there is no free slot the attempt will fail.
            /// Returns true iff it succeeds.
            pub fn put(
                self: *Bucket,
                depth: u8,
                // / Determines the hash function used for each key and is used to detect outdated (free) slots.
                rand_hash_used: *ByteBitset,
                // / The current bucket count. Is used to detect outdated (free) slots.
                current_count: u8,
                // / The current index the bucket has. Is used to detect outdated (free) slots.
                bucket_index: u8,
                // / The entry to be stored in the bucket.
                entry: Node,
            ) bool {
                return self.putIntoSame(depth, entry) or self.putIntoEmpty(entry) or self.putIntoOutdated(depth, rand_hash_used, current_count, bucket_index, entry);
            }

            /// Updates the pointer for the key stored in this bucket.
            pub fn putIntoEmpty(
                self: *Bucket,
                // / The new entry value.
                entry: Node,
            ) bool {
                for (self.slots) |*slot| {
                    if (slot.isNone()) {
                        slot.* = entry;
                        return true;
                    }
                }
                return false;
            }

            /// Updates the pointer for the key stored in this bucket.
            pub fn putIntoSame(
                self: *Bucket,
                depth: u8,
                // / The new entry value.
                entry: Node,
            ) bool {
                for (self.slots) |*slot| {
                    if (slot.unknown.tag != .none and ((slot.peek(depth).?) == (entry.peek(depth).?))) {
                        slot.* = entry;
                        return true;
                    }
                }
                return false;
            }

            pub fn putIntoOutdated(
                self: *Bucket,
                depth: u8,
                // / Determines the hash function used for each key and is used to detect outdated (free) slots.
                rand_hash_used: *ByteBitset,
                // / The current bucket count. Is used to detect outdated (free) slots.
                current_count: u8,
                // / The current index the bucket has. Is used to detect outdated (free) slots.
                bucket_index: u8,
                // / The entry to be stored in the bucket.
                entry: Node,
            ) bool {
                for (self.slots) |*slot| {
                    const slot_key = slot.peek(depth).?;
                    if (bucket_index != hashByteKey(rand_hash_used.isSet(slot_key), current_count, slot_key)) {
                        slot.* = entry;
                        return true;
                    }
                }
                return false;
            }

            /// Displaces a random existing slot.
            pub fn displaceRandomly(
                self: *Bucket,
                // / A random value to determine the slot to displace.
                random_value: u8,
                // / The entry that displaces an existing entry.
                entry: Node,
            ) Node {
                const index = random_value & (slot_count - 1);
                const prev = self.slots[index];
                self.slots[index] = entry;
                return prev;
            }

            /// Displaces the first slot that is using the alternate hash function.
            pub fn displaceRandHashOnly(
                self: *Bucket,
                depth: u8,
                // / Determines the hash function used for each key and is used to detect outdated (free) slots.
                rand_hash_used: *ByteBitset,
                // / The entry to be stored in the bucket.
                entry: Node,
            ) Node {
                for (self.slots) |*slot| {
                    if (rand_hash_used.isSet(slot.peek(depth).?)) {
                        const prev = slot.*;
                        slot.* = entry;
                        return prev;
                    }
                }
                unreachable;
            }
        };

        const BranchNodeBase = extern struct {
            const infix_len = 6;

            tag: NodeTag = .branch1,
            /// The infix stored in this head.
            infix: [infix_len]u8 = [_]u8{0} ** infix_len,
            /// The branch depth of the body.
            branch_depth: u8,
            /// The address of the pointer associated with the key.
            body: *Body,

            const Head = @This();

            const GrownHead = BranchNode(2);

            const BODY_ALIGNMENT = 64;

            const Body = extern struct {
                leaf_count: u64 = 0,
                ref_count: u16 = 1,
                padding: [2]u8 = undefined,
                segment_count: u32 = 0,
                node_hash: Hash = Hash{},
                segment_minhash: MinHash = MinHash{},
                bucket: Bucket = Bucket{},
            };

            pub fn format(
                self: Head,
                comptime fmt: []const u8,
                options: std.fmt.FormatOptions,
                writer: anytype,
            ) !void {
                _ = fmt;
                _ = options;

                _ = self;

                try writer.writeAll("Branch One");
            }

            pub fn init(branch_depth: u8, key: [key_length]u8, allocator: std.mem.Allocator) allocError!Head {
                const allocation = try allocator.allocAdvanced(u8, BODY_ALIGNMENT, @sizeOf(Body), .exact);
                const new_body = std.mem.bytesAsValue(Body, allocation[0..@sizeOf(Body)]);
                new_body.* = Body{};

                var new_head = Head{ .branch_depth = branch_depth, .body = new_body };

                const used_infix_len = @minimum(branch_depth, infix_len);
                mem.copy(u8, new_head.infix[infix_len - used_infix_len ..], key[branch_depth - used_infix_len .. branch_depth]);

                return new_head;
            }

            pub fn initBranch(branch_depth: u8, key: [key_length]u8, left: Node, right: Node, allocator: std.mem.Allocator) allocError!Node {
                const branch_node = try BranchNodeBase.init(branch_depth, key, allocator);

                // Note that these can't fail.
                _ = branch_node.createBranch(left, branch_depth, key);
                _ = branch_node.createBranch(right, branch_depth, key);

                return @bitCast(Node, branch_node);
            }

            pub fn ref(self: Head, allocator: std.mem.Allocator) allocError!?Node {
                if (self.body.ref_count == std.math.maxInt(@TypeOf(self.body.ref_count))) {
                    // Reference counter exhausted, we need to make a copy of this node.
                    return @bitCast(Node, try self.copy(allocator));
                } else {
                    self.body.ref_count += 1;
                    return null;
                }
            }

            pub fn rel(self: Head, allocator: std.mem.Allocator) void {
                self.body.ref_count -= 1;
                if (self.body.ref_count == 0) {
                    defer allocator.free(std.mem.asBytes(self.body));
                    for (self.body.bucket.slots) |slot| {
                        if (slot.unknown.tag != .none) {
                            slot.rel(allocator);
                        }
                    }
                }
            }

            pub fn count(self: Head) u64 {
                return self.body.leaf_count;
            }

            pub fn segmentCount(self: Head, depth: u8) u32 {
                if (segment_lut[depth] == segment_lut[self.branch_depth]) {
                    return self.body.segment_count;
                } else {
                    return 1;
                }
            }

            pub fn hash(self: Head, prefix: [key_length]u8) Hash {
                _ = prefix;
                return self.body.node_hash;
            }

            pub fn minhash(self: Head, prefix: [key_length]u8, depth: u8) MinHash {
                _ = self;
                _ = prefix;
                _ = depth;
                return MinHash{};
                //return self.body.segment_minhash;
            }

            pub fn range(self: Head) u8 {
                return self.branch_depth - @minimum(self.branch_depth, infix_len);
            }

            pub fn peek(self: Head, at_depth: u8) ?u8 {
                if (self.branch_depth <= at_depth) return null;
                return self.infix[(at_depth + infix_len) - self.branch_depth];
            }

            pub fn propose(self: Head, at_depth: u8, result_set: *ByteBitset) void {
                if (at_depth == self.branch_depth) {
                    var child_set = ByteBitset.initEmpty();
                    for (self.body.bucket.slots) |slot| {
                        if (!slot.isNone()) {
                            child_set.set(slot.peek(at_depth).?);
                        }
                    }
                    result_set.setIntersect(result_set, &child_set);
                    return;
                }

                if (self.peek(at_depth)) |byte_key| {
                    result_set.singleIntersect(byte_key);
                    return;
                }

                result_set.unsetAll();
            }

            pub fn put(self: Head, start_depth: u8, key: [key_length]u8, value: ?T, parent_single_owner: bool, allocator: std.mem.Allocator) allocError!Node {
                const single_owner = parent_single_owner and self.body.ref_count == 1;

                var branch_depth = start_depth;
                while (branch_depth < self.branch_depth) : (branch_depth += 1) {
                    if (key[branch_depth] != self.peek(branch_depth).?) break;
                } else {
                    // The entire compressed infix above this node matched with the key.
                    const byte_key = key[branch_depth];

                    const old_child = self.body.bucket.get(self.branch_depth, byte_key);
                    if (old_child.unknown.tag != .none) {
                        // The node already has a child branch with the same byte byte_key as the one in the key.
                        const old_child_hash = old_child.hash(key);
                        const old_child_leaf_count = old_child.count();
                        const old_child_segment_count = old_child.segmentCount(branch_depth);
                        const new_child = try old_child.put(branch_depth, key, value, single_owner, allocator);
                        const new_child_hash = new_child.hash(key);
                        if (Hash.equal(old_child_hash, new_child_hash)) {
                            return @bitCast(Node, self);
                        }
                        const new_hash = self.body.node_hash.update(old_child_hash, new_child_hash);
                        const new_leaf_count = self.body.leaf_count - old_child_leaf_count + new_child.count();
                        const new_segment_count = self.body.segment_count - old_child_segment_count + new_child.segmentCount(branch_depth);

                        var self_or_copy = self;
                        if (!single_owner) {
                            self_or_copy = try self.copy(allocator);
                            old_child.rel(allocator);
                        }
                        self_or_copy.body.node_hash = new_hash;
                        self_or_copy.body.leaf_count = new_leaf_count;
                        self_or_copy.body.segment_count = new_segment_count;

                        _ = self_or_copy.body.bucket.putIntoSame(self.branch_depth, new_child);
                        return @bitCast(Node, self_or_copy);
                    } else {
                        const new_child_node = try WrapInfixNode(branch_depth, key, InitLeafOrTwigNode(key, value), allocator);

                        var self_or_copy = if (single_owner) self else try self.copy(allocator);

                        var displaced = self_or_copy.createBranch(new_child_node, branch_depth, key);
                        var grown = @bitCast(Node, self_or_copy);
                        while (displaced) |entry| {
                            grown = try grown.grow(allocator);
                            displaced = grown.reinsertBranch(entry);
                        }
                        return grown;
                    }
                }

                const sibling_leaf_node = try WrapInfixNode(branch_depth, key, InitLeafOrTwigNode(key, value), allocator);

                return try BranchNodeBase.initBranch(branch_depth, key, sibling_leaf_node, @bitCast(Node, self), allocator);
            }

            pub fn get(self: Head, at_depth: u8, byte_key: u8) Node {
                if (at_depth == self.branch_depth) {
                    return self.body.bucket.get(self.branch_depth, byte_key);
                }
                if (self.peek(at_depth)) |own_key| {
                    if (own_key == byte_key) return @bitCast(Node, self);
                }
                return Node.none;
            }

            fn copy(self: Head, allocator: std.mem.Allocator) allocError!Head {
                const allocation = try allocator.allocAdvanced(u8, BODY_ALIGNMENT, @sizeOf(Body), .exact);
                const new_body = std.mem.bytesAsValue(Body, allocation[0..@sizeOf(Body)]);

                new_body.* = self.body.*;
                new_body.ref_count = 1;

                var new_head = self;
                new_head.body = self.body;

                for (new_head.body.bucket.slots) |*child| {
                    if (!child.isNone()) {
                        const potential_child_copy = try child.ref(allocator);
                        if (potential_child_copy) |new_child| {
                            child.* = new_child;
                        }
                    }
                }

                return new_head;
            }

            fn grow(self: Head, allocator: std.mem.Allocator) allocError!Node {
                const bucket = self.body.bucket;

                const allocation: []align(BODY_ALIGNMENT) u8 = try allocator.reallocAdvanced(std.mem.span(std.mem.asBytes(self.body)), BODY_ALIGNMENT, @sizeOf(GrownHead.Body), .exact);
                const new_body = std.mem.bytesAsValue(GrownHead.Body, allocation[0..@sizeOf(GrownHead.Body)]);

                new_body.buckets[0] = bucket;
                new_body.buckets[1] = bucket;

                new_body.child_set.unsetAll();
                new_body.rand_hash_used.unsetAll();

                for (bucket.slots) |child| {
                    new_body.child_set.set(child.peek(self.branch_depth).?);
                }

                return @bitCast(Node, GrownHead{ .branch_depth = self.branch_depth, .infix = self.infix, .body = new_body });
            }

            fn createBranch(self: Head, child: Node, at_depth: u8, prefix: [key_length]u8) ?Node {
                self.body.node_hash = self.body.node_hash.combine(child.hash(prefix));
                self.body.leaf_count += child.count();
                self.body.segment_count += child.segmentCount(at_depth);

                return self.reinsertBranch(child);
            }

            fn reinsertBranch(self: Head, node: Node) ?Node { //TODO get rid of this and make the other one return Node.none
                if (self.body.bucket.putIntoEmpty(node)) {
                    return null; //TODO use none.
                }
                return node;
            }

            fn mem_info(self: Head) MemInfo {
                var unused_slots: u8 = 0;
                var wasted_infix_bytes: usize = 0;
                for (self.body.bucket.slots) |child| {
                    if (child.isNone()) {
                        unused_slots += 1;
                    } else {
                        wasted_infix_bytes += (self.branch_depth - child.range());
                    }
                }

                return MemInfo{ .active_memory = @sizeOf(Body), .wasted_memory = (@sizeOf(Node) * unused_slots) + wasted_infix_bytes, .passive_memory = @sizeOf(Head), .allocation_count = 1 };
            }
        };

        fn BranchNode(comptime bucket_count: u8) type {
            const infix_len = 6;

            return extern struct {
                tag: NodeTag = Node.branchNodeTag(bucket_count),
                /// The infix stored in this head.
                infix: [infix_len]u8 = [_]u8{0} ** infix_len,
                /// The branch depth of the body.
                branch_depth: u8,
                /// The address of the pointer associated with the key.
                body: *Body,

                const Head = @This();

                const GrownHead = if (bucket_count == max_bucket_count) Head else BranchNode(bucket_count << 1);

                const BODY_ALIGNMENT = 64;

                const Body = extern struct {
                    leaf_count: u64 = 0,
                    ref_count: u16 = 1,
                    padding: [2]u8 = undefined,
                    segment_count: u32 = 0,
                    node_hash: Hash = Hash{},
                    segment_minhash: MinHash = MinHash{},
                    child_set: ByteBitset = ByteBitset.initEmpty(),
                    rand_hash_used: ByteBitset = ByteBitset.initEmpty(),
                    buckets: Buckets = if (bucket_count == 1) [_]Bucket{Bucket{}} else undefined,

                    const Buckets = [bucket_count]Bucket;
                };

                pub fn format(
                    self: Head,
                    comptime fmt: []const u8,
                    options: std.fmt.FormatOptions,
                    writer: anytype,
                ) !void {
                    _ = fmt;
                    _ = options;
                    _ = self;
                    //const card = cards.branchNodeCard(self);
                    //try writer.print("{s}\n", .{card});
                    try writer.writeAll("TODO");
                }

                pub fn init(branch_depth: u8, key: [key_length]u8, allocator: std.mem.Allocator) allocError!Head {
                    const allocation = try allocator.allocAdvanced(u8, BODY_ALIGNMENT, @sizeOf(Body), .exact);
                    const new_body = std.mem.bytesAsValue(Body, allocation[0..@sizeOf(Body)]);
                    new_body.* = Body{};

                    var new_head = Head{ .branch_depth = branch_depth, .body = new_body };

                    const used_infix_len = @minimum(branch_depth, infix_len);
                    mem.copy(u8, new_head.infix[infix_len - used_infix_len ..], key[branch_depth - used_infix_len .. branch_depth]);

                    return new_head;
                }

                pub fn ref(self: Head, allocator: std.mem.Allocator) allocError!?Node {
                    if (self.body.ref_count == std.math.maxInt(@TypeOf(self.body.ref_count))) {
                        // Reference counter exhausted, we need to make a copy of this node.
                        return @bitCast(Node, try self.copy(allocator));
                    } else {
                        self.body.ref_count += 1;
                        return null;
                    }
                }

                pub fn rel(self: Head, allocator: std.mem.Allocator) void {
                    self.body.ref_count -= 1;
                    if (self.body.ref_count == 0) {
                        defer allocator.free(std.mem.asBytes(self.body));
                        var child_iterator = self.body.child_set;
                        while (child_iterator.drainNext(true)) |child_byte_key| {
                            self.getBranch(@intCast(u8, child_byte_key)).rel(allocator);
                        }
                    }
                }

                pub fn count(self: Head) u64 {
                    return self.body.leaf_count;
                }

                pub fn segmentCount(self: Head, depth: u8) u32 {
                    if (segment_lut[depth] == segment_lut[self.branch_depth]) {
                        return self.body.segment_count;
                    } else {
                        return 1;
                    }
                }

                pub fn hash(self: Head, prefix: [key_length]u8) Hash {
                    _ = prefix;
                    return self.body.node_hash;
                }

                pub fn minhash(self: Head, prefix: [key_length]u8, depth: u8) MinHash {
                    _ = self;
                    _ = prefix;
                    _ = depth;
                    return MinHash{};
                    //return self.body.segment_minhash;
                }

                pub fn range(self: Head) u8 {
                    return self.branch_depth - @minimum(self.branch_depth, infix_len);
                }

                pub fn peek(self: Head, at_depth: u8) ?u8 {
                    if (self.branch_depth <= at_depth) return null;
                    return self.infix[(at_depth + infix_len) - self.branch_depth];
                }

                pub fn propose(self: Head, at_depth: u8, result_set: *ByteBitset) void {
                    if (at_depth == self.branch_depth) {
                        result_set.setIntersect(result_set, &self.body.child_set);
                        return;
                    }

                    if (self.peek(at_depth)) |byte_key| {
                        result_set.singleIntersect(byte_key);
                        return;
                    }

                    result_set.unsetAll();
                }

                pub fn put(self: Head, start_depth: u8, key: [key_length]u8, value: ?T, parent_single_owner: bool, allocator: std.mem.Allocator) allocError!Node {
                    const single_owner = parent_single_owner and self.body.ref_count == 1;

                    var branch_depth = start_depth;
                    while (branch_depth < self.branch_depth) : (branch_depth += 1) {
                        if (key[branch_depth] != self.peek(branch_depth).?) break;
                    } else {
                        // The entire compressed infix above this node matched with the key.
                        const byte_key = key[branch_depth];
                        if (self.hasBranch(byte_key)) {
                            // The node already has a child branch with the same byte byte_key as the one in the key.
                            const old_child = self.getBranch(byte_key);
                            const old_child_hash = old_child.hash(key);
                            const old_child_leaf_count = old_child.count();
                            const old_child_segment_count = old_child.segmentCount(branch_depth);
                            const new_child = try old_child.put(branch_depth, key, value, single_owner, allocator);
                            const new_child_hash = new_child.hash(key);
                            if (old_child_hash.equal(new_child_hash)) {
                                return @bitCast(Node, self);
                            }
                            const new_hash = self.body.node_hash.update(old_child_hash, new_child_hash);
                            const new_leaf_count = self.body.leaf_count - old_child_leaf_count + new_child.count();
                            const new_segment_count = self.body.segment_count - old_child_segment_count + new_child.segmentCount(branch_depth);

                            var self_or_copy = self;
                            if (!single_owner) {
                                self_or_copy = try self.copy(allocator);
                                old_child.rel(allocator);
                            }
                            self_or_copy.body.node_hash = new_hash;
                            self_or_copy.body.leaf_count = new_leaf_count;
                            self_or_copy.body.segment_count = new_segment_count;

                            self_or_copy.updateBranch(new_child);
                            return @bitCast(Node, self_or_copy);
                        } else {
                            const new_child_node = try WrapInfixNode(branch_depth, key, InitLeafOrTwigNode(key, value), allocator);

                            var self_or_copy = if (single_owner) self else try self.copy(allocator);

                            var displaced = self_or_copy.createBranch(new_child_node, branch_depth, key);
                            var grown = @bitCast(Node, self_or_copy);
                            while (displaced) |entry| {
                                grown = try grown.grow(allocator);
                                displaced = grown.reinsertBranch(entry);
                            }
                            return grown;
                        }
                    }

                    const sibling_leaf_node = try WrapInfixNode(branch_depth, key, InitLeafOrTwigNode(key, value), allocator);

                    return try BranchNodeBase.initBranch(branch_depth, key, sibling_leaf_node, @bitCast(Node, self), allocator);
                }

                pub fn get(self: Head, at_depth: u8, byte_key: u8) Node {
                    if (at_depth == self.branch_depth) {
                        if (self.hasBranch(byte_key)) {
                            return self.getBranch(byte_key);
                        } else {
                            return Node.none;
                        }
                    }
                    if (self.peek(at_depth)) |own_key| {
                        if (own_key == byte_key) return @bitCast(Node, self);
                    }
                    return Node.none;
                }

                fn copy(self: Head, allocator: std.mem.Allocator) allocError!Head {
                    const allocation = try allocator.allocAdvanced(u8, BODY_ALIGNMENT, @sizeOf(Body), .exact);
                    const new_body = std.mem.bytesAsValue(Body, allocation[0..@sizeOf(Body)]);

                    new_body.* = self.body.*;
                    new_body.ref_count = 1;

                    var new_head = self;
                    new_head.body = self.body;

                    var child_iterator = new_body.child_set;
                    while (child_iterator.drainNext(true)) |child_byte_key| {
                        const cast_child_byte_key = @intCast(u8, child_byte_key);
                        const child = new_head.getBranch(cast_child_byte_key);
                        const potential_child_copy = try child.ref(allocator);
                        if (potential_child_copy) |new_child| {
                            new_head.updateBranch(new_child);
                        }
                    }

                    return new_head;
                }

                fn grow(self: Head, allocator: std.mem.Allocator) allocError!Node {
                    if (bucket_count == max_bucket_count) {
                        return @bitCast(Node, self);
                    } else {
                        //std.debug.print("Grow:{*}\n {} -> {} : {} -> {} \n", .{ self.body, Head, GrownHead, @sizeOf(Body), @sizeOf(GrownHead.Body) });
                        const allocation: []align(BODY_ALIGNMENT) u8 = try allocator.reallocAdvanced(std.mem.span(std.mem.asBytes(self.body)), BODY_ALIGNMENT, @sizeOf(GrownHead.Body), .exact);
                        const new_body = std.mem.bytesAsValue(GrownHead.Body, allocation[0..@sizeOf(GrownHead.Body)]);
                        //std.debug.print("Growed:{*}\n", .{new_body});
                        new_body.buckets[new_body.buckets.len / 2 .. new_body.buckets.len].* = new_body.buckets[0 .. new_body.buckets.len / 2].*;
                        return @bitCast(Node, GrownHead{ .branch_depth = self.branch_depth, .infix = self.infix, .body = new_body });
                    }
                }

                fn createBranch(self: Head, child: Node, at_depth: u8, prefix: [key_length]u8) ?Node {
                    self.body.node_hash = self.body.node_hash.combine(child.hash(prefix));
                    self.body.leaf_count += child.count();
                    self.body.segment_count += child.segmentCount(at_depth);

                    return self.reinsertBranch(child);
                }

                fn reinsertBranch(self: Head, node: Node) ?Node {
                    var byte_key = node.peek(self.branch_depth).?;
                    self.body.child_set.set(byte_key);

                    const growable = (bucket_count != max_bucket_count);
                    const base_size = (bucket_count == 1);
                    var use_rand_hash = false;
                    var entry = node;
                    var attempts: u8 = 0;
                    while (true) {
                        random = rand_lut[random ^ byte_key];
                        const bucket_index = hashByteKey(use_rand_hash, bucket_count, byte_key);

                        if (self.body.buckets[bucket_index].put(self.branch_depth, &self.body.rand_hash_used, bucket_count, bucket_index, entry)) {
                            self.body.rand_hash_used.setValue(byte_key, use_rand_hash);
                            return null;
                        }

                        if (base_size or attempts == MAX_ATTEMPTS) {
                            return entry;
                        }

                        if (growable) {
                            attempts += 1;
                            entry = self.body.buckets[bucket_index].displaceRandomly(random, entry);
                            self.body.rand_hash_used.setValue(byte_key, use_rand_hash);
                            byte_key = entry.peek(self.branch_depth).?;
                            use_rand_hash = !self.body.rand_hash_used.isSet(byte_key);
                        } else {
                            entry = self.body.buckets[bucket_index].displaceRandHashOnly(self.branch_depth, &self.body.rand_hash_used, entry);
                            self.body.rand_hash_used.setValue(byte_key, use_rand_hash);
                            byte_key = entry.peek(self.branch_depth).?;
                        }
                    }
                    unreachable;
                }

                fn hasBranch(self: Head, byte_key: u8) bool {
                    return self.body.child_set.isSet(byte_key);
                }

                // Contract: Key looked up must exist. Ensure with hasBranch.
                fn getBranch(self: Head, byte_key: u8) Node {
                    assert(self.body.child_set.isSet(byte_key));
                    const bucket_index = hashByteKey(self.body.rand_hash_used.isSet(byte_key), bucket_count, byte_key);
                    return self.body.buckets[bucket_index].get(self.branch_depth, byte_key);
                }

                fn updateBranch(self: Head, node: Node) void {
                    const byte_key = node.peek(self.branch_depth).?;
                    const bucket_index = hashByteKey(self.body.rand_hash_used.isSet(byte_key), bucket_count, byte_key);
                    _ = self.body.buckets[bucket_index].putIntoSame(self.branch_depth, node);
                }

                fn mem_info(self: Head) MemInfo {
                    const child_count = self.body.child_set.count();
                    const total_slot_count = @as(usize, bucket_count) * @as(usize, Bucket.slot_count);
                    const unused_slots = total_slot_count - child_count;

                    var wasted_infix_bytes: usize = 0;
                    var child_iterator = self.body.child_set;
                    while (child_iterator.drainNext(true)) |child_byte_key| {
                        const child = self.getBranch(@intCast(u8, child_byte_key));
                        wasted_infix_bytes += (self.branch_depth - child.range());
                    }

                    return MemInfo{ .active_memory = @sizeOf(Body), .wasted_memory = (@sizeOf(Node) * unused_slots) + wasted_infix_bytes, .passive_memory = @sizeOf(Head), .allocation_count = 1 };
                }
            };
        }

        fn WrapInfixNode(start_depth: u8, key: [key_length]u8, child: Node, allocator: std.mem.Allocator) allocError!Node {
            const child_range = child.range();

            if (child_range <= start_depth) {
                return child;
            }

            const infix_length = child_range - start_depth;

            if (infix_length <= 8) {
                return @bitCast(Node, try InfixNode(8).init(key, child, allocator));
            }
            if (infix_length <= 16) {
                return @bitCast(Node, try InfixNode(16).init(key, child, allocator));
            }
            if (infix_length <= 24) {
                return @bitCast(Node, try InfixNode(24).init(key, child, allocator));
            }
            if (infix_length <= 32) {
                return @bitCast(Node, try InfixNode(32).init(key, child, allocator));
            }
            if (infix_length <= 40) {
                return @bitCast(Node, try InfixNode(40).init(key, child, allocator));
            }
            if (infix_length <= 48) {
                return @bitCast(Node, try InfixNode(48).init(key, child, allocator));
            }
            if (infix_length <= 54) {
                return @bitCast(Node, try InfixNode(56).init(key, child, allocator));
            }
            if (infix_length <= 64) {
                return @bitCast(Node, try InfixNode(64).init(key, child, allocator));
            }

            unreachable;
        }

        fn InfixNode(comptime infix_len: u8) type {
            const head_infix_len = 6;
            const body_infix_len = infix_len - head_infix_len;
            return extern struct {
                tag: NodeTag = Node.infixNodeTag(infix_len),

                child_depth: u8,

                infix: [head_infix_len]u8 = [_]u8{0} ** head_infix_len,

                body: *Body,

                const Head = @This();
                const Body = extern struct {
                    child: Node = Node.none,
                    ref_count: u16 = 1,
                    infix: [body_infix_len]u8 = undefined,
                };

                pub fn init(key: [key_length]u8, child: Node, allocator: std.mem.Allocator) allocError!Head {
                    const allocation = try allocator.allocAdvanced(u8, @alignOf(Body), @sizeOf(Body), .exact);
                    const new_body = std.mem.bytesAsValue(Body, allocation[0..@sizeOf(Body)]);

                    new_body.* = Body{ .child = child };

                    const child_depth = child.range();

                    var new_head = Head{ .child_depth = child_depth, .body = new_body };

                    const key_start_head = child_depth - @minimum(infix_len, child_depth);
                    const key_start_body = child_depth - @minimum(body_infix_len, child_depth);

                    const infix_start_head = @minimum(head_infix_len, depth_to_infix(infix_len, child_depth, key_start_head));
                    const infix_start_body = depth_to_infix(body_infix_len, child_depth, key_start_body);

                    mem.copy(u8, new_head.infix[infix_start_head..], key[key_start_head..key_start_body]);
                    mem.copy(u8, new_head.body.infix[infix_start_body..], key[key_start_body..child_depth]);

                    return new_head;
                }

                pub fn format(
                    self: Head,
                    comptime fmt: []const u8,
                    options: std.fmt.FormatOptions,
                    writer: anytype,
                ) !void {
                    _ = self;
                    _ = fmt;
                    _ = options;
                    try writer.print("{*} �{d}:\n", .{ self.body, self.body.ref_count });
                    try writer.print("  infixes: {[2]s:_>[0]} > {[3]s:_>[1]}\n", .{ head_infix_len, body_infix_len, std.fmt.fmtSliceHexUpper(&self.infix), std.fmt.fmtSliceHexUpper(&self.body.infix) });
                }

                fn copy(self: Head, allocator: std.mem.Allocator) allocError!Head {
                    const allocation = try allocator.allocAdvanced(u8, @alignOf(Body), @sizeOf(Body), .exact);
                    const new_body = std.mem.bytesAsValue(Body, allocation[0..@sizeOf(Body)]);
                    new_body.* = self.body.*;
                    new_body.ref_count = 1;

                    if (try new_body.child.ref(allocator)) |new_child| {
                        new_body.child = new_child;
                    }

                    return Head{ .child_depth = self.child_depth, .infix = self.infix, .body = new_body };
                }

                pub fn ref(self: Head, allocator: std.mem.Allocator) allocError!?Node {
                    if (self.body.ref_count == std.math.maxInt(@TypeOf(self.body.ref_count))) {
                        // Reference counter exhausted, we need to make a copy of this node.
                        return @bitCast(Node, try self.copy(allocator));
                    } else {
                        self.body.ref_count += 1;
                        return null;
                    }
                }

                pub fn rel(self: Head, allocator: std.mem.Allocator) void {
                    self.body.ref_count -= 1;
                    if (self.body.ref_count == 0) {
                        self.body.child.rel(allocator);
                        allocator.free(std.mem.asBytes(self.body));
                    }
                }

                pub fn count(self: Head) u64 {
                    return self.body.child.count();
                }

                pub fn segmentCount(self: Head, depth: u8) u32 {
                    if (segment_lut[depth] == segment_lut[self.child_depth]) {
                        return self.body.child.segmentCount(depth);
                    } else {
                        return 1;
                    }
                }

                pub fn hash(self: Head, prefix: [key_length]u8) Hash {
                    var key = prefix;
                    const key_start_head = self.child_depth - @minimum(infix_len, self.child_depth);
                    const key_start_body = self.child_depth - @minimum(body_infix_len, self.child_depth);

                    const infix_start_head = @minimum(head_infix_len, depth_to_infix(infix_len, self.child_depth, key_start_head));
                    const infix_start_body = depth_to_infix(body_infix_len, self.child_depth, key_start_body);

                    mem.copy(u8, key[key_start_head..key_start_body], self.infix[infix_start_head..]);
                    mem.copy(u8, key[key_start_body..self.child_depth], self.body.infix[infix_start_body..]);

                    return self.body.child.hash(key);
                }

                pub fn minhash(self: Head, prefix: [key_length]u8, depth: u8) MinHash {
                    _ = self;
                    _ = prefix;
                    _ = depth;
                    return MinHash{};
                    // var key = prefix;
                    // const key_start_head = self.child_depth - @minimum(infix_len, self.child_depth);
                    // const key_start_body = self.child_depth - @minimum(body_infix_len, self.child_depth);

                    // const infix_start_head = @minimum(head_infix_len, depth_to_infix(infix_len, self.child_depth, key_start_head));
                    // const infix_start_body = depth_to_infix(body_infix_len, self.child_depth, key_start_body);

                    // mem.copy(u8, key[key_start_head..key_start_body], self.infix[infix_start_head..]);
                    // mem.copy(u8, key[key_start_body..self.child_depth], self.body.infix[infix_start_body..]);

                    // return self.body.child.hash(key);
                }

                pub fn range(self: Head) u8 {
                    return self.child_depth - @minimum(self.child_depth, infix_len);
                }

                pub fn peek(self: Head, at_depth: u8) ?u8 {
                    if (self.child_depth <= at_depth) return null;
                    const infix_index = depth_to_infix(infix_len, self.child_depth, at_depth);
                    if (infix_index < head_infix_len) {
                        return self.infix[infix_index];
                    }
                    return self.body.infix[infix_index - head_infix_len];
                }

                pub fn propose(self: Head, at_depth: u8, result_set: *ByteBitset) void {
                    if (at_depth == self.child_depth) {
                        // We know that the child has its range maxed out otherwise
                        // there wouldn't be a infix node, so this access is easy and
                        // also cheap.
                        result_set.singleIntersect(self.body.child.peek(at_depth).?);
                        return;
                    }

                    if (self.peek(at_depth)) |byte_key| {
                        result_set.singleIntersect(byte_key);
                        return;
                    }

                    result_set.unsetAll();
                }

                pub fn get(self: Head, at_depth: u8, key: u8) Node {
                    if (at_depth == self.child_depth) {
                        if (self.body.child.peek(at_depth).? == key) {
                            return self.body.child;
                        } else {
                            return Node.none;
                        }
                    }
                    if (self.peek(at_depth)) |own_key| {
                        if (own_key == key) {
                            return @bitCast(Node, self);
                        }
                    }

                    return Node.none;
                }

                pub fn put(self: Head, start_depth: u8, key: [key_length]u8, value: ?T, parent_single_owner: bool, allocator: std.mem.Allocator) allocError!Node {
                    const single_owner = parent_single_owner and self.body.ref_count == 1;

                    var branch_depth = start_depth;
                    while (branch_depth < self.child_depth) : (branch_depth += 1) {
                        if (key[branch_depth] != (self.peek(branch_depth).?)) break;
                    } else {
                        // The entire compressed infix above this node matched with the key.
                        const old_child = self.body.child;
                        const old_child_hash = old_child.hash(key);
                        const new_child = try old_child.put(branch_depth, key, value, single_owner, allocator);
                        const new_child_hash = new_child.hash(key);
                        if (Hash.equal(old_child_hash, new_child_hash)) {
                            return @bitCast(Node, self);
                        }

                        if (new_child.range() != (self.child_depth)) { // TODO We could check if this changes the infix length and save the allocation on single_owner.
                            return try WrapInfixNode(start_depth, key, new_child, allocator);
                        }

                        var self_or_copy = self;
                        if (!single_owner) {
                            self_or_copy = try self.copy(allocator);
                            old_child.rel(allocator);
                        }
                        self_or_copy.body.child = new_child;
                        return @bitCast(Node, self_or_copy);
                    }

                    const sibling_leaf_node = try WrapInfixNode(branch_depth, key, InitLeafOrTwigNode(key, value), allocator);

                    var old_key = key;
                    const key_start_head = self.child_depth - @minimum(infix_len, self.child_depth);
                    const key_start_body = self.child_depth - @minimum(body_infix_len, self.child_depth);

                    const infix_start_head = @minimum(head_infix_len, depth_to_infix(infix_len, self.child_depth, key_start_head));
                    const infix_start_body = depth_to_infix(body_infix_len, self.child_depth, key_start_body);

                    mem.copy(u8, old_key[key_start_head..key_start_body], self.infix[infix_start_head..]);
                    mem.copy(u8, old_key[key_start_body..self.child_depth], self.body.infix[infix_start_body..]);

                    const child_node = try WrapInfixNode(branch_depth, old_key, self.body.child, allocator);
                    const branch_node_above = try BranchNodeBase.initBranch(branch_depth, key, sibling_leaf_node, child_node, allocator);

                    return try WrapInfixNode(start_depth, key, branch_node_above, allocator);
                }

                fn mem_info(self: Head) MemInfo {
                    _ = self;

                    return MemInfo{
                        .active_memory = @sizeOf(Body),
                        .wasted_memory = 0, // TODO this could be more accurate with parent depth info.
                        .passive_memory = @sizeOf(Head),
                        .allocation_count = 1,
                    };
                }
            };
        }

        fn InitLeafOrTwigNode(key: [key_length]u8, maybe_value: ?T) Node {
            if (maybe_value) |value| {
                return @bitCast(Node, LeafNode.init(key, value));
            } else {
                return @bitCast(Node, TwigNode.init(key));
            }
        }

        const LeafNode = extern struct {
            pub const suffix_len = 15 - @sizeOf(T); // TODO Check that this doesn't underflow.
            const key_start = key_length - suffix_len;

            tag: NodeTag = .twig,
            /// The key stored in this entry.
            suffix: [suffix_len]u8 = [_]u8{0} ** suffix_len,
            value: T,

            const Head = @This();

            pub fn init(key: [key_length]u8, value: T) Head {
                var new_head = Head{ .value = value };

                mem.copy(u8, new_head.suffix[0..suffix_len], key[key_start..]);

                return new_head;
            }

            pub fn format(
                self: Head,
                comptime fmt: []const u8,
                options: std.fmt.FormatOptions,
                writer: anytype,
            ) !void {
                _ = self;
                _ = fmt;
                _ = options;
                try writer.print("Twig suffix: {[1]s:_>[0]}\n", .{ suffix_len, std.fmt.fmtSliceHexUpper(&self.suffix) });
            }

            pub fn ref(self: Head, allocator: std.mem.Allocator) allocError!?Node {
                _ = self;
                _ = allocator;
                return null;
            }

            pub fn rel(self: Head, allocator: std.mem.Allocator) void {
                _ = self;
                _ = allocator;
            }

            pub fn count(self: Head) u64 {
                _ = self;
                return 1;
            }

            pub fn segmentCount(self: Head, depth: u8) u32 {
                _ = self;
                _ = depth;
                return 1;
            }

            pub fn hash(self: Head, prefix: [key_length]u8) Hash {
                var key = prefix;
                mem.copy(u8, key[key_start..], self.suffix[0..]);
                return Hash.init(&key);
            }

            pub fn minhash(self: Head, prefix: [key_length]u8, depth: u8) MinHash {
                _ = self;
                _ = prefix;
                _ = depth;
                return MinHash{};
            }

            pub fn range(self: Head) u8 {
                _ = self;
                return key_length - suffix_len;
            }

            pub fn peek(self: Head, at_depth: u8) ?u8 {
                if (key_length <= at_depth) return null;
                return self.suffix[at_depth - key_start];
            }

            pub fn propose(self: Head, at_depth: u8, result_set: *ByteBitset) void {
                if (self.peek(at_depth)) |byte_key| {
                    result_set.singleIntersect(byte_key);
                    return;
                }

                result_set.unsetAll();
            }

            pub fn get(self: Head, at_depth: u8, key: u8) Node {
                if (self.peek(at_depth)) |own_key| {
                    if (own_key == key) return @bitCast(Node, self);
                }
                return Node.none;
            }

            pub fn put(self: Head, start_depth: u8, key: [key_length]u8, value: ?T, single_owner: bool, allocator: std.mem.Allocator) allocError!Node {
                _ = single_owner;

                var branch_depth = start_depth;
                while (branch_depth < key_length) : (branch_depth += 1) {
                    if (key[branch_depth] != (self.peek(branch_depth).?)) break;
                } else {
                    return @bitCast(Node, self);
                }

                const sibling_leaf_node = InitLeafOrTwigNode(key, value);

                return try BranchNodeBase.initBranch(branch_depth, key, sibling_leaf_node, @bitCast(Node, self), allocator);
            }

            fn mem_info(self: Head) MemInfo {
                _ = self;

                return MemInfo{
                    .active_memory = 0,
                    .wasted_memory = 0, // TODO this could be more accurate with parent depth info.
                    .passive_memory = @sizeOf(Head),
                    .allocation_count = 0,
                };
            }
        };

        const TwigNode = extern struct {
            const suffix_len = 15;
            const key_start = key_length - suffix_len;

            tag: NodeTag = .twig,
            /// The key stored in this entry.
            suffix: [suffix_len]u8 = [_]u8{0} ** suffix_len,

            const Head = @This();

            pub fn init(key: [key_length]u8) Head {
                var new_head = Head{};

                mem.copy(u8, new_head.suffix[0..suffix_len], key[key_start..]);

                return new_head;
            }

            pub fn format(
                self: Head,
                comptime fmt: []const u8,
                options: std.fmt.FormatOptions,
                writer: anytype,
            ) !void {
                _ = self;
                _ = fmt;
                _ = options;
                try writer.print("Twig suffix: {[1]s:_>[0]}\n", .{ suffix_len, std.fmt.fmtSliceHexUpper(&self.suffix) });
            }

            pub fn ref(self: Head, allocator: std.mem.Allocator) allocError!?Node {
                _ = self;
                _ = allocator;
                return null;
            }

            pub fn rel(self: Head, allocator: std.mem.Allocator) void {
                _ = self;
                _ = allocator;
            }

            pub fn count(self: Head) u64 {
                _ = self;
                return 1;
            }

            pub fn segmentCount(self: Head, depth: u8) u32 {
                _ = self;
                _ = depth;
                return 1;
            }

            pub fn hash(self: Head, prefix: [key_length]u8) Hash {
                var key = prefix;
                mem.copy(u8, key[key_start..], self.suffix[0..]);
                return Hash.init(&key);
            }

            pub fn minhash(self: Head, prefix: [key_length]u8, depth: u8) MinHash {
                _ = self;
                _ = prefix;
                _ = depth;
                return MinHash{};
            }

            pub fn range(self: Head) u8 {
                _ = self;
                return key_length - suffix_len;
            }

            pub fn peek(self: Head, at_depth: u8) ?u8 {
                if (key_length <= at_depth) return null;
                return self.suffix[at_depth - key_start];
            }

            pub fn propose(self: Head, at_depth: u8, result_set: *ByteBitset) void {
                if (self.peek(at_depth)) |byte_key| {
                    result_set.singleIntersect(byte_key);
                    return;
                }

                result_set.unsetAll();
            }

            pub fn get(self: Head, at_depth: u8, key: u8) Node {
                if (self.peek(at_depth)) |own_key| {
                    if (own_key == key) return @bitCast(Node, self);
                }
                return Node.none;
            }

            pub fn put(self: Head, start_depth: u8, key: [key_length]u8, value: ?T, single_owner: bool, allocator: std.mem.Allocator) allocError!Node {
                _ = single_owner;

                var branch_depth = start_depth;
                while (branch_depth < key_length) : (branch_depth += 1) {
                    if (key[branch_depth] != (self.peek(branch_depth).?)) break;
                } else {
                    return @bitCast(Node, self);
                }

                const sibling_leaf_node = InitLeafOrTwigNode(key, value);

                return try BranchNodeBase.initBranch(branch_depth, key, sibling_leaf_node, @bitCast(Node, self), allocator);
            }

            fn mem_info(self: Head) MemInfo {
                _ = self;

                return MemInfo{
                    .active_memory = 0,
                    .wasted_memory = 0, // TODO this could be more accurate with parent depth info.
                    .passive_memory = @sizeOf(Head),
                    .allocation_count = 0,
                };
            }
        };

        pub const Tree = struct {
            child: Node = Node{ .none = .{} },
            allocator: std.mem.Allocator,

            const NodeIterator = struct {
                start_points: ByteBitset = ByteBitset.initEmpty(),
                path: [key_length]Node = [_]Node{Node.none} ** key_length,
                key: [key_length]u8 = [_]u8{0} ** key_length,
                branch_state: [key_length]ByteBitset = [_]ByteBitset{ByteBitset.initEmpty()} ** key_length,

                const IterationResult = struct {
                    node: Node,
                    start_depth: u8,
                    key: [key_length]u8,
                };

                pub fn next(self: *NodeIterator) ?IterationResult {
                    var start_depth = self.start_points.findLastSet() orelse return null;
                    var node = self.path[start_depth];

                    var branch_depth = start_depth;
                    infix: while (branch_depth < key_length) : (branch_depth += 1) {
                        self.key[branch_depth] = node.peek(branch_depth) orelse break :infix;
                    } else {
                        var exhausted_depth = self.start_points.drainNext(false).?;
                        while (self.start_points.findLastSet()) |parent_depth| {
                            var branches = &self.branch_state[exhausted_depth];
                            if (branches.drainNext(true)) |branch_key| {
                                self.start_points.set(exhausted_depth);
                                self.path[exhausted_depth] = self.path[parent_depth].get(exhausted_depth, branch_key);
                                assert(self.path[exhausted_depth].unknown.tag != .none);
                                break;
                            } else {
                                exhausted_depth = self.start_points.drainNext(false).?;
                            }
                        }
                        return IterationResult{ .start_depth = start_depth, .node = node, .key = self.key };
                    }

                    var branches = &self.branch_state[branch_depth];
                    branches.setAll();
                    node.propose(branch_depth, branches);

                    const branch_key = branches.drainNext(true).?;
                    self.path[branch_depth] = node.get(branch_depth, branch_key);

                    self.start_points.set(branch_depth);
                    return IterationResult{ .start_depth = start_depth, .node = node, .key = self.key };
                }
            };

            pub fn _nodes(self: *const Tree) NodeIterator {
                var iterator = NodeIterator{};
                if (self.child.unknown.tag != .none) {
                    iterator.start_points.set(0);
                    iterator.path[0] = self.child;
                }
                return iterator;
            }

            pub const Cursor = struct {
                depth: u8 = 0,
                path: [key_length]Node = [_]Node{Node.none} ** key_length,
                key: [key_length]u8 = [_]u8{0} ** key_length,

                pub const gaps = blk: {
                    var g = ByteBitset.initEmpty();

                    var depth = 0;
                    for (segments) | s | {
                        const pad = segment_size - s;

                        depth += pad;

                        var j = pad;
                        while (j < segment_size):(j += 1) {
                            g.set(depth);
                            depth += 1;
                        }
                    }

                    break :blk g;
                };

                // Maps push_depth -> path_depth.
                pub const depth_mapping = blk: {
                    var mapping: [256]u8 = [_]u8{0} ** 256;
                    var depth: u16 = 0;
                    var path_depth: u8 = 0;
                    while (depth < 256) {
                        mapping[depth] = path_depth;
                        if(gaps.isSet(depth)) path_depth += 1;
                        depth += 1;
                    }
                    break :blk mapping;
                };

                pub fn init(tree: Tree) @This() {
                    const self = @This(){};
                    self.path[0] = tree.child;
                }

                pub fn peek(self: *Cursor) ?u8 {
                    if (gaps.isUnset(self.depth)) return 0;
                    
                    const path_depth = depth_mapping[self.depth];
                    return self.nodePath[path_depth].peek(path_depth);
                }

                pub fn propose(self: *Cursor, bitset: *ByteBitset) void {
                    if (gaps.isUnset(self.depth)) {
                        bitset.singleBitIntersect(0);
                    } else {
                        const path_depth = depth_mapping[self.depth];
                        self.path[path_depth].propose(path_depth, bitset);
                    }
                }

                pub fn pop(self: *Cursor, times: u8) void {
                    self.depth -= times;
                }

                pub fn push(self: *Cursor, byte: u8) void {
                    if (gaps.isUnset(self.depth)) {
                        const path_depth = depth_mapping[self.depth];
                        self.path[path_depth + 1] = self.nodePath[path_depth].get(path_depth, byte);
                    }
                    self.depth += 1;
                }

                pub fn node(self: *Cursor) Node {
                    const path_depth = depth_mapping[self.depth];
                    return self.path[path_depth];
                }

                pub fn segmentCount(self: *Cursor) u32 {
                    const path_depth = depth_mapping[self.depth];
                    return self.path[path_depth].segmentCount(path_depth);
                }
            };

            pub fn cursor(self: *const Tree) Cursor {
                return Cursor.init(self);
            }

            pub fn init(allocator: std.mem.Allocator) Tree {
                return Tree{ .allocator = allocator };
            }

            pub fn deinit(self: *Tree) void {
                self.child.rel(self.allocator);
            }

            pub fn fork(self: *Tree) allocError!Tree {
                return Tree{ .child = (try self.child.ref(self.allocator)) orelse self.child, .allocator = self.allocator };
            }

            pub fn format(
                self: Tree,
                comptime fmt: []const u8,
                options: std.fmt.FormatOptions,
                writer: anytype,
            ) !void {
                _ = fmt;
                _ = options;

                _ = self;

                //const card = cards.treeCard(self);
                //try writer.print("{s}\n", .{card});
                try writer.writeAll("Tree");
            }

            pub fn count(self: *const Tree) u64 {
                return self.child.count();
            }

            pub fn put(self: *Tree, key: [key_length]u8, value: ?T) allocError!void {
                if (self.child.isNone()) {
                    self.child = try WrapInfixNode(0, key, InitLeafOrTwigNode(key, value), self.allocator);
                } else {
                    self.child = try self.child.put(0, key, value, true, self.allocator);
                }
            }
            
            pub fn get(self: *Tree, key: [key_length]u8) ?T {
              var node = self.child;
              var depth: u8 = 0; 
              while (!node.isNone()) {
                node = node.get(depth, key[depth]);
                if(depth == (key_length - 1)) return node.getValue();
                depth += 1;
              }
              return null;
            }

            pub fn isEmpty(self: *Tree) bool {
                return self.child.isNone();
            }

            pub fn isEqual(self: *Tree, other: *Tree) bool {
                return self.child.hash(undefined).equal(other.child.hash(undefined));
            }

            pub fn mem_info(self: *const Tree) MemInfo {
                var total = MemInfo{ .active_memory = @sizeOf(Tree), .wasted_memory = 0, .passive_memory = 0, .allocation_count = 0 };

                var node_iter = self.nodes();
                while (node_iter.next()) |res| {
                    total = total.combine(res.node.mem_info());
                }

                return total;
            }

            fn recursiveIsSubsetOf(leftNode: Node, rightNode: Node, initial_depth: u8, prefix: [key_length]u8) bool {
                if (leftNode.hash(prefix).equal(rightNode.hash(prefix))) return true;

                const max_depth = std.math.min(leftNode.coveredDepth(), rightNode.coveredDepth());
                var depth = initial_depth;
                while (depth < max_depth):(depth += 1) {
                    const left_peek = leftNode.peek(depth);
                    const right_peek = rightNode.peek(depth);
                    if (left_peek != right_peek) break;
                    prefix[depth] = left_peek;
                }
                if (depth == key_length) return true;

                const left_childbits = ByteBitset.initFull();
                const right_childbits = ByteBitset.initFull();
                const intersect_childbits = ByteBitset.initEmpty();

                leftNode.propose(depth, &left_childbits);
                rightNode.propose(depth, &right_childbits);

                intersect_childbits.setIntersect(left_childbits, right_childbits);
                // The left _only_ child bits.
                left_childbits.setSubtract(left_childbits, intersect_childbits);

                // The existence of children that only exist in the left node,
                // is a witness that prooves that there is no subset relationship.
                if (!left_childbits.isEmpty()) return false;

                while (intersect_childbits.drainNext(true)) | index | {
                    const left_child = leftNode.get(depth, index);
                    const right_child = rightNode.get(depth, index);
                    prefix[depth] = index;
                    if (!recursiveIsSubsetOf(left_child, right_child, depth + 1, prefix)) {
                        return false;
                    }
                }

                return true;
            }

            pub fn isSubsetOf(self: *const Tree, other: *const Tree ) bool {
                return self.child.isNone() or (!other.child.isNone() and recursiveIsSubsetOf(self.child, other.child, 0, undefined));
            }

            fn recursiveIsIntersecting(leftNode: Node, rightNode: Node, initial_depth: u8, prefix: [key_length]u8) bool {
                if (leftNode.hash(prefix).equal(rightNode.hash(prefix))) return true;

                const max_depth = std.math.min(leftNode.coveredDepth(), rightNode.coveredDepth());
                var depth = initial_depth;
                while (depth < max_depth):(depth += 1) {
                    const left_peek = leftNode.peek(depth);
                    const right_peek = rightNode.peek(depth);
                    if (left_peek != right_peek) break;
                    prefix[depth] = left_peek;
                }
                if (depth == key_length) return true;

                const intersect_childbits = ByteBitset.initFull();

                leftNode.propose(depth, intersect_childbits);
                rightNode.propose(depth, intersect_childbits);

                while (intersect_childbits.drainNext(true)) | index | {
                    const left_child = leftNode.get(depth, index);
                    const right_child = rightNode.get(depth, index);
                    prefix[depth] = index;
                    if (recursiveIsIntersecting(left_child, right_child, depth + 1, prefix)) {
                        return true;
                    }
                }

                return false;
            }

            pub fn isIntersecting(self: *const Tree, other: *const Tree ) bool {
                return !self.child.isNone() and
                       !other.child.isNone() and
                       recursiveIsIntersecting(self.child, other.child, 0, undefined);
            }

            fn recursiveUnion(comptime initial_node_count: u8, nodes: []Node, initial_depth: u8, prefix: *[key_length]u8, allocator: std.mem.Allocator) allocError!Node {
                const first_node = nodes[0];
                const other_nodes = nodes[1..];

                const first_node_hash = first_node.hash(prefix.*);
                
                for(other_nodes) |other_node| {
                    if (!first_node_hash.equal(other_node.hash(prefix.*))) break;
                } else {
                    return (try first_node.ref(allocator)) orelse first_node;
                }

                var max_depth = first_node.coveredDepth();
                for (other_nodes) |other_node| {
                    max_depth = std.math.min(max_depth, other_node.coveredDepth());
                }

                var depth = initial_depth;
                outer: while (depth < max_depth):(depth += 1) {
                    const first_peek = first_node.peek(depth).?;
                    for (other_nodes) |other_node| {
                        const other_peek = other_node.peek(depth).?;
                        if (first_peek != other_peek) break :outer;
                    }
                    prefix[depth] = first_peek;
                }

                if (depth == key_length) return (try first_node.ref(allocator)) orelse first_node;

                var union_childbits = ByteBitset.initEmpty(); // TODO use to allocate a better fitting branch node.

                for (nodes) |node| {
                    var node_childbits = ByteBitset.initFull();
                    node.propose(depth, &node_childbits);
                    union_childbits.setUnion(&union_childbits, &node_childbits);

                }

                var branch_node = @bitCast(Node, try BranchNodeBase.init(depth, prefix.*, allocator));

                var children: [initial_node_count]Node = undefined;
                var children_len: u8 = 0;
                while (union_childbits.drainNext(true)) | index | {
                    children_len = 0;
                    prefix[depth] = index;

                    for (nodes) |node| {
                        const child = node.get(depth, index);
                        if(!child.isNone()) {
                            children[children_len] = child;
                            children_len += 1;
                        }
                    }

                    const union_node = try recursiveUnion(initial_node_count, children[0..children_len], depth + 1, prefix, allocator);
                    const new_child_node = try WrapInfixNode(depth, prefix.*, union_node, allocator);

                    var displaced = branch_node.createBranch(new_child_node, depth, prefix.*);
                    while (displaced) |entry| {
                        branch_node = try branch_node.grow(allocator);
                        displaced = branch_node.reinsertBranch(entry);
                    }
                }

                return branch_node;
            }

            pub fn unionAll(comptime tree_count: u8, trees: []Tree, allocator: std.mem.Allocator) allocError!Tree {
                var children: [tree_count]Node = undefined;
                var children_len: u8 = 0;
                for (trees) |tree| {
                    const child = tree.child;
                    if(!child.isNone()) {
                        children[children_len] = child;
                        children_len += 1;
                    }
                }
                
                if(children_len == 0) return Tree{ .allocator = allocator };
                if(children_len == 1) return Tree{ .child = (try children[0].ref(allocator)) orelse children[0], .allocator = allocator };
                
                var prefix: [key_length]u8 = undefined;

                return Tree{ .child = try recursiveUnion(tree_count, children[0..children_len], 0, &prefix, allocator), .allocator = allocator };
            }

        //     subtract(other) {
        //     const thisNode = this.child;
        //     const otherNode = other.child;
        //     if (otherNode === null) {
        //         return new PACTTree(thisNode);
        //     }
        //     if (
        //         this.child === null ||
        //         hash_equal(this.child.hash, other.child.hash)
        //     ) {
        //         return new PACTTree();
        //     } else {
        //         return new PACTTree(_subtract(thisNode, otherNode));
        //     }
        //     }

        //     intersect(other) {
        //     const thisNode = this.child;
        //     const otherNode = other.child;

        //     if (thisNode === null || otherNode === null) {
        //         return new PACTTree(null);
        //     }
        //     if (thisNode === otherNode || hash_equal(thisNode.hash, otherNode.hash)) {
        //         return new PACTTree(otherNode);
        //     }
        //     return new PACTTree(_intersect(thisNode, otherNode));
        //     }

        //     difference(other) {
        //     const thisNode = this.child;
        //     const otherNode = other.child;

        //     if (thisNode === null) {
        //         return new PACTTree(otherNode);
        //     }
        //     if (otherNode === null) {
        //         return new PACTTree(thisNode);
        //     }
        //     if (thisNode === otherNode || hash_equal(thisNode.hash, otherNode.hash)) {
        //         return new PACTTree(null);
        //     }
        //     return new PACTTree(_difference(thisNode, otherNode));
        //     }
        };

        test "Alignment & Size" {
            std.debug.print("{} Size: {}, Alignment: {}\n", .{ Node, @sizeOf(Node), @alignOf(Node) });
            std.debug.print("{} Size: {}, Alignment: {}\n", .{ BranchNodeBase.Head, @sizeOf(BranchNodeBase.Head), @alignOf(BranchNodeBase.Head) });
            std.debug.print("{} Size: {}, Alignment: {}\n", .{ BranchNode(2).Head, @sizeOf(BranchNode(2).Head), @alignOf(BranchNode(2).Head) });
            std.debug.print("{} Size: {}, Alignment: {}\n", .{ BranchNode(4).Head, @sizeOf(BranchNode(4).Head), @alignOf(BranchNode(4).Head) });
            std.debug.print("{} Size: {}, Alignment: {}\n", .{ BranchNode(8).Head, @sizeOf(BranchNode(8).Head), @alignOf(BranchNode(8).Head) });
            std.debug.print("{} Size: {}, Alignment: {}\n", .{ BranchNode(16).Head, @sizeOf(BranchNode(16).Head), @alignOf(BranchNode(16).Head) });
            std.debug.print("{} Size: {}, Alignment: {}\n", .{ BranchNode(32).Head, @sizeOf(BranchNode(32).Head), @alignOf(BranchNode(32).Head) });
            std.debug.print("{} Size: {}, Alignment: {}\n", .{ BranchNode(64).Head, @sizeOf(BranchNode(64).Head), @alignOf(BranchNode(64).Head) });
            std.debug.print("{} Size: {}, Alignment: {}\n", .{ BranchNodeBase.Body, @sizeOf(BranchNodeBase.Body), @alignOf(BranchNodeBase.Body) });
            std.debug.print("{} Size: {}, Alignment: {}\n", .{ BranchNode(2).Body, @sizeOf(BranchNode(2).Body), @alignOf(BranchNode(2).Body) });
            std.debug.print("{} Size: {}, Alignment: {}\n", .{ BranchNode(4).Body, @sizeOf(BranchNode(4).Body), @alignOf(BranchNode(4).Body) });
            std.debug.print("{} Size: {}, Alignment: {}\n", .{ BranchNode(8).Body, @sizeOf(BranchNode(8).Body), @alignOf(BranchNode(8).Body) });
            std.debug.print("{} Size: {}, Alignment: {}\n", .{ BranchNode(16).Body, @sizeOf(BranchNode(16).Body), @alignOf(BranchNode(16).Body) });
            std.debug.print("{} Size: {}, Alignment: {}\n", .{ BranchNode(32).Body, @sizeOf(BranchNode(32).Body), @alignOf(BranchNode(32).Body) });
            std.debug.print("{} Size: {}, Alignment: {}\n", .{ BranchNode(64).Body, @sizeOf(BranchNode(64).Body), @alignOf(BranchNode(64).Body) });
        }
    };
}

// test "create tree" {
//     var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{ .verbose_log = true, .retain_metadata = true, .safety = true }){};
//     defer _ = general_purpose_allocator.deinit();
//     const gpa = general_purpose_allocator.allocator();

//     var tree = Tree.init(gpa);
//     defer tree.deinit();
// }

// test "empty tree has count 0" {
//     var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{ .verbose_log = true, .retain_metadata = true, .safety = true }){};
//     defer _ = general_purpose_allocator.deinit();
//     const gpa = general_purpose_allocator.allocator();

//     var tree = Tree.init(gpa);
//     defer tree.deinit();

//     try expectEqual(tree.count(), 0);
// }

// test "single item tree has count 1" {
//     var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{ .verbose_log = true, .retain_metadata = true, .safety = true }){};
//     defer _ = general_purpose_allocator.deinit();
//     const gpa = general_purpose_allocator.allocator();

//     var tree = Tree.init(gpa);
//     defer tree.deinit();

//     const key: [key_length]u8 = [_]u8{0} ** key_length;
//     try tree.put(&key, 42);

//     try expectEqual(tree.count(), 1);
// }

// test "immutable tree fork" {
//     var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{ .verbose_log = true }){};
//     defer _ = general_purpose_allocator.deinit();
//     const gpa = general_purpose_allocator.allocator();

//     var tree = Tree.init(gpa);
//     defer tree.deinit();

//     var new_tree = try tree.fork();
//     defer new_tree.deinit();

//     const key: [key_length]u8 = [_]u8{0} ** key_length;
//     try new_tree.put(&key, 42);

//     try expectEqual(tree.count(), 0);
//     try expectEqual(new_tree.count(), 1);
// }

// test "multi item tree has correct count" {
//     var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{ .verbose_log = true, .retain_metadata = true, .safety = true }){};
//     defer _ = general_purpose_allocator.deinit();
//     const gpa = general_purpose_allocator.allocator();

//     const total_runs = 10;

//     var rnd = std.rand.DefaultPrng.init(0).random();

//     var tree = Tree.init(gpa);
//     defer tree.deinit();

//     var key: [key_length]u8 = undefined;

//     var i: u64 = 0;
//     while (i < total_runs) : (i += 1) {
//         try expectEqual(tree.count(), i);

//         rnd.bytes(&key);
//         try tree.put(&key, rnd.int(usize));
//         std.debug.print("Inserted {d} of {d}:{s}\n{s}\n", .{ i + 1, total_runs, std.fmt.fmtSliceHexUpper(&key), tree.child });
//     }
//     try expectEqual(tree.count(), total_runs);
// }
