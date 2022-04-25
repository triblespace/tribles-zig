const std = @import("std");
const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;
const ByteBitset = @import("ByteBitset.zig").ByteBitset;
const Card = @import("Card.zig").Card;

const mem = std.mem;

// TODO: change hash set index to boolean or at least make set -> 1, unset -> 0

// Uninitialized memory initialized by init()
var instance_secret: [16]u8 = undefined;

pub fn init() void {
    // XXX: (crest) Should this be a deterministic pseudo-RNG seeded by a constant for reproducable tests?
    std.crypto.random.bytes(&instance_secret);
}

pub const Hash = extern struct {
    data: [16]u8,

    pub fn xor(left: Hash, right: Hash) Hash { // TODO make this vector SIMD stuff?
        var hash: Hash = undefined;
        for (hash.data) |*byte, i| {
            byte.* = left.data[i] ^ right.data[i];
        }
        return hash;
    }

    pub fn equal(left: Hash, right: Hash) bool {
        return std.mem.eql(u8, &left.data, &right.data);
    }
};
// const Vector = std.meta.Vector;
// const Hash = Vector(16, u8);

pub fn keyHash(key: []const u8) Hash {
    const siphash = comptime std.hash.SipHash128(2, 4);
    var hash: Hash = undefined;
    siphash.create(&hash.data, key, &instance_secret);

    return hash;
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

pub const key_length = 64;
pub const T = u64;

const allocError = std.mem.Allocator.Error;

const NodeTag = enum(u8) {
    none,
    inner1,
    inner2,
    inner4,
    inner8,
    inner16,
    inner32,
    inner64,
    leaf8,
    leaf16,
    leaf24,
    leaf32,
    leaf40,
    leaf48,
    leaf56,
    leaf64,
    twig15,
    twig16,
    twig24,
    twig32,
    twig40,
    twig48,
    twig56,
    twig64,
};

const Node = extern union {
    unknown: extern struct {
        tag: NodeTag,
        padding: [15]u8 = undefined,
    },
    none: extern struct {
        tag: NodeTag = .none,
        padding: [15]u8 = undefined,
    },
    inner1: InnerNode(1),
    inner2: InnerNode(2),
    inner4: InnerNode(4),
    inner8: InnerNode(8),
    inner16: InnerNode(16),
    inner32: InnerNode(32),
    inner64: InnerNode(64),
    leaf8: LeafNode(false, 8),
    leaf16: LeafNode(false, 16),
    leaf24: LeafNode(false, 24),
    leaf32: LeafNode(false, 32),
    leaf40: LeafNode(false, 40),
    leaf48: LeafNode(false, 48),
    leaf56: LeafNode(false, 56),
    leaf64: LeafNode(false, 64),
    twig15: InlineLeafNode,
    twig16: LeafNode(true, 16),
    twig24: LeafNode(true, 24),
    twig32: LeafNode(true, 32),
    twig40: LeafNode(true, 40),
    twig48: LeafNode(true, 48),
    twig56: LeafNode(true, 56),
    twig64: LeafNode(true, 64),

    const none = Node{ .none = .{ .tag = .none } };

    fn innerNodeTag(comptime bucket_count: u8) NodeTag {
        return switch (bucket_count) {
            1 => NodeTag.inner1,
            2 => NodeTag.inner2,
            4 => NodeTag.inner4,
            8 => NodeTag.inner8,
            16 => NodeTag.inner16,
            32 => NodeTag.inner32,
            64 => NodeTag.inner64,
            else => @panic("Bad bucket count for tag."),
        };
    }

    fn leafNodeTag(comptime no_value: bool, comptime suffix_len: u8) NodeTag {
        return if (no_value) switch (suffix_len) {
            16 => NodeTag.twig16,
            24 => NodeTag.twig24,
            32 => NodeTag.twig32,
            40 => NodeTag.twig40,
            48 => NodeTag.twig48,
            56 => NodeTag.twig56,
            64 => NodeTag.twig64,
            else => @panic("Bad suffix count for twig tag."),
        } else switch (suffix_len) {
            8 => NodeTag.leaf8,
            16 => NodeTag.leaf16,
            24 => NodeTag.leaf24,
            32 => NodeTag.leaf32,
            40 => NodeTag.leaf40,
            48 => NodeTag.leaf48,
            56 => NodeTag.leaf56,
            64 => NodeTag.leaf64,
            else => @panic("Bad suffix count for leaf tag."),
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
            .inner1 => try writer.print("{s}", .{self.inner1}),
            .inner2 => try writer.print("{s}", .{self.inner2}),
            .inner4 => try writer.print("{s}", .{self.inner4}),
            .inner8 => try writer.print("{s}", .{self.inner8}),
            .inner16 => try writer.print("{s}", .{self.inner16}),
            .inner32 => try writer.print("{s}", .{self.inner32}),
            .inner64 => try writer.print("{s}", .{self.inner64}),
            .leaf8 => try writer.print("{s}", .{self.leaf8}),
            .leaf16 => try writer.print("{s}", .{self.leaf16}),
            .leaf24 => try writer.print("{s}", .{self.leaf24}),
            .leaf32 => try writer.print("{s}", .{self.leaf32}),
            .leaf40 => try writer.print("{s}", .{self.leaf40}),
            .leaf48 => try writer.print("{s}", .{self.leaf48}),
            .leaf56 => try writer.print("{s}", .{self.leaf56}),
            .leaf64 => try writer.print("{s}", .{self.leaf64}),
            .twig15 => try writer.print("{s}", .{self.twig15}),
            .twig16 => try writer.print("{s}", .{self.twig16}),
            .twig24 => try writer.print("{s}", .{self.twig24}),
            .twig32 => try writer.print("{s}", .{self.twig32}),
            .twig40 => try writer.print("{s}", .{self.twig40}),
            .twig48 => try writer.print("{s}", .{self.twig48}),
            .twig56 => try writer.print("{s}", .{self.twig56}),
            .twig64 => try writer.print("{s}", .{self.twig64}),
        }
        try writer.writeAll("");
    }

    pub fn ref(self: Node, allocator: std.mem.Allocator) allocError!?Node {
        return switch (self.unknown.tag) {
            .none => Node{ .none = .{} },
            .inner1 => self.inner1.ref(allocator),
            .inner2 => self.inner2.ref(allocator),
            .inner4 => self.inner4.ref(allocator),
            .inner8 => self.inner8.ref(allocator),
            .inner16 => self.inner16.ref(allocator),
            .inner32 => self.inner32.ref(allocator),
            .inner64 => self.inner64.ref(allocator),
            .leaf8 => self.leaf8.ref(allocator),
            .leaf16 => self.leaf16.ref(allocator),
            .leaf24 => self.leaf24.ref(allocator),
            .leaf32 => self.leaf32.ref(allocator),
            .leaf40 => self.leaf40.ref(allocator),
            .leaf48 => self.leaf48.ref(allocator),
            .leaf56 => self.leaf56.ref(allocator),
            .leaf64 => self.leaf64.ref(allocator),
            .twig15 => self.twig15.ref(allocator),
            .twig16 => self.twig16.ref(allocator),
            .twig24 => self.twig24.ref(allocator),
            .twig32 => self.twig32.ref(allocator),
            .twig40 => self.twig40.ref(allocator),
            .twig48 => self.twig48.ref(allocator),
            .twig56 => self.twig56.ref(allocator),
            .twig64 => self.twig64.ref(allocator),
        };
    }

    pub fn rel(self: Node, allocator: std.mem.Allocator) void {
        switch (self.unknown.tag) {
            .none => {},
            .inner1 => self.inner1.rel(allocator),
            .inner2 => self.inner2.rel(allocator),
            .inner4 => self.inner4.rel(allocator),
            .inner8 => self.inner8.rel(allocator),
            .inner16 => self.inner16.rel(allocator),
            .inner32 => self.inner32.rel(allocator),
            .inner64 => self.inner64.rel(allocator),
            .leaf8 => self.leaf8.rel(allocator),
            .leaf16 => self.leaf16.rel(allocator),
            .leaf24 => self.leaf24.rel(allocator),
            .leaf32 => self.leaf32.rel(allocator),
            .leaf40 => self.leaf40.rel(allocator),
            .leaf48 => self.leaf48.rel(allocator),
            .leaf56 => self.leaf56.rel(allocator),
            .leaf64 => self.leaf64.rel(allocator),
            .twig15 => self.twig15.rel(allocator),
            .twig16 => self.twig16.rel(allocator),
            .twig24 => self.twig24.rel(allocator),
            .twig32 => self.twig32.rel(allocator),
            .twig40 => self.twig40.rel(allocator),
            .twig48 => self.twig48.rel(allocator),
            .twig56 => self.twig56.rel(allocator),
            .twig64 => self.twig64.rel(allocator),
        }
    }

    pub fn count(self: Node) u64 {
        return switch (self.unknown.tag) {
            .none => 0,
            .inner1 => self.inner1.count(),
            .inner2 => self.inner2.count(),
            .inner4 => self.inner4.count(),
            .inner8 => self.inner8.count(),
            .inner16 => self.inner16.count(),
            .inner32 => self.inner32.count(),
            .inner64 => self.inner64.count(),
            .leaf8 => self.leaf8.count(),
            .leaf16 => self.leaf16.count(),
            .leaf24 => self.leaf24.count(),
            .leaf32 => self.leaf32.count(),
            .leaf40 => self.leaf40.count(),
            .leaf48 => self.leaf48.count(),
            .leaf56 => self.leaf56.count(),
            .leaf64 => self.leaf64.count(),
            .twig15 => self.twig15.count(),
            .twig16 => self.twig16.count(),
            .twig24 => self.twig24.count(),
            .twig32 => self.twig32.count(),
            .twig40 => self.twig40.count(),
            .twig48 => self.twig48.count(),
            .twig56 => self.twig56.count(),
            .twig64 => self.twig64.count(),
        };
    }

    pub fn hash(self: Node, start_depth: u8, prefix: *const [key_length]u8) Hash {
        return switch (self.unknown.tag) {
            .none => @panic("Called `hash` on none."),
            .inner1 => self.inner1.hash(start_depth, prefix),
            .inner2 => self.inner2.hash(start_depth, prefix),
            .inner4 => self.inner4.hash(start_depth, prefix),
            .inner8 => self.inner8.hash(start_depth, prefix),
            .inner16 => self.inner16.hash(start_depth, prefix),
            .inner32 => self.inner32.hash(start_depth, prefix),
            .inner64 => self.inner64.hash(start_depth, prefix),
            .leaf8 => self.leaf8.hash(start_depth, prefix),
            .leaf16 => self.leaf16.hash(start_depth, prefix),
            .leaf24 => self.leaf24.hash(start_depth, prefix),
            .leaf32 => self.leaf32.hash(start_depth, prefix),
            .leaf40 => self.leaf40.hash(start_depth, prefix),
            .leaf48 => self.leaf48.hash(start_depth, prefix),
            .leaf56 => self.leaf56.hash(start_depth, prefix),
            .leaf64 => self.leaf64.hash(start_depth, prefix),
            .twig15 => self.twig15.hash(start_depth, prefix),
            .twig16 => self.twig16.hash(start_depth, prefix),
            .twig24 => self.twig24.hash(start_depth, prefix),
            .twig32 => self.twig32.hash(start_depth, prefix),
            .twig40 => self.twig40.hash(start_depth, prefix),
            .twig48 => self.twig48.hash(start_depth, prefix),
            .twig56 => self.twig56.hash(start_depth, prefix),
            .twig64 => self.twig64.hash(start_depth, prefix),
        };
    }

    pub fn depth(self: Node) u8 {
        return switch (self.unknown.tag) {
            .none => @panic("Called `depth` on none."),
            .inner1 => self.inner1.depth(),
            .inner2 => self.inner2.depth(),
            .inner4 => self.inner4.depth(),
            .inner8 => self.inner8.depth(),
            .inner16 => self.inner16.depth(),
            .inner32 => self.inner32.depth(),
            .inner64 => self.inner64.depth(),
            .leaf8 => self.leaf8.depth(),
            .leaf16 => self.leaf16.depth(),
            .leaf24 => self.leaf24.depth(),
            .leaf32 => self.leaf32.depth(),
            .leaf40 => self.leaf40.depth(),
            .leaf48 => self.leaf48.depth(),
            .leaf56 => self.leaf56.depth(),
            .leaf64 => self.leaf64.depth(),
            .twig15 => self.twig15.depth(),
            .twig16 => self.twig16.depth(),
            .twig24 => self.twig24.depth(),
            .twig32 => self.twig32.depth(),
            .twig40 => self.twig40.depth(),
            .twig48 => self.twig48.depth(),
            .twig56 => self.twig56.depth(),
            .twig64 => self.twig64.depth(),
        };
    }

    pub fn peekFirst(self: Node) u8 {
        return switch (self.unknown.tag) {
            .none => @panic("Called `peek` on none."),
            .inner1 => self.inner1.peekFirst(),
            .inner2 => self.inner2.peekFirst(),
            .inner4 => self.inner4.peekFirst(),
            .inner8 => self.inner8.peekFirst(),
            .inner16 => self.inner16.peekFirst(),
            .inner32 => self.inner32.peekFirst(),
            .inner64 => self.inner64.peekFirst(),
            .leaf8 => self.leaf8.peekFirst(),
            .leaf16 => self.leaf16.peekFirst(),
            .leaf24 => self.leaf24.peekFirst(),
            .leaf32 => self.leaf32.peekFirst(),
            .leaf40 => self.leaf40.peekFirst(),
            .leaf48 => self.leaf48.peekFirst(),
            .leaf56 => self.leaf56.peekFirst(),
            .leaf64 => self.leaf64.peekFirst(),
            .twig15 => self.twig15.peekFirst(),
            .twig16 => self.twig16.peekFirst(),
            .twig24 => self.twig24.peekFirst(),
            .twig32 => self.twig32.peekFirst(),
            .twig40 => self.twig40.peekFirst(),
            .twig48 => self.twig48.peekFirst(),
            .twig56 => self.twig56.peekFirst(),
            .twig64 => self.twig64.peekFirst(),
        };
    }

    pub fn peek(self: Node, start_depth: u8, at_depth: u8) ?u8 {
        return switch (self.unknown.tag) {
            .none => @panic("Called `peek` on none."),
            .inner1 => self.inner1.peek(start_depth, at_depth),
            .inner2 => self.inner2.peek(start_depth, at_depth),
            .inner4 => self.inner4.peek(start_depth, at_depth),
            .inner8 => self.inner8.peek(start_depth, at_depth),
            .inner16 => self.inner16.peek(start_depth, at_depth),
            .inner32 => self.inner32.peek(start_depth, at_depth),
            .inner64 => self.inner64.peek(start_depth, at_depth),
            .leaf8 => self.leaf8.peek(start_depth, at_depth),
            .leaf16 => self.leaf16.peek(start_depth, at_depth),
            .leaf24 => self.leaf24.peek(start_depth, at_depth),
            .leaf32 => self.leaf32.peek(start_depth, at_depth),
            .leaf40 => self.leaf40.peek(start_depth, at_depth),
            .leaf48 => self.leaf48.peek(start_depth, at_depth),
            .leaf56 => self.leaf56.peek(start_depth, at_depth),
            .leaf64 => self.leaf64.peek(start_depth, at_depth),
            .twig15 => self.twig15.peek(start_depth, at_depth),
            .twig16 => self.twig16.peek(start_depth, at_depth),
            .twig24 => self.twig24.peek(start_depth, at_depth),
            .twig32 => self.twig32.peek(start_depth, at_depth),
            .twig40 => self.twig40.peek(start_depth, at_depth),
            .twig48 => self.twig48.peek(start_depth, at_depth),
            .twig56 => self.twig56.peek(start_depth, at_depth),
            .twig64 => self.twig64.peek(start_depth, at_depth),
        };
    }

    pub fn propose(self: Node, start_depth: u8, at_depth: u8, result_set: *ByteBitset) void {
        return switch (self.unknown.tag) {
            .none => @panic("Called `propose` on none."),
            .inner1 => self.inner1.propose(start_depth, at_depth, result_set),
            .inner2 => self.inner2.propose(start_depth, at_depth, result_set),
            .inner4 => self.inner4.propose(start_depth, at_depth, result_set),
            .inner8 => self.inner8.propose(start_depth, at_depth, result_set),
            .inner16 => self.inner16.propose(start_depth, at_depth, result_set),
            .inner32 => self.inner32.propose(start_depth, at_depth, result_set),
            .inner64 => self.inner64.propose(start_depth, at_depth, result_set),
            .leaf8 => self.leaf8.propose(start_depth, at_depth, result_set),
            .leaf16 => self.leaf16.propose(start_depth, at_depth, result_set),
            .leaf24 => self.leaf24.propose(start_depth, at_depth, result_set),
            .leaf32 => self.leaf32.propose(start_depth, at_depth, result_set),
            .leaf40 => self.leaf40.propose(start_depth, at_depth, result_set),
            .leaf48 => self.leaf48.propose(start_depth, at_depth, result_set),
            .leaf56 => self.leaf56.propose(start_depth, at_depth, result_set),
            .leaf64 => self.leaf64.propose(start_depth, at_depth, result_set),
            .twig15 => self.twig15.propose(start_depth, at_depth, result_set),
            .twig16 => self.twig16.propose(start_depth, at_depth, result_set),
            .twig24 => self.twig24.propose(start_depth, at_depth, result_set),
            .twig32 => self.twig32.propose(start_depth, at_depth, result_set),
            .twig40 => self.twig40.propose(start_depth, at_depth, result_set),
            .twig48 => self.twig48.propose(start_depth, at_depth, result_set),
            .twig56 => self.twig56.propose(start_depth, at_depth, result_set),
            .twig64 => self.twig64.propose(start_depth, at_depth, result_set),
        };
    }

    pub fn get(self: Node, start_depth: u8, at_depth: u8, byte_key: u8) Node {
        return switch (self.unknown.tag) {
            .none => @panic("Called `get` on none."),
            .inner1 => self.inner1.get(start_depth, at_depth, byte_key),
            .inner2 => self.inner2.get(start_depth, at_depth, byte_key),
            .inner4 => self.inner4.get(start_depth, at_depth, byte_key),
            .inner8 => self.inner8.get(start_depth, at_depth, byte_key),
            .inner16 => self.inner16.get(start_depth, at_depth, byte_key),
            .inner32 => self.inner32.get(start_depth, at_depth, byte_key),
            .inner64 => self.inner64.get(start_depth, at_depth, byte_key),
            .leaf8 => self.leaf8.get(start_depth, at_depth, byte_key),
            .leaf16 => self.leaf16.get(start_depth, at_depth, byte_key),
            .leaf24 => self.leaf24.get(start_depth, at_depth, byte_key),
            .leaf32 => self.leaf32.get(start_depth, at_depth, byte_key),
            .leaf40 => self.leaf40.get(start_depth, at_depth, byte_key),
            .leaf48 => self.leaf48.get(start_depth, at_depth, byte_key),
            .leaf56 => self.leaf56.get(start_depth, at_depth, byte_key),
            .leaf64 => self.leaf64.get(start_depth, at_depth, byte_key),
            .twig15 => self.twig15.get(start_depth, at_depth, byte_key),
            .twig16 => self.twig16.get(start_depth, at_depth, byte_key),
            .twig24 => self.twig24.get(start_depth, at_depth, byte_key),
            .twig32 => self.twig32.get(start_depth, at_depth, byte_key),
            .twig40 => self.twig40.get(start_depth, at_depth, byte_key),
            .twig48 => self.twig48.get(start_depth, at_depth, byte_key),
            .twig56 => self.twig56.get(start_depth, at_depth, byte_key),
            .twig64 => self.twig64.get(start_depth, at_depth, byte_key),
        };
    }

    pub fn put(self: Node, start_depth: u8, key: *const [key_length]u8, value: ?T, single_owner: bool, allocator: std.mem.Allocator) allocError!Node {
        return switch (self.unknown.tag) {
            .none => @panic("Called `put` on none."),
            .inner1 => self.inner1.put(start_depth, key, value, single_owner, allocator),
            .inner2 => self.inner2.put(start_depth, key, value, single_owner, allocator),
            .inner4 => self.inner4.put(start_depth, key, value, single_owner, allocator),
            .inner8 => self.inner8.put(start_depth, key, value, single_owner, allocator),
            .inner16 => self.inner16.put(start_depth, key, value, single_owner, allocator),
            .inner32 => self.inner32.put(start_depth, key, value, single_owner, allocator),
            .inner64 => self.inner64.put(start_depth, key, value, single_owner, allocator),
            .leaf8 => self.leaf8.put(start_depth, key, value, single_owner, allocator),
            .leaf16 => self.leaf16.put(start_depth, key, value, single_owner, allocator),
            .leaf24 => self.leaf24.put(start_depth, key, value, single_owner, allocator),
            .leaf32 => self.leaf32.put(start_depth, key, value, single_owner, allocator),
            .leaf40 => self.leaf40.put(start_depth, key, value, single_owner, allocator),
            .leaf48 => self.leaf48.put(start_depth, key, value, single_owner, allocator),
            .leaf56 => self.leaf56.put(start_depth, key, value, single_owner, allocator),
            .leaf64 => self.leaf64.put(start_depth, key, value, single_owner, allocator),
            .twig15 => self.twig15.put(start_depth, key, value, single_owner, allocator),
            .twig16 => self.twig16.put(start_depth, key, value, single_owner, allocator),
            .twig24 => self.twig24.put(start_depth, key, value, single_owner, allocator),
            .twig32 => self.twig32.put(start_depth, key, value, single_owner, allocator),
            .twig40 => self.twig40.put(start_depth, key, value, single_owner, allocator),
            .twig48 => self.twig48.put(start_depth, key, value, single_owner, allocator),
            .twig56 => self.twig56.put(start_depth, key, value, single_owner, allocator),
            .twig64 => self.twig64.put(start_depth, key, value, single_owner, allocator),
        };
    }

    fn cuckooPut(self: Node, node: Node) ?Node {
        return switch (self.unknown.tag) {
            .inner1 => self.inner1.cuckooPut(node),
            .inner2 => self.inner2.cuckooPut(node),
            .inner4 => self.inner4.cuckooPut(node),
            .inner8 => self.inner8.cuckooPut(node),
            .inner16 => self.inner16.cuckooPut(node),
            .inner32 => self.inner32.cuckooPut(node),
            .inner64 => self.inner64.cuckooPut(node),
            .none => @panic("Called `cuckooPut` on none."),
            else => @panic("Called `cuckooPut` on non-inner node."),
        };
    }

    fn grow(self: Node, allocator: std.mem.Allocator) allocError!Node {
        return switch (self.unknown.tag) {
            .inner1 => self.inner1.grow(allocator),
            .inner2 => self.inner2.grow(allocator),
            .inner4 => self.inner4.grow(allocator),
            .inner8 => self.inner8.grow(allocator),
            .inner16 => self.inner16.grow(allocator),
            .inner32 => self.inner32.grow(allocator),
            .inner64 => self.inner64.grow(allocator),
            .none => @panic("Called `grow` on none."),
            else => @panic("Called `grow` on non-inner node."),
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

const max_bucket_count = 64; // BRANCH_FACTOR / Bucket.SLOT_COUNT;

const HLL = extern struct {
    pub const bucket_count = 32;
    buckets: [bucket_count]u8,

    pub fn init() HLL {
        return HLL{ .buckets = [_]u8{0} ** bucket_count };
    }
};

fn InnerNode(comptime bucket_count: u8) type {
    const head_infix_len = 6;
    const buffer_size = max_bucket_count - bucket_count;

    return extern struct {
        tag: NodeTag = Node.innerNodeTag(bucket_count),
        /// The infix stored in this head.
        infix: [head_infix_len]u8 = [_]u8{0} ** head_infix_len,
        /// The branch depth of the body.
        branch_depth: u8,
        /// The address of the pointer associated with the key.
        body: *Body,

        const Head = @This();

        const GrownHead = if (bucket_count == max_bucket_count) Head else InnerNode(bucket_count << 1);

        const BODY_ALIGNMENT = 64;

        const Body = extern struct {
            leaf_count: u64,
            ref_count: u32 = 1,
            buffer_count: u32 = 0,
            child_sum_hash: Hash = .{ .data = [_]u8{0} ** 16 },
            segment_hll: HLL = HLL.init(),
            infix: [key_length]u8,
            child_set: ByteBitset = ByteBitset.initEmpty(),
            rand_hash_used: ByteBitset = ByteBitset.initEmpty(),
            buckets: Buckets = if (bucket_count == 1) [_]Bucket{Bucket{}} else undefined,
            buffer: Buffer = undefined,

            const Buckets = [bucket_count]Bucket;
            const Buffer = [buffer_size][key_length]u8;

            const Bucket = extern struct {
                const SLOT_COUNT = 4;

                slots: [SLOT_COUNT]Node = [_]Node{Node{ .none = .{} }} ** SLOT_COUNT,

                pub fn get(self: *const Bucket, byte_key: u8) Node {
                    for (self.slots) |slot| {
                        if (slot.unknown.tag != .none and slot.peekFirst() == byte_key) {
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
                        if (slot.unknown.tag != .none and slot.peekFirst() == entry.peekFirst()) {
                            slot.* = entry;
                            return true;
                        }
                    }
                    for (self.slots) |*slot| {
                        if (slot.unknown.tag == .none) {
                            slot.* = entry;
                            return true;
                        }
                        const slot_key = slot.peekFirst();
                        if (bucket_index != hashByteKey(rand_hash_used.isSet(slot_key), current_count, slot_key)) {
                            slot.* = entry;
                            return true;
                        }
                    }
                    return false;
                }

                /// Updates the pointer for the key stored in this bucket.
                pub fn update(
                    self: *Bucket,
                    // / The new entry value.
                    entry: Node,
                ) void {
                    for (self.slots) |*slot| {
                        if (slot.unknown.tag != .none and slot.peekFirst() == entry.peekFirst()) {
                            slot.* = entry;
                            return;
                        }
                    }
                }

                /// Displaces a random existing slot.
                pub fn displaceRandomly(
                    self: *Bucket,
                    // / A random value to determine the slot to displace.
                    random_value: u8,
                    // / The entry that displaces an existing entry.
                    entry: Node,
                ) Node {
                    const index = random_value & (SLOT_COUNT - 1);
                    const prev = self.slots[index];
                    self.slots[index] = entry;
                    return prev;
                }

                /// Displaces the first slot that is using the alternate hash function.
                pub fn displaceRandHashOnly(
                    self: *Bucket,
                    // / Determines the hash function used for each key and is used to detect outdated (free) slots.
                    rand_hash_used: *ByteBitset,
                    // / The entry to be stored in the bucket.
                    entry: Node,
                ) Node {
                    for (self.slots) |*slot| {
                        if (rand_hash_used.isSet(slot.peekFirst())) {
                            const prev = slot.*;
                            slot.* = entry;
                            return prev;
                        }
                    }
                    unreachable;
                }
            };
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
            var card = Card.from(
                \\┌────────────────────────────────────────────────────────────────────────────────┐
                \\│ Inner Node @󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀                                                   │
                \\│━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━                                                  │
                \\│                                                                                │
                \\│ Metadata                                                                       │
                \\│ ═════════                                                                      │
                \\│                                                                                │
                \\│   Hash: 󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁    Leafs: 󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂        │
                \\│   Ref#: 󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃                Depth: 󰀇󰀇                          │
                \\│                                                                                │
                \\│ Infix                                                                          │
                \\│ ══════                                                                         │
                \\│                                                                                │
                \\│   Head: 󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅                                                           │
                \\│                                                                                │
                \\│   Body: 󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆       │
                \\│         󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆       │
                \\│         ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔         │
                \\│ Children                                                                       │
                \\│ ══════════                                                                     │
                \\│                         TODO add %      0123456789ABCDEF     0123456789ABCDEF  │
                \\│  ▼                                     ┌────────────────┐   ┌────────────────┐ │
                \\│  ┌                  ● Seq Hash       0_│󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏│ 8_│󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐│ │
                \\│  │󰀈                 ◆ Rand Hash      1_│󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏│ 9_│󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐│ │
                \\│  │󰀉                 ○ Seq Missing    2_│󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏│ A_│󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐│ │
                \\│  │󰀊󰀊                ◇ Rand Missing   3_│󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏│ B_│󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐│ │
                \\│  │󰀋󰀋󰀋󰀋                               4_│󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏│ C_│󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐│ │
                \\│  │󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌                           5_│󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏│ D_│󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐│ │
                \\│  │󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍                   6_│󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏│ E_│󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐│ │
                \\│  │󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎   7_│󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏│ F_│󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐│ │
                \\│  └                                     └────────────────┘   └────────────────┘ │
                \\└────────────────────────────────────────────────────────────────────────────────┘
            ) catch unreachable;

            var addr_data: [16:0]u8 = undefined;
            _ = std.fmt.bufPrint(&addr_data, "{x:0>16}", .{@ptrToInt(self.body)}) catch unreachable;
            var addr_iter = (std.unicode.Utf8View.init(&addr_data) catch unreachable).iterator();

            var hash_data: [32:0]u8 = undefined;
            _ = std.fmt.bufPrint(&hash_data, "{s:_>32}", .{std.fmt.fmtSliceHexUpper(&self.body.child_sum_hash.data)}) catch unreachable;
            var hash_iter = (std.unicode.Utf8View.init(&hash_data) catch unreachable).iterator();

            var leaf_count_data: [20:0]u8 = undefined;
            _ = std.fmt.bufPrint(&leaf_count_data, "{d:_>20}", .{self.body.leaf_count}) catch unreachable;
            var leaf_count_iter = (std.unicode.Utf8View.init(&leaf_count_data) catch unreachable).iterator();

            var ref_count_data: [20:0]u8 = undefined;
            _ = std.fmt.bufPrint(&ref_count_data, "{d:_>20}", .{self.body.ref_count}) catch unreachable;
            var ref_count_iter = (std.unicode.Utf8View.init(&ref_count_data) catch unreachable).iterator();

            var head_infix_data: [12:0]u8 = undefined;
            _ = std.fmt.bufPrint(&head_infix_data, "{s:_>12}", .{std.fmt.fmtSliceHexUpper(&self.infix)}) catch unreachable;
            var head_infix_iter = (std.unicode.Utf8View.init(&head_infix_data) catch unreachable).iterator();

            var body_infix_data: [128:0]u8 = undefined;
            _ = std.fmt.bufPrint(&body_infix_data, "{s:_>128}", .{std.fmt.fmtSliceHexUpper(&self.body.infix)}) catch unreachable;
            var body_infix_iter = (std.unicode.Utf8View.init(&body_infix_data) catch unreachable).iterator();

            var branch_depth_data: [2:0]u8 = undefined;
            _ = std.fmt.bufPrint(&branch_depth_data, "{d:_>2}", .{self.branch_depth}) catch unreachable;
            var branch_depth_iter = (std.unicode.Utf8View.init(&branch_depth_data) catch unreachable).iterator();

            const lower_childset_pos = card.findTopLeft('\u{F000F}').?;
            const upper_childset_pos = card.findTopLeft('\u{F0010}').?;

            for (card.grid) |*row, y| {
                for (row.*) |*cell, x| {
                    cell.* = switch (cell.*) {
                        '\u{F0000}' => addr_iter.nextCodepoint().?,
                        '\u{F0001}' => hash_iter.nextCodepoint().?,
                        '\u{F0002}' => leaf_count_iter.nextCodepoint() orelse unreachable,
                        '\u{F0003}' => ref_count_iter.nextCodepoint() orelse unreachable,
                        '\u{F0005}' => head_infix_iter.nextCodepoint() orelse unreachable,
                        '\u{F0006}' => body_infix_iter.nextCodepoint() orelse unreachable,
                        '\u{F0007}' => branch_depth_iter.nextCodepoint() orelse unreachable,
                        '\u{F0008}' => if (bucket_count >= 1) '█' else '░',
                        '\u{F0009}' => if (bucket_count >= 2) '█' else '░',
                        '\u{F000A}' => if (bucket_count >= 4) '█' else '░',
                        '\u{F000B}' => if (bucket_count >= 8) '█' else '░',
                        '\u{F000C}' => if (bucket_count >= 16) '█' else '░',
                        '\u{F000D}' => if (bucket_count >= 32) '█' else '░',
                        '\u{F000E}' => if (bucket_count >= 64) '█' else '░',
                        '\u{F000F}' => blk: {
                            const lx: u8 = @intCast(u8, x) - lower_childset_pos.x;
                            const ly: u8 = @intCast(u8, y) - lower_childset_pos.y;
                            const byte_key: u8 = @as(u8, lx + (ly * 16));

                            if (!self.body.child_set.isSet(byte_key)) break :blk ' ';

                            var s: u21 = undefined;
                            const rand_hash_used = self.body.rand_hash_used.isSet(byte_key);

                            const bucket_index = hashByteKey(rand_hash_used, bucket_count, byte_key);
                            if (self.body.buckets[bucket_index].get(byte_key).tag != .none) {
                                s = if (rand_hash_used) '◆' else '●';
                            } else {
                                s = if (rand_hash_used) '◇' else '○';
                            }

                            break :blk s;
                        },
                        '\u{F0010}' => blk: {
                            const lx: u8 = @intCast(u8, x) - upper_childset_pos.x;
                            const ly: u8 = @intCast(u8, y) - upper_childset_pos.y;
                            const byte: u8 = @as(u8, 128 + lx + (ly * 16));
                            if (!self.body.child_set.isSet(byte)) break :blk ' ';
                            const s: u21 = if (self.body.rand_hash_used.isSet(byte)) '◆' else '●';
                            break :blk s;
                        },
                        else => cell.*,
                    };
                }
            }

            try writer.print("{s}\n", .{card});
            try writer.writeAll("");
        }

        pub fn init(start_depth: u8, branch_depth: u8, key: *const [key_length]u8, allocator: std.mem.Allocator) allocError!Head {
            const allocation = try allocator.allocAdvanced(u8, BODY_ALIGNMENT, @sizeOf(Body), .exact);
            const new_body = std.mem.bytesAsValue(Body, allocation[0..@sizeOf(Body)]);
            new_body.* = Body{ .ref_count = 1, .leaf_count = 1, .infix = undefined };
            new_body.infix = key.*;

            var new_head = Head{ .branch_depth = branch_depth, .body = new_body };

            const head_infix_length = @minimum(key.len - start_depth, new_head.infix.len);
            const key_end = start_depth + head_infix_length;
            mem.copy(u8, new_head.infix[0..head_infix_length], key[start_depth..key_end]);

            return new_head;
        }

        pub fn initBranch(start_depth: u8, branch_depth: u8, key: *const [key_length]u8, left: Node, right: Node, allocator: std.mem.Allocator) allocError!Node {
            const branch_node = try InnerNode(bucket_count).init(start_depth, branch_depth, key, allocator);

            _ = branch_node.cuckooPut(left); // We know that these can't fail.
            _ = branch_node.cuckooPut(right);

            branch_node.body.child_sum_hash = Hash.xor(left.hash(branch_depth, key), right.hash(branch_depth, key));
            branch_node.body.leaf_count = left.count() + right.count();

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
                var child_iterator = self.body.child_set;
                while (child_iterator.drainNext(true)) |child_byte_key| {
                    self.cuckooGet(@intCast(u8, child_byte_key)).rel(allocator);
                }
            }
        }

        pub fn count(self: Head) u64 {
            return self.body.leaf_count;
        }

        pub fn hash(self: Head, start_depth: u8, prefix: *const [key_length]u8) Hash {
            _ = start_depth;
            _ = prefix;
            return self.body.child_sum_hash;
        }

        pub fn depth(self: Head) u8 {
            return self.branch_depth;
        }

        pub fn peekFirst(self: Head) u8 {
            return self.infix[0];
        }

        pub fn peek(self: Head, start_depth: u8, at_depth: u8) ?u8 {
            if (at_depth < start_depth or self.branch_depth <= at_depth) return null;
            if (at_depth < start_depth + head_infix_len) return self.infix[at_depth - start_depth];
            return self.body.infix[at_depth];
        }

        pub fn propose(self: Head, start_depth: u8, at_depth: u8, result_set: *ByteBitset) void {
            if (at_depth == self.branch_depth) {
                result_set.setIntersect(result_set, &self.body.child_set);
                return;
            }

            if (self.peek(start_depth, at_depth)) |byte_key| {
                result_set.singleIntersect(byte_key);
                return;
            }

            result_set.unsetAll();
        }

        pub fn put(self: Head, start_depth: u8, key: *const [key_length]u8, value: ?T, parent_single_owner: bool, allocator: std.mem.Allocator) allocError!Node {
            const single_owner = parent_single_owner and self.body.ref_count == 1;

            var branch_depth = start_depth;
            while (branch_depth < self.branch_depth) : (branch_depth += 1) {
                if (key[branch_depth] != self.peek(start_depth, branch_depth).?) break;
            } else {
                // The entire compressed infix above this node matched with the key.
                const byte_key = key[branch_depth];
                if (self.cuckooHas(byte_key)) {
                    // The node already has a child branch with the same byte byte_key as the one in the key.
                    const old_child = self.cuckooGet(byte_key);
                    const old_child_hash = old_child.hash(self.branch_depth, key);
                    const old_child_count = old_child.count();
                    const new_child = try old_child.put(branch_depth, key, value, single_owner, allocator);
                    const new_child_hash = new_child.hash(branch_depth, key);
                    if (Hash.equal(old_child_hash, new_child_hash)) {
                        std.debug.print("Key already in tree. Hash is equal.\n{s}\n{s}\n", .{ std.fmt.fmtSliceHexUpper(&old_child_hash.data), std.fmt.fmtSliceHexUpper(&new_child_hash.data) });
                        return @bitCast(Node, self);
                    }
                    const new_hash = Hash.xor(Hash.xor(self.body.child_sum_hash, old_child_hash), new_child_hash);
                    const new_count = self.body.leaf_count - old_child_count + new_child.count();

                    var self_or_copy = self;
                    if (!single_owner) {
                        self_or_copy = try self.copy(allocator);
                        old_child.rel(allocator);
                    }
                    self_or_copy.body.child_sum_hash = new_hash;
                    self_or_copy.body.leaf_count = new_count;
                    self_or_copy.cuckooUpdate(new_child);
                    return @bitCast(Node, self_or_copy);
                } else {
                    const new_child_node = try InitLeafNode(branch_depth, key, value, allocator);
                    const new_hash = Hash.xor(self.body.child_sum_hash, new_child_node.hash(branch_depth, key));
                    const new_count = self.body.leaf_count + 1;

                    var self_or_copy = if (single_owner) self else try self.copy(allocator);

                    self_or_copy.body.child_sum_hash = new_hash;
                    self_or_copy.body.leaf_count = new_count;

                    var displaced = self_or_copy.cuckooPut(new_child_node);
                    if(displaced) |_| {
                        var buff = self_or_copy.body.buffer;

                        var grown = @bitCast(Node, self_or_copy);
                        while(displaced) |entry| {
                            grown = try grown.grow(allocator);
                            displaced = grown.cuckooPut(entry);
                        }
                        _ = buff;
                        return grown;
                    }

                    return @bitCast(Node, self_or_copy);
                }
            }

            var recycled_self = self;
            for (recycled_self.infix) |*byte, i| {
                byte.* = self.peek(start_depth, branch_depth + @intCast(u8, i)) orelse break;
            }

            const sibling_leaf_node = try InitLeafNode(branch_depth, key, value, allocator);

            return try InnerNode(1).initBranch(start_depth, branch_depth, key, sibling_leaf_node, @bitCast(Node, recycled_self), allocator);
        }

        pub fn get(self: Head, start_depth: u8, at_depth: u8, byte_key: u8) Node {
            if (at_depth == self.branch_depth and self.cuckooHas(byte_key)) {
                return self.cuckooGet(byte_key);
            }
            if (self.peek(start_depth, at_depth)) |own_key| {
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
                const child = new_head.cuckooGet(cast_child_byte_key);
                const potential_child_copy = try child.ref(allocator);
                if (potential_child_copy) |new_child| {
                    new_head.cuckooUpdate(new_child);
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

        fn cuckooPut(self: Head, node: Node) ?Node {
            var byte_key = node.peekFirst();
            self.body.child_set.set(byte_key);

            const growable = (bucket_count != max_bucket_count);
            const base_size = (bucket_count == 1);
            var use_rand_hash = false;
            var entry = node;
            var attempts: u8 = 0;
            while (true) {
                random = rand_lut[random ^ byte_key];
                const bucket_index = hashByteKey(use_rand_hash, bucket_count, byte_key);

                if (self.body.buckets[bucket_index].put(&self.body.rand_hash_used, bucket_count, bucket_index, entry)) {
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
                    byte_key = entry.peekFirst();
                    use_rand_hash = !self.body.rand_hash_used.isSet(byte_key);
                } else {
                    entry = self.body.buckets[bucket_index].displaceRandHashOnly(&self.body.rand_hash_used, entry);
                    self.body.rand_hash_used.setValue(byte_key, use_rand_hash);
                    byte_key = entry.peekFirst();
                }
            }
            unreachable;
        }

        fn cuckooHas(self: Head, byte_key: u8) bool {
            return self.body.child_set.isSet(byte_key);
        }

        // Contract: Key looked up must exist. Ensure with cuckooHas.
        fn cuckooGet(self: Head, byte_key: u8) Node {
            assert(self.body.child_set.isSet(byte_key));
            const bucket_index = hashByteKey(self.body.rand_hash_used.isSet(byte_key), bucket_count, byte_key);
            return self.body.buckets[bucket_index].get(byte_key);
        }

        fn cuckooUpdate(self: Head, node: Node) void {
            const byte_key = node.peekFirst();
            const bucket_index = hashByteKey(self.body.rand_hash_used.isSet(byte_key), bucket_count, byte_key);
            self.body.buckets[bucket_index].update(node);
        }
    };
}

fn InitLeafNode(start_depth: u8, key: *const [key_length]u8, opt_value: ?T, allocator: std.mem.Allocator) allocError!Node {
    const suffix_length = key_length - start_depth;
    if (opt_value) |value| {
        if (suffix_length <= 8) {
            return @bitCast(Node, try LeafNode(false, 8).init(start_depth, key, value, allocator));
        }
        if (suffix_length <= 16) {
            return @bitCast(Node, try LeafNode(false, 16).init(start_depth, key, value, allocator));
        }
        if (suffix_length <= 24) {
            return @bitCast(Node, try LeafNode(false, 24).init(start_depth, key, value, allocator));
        }
        if (suffix_length <= 32) {
            return @bitCast(Node, try LeafNode(false, 32).init(start_depth, key, value, allocator));
        }
        if (suffix_length <= 40) {
            return @bitCast(Node, try LeafNode(false, 40).init(start_depth, key, value, allocator));
        }
        if (suffix_length <= 48) {
            return @bitCast(Node, try LeafNode(false, 48).init(start_depth, key, value, allocator));
        }
        if (suffix_length <= 54) {
            return @bitCast(Node, try LeafNode(false, 56).init(start_depth, key, value, allocator));
        }
        if (suffix_length <= 64) {
            return @bitCast(Node, try LeafNode(false, 64).init(start_depth, key, value, allocator));
        }
    } else {
        if (suffix_length <= InlineLeafNode.head_suffix_len) {
            return @bitCast(Node, try InlineLeafNode.init(start_depth, key));
        }
        if (suffix_length <= 16) {
            return @bitCast(Node, try LeafNode(true, 16).init(start_depth, key, void{}, allocator));
        }
        if (suffix_length <= 24) {
            return @bitCast(Node, try LeafNode(true, 24).init(start_depth, key, void{}, allocator));
        }
        if (suffix_length <= 32) {
            return @bitCast(Node, try LeafNode(true, 32).init(start_depth, key, void{}, allocator));
        }
        if (suffix_length <= 40) {
            return @bitCast(Node, try LeafNode(true, 40).init(start_depth, key, void{}, allocator));
        }
        if (suffix_length <= 48) {
            return @bitCast(Node, try LeafNode(true, 48).init(start_depth, key, void{}, allocator));
        }
        if (suffix_length <= 54) {
            return @bitCast(Node, try LeafNode(true, 56).init(start_depth, key, void{}, allocator));
        }
        if (suffix_length <= 64) {
            return @bitCast(Node, try LeafNode(true, 64).init(start_depth, key, void{}, allocator));
        }
    }

    unreachable;
}

const InlineLeafNode = extern struct {
    const head_suffix_len = 15;

    tag: NodeTag = .twig15,
    /// The key stored in this entry.
    suffix: [head_suffix_len]u8 = [_]u8{0} ** head_suffix_len,

    const Head = @This();

    pub fn init(start_depth: u8, key: *const [key_length]u8) allocError!Head {
        var new_head = Head{};

        const head_suffix_length = @minimum(key.len - start_depth, new_head.suffix.len);
        const key_end = start_depth + head_suffix_length;
        mem.copy(u8, new_head.suffix[0..head_suffix_length], key[start_depth..key_end]);

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
        try writer.print("Inline suffix: {[1]s:_>[0]}\n", .{ head_suffix_len, std.fmt.fmtSliceHexUpper(&self.suffix) });
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

    pub fn hash(self: Head, start_depth: u8, prefix: *const [key_length]u8) Hash {
        var key = prefix.*;
        for (key[start_depth..key_length]) |*byte, i| {
            byte.* = self.peek(start_depth, start_depth + @intCast(u8, i)) orelse unreachable;
        }
        return keyHash(&key);
    }

    pub fn depth(self: Head) u8 {
        _ = self;
        return key_length;
    }

    pub fn peekFirst(self: Head) u8 {
        return self.suffix[0];
    }

    pub fn peek(self: Head, start_depth: u8, at_depth: u8) ?u8 {
        if (at_depth < start_depth or key_length <= at_depth) return null;
        return self.suffix[at_depth - start_depth];
    }

    pub fn propose(self: Head, start_depth: u8, at_depth: u8, result_set: *ByteBitset) void {
        if (self.peek(start_depth, at_depth)) |byte_key| {
            result_set.singleIntersect(byte_key);
            return;
        }

        result_set.unsetAll();
    }

    pub fn get(self: Head, start_depth: u8, at_depth: u8, key: u8) Node {
        if (self.peek(start_depth, at_depth)) |own_key| {
            if (own_key == key) return @bitCast(Node, self);
        }
        return Node.none;
    }

    pub fn put(self: Head, start_depth: u8, key: *const [key_length]u8, value: ?T, single_owner: bool, allocator: std.mem.Allocator) allocError!Node {
        _ = single_owner;

        var branch_depth = start_depth;
        while (branch_depth < key_length) : (branch_depth += 1) {
            if (key[branch_depth] != self.peek(start_depth, branch_depth).?) break;
        } else {
            return @bitCast(Node, self);
        }

        var recycled_self = self;

        for (recycled_self.suffix) |*byte, i| {
            byte.* = self.peek(start_depth, branch_depth + @intCast(u8, i)) orelse break;
        }

        const sibling_leaf_node = try InitLeafNode(branch_depth, key, value, allocator);

        return try InnerNode(1).initBranch(start_depth, branch_depth, key, sibling_leaf_node, @bitCast(Node, recycled_self), allocator);
    }
};

fn LeafNode(comptime no_value: bool, comptime suffix_len: u8) type {
    const head_suffix_len = 7;
    const body_suffix_len = suffix_len - head_suffix_len;
    return extern struct {
        tag: NodeTag = Node.leafNodeTag(no_value, suffix_len),
        /// The key stored in this entry.
        suffix: [head_suffix_len]u8 = [_]u8{0} ** head_suffix_len,
        /// The address of the pointer associated with the key.
        body: *Body,

        const Head = @This();
        const Body = if (no_value) extern struct {
            ref_count: u16 = 1,
            suffix: [body_suffix_len]u8 = undefined,
        } else extern struct {
            value: T,
            ref_count: u16 = 1,
            suffix: [body_suffix_len]u8 = undefined,
        };

        pub fn init(start_depth: u8, key: *const [key_length]u8, value: if (no_value) void else T, allocator: std.mem.Allocator) allocError!Head {
            const allocation = try allocator.allocAdvanced(u8, @alignOf(Body), @sizeOf(Body), .exact);
            const new_body = std.mem.bytesAsValue(Body, allocation[0..@sizeOf(Body)]);

            if (no_value) {
                new_body.* = Body{};
            } else {
                new_body.* = Body{ .value = value };
            }

            const key_start = (key.len - new_body.suffix.len);
            mem.copy(u8, new_body.suffix[0..new_body.suffix.len], key[key_start..key.len]);

            var new_head = Head{ .body = new_body };

            const head_suffix_length = @minimum(key.len - start_depth, new_head.suffix.len);
            const key_end = start_depth + head_suffix_length;
            mem.copy(u8, new_head.suffix[0..head_suffix_length], key[start_depth..key_end]);

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
            try writer.print("{*} ◁{d}:\n", .{ self.body, self.body.ref_count });
            if (no_value) {
                try writer.print("  value: null\n", .{});
            } else {
                try writer.print("  value: {}\n", .{self.body.value});
            }
            try writer.print("  suffixes: {[2]s:_>[0]} > {[3]s:_>[1]}\n", .{ head_suffix_len, body_suffix_len, std.fmt.fmtSliceHexUpper(&self.suffix), std.fmt.fmtSliceHexUpper(&self.body.suffix) });
        }

        pub fn ref(self: Head, allocator: std.mem.Allocator) allocError!?Node {
            if (self.body.ref_count == std.math.maxInt(@TypeOf(self.body.ref_count))) {
                // Reference counter exhausted, we need to make a copy of this node.
                const allocation = try allocator.allocAdvanced(u8, @alignOf(Body), @sizeOf(Body), .exact);
                const new_body = std.mem.bytesAsValue(Body, allocation[0..@sizeOf(Body)]);
                new_body.* = self.body.*;
                new_body.ref_count = 1;

                return @bitCast(Node, Head{ .suffix = self.suffix, .body = new_body });
            } else {
                self.body.ref_count += 1;
                return null;
            }
        }

        pub fn rel(self: Head, allocator: std.mem.Allocator) void {
            self.body.ref_count -= 1;
            if (self.body.ref_count == 0) {
                defer allocator.free(std.mem.asBytes(self.body));
            }
        }

        pub fn count(self: Head) u64 {
            _ = self;
            return 1;
        }

        pub fn hash(self: Head, start_depth: u8, prefix: *const [key_length]u8) Hash {
            var key = prefix.*;
            for (key[start_depth..key_length]) |*byte, i| {
                byte.* = self.peek(start_depth, start_depth + @intCast(u8, i)) orelse unreachable;
            }
            return keyHash(&key);
        }

        pub fn depth(self: Head) u8 {
            _ = self;
            return key_length;
        }

        pub fn peekFirst(self: Head) u8 {
            return self.suffix[0];
        }

        pub fn peek(self: Head, start_depth: u8, at_depth: u8) ?u8 {
            if (at_depth < start_depth or key_length <= at_depth) return null;
            if (at_depth < start_depth + head_suffix_len) {
                const head_suffix_depth = at_depth - start_depth;
                return self.suffix[head_suffix_depth];
            }
            const body_suffix_depth = at_depth - (key_length - body_suffix_len);
            return self.body.suffix[body_suffix_depth];
        }

        pub fn propose(self: Head, start_depth: u8, at_depth: u8, result_set: *ByteBitset) void {
            if (self.peek(start_depth, at_depth)) |byte_key| {
                result_set.singleIntersect(byte_key);
                return;
            }

            result_set.unsetAll();
        }

        pub fn get(self: Head, start_depth: u8, at_depth: u8, key: u8) Node {
            if (self.peek(start_depth, at_depth)) |own_key| {
                if (own_key == key) return @bitCast(Node, self);
            }
            return Node.none;
        }

        pub fn put(self: Head, start_depth: u8, key: *const [key_length]u8, value: ?T, single_owner: bool, allocator: std.mem.Allocator) allocError!Node {
            _ = single_owner;
            if (body_suffix_len == 0) unreachable;

            var branch_depth = start_depth;
            while (branch_depth < key_length) : (branch_depth += 1) {
                if (key[branch_depth] != self.peek(start_depth, branch_depth).?) break;
            } else {
                return @bitCast(Node, self);
            }

            var recycled_self = self;
            for (recycled_self.suffix) |*byte, i| {
                byte.* = self.peek(start_depth, branch_depth + @intCast(u8, i)) orelse break;
            }

            const sibling_leaf_node = try InitLeafNode(branch_depth, key, value, allocator);

            return try InnerNode(1).initBranch(start_depth, branch_depth, key, sibling_leaf_node, @bitCast(Node, recycled_self), allocator);
        }
    };
}

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
                self.key[branch_depth] = node.peek(start_depth, branch_depth) orelse break :infix;
            } else {
                var exhausted_depth = self.start_points.drainNext(false).?;
                while (self.start_points.findLastSet()) |parent_depth| {
                    var branches = &self.branch_state[exhausted_depth];
                    if (branches.drainNext(true)) |branch_key| {
                        self.start_points.set(exhausted_depth);
                        self.path[exhausted_depth] = self.path[parent_depth].get(parent_depth, exhausted_depth, branch_key);
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
            node.propose(start_depth, branch_depth, branches);

            const branch_key = branches.drainNext(true).?;
            self.path[branch_depth] = node.get(start_depth, branch_depth, branch_key);

            self.start_points.set(branch_depth);
            return IterationResult{ .start_depth = start_depth, .node = node, .key = self.key };
        }
    };

    pub fn nodes(self: *const Tree) NodeIterator {
        var iterator = NodeIterator{};
        if (self.child.unknown.tag != .none) {
            iterator.start_points.set(0);
            iterator.path[0] = self.child;
        }
        return iterator;
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

        var card = Card.from(
            \\┌────────────────────────────────────────────────────────────────────────────────┐
            \\│  Tree                                                                          │
            \\│ ━━━━━━                                                                         │
            \\│                                                                                │
            \\│         Count: 󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀      Memory (keys): 󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃           │
            \\│    Node Count: 󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁    Memory (actual): 󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄           │
            \\│   Alloc Count: 󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂           Overhead: 󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅           │
            \\│                                                                                │
            \\│   Node Distribution                                                            │
            \\│  ═══════════════════                                                           │
            \\│                                                                                │
            \\│      none 󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆   leaf8 󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎  twig15 󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖   │
            \\│    inner1 󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇  leaf16 󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏  twig16 󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗   │
            \\│    inner2 󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈  leaf24 󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐  twig24 󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘󰀘   │
            \\│    inner4 󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉  leaf32 󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑  twig32 󰀙󰀙󰀙󰀙󰀙󰀙󰀙󰀙󰀙󰀙󰀙󰀙󰀙󰀙󰀙󰀙   │
            \\│    inner8 󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊  leaf40 󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒  twig40 󰀚󰀚󰀚󰀚󰀚󰀚󰀚󰀚󰀚󰀚󰀚󰀚󰀚󰀚󰀚󰀚   │
            \\│   inner16 󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋  leaf48 󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓  twig48 󰀛󰀛󰀛󰀛󰀛󰀛󰀛󰀛󰀛󰀛󰀛󰀛󰀛󰀛󰀛󰀛   │
            \\│   inner32 󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌  leaf56 󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔  twig56 󰀜󰀜󰀜󰀜󰀜󰀜󰀜󰀜󰀜󰀜󰀜󰀜󰀜󰀜󰀜󰀜   │
            \\│   inner64 󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍  leaf64 󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕  twig64 󰀝󰀝󰀝󰀝󰀝󰀝󰀝󰀝󰀝󰀝󰀝󰀝󰀝󰀝󰀝󰀝   │
            \\│                                                                                │
            \\│   Density                                                                      │
            \\│  ═════════                                                                     │
            \\│        ┐󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞       │
            \\│        │󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞       │
            \\│        │󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞       │
            \\│        │󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞       │
            \\│        │󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞       │
            \\│        │󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞       │
            \\│        │󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞       │
            \\│        ┘󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞󰀞       │
            \\│        0┌──────────────┬───────────────┬───────────────┬───────────────┐63     │
            \\└────────────────────────────────────────────────────────────────────────────────┘
        ) catch unreachable;

        var none_count: u64 = 0;
        var inner_1_count: u64 = 0;
        var inner_2_count: u64 = 0;
        var inner_4_count: u64 = 0;
        var inner_8_count: u64 = 0;
        var inner_16_count: u64 = 0;
        var inner_32_count: u64 = 0;
        var inner_64_count: u64 = 0;
        var leaf_8_count: u64 = 0;
        var leaf_16_count: u64 = 0;
        var leaf_24_count: u64 = 0;
        var leaf_32_count: u64 = 0;
        var leaf_40_count: u64 = 0;
        var leaf_48_count: u64 = 0;
        var leaf_56_count: u64 = 0;
        var leaf_64_count: u64 = 0;
        var twig_15_count: u64 = 0;
        var twig_16_count: u64 = 0;
        var twig_24_count: u64 = 0;
        var twig_32_count: u64 = 0;
        var twig_40_count: u64 = 0;
        var twig_48_count: u64 = 0;
        var twig_56_count: u64 = 0;
        var twig_64_count: u64 = 0;

        var density_at_depth: [key_length]u64 = [_]u64{0} ** key_length;

        var node_iter = self.nodes();
        while (node_iter.next()) |res| {
            density_at_depth[res.start_depth] += 1;
            switch (res.node.unknown.tag) {
                .none => none_count += 1,
                .inner1 => inner_1_count += 1,
                .inner2 => inner_2_count += 1,
                .inner4 => inner_4_count += 1,
                .inner8 => inner_8_count += 1,
                .inner16 => inner_16_count += 1,
                .inner32 => inner_32_count += 1,
                .inner64 => inner_64_count += 1,
                .leaf8 => leaf_8_count += 1,
                .leaf16 => leaf_16_count += 1,
                .leaf24 => leaf_24_count += 1,
                .leaf32 => leaf_32_count += 1,
                .leaf40 => leaf_40_count += 1,
                .leaf48 => leaf_48_count += 1,
                .leaf56 => leaf_56_count += 1,
                .leaf64 => leaf_64_count += 1,
                .twig15 => twig_15_count += 1,
                .twig16 => twig_16_count += 1,
                .twig24 => twig_24_count += 1,
                .twig32 => twig_32_count += 1,
                .twig40 => twig_40_count += 1,
                .twig48 => twig_48_count += 1,
                .twig56 => twig_56_count += 1,
                .twig64 => twig_64_count += 1,
            }
        }

        const node_count: u64 = none_count + inner_1_count + inner_2_count + inner_4_count + inner_8_count + inner_16_count + inner_32_count + inner_64_count + leaf_8_count + leaf_16_count + leaf_24_count + leaf_32_count + leaf_40_count + leaf_48_count + leaf_56_count + leaf_64_count + twig_15_count + twig_16_count + twig_24_count + twig_32_count + twig_40_count + twig_48_count + twig_56_count + twig_64_count;

        const alloc_count: u64 = inner_1_count + inner_2_count + inner_4_count + inner_8_count + inner_16_count + inner_32_count + inner_64_count + leaf_8_count + leaf_16_count + leaf_24_count + leaf_32_count + leaf_40_count + leaf_48_count + leaf_56_count + leaf_64_count + twig_16_count + twig_24_count + twig_32_count + twig_40_count + twig_48_count + twig_56_count + twig_64_count;

        var mem_keys: u64 = self.count() * key_length;

        const mem_actual: u64 = inner_1_count * @sizeOf(InnerNode(1).Body) + inner_2_count * @sizeOf(InnerNode(2).Body) + inner_4_count * @sizeOf(InnerNode(4).Body) + inner_8_count * @sizeOf(InnerNode(8).Body) + inner_16_count * @sizeOf(InnerNode(16).Body) + inner_32_count * @sizeOf(InnerNode(32).Body) + inner_64_count * @sizeOf(InnerNode(64).Body) + leaf_8_count * @sizeOf(LeafNode(false, 8).Body) + leaf_16_count * @sizeOf(LeafNode(false, 16).Body) + leaf_24_count * @sizeOf(LeafNode(false, 24).Body) + leaf_32_count * @sizeOf(LeafNode(false, 32).Body) + leaf_40_count * @sizeOf(LeafNode(false, 40).Body) + leaf_48_count * @sizeOf(LeafNode(false, 48).Body) + leaf_56_count * @sizeOf(LeafNode(false, 56).Body) + leaf_64_count * @sizeOf(LeafNode(false, 64).Body) + twig_16_count * @sizeOf(LeafNode(true, 16).Body) + twig_24_count * @sizeOf(LeafNode(true, 24).Body) + twig_32_count * @sizeOf(LeafNode(true, 32).Body) + twig_40_count * @sizeOf(LeafNode(true, 40).Body) + twig_48_count * @sizeOf(LeafNode(true, 48).Body) + twig_56_count * @sizeOf(LeafNode(true, 56).Body) + twig_64_count * @sizeOf(LeafNode(true, 64).Body);

        const mem_overhead: f64 = @intToFloat(f64, mem_actual) - @intToFloat(f64, mem_keys);

        var max_density: u64 = 0;
        for (density_at_depth) |density| {
            max_density = std.math.max(max_density, density);
        }

        var count_data: [16:0]u8 = undefined;
        _ = std.fmt.bufPrint(&count_data, "{d:_>16}", .{self.count()}) catch unreachable;
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

        var inner_1_count_data: [16:0]u8 = undefined;
        _ = std.fmt.bufPrint(&inner_1_count_data, "{d:_>16}", .{inner_1_count}) catch unreachable;
        var inner_1_count_iter = (std.unicode.Utf8View.init(&inner_1_count_data) catch unreachable).iterator();

        var inner_2_count_data: [16:0]u8 = undefined;
        _ = std.fmt.bufPrint(&inner_2_count_data, "{d:_>16}", .{inner_2_count}) catch unreachable;
        var inner_2_count_iter = (std.unicode.Utf8View.init(&inner_2_count_data) catch unreachable).iterator();

        var inner_4_count_data: [16:0]u8 = undefined;
        _ = std.fmt.bufPrint(&inner_4_count_data, "{d:_>16}", .{inner_4_count}) catch unreachable;
        var inner_4_count_iter = (std.unicode.Utf8View.init(&inner_4_count_data) catch unreachable).iterator();

        var inner_8_count_data: [16:0]u8 = undefined;
        _ = std.fmt.bufPrint(&inner_8_count_data, "{d:_>16}", .{inner_8_count}) catch unreachable;
        var inner_8_count_iter = (std.unicode.Utf8View.init(&inner_8_count_data) catch unreachable).iterator();

        var inner_16_count_data: [16:0]u8 = undefined;
        _ = std.fmt.bufPrint(&inner_16_count_data, "{d:_>16}", .{inner_16_count}) catch unreachable;
        var inner_16_count_iter = (std.unicode.Utf8View.init(&inner_16_count_data) catch unreachable).iterator();

        var inner_32_count_data: [16:0]u8 = undefined;
        _ = std.fmt.bufPrint(&inner_32_count_data, "{d:_>16}", .{inner_32_count}) catch unreachable;
        var inner_32_count_iter = (std.unicode.Utf8View.init(&inner_32_count_data) catch unreachable).iterator();

        var inner_64_count_data: [16:0]u8 = undefined;
        _ = std.fmt.bufPrint(&inner_64_count_data, "{d:_>16}", .{inner_64_count}) catch unreachable;
        var inner_64_count_iter = (std.unicode.Utf8View.init(&inner_64_count_data) catch unreachable).iterator();

        var leaf_8_count_data: [16:0]u8 = undefined;
        _ = std.fmt.bufPrint(&leaf_8_count_data, "{d:_>16}", .{leaf_8_count}) catch unreachable;
        var leaf_8_count_iter = (std.unicode.Utf8View.init(&leaf_8_count_data) catch unreachable).iterator();

        var leaf_16_count_data: [16:0]u8 = undefined;
        _ = std.fmt.bufPrint(&leaf_16_count_data, "{d:_>16}", .{leaf_16_count}) catch unreachable;
        var leaf_16_count_iter = (std.unicode.Utf8View.init(&leaf_16_count_data) catch unreachable).iterator();

        var leaf_24_count_data: [16:0]u8 = undefined;
        _ = std.fmt.bufPrint(&leaf_24_count_data, "{d:_>16}", .{leaf_24_count}) catch unreachable;
        var leaf_24_count_iter = (std.unicode.Utf8View.init(&leaf_24_count_data) catch unreachable).iterator();

        var leaf_32_count_data: [16:0]u8 = undefined;
        _ = std.fmt.bufPrint(&leaf_32_count_data, "{d:_>16}", .{leaf_32_count}) catch unreachable;
        var leaf_32_count_iter = (std.unicode.Utf8View.init(&leaf_32_count_data) catch unreachable).iterator();

        var leaf_40_count_data: [16:0]u8 = undefined;
        _ = std.fmt.bufPrint(&leaf_40_count_data, "{d:_>16}", .{leaf_40_count}) catch unreachable;
        var leaf_40_count_iter = (std.unicode.Utf8View.init(&leaf_40_count_data) catch unreachable).iterator();

        var leaf_48_count_data: [16:0]u8 = undefined;
        _ = std.fmt.bufPrint(&leaf_48_count_data, "{d:_>16}", .{leaf_48_count}) catch unreachable;
        var leaf_48_count_iter = (std.unicode.Utf8View.init(&leaf_48_count_data) catch unreachable).iterator();

        var leaf_56_count_data: [16:0]u8 = undefined;
        _ = std.fmt.bufPrint(&leaf_56_count_data, "{d:_>16}", .{leaf_56_count}) catch unreachable;
        var leaf_56_count_iter = (std.unicode.Utf8View.init(&leaf_56_count_data) catch unreachable).iterator();

        var leaf_64_count_data: [16:0]u8 = undefined;
        _ = std.fmt.bufPrint(&leaf_64_count_data, "{d:_>16}", .{leaf_64_count}) catch unreachable;
        var leaf_64_count_iter = (std.unicode.Utf8View.init(&leaf_64_count_data) catch unreachable).iterator();

        var twig_15_count_data: [16:0]u8 = undefined;
        _ = std.fmt.bufPrint(&twig_15_count_data, "{d:_>16}", .{twig_15_count}) catch unreachable;
        var twig_15_count_iter = (std.unicode.Utf8View.init(&twig_15_count_data) catch unreachable).iterator();

        var twig_16_count_data: [16:0]u8 = undefined;
        _ = std.fmt.bufPrint(&twig_16_count_data, "{d:_>16}", .{twig_16_count}) catch unreachable;
        var twig_16_count_iter = (std.unicode.Utf8View.init(&twig_16_count_data) catch unreachable).iterator();

        var twig_24_count_data: [16:0]u8 = undefined;
        _ = std.fmt.bufPrint(&twig_24_count_data, "{d:_>16}", .{twig_24_count}) catch unreachable;
        var twig_24_count_iter = (std.unicode.Utf8View.init(&twig_24_count_data) catch unreachable).iterator();

        var twig_32_count_data: [16:0]u8 = undefined;
        _ = std.fmt.bufPrint(&twig_32_count_data, "{d:_>16}", .{twig_32_count}) catch unreachable;
        var twig_32_count_iter = (std.unicode.Utf8View.init(&twig_32_count_data) catch unreachable).iterator();

        var twig_40_count_data: [16:0]u8 = undefined;
        _ = std.fmt.bufPrint(&twig_40_count_data, "{d:_>16}", .{twig_40_count}) catch unreachable;
        var twig_40_count_iter = (std.unicode.Utf8View.init(&twig_40_count_data) catch unreachable).iterator();

        var twig_48_count_data: [16:0]u8 = undefined;
        _ = std.fmt.bufPrint(&twig_48_count_data, "{d:_>16}", .{twig_48_count}) catch unreachable;
        var twig_48_count_iter = (std.unicode.Utf8View.init(&twig_48_count_data) catch unreachable).iterator();

        var twig_56_count_data: [16:0]u8 = undefined;
        _ = std.fmt.bufPrint(&twig_56_count_data, "{d:_>16}", .{twig_56_count}) catch unreachable;
        var twig_56_count_iter = (std.unicode.Utf8View.init(&twig_56_count_data) catch unreachable).iterator();

        var twig_64_count_data: [16:0]u8 = undefined;
        _ = std.fmt.bufPrint(&twig_64_count_data, "{d:_>16}", .{twig_64_count}) catch unreachable;
        var twig_64_count_iter = (std.unicode.Utf8View.init(&twig_64_count_data) catch unreachable).iterator();

        const density_pos = card.findTopLeft('\u{F001E}').?;

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
                    '\u{F0007}' => inner_1_count_iter.nextCodepoint().?,
                    '\u{F0008}' => inner_2_count_iter.nextCodepoint().?,
                    '\u{F0009}' => inner_4_count_iter.nextCodepoint().?,
                    '\u{F000A}' => inner_8_count_iter.nextCodepoint().?,
                    '\u{F000B}' => inner_16_count_iter.nextCodepoint().?,
                    '\u{F000C}' => inner_32_count_iter.nextCodepoint().?,
                    '\u{F000D}' => inner_64_count_iter.nextCodepoint().?,
                    '\u{F000E}' => leaf_8_count_iter.nextCodepoint().?,
                    '\u{F000F}' => leaf_16_count_iter.nextCodepoint().?,
                    '\u{F0010}' => leaf_24_count_iter.nextCodepoint().?,
                    '\u{F0011}' => leaf_32_count_iter.nextCodepoint().?,
                    '\u{F0012}' => leaf_40_count_iter.nextCodepoint().?,
                    '\u{F0013}' => leaf_48_count_iter.nextCodepoint().?,
                    '\u{F0014}' => leaf_56_count_iter.nextCodepoint().?,
                    '\u{F0015}' => leaf_64_count_iter.nextCodepoint().?,
                    '\u{F0016}' => twig_15_count_iter.nextCodepoint().?,
                    '\u{F0017}' => twig_16_count_iter.nextCodepoint().?,
                    '\u{F0018}' => twig_24_count_iter.nextCodepoint().?,
                    '\u{F0019}' => twig_32_count_iter.nextCodepoint().?,
                    '\u{F001A}' => twig_40_count_iter.nextCodepoint().?,
                    '\u{F001B}' => twig_48_count_iter.nextCodepoint().?,
                    '\u{F001C}' => twig_56_count_iter.nextCodepoint().?,
                    '\u{F001D}' => twig_64_count_iter.nextCodepoint().?,
                    '\u{F001E}' => blk: {
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

        try writer.print("{s}\n", .{card});
        try writer.writeAll("");
    }

    pub fn count(self: *const Tree) u64 {
        return self.child.count();
    }

    pub fn put(self: *Tree, key: *const [key_length]u8, value: ?T) allocError!void {
        if (self.child.unknown.tag == .none) {
            self.child = try InitLeafNode(0, key, value, self.allocator);
        } else {
            self.child = try self.child.put(0, key, value, true, self.allocator);
        }
    }
    // get(key) {
    //   let node = this.child;
    //   if (node === null) return undefined;
    //   for (let depth = 0; depth < KEY_LENGTH; depth++) {
    //     const sought = key[depth];
    //     node = node.get(depth, sought);
    //     if (node === null) return undefined;
    //   }
    //   return node.value;
    // }

    // cursor() {
    //   return new PACTCursor(this);
    // }

    // isEmpty() {
    //   return this.child === null;
    // }

    // isEqual(other) {
    //   return (
    //     this.child === other.child ||
    //     (this.keyLength === other.keyLength &&
    //       !!this.child &&
    //       !!other.child &&
    //       hash_equal(this.child.hash, other.child.hash))
    //   );
    // }

    // isSubsetOf(other) {
    //   return (
    //     this.keyLength === other.keyLength &&
    //     (!this.child || (!!other.child && _isSubsetOf(this.child, other.child)))
    //   );
    // }

    // isIntersecting(other) {
    //   return (
    //     this.keyLength === other.keyLength &&
    //     !!this.child &&
    //     !!other.child &&
    //     (this.child === other.child ||
    //       hash_equal(this.child.hash, other.child.hash) ||
    //       _isIntersecting(this.child, other.child))
    //   );
    // }

    // union(other) {
    //   const thisNode = this.child;
    //   const otherNode = other.child;
    //   if (thisNode === null) {
    //     return new PACTTree(otherNode);
    //   }
    //   if (otherNode === null) {
    //     return new PACTTree(thisNode);
    //   }
    //   return new PACTTree(_union(thisNode, otherNode));
    // }

    // subtract(other) {
    //   const thisNode = this.child;
    //   const otherNode = other.child;
    //   if (otherNode === null) {
    //     return new PACTTree(thisNode);
    //   }
    //   if (
    //     this.child === null ||
    //     hash_equal(this.child.hash, other.child.hash)
    //   ) {
    //     return new PACTTree();
    //   } else {
    //     return new PACTTree(_subtract(thisNode, otherNode));
    //   }
    // }

    // intersect(other) {
    //   const thisNode = this.child;
    //   const otherNode = other.child;

    //   if (thisNode === null || otherNode === null) {
    //     return new PACTTree(null);
    //   }
    //   if (thisNode === otherNode || hash_equal(thisNode.hash, otherNode.hash)) {
    //     return new PACTTree(otherNode);
    //   }
    //   return new PACTTree(_intersect(thisNode, otherNode));
    // }

    // difference(other) {
    //   const thisNode = this.child;
    //   const otherNode = other.child;

    //   if (thisNode === null) {
    //     return new PACTTree(otherNode);
    //   }
    //   if (otherNode === null) {
    //     return new PACTTree(thisNode);
    //   }
    //   if (thisNode === otherNode || hash_equal(thisNode.hash, otherNode.hash)) {
    //     return new PACTTree(null);
    //   }
    //   return new PACTTree(_difference(thisNode, otherNode));
    // }

    // *entries() {
    //   if (this.child === null) return;
    //   for (const [k, v] of _walk(this.child)) {
    //     yield [k.slice(), v];
    //   }
    // }

    // *keys() {
    //   if (this.child === null) return;
    //   for (const [k, v] of _walk(this.child)) {
    //     yield k.slice();
    //   }
    // }

    // *values() {
    //   if (this.child === null) return;
    //   for (const [k, v] of _walk(this.child)) {
    //     yield v;
    //   }
    // }
};

test "Alignment & Size" {
    std.debug.print("{} Size: {}, Alignment: {}\n", .{ Node, @sizeOf(Node), @alignOf(Node) });
    std.debug.print("{} Size: {}, Alignment: {}\n", .{ InnerNode(1).Head, @sizeOf(InnerNode(1).Head), @alignOf(InnerNode(1).Head) });
    std.debug.print("{} Size: {}, Alignment: {}\n", .{ InnerNode(2).Head, @sizeOf(InnerNode(2).Head), @alignOf(InnerNode(2).Head) });
    std.debug.print("{} Size: {}, Alignment: {}\n", .{ InnerNode(4).Head, @sizeOf(InnerNode(4).Head), @alignOf(InnerNode(4).Head) });
    std.debug.print("{} Size: {}, Alignment: {}\n", .{ InnerNode(8).Head, @sizeOf(InnerNode(8).Head), @alignOf(InnerNode(8).Head) });
    std.debug.print("{} Size: {}, Alignment: {}\n", .{ InnerNode(16).Head, @sizeOf(InnerNode(16).Head), @alignOf(InnerNode(16).Head) });
    std.debug.print("{} Size: {}, Alignment: {}\n", .{ InnerNode(32).Head, @sizeOf(InnerNode(32).Head), @alignOf(InnerNode(32).Head) });
    std.debug.print("{} Size: {}, Alignment: {}\n", .{ InnerNode(64).Head, @sizeOf(InnerNode(64).Head), @alignOf(InnerNode(64).Head) });
    std.debug.print("{} Size: {}, Alignment: {}\n", .{ InnerNode(1).Body, @sizeOf(InnerNode(1).Body), @alignOf(InnerNode(1).Body) });
    std.debug.print("{} Size: {}, Alignment: {}\n", .{ InnerNode(2).Body, @sizeOf(InnerNode(2).Body), @alignOf(InnerNode(2).Body) });
    std.debug.print("{} Size: {}, Alignment: {}\n", .{ InnerNode(4).Body, @sizeOf(InnerNode(4).Body), @alignOf(InnerNode(4).Body) });
    std.debug.print("{} Size: {}, Alignment: {}\n", .{ InnerNode(8).Body, @sizeOf(InnerNode(8).Body), @alignOf(InnerNode(8).Body) });
    std.debug.print("{} Size: {}, Alignment: {}\n", .{ InnerNode(16).Body, @sizeOf(InnerNode(16).Body), @alignOf(InnerNode(16).Body) });
    std.debug.print("{} Size: {}, Alignment: {}\n", .{ InnerNode(32).Body, @sizeOf(InnerNode(32).Body), @alignOf(InnerNode(32).Body) });
    std.debug.print("{} Size: {}, Alignment: {}\n", .{ InnerNode(64).Body, @sizeOf(InnerNode(64).Body), @alignOf(InnerNode(64).Body) });
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
