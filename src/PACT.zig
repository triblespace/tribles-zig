const std = @import("std");
const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;
const ByteBitset = @import("ByteBitset.zig").ByteBitset;

const mem = std.mem;

// TODO: change hash set index to boolean or at least make set -> 1, unset -> 0

// Uninitialized memory initialized by init()
var instance_secret: [16]u8 = undefined;

pub fn init() void {
    // XXX: (crest) Should this be a deterministic pseudo-RNG seeded by a constant for reproducable tests?
    std.crypto.random.bytes(&instance_secret);
}

const Hash = extern struct {
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

fn keyHash(key: []const u8) Hash {
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
const MAX_ATTEMPTS = 8;

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

pub const MAX_KEY_LEN = 64;

/// Define a PACT datastructure with the given parameters.
pub fn makePACT(comptime key_length: u8, comptime T: type) type {
    assert(key_length <= MAX_KEY_LEN);

    return extern struct {
        const allocError = std.mem.Allocator.Error;

        const NodeTag = enum(u8) {
            none,
            inner1,
            inner2,
            inner4,
            inner8,
            inner16,
            inner32,
            leaf8,
            leaf16,
            leaf24,
            leaf32,
            leaf40,
            leaf48,
            leaf56,
            leaf64,
        };

        const Node = extern struct {
            head: Head,
            tag: NodeTag,

            const Head = extern union {
                none: void,
                inner1: InnerNode(1),
                inner2: InnerNode(2),
                inner4: InnerNode(4),
                inner8: InnerNode(8),
                inner16: InnerNode(16),
                inner32: InnerNode(32),
                leaf8: LeafNode(8),
                leaf16: LeafNode(16),
                leaf24: LeafNode(24),
                leaf32: LeafNode(32),
                leaf40: LeafNode(40),
                leaf48: LeafNode(48),
                leaf56: LeafNode(56),
                leaf64: LeafNode(64),
            };

            fn bucketCountToTag(comptime bucket_count: u8) NodeTag {
                return switch (bucket_count) {
                    1 => NodeTag.inner1,
                    2 => NodeTag.inner2,
                    4 => NodeTag.inner4,
                    8 => NodeTag.inner8,
                    16 => NodeTag.inner16,
                    32 => NodeTag.inner32,
                    else => @panic("Bad bucket count for tag."),
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

                switch (self.tag) {
                    .none => try writer.print("none", .{}),
                    .inner1 => try writer.print("{s}", .{self.head.inner1}),
                    .inner2 => try writer.print("{s}", .{self.head.inner2}),
                    .inner4 => try writer.print("{s}", .{self.head.inner4}),
                    .inner8 => try writer.print("{s}", .{self.head.inner8}),
                    .inner16 => try writer.print("{s}", .{self.head.inner16}),
                    .inner32 => try writer.print("{s}", .{self.head.inner32}),
                    .leaf8 => try writer.print("{s}", .{self.head.leaf8}),
                    .leaf16 => try writer.print("{s}", .{self.head.leaf16}),
                    .leaf24 => try writer.print("{s}", .{self.head.leaf24}),
                    .leaf32 => try writer.print("{s}", .{self.head.leaf32}),
                    .leaf40 => try writer.print("{s}", .{self.head.leaf40}),
                    .leaf48 => try writer.print("{s}", .{self.head.leaf48}),
                    .leaf56 => try writer.print("{s}", .{self.head.leaf56}),
                    .leaf64 => try writer.print("{s}", .{self.head.leaf64}),
                }
                try writer.writeAll("");
            }

            pub fn from(comptime variantType: type, variant: anytype) Node {
                return switch (variantType) {
                    InnerNode(1) => Node{ .tag = .inner1, .head = .{ .inner1 = variant } },
                    InnerNode(2) => Node{ .tag = .inner2, .head = .{ .inner2 = variant } },
                    InnerNode(4) => Node{ .tag = .inner4, .head = .{ .inner4 = variant } },
                    InnerNode(8) => Node{ .tag = .inner8, .head = .{ .inner8 = variant } },
                    InnerNode(16) => Node{ .tag = .inner16, .head = .{ .inner16 = variant } },
                    InnerNode(32) => Node{ .tag = .inner32, .head = .{ .inner32 = variant } },
                    LeafNode(8) => Node{ .tag = .leaf8, .head = .{ .leaf8 = variant } },
                    LeafNode(16) => Node{ .tag = .leaf16, .head = .{ .leaf16 = variant } },
                    LeafNode(24) => Node{ .tag = .leaf24, .head = .{ .leaf24 = variant } },
                    LeafNode(32) => Node{ .tag = .leaf32, .head = .{ .leaf32 = variant } },
                    LeafNode(40) => Node{ .tag = .leaf40, .head = .{ .leaf40 = variant } },
                    LeafNode(48) => Node{ .tag = .leaf48, .head = .{ .leaf48 = variant } },
                    LeafNode(56) => Node{ .tag = .leaf56, .head = .{ .leaf56 = variant } },
                    LeafNode(64) => Node{ .tag = .leaf64, .head = .{ .leaf64 = variant } },
                    else => @panic("Can't create node from provided type."),
                };
            }

            pub fn ref(self: Node, allocator: std.mem.Allocator) allocError!?Node {
                return switch (self.tag) {
                    .none => Node{ .tag = .none, .head = .{ .none = void{} } },
                    .inner1 => self.head.inner1.ref(allocator),
                    .inner2 => self.head.inner2.ref(allocator),
                    .inner4 => self.head.inner4.ref(allocator),
                    .inner8 => self.head.inner8.ref(allocator),
                    .inner16 => self.head.inner16.ref(allocator),
                    .inner32 => self.head.inner32.ref(allocator),
                    .leaf8 => self.head.leaf8.ref(allocator),
                    .leaf16 => self.head.leaf16.ref(allocator),
                    .leaf24 => self.head.leaf24.ref(allocator),
                    .leaf32 => self.head.leaf32.ref(allocator),
                    .leaf40 => self.head.leaf40.ref(allocator),
                    .leaf48 => self.head.leaf48.ref(allocator),
                    .leaf56 => self.head.leaf56.ref(allocator),
                    .leaf64 => self.head.leaf64.ref(allocator),
                };
            }

            pub fn rel(self: Node, allocator: std.mem.Allocator) void {
                switch (self.tag) {
                    .none => {},
                    .inner1 => self.head.inner1.rel(allocator),
                    .inner2 => self.head.inner2.rel(allocator),
                    .inner4 => self.head.inner4.rel(allocator),
                    .inner8 => self.head.inner8.rel(allocator),
                    .inner16 => self.head.inner16.rel(allocator),
                    .inner32 => self.head.inner32.rel(allocator),
                    .leaf8 => self.head.leaf8.rel(allocator),
                    .leaf16 => self.head.leaf16.rel(allocator),
                    .leaf24 => self.head.leaf24.rel(allocator),
                    .leaf32 => self.head.leaf32.rel(allocator),
                    .leaf40 => self.head.leaf40.rel(allocator),
                    .leaf48 => self.head.leaf48.rel(allocator),
                    .leaf56 => self.head.leaf56.rel(allocator),
                    .leaf64 => self.head.leaf64.rel(allocator),
                }
            }

            pub fn count(self: Node) u64 {
                return switch (self.tag) {
                    .none => 0,
                    .inner1 => self.head.inner1.count(),
                    .inner2 => self.head.inner2.count(),
                    .inner4 => self.head.inner4.count(),
                    .inner8 => self.head.inner8.count(),
                    .inner16 => self.head.inner16.count(),
                    .inner32 => self.head.inner32.count(),
                    .leaf8 => self.head.leaf8.count(),
                    .leaf16 => self.head.leaf16.count(),
                    .leaf24 => self.head.leaf24.count(),
                    .leaf32 => self.head.leaf32.count(),
                    .leaf40 => self.head.leaf40.count(),
                    .leaf48 => self.head.leaf48.count(),
                    .leaf56 => self.head.leaf56.count(),
                    .leaf64 => self.head.leaf64.count(),
                };
            }

            pub fn hash(self: Node) Hash {
                return switch (self.tag) {
                    .none => @panic("Called `hash` on none."),
                    .inner1 => self.head.inner1.hash(),
                    .inner2 => self.head.inner2.hash(),
                    .inner4 => self.head.inner4.hash(),
                    .inner8 => self.head.inner8.hash(),
                    .inner16 => self.head.inner16.hash(),
                    .inner32 => self.head.inner32.hash(),
                    .leaf8 => self.head.leaf8.hash(),
                    .leaf16 => self.head.leaf16.hash(),
                    .leaf24 => self.head.leaf24.hash(),
                    .leaf32 => self.head.leaf32.hash(),
                    .leaf40 => self.head.leaf40.hash(),
                    .leaf48 => self.head.leaf48.hash(),
                    .leaf56 => self.head.leaf56.hash(),
                    .leaf64 => self.head.leaf64.hash(),
                };
            }

            pub fn depth(self: Node) u8 {
                return switch (self.tag) {
                    .none => @panic("Called `depth` on none."),
                    .inner1 => self.head.inner1.depth(),
                    .inner2 => self.head.inner2.depth(),
                    .inner4 => self.head.inner4.depth(),
                    .inner8 => self.head.inner8.depth(),
                    .inner16 => self.head.inner16.depth(),
                    .inner32 => self.head.inner32.depth(),
                    .leaf8 => self.head.leaf8.depth(),
                    .leaf16 => self.head.leaf16.depth(),
                    .leaf24 => self.head.leaf24.depth(),
                    .leaf32 => self.head.leaf32.depth(),
                    .leaf40 => self.head.leaf40.depth(),
                    .leaf48 => self.head.leaf48.depth(),
                    .leaf56 => self.head.leaf56.depth(),
                    .leaf64 => self.head.leaf64.depth(),
                };
            }

            pub fn peekFirst(self: Node) u8 {
                return switch (self.tag) {
                    .none => @panic("Called `peek` on none."),
                    .inner1 => self.head.inner1.peekFirst(),
                    .inner2 => self.head.inner2.peekFirst(),
                    .inner4 => self.head.inner4.peekFirst(),
                    .inner8 => self.head.inner8.peekFirst(),
                    .inner16 => self.head.inner16.peekFirst(),
                    .inner32 => self.head.inner32.peekFirst(),
                    .leaf8 => self.head.leaf8.peekFirst(),
                    .leaf16 => self.head.leaf16.peekFirst(),
                    .leaf24 => self.head.leaf24.peekFirst(),
                    .leaf32 => self.head.leaf32.peekFirst(),
                    .leaf40 => self.head.leaf40.peekFirst(),
                    .leaf48 => self.head.leaf48.peekFirst(),
                    .leaf56 => self.head.leaf56.peekFirst(),
                    .leaf64 => self.head.leaf64.peekFirst(),
                };
            }

            pub fn peek(self: Node, start_depth: u8, at_depth: u8) ?u8 {
                return switch (self.tag) {
                    .none => @panic("Called `peek` on none."),
                    .inner1 => self.head.inner1.peek(start_depth, at_depth),
                    .inner2 => self.head.inner2.peek(start_depth, at_depth),
                    .inner4 => self.head.inner4.peek(start_depth, at_depth),
                    .inner8 => self.head.inner8.peek(start_depth, at_depth),
                    .inner16 => self.head.inner16.peek(start_depth, at_depth),
                    .inner32 => self.head.inner32.peek(start_depth, at_depth),
                    .leaf8 => self.head.leaf8.peek(start_depth, at_depth),
                    .leaf16 => self.head.leaf16.peek(start_depth, at_depth),
                    .leaf24 => self.head.leaf24.peek(start_depth, at_depth),
                    .leaf32 => self.head.leaf32.peek(start_depth, at_depth),
                    .leaf40 => self.head.leaf40.peek(start_depth, at_depth),
                    .leaf48 => self.head.leaf48.peek(start_depth, at_depth),
                    .leaf56 => self.head.leaf56.peek(start_depth, at_depth),
                    .leaf64 => self.head.leaf64.peek(start_depth, at_depth),
                };
            }

            pub fn propose(self: Node, start_depth: u8, at_depth: u8, result_set: *ByteBitset) void {
                return switch (self.tag) {
                    .none => @panic("Called `propose` on none."),
                    .inner1 => self.head.inner1.propose(start_depth, at_depth, result_set),
                    .inner2 => self.head.inner2.propose(start_depth, at_depth, result_set),
                    .inner4 => self.head.inner4.propose(start_depth, at_depth, result_set),
                    .inner8 => self.head.inner8.propose(start_depth, at_depth, result_set),
                    .inner16 => self.head.inner16.propose(start_depth, at_depth, result_set),
                    .inner32 => self.head.inner32.propose(start_depth, at_depth, result_set),
                    .leaf8 => self.head.leaf8.propose(start_depth, at_depth, result_set),
                    .leaf16 => self.head.leaf16.propose(start_depth, at_depth, result_set),
                    .leaf24 => self.head.leaf24.propose(start_depth, at_depth, result_set),
                    .leaf32 => self.head.leaf32.propose(start_depth, at_depth, result_set),
                    .leaf40 => self.head.leaf40.propose(start_depth, at_depth, result_set),
                    .leaf48 => self.head.leaf48.propose(start_depth, at_depth, result_set),
                    .leaf56 => self.head.leaf56.propose(start_depth, at_depth, result_set),
                    .leaf64 => self.head.leaf64.propose(start_depth, at_depth, result_set),
                };
            }

            pub fn get(self: Node, start_depth: u8, at_depth: u8, byte_key: u8) ?Node {
                return switch (self.tag) {
                    .none => @panic("Called `get` on none."),
                    .inner1 => self.head.inner1.get(start_depth, at_depth, byte_key),
                    .inner2 => self.head.inner2.get(start_depth, at_depth, byte_key),
                    .inner4 => self.head.inner4.get(start_depth, at_depth, byte_key),
                    .inner8 => self.head.inner8.get(start_depth, at_depth, byte_key),
                    .inner16 => self.head.inner16.get(start_depth, at_depth, byte_key),
                    .inner32 => self.head.inner32.get(start_depth, at_depth, byte_key),
                    .leaf8 => self.head.leaf8.get(start_depth, at_depth, byte_key),
                    .leaf16 => self.head.leaf16.get(start_depth, at_depth, byte_key),
                    .leaf24 => self.head.leaf24.get(start_depth, at_depth, byte_key),
                    .leaf32 => self.head.leaf32.get(start_depth, at_depth, byte_key),
                    .leaf40 => self.head.leaf40.get(start_depth, at_depth, byte_key),
                    .leaf48 => self.head.leaf48.get(start_depth, at_depth, byte_key),
                    .leaf56 => self.head.leaf56.get(start_depth, at_depth, byte_key),
                    .leaf64 => self.head.leaf64.get(start_depth, at_depth, byte_key),
                };
            }

            pub fn put(self: Node, start_depth: u8, key: *const [key_length]u8, value: T, single_owner: bool, allocator: std.mem.Allocator) allocError!Node {
                return switch (self.tag) {
                    .none => @panic("Called `put` on none."),
                    .inner1 => self.head.inner1.put(start_depth, key, value, single_owner, allocator),
                    .inner2 => self.head.inner2.put(start_depth, key, value, single_owner, allocator),
                    .inner4 => self.head.inner4.put(start_depth, key, value, single_owner, allocator),
                    .inner8 => self.head.inner8.put(start_depth, key, value, single_owner, allocator),
                    .inner16 => self.head.inner16.put(start_depth, key, value, single_owner, allocator),
                    .inner32 => self.head.inner32.put(start_depth, key, value, single_owner, allocator),
                    .leaf8 => self.head.leaf8.put(start_depth, key, value, single_owner, allocator),
                    .leaf16 => self.head.leaf16.put(start_depth, key, value, single_owner, allocator),
                    .leaf24 => self.head.leaf24.put(start_depth, key, value, single_owner, allocator),
                    .leaf32 => self.head.leaf32.put(start_depth, key, value, single_owner, allocator),
                    .leaf40 => self.head.leaf40.put(start_depth, key, value, single_owner, allocator),
                    .leaf48 => self.head.leaf48.put(start_depth, key, value, single_owner, allocator),
                    .leaf56 => self.head.leaf56.put(start_depth, key, value, single_owner, allocator),
                    .leaf64 => self.head.leaf64.put(start_depth, key, value, single_owner, allocator),
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

        fn InnerNode(comptime bucket_count: u8) type {
            const head_infix_len = 6;
            const body_infix_len = 30;
            return extern struct {
                /// The address of the pointer associated with the key.
                body: *Body,
                /// The branch depth of the body.
                branch_depth: u8,
                /// The infix stored in this head.
                infix: [head_infix_len]u8 = [_]u8{0} ** head_infix_len,

                const Head = @This();

                const GrownHead = if (bucket_count == max_bucket_count) Head else InnerNode(bucket_count << 1);

                const BODY_ALIGNMENT = 64;

                const Body = extern struct {
                    leaf_count: u64,
                    segment_count: u64,
                    child_sum_hash: Hash = .{ .data = [_]u8{0} ** 16 },
                    ref_count: u16 = 1,
                    infix: [body_infix_len]u8,
                    child_set: ByteBitset = ByteBitset.initEmpty(),
                    rand_hash_used: ByteBitset = ByteBitset.initEmpty(),
                    buckets: [bucket_count]Bucket = if (bucket_count == 1) [_]Bucket{Bucket{}} else undefined,

                    const Bucket = extern struct {
                        const SLOT_COUNT = 4;

                        slots: [SLOT_COUNT]Node = [_]Node{Node{ .tag = .none, .head = .{ .none = void{} } }} ** SLOT_COUNT,

                        pub fn format(
                            self: Bucket,
                            comptime fmt: []const u8,
                            options: std.fmt.FormatOptions,
                            writer: anytype,
                        ) !void {
                            _ = fmt;
                            _ = options;

                            for (self.slots) |slot, i| {
                                switch (slot.tag) {
                                    .none => try writer.print("|_", .{}),
                                    else => try writer.print("| {d}: {d:3}", .{ i, slot.peekFirst() }),
                                }
                            }

                            try writer.writeAll("|");
                        }
                        /// Retrieve the value stored, value must exist.
                        pub fn get(self: *const Bucket, byte_key: u8) Node {
                            for (self.slots) |slot| {
                                if (slot.tag != .none and slot.peekFirst() == byte_key) {
                                    return slot;
                                }
                            }
                            std.debug.print("Constraint violation, byte key {d} not found in: {s}\n", .{ byte_key, self });
                            unreachable;
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
                                if (slot.tag != .none and slot.peekFirst() == entry.peekFirst()) {
                                    slot.* = entry;
                                    return true;
                                }
                            }
                            for (self.slots) |*slot| {
                                if (slot.tag == .none) {
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
                                if (slot.tag != .none and slot.peekFirst() == entry.peekFirst()) {
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

                    try writer.print("{*} ◁{d}:\n", .{ self.body, self.body.ref_count });
                    try writer.print("  depth: {d} | count: {d} | segment_count: {d}\n", .{ self.branch_depth, self.body.leaf_count, self.body.segment_count });
                    try writer.print("  hash: {s}\n", .{self.body.child_sum_hash});
                    try writer.print("  infixes: {any} > {any}\n", .{ self.infix, self.body.infix });
                    try writer.print("  child_set: {s}\n", .{self.body.child_set});
                    try writer.print("  rand_hash_used: {s}\n", .{self.body.rand_hash_used});
                    try writer.print("  children: ", .{});

                    var child_iterator = self.body.child_set;
                    while (child_iterator.drainNext(true)) |child_byte_key| {
                        const cast_child_byte_key = @intCast(u8, child_byte_key);
                        const use_rand_hash = self.body.rand_hash_used.isSet(cast_child_byte_key);
                        const bucket_index = hashByteKey(use_rand_hash, bucket_count, cast_child_byte_key);
                        const hash_name = @as([]const u8, if (use_rand_hash) "rnd" else "seq");
                        try writer.print("|{d}:{s}@{d}", .{ cast_child_byte_key, hash_name, bucket_index });
                    }
                    try writer.print("|\n", .{});
                    try writer.print("  buckets:!\n", .{});

                    for (self.body.buckets) |bucket, i| {
                        try writer.print("    {d}: {s}\n", .{ i, bucket });
                    }
                }

                pub fn init(start_depth: u8, branch_depth: u8, key: *const [key_length]u8, allocator: std.mem.Allocator) allocError!Head {
                    const allocation = try allocator.allocAdvanced(u8, BODY_ALIGNMENT, @sizeOf(Body), .exact);
                    const new_body = std.mem.bytesAsValue(Body, allocation[0..@sizeOf(Body)]);
                    new_body.* = Body{ .ref_count = 1, .leaf_count = 1, .segment_count = 1, .infix = undefined };

                    const body_infix_length = @minimum(branch_depth, new_body.infix.len);
                    const body_infix_start = new_body.infix.len - body_infix_length;
                    const key_start = branch_depth - body_infix_length;
                    mem.copy(u8, new_body.infix[body_infix_start..new_body.infix.len], key[key_start..branch_depth]);

                    var new_head = Head{ .branch_depth = branch_depth, .body = new_body };

                    const head_infix_length = @minimum(key.len - start_depth, new_head.infix.len);
                    const key_end = start_depth + head_infix_length;
                    mem.copy(u8, new_head.infix[0..head_infix_length], key[start_depth..key_end]);

                    return new_head;
                }

                /// TODO: document this!
                pub fn ref(self: Head, allocator: std.mem.Allocator) allocError!?Node {
                    if (self.body.ref_count == std.math.maxInt(@TypeOf(self.body.ref_count))) {
                        // Reference counter exhausted, we need to make a copy of this node.
                        return Node.from(Head, try self.copy(allocator));
                    } else {
                        self.body.ref_count += 1;
                        return null;
                    }
                }

                pub fn rel(self: Head, allocator: std.mem.Allocator) void {
                    self.body.ref_count -= 1;
                    if (self.body.ref_count == 0) {
                        defer allocator.free(std.mem.asBytes(self.body));
                        while (self.body.child_set.drainNext(true)) |child_byte_key| {
                            self.cuckooGet(@intCast(u8, child_byte_key)).rel(allocator);
                        }
                    }
                }

                pub fn count(self: Head) u64 {
                    return self.body.leaf_count;
                }

                pub fn hash(self: Head) Hash {
                    return self.body.child_sum_hash;
                }

                pub fn depth(self: Head) u8 {
                    return self.branch_depth;
                }

                pub fn peekFirst(self: Head) u8 {
                    return self.infix[0];
                }

                pub fn peek(self: Head, start_depth: u8, at_depth: u8) ?u8 {
                    if (self.branch_depth <= at_depth or at_depth < start_depth) return null;
                    if (at_depth < start_depth + head_infix_len) return self.infix[at_depth - start_depth];
                    return self.body.infix[(start_depth + @as(u8, self.body.infix.len)) - self.branch_depth];
                }

                pub fn put(self: Head, start_depth: u8, key: *const [key_length]u8, value: T, parent_single_owner: bool, allocator: std.mem.Allocator) allocError!Node {
                    const single_owner = parent_single_owner and self.body.ref_count == 1;

                    var branch_depth = start_depth;
                    var infix_index: u8 = (start_depth + @as(u8, self.body.infix.len)) - self.branch_depth;
                    while (branch_depth < self.branch_depth) : ({
                        branch_depth += 1;
                        infix_index += 1;
                    }) {
                        const infix = self.body.infix;
                        if (key[branch_depth] != infix[infix_index]) break;
                    } else {
                        // The entire compressed infix above this node matched with the key.
                        const byte_key = key[branch_depth];
                        if (self.cuckooHas(byte_key)) {
                            // The node already has a child branch with the same byte byte_key as the one in the key.
                            const old_child = self.cuckooGet(byte_key);
                            const old_child_hash = old_child.hash();
                            const old_child_count = old_child.count();
                            const old_child_segment_count = 1; // TODO old_child.segmentCount(branch_depth);
                            const new_child = try old_child.put(branch_depth, key, value, single_owner, allocator);
                            if (Hash.equal(old_child_hash, new_child.hash())) {
                                return Node.from(Head, self);
                            }
                            const new_hash = Hash.xor(Hash.xor(self.hash(), old_child_hash), new_child.hash());
                            const new_count = self.body.leaf_count - old_child_count + new_child.count();
                            const new_segment_count =
                                self.body.segment_count -
                                old_child_segment_count +
                                1; // TODO new_child.segmentCount(branch_depth);

                            var self_or_copy = self;
                            if (!single_owner) {
                                self_or_copy = try self.copy(allocator);
                                old_child.rel(allocator);
                            }
                            self_or_copy.body.child_sum_hash = new_hash;
                            self_or_copy.body.leaf_count = new_count;
                            self_or_copy.body.segment_count = new_segment_count;
                            self_or_copy.cuckooUpdate(new_child);
                            return Node.from(Head, self_or_copy);
                        } else {
                            const new_child_node = try InitLeafNode(branch_depth, key, value, keyHash(key), allocator);
                            const new_hash = Hash.xor(self.hash(), new_child_node.hash());
                            const new_count = self.body.leaf_count + 1;
                            const new_segment_count = self.body.segment_count + 1;

                            var self_or_copy = if (single_owner) self else try self.copy(allocator);

                            self_or_copy.body.child_sum_hash = new_hash;
                            self_or_copy.body.leaf_count = new_count;
                            self_or_copy.body.segment_count = new_segment_count;

                            return try self_or_copy.cuckooPut(new_child_node, allocator);
                        }
                    }

                    const new_branch_node_above = try InnerNode(1).init(start_depth, branch_depth, key, allocator);
                    const new_sibling_leaf_node = try InitLeafNode(branch_depth, key, value, keyHash(key), allocator);

                    var recycled_self = self;
                    recycled_self.branch_depth = branch_depth;

                    for (recycled_self.infix) |*byte, i| {
                        byte.* = self.peek(start_depth, branch_depth + @intCast(u8, i)) orelse break;
                    }

                    _ = try new_branch_node_above.cuckooPut(Node.from(Head, recycled_self), allocator); // We know that these can't fail and won't reallocate.
                    _ = try new_branch_node_above.cuckooPut(new_sibling_leaf_node, allocator);

                    new_branch_node_above.body.child_sum_hash = Hash.xor(recycled_self.hash(), new_sibling_leaf_node.hash());
                    new_branch_node_above.body.leaf_count = recycled_self.body.leaf_count + 1;
                    new_branch_node_above.body.segment_count = 3;
                    // We need to check if this insered moved our branchDepth across a segment boundary.
                    // const segmentCount =
                    //     SEGMENT_LUT[depth] === SEGMENT_LUT[this.branchDepth]
                    //     ? this._segmentCount + 1
                    //     : 2;

                    return Node.from(InnerNode(1), new_branch_node_above);
                }

                pub fn get(self: Head, at_depth: u8, byte_key: u8) ?Node {
                    if (at_depth < self.branch_depth) {
                        const index: u8 = (at_depth + @as(u8, self.infix.len)) - self.branch_depth;
                        const infix_key = self.infix[index];
                        if (infix_key == byte_key) {
                            return Node.from(Head, self);
                        }
                    } else {
                        if (self.cuckooHas(byte_key)) {
                            return self.cuckooGet(byte_key);
                        }
                    }
                    return null;
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

                fn grow(self: Head, allocator: std.mem.Allocator) allocError!GrownHead {
                    if (bucket_count == max_bucket_count) {
                        return self;
                    } else {
                        std.debug.print("Grow:{*}\n {} -> {} : {} -> {} \n", .{ self.body, Head, GrownHead, @sizeOf(Body), @sizeOf(GrownHead.Body) });
                        const allocation: []align(@alignOf(GrownHead.Body)) u8 = try allocator.reallocAdvanced(std.mem.span(std.mem.asBytes(self.body)), @alignOf(GrownHead.Body), @sizeOf(GrownHead.Body), .exact);
                        const new_body = std.mem.bytesAsValue(GrownHead.Body, allocation[0..@sizeOf(GrownHead.Body)]);
                        std.debug.print("Growed:{*}\n", .{new_body});
                        new_body.buckets[new_body.buckets.len / 2 .. new_body.buckets.len].* = new_body.buckets[0 .. new_body.buckets.len / 2].*;
                        return GrownHead{ .branch_depth = self.branch_depth, .infix = self.infix, .body = new_body };
                    }
                }

                fn cuckooPut(self: Head, node: Node, allocator: std.mem.Allocator) allocError!Node {
                    var byte_key = node.peekFirst();
                    if (self.body.child_set.isSet(byte_key)) {
                        const index = hashByteKey(self.body.rand_hash_used.isSet(byte_key), bucket_count, byte_key);
                        self.body.buckets[index].update(node);
                        return Node.from(Head, self);
                    } else {
                        const growable = (bucket_count != max_bucket_count);
                        const base_size = (bucket_count == 1);
                        var use_rand_hash = false;
                        var entry = node;
                        var attempts: u8 = 0;
                        while (true) {
                            random = rand_lut[random ^ byte_key];
                            const bucket_index = hashByteKey(use_rand_hash, bucket_count, byte_key);

                            self.body.child_set.set(byte_key);
                            self.body.rand_hash_used.setValue(byte_key, use_rand_hash);

                            if (self.body.buckets[bucket_index].put(&self.body.rand_hash_used, bucket_count, bucket_index, entry)) {
                                return Node.from(Head, self);
                            }

                            if (base_size or attempts == MAX_ATTEMPTS) {
                                const grown = try self.grow(allocator);
                                return grown.cuckooPut(entry, allocator);
                            }

                            if (growable) {
                                attempts += 1;
                                entry = self.body.buckets[bucket_index].displaceRandomly(random, entry);
                                byte_key = entry.peekFirst();
                                use_rand_hash = !self.body.rand_hash_used.isSet(byte_key);
                            } else {
                                entry = self.body.buckets[bucket_index].displaceRandHashOnly(&self.body.rand_hash_used, entry);
                                use_rand_hash = false;
                            }
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
                    return self.body.buckets[bucket_index].update(node);
                }
            };
        }

        fn InitLeafNode(start_depth: u8, key: *const [key_length]u8, value: T, key_hash: Hash, allocator: std.mem.Allocator) allocError!Node {
            const suffix_lenth = key_length - start_depth;
            if (suffix_lenth <= 8) {
                return Node.from(LeafNode(8), try LeafNode(8).init(start_depth, key, value, key_hash, allocator));
            }
            if (suffix_lenth <= 16) {
                return Node.from(LeafNode(16), try LeafNode(16).init(start_depth, key, value, key_hash, allocator));
            }
            if (suffix_lenth <= 24) {
                return Node.from(LeafNode(24), try LeafNode(24).init(start_depth, key, value, key_hash, allocator));
            }
            if (suffix_lenth <= 32) {
                return Node.from(LeafNode(32), try LeafNode(32).init(start_depth, key, value, key_hash, allocator));
            }
            if (suffix_lenth <= 40) {
                return Node.from(LeafNode(40), try LeafNode(40).init(start_depth, key, value, key_hash, allocator));
            }
            if (suffix_lenth <= 48) {
                return Node.from(LeafNode(48), try LeafNode(48).init(start_depth, key, value, key_hash, allocator));
            }
            if (suffix_lenth <= 54) {
                return Node.from(LeafNode(56), try LeafNode(56).init(start_depth, key, value, key_hash, allocator));
            }
            if (suffix_lenth <= 64) {
                return Node.from(LeafNode(64), try LeafNode(64).init(start_depth, key, value, key_hash, allocator));
            }
            unreachable;
        }

        fn LeafNode(comptime suffix_len: u8) type {
            const head_suffix_len = 7;
            const body_suffix_len = suffix_len - head_suffix_len;
            return extern struct {
                /// The address of the pointer associated with the key.
                body: *Body,
                /// The key stored in this entry.
                suffix: [head_suffix_len]u8 = [_]u8{0} ** head_suffix_len,

                const Head = @This();
                const Body = extern struct {
                    key_hash: Hash,
                    value: T,
                    ref_count: u16 = 1,
                    suffix: [body_suffix_len]u8 = undefined,
                };

                pub fn init(start_depth: u8, key: *const [key_length]u8, value: T, key_hash: Hash, allocator: std.mem.Allocator) allocError!Head {
                    const allocation = try allocator.allocAdvanced(u8, @alignOf(Body), @sizeOf(Body), .exact);
                    const new_body = std.mem.bytesAsValue(Body, allocation[0..@sizeOf(Body)]);
                    new_body.* = Body{ .key_hash = key_hash, .value = value };

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
                    try writer.print("  value: {}\n", .{self.body.value});
                    try writer.print("  suffixes: {any} > {any}\n", .{ self.suffix, self.body.suffix });
                }

                pub fn ref(self: Head, allocator: std.mem.Allocator) allocError!?Node {
                    if (self.body.ref_count == std.math.maxInt(@TypeOf(self.body.ref_count))) {
                        // Reference counter exhausted, we need to make a copy of this node.
                        const allocation = try allocator.allocAdvanced(u8, @alignOf(Body), @sizeOf(Body), .exact);
                        const new_body = std.mem.bytesAsValue(Body, allocation[0..@sizeOf(Body)]);
                        new_body.* = self.body.*;
                        new_body.ref_count = 1;

                        return Node.from(Head, Head{ .suffix = self.suffix, .body = new_body });
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

                pub fn hash(self: Head) Hash {
                    return self.body.key_hash;
                }

                pub fn depth(self: Head) u8 {
                    _ = self;
                    return key_length;
                }

                pub fn peekFirst(self: Head) u8 {
                    return self.suffix[0];
                }

                pub fn peek(self: Head, start_depth: u8, at_depth: u8) ?u8 {
                    if (key_length <= at_depth or at_depth < start_depth) return null;
                    if (at_depth < start_depth + head_suffix_len) return self.suffix[at_depth - start_depth];
                    return self.body.suffix[at_depth - (key_length - body_suffix_len)];
                }

                pub fn propose(self: Head, start_depth: u8, at_depth: u8, result_set: *ByteBitset) void {
                    var set = ByteBitset.initEmpty();
                    const own_key = self.peek(start_depth, at_depth).?;
                    set.set(own_key);
                    result_set.setIntersection(set);
                }

                pub fn get(self: Head, start_depth: u8, at_depth: u8, key: u8) Node {
                    const own_key = self.peek(start_depth, at_depth).?;
                    if (own_key == key) return Node.from(Head, self);
                    return Node.none;
                }

                pub fn put(self: Head, start_depth: u8, key: *const [key_length]u8, value: T, single_owner: bool, allocator: std.mem.Allocator) allocError!Node {
                    _ = single_owner;
                    if (body_suffix_len == 0) unreachable;

                    var branch_depth = start_depth;
                    while (branch_depth < key_length) : (branch_depth += 1) {
                        if (key[branch_depth] != self.peek(start_depth, branch_depth).?) break;
                    } else {
                        return Node.from(Head, self);
                    }

                    var recycled_self = self;

                    const new_branch_node_above = try InnerNode(1).init(start_depth, branch_depth, key, allocator);
                    const sibling_leaf_node = try InitLeafNode(branch_depth, key, value, keyHash(key), allocator);

                    for (recycled_self.suffix) |*byte, i| {
                        byte.* = self.peek(start_depth, branch_depth + @intCast(u8, i)) orelse break;
                    }

                    _ = try new_branch_node_above.cuckooPut(Node.from(Head, recycled_self), allocator); // We know that these can't fail and won't reallocate.
                    _ = try new_branch_node_above.cuckooPut(sibling_leaf_node, allocator);
                    new_branch_node_above.body.child_sum_hash = Hash.xor(recycled_self.hash(), sibling_leaf_node.hash());
                    new_branch_node_above.body.leaf_count = 2;
                    new_branch_node_above.body.segment_count = 2;

                    return Node.from(InnerNode(1), new_branch_node_above);
                }
            };
        }

        const Tree = struct {
            child: Node = Node{ .tag = .none, .head = .{ .none = void{} } },
            allocator: std.mem.Allocator,

            pub fn init(allocator: std.mem.Allocator) Tree {
                return Tree{ .allocator = allocator };
            }

            pub fn deinit(self: *Tree) void {
                self.child.rel(self.allocator);
            }

            pub fn fork(self: *Tree) allocError!Tree {
                return Tree{ .child = (try self.child.ref(self.allocator)) orelse self.child, .allocator = self.allocator };
            }

            pub fn count(self: *Tree) u64 {
                return self.child.count();
            }

            pub fn put(self: *Tree, key: *const [key_length]u8, value: T) allocError!void {
                if (self.child.tag == .none) {
                    self.child = try InitLeafNode(0, key, value, keyHash(key), self.allocator);
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
    };
}

test "Alignment" {
    const key_length = 64;
    const PACT = makePACT(key_length, usize);

    std.debug.print("Alignment: {} {}\n", .{ PACT.InnerNode(1), @alignOf(PACT.InnerNode(1).Body) });
    std.debug.print("Alignment: {} {}\n", .{ PACT.InnerNode(2), @alignOf(PACT.InnerNode(2).Body) });
    std.debug.print("Alignment: {} {}\n", .{ PACT.InnerNode(4), @alignOf(PACT.InnerNode(4).Body) });
    std.debug.print("Alignment: {} {}\n", .{ PACT.InnerNode(8), @alignOf(PACT.InnerNode(8).Body) });
    std.debug.print("Alignment: {} {}\n", .{ PACT.InnerNode(16), @alignOf(PACT.InnerNode(16).Body) });
    std.debug.print("Alignment: {} {}\n", .{ PACT.InnerNode(32), @alignOf(PACT.InnerNode(32).Body) });
    std.debug.print("Alignment: {} {}\n", .{ PACT.InnerNode(64), @alignOf(PACT.InnerNode(64).Body) });
}

test "create tree" {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{ .verbose_log = true, .retain_metadata = true, .safety = true }){};
    defer _ = general_purpose_allocator.deinit();
    const gpa = general_purpose_allocator.allocator();

    const key_length = 64;
    const PACT = makePACT(key_length, usize);
    var tree = PACT.Tree.init(gpa);
    defer tree.deinit();
}

test "empty tree has count 0" {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{ .verbose_log = true, .retain_metadata = true, .safety = true }){};
    defer _ = general_purpose_allocator.deinit();
    const gpa = general_purpose_allocator.allocator();

    const key_length = 64;
    const PACT = makePACT(key_length, usize);
    var tree = PACT.Tree.init(gpa);
    defer tree.deinit();

    try expectEqual(tree.count(), 0);
}

test "single item tree has count 1" {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{ .verbose_log = true, .retain_metadata = true, .safety = true }){};
    defer _ = general_purpose_allocator.deinit();
    const gpa = general_purpose_allocator.allocator();

    const key_length = 64;
    const PACT = makePACT(key_length, usize);
    var tree = PACT.Tree.init(gpa);
    defer tree.deinit();

    const key: [key_length]u8 = [_]u8{0} ** key_length;
    try tree.put(&key, 42);

    try expectEqual(tree.count(), 1);
}

test "immutable tree fork" {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{ .verbose_log = true }){};
    defer _ = general_purpose_allocator.deinit();
    const gpa = general_purpose_allocator.allocator();

    const key_length = 64;
    const PACT = makePACT(key_length, usize);
    var tree = PACT.Tree.init(gpa);
    defer tree.deinit();

    var new_tree = try tree.fork();
    defer new_tree.deinit();

    const key: [key_length]u8 = [_]u8{0} ** key_length;
    try new_tree.put(&key, 42);

    try expectEqual(tree.count(), 0);
    try expectEqual(new_tree.count(), 1);
}

test "multi item tree has correct count" {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{ .verbose_log = true, .retain_metadata = true, .safety = true }){};
    defer _ = general_purpose_allocator.deinit();
    const gpa = general_purpose_allocator.allocator();

    const total_runs = 10;

    var rnd = std.rand.DefaultPrng.init(0).random();

    const key_length = 64;
    const PACT = makePACT(key_length, usize);
    var tree = PACT.Tree.init(gpa);
    defer tree.deinit();

    var key: [key_length]u8 = undefined;

    var i: u64 = 0;
    while (i < total_runs) : (i += 1) {
        try expectEqual(tree.count(), i);

        rnd.bytes(&key);
        try tree.put(&key, rnd.int(usize));
        std.debug.print("Inserted {d} of {d}:{any}\n{s}\n", .{ i + 1, total_runs, key, tree.child });
    }
    try expectEqual(tree.count(), total_runs);
}

const time = std.time;

//                        | <------ 48 bit -------->|
// kernel space: | 1....1 | significant | alignment |
// user space:   | 0....1 | significant | alignemnt |

// 8:tag = 0 | 56:suffix
// 8:tag = 1 | 8:infix | 48:leaf ptr
// 8:tag = 2 | 8:infix | 48:inner ptr

test "benchmark" {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{ .verbose_log = true }){};
    defer _ = general_purpose_allocator.deinit();
    const gpa = general_purpose_allocator.allocator();

    const total_runs: usize = 10000000;

    var timer = try time.Timer.start();
    var t_total: u64 = 0;

    var rnd = std.rand.DefaultPrng.init(0).random();

    const key_length = 64;
    const PACT = makePACT(key_length, usize);
    var tree = PACT.Tree.init(gpa);
    defer tree.deinit();

    var key: [key_length]u8 = undefined;

    var i: u64 = 0;
    while (i < total_runs) : (i += 1) {
        rnd.bytes(&key);
        const value = rnd.int(usize);

        timer.reset();

        try tree.put(&key, value);

        t_total += timer.lap();
    }

    std.debug.print("Inserted {d} in {d}ns\n", .{ total_runs, t_total });
}

test "benchmark std" {
    const total_runs: usize = 100000;

    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = general_purpose_allocator.allocator();

    var timer = try time.Timer.start();
    var t_total: u64 = 0;

    var rnd = std.rand.DefaultPrng.init(0).random();

    const key_length = 64;

    var key: [key_length]u8 = undefined;

    var map = std.hash_map.AutoHashMap([key_length]u8, usize).init(gpa);
    defer map.deinit();

    var i: u64 = 0;
    while (i < total_runs) : (i += 1) {
        rnd.bytes(&key);
        const value = rnd.int(usize);

        timer.reset();

        try map.put(key, value);

        t_total += timer.lap();
    }

    std.debug.print("Inserted {d} in {d}ns\n", .{ total_runs, t_total });
}
