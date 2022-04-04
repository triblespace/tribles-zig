const std = @import("std");
const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;
const ByteBitset = @import("ByteBitset.zig").ByteBitset;
const Card = @import("Card.zig").Card;
const Trible = @import("Trible.zig").Trible;

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

        const Node = extern struct {
            head: Head,
            tag: NodeTag,

            const none = Node{ .tag = .none, .head = .{ .none = void{} } };

            const Head = extern union {
                none: void,
                branch1: BranchNode(1),
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
            };

            fn bucketCountToTag(comptime bucket_count: u8) NodeTag {
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
                    .branch1 => try writer.print("{s}", .{self.head.branch1}),
                    .branch2 => try writer.print("{s}", .{self.head.branch2}),
                    .branch4 => try writer.print("{s}", .{self.head.branch4}),
                    .branch8 => try writer.print("{s}", .{self.head.branch8}),
                    .branch16 => try writer.print("{s}", .{self.head.branch16}),
                    .branch32 => try writer.print("{s}", .{self.head.branch32}),
                    .branch64 => try writer.print("{s}", .{self.head.branch64}),
                    .infix8 => try writer.print("{s}", .{self.head.infix8}),
                    .infix16 => try writer.print("{s}", .{self.head.infix16}),
                    .infix24 => try writer.print("{s}", .{self.head.infix24}),
                    .infix32 => try writer.print("{s}", .{self.head.infix32}),
                    .infix40 => try writer.print("{s}", .{self.head.infix40}),
                    .infix48 => try writer.print("{s}", .{self.head.infix48}),
                    .infix56 => try writer.print("{s}", .{self.head.infix56}),
                    .infix64 => try writer.print("{s}", .{self.head.infix64}),
                    .leaf => try writer.print("{s}", .{self.head.leaf}),
                    .twig => try writer.print("{s}", .{self.head.twig}),
                }
                try writer.writeAll("");
            }

            pub fn from(comptime variantType: type, variant: anytype) Node {
                return switch (variantType) {
                    BranchNode(1) => Node{ .tag = .branch1, .head = .{ .branch1 = variant } },
                    BranchNode(2) => Node{ .tag = .branch2, .head = .{ .branch2 = variant } },
                    BranchNode(4) => Node{ .tag = .branch4, .head = .{ .branch4 = variant } },
                    BranchNode(8) => Node{ .tag = .branch8, .head = .{ .branch8 = variant } },
                    BranchNode(16) => Node{ .tag = .branch16, .head = .{ .branch16 = variant } },
                    BranchNode(32) => Node{ .tag = .branch32, .head = .{ .branch32 = variant } },
                    BranchNode(64) => Node{ .tag = .branch64, .head = .{ .branch64 = variant } },
                    InfixNode(8) => Node{ .tag = .infix8, .head = .{ .infix8 = variant } },
                    InfixNode(16) => Node{ .tag = .infix16, .head = .{ .infix16 = variant } },
                    InfixNode(24) => Node{ .tag = .infix24, .head = .{ .infix24 = variant } },
                    InfixNode(32) => Node{ .tag = .infix32, .head = .{ .infix32 = variant } },
                    InfixNode(40) => Node{ .tag = .infix40, .head = .{ .infix40 = variant } },
                    InfixNode(48) => Node{ .tag = .infix48, .head = .{ .infix48 = variant } },
                    InfixNode(56) => Node{ .tag = .infix56, .head = .{ .infix56 = variant } },
                    InfixNode(64) => Node{ .tag = .infix64, .head = .{ .infix64 = variant } },
                    LeafNode => Node{ .tag = .leaf, .head = .{ .leaf = variant } },
                    TwigNode => Node{ .tag = .twig, .head = .{ .twig = variant } },
                    else => std.debug.panic("Can't create node from provided type: {}", .{variantType}),
                };
            }

            pub fn ref(self: Node, allocator: std.mem.Allocator) allocError!?Node {
                return switch (self.tag) {
                    .none => Node{ .tag = .none, .head = .{ .none = void{} } },
                    .branch1 => self.head.branch1.ref(allocator),
                    .branch2 => self.head.branch2.ref(allocator),
                    .branch4 => self.head.branch4.ref(allocator),
                    .branch8 => self.head.branch8.ref(allocator),
                    .branch16 => self.head.branch16.ref(allocator),
                    .branch32 => self.head.branch32.ref(allocator),
                    .branch64 => self.head.branch64.ref(allocator),
                    .infix8 => self.head.infix8.ref(allocator),
                    .infix16 => self.head.infix16.ref(allocator),
                    .infix24 => self.head.infix24.ref(allocator),
                    .infix32 => self.head.infix32.ref(allocator),
                    .infix40 => self.head.infix40.ref(allocator),
                    .infix48 => self.head.infix48.ref(allocator),
                    .infix56 => self.head.infix56.ref(allocator),
                    .infix64 => self.head.infix64.ref(allocator),
                    .leaf => self.head.leaf.ref(allocator),
                    .twig => self.head.twig.ref(allocator),
                };
            }

            pub fn rel(self: Node, allocator: std.mem.Allocator) void {
                switch (self.tag) {
                    .none => {},
                    .branch1 => self.head.branch1.rel(allocator),
                    .branch2 => self.head.branch2.rel(allocator),
                    .branch4 => self.head.branch4.rel(allocator),
                    .branch8 => self.head.branch8.rel(allocator),
                    .branch16 => self.head.branch16.rel(allocator),
                    .branch32 => self.head.branch32.rel(allocator),
                    .branch64 => self.head.branch64.rel(allocator),
                    .infix8 => self.head.infix8.rel(allocator),
                    .infix16 => self.head.infix16.rel(allocator),
                    .infix24 => self.head.infix24.rel(allocator),
                    .infix32 => self.head.infix32.rel(allocator),
                    .infix40 => self.head.infix40.rel(allocator),
                    .infix48 => self.head.infix48.rel(allocator),
                    .infix56 => self.head.infix56.rel(allocator),
                    .infix64 => self.head.infix64.rel(allocator),
                    .leaf => self.head.leaf.rel(allocator),
                    .twig => self.head.twig.rel(allocator),
                }
            }

            pub fn count(self: Node) u64 {
                return switch (self.tag) {
                    .none => 0,
                    .branch1 => self.head.branch1.count(),
                    .branch2 => self.head.branch2.count(),
                    .branch4 => self.head.branch4.count(),
                    .branch8 => self.head.branch8.count(),
                    .branch16 => self.head.branch16.count(),
                    .branch32 => self.head.branch32.count(),
                    .branch64 => self.head.branch64.count(),
                    .infix8 => self.head.infix8.count(),
                    .infix16 => self.head.infix16.count(),
                    .infix24 => self.head.infix24.count(),
                    .infix32 => self.head.infix32.count(),
                    .infix40 => self.head.infix40.count(),
                    .infix48 => self.head.infix48.count(),
                    .infix56 => self.head.infix56.count(),
                    .infix64 => self.head.infix64.count(),
                    .leaf => self.head.leaf.count(),
                    .twig => self.head.twig.count(),
                };
            }

            pub fn hash(self: Node, start_depth: u8, prefix: *const [key_length]u8) Hash {
                std.debug.print("{s} {d} {any}\n", .{self.tag, start_depth, prefix});
                return switch (self.tag) {
                    .none => @panic("Called `hash` on none."),
                    .branch1 => self.head.branch1.hash(start_depth, prefix),
                    .branch2 => self.head.branch2.hash(start_depth, prefix),
                    .branch4 => self.head.branch4.hash(start_depth, prefix),
                    .branch8 => self.head.branch8.hash(start_depth, prefix),
                    .branch16 => self.head.branch16.hash(start_depth, prefix),
                    .branch32 => self.head.branch32.hash(start_depth, prefix),
                    .branch64 => self.head.branch64.hash(start_depth, prefix),
                    .infix8 => self.head.infix8.hash(start_depth, prefix),
                    .infix16 => self.head.infix16.hash(start_depth, prefix),
                    .infix24 => self.head.infix24.hash(start_depth, prefix),
                    .infix32 => self.head.infix32.hash(start_depth, prefix),
                    .infix40 => self.head.infix40.hash(start_depth, prefix),
                    .infix48 => self.head.infix48.hash(start_depth, prefix),
                    .infix56 => self.head.infix56.hash(start_depth, prefix),
                    .infix64 => self.head.infix64.hash(start_depth, prefix),
                    .leaf => self.head.leaf.hash(start_depth, prefix),
                    .twig => self.head.twig.hash(start_depth, prefix),
                };
            }

            pub fn depth(self: Node) u8 {
                return switch (self.tag) {
                    .none => @panic("Called `depth` on none."),
                    .branch1 => self.head.branch1.depth(),
                    .branch2 => self.head.branch2.depth(),
                    .branch4 => self.head.branch4.depth(),
                    .branch8 => self.head.branch8.depth(),
                    .branch16 => self.head.branch16.depth(),
                    .branch32 => self.head.branch32.depth(),
                    .branch64 => self.head.branch64.depth(),
                    .infix8 => self.head.infix8.depth(),
                    .infix16 => self.head.infix16.depth(),
                    .infix24 => self.head.infix24.depth(),
                    .infix32 => self.head.infix32.depth(),
                    .infix40 => self.head.infix40.depth(),
                    .infix48 => self.head.infix48.depth(),
                    .infix56 => self.head.infix56.depth(),
                    .infix64 => self.head.infix64.depth(),
                    .leaf => self.head.leaf.depth(),
                    .twig => self.head.twig.depth(),
                };
            }

            pub fn range(self: Node) u8 {
                return switch (self.tag) {
                    .none => @panic("Called `depth` on none."),
                    .branch1 => self.head.branch1.range(),
                    .branch2 => self.head.branch2.range(),
                    .branch4 => self.head.branch4.range(),
                    .branch8 => self.head.branch8.range(),
                    .branch16 => self.head.branch16.range(),
                    .branch32 => self.head.branch32.range(),
                    .branch64 => self.head.branch64.range(),
                    .infix8 => self.head.infix8.range(),
                    .infix16 => self.head.infix16.range(),
                    .infix24 => self.head.infix24.range(),
                    .infix32 => self.head.infix32.range(),
                    .infix40 => self.head.infix40.range(),
                    .infix48 => self.head.infix48.range(),
                    .infix56 => self.head.infix56.range(),
                    .infix64 => self.head.infix64.range(),
                    .leaf => self.head.leaf.range(),
                    .twig => self.head.twig.range(),
                };
            }

            pub fn peekFirst(self: Node) u8 {
                return switch (self.tag) {
                    .none => @panic("Called `peek` on none."),
                    .branch1 => self.head.branch1.peekFirst(),
                    .branch2 => self.head.branch2.peekFirst(),
                    .branch4 => self.head.branch4.peekFirst(),
                    .branch8 => self.head.branch8.peekFirst(),
                    .branch16 => self.head.branch16.peekFirst(),
                    .branch32 => self.head.branch32.peekFirst(),
                    .branch64 => self.head.branch64.peekFirst(),
                    .infix8 => self.head.infix8.peekFirst(),
                    .infix16 => self.head.infix16.peekFirst(),
                    .infix24 => self.head.infix24.peekFirst(),
                    .infix32 => self.head.infix32.peekFirst(),
                    .infix40 => self.head.infix40.peekFirst(),
                    .infix48 => self.head.infix48.peekFirst(),
                    .infix56 => self.head.infix56.peekFirst(),
                    .infix64 => self.head.infix64.peekFirst(),
                    .leaf => self.head.leaf.peekFirst(),
                    .twig => self.head.twig.peekFirst(),
                };
            }

            pub fn peek(self: Node, start_depth: u8, at_depth: u8) ?u8 {
                return switch (self.tag) {
                    .none => @panic("Called `peek` on none."),
                    .branch1 => self.head.branch1.peek(start_depth, at_depth),
                    .branch2 => self.head.branch2.peek(start_depth, at_depth),
                    .branch4 => self.head.branch4.peek(start_depth, at_depth),
                    .branch8 => self.head.branch8.peek(start_depth, at_depth),
                    .branch16 => self.head.branch16.peek(start_depth, at_depth),
                    .branch32 => self.head.branch32.peek(start_depth, at_depth),
                    .branch64 => self.head.branch64.peek(start_depth, at_depth),
                    .infix8 => self.head.infix8.peek(start_depth, at_depth),
                    .infix16 => self.head.infix16.peek(start_depth, at_depth),
                    .infix24 => self.head.infix24.peek(start_depth, at_depth),
                    .infix32 => self.head.infix32.peek(start_depth, at_depth),
                    .infix40 => self.head.infix40.peek(start_depth, at_depth),
                    .infix48 => self.head.infix48.peek(start_depth, at_depth),
                    .infix56 => self.head.infix56.peek(start_depth, at_depth),
                    .infix64 => self.head.infix64.peek(start_depth, at_depth),
                    .leaf => self.head.leaf.peek(start_depth, at_depth),
                    .twig => self.head.twig.peek(start_depth, at_depth),
                };
            }

            pub fn propose(self: Node, start_depth: u8, at_depth: u8, result_set: *ByteBitset) void {
                return switch (self.tag) {
                    .none => @panic("Called `propose` on none."),
                    .branch1 => self.head.branch1.propose(start_depth, at_depth, result_set),
                    .branch2 => self.head.branch2.propose(start_depth, at_depth, result_set),
                    .branch4 => self.head.branch4.propose(start_depth, at_depth, result_set),
                    .branch8 => self.head.branch8.propose(start_depth, at_depth, result_set),
                    .branch16 => self.head.branch16.propose(start_depth, at_depth, result_set),
                    .branch32 => self.head.branch32.propose(start_depth, at_depth, result_set),
                    .branch64 => self.head.branch64.propose(start_depth, at_depth, result_set),
                    .infix8 => self.head.infix8.propose(start_depth, at_depth, result_set),
                    .infix16 => self.head.infix16.propose(start_depth, at_depth, result_set),
                    .infix24 => self.head.infix24.propose(start_depth, at_depth, result_set),
                    .infix32 => self.head.infix32.propose(start_depth, at_depth, result_set),
                    .infix40 => self.head.infix40.propose(start_depth, at_depth, result_set),
                    .infix48 => self.head.infix48.propose(start_depth, at_depth, result_set),
                    .infix56 => self.head.infix56.propose(start_depth, at_depth, result_set),
                    .infix64 => self.head.infix64.propose(start_depth, at_depth, result_set),
                    .leaf => self.head.leaf.propose(start_depth, at_depth, result_set),
                    .twig => self.head.twig.propose(start_depth, at_depth, result_set),
                };
            }

            pub fn get(self: Node, start_depth: u8, at_depth: u8, byte_key: u8) Node {
                return switch (self.tag) {
                    .none => @panic("Called `get` on none."),
                    .branch1 => self.head.branch1.get(start_depth, at_depth, byte_key),
                    .branch2 => self.head.branch2.get(start_depth, at_depth, byte_key),
                    .branch4 => self.head.branch4.get(start_depth, at_depth, byte_key),
                    .branch8 => self.head.branch8.get(start_depth, at_depth, byte_key),
                    .branch16 => self.head.branch16.get(start_depth, at_depth, byte_key),
                    .branch32 => self.head.branch32.get(start_depth, at_depth, byte_key),
                    .branch64 => self.head.branch64.get(start_depth, at_depth, byte_key),
                    .infix8 => self.head.infix8.get(start_depth, at_depth, byte_key),
                    .infix16 => self.head.infix16.get(start_depth, at_depth, byte_key),
                    .infix24 => self.head.infix24.get(start_depth, at_depth, byte_key),
                    .infix32 => self.head.infix32.get(start_depth, at_depth, byte_key),
                    .infix40 => self.head.infix40.get(start_depth, at_depth, byte_key),
                    .infix48 => self.head.infix48.get(start_depth, at_depth, byte_key),
                    .infix56 => self.head.infix56.get(start_depth, at_depth, byte_key),
                    .infix64 => self.head.infix64.get(start_depth, at_depth, byte_key),
                    .leaf => self.head.leaf.get(start_depth, at_depth, byte_key),
                    .twig => self.head.twig.get(start_depth, at_depth, byte_key),
                };
            }

            pub fn put(self: Node, start_depth: u8, key: *const [key_length]u8, value: ?T, single_owner: bool, allocator: std.mem.Allocator) allocError!Node {
                return switch (self.tag) {
                    .none => @panic("Called `put` on none."),
                    .branch1 => self.head.branch1.put(start_depth, key, value, single_owner, allocator),
                    .branch2 => self.head.branch2.put(start_depth, key, value, single_owner, allocator),
                    .branch4 => self.head.branch4.put(start_depth, key, value, single_owner, allocator),
                    .branch8 => self.head.branch8.put(start_depth, key, value, single_owner, allocator),
                    .branch16 => self.head.branch16.put(start_depth, key, value, single_owner, allocator),
                    .branch32 => self.head.branch32.put(start_depth, key, value, single_owner, allocator),
                    .branch64 => self.head.branch64.put(start_depth, key, value, single_owner, allocator),
                    .infix8 => self.head.infix8.put(start_depth, key, value, single_owner, allocator),
                    .infix16 => self.head.infix16.put(start_depth, key, value, single_owner, allocator),
                    .infix24 => self.head.infix24.put(start_depth, key, value, single_owner, allocator),
                    .infix32 => self.head.infix32.put(start_depth, key, value, single_owner, allocator),
                    .infix40 => self.head.infix40.put(start_depth, key, value, single_owner, allocator),
                    .infix48 => self.head.infix48.put(start_depth, key, value, single_owner, allocator),
                    .infix56 => self.head.infix56.put(start_depth, key, value, single_owner, allocator),
                    .infix64 => self.head.infix64.put(start_depth, key, value, single_owner, allocator),
                    .leaf => self.head.leaf.put(start_depth, key, value, single_owner, allocator),
                    .twig => self.head.twig.put(start_depth, key, value, single_owner, allocator),
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

        fn BranchNode(comptime bucket_count: u8) type {
            const head_infix_len = 6;
            const body_infix_len = 30;
            const infix_len = head_infix_len + body_infix_len;

            return extern struct {
                /// The address of the pointer associated with the key.
                body: *Body,
                /// The branch depth of the body.
                branch_depth: u8,
                /// The infix stored in this head.
                infix: [head_infix_len]u8 = [_]u8{0} ** head_infix_len,

                const Head = @This();

                const GrownHead = if (bucket_count == max_bucket_count) Head else BranchNode(bucket_count << 1);

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

                        pub fn get(self: *const Bucket, byte_key: u8) Node {
                            for (self.slots) |slot| {
                                if (slot.tag != .none and slot.peekFirst() == byte_key) {
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
                        \\│   Ref#: 󰀃󰀃󰀃󰀃󰀃                            Segments: 󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄        │
                        \\│                                                                                │
                        \\│ Infix                                                                          │
                        \\│ ══════                                                                         │
                        \\│         ┌─node start                                                           │
                        \\│         ▼                                                                      │
                        \\│   Head: 󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅                                                           │
                        \\│   Body: 󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆           │
                        \\│         ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▔▔  ▲          │
                        \\│                                                     branch depth=󰀇󰀇─┘          │
                        \\│ Children                                                                       │
                        \\│ ══════════                                                                     │
                        \\│                                         0123456789ABCDEF     0123456789ABCDEF  │
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

                    var ref_count_data: [5:0]u8 = undefined;
                    _ = std.fmt.bufPrint(&ref_count_data, "{d:_>5}", .{self.body.ref_count}) catch unreachable;
                    var ref_count_iter = (std.unicode.Utf8View.init(&ref_count_data) catch unreachable).iterator();

                    var segment_count_data: [20:0]u8 = undefined;
                    _ = std.fmt.bufPrint(&segment_count_data, "{d:_>20}", .{self.body.segment_count}) catch unreachable;
                    var segment_count_iter = (std.unicode.Utf8View.init(&segment_count_data) catch unreachable).iterator();

                    var head_infix_data: [12:0]u8 = undefined;
                    _ = std.fmt.bufPrint(&head_infix_data, "{s:_>12}", .{std.fmt.fmtSliceHexUpper(&self.infix)}) catch unreachable;
                    var head_infix_iter = (std.unicode.Utf8View.init(&head_infix_data) catch unreachable).iterator();

                    var body_infix_data: [60:0]u8 = undefined;
                    _ = std.fmt.bufPrint(&body_infix_data, "{s:_>60}", .{std.fmt.fmtSliceHexUpper(&self.body.infix)}) catch unreachable;
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
                                '\u{F0004}' => segment_count_iter.nextCodepoint() orelse unreachable,
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
                    new_body.* = Body{ .ref_count = 1, .leaf_count = 1, .segment_count = 1, .infix = undefined };

                    const body_infix_length = @minimum(branch_depth, body_infix_len);
                    const body_infix_start = body_infix_len - body_infix_length;
                    const body_key_start = branch_depth - body_infix_length;
                    mem.copy(u8, new_body.infix[body_infix_start..new_body.infix.len], key[body_key_start..branch_depth]);

                    var new_head = Head{ .branch_depth = branch_depth, .body = new_body };

                    const infix_length = @minimum(branch_depth, infix_len);
                    const smallest_start_depth = branch_depth - infix_length;
                    const actual_start_depth = @maximum(start_depth, smallest_start_depth);
                    const head_infix_length = @minimum(key_length - actual_start_depth, head_infix_len);
                    const key_end = actual_start_depth + head_infix_length;
                    mem.copy(u8, new_head.infix[0..head_infix_length], key[actual_start_depth..key_end]);

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

                pub fn range(self: Head) u8 {
                    _ = self;
                    return infix_len;
                }

                pub fn peekFirst(self: Head) u8 {
                    return self.infix[0];
                }

                pub fn peek(self: Head, start_depth: u8, at_depth: u8) ?u8 {
                    if (self.branch_depth <= at_depth or at_depth < start_depth) return null;
                    if (at_depth < start_depth + head_infix_len) return self.infix[at_depth - start_depth];
                    return self.body.infix[(at_depth + @as(u8, body_infix_len)) - self.branch_depth];
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
                            const old_child_hash = old_child.hash(self.branch_depth, key);
                            const old_child_count = old_child.count();
                            const old_child_segment_count = 1; // TODO old_child.segmentCount(branch_depth);
                            const new_child = try old_child.put(branch_depth, key, value, single_owner, allocator);
                            const new_child_hash = new_child.hash(branch_depth, key);
                            if (Hash.equal(old_child_hash, new_child_hash)) {
                                return Node.from(Head, self);
                            }
                            const new_hash = Hash.xor(Hash.xor(self.body.child_sum_hash, old_child_hash), new_child_hash);
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
                            const new_child_node = try WrapInfixNode(branch_depth, key, InitLeafOrTwigNode(branch_depth, key, value), allocator);
                            const new_hash = Hash.xor(self.body.child_sum_hash, new_child_node.hash(branch_depth, key));
                            const new_count = self.body.leaf_count + 1;
                            const new_segment_count = self.body.segment_count + 1;

                            var self_or_copy = if (single_owner) self else try self.copy(allocator);

                            self_or_copy.body.child_sum_hash = new_hash;
                            self_or_copy.body.leaf_count = new_count;
                            self_or_copy.body.segment_count = new_segment_count;

                            return try self_or_copy.cuckooPut(new_child_node, allocator);
                        }
                    }

                    const new_branch_node_above = try BranchNode(1).init(start_depth, branch_depth, key, allocator); // TODO add check for infix length
                    const new_sibling_node = try WrapInfixNode(branch_depth, key, InitLeafOrTwigNode(branch_depth, key, value), allocator);

                    var recycled_self = self;
                    for (recycled_self.infix) |*byte, i| {
                        byte.* = self.peek(start_depth, branch_depth + @intCast(u8, i)) orelse break;
                    }

                    _ = try new_branch_node_above.cuckooPut(Node.from(Head, recycled_self), allocator); // We know that these can't fail and won't reallocate.
                    _ = try new_branch_node_above.cuckooPut(new_sibling_node, allocator);

                    new_branch_node_above.body.child_sum_hash = Hash.xor(recycled_self.body.child_sum_hash, new_sibling_node.hash(branch_depth, key));
                    new_branch_node_above.body.leaf_count = recycled_self.body.leaf_count + 1;
                    new_branch_node_above.body.segment_count = 3;
                    // We need to check if this insered moved our branchDepth across a segment boundary.
                    // const segmentCount =
                    //     SEGMENT_LUT[depth] === SEGMENT_LUT[this.branchDepth]
                    //     ? this._segmentCount + 1
                    //     : 2;

                    return try WrapInfixNode(start_depth, key, Node.from(BranchNode(1), new_branch_node_above), allocator);
                }

                pub fn get(self: Head, start_depth: u8, at_depth: u8, byte_key: u8) Node {
                    if (at_depth == self.branch_depth and self.cuckooHas(byte_key)) {
                        return self.cuckooGet(byte_key);
                    }
                    if (self.peek(start_depth, at_depth)) |own_key| {
                        if (own_key == byte_key) return Node.from(Head, self);
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

                fn grow(self: Head, allocator: std.mem.Allocator) allocError!GrownHead {
                    if (bucket_count == max_bucket_count) {
                        return self;
                    } else {
                        //std.debug.print("Grow:{*}\n {} -> {} : {} -> {} \n", .{ self.body, Head, GrownHead, @sizeOf(Body), @sizeOf(GrownHead.Body) });
                        const allocation: []align(BODY_ALIGNMENT) u8 = try allocator.reallocAdvanced(std.mem.span(std.mem.asBytes(self.body)), BODY_ALIGNMENT, @sizeOf(GrownHead.Body), .exact);
                        const new_body = std.mem.bytesAsValue(GrownHead.Body, allocation[0..@sizeOf(GrownHead.Body)]);
                        //std.debug.print("Growed:{*}\n", .{new_body});
                        new_body.buckets[new_body.buckets.len / 2 .. new_body.buckets.len].* = new_body.buckets[0 .. new_body.buckets.len / 2].*;
                        return GrownHead{ .branch_depth = self.branch_depth, .infix = self.infix, .body = new_body };
                    }
                }

                fn cuckooPut(self: Head, node: Node, allocator: std.mem.Allocator) allocError!Node {
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
                            return Node.from(Head, self);
                        }

                        if (base_size or attempts == MAX_ATTEMPTS) {
                            const grown = try self.grow(allocator);
                            return grown.cuckooPut(entry, allocator);
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

        fn WrapInfixNode(start_depth: u8, key: *const [key_length]u8, child: Node, allocator: std.mem.Allocator) allocError!Node {
            const child_depth = child.depth();
            const child_range = child.range();

            if(child_depth <= start_depth + child_range) {
                return child;
            }

            const child_start_depth = child_depth - child_range;
            const infix_length = child_start_depth - start_depth;
            
            if (infix_length <= 8) {
                return Node.from(InfixNode(8), try InfixNode(8).init(start_depth, child_start_depth, key, child, allocator));
            }
            if (infix_length <= 16) {
                return Node.from(InfixNode(16), try InfixNode(16).init(start_depth, child_start_depth, key, child, allocator));
            }
            if (infix_length <= 24) {
                return Node.from(InfixNode(24), try InfixNode(24).init(start_depth, child_start_depth, key, child, allocator));
            }
            if (infix_length <= 32) {
                return Node.from(InfixNode(32), try InfixNode(32).init(start_depth, child_start_depth, key, child, allocator));
            }
            if (infix_length <= 40) {
                return Node.from(InfixNode(40), try InfixNode(40).init(start_depth, child_start_depth, key, child, allocator));
            }
            if (infix_length <= 48) {
                return Node.from(InfixNode(48), try InfixNode(48).init(start_depth, child_start_depth, key, child, allocator));
            }
            if (infix_length <= 54) {
                return Node.from(InfixNode(56), try InfixNode(56).init(start_depth, child_start_depth, key, child, allocator));
            }
            if (infix_length <= 64) {
                return Node.from(InfixNode(64), try InfixNode(64).init(start_depth, child_start_depth, key, child, allocator));
            }

            unreachable;
        }


        fn InfixNode(comptime infix_len: u8) type {
            const head_infix_len = 6;
            const body_infix_len = infix_len - head_infix_len;
            return extern struct {
                /// The address of the pointer associated with the key.
                body: *Body,
                /// The child depth of the body.
                child_depth: u8,
                /// The key stored in this entry.
                infix: [head_infix_len]u8 = [_]u8{0} ** head_infix_len,

                const Head = @This();
                const Body = extern struct {
                    child: Node = Node.none,
                    ref_count: u16 = 1,
                    infix: [body_infix_len]u8 = undefined,
                };

                pub fn init(start_depth: u8, child_depth: u8, key: *const [key_length]u8, child: Node, allocator: std.mem.Allocator) allocError!Head {
                    const allocation = try allocator.allocAdvanced(u8, @alignOf(Body), @sizeOf(Body), .exact);
                    const new_body = std.mem.bytesAsValue(Body, allocation[0..@sizeOf(Body)]);

                    new_body.* = Body{ .child = child };


                    const body_infix_length = @minimum(child_depth, body_infix_len);
                    const body_infix_start = body_infix_len - body_infix_length;
                    const body_key_start = child_depth - body_infix_length;
                    mem.copy(u8, new_body.infix[body_infix_start..new_body.infix.len], key[body_key_start..child_depth]);

                    var new_head = Head{ .child_depth = child_depth, .body = new_body };

                    const head_infix_length = @minimum(child_depth - start_depth, head_infix_len);
                    const key_end = start_depth + head_infix_length;
                    mem.copy(u8, new_head.infix[0..head_infix_length], key[start_depth..key_end]);

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
                    try writer.print("  infixes: {[2]s:_>[0]} > {[3]s:_>[1]}\n", .{
                        head_infix_len, body_infix_len,
                        std.fmt.fmtSliceHexUpper(&self.infix),
                        std.fmt.fmtSliceHexUpper(&self.body.infix) });
                }

                fn copy(self: Head, allocator: std.mem.Allocator) allocError!Head {
                    const allocation = try allocator.allocAdvanced(u8, @alignOf(Body), @sizeOf(Body), .exact);
                    const new_body = std.mem.bytesAsValue(Body, allocation[0..@sizeOf(Body)]);
                    new_body.* = self.body.*;
                    new_body.ref_count = 1;
                    
                    if(try new_body.child.ref(allocator)) |new_child| {
                        new_body.child = new_child;
                    }

                    return Head{ .child_depth = self.child_depth, .infix = self.infix, .body = new_body };
                }

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
                        self.body.child.rel(allocator);
                        allocator.free(std.mem.asBytes(self.body));
                    }
                }

                pub fn count(self: Head) u64 {
                    return self.body.child.count();
                }

                pub fn hash(self: Head, start_depth: u8, prefix: *const [key_length]u8) Hash {
                    var key = prefix.*; //TODO Get rid of this copy.
                    for (key[start_depth..self.child_depth]) |*byte, i| {
                        byte.* = self.peek(start_depth, start_depth + @intCast(u8, i)) orelse @panic("WTF!!!");
                    }
                    return self.body.child.hash(self.child_depth, &key);
                }

                pub fn depth(self: Head) u8 {
                    return self.child_depth;
                }

                pub fn range(self: Head) u8 {
                    _ = self;
                    return infix_len;
                }

                pub fn peekFirst(self: Head) u8 {
                    return self.infix[0];
                }

                pub fn peek(self: Head, start_depth: u8, at_depth: u8) ?u8 {
                    if (self.child_depth <= at_depth or at_depth < start_depth) return null;
                    if (at_depth < start_depth + head_infix_len) return self.infix[at_depth - start_depth];
                    return self.body.infix[(at_depth + @as(u8, body_infix_len)) - self.child_depth];
                }

                pub fn propose(self: Head, start_depth: u8, at_depth: u8, result_set: *ByteBitset) void {
                    if (at_depth == self.child_depth) {
                        result_set.singleIntersect(self.body.child.peekFirst());
                        return;
                    }

                    if (self.peek(start_depth, at_depth)) |byte_key| {
                        result_set.singleIntersect(byte_key);
                        return;
                    }

                    result_set.unsetAll();
                }

                pub fn get(self: Head, start_depth: u8, at_depth: u8, key: u8) Node {
                    if (at_depth == self.child_depth and (key == self.body.child.peekFirst())) {
                        return self.body.child;
                    }

                    if (self.peek(start_depth, at_depth)) |own_key| {
                        if (own_key == key) return Node.from(Head, self);
                    }
                    
                    return Node.none;
                }

                pub fn put(self: Head, start_depth: u8, key: *const [key_length]u8, value: ?T, single_owner: bool, allocator: std.mem.Allocator) allocError!Node {
                    _ = single_owner;
                    if (body_infix_len == 0) unreachable;

                    var branch_depth = start_depth;
                    while (branch_depth < self.child_depth) : (branch_depth += 1) {
                        if (key[branch_depth] != self.peek(start_depth, branch_depth).?) break;
                    } else {
                        const old_child = self.body.child;
                        const old_child_hash = old_child.hash(self.child_depth, key);
                        const new_child = try old_child.put(branch_depth, key, value, single_owner, allocator);
                        const new_child_hash = new_child.hash(branch_depth, key);
                        if (Hash.equal(old_child_hash, new_child_hash)) { //TODO could these be replaced by pointer comparisons?
                            return Node.from(Head, self);
                        }

                        const child_depth = new_child.depth();
                        const child_range = new_child.range();

                        if(child_depth <= start_depth + child_range) {
                            if (single_owner) {
                                allocator.free(std.mem.asBytes(self.body));
                            }
                            return new_child;
                        }

                        var self_or_copy = self;
                        if (!single_owner) {
                            self_or_copy = try self.copy(allocator);
                            old_child.rel(allocator);
                        }
                        
                        for (self_or_copy.infix) |*byte, i| {
                            byte.* = self.peek(start_depth, branch_depth + @intCast(u8, i)) orelse break;
                        }
                        self_or_copy.body.child = new_child;
                        return Node.from(Head, self_or_copy);
                    }

                    var recycled_self = self;

                    const new_branch_node_above = try BranchNode(1).init(start_depth, branch_depth, key, allocator);
                    const sibling_node = try WrapInfixNode(branch_depth, key, InitLeafOrTwigNode(branch_depth, key, value), allocator);
                    for (recycled_self.infix) |*byte, i| {
                        byte.* = self.peek(start_depth, branch_depth + @intCast(u8, i)) orelse break;
                    }

                    _ = try new_branch_node_above.cuckooPut(Node.from(Head, recycled_self), allocator); // We know that these can't fail and won't reallocate.
                    _ = try new_branch_node_above.cuckooPut(sibling_node, allocator);
                    new_branch_node_above.body.child_sum_hash = Hash.xor(recycled_self.hash(branch_depth, key), sibling_node.hash(branch_depth, key));
                    new_branch_node_above.body.leaf_count = recycled_self.count() + 1;
                    new_branch_node_above.body.segment_count = 2; //TODO

                    return try WrapInfixNode(start_depth, key, Node.from(BranchNode(1), new_branch_node_above), allocator);
                }
            };
        }

        fn InitLeafOrTwigNode(start_depth: u8, key: *const [key_length]u8, maybe_value: ?T) Node {
            if (maybe_value) |value| {
                return Node.from(LeafNode, LeafNode.init(start_depth, key, value));
            } else {
                return Node.from(TwigNode, TwigNode.init(start_depth, key));
            }
        }

        const LeafNode = extern struct {
            pub const suffix_len = 15 - @sizeOf(T); // TODO Check that this doesn't underflow.

            /// The key stored in this entry.
            suffix: [suffix_len]u8 = [_]u8{0} ** suffix_len,
            value: T,

            const Head = @This();

            pub fn init(start_depth: u8, key: *const [key_length]u8, value: T) Head {
                var new_head = Head{.value = value};

                const head_suffix_length = @minimum(key_length - start_depth, new_head.suffix.len);
                const key_start = key_length - head_suffix_length;
                mem.copy(u8, new_head.suffix[0..head_suffix_length], key[key_start..key_length]);

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
                try writer.print("Leaf suffix: {[1]s:_>[0]}\n", .{
                    suffix_len,
                    std.fmt.fmtSliceHexUpper(&self.suffix) });
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
                    std.debug.print("{d} {d} {d}\n", .{i, 0, start_depth + @intCast(u8, i)});
                    byte.* = self.peek(start_depth, start_depth + @intCast(u8, i)) orelse @panic("WTF2!!!");
                }
                return keyHash(&key);
            }

            pub fn depth(self: Head) u8 {
                _ = self;
                return key_length;
            }

            pub fn range(self: Head) u8 {
                    _ = self;
                    return suffix_len;
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
                    if (own_key == key) return Node.from(Head, self);
                }
                return Node.none;
            }

            pub fn put(self: Head, start_depth: u8, key: *const [key_length]u8, value: ?T, single_owner: bool, allocator: std.mem.Allocator) allocError!Node {
                _ = single_owner;

                var branch_depth = start_depth;
                while (branch_depth < key_length) : (branch_depth += 1) {
                    if (key[branch_depth] != self.peek(start_depth, branch_depth).?) break;
                } else {
                    return Node.from(Head, self);
                }

                var recycled_self = self;

                const new_branch_node_above = try BranchNode(1).init(start_depth, branch_depth, key, allocator);
                const sibling_leaf_node = InitLeafOrTwigNode(branch_depth, key, value);

                for (recycled_self.suffix) |*byte, i| {
                    byte.* = self.peek(start_depth, branch_depth + @intCast(u8, i)) orelse break;
                }

                _ = try new_branch_node_above.cuckooPut(Node.from(Head, recycled_self), allocator); // We know that these can't fail and won't reallocate.
                _ = try new_branch_node_above.cuckooPut(sibling_leaf_node, allocator);
                new_branch_node_above.body.child_sum_hash = Hash.xor(recycled_self.hash(branch_depth, key), sibling_leaf_node.hash(branch_depth, key));
                new_branch_node_above.body.leaf_count = 2;
                new_branch_node_above.body.segment_count = 2;

                return Node.from(BranchNode(1), new_branch_node_above);
            }
        };

        const TwigNode = extern struct {
            const suffix_len = 15;

            /// The key stored in this entry.
            suffix: [suffix_len]u8 = [_]u8{0} ** suffix_len,

            const Head = @This();

            pub fn init(start_depth: u8, key: *const [key_length]u8) Head {
                var new_head = Head{};

                const head_suffix_length = @minimum(key_length - start_depth, new_head.suffix.len);
                const key_start = key_length - head_suffix_length;
                mem.copy(u8, new_head.suffix[0..head_suffix_length], key[key_start..key_length]);

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
                try writer.print("Twig suffix: {[1]s:_>[0]}\n", .{
                    suffix_len,
                    std.fmt.fmtSliceHexUpper(&self.suffix) });
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
                    byte.* = self.peek(start_depth, start_depth + @intCast(u8, i)) orelse @panic("WTF3!!!");
                }
                return keyHash(&key);
            }

            pub fn depth(self: Head) u8 {
                _ = self;
                return key_length;
            }

            pub fn range(self: Head) u8 {
                _ = self;
                return suffix_len;
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
                    if (own_key == key) return Node.from(Head, self);
                }
                return Node.none;
            }

            pub fn put(self: Head, start_depth: u8, key: *const [key_length]u8, value: ?T, single_owner: bool, allocator: std.mem.Allocator) allocError!Node {
                _ = single_owner;

                var branch_depth = start_depth;
                while (branch_depth < key_length) : (branch_depth += 1) {
                    if (key[branch_depth] != self.peek(start_depth, branch_depth).?) break;
                } else {
                    return Node.from(Head, self);
                }

                var recycled_self = self;

                const new_branch_node_above = try BranchNode(1).init(start_depth, branch_depth, key, allocator);
                const sibling_leaf_node = InitLeafOrTwigNode(branch_depth, key, value);

                for (recycled_self.suffix) |*byte, i| {
                    byte.* = self.peek(start_depth, branch_depth + @intCast(u8, i)) orelse break;
                }

                _ = try new_branch_node_above.cuckooPut(Node.from(Head, recycled_self), allocator); // We know that these can't fail and won't reallocate.
                _ = try new_branch_node_above.cuckooPut(sibling_leaf_node, allocator);
                new_branch_node_above.body.child_sum_hash = Hash.xor(recycled_self.hash(branch_depth, key), sibling_leaf_node.hash(branch_depth, key));
                new_branch_node_above.body.leaf_count = 2;
                new_branch_node_above.body.segment_count = 2;

                return Node.from(BranchNode(1), new_branch_node_above);
            }
        };

        const Tree = struct {
            child: Node = Node{ .tag = .none, .head = .{ .none = void{} } },
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

                fn next(self: *NodeIterator) ?IterationResult {
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
                                assert(self.path[exhausted_depth].tag != .none);
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

            fn nodes(self: *const Tree) NodeIterator {
                var iterator = NodeIterator{};
                if (self.child.tag != .none) {
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
                    \\│ Tree                                                                           │
                    \\│━━━━━━                                                                          │
                    \\│                                                                                │
                    \\│        Count: 󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀󰀀      Memory (keys): 󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂󰀂            │
                    \\│                                                                                │
                    \\│   Node Count: 󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁󰀁    Memory (actual): 󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄󰀄            │
                    \\│                                                                                │
                    \\│  Alloc Count: 󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃󰀃        Compression: 󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅󰀅            │
                    \\│                                                                                │
                    \\│  Node Distribution                                   infix8 󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐󰀐   │
                    \\│ ═══════════════════       branch1 󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉󰀉  infix16 󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑󰀑   │
                    \\│                           branch2 󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊󰀊  infix24 󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒󰀒   │
                    \\│                           branch4 󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋󰀋  infix32 󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓󰀓   │
                    \\│                           branch8 󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌󰀌  infix40 󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔󰀔   │
                    \\│   leaf 󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆󰀆  branch16 󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍󰀍  infix48 󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕󰀕   │
                    \\│   twig 󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇󰀇  branch32 󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎󰀎  infix56 󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖󰀖   │
                    \\│   none 󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈󰀈  branch64 󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏󰀏  infix64 󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗󰀗   │
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

                var mem_keys: u64 = item_count * key_length;

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

                var density_at_depth: [key_length]u64 = [_]u64{0} ** key_length;

                var node_iter = self.nodes();
                while (node_iter.next()) |res| {
                    node_count += 1;
                    density_at_depth[res.start_depth] += 1;
                    switch (res.node.tag) {
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

                const mem_ratio: f64 = 0;//@intToFloat(f64, mem_keys) / @intToFloat(f64, mem_actual)

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

                var mem_ratio_data: [16:0]u8 = undefined;
                _ = std.fmt.bufPrint(&mem_ratio_data, "{d:_>16}", .{mem_ratio}) catch unreachable;
                var mem_ratio_iter = (std.unicode.Utf8View.init(&mem_ratio_data) catch unreachable).iterator();

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

                const density_pos = card.findTopLeft('\u{F0019}').?;

                for (card.grid) |*row, global_y| {
                    for (row.*) |*cell, global_x| {
                        cell.* = switch (cell.*) {
                            '\u{F0000}' => count_iter.nextCodepoint().?,
                            '\u{F0001}' => node_count_iter.nextCodepoint().?,
                            '\u{F0002}' => alloc_count_iter.nextCodepoint().?,
                            '\u{F0003}' => mem_keys_iter.nextCodepoint().?,
                            '\u{F0004}' => mem_actual_iter.nextCodepoint().?,
                            '\u{F0005}' => mem_ratio_iter.nextCodepoint().?,
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

                try writer.print("{s}\n", .{card});
                try writer.writeAll("");
            }

            pub fn count(self: *const Tree) u64 {
                return self.child.count();
            }

            pub fn put(self: *Tree, key: *const [key_length]u8, value: ?T) allocError!void {
                if (self.child.tag == .none) {
                    const leaf_node = InitLeafOrTwigNode(0, key, value);
                    self.child = try WrapInfixNode(0, key, leaf_node, self.allocator);
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

    std.debug.print("Alignment: {} {}\n", .{ PACT.BranchNode(1), @alignOf(PACT.BranchNode(1).Body) });
    std.debug.print("Alignment: {} {}\n", .{ PACT.BranchNode(2), @alignOf(PACT.BranchNode(2).Body) });
    std.debug.print("Alignment: {} {}\n", .{ PACT.BranchNode(4), @alignOf(PACT.BranchNode(4).Body) });
    std.debug.print("Alignment: {} {}\n", .{ PACT.BranchNode(8), @alignOf(PACT.BranchNode(8).Body) });
    std.debug.print("Alignment: {} {}\n", .{ PACT.BranchNode(16), @alignOf(PACT.BranchNode(16).Body) });
    std.debug.print("Alignment: {} {}\n", .{ PACT.BranchNode(32), @alignOf(PACT.BranchNode(32).Body) });
    std.debug.print("Alignment: {} {}\n", .{ PACT.BranchNode(64), @alignOf(PACT.BranchNode(64).Body) });
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
        std.debug.print("Inserted {d} of {d}:{s}\n{s}\n", .{ i + 1, total_runs, std.fmt.fmtSliceHexUpper(&key), tree.child });
    }
    try expectEqual(tree.count(), total_runs);
}

const time = std.time;

//                        | <------ 48 bit -------->|
// kernel space: | 1....1 | significant | alignment |
// user space:   | 0....1 | significant | alignemnt |

// 8:tag = 0 | 56:suffix
// 8:tag = 1 | 8:infix | 48:leaf ptr
// 8:tag = 2 | 8:infix | 48:branch ptr

const benchmark_size: usize = 50;

test "benchmark" {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = general_purpose_allocator.deinit();
    const gpa = general_purpose_allocator.allocator();

    var timer = try time.Timer.start();
    var t_total: u64 = 0;

    var rnd = std.rand.DefaultPrng.init(0).random();

    const PACT = makePACT(Trible.size, usize);
    var tree = PACT.Tree.init(gpa);
    defer tree.deinit();

    std.debug.print("Inserting {d} tribles into PACT.\n", .{benchmark_size});

    var i: u64 = 0;
    var t = Trible.initAribitrary(rnd);
    while (i < benchmark_size) : (i += 1) {
        t = Trible.initAribitraryLike(rnd, 0.2, t);
        //const value = rnd.int(usize);

        timer.reset();

        try tree.put(&t.data, null);

        t_total += timer.lap();
    }

    std.debug.print("Inserted {d} in {d}ns\n", .{ benchmark_size, t_total });

    std.debug.print("{s}\n", .{tree});

    var node_iter = tree.nodes();
    while(node_iter.next()) |res| {
         std.debug.print("Depth: {d}..{d}\n{s}\n", .{res.start_depth, res.node.depth(), res.node});
    }

}

test "benchmark hashing" {
    var timer = try time.Timer.start();
    var t_total: u64 = 0;

    var rnd = std.rand.DefaultPrng.init(0).random();

    std.debug.print("Hashing {d} tribles.\n", .{benchmark_size});

    var i: u64 = 0;
    var t = Trible.initAribitrary(rnd);
    while (i < benchmark_size) : (i += 1) {
        t = Trible.initAribitraryLike(rnd, 0.2, t);

        timer.reset();

        _ = keyHash(&t.data);

        t_total += timer.lap();
    }

    std.debug.print("Hashed {d} in {d}ns\n", .{ benchmark_size, t_total });
}

test "benchmark std" {
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
        t = Trible.initAribitraryLike(rnd, 0.2, t);
        //const value = rnd.int(usize);

        timer.reset();

        try map.put(t, null);

        t_total += timer.lap();
    }

    std.debug.print("Inserted {d} in {d}ns\n", .{ benchmark_size, t_total });
}
