const std = @import("std");
const packedBS = @import("./PackedArrayBitSet.zig");
const assert = std.debug.assert;
const expect = std.testing.expect;
const mem = std.mem;

const ByteBitset = packedBS.ArrayBitSet(usize, 256);

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
        element.* = @bitReverse(u8, i);
    }
    return lut;
}

/// Choose a random element from a bitset.
fn random_choice(rng: *std.rand.Random, set: ByteBitset) ?u8 {
    if (set.count() == 0) return null;

    var possible_values: [256]u8 = undefined;
    var possible_values_len: usize = 0;

    var iter = set.iterator(.{});
    while (iter.next()) |b| {
        possible_values[possible_values_len] = @intCast(u8, b);
        possible_values_len += 1;
    }

    const rand_index: u8 = @intCast(u8, rng.uintLessThan(usize, possible_values_len));
    return possible_values[rand_index];
}

fn generate_rand_LUT_helper(rng: *std.rand.Random, dependencies: []const Byte_LUT, i: usize, remaining: ByteBitset, mask: u8, lut: *Byte_LUT) bool {
    if (i == 256) return true;

    var candidates = remaining;
    var iter = remaining.iterator(.{});
    while (iter.next()) |candidate| {
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
    rng: *std.rand.Random,
    dependencies: []const Byte_LUT,
    mask: u8,
) void {
    var lut: Byte_LUT = undefined;
    if (!generate_rand_LUT_helper(rng, dependencies, 0, ByteBitset.initFull(), mask, lut)) unreachable;
    return lut;
}

fn generate_hash_LUTs(comptime rng: *std.rand.Random) Hash_LUT {
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

fn generate_pearson_LUT(comptime rng: *std.rand.Random) Byte_LUT {
    const no_deps = [0]Byte_LUT{};
    return generate_rand_LUT(rng, no_deps[0..], 0b11111111);
}

fn makePACT(comptime key_length: u8, comptime T: type, allocator: std.mem.Allocator) type {
    return struct {
        const NodeType = enum(u8) { inner, leaf };

        const Node = union(NodeType) { inner: *InnerNode, leaf: *LeafNode };

        const PACTHeader = packed struct {
            refcount: u16 = 1,
            type_tag: NodeType,
            branch_depth: u8,

            pub fn toNode(self: *PACTHeader) Node {
                return switch (self.type_tag) {
                    .leaf => .{ .leaf = @fieldParentPtr(LeafNode, "header", self) },
                    .inner => .{ .inner = @fieldParentPtr(InnerNode, "header", self) },
                };
            }
        };

        const InnerNode = packed struct {
            const rand_lut = blk: {
                @setEvalBranchQuota(1000000);
                var rand_state = std.rand.Xoroshiro128.init(0);
                break :blk generate_pearson_LUT(&rand_state.random);
            };
            /// Hashes the value provided with the selected permuation and provided compression.
            fn hash(
                // / Selects the permutation used.
                p: u8,
                // / Bucket count to parameterize the compression used to pigeonhole the items. Must be a power of 2.
                c: u8,
                // / The value to hash.
                v: u8,
            ) u8 {
                @setEvalBranchQuota(1000000);
                comptime var rand = std.rand.Xoroshiro128.init(0);
                const luts = generate_hash_LUTs(&rand.random);
                const mask = c - 1;
                return mask & luts[v][p];
            }

            const Bucket = packed struct {
                const SLOT_COUNT = 8;

                const Entry = packed struct {
                    /// The address of the pointer associated with the key.
                    ptr: u48 align(@alignOf(u64)) = 0,
                    /// The key stored in this entry.
                    key: u8 = 0,
                    padding: u8 = 0,

                    /// Check if the entry has a value for the provided key.
                    fn has(self: *Entry, key: u8) bool {
                        return self.key == key and self.ptr != 0;
                    }
                    /// Try to return the pointer stored in this entry iff
                    /// it matches the provided key.
                    fn get(self: *Entry, key: u8) ?*PACTHeader {
                        return if (self.has(key))
                            @intToPtr(*PACTHeader, self.ptr)
                        else
                            null;
                    }

                    /// Store the key and associated pointer in this entry.
                    fn set(self: *Entry, key: u8, ptr: *PACTHeader) void {
                        self.key = key;
                        self.ptr = @intCast(u56, @ptrToInt(ptr));
                    }

                    /// Checks if the bucket slot is free to use, either
                    /// because it is empty, or because it stores a value
                    /// that is no longer pigeonholed to this index.
                    fn isFree(
                        self: *Entry, // /Answers which hash is used for each item.
                        h: *ByteBitset, // / Current bucket count.
                        c: u8, // / This buckets current index.
                        i: u8,
                    ) bool {
                        return self.ptr == 0 or
                            (i != hash(if (h.isSet(self.key)) 0 else 1, c, self.key));
                    }
                };

                slots: [SLOT_COUNT]Entry = undefined,

                /// Attempts to retrieve the value stored  
                pub fn get(self: *Bucket, key: u8) ?*PACTHeader {
                    for (self.slots) |slot| {
                        return slot.get(key) orelse continue;
                    }
                    return null;
                }

                /// Attempt to store a new key and pointer in this bucket,
                /// the key must not exist in this bucket beforehand.
                /// If there is no free slot the attempt will fail.
                /// Returns true iff it succeeds.
                pub fn put(
                    self: *Bucket(),
                    // / Determines the hash function used for each key and is used to detect outdated (free) slots.
                    hash_select: *ByteBitset,
                    // / The current bucket count. Is used to detect outdated (free) slots.
                    bucket_count: u8,
                    // / The current index the bucket has. Is used to detect outdated (free) slots.
                    bucket_index: u8,
                    // / The entry to be stored in the bucket.
                    entry: Entry,
                ) bool {
                    for (self.slots) |*slot| {
                        if (slot.has(entry.key) or
                            slot.isFree(hash_select, bucket_count, bucket_index))
                        {
                            slot.* = Entry;
                            return true;
                        }
                    }
                    return false;
                }

                /// Updates the pointer for the key stored in this bucket.
                pub fn update(
                    self: *Bucket,
                    // / The new entry value.
                    entry: Entry,
                ) void {
                    for (self.slots) |*slot| {
                        if (slot.has(entry.key)) {
                            slot.* = entry;
                            return;
                        }
                    }
                }

                /// Displaces an existing slot with the .
                pub fn displace(
                    self: *Bucket,
                    // / A random value to determine the slot to displace.
                    randomly_displaced: u8,
                    // / The entry that displaces an existing entry.
                    entry: Entry,
                ) Entry {
                    const index = randomly_displaced & (SLOT_COUNT - 1);
                    const prev = self.slots[index];
                    self.slots[index] = entry;
                    return prev;
                }
            };

            const max_bucket_count = BRANCH_FACTOR / Bucket.SLOT_COUNT;
            var random: u8 = 4; // Chosen by fair dice roll.
            header: PACTHeader,
            bucket_count: u16 = 1,
            count: u40,
            segment_count: u40,
            key_infix: [32]u8,
            child_set: ByteBitset = ByteBitset.initEmpty(),
            hash_set: ByteBitset = ByteBitset.initEmpty(),

            pub fn init(branch_depth: u8, key: *const [key_length]u8) *InnerNode {
                const raw = allocator.allocWithOptions(u8, @sizeOf(InnerNode) +
                    @sizeOf([1]Bucket), @alignOf(InnerNode), null) catch unreachable;
                const new = @ptrCast(*InnerNode, raw);
                new.* = InnerNode{ .header = PACTHeader{ .type_tag = .inner,.branch_depth = branch_depth },
                                   .count = 1,
                                   .segment_count = 1,
                                   .key_infix = undefined };
                const segmentLength = @minimum(branch_depth, 32);
                const segmentStart = 32 - segmentLength;
                const keyStart = branch_depth - segmentLength;
                mem.set(u8, new.key_infix[0..segmentStart], 0);
                mem.copy(u8, new.key_infix[segmentStart..32], key[keyStart..branch_depth]);
                for (new.bucketSlice()) |*bucket| {
                    bucket.* = Bucket{};
                }
                return new;
            }

            fn putBranch(self: *InnerNode, key: u8, value: *PACTHeader) *InnerNode {
                const buckets = self.bucketSlice();
                var entry: Bucket.Entry = undefined;
                entry.set(key, value);

                if (self.child_set.isSet(key)) {
                    const index = hash(if (self.hash_set.isSet(key)) 0 else 1, self.bucket_count, key);
                    buckets[index].update(entry);
                } else {
                    var attempts: u8 = 0;
                    while (true) {
                        random = rand_lut[random ^ entry.key];
                        const hash_index = if (self.bucket_count == max_bucket_count) 0 else random & 1;
                        const bucket_index = hash(hash_index, self.bucket_count, key);
                        if (buckets[bucket_index].put(self.hash_set, self.bucket_count, bucket_index, entry)) break;
                        entry = buckets[bucket_index].displace(random, entry);
                        if (self.bucket_count != max_bucket_count) {
                            attempts += 1;
                            if (attempts == MAX_ATTEMPTS) {
                                attempts = 0;
                                self = self.grow();
                            }
                        }
                    }
                }
                return self;
            }

            fn grow(self: *@This(), copy: bool) *@This() {
                _ = copy;
                self.bucket_count = self.bucket_count << 1;
                const raw = allocator.realloc(u8, self, @sizeOf(@This()) + @sizeOf(Bucket) * self.bucket_count) catch unreachable;
                const new = @ptrCast(@This(), raw);
                const buckets = new.bucketSlice();
                mem.copy(Bucket, buckets[0 .. self.bucket_count / 2], buckets[self.bucket_count / 2 .. self.bucket_count]);
                return new;
            }

            fn bucketSlice(self: *@This()) []Bucket {
                const ptr = @intToPtr([*]Bucket, @ptrToInt(self) + @sizeOf(@This()));
                return ptr[0..self.bucket_count];
            }

            pub fn put(self: *@This(), depth: u8, key: u8, ptr: *PACTHeader) void {
                _ = self;
                _ = depth;
                _ = key;
                _ = ptr;
            }
            pub fn get(self: *@This(), depth: u8, key: u8) ?*PACTHeader {
                if (depth < self.header.branch_depth) {
                    const index: u8 = (depth + @as(u8, self.key_infix.len)) - self.header.branch_depth;
                    const infix_key = self.key_infix[index];
                    if (infix_key == key) {
                        return self;
                    }
                } else {
                    if (self.child_set.isSet(key)) {
                        const bucketIndex = hash(if (self.hash_select.isSet(key)) 0 else 1, self.bucket_count, key);
                        return self.bucketSlice()[bucketIndex].get(key);
                    }
                }
                return null;
            }
        };

        const LeafNode = packed struct {
            header: PACTHeader,
            key: [key_length]u8,
            value: T,

            pub fn init(key: *[key_length]u8, value: T) *@This() {
                const new = allocator.create(@This()) catch unreachable;
                new.* = @This(){ .header = PACTHeader{ .branch_depth = key_length, .refcount = 1 }, .key = key.*, .value = value };
                return new;
            }

            pub fn peek(self: *@This(), depth: u8) ?u8 {
                if (depth < key_length) return self.key[depth];
                return null;
            }

            pub fn propose(self: *@This(), depth: u8, result_set: *ByteBitset) void {
                var set = ByteBitset.initEmpty();
                set.set(self.key[depth]);
                result_set.setIntersection(set);
            }

            pub fn get(self: *@This(), depth: u8, key: u8) ?*PACTHeader {
                if (depth < key_length and self.key[depth] == key) return &self.header;
                return null;
            }

            pub fn put(self: *@This(), depth: u8, key: *[key_length]u8, value: T) *PACTHeader {
                while (depth < key_length and self.key[depth] != key[depth]) depth += 1;

                if (depth == key_length) {
                    return &self.header;
                }

                _ = value;

                // const sibling = LeafNode.init(key, value);

                // const branchChildren = [];
                // const leftIndex = this.key[depth];
                // const rightIndex = key[depth];
                // branchChildren[leftIndex] = this;
                // branchChildren[rightIndex] = sibling;
                // const branchChildbits = emptySet();
                // setBit(branchChildbits, leftIndex);
                // setBit(branchChildbits, rightIndex);
                // const hash = hash_combine(this.hash, sibling.hash);

                // return new Node(
                // this.key,
                // depth,
                // branchChildbits,
                // branchChildren,
                // hash,
                // 2,
                // owner
                // );
            }
        };
    };
}

test "put nothing -> get nothing" {
    const trible_length = 64;
    const PACT = makePACT(trible_length, usize, std.testing.allocator);
    var key = [_]u8{0} ** trible_length;
    var inner = PACT.InnerNode.init(32, &key);
    var node = PACT.Node{ .inner = inner };
    try expect(node.inner.get(0, 0) == null);
}
