const std = @import("std");
const assert = std.debug.assert;
const expect = std.testing.expect;
const mem = std.mem;

const ByteBitset = std.StaticBitSet(256);

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
        element.* = @bitReverse(u8, @intCast(u8, i));
    }
    return lut;
}

/// Choose a random element from a bitset.
fn random_choice(rng: std.rand.Random, set: ByteBitset) ?u8 {
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

fn generate_rand_LUT_helper(rng: std.rand.Random, dependencies: []const Byte_LUT, i: usize, remaining: ByteBitset, mask: u8, lut: *Byte_LUT) bool {
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

fn generate_pearson_LUT(comptime rng: std.rand.Random) Byte_LUT {
    const no_deps = [0]Byte_LUT{};
    return generate_rand_LUT(rng, no_deps[0..], 0b11111111);
}

fn makePACT(comptime key_length: u8, comptime T: type, allocator: std.mem.Allocator) type {
    return struct {
        const Node = union(NodeHeader) { inner: *InnerNode, leaf: *LeafNode };

        const NodeHeader = enum(u8) {
            inner, leaf,

            pub fn toNode(self: *NodeHeader) Node {
                return switch (self.*) {
                    .leaf => .{ .leaf = @fieldParentPtr(LeafNode, "header", self) },
                    .inner => .{ .inner = @fieldParentPtr(InnerNode, "header", self) },
                };
            }
                        
            pub fn ref(self: *NodeHeader) !void {
                try switch(self.toNode()) {
                    .inner => |node| node.ref(),
                    .leaf => |node| node.ref(),
                };
            }
                        
            pub fn rel(self: *NodeHeader) void {
                switch(self.toNode()) {
                    .inner => |node| node.rel(),
                    .leaf => |node| node.rel(),
                }
            }

            pub fn peek(self: *NodeHeader, depth: u8) ?u8 {
                return switch(self.toNode()) {
                    .inner => |node| node.peek(depth),
                    .leaf => |node| node.peek(depth),
                };
            }

            pub fn propose(self: *NodeHeader, depth: u8, result_set: *ByteBitset) void {
                return switch(self.toNode()) {
                    .inner => |node| node.propose(depth, result_set),
                    .leaf => |node| node.propose(depth, result_set),
                };
            }

            pub fn get(self: *NodeHeader, depth: u8, key: u8) ?*NodeHeader {
                return switch(self.toNode()) {
                    .inner => |node| node.get(depth, key),
                    .leaf => |node| node.get(depth, key),
                };
            }

            pub fn put(self: *NodeHeader, depth: u8, key: *[key_length]u8, value: T) *NodeHeader {
                return switch(self.toNode()) {
                    .inner => |node| node.put(depth, key, value),
                    .leaf => |node| node.put(depth, key, value),
                };
            }
        };

        const InnerNode = struct {
            const rand_lut = blk: {
                @setEvalBranchQuota(1000000);
                var rand_state = std.rand.Xoroshiro128.init(0);
                break :blk generate_pearson_LUT(rand_state.random());
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
                const luts = generate_hash_LUTs(comptime rand.random());
                const mask = c - 1;
                return mask & luts[v][p];
            }

            const Bucket = struct {
                const SLOT_COUNT = 8;

                const Entry = struct {
                    /// The address of the pointer associated with the key.
                    ptr: u48 align(@alignOf(u64)) = 0,
                    /// The key stored in this entry.
                    key: u8 = 0,
                    padding: u8 = 0,

                    /// Create new bucket entry for the given key and ptr.
                    fn with(key: u8, ptr: *NodeHeader) Entry {
                        return Entry{.key = key, .ptr = @intCast(u48, @ptrToInt(ptr))};
                    }

                    /// Check if the entry has a value for the provided key.
                    fn has(self: *const Entry, key: u8) bool {
                        return self.key == key and self.ptr != 0;
                    }
                    /// Try to return the pointer stored in this entry iff
                    /// it matches the provided key.
                    fn get(self: *const Entry, key: u8) ?*NodeHeader {
                        return if (self.has(key)) @intToPtr(*NodeHeader, self.ptr)
                        else null;
                    }

                    /// Store the key and associated pointer in this entry.
                    fn set(self: *Entry, key: u8, ptr: *NodeHeader) void {
                        self.key = key;
                        self.ptr = @intCast(u48, @ptrToInt(ptr));
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

                /// Retrieve the value stored, value must exist.
                pub fn get(self: *const Bucket, key: u8) *NodeHeader {
                    for (self.slots) |slot| {
                        return slot.get(key) orelse continue;
                    }
                    unreachable;
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
            header: NodeHeader,
            ref_count: u16 = 1,
            branch_depth: u8,
            padding: u8 = 0,
            bucket_count: u8 = 1,
            count: u40,
            segment_count: u40,
            key_infix: [32]u8,
            child_set: ByteBitset = ByteBitset.initEmpty(),
            hash_set: ByteBitset = ByteBitset.initEmpty(),

            fn byte_size(bucket_count: u8) u8 {
                return @sizeOf(InnerNode) + (bucket_count * @sizeOf(Bucket));
            }

            pub fn init(branch_depth: u8, key: *const [key_length]u8) *InnerNode {
                const allocation = allocator.allocWithOptions(u8, byte_size(1), @alignOf(InnerNode), null) catch unreachable;
                const new = @ptrCast(*InnerNode, allocation);
                new.* = InnerNode{ .header = .inner,
                                   .ref_count = 1,
                                   .branch_depth = branch_depth,
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

            fn raw(self: *InnerNode) []u8 {
                return @ptrCast([*]u8, self)[0..byte_size(self.bucket_count)];
            }

            pub fn ref(self: *InnerNode) !*InnerNode {
                if(self.ref_count == std.math.maxInt(@TypeOf(self.ref_count))) {
                    // Reference counter exhausted, we need to make a copy of this node.
                    const byte_count =  byte_size(self.bucket_count);
                    const allocation = allocator.allocWithOptions(u8, byte_count, @alignOf(InnerNode), null) catch unreachable;
                    const new = @ptrCast(*InnerNode, allocation);
                    mem.copy(u8, new.raw(), self.raw());
                    new.ref_count = 1;
                    
                    var child_iterator = new.child_set.iterator(.{.direction = .forward});
                    while(child_iterator.next()) |child_byte_key| {
                        const child = new.cuckooGet(child_byte_key);
                        const new_child = child.ref();
                        if(child != new_child) {
                            new.cuckooUpdate(Bucket.Entry.with(child_byte_key, new_child));
                        }
                    }
                    return new;
                } else {
                    self.header.ref_count += 1;
                    return self;
                }
            }

            pub fn rel(self: *InnerNode) void {
                self.ref_count -= 1;
                if(self.ref_count == 0) {
                    defer allocator.free(self.raw());
                    var child_iterator = self.child_set.iterator(.{.direction = .forward});
                    while(child_iterator.next()) |child_byte_key| {
                        self.cuckooGet(@intCast(u8, child_byte_key)).rel();
                    }
                }
            }

            fn putBranch(self: *InnerNode, key: u8, value: *NodeHeader) *InnerNode {
                const buckets = self.bucketSlice();

                if (self.child_set.isSet(key)) {
                    const index = hash(if (self.hash_set.isSet(key)) 0 else 1, self.bucket_count, key);
                    buckets[index].update(Bucket.Entry.with(key, value));
                } else {
                    var entry = Bucket.Entry.with(key, value);
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

            fn grow(self: *InnerNode, copy: bool) *@This() {
                _ = copy;
                self.bucket_count = self.bucket_count << 1;
                const allocation = allocator.realloc(u8, self, @sizeOf(@This()) + @sizeOf(Bucket) * self.bucket_count) catch unreachable;
                const new = @ptrCast(@This(), allocation);
                const buckets = new.bucketSlice();
                mem.copy(Bucket, buckets[0 .. self.bucket_count / 2], buckets[self.bucket_count / 2 .. self.bucket_count]);
                return new;
            }

            fn bucketSlice(self: *InnerNode) []Bucket {
                const ptr = @intToPtr([*]Bucket, @ptrToInt(self) + @sizeOf(@This()));
                return ptr[0..self.bucket_count];
            }

            fn cuckooGet(self: *InnerNode, key: u8) *NodeHeader {
                const bucketIndex = hash(if (self.hash_set.isSet(key)) 0 else 1, self.bucket_count, key);
                return self.bucketSlice()[bucketIndex].get(key);
            }

            fn cuckooUpdate(self: *InnerNode, key: u8, ptr: *NodeHeader) void {
                const bucketIndex = hash(if (self.hash_set.isSet(key)) 0 else 1, self.bucket_count, key);
                return self.bucketSlice()[bucketIndex].update(Bucket.Entry.from(key, ptr));
            }

            pub fn put(self: *InnerNode, depth: u8, key: u8, ptr: *NodeHeader) void {
                _ = self;
                _ = depth;
                _ = key;
                _ = ptr;
            }

            pub fn get(self: *InnerNode, depth: u8, key: u8) ?*NodeHeader {
                if (depth < self.branch_depth) {
                    const index: u8 = (depth + @as(u8, self.key_infix.len)) - self.branch_depth;
                    const infix_key = self.key_infix[index];
                    if (infix_key == key) {
                        return &self.header;
                    }
                } else {
                    if (self.child_set.isSet(key)) {
                        return self.cuckooGet(key);
                    }
                }
                return null;
            }
        };

        const LeafNode = struct {
            header: NodeHeader,
            suffix_len: u8,
            ref_count: u16 = 1,
            value: T,

            fn byte_size(suffix_len: u8) u8 {
                return @sizeOf(LeafNode) + suffix_len;
            }

            pub fn init(branch_depth: u8, key: *const [key_length]u8) *InnerNode {
                const new_suffix_len = key_length - branch_depth;
                const allocation = allocator.allocWithOptions(u8, byte_size(new_suffix_len), @alignOf(LeafNode), null) catch unreachable;
                const new = @ptrCast(*LeafNode, allocation);
                new.* = LeafNode{
                                .header = .leaf,
                                .ref_count = 1,
                                .suffix_len = new_suffix_len,
                                .value = undefined };
                const new_suffix = new.suffixSlice();
                const key_start = key_length - new_suffix.len;
                mem.copy(u8, new.suffixSlice(), key[key_start..key_length]);
                return new;
            }

            fn raw(self: *LeafNode) []u8 {
                return @ptrCast([*]u8, self)[0..byte_size(self.suffix_len)];
            }

            pub fn ref(self: *LeafNode) !*LeafNode {
                if(self.ref_count == std.math.maxInt(@TypeOf(self.ref_count))) {
                    // Reference counter exhausted, we need to make a copy of this node.
                    const byte_count = byte_size(self.suffix_len);
                    const allocation = allocator.allocWithOptions(u8, byte_count, @alignOf(LeafNode), null) catch unreachable;
                    const new = @ptrCast(*LeafNode, allocation);
                    mem.copy(u8, new.raw(), self.raw());
                    new.ref_count = 1;
                    return new;
                } else {
                    self.ref_count += 1;
                    return self;
                }
            }

            pub fn rel(self: *LeafNode) void {
                self.ref_count -= 1;
                if(self.ref_count == 0) {
                    defer allocator.free(self.raw());
                }
            }

            fn suffixSlice(self: *LeafNode) []u8 {
                const ptr = @intToPtr([*]u8, @ptrToInt(self) + @sizeOf(@This()));
                return ptr[0..self.suffix_len];
            }

            pub fn peek(self: *LeafNode, depth: u8) ?u8 {
                if (depth < key_length) return self.key[depth];
                return null;
            }

            pub fn propose(self: *LeafNode, depth: u8, result_set: *ByteBitset) void {
                var set = ByteBitset.initEmpty();
                set.set(self.key[depth]);
                result_set.setIntersection(set);
            }

            pub fn get(self: *LeafNode, depth: u8, key: u8) ?*NodeHeader {
                if (depth < key_length and self.key[depth] == key) return &self.header;
                return null;
            }

            pub fn put(self: *LeafNode, depth: u8, key: *[key_length]u8, value: T) *NodeHeader {
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
    defer inner.rel();
    var node = PACT.Node{ .inner = inner };
    try expect(node.inner.get(0, 0) == null);
}
