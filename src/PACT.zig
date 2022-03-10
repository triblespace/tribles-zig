const std = @import("std");
const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;

const mem = std.mem;

// TODO: change hash set index to boolean or at least make set -> 1, unset -> 0

// Uninitialized memory initialized by init()
var instance_secret : [16]u8 = undefined;

pub fn init() void {
    // XXX: (crest) Should this be a deterministic pseudo-RNG seeded by a constant for reproducable tests?
    std.crypto.random.bytes(&instance_secret);
}

const Hash = struct {
    data: [16]u8,

    pub fn xor(left: Hash, right: Hash) Hash { // TODO make this vector SIMD stuff?
        var hash: Hash = undefined;
        for(hash.data) |*byte, i| {
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

/// One bit per possible byte value 
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

/// Generates a byte -> byte lookup table for pearson hashing.
fn generate_pearson_LUT(comptime rng: std.rand.Random) Byte_LUT {
    const no_deps = [0]Byte_LUT{};
    const lut = generate_rand_LUT(rng, no_deps[0..], 0b11111111);
    assert(is_permutation(&lut));
    return lut;
}

/// Define a PACT datastructure with the given parameters.
pub fn makePACT(comptime key_length: u8, comptime T: type) type {
    return struct {
        const allocError = std.mem.Allocator.Error;

        const Node = union(NodeHeader) { inner: *InnerNode, leaf: *LeafNode };

        const NodeHeader = enum(u8) {
            inner, leaf,

            pub fn toNode(self: *NodeHeader) Node {
                return switch (self.*) {
                    .leaf => .{ .leaf = @fieldParentPtr(LeafNode, "header", self) },
                    .inner => .{ .inner = @fieldParentPtr(InnerNode, "header", self) },
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

                switch(self.toNode()) {
                    .inner => |node| try writer.print("{s}", .{node}),
                    .leaf => |node| try writer.print("{s}", .{node}),
                }
                try writer.writeAll("");
            }

            pub fn ref(self: *NodeHeader, allocator: std.mem.Allocator) allocError!*NodeHeader {
                return switch(self.toNode()) {
                    .inner => |node| node.ref(allocator),
                    .leaf => |node| node.ref(allocator),
                };
            }
                        
            pub fn rel(self: *NodeHeader, allocator: std.mem.Allocator) void {
                switch(self.toNode()) {
                    .inner => |node| node.rel(allocator),
                    .leaf => |node| node.rel(allocator),
                }
            }

            pub fn count(self: *NodeHeader) u40 {
                return switch(self.toNode()) {
                    .inner => |node| node.count(),
                    .leaf => |node| node.count(),
                };
            }

            pub fn hash(self: *NodeHeader) Hash {
                return switch(self.toNode()) {
                    .inner => |node| node.hash(),
                    .leaf => |node| node.hash(),
                };
            }

            pub fn depth(self: *NodeHeader) u8 {
                return switch(self.toNode()) {
                    .inner => |node| node.depth(),
                    .leaf => |node| node.depth(),
                };
            }

            pub fn peek(self: *NodeHeader, at_depth: u8) ?u8 {
                return switch(self.toNode()) {
                    .inner => |node| node.peek(at_depth),
                    .leaf => |node| node.peek(at_depth),
                };
            }

            pub fn propose(self: *NodeHeader, at_depth: u8, result_set: *ByteBitset) void {
                return switch(self.toNode()) {
                    .inner => |node| node.propose(at_depth, result_set),
                    .leaf => |node| node.propose(at_depth, result_set),
                };
            }

            pub fn get(self: *NodeHeader, at_depth: u8, byte_key: u8) ?*NodeHeader {
                return switch(self.toNode()) {
                    .inner => |node| node.get(at_depth, byte_key),
                    .leaf => |node| node.get(at_depth, byte_key),
                };
            }

            pub fn put(self: *NodeHeader, at_depth: u8, key: *const [key_length]u8, value: T, single_owner: bool, allocator: std.mem.Allocator) allocError!*NodeHeader {
                return switch(self.toNode()) {
                    .inner => |node| node.put(at_depth, key, value, single_owner, allocator),
                    .leaf => |node| node.put(at_depth, key, value, single_owner, allocator),
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
                const pi:u8  = if(p) 1 else 0;
                return mask & luts[v][pi];
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
                    fn with(byte_key: u8, ptr: *NodeHeader) Entry {
                        return Entry{.key = byte_key, .ptr = @intCast(u48, @ptrToInt(ptr))};
                    }

                    /// Check if the entry has a value for the provided key.
                    fn has(self: *const Entry, byte_key: u8) bool {
                        return (self.key == byte_key) and (self.ptr != 0);
                    }
                    /// Try to return the pointer stored in this entry iff
                    /// it matches the provided key.
                    fn get(self: *const Entry, byte_key: u8) ?*NodeHeader {
                        // XXX: (crest) !!!BUG!!! doesn't restore pointers to the upper half of the virtual address space into canonical representation on x86_64 -> segfault !!!BUG!!!
                        return if (self.has(byte_key)) @intToPtr(*NodeHeader, self.ptr)
                        else null;
                    }

                    /// Store the key and associated pointer in this entry.
                    fn set(self: *Entry, byte_key: u8, ptr: *NodeHeader) void {
                        self.key = byte_key;
                        self.ptr = @intCast(u48, @ptrToInt(ptr));
                    }

                    /// Checks if the bucket slot is free to use, either
                    /// because it is empty, or because it stores a value
                    /// that is no longer pigeonholed to this index.
                    fn isFree(
                        self: *Entry, 
                        uses_rand_hash: bool, // /Answers which hash was used for this entry.
                        current_count: u8, // / Current bucket count.
                        current_index: u8, // / This buckets current index.
                    ) bool {
                        //std.debug.print("isFree: free={s} or moved={s}\n", .{self.ptr == 0, current_index != hashByteKey(alt_hash, current_count, self.key)});
                        return (self.ptr == 0) or (current_index != hashByteKey(uses_rand_hash, current_count, self.key));
                    }
                };

                slots: [SLOT_COUNT]Entry = [_]Entry{Entry{}}**SLOT_COUNT,

                /// TODO: dump bitsets
                pub fn format(
                    self: Bucket,
                    comptime fmt: []const u8,
                    options: std.fmt.FormatOptions,
                    writer: anytype,
                ) !void {
                    _ = fmt;
                    _ = options;

                    for (self.slots) |slot, i| {
                        if(slot.ptr != 0) {
                            try writer.print("| {d}: {d:3}", .{i, slot.key});
                        } else {
                            try writer.print("|_", .{});
                        }
                    }
                    try writer.writeAll("|");
                }

                /// Retrieve the value stored, value must exist.
                pub fn get(self: *const Bucket, byte_key: u8) *NodeHeader {
                    for (self.slots) |slot| {
                        return slot.get(byte_key) orelse continue;
                    }
                    //std.debug.print("get: {d}\n in: {s}\n", .{byte_key, self});
                    unreachable;
                }

                /// Attempt to store a new key and pointer in this bucket,
                /// the key must not exist in this bucket beforehand.
                /// If there is no free slot the attempt will fail.
                /// Returns true iff it succeeds.
                pub fn put(
                    self: *Bucket,
                    // / Determines the hash function used for each key and is used to detect outdated (free) slots.
                    rand_hash_used: *ByteBitset,
                    // / The current bucket count. Is used to detect outdated (free) slots.
                    bucket_count: u8,
                    // / The current index the bucket has. Is used to detect outdated (free) slots.
                    bucket_index: u8,
                    // / The entry to be stored in the bucket.
                    entry: Entry,
                ) bool {
                    //std.debug.print("setting: {d}\n", .{entry.key});
                    for (self.slots) |*slot| {
                        if (slot.has(entry.key)) {
                            //std.debug.print("did replace: {d}\n", .{entry.key});
                            slot.* = entry;
                            return true;
                        }
                    }
                    for (self.slots) |*slot| {
                        if (slot.isFree(rand_hash_used.isSet(slot.key), bucket_count, bucket_index)) {
                            //std.debug.print("did set: {d}\n", .{entry.key});
                            slot.* = entry;
                            return true;
                        }
                    }
                    //std.debug.print("not set: {d}\n", .{entry.key});
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

                /// Displaces a random existing slot.
                pub fn displaceRandomly(
                    self: *Bucket,
                    // / A random value to determine the slot to displace.
                    random_value: u8,
                    // / The entry that displaces an existing entry.
                    entry: Entry,
                ) Entry {
                    const index = random_value & (SLOT_COUNT - 1);
                    const prev = self.slots[index];
                    self.slots[index] = entry;
                    //std.debug.print("{d} displaces {d}\n", .{entry.key, prev.key});
                    return prev;
                }

                /// Displaces the first slot that is using the alternate hash function.
                pub fn displaceRandHashOnly(
                    self: *Bucket,
                    // / Determines the hash function used for each key and is used to detect outdated (free) slots.
                    rand_hash_used: *ByteBitset,
                    // / The entry to be stored in the bucket.
                    entry: Entry,
                ) Entry {
                    for (self.slots) |*slot| {
                        if (rand_hash_used.isSet(slot.key)) {
                            const prev = slot.*;
                            slot.* = entry;
                            return prev;
                        }
                    }
                    std.debug.print("Somthing went wrong alt-displacing {d} in: {s}\n", .{entry.key, self});
                    unreachable;
                }
            };

            const max_bucket_count = BRANCH_FACTOR / Bucket.SLOT_COUNT;
            var random: u8 = 4; // Chosen by fair dice roll.
            header: NodeHeader,
            ref_count: u16 = 1,
            branch_depth: u8,
            padding: u8 = 0,
            bucket_count: u8 = 1,
            leaf_count: u40 = 1,
            segment_count: u40,
            child_sum_hash: Hash = .{.data=[_]u8{0} ** 16},
            key_infix: [32]u8,
            child_set: ByteBitset = ByteBitset.initEmpty(),
            rand_hash_used: ByteBitset = ByteBitset.initEmpty(),

            fn byte_size(bucket_count: u8) usize {
                return @sizeOf(InnerNode) + (@intCast(usize, bucket_count) * @sizeOf(Bucket));
            }

// ┌─1:type enum / bucket count                                    
// │┌─1:branch depth                                               
// ││ ┌──2:refcount                                                
// ││ │   ┌────6:count                                             
// ││ │   │     ┌─6:segment count                                  
// ││ │   │     │                                                  
// ╻╻┌┐┌────┐┌────┐┌──────────────┐┌──────────────────────────────┐
// ┃┃│││    ││    ││   16:hash    ││         32:key infix         │
// ╹╹└┘└────┘└────┘└──────────────┘└──────────────────────────────┘
// ┌──────────────────────────────┐┌──────────────────────────────┐
// │     32:has-child bitset      ││     32:child hash-choice     │
// └──────────────────────────────┘└──────────────────────────────┘
// ┌──────────────────────────────────────────────────────────────┐
// │                           bucket 0                           │
// └──────────────────────────────────────────────────────────────┘
// ┌ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ 
//                          bucket 1...31                         │
// └ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ 

            pub fn format(
                    self: InnerNode,
                    comptime fmt: []const u8,
                    options: std.fmt.FormatOptions,
                    writer: anytype,
                ) !void {
                    _ = fmt;
                    _ = options;

                    try writer.print("{*} ◁{d}:\n", .{&self, self.ref_count});
                    try writer.print("      depth: {d} | count: {d} | segment_count: {d}\n", .{self.branch_depth, self.leaf_count, self.segment_count});
                    try writer.print("       hash: {s}\n", .{self.child_sum_hash});
                    try writer.print("  key_infix: {s}\n", .{self.key_infix});
                    try writer.print("  child_set: {s}\n", .{self.child_set});
                    try writer.print("   rand_hash_used: {s}\n", .{self.rand_hash_used});
                    try writer.print("   children: ", .{});

                    var child_iterator = self.child_set.iterator(.{.direction = .forward});
                    while(child_iterator.next()) |child_byte_key| {
                        const cast_child_byte_key = @intCast(u8, child_byte_key);
                        const use_rand_hash = self.rand_hash_used.isSet(cast_child_byte_key);
                        const bucket_index = hashByteKey(use_rand_hash, self.bucket_count, cast_child_byte_key);
                        const hash_name = @as([]const u8, if (use_rand_hash) "rnd" else "seq");
                        try writer.print("|{d}:{s}@{d}", .{cast_child_byte_key, hash_name, bucket_index});
                    }
                    try writer.print("|\n", .{});
                    try writer.print("    buckets:\n", .{});
                    for (self.bucketSliceView()) |bucket, i| {
                        try writer.print("        {d}: {s}\n", .{i, bucket});
                    }
                }

            pub fn init(branch_depth: u8, key: *const [key_length]u8, allocator: std.mem.Allocator) !*InnerNode {
                const allocation = try allocator.allocWithOptions(u8, byte_size(1), @alignOf(InnerNode), null);
                const new = @ptrCast(*InnerNode, allocation);
                new.* = InnerNode{ .header = .inner,
                                   .ref_count = 1,
                                   .branch_depth = branch_depth,
                                   .leaf_count = 1,
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
                return @ptrCast([*] align(@alignOf(InnerNode)) u8, self)[0..byte_size(self.bucket_count)];
            }

            fn copy(self: *InnerNode, allocator: std.mem.Allocator) !*InnerNode {
                const byte_count =  byte_size(self.bucket_count);
                const allocation = try allocator.allocWithOptions(u8, byte_count, @alignOf(InnerNode), null);
                const new = @ptrCast(*InnerNode, allocation);
                mem.copy(u8, new.raw(), self.raw());
                new.ref_count = 1;

                var child_iterator = new.child_set.iterator(.{.direction = .forward});
                while(child_iterator.next()) |child_byte_key| {
                    const cast_child_byte_key = @intCast(u8, child_byte_key);
                    const child = new.cuckooGet(cast_child_byte_key);
                    const new_child = try child.ref(allocator);
                    if(child != new_child) {
                        new.cuckooUpdate(cast_child_byte_key, new_child);
                    }
                }

                return new;
            }

            fn head(self: *InnerNode) *NodeHeader {
                return &self.header;
            }

            /// TODO: document this!
            pub fn ref(self: *InnerNode, allocator: std.mem.Allocator) allocError!*NodeHeader {
                if(self.ref_count == std.math.maxInt(@TypeOf(self.ref_count))) {
                    // Reference counter exhausted, we need to make a copy of this node.
                    const new = try self.copy(allocator);
                    return new.head();
                } else {
                    self.ref_count += 1;
                    return self.head();
                }
            }

            pub fn rel(self: *InnerNode, allocator: std.mem.Allocator) void {                
                self.ref_count -= 1;
                if(self.ref_count == 0) {
                    //std.debug.print("Releasing:\n{s}\n", .{self});
                    defer allocator.free(self.raw());
                    var child_iterator = self.child_set.iterator(.{.direction = .forward});
                    while(child_iterator.next()) |child_byte_key| {
                        self.cuckooGet(@intCast(u8, child_byte_key)).rel(allocator);
                    }
                }
            }

            pub fn count(self: *InnerNode) u40 {
                return self.leaf_count;
            }

            pub fn hash(self: *InnerNode) Hash {
                return self.child_sum_hash;
            }

            pub fn depth(self: *InnerNode) u8 {
                return self.branch_depth;
            }

            fn grow(self: *InnerNode, allocator: std.mem.Allocator) !*InnerNode {
                const new_bucket_count = self.bucket_count << 1;
                //std.debug.print("Growing: {s}\n", .{self});
                const allocation = try allocator.reallocAdvanced(self.raw(), @alignOf(InnerNode), byte_size(new_bucket_count), .exact);
                const new = @ptrCast(*InnerNode, allocation);
                new.bucket_count = new_bucket_count;
                const buckets = new.bucketSlice();
                mem.copy(Bucket, buckets[buckets.len / 2 .. buckets.len], buckets[0 .. buckets.len / 2]);
                //std.debug.print("Growed: {s}\n", .{new});
                return new;
            }

            fn bucketSlice(self: *InnerNode) []Bucket {
                const ptr = @intToPtr([*]Bucket, @ptrToInt(self) + @sizeOf(InnerNode));
                return ptr[0..self.bucket_count];
            }

            fn bucketSliceView(self: *const InnerNode) []const Bucket {
                const ptr = @intToPtr([*]Bucket, @ptrToInt(self) + @sizeOf(InnerNode));
                return ptr[0..self.bucket_count];
            }

            fn cuckooPut(self: *InnerNode, byte_key: u8, value: *NodeHeader, allocator: std.mem.Allocator) !*InnerNode {
                assert(!self.child_set.isSet(byte_key)); // XXX: (crest) current debugging session only
                
                if (self.child_set.isSet(byte_key)) {
                    const buckets = self.bucketSlice();
                    const index = hashByteKey(self.rand_hash_used.isSet(byte_key), self.bucket_count, byte_key);
                    buckets[index].update(Bucket.Entry.with(byte_key, value));
                    return self;
                } else {
                    var node = self;
                    var entry = Bucket.Entry.with(byte_key, value);
                    var buckets = node.bucketSlice();
                    var attempts: u8 = 0;
                    var growable = (node.bucket_count != max_bucket_count);
                    var base_size = (node.bucket_count == 1);
                    var use_rand_hash = false;
                    while (true) {
                        //std.debug.print("put loop: {d} with rand: {d} and {d} buckets\n", .{entry.key, random, node.bucket_count});
                        random = rand_lut[random ^ entry.key];
                        const bucket_index = hashByteKey(use_rand_hash, node.bucket_count, entry.key);

                        node.child_set.set(entry.key);
                        node.rand_hash_used.setValue(entry.key, use_rand_hash);
                        
                        if (buckets[bucket_index].put(&node.rand_hash_used, node.bucket_count, bucket_index, entry)) {
                            assert(node.child_set.isSet(entry.key));
                            return node;
                        }

                        if (base_size) {
                            base_size = false;
                            node = try node.grow(allocator);
                            buckets = node.bucketSlice();
                            continue;
                        }

                        if(growable) {
                            entry = buckets[bucket_index].displaceRandomly(random, entry);
                            use_rand_hash = !node.rand_hash_used.isSet(entry.key);

                            attempts += 1;
                            if (attempts == MAX_ATTEMPTS) {
                                attempts = 0;
                                use_rand_hash = false;
                                node = try node.grow(allocator);
                                buckets = node.bucketSlice();
                                growable = (node.bucket_count != max_bucket_count);
                            }
                        } else {
                            entry = buckets[bucket_index].displaceRandHashOnly(&node.rand_hash_used, entry);
                            use_rand_hash = false;
                        }
                    }
                }
                unreachable;
            }

            fn cuckooHas(self: *InnerNode, byte_key: u8) bool {
                return self.child_set.isSet(byte_key);
            }

            // Contract: Key looked up must exist. Ensure with cuckooHas.
            fn cuckooGet(self: *InnerNode, byte_key: u8) *NodeHeader {
                assert(self.child_set.isSet(byte_key));
                const bucket_index = hashByteKey(self.rand_hash_used.isSet(byte_key), self.bucket_count, byte_key);
                return self.bucketSlice()[bucket_index].get(byte_key);
            }

            fn cuckooUpdate(self: *InnerNode, byte_key: u8, ptr: *NodeHeader) void {
                const bucket_index = hashByteKey(self.rand_hash_used.isSet(byte_key), self.bucket_count, byte_key);
                return self.bucketSlice()[bucket_index].update(Bucket.Entry.with(byte_key, ptr));
            }

            pub fn put(self: *InnerNode, at_depth: u8, key: *const [key_length]u8, value: T, parent_single_owner: bool, allocator: std.mem.Allocator) allocError!*NodeHeader {
                const single_owner = parent_single_owner and self.ref_count == 1;
                
                var branch_depth = at_depth;
                var infix_index: u8 = (at_depth + @as(u8, self.key_infix.len)) - self.branch_depth;
                while (branch_depth < self.branch_depth) : ({branch_depth += 1; infix_index += 1;}) {
                    if (key[branch_depth] != self.key_infix[infix_index]) break;
                } else {
                    // The entire compressed infix above this node matched with the key.
                    const byte_key = key[branch_depth];
                    if (self.cuckooHas(byte_key)) {
                        // The node already has a child branch with the same byte discriminator as the one in the key.
                        const old_child = self.cuckooGet(byte_key);
                        const old_child_hash = old_child.hash();
                        const old_child_count = old_child.count();
                        const old_child_segment_count = 1; // TODO old_child.segmentCount(branch_depth);
                        const new_child = try old_child.put(branch_depth + 1, key, value, single_owner, allocator);
                        if (Hash.equal(old_child_hash, new_child.hash())) return self.head();
                        const new_hash = Hash.xor(Hash.xor(self.hash(), old_child_hash), new_child.hash());
                        const new_count = self.leaf_count - old_child_count + new_child.count();
                        const new_segment_count =
                            self.segment_count -
                            old_child_segment_count +
                            1; // TODO new_child.segmentCount(branch_depth);

                        var node = self;
                        if (!single_owner) {
                            node = try self.copy(allocator);
                            old_child.rel(allocator);
                        }
                        node.cuckooUpdate(byte_key, new_child);
                        node.child_sum_hash = new_hash;
                        node.leaf_count = new_count;
                        node.segment_count = new_segment_count;
                        return node.head();
                    } else {
                        const new_child = try LeafNode.init(branch_depth + 1, key, value, keyHash(key), allocator);
                        const new_hash = Hash.xor(self.hash(), new_child.hash());
                        const new_count = self.leaf_count + 1;
                        const new_segment_count = self.segment_count + 1;

                        var node = self;

                        if (!single_owner) {
                            node = try self.copy(allocator);
                        }
                        
                        node = try node.cuckooPut(byte_key, new_child.head(), allocator);
                        node.child_sum_hash = new_hash;
                        node.leaf_count = new_count;
                        node.segment_count = new_segment_count;
                        
                        return node.head();
                    }
                }

                const branch_node = try InnerNode.init(branch_depth, key, allocator);
                const sibling_node = try LeafNode.init(branch_depth + 1, key, value, keyHash(key), allocator);

                const self_byte_key = self.key_infix[infix_index];
                const sibling_byte_key = key[branch_depth];

                _ = try branch_node.cuckooPut(self_byte_key, self.head(), allocator); // We know that these can't fail and won't reallocate.
                _ = try branch_node.cuckooPut(sibling_byte_key, sibling_node.head(), allocator);
                branch_node.child_sum_hash = Hash.xor(self.hash(), sibling_node.hash());
                branch_node.leaf_count = self.leaf_count + 1;
                branch_node.segment_count = 3;
                // We need to check if this insered moved our branchDepth across a segment boundary.
                // const segmentCount =
                //     SEGMENT_LUT[depth] === SEGMENT_LUT[this.branchDepth]
                //     ? this._segmentCount + 1
                //     : 2;

                return branch_node.head();
            }

            pub fn get(self: *InnerNode, at_depth: u8, byte_key: u8) ?*NodeHeader {
                if (at_depth < self.branch_depth) {
                    const index: u8 = (at_depth + @as(u8, self.key_infix.len)) - self.branch_depth;
                    const infix_key = self.key_infix[index];
                    if (infix_key == byte_key) {
                        return self.head();
                    }
                } else {
                    if (self.cuckooHas(byte_key)) {
                        return self.cuckooGet(byte_key);
                    }
                }
                return null;
            }
        };

        const LeafNode = struct {
            header: NodeHeader,
            suffix_len: u8,
            ref_count: u16 = 1,
            key_hash: Hash,
            value: T,

            fn byte_size(suffix_len: u8) usize {
                return @sizeOf(LeafNode) + @intCast(usize, suffix_len);
            }

            pub fn init(branch_depth: u8, key: *const [key_length]u8, value: T, key_hash: Hash, allocator: std.mem.Allocator) !*LeafNode {
                const new_suffix_len = key_length - branch_depth;
                const allocation = try allocator.allocWithOptions(u8, byte_size(new_suffix_len), @alignOf(LeafNode), null);
                const new = @ptrCast(*LeafNode, allocation);
                new.* = LeafNode{
                                .header = .leaf,
                                .ref_count = 1,
                                .suffix_len = new_suffix_len,
                                .key_hash = key_hash,
                                .value = value };
                const new_suffix = new.suffixSlice();
                const key_start = key_length - new_suffix.len;
                mem.copy(u8, new.suffixSlice(), key[key_start..key_length]);
                return new;
            }

            pub fn format(
                self: LeafNode,
                comptime fmt: []const u8,
                options: std.fmt.FormatOptions,
                writer: anytype,
            ) !void {
                _ = self;
                _ = fmt;
                _ = options;
                try writer.writeAll("LEAF! (TODO)");
            }

            fn raw(self: *LeafNode) []u8 {
                return @ptrCast([*] align(@alignOf(LeafNode)) u8, self)[0..byte_size(self.suffix_len)]; // TODO add alignment
            }

            fn head(self: *LeafNode) *NodeHeader {
                return &self.header;
            }

            pub fn ref(self: *LeafNode, allocator: std.mem.Allocator) allocError!*NodeHeader {
                if(self.ref_count == std.math.maxInt(@TypeOf(self.ref_count))) {
                    // Reference counter exhausted, we need to make a copy of this node.
                    const byte_count = byte_size(self.suffix_len);
                    const allocation = try allocator.allocWithOptions(u8, byte_count, @alignOf(LeafNode), null);
                    const new = @ptrCast(*LeafNode, allocation);
                    mem.copy(u8, new.raw(), self.raw());
                    new.ref_count = 1;
                    return new.head();
                } else {
                    self.ref_count += 1;
                    return self.head();
                }
            }

            pub fn rel(self: *LeafNode, allocator: std.mem.Allocator) void {
                self.ref_count -= 1;
                if(self.ref_count == 0) {
                    defer allocator.free(self.raw());
                }
            }

            fn suffixSlice(self: *LeafNode) []u8 {
                const ptr = @intToPtr([*]u8, @ptrToInt(self) + @sizeOf(LeafNode));
                return ptr[0..self.suffix_len];
            }

            pub fn count(self: *LeafNode) u40 {
                _ = self;
                return 1;
            }

            pub fn hash(self: *LeafNode) Hash {
                return self.key_hash;
            }

            pub fn depth(self: *LeafNode) u8 {
                _ = self;
                return key_length;
            }

            pub fn peek(self: *LeafNode, at_depth: u8) ?u8 {
                if (depth < key_length) return self.key[at_depth];
                return null;
            }

            pub fn propose(self: *LeafNode, at_depth: u8, result_set: *ByteBitset) void {
                var set = ByteBitset.initEmpty();
                set.set(self.key[at_depth]);
                result_set.setIntersection(set);
            }

            pub fn get(self: *LeafNode, at_depth: u8, key: u8) ?*NodeHeader {
                const index = at_depth - (key_length - self.suffix_len);
                // The formula used here is different from the one of the inner node as key_length > suffix_length.
                if (at_depth < key_length and self.suffixSlice()[index] == key) return self.head();
                return null;
            }

            pub fn put(self: *LeafNode, at_depth: u8, key: *const [key_length]u8, value: T, single_owner: bool, allocator: std.mem.Allocator) allocError!*NodeHeader {
                _ = single_owner;
                const suffix = self.suffixSlice();
                var branch_depth = at_depth;
                var suffix_index: u8 = at_depth - (key_length - self.suffix_len);
                while (branch_depth < key_length) : ({branch_depth += 1; suffix_index += 1;}) {
                    if (key[branch_depth] != suffix[suffix_index]) break;
                } else {
                    return self.head();
                }


                const branch_node = try InnerNode.init(branch_depth, key, allocator);
                const sibling_node = try LeafNode.init(branch_depth + 1, key, value, keyHash(key), allocator);

                const self_byte_key = suffix[suffix_index];
                const sibling_byte_key = key[branch_depth];

                _ = try branch_node.cuckooPut(self_byte_key, self.head(), allocator); // We know that these can't fail and won't reallocate.
                _ = try branch_node.cuckooPut(sibling_byte_key, sibling_node.head(), allocator);
                branch_node.child_sum_hash = Hash.xor(self.hash(), sibling_node.hash());
                branch_node.leaf_count = 2;
                branch_node.segment_count = 2;

                return branch_node.head();
            }
        };

        const Tree = struct {
            child: ?*NodeHeader = null,
            allocator: std.mem.Allocator,

            pub fn init(allocator: std.mem.Allocator) Tree {
                return Tree{.allocator=allocator};
            }

            pub fn deinit(self: *Tree) void {
                if(self.child) |child| {
                    child.rel(self.allocator);
                }
            }
            
            pub fn fork(self: *Tree) !Tree {
                var own_child = self.child;
                if(own_child) |*c| {
                    c.* = try c.*.ref(self.allocator);
                }
                return Tree{.child = own_child, .allocator=self.allocator};
            }

            pub fn count(self: *Tree) u40 {
                return if (self.child) |child| child.count()
                else 0;
            }

            pub fn put(self: *Tree, key: *const [key_length]u8, value: T) !void {
                if (self.child) |*child| {
                    //std.debug.print("tree put old: {*}\n", .{child.*});
                    child.* = try child.*.put(0, key, value, true, self.allocator);
                    //std.debug.print("tree put new: {*}\n", .{child.*});
                } else {
                    self.child = (try LeafNode.init(0, key, value, keyHash(key), self.allocator)).head();
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

test "create tree" {
    const key_length = 64;
    const PACT = makePACT(key_length, usize);
    var tree = PACT.Tree.init(std.testing.allocator);
    defer tree.deinit();
}

test "empty tree has count 0" {
    const key_length = 64;
    const PACT = makePACT(key_length, usize);
    var tree = PACT.Tree.init(std.testing.allocator);
    defer tree.deinit();

    try expectEqual(tree.count(), 0);
}

test "single item tree has count 1" {
    const key_length = 64;
    const PACT = makePACT(key_length, usize);
    var tree = PACT.Tree.init(std.testing.allocator);
    defer tree.deinit();

    const key:[key_length]u8 = [_]u8{0} ** key_length;
    try tree.put(&key, 42);

    try expectEqual(tree.count(), 1);
}

test "immutable tree fork" {
    const key_length = 64;
    const PACT = makePACT(key_length, usize);
    var tree = PACT.Tree.init(std.testing.allocator);
    defer tree.deinit();

    var new_tree = try tree.fork();
    defer new_tree.deinit();

    const key:[key_length]u8 = [_]u8{0} ** key_length;
    try new_tree.put(&key, 42);

    try expectEqual(tree.count(), 0);
    try expectEqual(new_tree.count(), 1);
}

test "multi item tree has correct count" {
    const total_runs = 10;

    var rnd = std.rand.DefaultPrng.init(0).random();
    
    const key_length = 64;
    const PACT = makePACT(key_length, usize);
    var tree = PACT.Tree.init(std.testing.allocator);
    defer tree.deinit();

    var key:[key_length]u8 = undefined;

    var i: u40 = 0;
    while(i < total_runs) : (i += 1) {
        
        try expectEqual(tree.count(), i);

        rnd.bytes(&key);
        try tree.put(&key, rnd.int(usize));
        //std.debug.print("Inserted {d} of {d}:{any}\n{s}\n", .{i+1, total_runs, key, tree.child.?.toNode()});
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
    const total_runs: usize = 10000000;

    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = general_purpose_allocator.allocator();

    var timer = try time.Timer.start();
    var t_total: u64 = 0;

    var rnd = std.rand.DefaultPrng.init(0).random();
    
    const key_length = 64;
    const PACT = makePACT(key_length, usize);
    var tree = PACT.Tree.init(gpa);
    defer tree.deinit();

    var key:[key_length]u8 = undefined;

    var i: u40 = 0;
    while(i < total_runs) : (i += 1) {
        rnd.bytes(&key);
        const value = rnd.int(usize);

        timer.reset();
        
        try tree.put(&key, value);

        t_total += timer.lap();
    }

    std.debug.print("Inserted {d} in {d}ns\n", .{total_runs, t_total});
}

test "benchmark std" {
    const total_runs: usize = 100000;

    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = general_purpose_allocator.allocator();

    var timer = try time.Timer.start();
    var t_total: u64 = 0;

    var rnd = std.rand.DefaultPrng.init(0).random();
    
    const key_length = 64;

    var key:[key_length]u8 = undefined;

    var map = std.hash_map.AutoHashMap([key_length]u8, usize).init(gpa);
    defer map.deinit();

    var i: u40 = 0;
    while(i < total_runs) : (i += 1) {
        rnd.bytes(&key);
        const value = rnd.int(usize);

        timer.reset();
        
        try map.put(key, value);

        t_total += timer.lap();
    }

    std.debug.print("Inserted {d} in {d}ns\n", .{total_runs, t_total});
}

