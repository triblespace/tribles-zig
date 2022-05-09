const std = @import("std");

// Uninitialized memory initialized by init()
var instance_secret: [32]u8 = undefined;

pub fn init() void {
    // XXX: (crest) Should this be a deterministic pseudo-RNG seeded by a constant for reproducable tests?
    std.crypto.random.bytes(&instance_secret);
}

pub const Hash = extern struct {
    data: @Vector(4, u32) = @Vector(4, u32){ 0, 0, 0, 0 },

    pub fn init(key: []const u8) Hash {
        const siphash_strong = comptime std.hash.SipHash128(2, 4);

        var hash: Hash = undefined;

        siphash_strong.create(std.mem.asBytes(&hash.data), key, instance_secret[0..16]);

        return hash;
    }

    pub fn update(self: *Hash, old: Hash, new: Hash) Hash {
        return Hash{.data = (self.data ^ old.data) ^ new.data};
    }

    pub fn combine(left: Hash, right: Hash) Hash {
        return Hash{.data = left.data ^ right.data};
    }

    pub fn equal(left: Hash, right: Hash) bool {
        return std.meta.eql(left.data, right.data);
    }
};

pub const MinHash = extern struct {
    data: @Vector(8, u32) = @Vector(8, u32){ 0, 0, 0, 0, 0, 0, 0, 0 },

    pub fn init(key: []const u8) MinHash {
        const siphash_fast = comptime std.hash.SipHash128(1, 3);

        var min_hash: MinHash = undefined;

        siphash_fast.create(std.mem.asBytes(&min_hash.data)[0..16], key, instance_secret[0..16]);
        siphash_fast.create(std.mem.asBytes(&min_hash.data)[16..32], key, instance_secret[16..32]);

        return min_hash;
    }

    pub fn combine(left: MinHash, right: MinHash) MinHash {
        return MinHash{.data = @maximum(left.data, right.data)};
    }

    pub fn equal(left: Hash, right: Hash) bool {
        return left.hash.equal(right.hash);
    }
};
