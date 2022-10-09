const std = @import("std");
const blaked25519 = @import("./deps/zig-ed25519-blake2b/ed25519-blake2b.zig").Ed25519Blake2b;
const Trible = @import("./Trible.zig").Trible;
const TribleSet = @import("./TribleSet.zig").TribleSet;

pub const KeyPair = blaked25519.KeyPair;

//      16 byte                 32 byte
//         │                       │
// ┌──────────────┐┌──────────────────────────────┐
// ┌──────────────┐┌──────────────────────────────┐┌──────────────┐
// │     zero     ││          public key          ││  signature   │
// └──────────────┘└──────────────────────────────┘└──────────────┘
//                                                 └──────────────┘
//                         ┌───────────────────────────────┘
//                      64 byte                         16 byte
//                         │                               │
// ┌──────────────────────────────────────────────┐┌──────────────┐
// ┌──────────────────────────────────────────────┐┌──────────────┐
// │                  signature                   ││  commit id   │
// └──────────────────────────────────────────────┘└──────────────┘
//
//                              64 byte
//                                 │
// ┌──────────────────────────────────────────────────────────────┐
// ┌──────────────┬┬──────────────┬┬──────────────────────────────┐*
// │    entity    ││  attribute   ││            value             │
// └──────────────┴┴──────────────┴┴──────────────────────────────┘
//                                 │
//                              trible

pub const Commit = struct {
    data: []u8,

    pub const header_size = 128;
    /// This limit enforces compatibility with UDP, DDS, WebRTC and friends.
    /// Since the entire datamodel is build on calm consistency there is no
    /// real need for large "transactions" except for metadata austerity.
    pub const max_trible_count = 1020;

    pub const TribleIterator = struct {
        commit: Commit,
        offset: usize,

        pub fn init(commit: Commit) TribleIterator {
            return TribleIterator{.commit=commit, .offset=header_size};
        }

        pub fn next(self: *TribleIterator) ?*Trible {
            if(self.offset >= self.commit.data.len) return null;

            const t = std.mem.bytesAsValue(Trible, self.commit.data[self.offset..][0..Trible.size]);
            self.offset += Trible.size;
            return t;
        }
    };

    pub fn initFromTribles(key_pair: KeyPair, commit_id: [16]u8, trible_set: *TribleSet, allocator: std.mem.Allocator) !Commit {
        const trible_count = trible_set.eav.count();
        if(trible_count == 0) return error.EmptyCommit;
        const size = header_size + (trible_count * Trible.size);

        const allocation = try allocator.allocAdvanced(u8, Trible.size, size, .exact);
        const self = Commit{.data = allocation};

        std.mem.copy(u8, self.data[16..48], key_pair.public_key[0..]);
        std.mem.copy(u8, self.data[112..128], commit_id[0..]);

        var iter = trible_set.eav.cursor().iterate();

        var i: usize = 0;
        while(iter.next()) |trible_data| {
            std.mem.copy(u8, self.data[128 + (i * Trible.size)..128 + ((i+1) * Trible.size)], trible_data[0..]);
            i += 1;
        }

        const msg = self.data[112..];
        const sig = try blaked25519.sign(msg, key_pair, null);
        std.mem.copy(u8, self.data[48..112], sig[0..]);

        return self;
    }

    pub fn fromBytes(data: []u8) !Commit {
        if((data.len < header_size + Trible.size) or (data.len % Trible.size != 0)) return error.BadCommitSize;
        if(!std.mem.allEqual(u8, data[0..16], 0)) return error.BadCommitFraming;
        var i: usize = Trible.size;
        while(i < data.len):(i += Trible.size){
            if(std.mem.allEqual(u8, data[i..i+Trible.size], 0)) return error.MultipleCommitFrames;

        }

        return Commit{.data = data};
    }

    pub fn deinit(self: Commit, allocator: std.mem.Allocator) !void {
        allocator.free(self.data);
    }

    pub fn pubkey(self: Commit) [32]u8 {
        return self.data[16..48];
    }

    pub fn signature(self: Commit) [64]u8 {
        return self.data[48..112];
    }

    pub fn commitId(self: Commit) [32]u8 {
        return self.data[112..128];
    }

    pub fn verify(self: Commit) !void {
        const msg = self.data[112..];
        blaked25519.verify(self.signature(), msg, self.pubkey());
    }

    pub fn iterate(self: Commit) TribleIterator {
        return TribleIterator.init(self);
    }

    pub fn toTriblesetSet(self: Commit, allocator: std.mem.Allocator) !*TribleSet {
        var iter = self.iterate();
        var set = try TribleSet.init(allocator);

        while(iter.next()) |trible| {
            try set.put(trible);
        }
        
        return set;
    }
};


// const CommitIterator = struct {
//     pub fn next(self: *NodeIterator) ?IterationResult {
//     }
// };

// fn commits(data: []u8) CommitIterator {
//     var iterator = CommitIterator{};
//     return iterator;
// }
