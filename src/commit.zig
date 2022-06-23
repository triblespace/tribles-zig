const std = @import("std");
const blaked25519 = @import("../deps/zig-ed25519-blake2b/d25519-blake2b.zig");
const Trible = @import("./Trible.zig").Trible;

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

const header_size = 128;

const Commit = struct {
    data: []u8,

    const TribleIterator = struct {
        commit: *Commit;
        offset: usize;
        pub fn init(commit: *Commit) TribleIterator {
            return TribleIterator{.commit=commit, .offset=header_size};
        }

        pub fn next(self: *TribleIterator) ?*Trible {
            if(self.offset >= commit.data.len) return null;

            const t: *Trible = std.mem.bytesAsValue(Trible, allocation[offset, offset + Trible.size]);
            self.offset += Trible.size;
            return t;
        }
    };

    fn initFromTribles(key_pair: blaked25519.KeyPair, commit_id: [16]u8, trible_set: TribleSet, allocator: std.mem.Allocator) !Commit {
        const trible_count = trible_set.eav.count();
        if(trible_count == 0) return error.EmptyCommit;
        const size = header_size + (trible_count * Trible.size);

        const allocation = try allocator.allocAdvanced(u8, Trible.size, size, .exact);
        const self = Commit{.data = allocation};

        std.mem.copy(u8, data[16..48], key_pair.public_key);
        std.mem.copy(u8, data[112..128], commit_id);

        const tribles = trible_set.eav.cursor().iterator();
        for(tribles) |trible, i| {
            std.mem.copy(u8, self.data[128 + (i * Trible.size)..128 + ((i+1) * Trible.size)], trible);
        }

        const msg = self.data[112..];
        const signature = try blaked25519.sign(msg, key_pair, null);
        std.mem.copy(u8, data[48..112], signature);

        return self;
    }

    fn fromBytes(data: []u8) !Commit {
        if((data.len < header_size + Trible.size) or (data.len % Trible.size != 0)) return error.BadCommitSize
        if(!std.mem.allEqual(u8, data[0..16], 0)) return error.BadCommitFraming;
        var i = Trible.size;
        while(i < data.len):(i += Trible.size){
            if(std.mem.allEqual(u8, data[i..i+Trible.size], 0)) return error.MultipleCommitFrames;

        }

        return Commit{.data = data};
    }

    fn deinit(self: Commit, allocator: std.mem.Allocator) !void {
        allocator.free(self.data);
    }

    fn pubkey(self: Commit) [32]u8 {
        return self.data[16..48];
    }

    fn signature(self: Commit) [64]u8 {
        return self.data[48..112];
    }

    fn commitId(self: Commit) [32]u8 {
        return self.data[112..128];
    }

    fn verify(self: Commit) !void {
        const msg = self.data[112..];
        blaked25519.verify(self.signature(), msg, self.pubkey());
    }

    fn tribles(self: Commit) TribleIterator {
        return TribleIterator.init(self, offset);
    }
}

// const CommitIterator = struct {
//     pub fn next(self: *NodeIterator) ?IterationResult {
//     }
// };

// fn commits(data: []u8) CommitIterator {
//     var iterator = CommitIterator{};
//     return iterator;
// }
