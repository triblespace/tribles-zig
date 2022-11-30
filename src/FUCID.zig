const std = @import("std");

var counter: u64 = 0;
var randomness:[32]u8 = undefined;

pub fn init(rnd: std.rand.Random) void {
    rnd.bytes(&next_id);
}

pub const FUCID = extern struct {
    data: [32]u8,

    pub fn init() FUCID {
        const next_id = randomness;
        var counter_bytes: [8]u8 = undefined;
        std.mem.writeIntBig(i64, &counter_bytes, counter);

        for(counter_bytes) |b, i| {
            next_id[24 + i] ^= b;
        }
        
        return FUCID{.data = next_id};
    }

    pub fn decode(data: *const [32]u8) FUCID {
        return FUCID{.data = data.*};
    }

    pub fn encode(self: *const FUCID) [32]u8 {
        return self.data;
    }   
};