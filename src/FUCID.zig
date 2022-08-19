const std = @import("std");

var next_id:[32]u8 = undefined;

pub fn init(rnd: std.rand.Random) void {
    rnd.bytes(&next_id);
}

pub const FUCID = extern struct {
    data: [32]u8,

    pub fn init() FUCID {
        const id = next_id;

        var i: usize = 32;
        while(0 < i and id[i-1] == 255):(i -= 1) {
            next_id[i-1] = 0;
        }
        if(i == 0) i = 32;

        next_id[i-1] += 1;

        return FUCID{.data = next_id};
    }

    pub fn decode(data: *const [32]u8) FUCID {
        return FUCID{.data = data.*};
    }

    pub fn encode(self: *const FUCID) [32]u8 {
        return self.data;
    }   
};