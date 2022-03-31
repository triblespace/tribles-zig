const std = @import("std");


pub const UFOID = extern struct {
    data: [32]u8,

    pub fn initNow(rnd: std.rand.Random) UFOID {
        return initWithTime(std.time.milliTimestamp(), rnd);
    }

    const zero_prefix = [_]u8{0}**16;
    
    pub fn initWithTime(timestamp_ms: i64, rnd: std.rand.Random) UFOID {
        var timestamp_bytes: [8]u8 = undefined;
        std.mem.writeIntBig(i64, &timestamp_bytes, timestamp_ms);

        var id = UFOID{.data = undefined};
        std.mem.copy(u8, id.data[0..16], &zero_prefix);
        std.mem.copy(u8, id.data[16..20], timestamp_bytes[4..8]);
        rnd.bytes(id.data[20..32]);
        
        return id;
    }

    pub fn decode(data: *const [32]u8) UFOID {
        return UFOID{.data = data.*};
    }

    pub fn encode(self: *const UFOID) [32]u8 {
        return self.data;
    }   
};