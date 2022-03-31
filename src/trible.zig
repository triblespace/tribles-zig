const std = @import("std");

const UFOID = @import("ufoid.zig").UFOID;

pub const Trible = extern struct {
    pub const size = 64;
    data: [size]u8,

    pub fn initAribitrary(rnd: std.rand.Random) Trible {
        const e = UFOID.initNow(rnd);
        const a = UFOID.initNow(rnd);
        const v = UFOID.initNow(rnd);

        var t = Trible{.data = undefined};
        std.mem.copy(u8, t.data[0..16], e.encode()[16..32]);
        std.mem.copy(u8, t.data[16..32], a.encode()[16..32]);
        std.mem.copy(u8, t.data[32..64], v.encode()[0..32]);

        return t;
    }

    pub fn initWithData(data: [size]u8) Trible {
        return Trible{.data = data};
    }
};