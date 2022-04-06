const std = @import("std");

const UFOID = @import("UFOID.zig").UFOID;

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

    pub fn initAribitraryLike(rnd: std.rand.Random, change_prob: f32, other: Trible) Trible {
        var t = other;
        if(rnd.floatNorm(f32) < change_prob) {
            const e = UFOID.initNow(rnd);
            std.mem.copy(u8, t.data[0..16], e.encode()[16..32]);
        } 
        if(rnd.floatNorm(f32) < change_prob) {
            const a = UFOID.initNow(rnd);
            std.mem.copy(u8, t.data[16..32], a.encode()[16..32]);
        }
        if(rnd.floatNorm(f32) < change_prob) {
            const v = UFOID.initNow(rnd);
            std.mem.copy(u8, t.data[32..64], v.encode()[0..32]);
        }

        return t;
    }

    pub fn format(
        self: Trible,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = self;
        _ = fmt;
        _ = options;
        try writer.print("{32s:_>[0]}|{32s:_>[1]}|{64s:_>[2]}", .{
            std.fmt.fmtSliceHexUpper(&self.data[0..16]),
            std.fmt.fmtSliceHexUpper(&self.data[16..32]),
            std.fmt.fmtSliceHexUpper(&self.data[32..64])});
    }

    pub fn initWithData(data: [size]u8) Trible {
        return Trible{.data = data};
    }
};