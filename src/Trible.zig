const std = @import("std");

const UFOID = @import("UFOID.zig").UFOID;

pub const Trible = extern struct {
    pub const e_len = 16;
    pub const a_len = 16;
    pub const v_len = 32;

    pub const size = e_len + a_len + v_len;
    data: [size]u8,

    const Ordering = enum { eav, eva, aev, ave, vea, vae };

    pub fn initAribitrary(rnd: std.rand.Random) Trible {
        const e_value = UFOID.initNow(rnd);
        const a_value = UFOID.initNow(rnd);
        const v_value = UFOID.initNow(rnd);

        var t = Trible{ .data = undefined };
        std.mem.copy(u8, t.data[0..16], e_value.encode()[16..32]);
        std.mem.copy(u8, t.data[16..32], a_value.encode()[16..32]);
        std.mem.copy(u8, t.data[32..64], v_value.encode()[0..32]);

        return t;
    }

    pub fn initAribitraryLike(rnd: std.rand.Random, change_prob: f32, other: Trible) Trible {
        var t = other;

        const v_value = UFOID.initNow(rnd);
        std.mem.copy(u8, t.data[32..64], v_value.encode()[0..32]);

        if (rnd.floatNorm(f32) < change_prob) {
            const a_value = UFOID.initNow(rnd);
            std.mem.copy(u8, t.data[16..32], a_value.encode()[16..32]);
            
            if (rnd.floatNorm(f32) < change_prob) {
                const e_value = UFOID.initNow(rnd);
                std.mem.copy(u8, t.data[0..16], e_value.encode()[16..32]);
            }
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
        try writer.print("{s:_>32}|{s:_>32}|{s:_>64}", .{ std.fmt.fmtSliceHexUpper(self.data[0..16]), std.fmt.fmtSliceHexUpper(self.data[16..32]), std.fmt.fmtSliceHexUpper(self.data[32..64]) });
    }

    pub fn initWithData(data: [size]u8) Trible {
        return Trible{ .data = data };
    }

    pub fn e(self: Trible) [16]u8 {
        return self.data[0..16].*;
    }

    pub fn a(self: Trible) [16]u8 {
        return self.data[16..32].*;
    }

    pub fn v(self: Trible) [32]u8 {
        return self.data[32..64].*;
    }

    pub fn v1(self: Trible) [16]u8 {
        return self.data[32..48].*;
    }

    pub fn v2(self: Trible) [16]u8 {
        return self.data[48..64].*;
    }

    pub fn ordered(self: Trible, comptime order: Ordering) [size]u8 {
        var reordered: [size]u8 = undefined;

        const e_data = self.data[0..16];
        const a_data = self.data[16..32];
        const v_data = self.data[32..64];

        switch (order) {
            .eav => {
                std.mem.copy(u8, reordered[0..16], e_data);
                std.mem.copy(u8, reordered[16..32], a_data);
                std.mem.copy(u8, reordered[32..64], v_data);
            },
            .eva => {
                std.mem.copy(u8, reordered[0..16], e_data);
                std.mem.copy(u8, reordered[16..48], v_data);
                std.mem.copy(u8, reordered[48..64], a_data);
            },
            .aev => {
                std.mem.copy(u8, reordered[0..16], a_data);
                std.mem.copy(u8, reordered[16..32], e_data);
                std.mem.copy(u8, reordered[32..64], v_data);
            },
            .ave => {
                std.mem.copy(u8, reordered[0..16], a_data);
                std.mem.copy(u8, reordered[16..48], v_data);
                std.mem.copy(u8, reordered[48..64], e_data);
            },
            .vea => {
                std.mem.copy(u8, reordered[0..32], v_data);
                std.mem.copy(u8, reordered[32..48], e_data);
                std.mem.copy(u8, reordered[48..64], a_data);
            },
            .vae => {
                std.mem.copy(u8, reordered[0..32], v_data);
                std.mem.copy(u8, reordered[32..48], a_data);
                std.mem.copy(u8, reordered[48..64], e_data);
            },
        }

        return reordered;
    }
};
