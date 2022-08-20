const std = @import("std");
const PACT = @import("./PACT.zig");
const Trible = @import("./Trible.zig").Trible;
const mem = std.mem;
const allocError = std.mem.Allocator.Error;


pub fn isZeroes(bytes: [16]u8) bool {
    for (bytes) |byte| {
      if (byte != 0) return false;
    }
    return true;
}

pub const TribleSet = struct {
    eav: PACT.Tree,
    eva: PACT.Tree,
    aev: PACT.Tree,
    ave: PACT.Tree,
    vea: PACT.Tree,
    vae: PACT.Tree,

    pub fn init(allocator: std.mem.Allocator) TribleSet {
        return TribleSet{
            .eav = PACT.Tree.init(allocator),
            .eva = PACT.Tree.init(allocator),
            .aev = PACT.Tree.init(allocator),
            .ave = PACT.Tree.init(allocator),
            .vea = PACT.Tree.init(allocator),
            .vae = PACT.Tree.init(allocator),
        };
    }

    pub fn deinit(self: *TribleSet) void {
        self.eav.deinit();
        self.eva.deinit();
        self.aev.deinit();
        self.ave.deinit();
        self.vea.deinit();
        self.vae.deinit();
    }

    pub fn fork(self: *TribleSet) allocError!TribleSet {
        return TribleSet{
            .eav = self.eav.fork(),
            .eva = self.eva.fork(),
            .aev = self.aev.fork(),
            .ave = self.ave.fork(),
            .vea = self.vea.fork(),
            .vae = self.vae.fork(),
        };
    }

    pub fn count(self: *const TribleSet) u64 {
        return self.eav.count();
    }

    pub fn put(self: *TribleSet, trible: *const Trible) allocError!void {
        try self.eav.put(trible.ordered(.eav), null);
        try self.eva.put(trible.ordered(.eva), null);
        try self.aev.put(trible.ordered(.aev), null);
        try self.ave.put(trible.ordered(.ave), null);
        try self.vea.put(trible.ordered(.vea), null);
        try self.vae.put(trible.ordered(.vae), null);
    }
};
