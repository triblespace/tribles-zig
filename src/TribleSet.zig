const std = @import("std");
const PACT = @import("./PACT.zig").PACT;
const MemInfo = @import("./MemInfo.zig").MemInfo;
const Trible = @import("./Trible.zig").Trible;
const mem = std.mem;
const allocError = std.mem.Allocator.Error;


pub fn isZeroes(bytes: [16]u8) bool {
    for (bytes) |byte| {
      if (byte != 0) return false;
    }
    return true;
}

const EAVIndex = PACT(&[_]u8{16, 16, 32}, 32, u8).Tree;
const EVAIndex = PACT(&[_]u8{16, 32, 16}, 32, u8).Tree;
const AEVIndex = PACT(&[_]u8{16, 16, 32}, 32, u8).Tree;
const AVEIndex = PACT(&[_]u8{16, 32, 16}, 32, u8).Tree;
const VEAIndex = PACT(&[_]u8{32, 16, 16}, 32, u8).Tree;
const VAEIndex = PACT(&[_]u8{32, 16, 16}, 32, u8).Tree;
const EisAIndex = PACT(&[_]u8{16, 16, 32}, 32, u8).Tree; // Same order as EAV
const EisVIndex = PACT(&[_]u8{16, 16, 32}, 32, u8).Tree; // Same order as EAV
const AisVIndex = PACT(&[_]u8{16, 16, 32}, 32, u8).Tree; // Same order as AEV

pub const TribleSet = struct {
    eav: EAVIndex,
    eva: EVAIndex,
    aev: AEVIndex,
    ave: AVEIndex,
    vea: VEAIndex,
    vae: VAEIndex,
    eIsA: EisAIndex,
    eIsV: EisVIndex,
    aIsV: AisVIndex,

    pub fn init(allocator: std.mem.Allocator) TribleSet {
        return TribleSet{
            .eav = EAVIndex.init(allocator),
            .eva = EVAIndex.init(allocator),
            .aev = AEVIndex.init(allocator),
            .ave = AVEIndex.init(allocator),
            .vea = VEAIndex.init(allocator),
            .vae = VAEIndex.init(allocator),
            .eIsA = EisAIndex.init(allocator),
            .eIsV = EisVIndex.init(allocator),
            .aIsV = AisVIndex.init(allocator),
        };
    }

    pub fn deinit(self: *TribleSet) void {
        self.eav.deinit();
        self.eva.deinit();
        self.aev.deinit();
        self.ave.deinit();
        self.vea.deinit();
        self.vae.deinit();
        self.eIsA.deinit();
        self.eIsV.deinit();
        self.aIsV.deinit();
    }

    pub fn fork(self: *TribleSet) allocError!TribleSet {
        return TribleSet{
            .eav = self.eav.fork(),
            .eva = self.eva.fork(),
            .aev = self.aev.fork(),
            .ave = self.ave.fork(),
            .vea = self.vea.fork(),
            .vae = self.vae.fork(),
            .eIsA = self.eIsA.fork(),
            .eIsV = self.eIsV.fork(),
            .aIsV = self.aIsV.fork(),
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

        const e_data = trible.e();
        const a_data = trible.a();
        const v1_data = trible.v1();
        const v2_data = trible.v2();
        const eIsA = std.mem.eql(u8, &e_data, &a_data);
        const eIsV = isZeroes(v1_data) and std.mem.eql(u8, &e_data, &v2_data);
        const aIsV = isZeroes(v1_data) and std.mem.eql(u8, &a_data, &v2_data);

        if (eIsA) {
          try self.eIsA.put(trible.ordered(.eav), null);
        }
        if (eIsV) {
          try self.eIsV.put(trible.ordered(.eav), null);
        }
        if (aIsV) {
          try self.aIsV.put(trible.ordered(.aev), null);
        }
    }

    pub fn mem_info(self: *TribleSet) MemInfo {
        var total = MemInfo{};

        total = total.combine(self.eav.mem_info());
        total = total.combine(self.eva.mem_info());
        total = total.combine(self.aev.mem_info());
        total = total.combine(self.ave.mem_info());
        total = total.combine(self.vea.mem_info());
        total = total.combine(self.vae.mem_info());
        total = total.combine(self.eIsA.mem_info());
        total = total.combine(self.eIsV.mem_info());
        total = total.combine(self.aIsV.mem_info());

        return total;
    }
};
