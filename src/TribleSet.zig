const std = @import("std");
const PACT = @import("./PACT.zig").PACT;
const MemInfo = @import("./MemInfo.zig").MemInfo;
const Trible = @import("./Trible.zig").Trible;
const mem = std.mem;
const allocError = std.mem.Allocator.Error;


const EAVIndex = PACT(&[_]u8{16, 16, 32}, u8).Tree;
const EVAIndex = PACT(&[_]u8{16, 32, 16}, u8).Tree;
const AEVIndex = PACT(&[_]u8{16, 16, 32}, u8).Tree;
const AVEIndex = PACT(&[_]u8{16, 32, 16}, u8).Tree;
const VEAIndex = PACT(&[_]u8{32, 16, 16}, u8).Tree;
const VAEIndex = PACT(&[_]u8{32, 16, 16}, u8).Tree;

pub const TribleSet = struct {
    eav: EAVIndex,
    eva: EVAIndex,
    aev: AEVIndex,
    ave: AVEIndex,
    vea: VEAIndex,
    vae: VAEIndex,

    const TribleConstraint = struct {
        const State = enum {
            stack_empty,
            stack_e, stack_a, stack_v,
            stack_ea, stack_ev, stack_ae, stack_av, stack_ve, stack_va,
            stack_eav, stack_eva, stack_aev, stack_ave, stack_vea, stack_vae,
        }

        eVar: u8,
        aVar: u8,
        vVar: u8,
        state: State,

        eavCursor: PaddedCursor(EAVIndex.Cursor, 32),
        evaCursor: PaddedCursor(EVAIndex.Cursor, 32),
        aevCursor: PaddedCursor(AEVIndex.Cursor, 32),
        aveCursor: PaddedCursor(AVEIndex.Cursor, 32),
        veaCursor: PaddedCursor(VEAIndex.Cursor, 32),
        vaeCursor: PaddedCursor(VAEIndex.Cursor, 32),

        pub fn peekByte(self: *@This()) ?u8 {
            return switch(self.state) {
                .stack_empty => unreachable,

                .stack_e => eavCursor.peek(),
                .stack_a => aevCursor.peek(),
                .stack_v => veaCursor.peek(),

                .stack_ea => eavCursor.peek(),
                .stack_ev => evaCursor.peek(),
                .stack_ae => aevCursor.peek(),
                .stack_av => aveCursor.peek(),
                .stack_ve => veaCursor.peek(),
                .stack_va => vaeCursor.peek(),

                .stack_eav => eavCursor.peek(),
                .stack_eva => evaCursor.peek(),
                .stack_aev => aevCursor.peek(),
                .stack_ave => aveCursor.peek(),
                .stack_vea => veaCursor.peek(),
                .stack_vae => vaeCursor.peek(),
            };
        }

        pub fn proposeByte(self: *@This(), bitset: *ByteBitset) void {
            switch(self.state) {
                .stack_empty => unreachable,

                .stack_e => eavCursor.propose(bitset),
                .stack_a => aevCursor.propose(bitset),
                .stack_v => veaCursor.propose(bitset),

                .stack_ea => eavCursor.propose(bitset),
                .stack_ev => evaCursor.propose(bitset),
                .stack_ae => aevCursor.propose(bitset),
                .stack_av => aveCursor.propose(bitset),
                .stack_ve => veaCursor.propose(bitset),
                .stack_va => vaeCursor.propose(bitset),

                .stack_eav => eavCursor.propose(bitset),
                .stack_eva => evaCursor.propose(bitset),
                .stack_aev => aevCursor.propose(bitset),
                .stack_ave => aveCursor.propose(bitset),
                .stack_vea => veaCursor.propose(bitset),
                .stack_vae => vaeCursor.propose(bitset),
            };
        }

        pub fn pushByte(self: *@This(), key_fragment: u8) void {
            switch(self.state) {
                .stack_empty => unreachable,

                .stack_e => eavCursor.push(key_fragment),
                .stack_a => aevCursor.push(key_fragment),
                .stack_v => veaCursor.push(key_fragment),

                .stack_ea => eavCursor.push(key_fragment),
                .stack_ev => evaCursor.push(key_fragment),
                .stack_ae => aevCursor.push(key_fragment),
                .stack_av => aveCursor.push(key_fragment),
                .stack_ve => veaCursor.push(key_fragment),
                .stack_va => vaeCursor.push(key_fragment),

                .stack_eav => eavCursor.push(key_fragment),
                .stack_eva => evaCursor.push(key_fragment),
                .stack_aev => aevCursor.push(key_fragment),
                .stack_ave => aveCursor.push(key_fragment),
                .stack_vea => veaCursor.push(key_fragment),
                .stack_vae => vaeCursor.push(key_fragment),
            };
        }

        pub fn popByte(self: *@This()) void {
            switch(self.state) {
                .stack_empty => unreachable,

                .stack_e => eavCursor.pop(),
                .stack_a => aevCursor.pop(),
                .stack_v => veaCursor.pop(),

                .stack_ea => eavCursor.pop(),
                .stack_ev => evaCursor.pop(),
                .stack_ae => aevCursor.pop(),
                .stack_av => aveCursor.pop(),
                .stack_ve => veaCursor.pop(),
                .stack_va => vaeCursor.pop(),

                .stack_eav => eavCursor.pop(),
                .stack_eva => evaCursor.pop(),
                .stack_aev => aevCursor.pop(),
                .stack_ave => aveCursor.pop(),
                .stack_vea => veaCursor.pop(),
                .stack_vae => vaeCursor.pop(),
            }
        }

        pub fn proposeVariable(self: *@This(), bitset: *ByteBitset) void {
            bitset.unsetAll();

            switch(self.state) {
                .stack_empty => {
                    bitset.set(self.eVar);
                    bitset.set(self.aVar);
                    bitset.set(self.vVar);
                },

                .stack_e => {
                    bitset.set(self.aVar);
                    bitset.set(self.vVar);
                },
                .stack_a => {
                    bitset.set(self.eVar);
                    bitset.set(self.vVar);
                },
                .stack_v => {
                    bitset.set(self.eVar);
                    bitset.set(self.aVar);
                },

                .stack_ea => bitset.set(self.vVar),
                .stack_ev => bitset.set(self.aVar),
                .stack_ae => bitset.set(self.vVar),
                .stack_av => bitset.set(self.eVar),
                .stack_ve => bitset.set(self.aVar),
                .stack_va => bitset.set(self.eVar),

                else => return,
            }
        }

        pub fn pushVariable(self: *@This(), variable: u8) bool {
            if(self.eVar == variable) {
                self.state = switch(self.state) {
                    .stack_empty => .stack_e,

                    .stack_a => .stack_ae,
                    .stack_v => .stack_ve,

                    .stack_av => .stack_ave,
                    .stack_va => .stack_vae,

                    else => unreachable,
                }
                return;
            }
            if(self.aVar == variable) {
                self.state = switch(self.state) {
                    .stack_empty => .stack_a,

                    .stack_e => .stack_ea,
                    .stack_v => .stack_va,

                    .stack_ev => .stack_eva,
                    .stack_ve => .stack_vea,

                    else => unreachable,
                }
                return;
            }
            if(self.vVar == variable) {
                self.state = switch(self.state) {
                    .stack_empty => .stack_v,

                    .stack_e => .stack_ev,
                    .stack_a => .stack_av,

                    .stack_ea => .stack_eav,
                    .stack_ae => .stack_aev,

                    else => unreachable,
                }
                return;
            }

        }
        
        pub fn popVariable(self: *@This()) bool {
            switch(self.state) {
                .stack_empty => unreachable,

                .stack_e =>   stack_empty,
                .stack_a =>   stack_empty,
                .stack_v =>   stack_empty,

                .stack_ea =>  stack_e,
                .stack_ev =>  stack_e,
                .stack_ae =>  stack_a,
                .stack_av =>  stack_a,
                .stack_ve =>  stack_v,
                .stack_va =>  stack_v,

                .stack_eav => stack_ea,
                .stack_eva => stack_ev,
                .stack_aev => stack_ae,
                .stack_ave => stack_av,
                .stack_vea => stack_ve,
                .stack_vae => stack_va,
            }
        }

        pub fn countVariable(self: *@This(), variable: u8) usize {
            if(self.eVar == variable) {
                self.state = switch(self.state) {
                    .stack_empty => self.eavCursor.segmentCount(),

                    .stack_a => self.aevCursor.segmentCount(),
                    .stack_v => self.veaCursor.segmentCount(),

                    .stack_av => self.aveCursor.segmentCount(),
                    .stack_va => self.vaeCursor.segmentCount(),

                    else => unreachable,
                }
                return;
            }
            if(self.aVar == variable) {
                self.state = switch(self.state) {
                    .stack_empty => self.aevCursor.segmentCount(),

                    .stack_e => self.eavCursor.segmentCount(),
                    .stack_v => self.vaeCursor.segmentCount(),

                    .stack_ev => self.evaCursor.segmentCount(),
                    .stack_ve => self.veaCursor.segmentCount(),

                    else => unreachable,
                }
                return;
            }
            if(self.vVar == variable) {
                self.state = switch(self.state) {
                    .stack_empty => self.veaCursor.segmentCount(),

                    .stack_e => self.evaCursor.segmentCount(),
                    .stack_a => self.aveCursor.segmentCount(),

                    .stack_ea => self.eavCursor.segmentCount(),
                    .stack_ae => self.aevCursor.segmentCount(),

                    else => unreachable,
                }
                return;
            }
        }

        pub fn sampleVariable(self: *@This(), variable: u8) usize {
            if(self.eVar == variable) {
                self.state = switch(self.state) {
                    .stack_empty => self.eavCursor.minhash(),

                    .stack_a => self.aevCursor.minhash(),
                    .stack_v => self.veaCursor.minhash(),

                    .stack_av => self.aveCursor.minhash(),
                    .stack_va => self.vaeCursor.minhash(),

                    else => unreachable,
                }
                return;
            }
            if(self.aVar == variable) {
                self.state = switch(self.state) {
                    .stack_empty => self.aevCursor.minhash(),

                    .stack_e => self.eavCursor.minhash(),
                    .stack_v => self.vaeCursor.minhash(),

                    .stack_ev => self.evaCursor.minhash(),
                    .stack_ve => self.veaCursor.minhash(),

                    else => unreachable,
                }
                return;
            }
            if(self.vVar == variable) {
                self.state = switch(self.state) {
                    .stack_empty => self.veaCursor.minhash(),

                    .stack_e => self.evaCursor.minhash(),
                    .stack_a => self.aveCursor.minhash(),

                    .stack_ea => self.eavCursor.minhash(),
                    .stack_ae => self.aevCursor.minhash(),

                    else => unreachable,
                }
                return;
            }
        }
    };

    pub fn init(allocator: std.mem.Allocator) TribleSet {
        return TribleSet{
            .eav = EAVIndex.init(allocator),
            .eva = EVAIndex.init(allocator),
            .aev = AEVIndex.init(allocator),
            .ave = AVEIndex.init(allocator),
            .vea = VEAIndex.init(allocator),
            .vae = VAEIndex.init(allocator),
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

    pub fn mem_info(self: *TribleSet) MemInfo {
        var total = MemInfo{};

        total = total.combine(self.eav.mem_info());
        total = total.combine(self.eva.mem_info());
        total = total.combine(self.aev.mem_info());
        total = total.combine(self.ave.mem_info());
        total = total.combine(self.vea.mem_info());
        total = total.combine(self.vae.mem_info());

        return total;
    }
};
