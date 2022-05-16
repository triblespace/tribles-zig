pub const MemInfo = struct {
    active_memory: u64 = 0,
    wasted_memory: u64 = 0,
    passive_memory: u64 = 0,
    allocation_count: u64 = 0,

    pub fn combine(self: MemInfo, other: MemInfo) MemInfo {
        return MemInfo{ .active_memory = self.active_memory + other.active_memory, .wasted_memory = self.wasted_memory + other.wasted_memory, .passive_memory = self.passive_memory + other.passive_memory, .allocation_count = self.allocation_count + other.allocation_count };
    }
};
