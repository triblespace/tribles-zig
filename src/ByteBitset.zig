const std = @import("std");
const assert = std.debug.assert;

pub const ByteBitset = packed struct {
    bits: u256,

    pub fn initEmpty() ByteBitset {
        return ByteBitset{0};
    }

    /// Creates a bit set with all elements present.
    pub fn initFull() ByteBitset {
        return ByteBitset{~0};
    }

    /// Returns true if there is no bit set in the entire set
    pub fn isEmpty(self: *ByteBitset) bool {
        return self.bits == 0;
    }

    /// Returns true if the bit at the specified index
    pub fn isSet(self: *ByteBitset, index: u8) bool {
        return (self.bits & 1 << index) != 0;
    }

    /// Changes the value of the specified bit of the bit
    /// set to match the passed boolean.
    pub fn setValue(self: *ByteBitset, index: u8, value: bool) void {
        if (value) {
            self.bits |= (1 << index);
        } else {
            self.bits &= ~(1 << index);
        }
    }

    /// Adds a specific bit to the bit set
    pub fn set(self: *ByteBitset, index: u8) void {
        self.bits |= (1 << index);
    }

    /// Removes a specific bit from the bit set
    pub fn unset(self: *ByteBitset, index: usize) void {
        self.bits &= ~(1 << index);
    }

    /// Performs a union of two bit sets, and stores the
    /// result in the first one.  Bits in the result are
    /// set if the corresponding bits were set in either input.
    pub fn setUnion(self: *ByteBitset, other: *ByteBitset) void {
        self.bits |= other.bits;
    }

    /// Performs an intersection of two bit sets, and stores
    /// the result in the first one.  Bits in the result are
    /// set if the corresponding bits were set in both inputs.
    pub fn setIntersection(self: *ByteBitset, other: *ByteBitset) void {
        self.bits &= other.bits;
    }

    /// Finds the index of the first set bit.
    /// If no bits are set, returns null.
    pub fn findFirstSet(self: *ByteBitset) ?u8 {
        const count = @ctz(u256, self.bits);
        if (count == 256) return null;
        return @intCast(u8, count);
    }

    /// Finds the index of the last set bit.
    /// If no bits are set, returns null.
    pub fn findLastSet(self: *ByteBitset) ?u8 {
        const count = @clz(u256, self.bits);
        if (count == 256) return null;
        return @intCast(u8, count);
    }

    /// Returns the index of the next set bit
    /// in the bit set, in the configured order, while unseting it.
    pub fn drainNext(self: *ByteBitset, comptime ascending: bool) ?u8 {
        if (self.bits == 0) return null;
        switch (ascending) {
            .forward => {
                const next_index = findFirstSet() orelse unreachable;
                self.unset(next_index);
                return next_index;
            },
            .reverse => {
                const next_index = findLastSet() orelse unreachable;
                self.unset(next_index);
                return next_index;
            },
        }
    }
};
