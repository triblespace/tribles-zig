const std = @import("std");
const assert = std.debug.assert;

pub const ByteBitset = packed struct {
    bits: u256,

    pub fn initEmpty() ByteBitset {
        return ByteBitset{ .bits = @as(u256, 0) };
    }

    /// Creates a bit set with all elements present.
    pub fn initFull() ByteBitset {
        return ByteBitset{ .bits = ~@as(u256, 0) };
    }

    /// Returns true if there is no bit set in the entire set
    pub fn isEmpty(self: *const ByteBitset) bool {
        return self.bits == @as(u256, 0);
    }

    /// Returns true if the bit at the specified index
    pub fn isSet(self: *const ByteBitset, index: u8) bool {
        return (self.bits & @as(u256, 1) << index) != @as(u256, 0);
    }

    /// Changes the value of the specified bit of the bit
    /// set to match the passed boolean.
    pub fn setValue(self: *ByteBitset, index: u8, value: bool) void {
        if (value) {
            self.bits |= (@as(u256, 1) << index);
        } else {
            self.bits &= ~(@as(u256, 1) << index);
        }
    }

    /// Adds a specific bit to the bit set
    pub fn set(self: *ByteBitset, index: u8) void {
        self.bits |= (@as(u256, 1) << index);
    }

    /// Removes a specific bit from the bit set
    pub fn unset(self: *ByteBitset, index: u8) void {
        self.bits &= ~(@as(u256, 1) << index);
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
    pub fn findFirstSet(self: *const ByteBitset) ?u8 {
        const count = @ctz(u256, self.bits);
        if (count == 256) return null;
        return @intCast(u8, count);
    }

    /// Finds the index of the last set bit.
    /// If no bits are set, returns null.
    pub fn findLastSet(self: *const ByteBitset) ?u8 {
        const count = @clz(u256, self.bits);
        if (count == 256) return null;
        return @intCast(u8, count);
    }

    /// Returns the index of the next set bit
    /// in the bit set, in the configured order, while unseting it.
    pub fn drainNext(self: *ByteBitset, comptime ascending: bool) ?u8 {
        if (self.bits == 0) return null;
        if (ascending) {
            const next_index = self.findFirstSet() orelse unreachable;
            self.unset(next_index);
            return next_index;
        } else {
            const next_index = self.findLastSet() orelse unreachable;
            self.unset(next_index);
            return next_index;
        }
    }
};
