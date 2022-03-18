const std = @import("std");
const assert = std.debug.assert;

pub const ByteBitset = packed struct {
    bits: [4]u64 = .{ 0, 0, 0, 0 },

    pub fn initEmpty() ByteBitset {
        return ByteBitset{ .bits = .{ 0, 0, 0, 0 } };
    }

    /// Creates a bit set with all elements present.
    pub fn initFull() ByteBitset {
        return ByteBitset{ .bits = .{ ~0, ~0, ~0, ~0 } };
    }

    pub fn isEmpty(self: *const ByteBitset) bool {
        return self.bits[0] == 0 and self.bits[1] == 0 and self.bits[2] == 0 and self.bits[3] == 0;
    }

    pub fn set(self: *ByteBitset, index: u8) void {
        self.bits[index >> 6] |= 1 << (index & 0b00111111);
    }

    pub fn unset(self: *ByteBitset, index: u8) void {
        self.bits[index >> 6] &= ~(1 << (index & 0b00111111));
    }

    pub fn setValue(self: *ByteBitset, index: u8, value: bool) void {
        if (value) {
            self.set(index);
        } else {
            self.unset(index);
        }
    }

    pub fn setAll(self: *ByteBitset) void {
        self.bits[0] = ~0;
        self.bits[1] = ~0;
        self.bits[2] = ~0;
        self.bits[3] = ~0;
    }

    pub fn unsetAll(self: *ByteBitset) void {
        self.bits[0] = 0;
        self.bits[1] = 0;
        self.bits[2] = 0;
        self.bits[3] = 0;
    }

    pub fn isSet(self: *const ByteBitset, index: u8) bool {
        return (self.bits[index >> 6] & (1 << (index & 0b00111111))) != 0;
    }

    /// Finds the index of the first set bit.
    /// If no bits are set, returns null.
    pub fn findFirstSet(self: *const ByteBitset) ?u8 {
        if (self.bits[0] != 0) return @ctz(u64, self.bits[0]);
        if (self.bits[1] != 0) return (1 << 6) + @ctz(u64, self.bits[1]);
        if (self.bits[2] != 0) return (2 << 6) + @ctz(u64, self.bits[2]);
        if (self.bits[3] != 0) return (3 << 6) + @ctz(u64, self.bits[3]);
        return null;
    }

    /// Finds the index of the last set bit.
    /// If no bits are set, returns null.
    pub fn findLastSet(self: *const ByteBitset) ?u8 {
        if (self.bits[3] != 0) return (3 << 6) + @clz(u64, self.bits[3]);
        if (self.bits[2] != 0) return (2 << 6) + @clz(u64, self.bits[2]);
        if (self.bits[1] != 0) return (1 << 6) + @clz(u64, self.bits[1]);
        if (self.bits[0] != 0) return @clz(u64, self.bits[0]);
        return null;
    }

    /// Returns the index of the next set bit
    /// in the bit set, in the configured order, while unseting it.
    pub fn drainNext(self: *ByteBitset, comptime ascending: bool) ?u8 {
        if (self.isEmpty()) return null;
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

    pub fn isSupersetOf(left: *ByteBitset, right: *ByteBitset) bool {
        return ((left.bits[0] & right.bits[0]) ^ right.bits[0]) == 0 and
            ((left.bits[1] & right.bits[1]) ^ right.bits[1]) == 0 and
            ((left.bits[2] & right.bits[2]) ^ right.bits[2]) == 0 and
            ((left.bits[3] & right.bits[3]) ^ right.bits[3]) == 0;
    }

    pub fn isSubsetOf(left: *ByteBitset, right: *ByteBitset) bool {
        return ((left.bits[0] & right.bits[0]) ^ left.bits[0]) == 0 and
            ((left.bits[1] & right.bits[1]) ^ left.bits[1]) == 0 and
            ((left.bits[2] & right.bits[2]) ^ left.bits[2]) == 0 and
            ((left.bits[3] & right.bits[3]) ^ left.bits[3]) == 0;
    }

    pub fn setIntersect(self: *ByteBitset, left: *ByteBitset, right: *ByteBitset) void {
        self.bits[0] = left.bits[0] & right.bits[0];
        self.bits[1] = left.bits[1] & right.bits[1];
        self.bits[2] = left.bits[2] & right.bits[2];
        self.bits[3] = left.bits[3] & right.bits[3];
    }

    pub fn setUnion(self: *ByteBitset, left: *ByteBitset, right: *ByteBitset) void {
        self.bits[0] = left.bits[0] | right.bits[0];
        self.bits[1] = left.bits[1] | right.bits[1];
        self.bits[2] = left.bits[2] | right.bits[2];
        self.bits[3] = left.bits[3] | right.bits[3];
    }

    pub fn setSubtract(self: *ByteBitset, left: *ByteBitset, right: *ByteBitset) void {
        self.bits[0] = left.bits[0] & ~right.bits[0];
        self.bits[1] = left.bits[1] & ~right.bits[1];
        self.bits[2] = left.bits[2] & ~right.bits[2];
        self.bits[3] = left.bits[3] & ~right.bits[3];
    }

    pub fn setDiff(self: *ByteBitset, left: *ByteBitset, right: *ByteBitset) void {
        self.bits[0] = left.bits[0] ^ right.bits[0];
        self.bits[1] = left.bits[1] ^ right.bits[1];
        self.bits[2] = left.bits[2] ^ right.bits[2];
        self.bits[3] = left.bits[3] ^ right.bits[3];
    }

    pub fn bitComplement(self: *ByteBitset, in: *ByteBitset) void {
        out[0] = ~in[0];
        out[1] = ~in[1];
        out[2] = ~in[2];
        out[3] = ~in[3];
    }

    pub fn singleIndexIntersect(self: *ByteBitset, index: u8) void {
        const had_bit = self.isSet(index);
        self.unsetAllBit();
        if (had_bit) {
            self.set(index);
        }
    }

    pub fn intersectRange(self: *ByteBitset, from_index: u8, to_index: u8) void {
        const from_word_index = from_index >> 6;
        const to_word_index = to_index >> 6;

        var word_position = 0;
        while (word_position < from_word_index) : (wordPosition += 1) {
            self.bits[word_position] = 0;
        }

        self.bits[from_word_index] &= (~0) >> (from_index & 0b0111111);
        self.bits[to_word_index] &= ~(~(1 << 63) >> (to_index & 0b00111111));

        var word_position = to_word_index;
        while (word_position < 4) : (wordPosition += 1) {
            self.bits[word_position] = 0;
        }
    }
};
