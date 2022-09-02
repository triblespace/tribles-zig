const std = @import("std");
const assert = std.debug.assert;

pub const ByteBitset = extern struct {
    bits: [4]u64 = .{ 0, 0, 0, 0 },

    pub fn initEmpty() ByteBitset {
        return ByteBitset{ .bits = .{ 0, 0, 0, 0 } };
    }

    /// Creates a bit set with all elements present.
    pub fn initFull() ByteBitset {
        return ByteBitset{ .bits = .{ ~@as(u64, 0), ~@as(u64, 0), ~@as(u64, 0), ~@as(u64, 0) } };
    }

    pub fn isEmpty(self: *const ByteBitset) bool {
        return self.bits[0] == 0 and self.bits[1] == 0 and self.bits[2] == 0 and self.bits[3] == 0;
    }

    pub fn count(self: *const ByteBitset) u16 {
        return @as(u16, @popCount(self.bits[0]))
             + @as(u16, @popCount(self.bits[1]))
             + @as(u16, @popCount(self.bits[2]))
             + @as(u16, @popCount(self.bits[3]));
    }

    pub fn set(self: *ByteBitset, index: u8) void {
        self.bits[index >> 6] |= @as(u64, 1) << @truncate(u6, index);
    }

    pub fn unset(self: *ByteBitset, index: u8) void {
        self.bits[index >> 6] &= ~(@as(u64, 1) << @truncate(u6, index));
    }

    pub fn setValue(self: *ByteBitset, index: u8, value: bool) void {
        if (value) {
            self.set(index);
        } else {
            self.unset(index);
        }
    }

    pub fn setAll(self: *ByteBitset) void {
        self.bits[0] = ~@as(u64, 0);
        self.bits[1] = ~@as(u64, 0);
        self.bits[2] = ~@as(u64, 0);
        self.bits[3] = ~@as(u64, 0);
    }

    pub fn unsetAll(self: *ByteBitset) void {
        self.bits[0] = 0;
        self.bits[1] = 0;
        self.bits[2] = 0;
        self.bits[3] = 0;
    }

    pub fn isSet(self: *const ByteBitset, index: u8) bool {
        return (self.bits[index >> 6] & (@as(u64, 1) << @truncate(u6, index))) != 0;
    }

    /// Finds the index of the first set bit.
    /// If no bits are set, returns null.
    pub fn findFirstSet(self: *const ByteBitset) ?u8 {
        if (self.bits[0] != 0) return @as(u8, @ctz(self.bits[0]));
        if (self.bits[1] != 0) return (1 << 6) + @as(u8, @ctz(self.bits[1]));
        if (self.bits[2] != 0) return (2 << 6) + @as(u8, @ctz(self.bits[2]));
        if (self.bits[3] != 0) return (3 << 6) + @as(u8, @ctz(self.bits[3]));
        return null;
    }

    /// Finds the index of the last set bit.
    /// If no bits are set, returns null.
    pub fn findLastSet(self: *const ByteBitset) ?u8 {
        if (self.bits[3] != 0) return (1 << 6) + (63 - @as(u8, @clz(self.bits[3])));
        if (self.bits[2] != 0) return (2 << 6) + (63 - @as(u8, @clz(self.bits[2])));
        if (self.bits[1] != 0) return (3 << 6) + (63 - @as(u8, @clz(self.bits[1])));
        if (self.bits[0] != 0) return (63 - @as(u8, @clz(self.bits[0])));
        return null;
    }

    /// Returns the index of the next set bit
    /// in the bit set, in ascending order, while unseting it.
    pub fn drainNextAscending(self: *ByteBitset) ?u8 {
        if (self.isEmpty()) return null;
        const next_index = self.findFirstSet() orelse unreachable;
        self.unset(next_index);
        return next_index;
    }

    /// Returns the index of the next set bit
    /// in the bit set, in descending order, while unseting it.
    pub fn drainNextDescending(self: *ByteBitset) ?u8 {
        if (self.isEmpty()) return null;
        const next_index = self.findLastSet() orelse unreachable;
        self.unset(next_index);
        return next_index;
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
        self.bits[0] = ~in.bits[0];
        self.bits[1] = ~in.bits[1];
        self.bits[2] = ~in.bits[2];
        self.bits[3] = ~in.bits[3];
    }

    pub fn singleIntersect(self: *ByteBitset, index: u8) void {
        const had_bit = self.isSet(index);
        self.unsetAll();
        if (had_bit) {
            self.set(index);
        }
    }

    pub fn intersectRange(self: *ByteBitset, from_index: u8, to_index: u8) void {
        const from_word_index = from_index >> 6;
        const to_word_index = to_index >> 6;

        var word_index = 0;
        while (word_index < from_word_index) : (word_index += 1) {
            self.bits[word_index] = 0;
        }

        self.bits[from_word_index] &= (~0) >> @truncate(u8, from_index);
        self.bits[to_word_index] &= ~(~(1 << 63) >> @truncate(u6, to_index));

        word_index = to_word_index;
        while (word_index < 4) : (word_index += 1) {
            self.bits[word_index] = 0;
        }
    }
};
