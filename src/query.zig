const std = @import("std");
const ByteBitset = @import("ByteBitset.zig").ByteBitset;

pub fn CursorIterator(comptime cursor_type: type, comptime max_depth: u8) type {
    return struct {
        branch_points: ByteBitset = ByteBitset.initEmpty(),
        key: [max_depth]u8 = [_]u8{0} ** max_depth,
        branch_state: [max_depth]ByteBitset = [_]ByteBitset{ByteBitset.initEmpty()} ** max_depth,
        mode: ExplorationMode = .path,
        depth: u8 = 0,
        cursor: cursor_type,

        const ExplorationMode = enum { path, branch, backtrack };

        pub fn init(iterated_cursor: cursor_type) @This() {
            return @This(){.cursor = iterated_cursor};
        }

        pub fn next(self: *@This()) ?[max_depth]u8 {
            outer: while (true) {
                switch (self.mode) {
                    .path => {
                        while (self.depth < max_depth) {
                            if (self.cursor.peek()) |key_fragment| {
                                self.key[self.depth] = key_fragment;
                                self.cursor.push(key_fragment);
                                self.depth += 1;
                            } else {
                                self.cursor.propose(&self.branch_state[self.depth]);
                                self.branch_points.set(self.depth);
                                self.mode = .branch;
                                continue :outer;
                            }
                        } else {
                            self.mode = .backtrack;
                            return self.key;
                        }
                    },
                    .branch => {
                        if(self.branch_state[self.depth].drainNextAscending()) |key_fragment| {
                            self.key[self.depth] = key_fragment;
                            self.cursor.push(key_fragment);
                            self.depth += 1;
                            self.mode = .path;
                            continue :outer;
                        } else {
                            self.branch_points.unset(self.depth);
                            self.mode = .backtrack;
                            continue :outer;
                        }
                    },
                    .backtrack => {
                        if(self.branch_points.findLastSet()) |parent_depth| {
                            while (parent_depth < self.depth) : (self.depth -= 1) self.cursor.pop();
                            self.mode = .branch;
                            continue :outer;
                        } else {
                            return null;
                        }
                    }
                }
            }
        }
    };
}

pub fn PaddedCursor(comptime cursor_type: type, comptime segment_size: u8) type {
    const segments = cursor_type.segments;
    return struct {
        depth: u8 = 0,
        cursor: cursor_type,

        const padded_size = segments.len * segment_size;

        pub const padding = blk: {
            var g = ByteBitset.initFull();

            var depth = 0;
            for (segments) | s | {
                const pad = segment_size - s;

                depth += pad;

                var j = pad;
                while (j < segment_size):(j += 1) {
                    g.unset(depth);
                    depth += 1;
                }
            }

            break :blk g;
        };

        pub fn init(cursor_to_pad: cursor_type) @This() {
            return @This(){.cursor = cursor_to_pad};
        }

        // Interface API >>>

        pub fn peek(self: *@This()) ?u8 {
            if (padding.isSet(self.depth)) return 0;
            return self.cursor.peek();
        }

        pub fn propose(self: *@This(), bitset: *ByteBitset) void {
            if (padding.isSet(self.depth)) {
                bitset.unsetAll();
                bitset.set(0);
            } else {
                self.cursor.propose(bitset);
            }
        }

        pub fn pop(self: *@This()) void {
            self.depth -= 1;
            if (padding.isUnset(self.depth)) {
                self.cursor.pop();
            }
        }

        pub fn push(self: *@This(), key_fragment: u8) void {
            if (padding.isUnset(self.depth)) {
                self.cursor.push(key_fragment);
            }
            self.depth += 1;
        }

        pub fn segmentCount(self: *@This()) u32 {
            return self.cursor.segmentCount();
        }

        // <<< Interface API

        pub fn iterate(self: *cursor_type) CursorIterator(@This(), padded_size) {
            return CursorIterator(@This(), padded_size).init(self);
        }
    };
}

pub fn IntersectionCursor(comptime cursor_type: type, comptime key_size: u8) type {
    return struct {
        cursors: []cursor_type,

        pub fn init(cursors: []cursor_type) @This() {
            return @This(){.cursors = cursors};
        }

        // Interface API >>>

        pub fn peek(self: *@This()) ?u8 {
          var byte: ?u8 = null;

          for (self.cursors) |cursor| {
            if (cursor.peek()) | peeked| {
                byte = byte orelse peeked;
                if (byte != peeked) return null;
            } else {
                return null;
            }
          }
        }

        pub fn propose(self: *@This(), bitset: *ByteBitset) void {
            bitset.setAll();
            for (self.cursors) |cursor| {
                const proposed: ByteBitset = undefined;
                cursor.propose(&proposed);
                bitset.intersect(&bitset, &proposed);
            }
        }

        pub fn pop(self: *@This()) void {
            for (self.cursors) |cursor| {
                cursor.pop();
            }
        }

        pub fn push(self: *@This(), key_fragment: u8) void {
            for (self.cursors) |cursor| {
                cursor.push(key_fragment);
            }
        }

        pub fn iterate(self: *@This()) CursorIterator(@This(), key_size) {
            return CursorIterator(@This(), key_size).init(self);
        }
    };
}

const ConstraintInterface = struct {
    impl: *anyopaque,

    peekByteFn: fn (*anyopaque) ?u8,
    proposeByteFn: fn (*anyopaque, *ByteBitset) void,
    pushByteFn: fn (*anyopaque, u8) void,
    popByteFn: fn (*anyopaque) void,

    proposeVariableFn: fn (*anyopaque, *ByteBitset) void,
    pushVariableFn: fn (*anyopaque, u8) bool,
    popVariableFn: fn (*anyopaque) void,
    countVariableFn: fn (*anyopaque, u8) usize,
    sampleVariableFn: fn (*anyopaque, u8) usize,

    // Interface API >>>

    pub fn peekByte(self: *@This()) ?u8 {
    }

    pub fn proposeByte(self: *@This(), bitset: *ByteBitset) void {
    }

    pub fn pushByte(self: *@This(), key_fragment: u8) void {
    }

    pub fn popByte(self: *@This()) void {
    }

    pub fn variables(self: *@This(), bitset: *ByteBitset) void {
    }

    pub fn pushVariable(self: *@This(), variable: u8) bool {
    }
    
    pub fn popVariable(self: *@This()) bool {
    }

    pub fn countVariable(self: *@This(), variable: u8) usize {
    }

    pub fn sampleVariable(self: *@This(), variable: u8) usize {
    }
};



// var v = vars();
// const loveQuery = query('r', 'characters').find(v('name'), v('title'))
//     .in(KB.from('characters').where(.{
//         .{v('r'), ns(v, 'name'), v('name')},
//         .{v('r'), ns(v, 'loves'), v('j')},
//         ns.loves(v('r'), v('j')),
//         ns.name(v('j'), 'juliet'),
//         .{v('j'), ns(v, 'title'), v('title')}}))
//     .in(v());
//
// loveQuery.run(.{.characters = characterKB}, .{.r = "Romeo"})