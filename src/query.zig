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

pub fn PaddedCursor(comptime cursor_type: type, comptime segments: []const u8, comptime segment_size: u8) type {
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

        pub fn peek(self: *PaddedCursor) ?u8 {
            if (padding.isSet(self.depth)) return 0;
            return self.cursor.peek();
        }

        pub fn propose(self: *PaddedCursor, bitset: *ByteBitset) void {
            if (padding.isSet(self.depth)) {
                bitset.unsetAll();
                bitset.set(0);
            } else {
                self.cursor.propose(bitset);
            }
        }

        pub fn pop(self: *PaddedCursor) void {
            self.depth -= 1;
            if (padding.isUnset(self.depth)) {
                self.cursor.pop();
            }
        }

        pub fn push(self: *PaddedCursor, key_fragment: u8) void {
            if (padding.isUnset(self.depth)) {
                self.cursor.push(key_fragment);
            }
            self.depth += 1;
        }

        pub fn segmentCount(self: *PaddedCursor) u32 {
            return self.cursor.segmentCount();
        }

        // <<< Interface API

        pub fn iterate(self: *cursor_type) CursorIterator(PaddedCursor, padded_size) {
            return CursorIterator(PaddedCursor, padded_size).init(self);
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

        pub fn peek(self: *cursor_type) ?u8 {
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

        pub fn propose(self: *IntersectionCursor, bitset: *ByteBitset) void {
            bitset.setAll();
            for (self.cursors) |cursor| {
                const proposed: ByteBitset = undefined;
                cursor.propose(&proposed);
                bitset.intersect(&bitset, &proposed);
            }
        }

        pub fn pop(self: *IntersectionCursor) void {
            for (self.cursors) |cursor| {
                cursor.pop();
            }
        }

        pub fn push(self: *IntersectionCursor, key_fragment: u8) void {
            for (self.cursors) |cursor| {
                cursor.push(key_fragment);
            }
        }

        pub fn iterate(self: *IntersectionCursor) CursorIterator(IntersectionCursor, key_size) {
            return CursorIterator(IntersectionCursor, key_size).init(self);
        }
    };
}

// test {
//   NS([_]Attr{
//     .{.id = "", .type = u64},
//     .{}
//   })
//   Query(.{})
//   find(({ name, title }) => [
//       ,
//     ]).run()

//   knightsNS.find(.{v(.name), v(.title)}).in(knightskb.where(.{.{ .name = v(.name), .titles = .{v(.title)} }}))
//   kb.pull(id, T)
//   kb.walk(id).get(.name)
// }

// fn myQuery(variable) anytype {
//  return .{

//  }
// }

// struct {
//   field: Value(Int)
//   field2: Entity(User)
// }

// Partial(T){}