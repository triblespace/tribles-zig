const std = @import("std");
const ByteBitset = @import("ByteBitset.zig").ByteBitset;
const Constraint = @import("Constraint.zig");

pub fn VariableIterator(comptime cursor_type: type, comptime max_depth: u8) type {
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

pub fn IntersectionConstraint(comptime ConstraintsType: type) type {
    return struct {
        constraints: ConstraintsType,
        activeConstraints: ByteBitset = ByteBitset.initEmpty(),
        variableStack: [256]u8 = [_]u8{0} ** 256,
        variableStackLen: u8 = 0,

        pub fn init(constraints: ConstraintsType) @This() {
            return @This(){.constraints = constraints};
        }

        pub fn constraint(self: *@This()) Constraint {
            return Constraint.init(self,
                peekByte, proposeByte, pushByte, popByte,
                variables, pushVariable, popVariable, countVariable);
        }

        pub fn peekByte(self: *@This()) ?u8 {
            var byte: ?u8 = null;
            var constraintIter = self.activeConstraints; 
            while (constraintIter.drainNextAscending()) |i| {
                if(self.constraints[i].peekByte()) |peeked| {
                    byte = byte or peeked;
                    if(byte != peeked) return null;
                } else {
                    return null;
                }
            }

            return byte;
        }

        pub fn proposeByte(self: *@This(), bitset: *ByteBitset) void {
            bitset.setAll();
            var constraintIter = self.activeConstraints; 
            while (constraintIter.drainNextAscending()) |i| {
                const proposed: ByteBitset = undefined;
                this.constraints[i].proposeByte(&proposed);
                bitset.intersect(bitset, &proposed);
            }
        }

        pub fn pushByte(self: *@This(), key_fragment: u8) void {
            var constraintIter = self.activeConstraints; 
            while (constraintIter.drainNextAscending()) |i| {
                this.constraints[i].pushByte(key_fragment);
            }
        }

        pub fn popByte(self: *@This()) void {
            var constraintIter = self.activeConstraints; 
            while (constraintIter.drainNextAscending()) |i| {
                this.constraints[i].popByte();
            }
        }

        pub fn variables(self: *@This(), bitset: *ByteBitset) void {
            for(self.constraints) |constraint| {
                var constraint_variables: ByteBitset = undefined;
                constraint.variables(&constraint_variables);
                bitset.setUnion(bitset, &constraint_variables);
            }
        }

        pub fn pushVariable(self: *@This(), variable: u8) void {
            self.variableStack[self.variableStackLen] = variable;
            self.variableStackLen += 1;
            self.activeConstraints.unsetAll();
            for(self.constraints) |constraint, i| {
                var constraint_variables: ByteBitset = undefined;
                constraint.variables(&constraint_variables);
                if(constraint_variables.has(variable)) {
                    self.activeConstraints.set(i);
                    constraint.pushVariable(variable);
                }
            }
        }
        
        pub fn popVariable(self: *@This()) void {
            var constraintIter = self.activeConstraints;
            while (constraintIter.drainNextAscending()) |i| {
                this.constraints[i].popVariable();
            }

            self.variableStackLen -= 1;
            self.activeConstraints.unsetAll();

            if(0 < self.variableStackLen) {
                const variable = self.variableStack[self.variableStackLen-1];
                for(self.constraints) |constraint, i| {
                    var constraint_variables: ByteBitset = undefined;
                    constraint.variables(&constraint_variables);
                    if(constraint_variables.has(variable)) {
                        self.activeConstraints.set(i);
                        constraint.pushVariable(variable);
                    }
                }
            }
        }

        pub fn countVariable(self: *@This(), variable: u8) usize {
            var min: usize = std.math.maxInt(usize);

            for(self.constraints) |constraint| {
                var constraint_variables: ByteBitset = undefined;
                constraint.variables(&constraint_variables);
                if(constraint_variables.has(variable)) {
                    min = @minimum(min, constraint.countVariable(variable));
                }
            }

            return min;
        }
    };
}

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