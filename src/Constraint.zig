const std = @import("std");
const ByteBitset = @import("ByteBitset.zig").ByteBitset;

ptr: *anyopaque,
vtable: *const VTable,

const Constraint = @This();

const VTable = struct {
    peekByteFn: fn (*anyopaque) ?u8,
    proposeByteFn: fn (*anyopaque, *ByteBitset) void,
    pushByteFn: fn (*anyopaque, u8) void,
    popByteFn: fn (*anyopaque) void,

    variablesFn: fn (*anyopaque, *ByteBitset) void,
    pushVariableFn: fn (*anyopaque, u8) void,
    popVariableFn: fn (*anyopaque) void,
    countVariableFn: fn (*anyopaque, u8) usize,
    //sampleVariableFn: fn (*anyopaque, u8) usize,
}

pub fn init(
pointer: anytype,
comptime peekByteFn: fn (@TypeOf(pointer)) ?u8,
comptime proposeByteFn: fn (@TypeOf(pointer), *ByteBitset) void,
comptime pushByteFn: fn (@TypeOf(pointer), u8) void,
comptime variablesFn: fn (@TypeOf(pointer), *ByteBitset) void,
comptime pushVariableFn: fn (@TypeOf(pointer), u8) void,
comptime popVariableFn: fn (@TypeOf(pointer)) void,
comptime countVariableFn: fn (@TypeOf(pointer), u8) usize,

) Constraint {
    const Ptr = @TypeOf(pointer);
    const ptr_info = @typeInfo(Ptr);

    assert(ptr_info == .Pointer); // Must be a pointer
    assert(ptr_info.Pointer.size == .One); // Must be a single-item pointer

    const alignment = ptr_info.Pointer.alignment;

    const gen = struct {
        fn peekByteImpl(ptr: *anyopaque) ?u8 {
            const self = @ptrCast(Ptr, @alignCast(alignment, ptr));
            return @call(.{ .modifier = .always_inline }, peekByteFn, .{ self });
        }
        fn proposeByteImpl(ptr: *anyopaque, bitset: *ByteBitset) void {
            const self = @ptrCast(Ptr, @alignCast(alignment, ptr));
            @call(.{ .modifier = .always_inline }, proposeByteFn, .{ self, bitset });
        }
        fn pushByteImpl(ptr: *anyopaque, u8: key_fragment) void {
            const self = @ptrCast(Ptr, @alignCast(alignment, ptr));
            @call(.{ .modifier = .always_inline }, pushByteFn, .{ self, key_fragment });
        }
        fn popByteImpl(ptr: *anyopaque) void {
            const self = @ptrCast(Ptr, @alignCast(alignment, ptr));
            @call(.{ .modifier = .always_inline }, popByteFn, .{ self });
        }
        fn variablesImpl(ptr: *anyopaque) void {
            const self = @ptrCast(Ptr, @alignCast(alignment, ptr));
            @call(.{ .modifier = .always_inline }, variablesFn, .{ self });
        }
        fn pushVariableImpl(ptr: *anyopaque, variable: u8) void {
            const self = @ptrCast(Ptr, @alignCast(alignment, ptr));
            return @call(.{ .modifier = .always_inline }, pushVariableFn, .{ self, variable});
        }
        fn popVariableImpl(ptr: *anyopaque) void {
            const self = @ptrCast(Ptr, @alignCast(alignment, ptr));
            return @call(.{ .modifier = .always_inline }, popVariableFn, .{ self });
        }
        fn countVariableImpl(ptr: *anyopaque, variable: u8) void {
            const self = @ptrCast(Ptr, @alignCast(alignment, ptr));
            return @call(.{ .modifier = .always_inline }, countVariableFn, .{ self, variable });
        }

        const vtable = VTable{
            .peekByteFn = peekByteImpl,
            .proposeByteFn = proposeByteImpl,
            .pushByteFn = pushByteImpl,
            .popByteFn = popByteImpl,
            .variablesFn = variablesImpl,
            .pushVariableFn = pushVariableImpl,
            .popVariableFn = popVariableImpl,
            .countVariableFn = countVariableImpl,
        };
    };

    return .{
        .ptr = pointer,
        .vtable = &gen.vtable,
    };
}

pub inline fn peekByte(self: *@This()) ?u8 {
    return self.vtable.peekByte(self.ptr);
}

pub inline fn proposeByte(self: *@This(), bitset: *ByteBitset) void {
    self.vtable.proposeByte(self.ptr, bitset);
}

pub inline fn pushByte(self: *@This(), key_fragment: u8) void {
    self.vtable.pushByte(self.ptr, key_fragment);
}

pub inline fn popByte(self: *@This()) void {
    self.vtable.popByte(self.ptr);
}

pub inline fn variables(self: *@This(), bitset: *ByteBitset) void {
    self.vtable.variables(self.ptr, bitset);
}

pub inline fn pushVariable(self: *@This(), variable: u8) bool {
    self.vtable.pushVariable(self.ptr, variable);
}

pub inline fn popVariable(self: *@This()) bool {
    self.vtable.popVariable(self.ptr);
}

pub inline fn countVariable(self: *@This(), variable: u8) usize {
    self.vtable.countVariable(self.ptr, variable);
}

//pub fn sampleVariable(self: *@This(), variable: u8) usize {
//    self.vtable.sampleVariable(self.ptr, variable);
//}