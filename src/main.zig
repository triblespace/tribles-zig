const std = @import("std");
const testing = std.testing;

const PACT = @import("./PACT.zig");

test {
    PACT.init();
    testing.refAllDecls(@This());
}
