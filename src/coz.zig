const c = @cImport({
    @cInclude("ccoz.h");
});

pub fn begin(name: [:0] u8) void {
    c.cozBegin(name);
}

pub fn end(name: [:0] u8) void {
    c.cozEnd(name);
}

pub fn progressNamed(name: [:0] u8) void {
    c.cozProgressNamed(name);
}

pub fn progress() void {
    c.cozProgress();
}
