const c = @cImport({
    @cInclude("ccoz.h");
});

pub fn begin(name: [:0]const u8) void {
    c.cozBegin(name);
}

pub fn end(name: [:0]const u8) void {
    c.cozEnd(name);
}

pub fn progress(name: [:0]const u8) void {
    c.cozProgressNamed(name);
}
