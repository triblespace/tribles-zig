const std = @import("std");

pub const Position = struct {
    x: u8,
    y: u8,
};

pub const Card = struct {
    pub const width = 80;
    pub const height = 30;
    pub const Symbol = u21; // Yes, yes, technically it's a codepoint, but codepoints are an abomination.

    allocator: std.mem.Allocator,
    grid: [height][width]u21 = undefined,

    pub fn format(
    self: *const Card,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        
        try writer.print("┌"++("─"**width)++"┐\n", .{});

        var y:usize = 0;
        while(y < height):(y+=1) {
            var x:usize = 0;
            try writer.print("│", .{});
            while(x < width):(x+=1) {
                    try writer.print("{u}", .{self.grid[y][x]});
            }
            try writer.print("│\n", .{});
        }
        
        try writer.print("└"++("─"**width)++"┘", .{});

        try writer.writeAll("");
    }

    pub fn init(allocator: std.mem.Allocator) !*Card {
        const card = try allocator.create(Card);
        card.* = Card{ .allocator = allocator};

        return card;
    }

    pub fn deinit(self: *Card) void {
        self.allocator.destroy(self);
    }

    pub fn from(self: *Card, string: []const u8) !*Card {        
        var r: usize = 0;
        var rows = std.mem.split(u8, string, "\n");
        while(rows.next()) |row| {
            var c: usize = 0;
            var columns = (try std.unicode.Utf8View.init(row)).iterator();
            while (columns.nextCodepoint()) |codepoint| {
                    if((r == 0 and c == 0 and codepoint != '┌') or
                       (r == 0 and c == 81 and codepoint != '┐') or
                       (r == 31 and c == 0 and codepoint != '└') or
                       (r == 31 and c == 81 and codepoint != '┘') or
                       (r == 0 and (c != 0 and c != 81) and codepoint != '─') or
                       (r == 31 and (c != 0 and c != 81) and codepoint != '─') or
                       ((r != 0 and r != 31) and c == 0 and codepoint != '│') or
                       ((r != 0 and r != 31) and c == 81 and codepoint != '│')) {
                        return error.BadFormat;
                    }
                    if(0 < r and r < 31 and 0 < c and c < 81) {
                        self.grid[r-1][c-1] = codepoint;
                    }
                    c += 1;
            }
            if(c != 82) {
                return error.BadFormat;
            }
            r += 1;
        }
        if(r != 32) {
            return error.BadFormat;
        }

        return self;
    }

    pub fn at(self: *Card, x: usize, y: usize) *u21 {  
        return &self.grid[y][x];
    }

    pub fn label(self: *Card, x: usize, y: usize, text: []const u8) !void {  
        var codepoints = (try std.unicode.Utf8View.init(text)).iterator();

        var i: usize = 0;
        while (codepoints.nextCodepoint()) |codepoint| {
            const x_offset = x + i;
            if(x_offset >= width) break;
            self.at(x_offset, y).* = codepoint;
            i += 1;
        }
    }

    pub fn labelFmt(self: *Card, x: usize, y: usize, comptime fmt: []const u8, args: anytype) !void {  
        const string = try std.fmt.allocPrint(self.allocator, fmt, args);
        defer self.allocator.free(string);

        try self.label(x, y, string);
    }

    pub fn clear(self: *Card, char: u21) void {
        for(self.grid) |*row| {
            for(row) |*column| {
                column.* = char;
            }
        }
    }

};