const std = @import("std");

pub const Position = struct {
    x: u8,
    y: u8,
};

pub const Card = struct {
    pub const width = 80;
    pub const height = 30;
    pub const Symbol = u21; // Yes, yes, technically it's a codepoint, but codepoints are an abomination.

    grid: [height][width]u21,

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

    pub fn from(card_string: []const u8) !Card {
        var card = Card{.grid = undefined};
        
        var r: usize = 0;
        var rows = std.mem.split(u8, card_string, "\n");
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
                        card.grid[r-1][c-1] = codepoint;
                    }
                    c += 1;
            }
            if(c != 82) {
            }
            r += 1;
        }
        if(r != 32) {
            return error.BadFormat;
        }

        return card;
    }

    pub fn findTopLeft(self: *const Card, char: u21) ?Position {
        for(self.grid) |row, y| {
            for(row) |symbol, x| {
                if(symbol == char) return Position{.x=@intCast(u8, x), .y=@intCast(u8, y)};
            }
        }
        return null;
    }
};