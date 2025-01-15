const std = @import("std");

pub const Token = union(enum) {
    ident: []const u8,
    integer: []const u8,

    assign,
    plus,

    let,

    illegal,
    eof,
};

pub const Lexer = struct {
    const Self = @This();

    pos: usize = 0,
    content: []const u8,

    fn current(self: *Self) ?u8 {
        if (self.pos < 0 or self.pos >= self.content.len) {
            return null;
        }
        return self.content[self.pos - 1];
    }

    fn advance(self: *Self) ?u8 {
        if (self.pos < self.content.len) {
            self.pos += 1;
            return self.current();
        }
        return null;
    }

    fn next_token(self: *Self) ?Token {
        if (self.advance()) |ch| {
            return switch (ch) {
                '=' => .assign,
                '+' => .plus,
                else => .illegal,
            };
        }

        return null;
    }
};

pub fn tokenize(content: []const u8) ![]Token {
    var lexer = Lexer{ .content = content, .pos = 0 };

    var tokens = std.ArrayList(Token).init(std.heap.page_allocator);
    defer tokens.deinit();

    while (lexer.next_token()) |tok| {
        try tokens.append(tok);
        // std.debug.print("current: {}\n", .{tok});
    }

    return tokens.items;
}
