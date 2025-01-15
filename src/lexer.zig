const std = @import("std");

const Token = union(enum) {
    ident: []const u8,
    integer: []const u8,

    assign,
    plus,

    let,

    illegal,
    eof,
};

const Lexer = struct {
    const Self = @This();

    pos: usize = 0,
    content: []const u8,

    pub fn current(self: *Self) u8 {
        return self.content[self.pos];
    }
};

// pub fn tokenize() []Token {
// }
