const std = @import("std");
const utils = @import("utils.zig");

pub const Keyword = enum {
    let,
    mut,
    function,
    match,
    use,

    pub_token,
    return_token,
    if_token,
    else_token,
    true_token,
    false_token,

    fn from_str(str: []const u8) ?Keyword {
        const map = std.StaticStringMap(Keyword).initComptime(.{
            .{ "let", .let },
            .{ "mut", .mut },
            .{ "func", .function },
            .{ "match", .match },
            .{ "use", .use },

            .{ "pub", .pub_token },
            .{ "return", .return_token },
            .{ "if", .if_token },
            .{ "else", .else_token },
            .{ "true", .true_token },
            .{ "false", .false_token },
        });
        return map.get(str);
    }
};

pub const Token = union(enum) {
    ident: []const u8,
    integer: []const u8,
    keyword: Keyword,
    string_literal: []const u8,

    plus,
    minus,
    equal,
    asterisk,
    forward_slash,
    backward_slash,

    dot,
    comma,
    colon,
    semicolon,

    langle,
    rangle,
    lsquirly,
    rsquirly,
    lbracket,
    rbracket,
    lparen,
    rparen,

    illegal,
    eof,
};

pub const Lexer = struct {
    read_position: usize,
    position: usize,
    content: []const u8,

    const Self = @This();

    pub fn new(content: []const u8) Self {
        return Self{
            .content = content,
            .position = 0,
            .read_position = 0,
        };
    }

    fn current(self: *Self) u8 {
        if (self.read_position >= self.content.len) {
            return 0;
        }

        return self.content[self.read_position];
    }

    fn lookahead(self: *Self) u8 {
        if (self.read_position + 1 >= self.content.len) {
            return 0;
        }

        return self.content[self.read_position + 1];
    }

    fn skip_whitespace(self: *Self) void {
        while (utils.is_whitespace(self.current())) {
            self.advance();
        }
    }

    pub fn advance(self: *Self) void {
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn readToken(self: *Self) Token {
        self.skip_whitespace();

        return switch (self.current()) {
            '=' => .equal,
            '+' => .plus,
            '*' => .asterisk,
            '/' => .forward_slash,
            '-' => .minus,
            '<' => .langle,
            '>' => .rangle,
            '{' => .lsquirly,
            '}' => .rsquirly,
            '[' => .lbracket,
            ']' => .rbracket,
            '(' => .lparen,
            ')' => .rparen,
            ':' => .colon,
            ';' => .semicolon,
            ',' => .comma,
            '.' => .dot,
            '\'' => {
                self.advance();
                return .{ .string_literal = self.read_until('\'') };
            },
            '"' => {
                self.advance();
                return .{ .string_literal = self.read_until('"') };
            },

            'a'...'z', 'A'...'Z', '_' => {
                const ident = self.read_ident();
                if (Keyword.from_str(ident)) |keyword| {
                    return .{ .keyword = keyword };
                }
                return .{ .ident = ident };
            },
            '0'...'9' => .{ .integer = self.read_int() },

            0 => .eof,
            else => .illegal,
        };
    }

    fn read_until(self: *Self, ch: u8) []const u8 {
        const start = self.read_position;
        while (self.current() != ch) {
            self.advance();
        }

        return self.content[start..self.read_position];
    }

    // TODO: i don't understand why the offset D:
    fn read_ident(self: *Self) []const u8 {
        const start = self.read_position;
        while (utils.is_letter(self.lookahead())) {
            self.advance();
        }

        return self.content[start .. self.read_position + 1];
    }

    fn read_int(self: *Self) []const u8 {
        const start = self.read_position;
        while (std.ascii.isDigit(self.lookahead())) {
            self.advance();
        }

        return self.content[start .. self.read_position + 1];
    }
};

const assertEq = std.testing.expectEqualDeep;

test "Lexer - special charachters" {
    const input = "=({}+,);";
    var lex = Lexer.new(input);

    const tokens = [_]Token{
        .equal,
        .lparen,
        .lsquirly,
        .rsquirly,
        .plus,
        .comma,
        .rparen,
        .semicolon,
        .eof,
    };

    for (tokens) |token| {
        const tok = lex.readToken();
        lex.advance();
        try assertEq(token, tok);
    }
}

test "Lexer - let statement" {
    const input = "let woof = true;";
    var lex = Lexer.new(input);

    const tokens = [_]Token{
        .{ .keyword = .let },
        .{ .ident = "woof" },
        .plus,
        .{ .keyword = .true_token },
        .semicolon,
        .eof,
    };

    for (tokens) |token| {
        const tok = lex.readToken();
        lex.advance();
        try assertEq(token, tok);
    }
}

test "Lexer - rand" {
    const input = "let twohundredfifty = 25 * 10;";
    var lex = Lexer.new(input);

    const tokens = [_]Token{
        .{ .keyword = .let },
        .{ .ident = "twohundredfifty" },
        .plus,
        .{ .integer = "25" },
        .asterisk,
        .{ .integer = "10" },
        .semicolon,
        .eof,
    };

    for (tokens) |token| {
        const tok = lex.readToken();
        lex.advance();
        try assertEq(token, tok);
    }
}
