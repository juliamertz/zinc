const std = @import("std");
const utils = @import("utils.zig");

pub const Keyword = enum {
    let,
    mut,
    function,
    match,
    use,

    in,
    @"for",
    @"while",
    @"and",
    @"or",
    @"pub",
    @"return",
    @"if",
    @"else",
    true,
    false,

    pub fn fromStr(str: []const u8) ?Keyword {
        const map = std.StaticStringMap(Keyword).initComptime(.{
            .{ "let", .let },
            .{ "mut", .mut },
            .{ "func", .function },
            .{ "match", .match },
            .{ "use", .use },

            .{ "for", .@"for" },
            .{ "while", .@"while" },
            .{ "in", .in },
            .{ "or", .@"or" },
            .{ "and", .@"and" },
            .{ "pub", .@"pub" },
            .{ "return", .@"return" },
            .{ "if", .@"if" },
            .{ "else", .@"else" },
            .{ "true", .true },
            .{ "false", .false },
        });
        return map.get(str);
    }
};

pub const Token = union(enum) {
    ident: []const u8,
    integer: i64,
    keyword: Keyword,
    string_literal: []const u8,

    plus,
    minus,
    equal,
    asterisk,
    forward_slash,
    backward_slash,
    ampersand,
    pipe,
    bang,

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
    read_position: usize = 0,
    position: usize = 0,
    content: []const u8,

    const Self = @This();

    pub fn new(content: []const u8) Self {
        return Self{
            .content = content,
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

    fn skipWhitespace(self: *Self) void {
        while (utils.isWhitespace(self.current())) {
            self.advance();
        }
    }

    pub fn advance(self: *Self) void {
        if (self.read_position != self.content.len) {
            self.position = self.read_position;
            self.read_position += 1;
        }
    }

    pub fn readToken(self: *Self) Token {
        self.skipWhitespace();
        const token: Token = switch (self.current()) {
            '=' => .equal,
            '+' => .plus,
            '*' => .asterisk,
            '/' => .forward_slash,
            '-' => blk: {
                // skip comments
                if (self.lookahead() == '-') {
                    while (self.current() != '\n') self.advance();
                    self.skipWhitespace();
                    break :blk self.readToken();
                } else {
                    break :blk .minus;
                }
            },
            '&' => .ampersand,
            '|' => .pipe,
            '!' => .bang,
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
            '\'' => blk: {
                self.advance();
                break :blk .{ .string_literal = self.readUntil('\'') };
            },
            '"' => blk: {
                self.advance();
                break :blk .{ .string_literal = self.readUntil('"') };
            },

            'a'...'z', 'A'...'Z', '_' => blk: {
                const ident = self.readIdent();

                if (Keyword.fromStr(ident)) |keyword| {
                    break :blk .{ .keyword = keyword };
                }
                break :blk .{ .ident = ident };
            },

            '0'...'9' => .{ .integer = self.readInt() catch @panic("unable to parse into i64") },

            0 => .eof,
            else => .illegal,
        };

        return token;
    }

    fn readUntil(self: *Self, ch: u8) []const u8 {
        const start = self.read_position;
        while (self.current() != ch) {
            self.advance();
        }

        return self.content[start..self.read_position];
    }

    fn readIdent(self: *Self) []const u8 {
        const start = self.read_position;
        while (utils.isLetter(self.lookahead())) {
            self.advance();
        }

        return self.content[start .. self.read_position + 1];
    }

    fn readInt(self: *Self) !i64 {
        const start = self.read_position;
        while (std.ascii.isDigit(self.lookahead())) {
            self.advance();
        }
        const numbers = self.content[start .. self.read_position + 1];
        return std.fmt.parseInt(i64, numbers, 10);
    }
};

// tests

fn expectLexerOutput(input: []const u8, expected: []const Token) !void {
    var lex = Lexer.new(input);
    for (expected) |token| {
        const tok = lex.readToken();
        lex.advance();
        try std.testing.expectEqualDeep(token, tok);
    }
}

test "Lexer - special charachters" {
    try expectLexerOutput(
        "=(*{}+,-);&|",
        &[_]Token{
            .equal,
            .lparen,
            .asterisk,
            .lsquirly,
            .rsquirly,
            .plus,
            .comma,
            .minus,
            .rparen,
            .semicolon,
            .ampersand,
            .pipe,
            .eof,
        },
    );
}

test "Lexer - let statements" {
    try expectLexerOutput(
        "let value=10;",
        &[_]Token{
            .{ .keyword = .let },
            .{ .ident = "value" },
            .equal,
            .{ .integer = 10 },
            .semicolon,
            .eof,
        },
    );

    try expectLexerOutput(
        "let value = 10;",
        &[_]Token{
            .{ .keyword = .let },
            .{ .ident = "value" },
            .equal,
            .{ .integer = 10 },
            .semicolon,
            .eof,
        },
    );

    try expectLexerOutput(
        "let a = 2500;",
        &[_]Token{
            .{ .keyword = .let },
            .{ .ident = "a" },
            .equal,
            .{ .integer = 2500 },
            .semicolon,
            .eof,
        },
    );
}

test "Lexer - let statement" {
    try expectLexerOutput(
        "let woof = true;",
        &[_]Token{
            .{ .keyword = .let },
            .{ .ident = "woof" },
            .equal,
            .{ .keyword = .true },
            .semicolon,
            .eof,
        },
    );
}

test "Lexer - idk" {
    try expectLexerOutput(
        "let twohundredfifty = 25 * 10;",
        &[_]Token{
            .{ .keyword = .let },
            .{ .ident = "twohundredfifty" },
            .equal,
            .{ .integer = 25 },
            .asterisk,
            .{ .integer = 10 },
            .semicolon,
            .eof,
        },
    );
}
