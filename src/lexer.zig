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
    illegal,
    eof,

    ident: []const u8,
    keyword: Keyword,

    integer_literal: i64,
    string_literal: []const u8,

    // operators
    assign,
    plus,
    minus,
    bang,
    asterisk,
    slash,
    pipe,
    ampersand,
    arrow,

    @"and",
    @"or",

    equal,
    not_equal,
    less_than,
    greater_than,
    less_than_or_eq,
    greater_than_or_eq,

    dot, // TODO: rename to chain or smth

    // delimiters
    comma,
    colon,
    semicolon,

    lsquirly,
    rsquirly,
    lbracket,
    rbracket,
    lparen,
    rparen,
};

pub const Lexer = struct {
    read_position: usize = 0,
    position: usize = 0,
    content: []const u8,

    const Self = @This();

    pub fn init(content: []const u8) Self {
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
        // TODO: implement new operators
        const token: Token = switch (self.current()) {
            '=' => blk: {
                if (self.lookahead() == '=') {
                    self.advance();
                    break :blk .equal;
                } else {
                    break :blk .assign;
                }
            },
            '+' => .plus,
            '*' => .asterisk,
            '/' => .slash,
            '-' => blk: {
                switch (self.lookahead()) {
                    // skip comments
                    '-' => {
                        while (self.current() != '\n') self.advance();
                        self.skipWhitespace();
                        break :blk self.readToken();
                    },
                    '>' => {
                        self.advance();
                        break :blk .arrow;
                    },
                    else => break :blk .minus,
                }
            },
            '&' => .ampersand,
            '|' => .pipe,
            '!' => blk: {
                if (self.lookahead() == '=') {
                    self.advance();
                    break :blk .not_equal;
                } else {
                    break :blk .bang;
                }
            },
            '<' => blk: {
                if (self.lookahead() == '=') {
                    self.advance();
                    break :blk .less_than_or_eq;
                } else {
                    break :blk .less_than;
                }
            },
            '>' => blk: {
                if (self.lookahead() == '=') {
                    self.advance();
                    break :blk .greater_than_or_eq;
                } else {
                    break :blk .greater_than;
                }
            },

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
                const text = self.readUntil('"');
                const pre_escaped = utils.preEscape(std.heap.page_allocator, text) catch @panic("buy more RAM"); // TODO: allocator
                break :blk .{ .string_literal = pre_escaped };
            },

            'a'...'z', 'A'...'Z', '_' => blk: {
                const ident = self.readIdent();

                if (Keyword.fromStr(ident)) |keyword| {
                    break :blk .{ .keyword = keyword };
                }
                break :blk .{ .ident = ident };
            },

            '0'...'9' => .{ .integer_literal = self.readInt() catch @panic("unable to parse into i64") },

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

fn expectLexerOutput(input: []const u8, expected_tokens: []const Token) !void {
    var lexer = Lexer.init(input);
    for (expected_tokens, 0..) |expected, i| {
        const actual = lexer.readToken();
        lexer.advance();

        if (@intFromEnum(actual) != @intFromEnum(expected)) {
            std.debug.print(
                "mismatching token at position {d}, expected: {any}, actual: {any}\n",
                .{ i, expected, actual },
            );
        }

        try std.testing.expectEqualDeep(expected, actual);
    }
}

test "Lexer - operators" {
    try expectLexerOutput(
        "=+-!*/|& and or ->",
        &[_]Token{
            .assign,
            .plus,
            .minus,
            .bang,
            .asterisk,
            .slash,
            .pipe,
            .ampersand,
            .{ .keyword = .@"and" },
            .{ .keyword = .@"or" },
            .arrow,
            .eof,
        },
    );

    try expectLexerOutput(
        "==!=<><=>=",
        &[_]Token{
            .equal,
            .not_equal,
            .less_than,
            .greater_than,
            .less_than_or_eq,
            .greater_than_or_eq,
            .eof,
        },
    );
}

test "Lexer - delimiters" {
    try expectLexerOutput(
        "{[()]}",
        &[_]Token{
            .lsquirly,
            .lbracket,
            .lparen,
            .rparen,
            .rbracket,
            .rsquirly,
            .eof,
        },
    );

    try expectLexerOutput(
        ":;,.",
        &[_]Token{
            .colon,
            .semicolon,
            .comma,
            .dot,
            .eof,
        },
    );
}

test "Lexer - identifiers" {
    try expectLexerOutput(
        "hey young world",
        &[_]Token{
            .{ .ident = "hey" },
            .{ .ident = "young" },
            .{ .ident = "world" },
            .eof,
        },
    );
}

test "Lexer - literals" {
    try expectLexerOutput(
        "\"hey young world\"",
        &[_]Token{
            .{ .string_literal = "hey young world" },
            .eof,
        },
    );
    try expectLexerOutput(
        "325",
        &[_]Token{
            .{ .integer_literal = 325 },
            .eof,
        },
    );
}

test "Lexer - pre-escaping" {
    try expectLexerOutput(
        "\"new\\nline\\ttab\"",
        &[_]Token{
            .{ .string_literal = "new\nline\ttab" },
            .eof,
        },
    );
}
