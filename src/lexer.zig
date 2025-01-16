const std = @import("std");
const utils = @import("./utils.zig");

pub const Keyword = enum {
    let,
    mut,
    function,
    match,
    use,

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
    // single_quote,
    // double_quote,

    dot,
    assign,
    plus,
    minus,
    greater_than,
    less_than,
    concat,

    lcurly,
    rcurly,
    lbracket,
    rbracket,
    lparen,
    rparen,

    arrow,

    illegal,
    eof,
};

pub const Lexer = struct {
    position: usize = 0,
    content: []const u8,

    const Self = @This();

    pub fn new(content: []const u8) Self {
        return Self{ .content = content, .position = 0 };
    }

    fn current(self: *Self) u8 {
        if (self.position >= self.content.len) {
            return 0;
        }
        return self.content[self.position];
    }

    fn lookahead(self: *Self) u8 {
        if (self.position + 1 >= self.content.len) {
            return 0;
        }
        return self.content[self.position + 1];
    }

    fn advance(self: *Self) void {
        self.position += 1;
    }

    fn skip_whitespace(self: *Self) void {
        while (utils.is_whitespace(self.current())) {
            self.advance();
        }
    }

    fn read_token(self: *Self) Token {
        self.skip_whitespace();

        // std.debug.print("current: {c}\n", .{self.current()});
        return switch (self.current()) {
            '=' => .assign,
            '+' => .plus,
            '-' => {
                if (self.lookahead() == '>') {
                    self.advance();
                    return .arrow;
                }
                return .minus;
            },
            '>' => .greater_than,
            '<' => .less_than,
            '{' => .lcurly,
            '}' => .rcurly,
            '[' => .lbracket,
            ']' => .rbracket,
            '(' => .lparen,
            ')' => .rparen,
            '.' => {
                if (self.lookahead() == '.') {
                    self.advance();
                    return .concat;
                } else {
                    return .dot;
                }
            },
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
                // self.position -= 1;
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

    // TODO: cleanup read functions

    fn read_until(self: *Self, ch: u8) []const u8 {
        const start = self.position;
        while (self.current() != ch) {
            self.advance();
        }

        return self.content[start..self.position];
    }

    // TODO: i don't understand why the offset D:
    fn read_ident(self: *Self) []const u8 {
        const start = self.position;
        while (utils.is_letter(self.lookahead())) {
            self.advance();
        }

        return self.content[start .. self.position + 1];
    }

    fn read_int(self: *Self) []const u8 {
        const start = self.position;
        while (std.ascii.isDigit(self.lookahead())) {
            self.advance();
        }

        return self.content[start .. self.position + 1];
    }
};

pub fn tokenize(buff: *std.ArrayList(Token), content: []const u8) !void {
    var lexer = Lexer.new(content);

    while (true) {
        const token = lexer.read_token();
        try buff.append(token);

        if (token == .eof) {
            break;
        } else {
            lexer.advance();
        }
    }
}
