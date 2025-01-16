const std = @import("std");
const ast = @import("./ast.zig");
const lex = @import("./lexer.zig");

pub const ParseError = error{IdentifierExpected};

pub const Parser = struct {
    lexer: lex.Lexer,

    curr_token: ?lex.Token,
    peek_token: ?lex.Token,

    const Self = @This();

    fn next_token(self: *Self) void {
        self.curr_token = self.peek_token;
        self.peek_token = self.lexer.read_token();
    }

    pub fn new(content: []const u8) Self {
        var lexer = lex.Lexer.new(content);

        return Self{
            .lexer = lexer,
            .curr_token = lexer.read_token(),
            .peek_token = lexer.read_token(),
        };
    }

    fn parse_expression(self: *Self) ast.Expression {
        if (self.curr_token == .integer) {}
        // var module = ast.Module{};
    }

    fn parse_let_statement(self: *Self) ast.LetStatement {
        if (self.curr_token != .identifier) return ParseError.IdentifierExpected;
        // var module = ast.Module{};
    }

    fn parse_module(self: *Self) void {
        switch (self.curr_token) {
            .let => self.parse_let_statement(),
            else => {},
        }
    }
};
