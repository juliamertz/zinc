const std = @import("std");
const ast = @import("ast.zig");
const lex = @import("lexer.zig");

pub const ParseError = error{
    IdentifierExpected,
    AssignmentExpected,
    SemicolonExpected,
    IntegerExpected,
    OperatorExpected,
    InvalidInteger,
    InvalidToken, // Maybe this is a bit too general
};

pub const Parser = struct {
    lexer: lex.Lexer,
    curr_token: ?lex.Token,
    peek_token: ?lex.Token,

    const Self = @This();

    pub fn new(content: []const u8) Self {
        var lexer = lex.Lexer.new(content);
        const curr = lexer.readToken();
        lexer.advance();
        const next = lexer.readToken();
        lexer.advance();

        return Parser{
            .lexer = lexer,
            .curr_token = curr,
            .peek_token = next,
        };
    }

    pub fn nextToken(self: *Self) void {
        self.curr_token = self.peek_token;
        self.lexer.advance();
        self.peek_token = self.lexer.readToken();
    }

    fn expectOperator(self: *Self, op: lex.Operator) bool {
        return switch (self.curr_token.?) {
            .operator => |val| val == op,
            else => false,
        };
    }

    // value parsers

    fn parseIdentifier(self: *Self) ParseError![]const u8 {
        return switch (self.curr_token.?) {
            .ident => |val| blk: {
                self.nextToken();
                break :blk val;
            },
            else => return ParseError.IdentifierExpected,
        };
    }

    fn parseIntegerLiteral(self: *Self) ParseError!i64 {
        const int = switch (self.curr_token.?) {
            .integer => |val| std.fmt.parseInt(i64, val, 10) catch {
                return ParseError.InvalidInteger;
            },
            else => ParseError.IntegerExpected,
        };

        self.nextToken();
        return int;
    }

    // statements

    pub fn parseStatement(self: *Self) ParseError!ast.Statement {
        const statement = switch (self.curr_token.?) {
            .keyword => |kw| switch (kw) {
                .let => blk: {
                    self.nextToken();
                    break :blk ast.Statement{
                        .let = try self.parseLetStatement(),
                    };
                },
                else => return ParseError.InvalidToken,
            },
            else => return ParseError.InvalidToken,
        };

        if (self.curr_token.? != lex.Token.semicolon) {
            return ParseError.SemicolonExpected;
        }

        return statement;
    }

    pub fn parseLetStatement(self: *Self) ParseError!ast.LetStatement {
        const ident = try self.parseIdentifier();

        if (!self.expectOperator(lex.Operator.assign)) {
            return ParseError.AssignmentExpected;
        } else {
            self.nextToken();
        }

        return ast.LetStatement{
            .identifier = ident,
            .value = try self.parseExpression(),
        };
    }

    // expressions

    pub fn parseExpression(self: *Self) ParseError!ast.Expression {
        switch (self.curr_token.?) {
            .integer => {
                const tok = .{ .integer_literal = try self.parseIntegerLiteral() };

                switch (self.curr_token.?) {
                    .operator => {
                        const expr = try self.parseOperatorExpression(tok);
                        return .{ .operator = &expr };
                    },
                    .integer => return ParseError.InvalidToken,
                    else => return tok,
                }
            },
            else => {
                return ParseError.InvalidToken;
            },
        }
        return ParseError.InvalidToken;
    }

    fn parseOperatorExpression(self: *Self, left: ast.Expression) ParseError!ast.OperatorExpression {
        const op: lex.Operator = switch (self.curr_token.?) {
            .operator => |val| blk: {
                self.nextToken();
                break :blk val;
            },
            else => return ParseError.OperatorExpected,
        };

        return ast.OperatorExpression{
            .left = left,
            .operator = .{ .operator = op },
            .right = try self.parseExpression(),
        };
    }
};

const assertEq = std.testing.expectEqualDeep;

test "Parse - basic integer let statement" {
    const content = "let name = 25;";
    const tree = ast.Statement{
        .let = .{ .identifier = "name", .value = .{ .integer_literal = 25 } },
    };

    var parser = Parser.new(content);
    try assertEq(try parser.parseStatement(), tree);
}

test "Parse - expressions" {
    const content = "let twohundredfifty = 25 * 10;";
    const tree = ast.Statement{
        .let = .{
            .identifier = "twohundredfifty",
            .value = .{
                .operator = &ast.OperatorExpression{
                    .left = .{ .integer_literal = 25 },
                    .operator = .{ .operator = .multiply },
                    .right = .{ .integer_literal = 10 },
                },
            },
        },
    };

    var parser = Parser.new(content);
    try std.testing.expectEqualDeep(try parser.parseStatement(), tree);
}

test "Parse - expressions complex" {
    const content = "let twohundredfifty = 25 * 10 + 30;";
    const tree = ast.Statement{
        .let = .{
            .identifier = "twohundredfifty",
            .value = .{
                .operator = &ast.OperatorExpression{
                    .left = .{ .integer_literal = 25 },
                    .operator = .{ .operator = .multiply },
                    .right = .{
                        .operator = &ast.OperatorExpression{
                            .left = .{ .integer_literal = 10 },
                            .operator = .{ .operator = .add },
                            .right = .{ .integer_literal = 30 },
                        },
                    },
                },
            },
        },
    };

    var parser = Parser.new(content);
    try std.testing.expectEqualDeep(try parser.parseStatement(), tree);
}
