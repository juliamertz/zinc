const std = @import("std");
const ast = @import("ast.zig");
const lex = @import("lexer.zig");

pub const ParseError = error{
    IdentifierExpected,
    AssignmentExpected,
    SemicolonExpected,
    IntegerExpected,
    OperatorExpected,
    ExpressionExpected,
    InvalidInteger,

    OpenParenExpected,
    CloseParenExpected,
    OpenSquirlyExpected,
    CloseSquirlyExpected,

    InvalidToken, // Maybe this is a bit too general
};

pub const Parser = struct {
    lexer: lex.Lexer,
    alloc: std.mem.Allocator,

    curr_token: ?lex.Token,
    peek_token: ?lex.Token,

    const Self = @This();

    pub fn new(content: []const u8, alloc: std.mem.Allocator) Self {
        var lexer = lex.Lexer.new(content);

        const curr = lexer.readToken();
        lexer.advance();
        const next = lexer.readToken();
        lexer.advance();

        return Parser{
            .lexer = lexer,
            .alloc = alloc,
            .curr_token = curr,
            .peek_token = next,
        };
    }

    pub fn nextToken(self: *Self) void {
        self.curr_token = self.peek_token;
        self.lexer.advance();
        self.peek_token = self.lexer.readToken();
    }

    fn expectOperator(self: *Self, expected: ast.Operator) bool {
        const operator = self.scanOperator() catch {
            return false;
        };
        return operator == expected;
    }

    fn expectToken(self: *Self, expected: lex.Token) !void {
        if (self.curr_token) |tok| {
            if (std.meta.eql(tok, expected)) {
                self.nextToken();
                return;
            } else {
                // TODO: find out if zig has a nicer way of doing this
                return switch (expected) {
                    .lparen => ParseError.OpenParenExpected,
                    .rparen => ParseError.CloseParenExpected,
                    .lsquirly => ParseError.OpenSquirlyExpected,
                    .rsquirly => ParseError.CloseSquirlyExpected,
                    else => ParseError.InvalidToken,
                };
            }
        }

        return ParseError.InvalidToken;
    }

    fn scanOperator(self: *Self) ParseError!ast.Operator {
        const token = self.curr_token orelse return ParseError.OperatorExpected;
        const operator: ast.Operator = switch (token) {
            .equal => .equal,
            .plus => .add,
            .minus => .subtract,
            .asterisk => .multiply,
            .forward_slash => .divide,
            .dot => blk: {
                if (std.meta.eql(self.peek_token.?, .dot)) break :blk .concat;
                return ParseError.OperatorExpected;
            },
            else => return ParseError.OperatorExpected,
        };

        return operator;
    }

    pub fn parseBlock(self: *Self) ParseError!ast.Block {
        var module = std.ArrayList(ast.Statement).init(self.alloc);

        while (!std.meta.eql(self.curr_token, .eof)) {
            const statement = self.parseStatement() catch |err| {
                if (std.meta.eql(self.curr_token, .rsquirly)) break;
                return err;
            };
            module.append(statement) catch @panic("unable to append");
        }

        return .{ .statements = module.items };
    }

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

    fn parseOperator(self: *Self) ParseError!ast.Operator {
        const token = try self.scanOperator();
        self.nextToken();
        return token;
    }

    pub fn parseStatement(self: *Self) ParseError!ast.Statement {
        const statement: ast.Statement = switch (self.curr_token.?) {
            .keyword => |kw| switch (kw) {
                .let => .{ .let = try self.parseLetStatement() },
                .return_token => .{ .return_ = try self.parseReturnStatement() },
                .function => .{ .function = try self.parseFunctionStatement() },
                else => return ParseError.InvalidToken,
            },
            else => return ParseError.InvalidToken,
        };

        if (self.curr_token.? != lex.Token.semicolon) {
            return ParseError.SemicolonExpected;
        } else self.nextToken();

        return statement;
    }

    pub fn parseFunctionArgument(self: *Self) ParseError!ast.FunctionArgument {
        return ast.FunctionArgument{
            .identifier = try self.parseIdentifier(),
        };
    }

    pub fn parseFunctionStatement(self: *Self) ParseError!ast.FunctionStatement {
        self.nextToken();

        const ident = try self.parseIdentifier();

        // arguments
        try self.expectToken(lex.Token.lparen);

        var arguments = std.ArrayList(ast.FunctionArgument).init(self.alloc);
        while (!std.meta.eql(self.curr_token.?, .rparen)) {
            const arg = try self.parseFunctionArgument();
            arguments.append(arg) catch @panic("unable to append");
        }

        try self.expectToken(lex.Token.rparen);

        // body
        try self.expectToken(lex.Token.lsquirly);

        const body = try self.parseBlock();

        try self.expectToken(lex.Token.rsquirly);

        return ast.FunctionStatement{
            .identifier = ident,
            .arguments = arguments.items,
            .body = body,
        };
    }

    pub fn parseReturnStatement(self: *Self) ParseError!ast.ReturnStatement {
        self.nextToken();

        return ast.ReturnStatement{
            .value = try self.parseExpression(),
        };
    }

    pub fn parseLetStatement(self: *Self) ParseError!ast.LetStatement {
        self.nextToken();

        const ident = try self.parseIdentifier();

        if (!self.expectOperator(ast.Operator.equal)) {
            return ParseError.AssignmentExpected;
        } else {
            self.nextToken();
        }

        return ast.LetStatement{
            .identifier = ident,
            .value = try self.parseExpression(),
        };
    }

    pub fn parseExpression(self: *Self) ParseError!ast.Expression {
        const token: ast.Expression = switch (self.curr_token.?) {
            .integer => |val| .{
                .integer_literal = std.fmt.parseInt(i64, val, 10) catch {
                    return ParseError.InvalidInteger;
                },
            },
            .ident => |ident| .{ .identifier = ident },

            else => return ParseError.ExpressionExpected,
        };

        self.nextToken();
        _ = self.scanOperator() catch return token;

        return .{
            .operator = try self.parseOperatorExpression(token),
        };
    }

    // TODO: operator precedence
    fn parseOperatorExpression(self: *Self, left: ast.Expression) ParseError!*ast.OperatorExpression {
        const expr = self.alloc.create(ast.OperatorExpression) catch @panic("unable to allocate");
        expr.* = ast.OperatorExpression{
            .left = left,
            .operator = try self.parseOperator(),
            .right = try self.parseExpression(),
        };

        return expr;
    }

    fn printDebug(self: *Self, message: []const u8) void {
        std.debug.print("{s}:\ncurr_token: {any}\npeek_token: {any}\n", .{
            message,
            self.curr_token,
            self.peek_token,
        });
    }
};

const assertEq = std.testing.expectEqualDeep;

test "Parse - basic integer let statement" {
    const content = "let name = 25;";
    const tree = ast.Statement{
        .let = .{ .identifier = "name", .value = .{ .integer_literal = 25 } },
    };

    var parser = Parser.new(content, std.testing.allocator);
    try assertEq(try parser.parseStatement(), tree);
    parser.alloc.deinit();
}

test "Parse - return" {
    const content = "return 2500 - 10;";
    var parser = Parser.new(content, std.testing.allocator);

    // FIX: return statement semicolon doesn't get parsed because of lexer advancing twice at start??
    // this only happens when passing single integer_literal
    // failing input: const content = "return 2500;";
    // working input: const content = "return 2500 ;";

    const expr = try parser.alloc.create(ast.OperatorExpression);
    expr.* = ast.OperatorExpression{
        .left = .{ .integer_literal = 2500 },
        .operator = .subtract,
        .right = .{ .integer_literal = 10 },
    };
    const tree = ast.Statement{
        .return_ = .{ .value = .{ .operator = expr } },
    };

    try assertEq(try parser.parseStatement(), tree);
    parser.alloc.deinit();
}

test "Parse - operator expressions" {
    const content = "let name = 25*10;";
    var parser = Parser.new(content, std.testing.allocator);

    const expr = try parser.alloc.create(ast.OperatorExpression);
    expr.* = ast.OperatorExpression{
        .left = .{ .integer_literal = 25 },
        .operator = .multiply,
        .right = .{ .integer_literal = 10 },
    };
    const statement = ast.Statement{
        .let = .{
            .identifier = "name",
            .value = .{
                .operator = expr,
            },
        },
    };

    try assertEq(try parser.parseStatement(), statement);

    parser.alloc.deinit();
}

test "Parse - nested operator expressions" {
    const content = "let name = 25 * 10 -50;";
    var parser = Parser.new(content, std.testing.allocator);

    const expr_inner = try parser.alloc.create(ast.OperatorExpression);
    expr_inner.* = ast.OperatorExpression{
        .left = .{ .integer_literal = 10 },
        .operator = .subtract,
        .right = .{ .integer_literal = 50 },
    };
    const expr_outer = try parser.alloc.create(ast.OperatorExpression);
    expr_outer.* = ast.OperatorExpression{
        .left = .{ .integer_literal = 25 },
        .operator = .multiply,
        .right = .{ .operator = expr_inner },
    };
    const statement = ast.Statement{
        .let = .{
            .identifier = "name",
            .value = .{
                .operator = expr_outer,
            },
        },
    };

    try assertEq(try parser.parseStatement(), statement);

    parser.alloc.deinit();
}
