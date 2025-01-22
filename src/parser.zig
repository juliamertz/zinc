const std = @import("std");
const ast = @import("ast.zig");
const utils = @import("utils.zig");
const lex = @import("lexer.zig");

const Array = std.ArrayList;

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

    IllegalKeyword,
    IllegalIdentifier,

    UnexpectedEof,
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
        // const next = lexer.readToken();
        // lexer.advance();

        return Parser{
            .lexer = lexer,
            .alloc = alloc,
            .curr_token = curr,
            .peek_token = null,
        };
    }

    pub fn nextToken(self: *Self) void {
        if (self.peek_token) |tok| {
            self.curr_token = tok;
        }
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

    pub fn parseNode(self: *Self) ParseError!ast.Node {
        if (self.curr_token.? == .keyword) {
            return .{ .statement = try self.parseStatement() };
        }

        return .{ .expression = try self.parseExpression() };
    }

    pub fn parseNodes(self: *Self) ParseError![]ast.Node {
        var nodes = Array(ast.Node).init(self.alloc);

        while (!std.meta.eql(self.curr_token, .eof)) {
            const node = self.parseNode() catch |err| {
                if (std.meta.eql(self.curr_token, .rsquirly)) break;
                return err;
            };

            nodes.append(node) catch @panic("unable to append");
        }

        return nodes.items;
    }

    pub fn parseBlock(self: *Self) ParseError!ast.BlockStatement {
        try self.expectToken(.lsquirly);

        const nodes = try self.parseNodes();

        try self.expectToken(.rsquirly);

        return .{ .nodes = nodes };
    }

    fn parseIdentifier(self: *Self) ParseError![]const u8 {
        const ident: []const u8 = switch (self.curr_token.?) {
            .ident => |val| blk: {
                self.nextToken();
                break :blk val;
            },
            else => return ParseError.IdentifierExpected,
        };

        return ident;
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

    fn scanOperator(self: *Self) ParseError!ast.Operator {
        const token = self.curr_token orelse return ParseError.OperatorExpected;
        const operator: ast.Operator = switch (token) {
            .equal => .equal,
            .plus => .add,
            .minus => .subtract,
            .asterisk => .multiply,
            .forward_slash => .divide,
            .dot => blk: {
                if (std.meta.eql(self.peek_token.?, .dot)) {
                    break :blk .concat;
                }
                return ParseError.OperatorExpected;
            },
            .langle => blk: {
                if (self.peek_token.? == .equal) {
                    break :blk .less_than_or_eq;
                }
                break :blk .less_than;
            },
            .rangle => blk: {
                if (self.peek_token.? == .equal) {
                    break :blk .greater_than_or_eq;
                }
                break :blk .greater_than;
            },
            .keyword => |val| switch (val) {
                .and_token => .and_operator,
                .or_token => .or_operator,
                else => return ParseError.IllegalIdentifier,
            },
            else => return ParseError.OperatorExpected,
        };

        return operator;
    }

    fn parseOperator(self: *Self) ParseError!ast.Operator {
        const operator = try self.scanOperator();
        switch (operator) {
            // skip extra for operators that span 2 tokens
            .less_than_or_eq, .greater_than_or_eq, .concat => self.nextToken(),
            else => {},
        }
        self.nextToken();
        return operator;
    }

    pub fn parseStatement(self: *Self) ParseError!ast.Statement {
        const statement: ast.Statement = switch (self.curr_token.?) {
            .keyword => |kw| switch (kw) {
                .let => .{ .let = try self.parseLetStatement() },
                .return_token => .{ .return_ = try self.parseReturnStatement() },
                .function => .{ .function = try self.parseFunctionStatement() },
                .if_token => .{ .if_else = try self.parseIfElseStatement() },
                else => return ParseError.InvalidToken,
            },
            else => return ParseError.InvalidToken,
        };

        if (self.curr_token.? != lex.Token.semicolon) {
            if (self.curr_token.? == lex.Token.eof) {
                return statement;
            }
            return ParseError.SemicolonExpected;
        } else self.nextToken();

        return statement;
    }

    pub fn parseFunctionArgument(self: *Self) ParseError!ast.FunctionArgument {
        return ast.FunctionArgument{
            .identifier = try self.parseIdentifier(),
        };
    }

    pub fn parseIfElseStatement(self: *Self) ParseError!ast.IfStatement {
        self.nextToken();

        return ast.IfStatement{
            .condition = try self.parseExpression(),
            .consequence = try self.parseBlock(),
            .alternative = null,
        };
    }

    pub fn parseFunctionCallExpression(self: *Self, ident: []const u8) ParseError!ast.FunctionCall {
        var arguments = Array(ast.Expression).init(self.alloc);

        self.nextToken();
        try self.expectToken(.lparen);

        while (!std.meta.eql(self.curr_token.?, .rparen)) {
            const arg = try self.parseExpression();
            arguments.append(arg) catch @panic("unable to append");
        }

        try self.expectToken(.rparen);

        return ast.FunctionCall{
            .arguments = arguments.items,
            .identifier = ident,
        };
    }

    pub fn parseFunctionStatement(self: *Self) ParseError!ast.FunctionStatement {
        self.nextToken();

        const ident = try self.parseIdentifier();

        // arguments
        try self.expectToken(lex.Token.lparen);
        var arguments = Array(ast.FunctionArgument).init(self.alloc);
        while (!std.meta.eql(self.curr_token.?, .rparen)) {
            const arg = try self.parseFunctionArgument();
            arguments.append(arg) catch @panic("unable to append");
        }
        try self.expectToken(lex.Token.rparen);

        const body = try self.parseBlock();

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

        const res = ast.LetStatement{
            .identifier = ident,
            .value = try self.parseExpression(),
        };

        return res;
    }

    pub fn parseExpression(self: *Self) ParseError!ast.Expression {
        const token: ast.Expression = switch (self.curr_token.?) {
            .integer => |val| .{
                .integer_literal = std.fmt.parseInt(i64, val, 10) catch {
                    return ParseError.InvalidInteger;
                },
            },

            .keyword => |val| switch (val) {
                .true_token => .{ .boolean = true },
                .false_token => .{ .boolean = false },
                else => return ParseError.IllegalKeyword,
            },

            .ident => |ident| blk: {
                if (std.meta.eql(self.peek_token, .lparen)) {
                    const expr = self.alloc.create(ast.FunctionCall) catch @panic("unable to allocate");
                    expr.* = try self.parseFunctionCallExpression(ident);

                    break :blk .{ .function_call = expr };
                }
                break :blk .{ .identifier = ident };
            },
            .string_literal => |value| blk: {
                break :blk .{ .string_literal = value };
            },

            .eof => unreachable,

            else => return ParseError.ExpressionExpected,
        };

        self.nextToken();

        if (self.curr_token.? == .semicolon) {
            return token;
        }

        _ = self.scanOperator() catch return token;

        return .{
            .operator = try self.parseBinaryExpression(token),
        };
    }

    // TODO: operator precedence
    // A discussion with Casey Muratori about how easy precedence is...
    // https://www.youtube.com/watch?v=fIPO4G42wYE&t=6165s
    fn parseBinaryExpression(self: *Self, left: ast.Expression) ParseError!*ast.OperatorExpression {
        const op = try self.parseOperator();

        const expr = self.alloc.create(ast.OperatorExpression) catch @panic("unable to allocate");
        expr.* = ast.OperatorExpression{
            .left = left,
            .operator = op,
            .right = try self.parseExpression(),
        };

        return expr;
    }

    pub fn printDebug(self: *Self, message: []const u8, err: bool) void {
        if (err) {
            var iter = std.mem.splitSequence(u8, self.lexer.content, "\n");
            var number: u32 = 0;
            while (iter.next()) |line| {
                number += 1;
                if (self.lexer.line == number) {
                    const linenumber = std.fmt.allocPrint(self.alloc, "{d}: ", .{number}) catch unreachable;
                    std.debug.print("{s}{s}\n", .{ linenumber, line });
                    const padding = utils.repeatString(self.alloc, " ", line.len + linenumber.len - 1) catch unreachable;
                    std.debug.print("{s}^\n", .{padding});
                }
            }
        }
        std.debug.print("{s}:\ncurr_token: {any}\npeek_token: {any}\n", .{
            message,
            self.curr_token,
            self.peek_token,
        });
    }
};

const assertEq = std.testing.expectEqualDeep;

fn expectEqualAst(content: []const u8, expected: []const u8) !void {
    // var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const alloc = arena.allocator();

    var parser = Parser.new(content, alloc);
    const nodes = try parser.parseNodes();
    var result = Array(u8).init(alloc);

    const pretty = @import("./pretty/src/pretty.zig");
    try pretty.printWriter(alloc, result.writer(), nodes, .{
        .ptr_skip_dup_unfold = false,
        .array_show_item_idx = false,
        .show_type_names = false,
    });

    const trimmed = std.mem.trim(u8, result.items, " ␃\n");
    std.testing.expectEqualStrings(expected, trimmed) catch |err| {
        const file = try std.fs.cwd().createFile("actual", .{});
        defer file.close();
        try file.writeAll(trimmed);
        return err;
    };

    arena.deinit();
}

test "Parse - basic integer let statement" {
    try expectEqualAst("let name = 25;",
        \\.statement:
        \\  .let:
        \\    .identifier: "name"
        \\    .value:
        \\      .integer_literal: 25
    );
}

// FIX: parse fails if the first statement doesn't include space before =
// probably caused by parser initialization step
// test "Parse - basic integer let statement without spaces" {
//     const content = "let name=25;";
//     const tree = ast.Statement{
//         .let = .{ .identifier = "name", .value = .{ .integer_literal = 25 } },
//     };
//     var parser = Parser.new(content, std.heap.page_allocator);
//     try assertEq(try parser.parseStatement(), tree);
// }

test "Parse - return" {
    try expectEqualAst("return 2500 - 10;",
        \\.statement:
        \\  .return_:
        \\    .value:
        \\      .operator:
        \\        .left:
        \\          .integer_literal: 2500
        \\        .operator: .subtract
        \\        .right:
        \\          .integer_literal: 10
    );
    // var parser = Parser.new(content, std.heap.page_allocator);
    //
    // // Probably the same bug as above ^
    // // FIX: return statement semicolon doesn't get parsed because of lexer advancing twice at start??
    // // this only happens when passing single integer_literal
    // // failing input: const content = "return 2500;";
    // // working input: const content = "return 2500 ;";
    //
    // const expr = try parser.alloc.create(ast.OperatorExpression);
    // expr.* = ast.OperatorExpression{
    //     .left = .{ .integer_literal = 2500 },
    //     .operator = .subtract,
    //     .right = .{ .integer_literal = 10 },
    // };
    // const tree = ast.Statement{
    //     .return_ = .{ .value = .{ .operator = expr } },
    // };
    //
    // try assertEq(try parser.parseStatement(), tree);
}

test "Parse - operator expressions" {
    try expectEqualAst("let name = 25*10;",
        \\.statement:
        \\  .let:
        \\    .identifier: "name"
        \\    .value:
        \\      .operator:
        \\        .left:
        \\          .integer_literal: 25
        \\        .operator: .multiply
        \\        .right:
        \\          .integer_literal: 10
    );
}

test "Parse - nested operator expressions" {
    try expectEqualAst("let name = 25 * 10 -50;",
        \\.statement:
        \\  .let:
        \\    .identifier: "name"
        \\    .value:
        \\      .operator:
        \\        .left:
        \\          .integer_literal: 25
        \\        .operator: .multiply
        \\        .right:
        \\          .operator:
    );
}
