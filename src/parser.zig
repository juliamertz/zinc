const std = @import("std");
const ast = @import("ast.zig");
const utils = @import("utils.zig");
const lex = @import("lexer.zig");

const Array = std.ArrayList;
const eql = std.meta.eql;

pub const ParseErrorKind = error{
    IdentifierExpected,
    AssignmentExpected,
    SemicolonExpected,
    IntegerExpected,
    OperatorExpected,
    ExpressionExpected,

    IllegalKeyword,
    IllegalIdentifier,

    UnexpectedEof,
    InvalidInteger,
    InvalidToken, // Maybe this is a bit too general
};

pub const ParseError = struct {
    kind: ParseErrorKind,
    line: usize,
    message: []const u8,
};

pub const Parser = struct {
    lexer: lex.Lexer,
    alloc: std.mem.Allocator,
    errors: Array(ParseError),

    curr_token: lex.Token,
    peek_token: lex.Token,

    const Self = @This();

    pub fn new(content: []const u8, alloc: std.mem.Allocator) Self {
        var lexer = lex.Lexer.new(content);
        const curr = lexer.readToken();
        lexer.advance();
        const next = lexer.readToken();

        return Parser{
            .lexer = lexer,
            .errors = Array(ParseError).init(alloc),
            .alloc = alloc,
            .curr_token = curr,
            .peek_token = next,
        };
    }

    pub fn newError(self: *Self, kind: ParseErrorKind, comptime msg: []const u8, args: anytype) ParseErrorKind {
        const err = ParseError{
            .kind = kind,
            .message = std.fmt.allocPrint(self.alloc, msg, args) catch "failed to print message??",
            .line = self.lexer.position,
        };
        self.errors.append(err) catch @panic("unable to allocate for ParseError");
        return kind;
    }

    pub fn nextToken(self: *Self) void {
        self.curr_token = self.peek_token;
        self.lexer.advance();
        self.peek_token = self.lexer.readToken();
    }

    fn expectInfixOperator(self: *Self, expected: ast.InfixOperator) bool {
        const operator = self.scanInfixOperator() catch {
            return false;
        };
        return operator == expected;
    }

    fn expectKeyword(self: *Self, expected: lex.Keyword) ParseErrorKind!void {
        if (!eql(self.curr_token, .{ .keyword = expected })) {
            return ParseErrorKind.InvalidToken;
        } else self.nextToken();
    }

    fn expectToken(self: *Self, expected: lex.Token) !void {
        if (eql(self.curr_token, expected)) {
            self.nextToken();
            return;
        }
        // // TODO: find out if zig has a nicer way of doing this
        // return switch (expected) {
        //     .lparen => ParseErrorKind.OpenParenExpected,
        //     .rparen => ParseErrorKind.CloseParenExpected,
        //     .lsquirly => ParseErrorKind.OpenSquirlyExpected,
        //     .rsquirly => ParseErrorKind.CloseSquirlyExpected,
        //     else => ParseErrorKind.InvalidToken,
        // };
        //

        return self.newError(
            ParseErrorKind.InvalidToken,
            "expected next token to be {any}, got {any} instead",
            .{ expected, self.curr_token },
        );
    }

    pub fn parseNode(self: *Self) ParseErrorKind!ast.Node {
        switch (self.curr_token) {
            .keyword => return .{ .statement = try self.parseStatement() },
            .ident => return {
                const ident = try self.parseIdentifier();
                try self.expectToken(.equal);
                const expr = try self.parseExpression();
                try self.expectToken(.semicolon);

                return .{
                    .statement = .{
                        .assign = .{
                            .identifier = ident,
                            .value = expr,
                        },
                    },
                };
            },
            else => {},
        }

        return .{ .expression = try self.parseExpression() };
    }

    pub fn parseNodes(self: *Self) ParseErrorKind![]ast.Node {
        var nodes = Array(ast.Node).init(self.alloc);

        while (!eql(self.curr_token, .eof)) {
            const node = self.parseNode() catch |err| {
                if (err == ParseErrorKind.ExpressionExpected and
                    eql(self.curr_token, .rsquirly))
                {
                    break;
                }
                return err;
            };

            nodes.append(node) catch @panic("unable to append");
        }

        return nodes.items;
    }

    pub fn parseBlock(self: *Self) ParseErrorKind!ast.Block {
        try self.expectToken(.lsquirly);
        const nodes = try self.parseNodes();
        try self.expectToken(.rsquirly);
        return .{ .nodes = nodes };
    }

    fn parseIdentifier(self: *Self) ParseErrorKind![]const u8 {
        const ident: []const u8 = switch (self.curr_token) {
            .ident => |val| blk: {
                self.nextToken();
                break :blk val;
            },
            else => return self.newError(ParseErrorKind.IdentifierExpected, "", .{}),
        };

        return ident;
    }

    fn parseIntegerLiteral(self: *Self) ParseErrorKind!i64 {
        const int = switch (self.curr_token) {
            .integer => |val| std.fmt.parseInt(i64, val, 10) catch {
                return self.newError(ParseErrorKind.InvalidInteger, "", .{});
            },
            else => self.newError(ParseErrorKind.IntegerExpected, "", .{}),
        };

        self.nextToken();
        return int;
    }

    fn scanPrefixOperator(self: *Self) ?ast.PrefixOperator {
        return switch (self.curr_token) {
            .minus => .minus,
            .bang => .negate,
            else => null,
        };
    }

    fn scanInfixOperator(self: *Self) ParseErrorKind!ast.InfixOperator {
        return switch (self.curr_token) {
            .equal => .equal,
            .plus => .add,
            .minus => .subtract,
            .asterisk => .multiply,
            .forward_slash => .divide,
            .dot => blk: {
                if (eql(self.peek_token, .dot)) {
                    break :blk .concat;
                }
                return self.newError(ParseErrorKind.OperatorExpected, "", .{});
            },
            .langle => blk: {
                if (self.peek_token == .equal) {
                    break :blk .less_than_or_eq;
                }
                break :blk .less_than;
            },
            .rangle => blk: {
                if (self.peek_token == .equal) {
                    break :blk .greater_than_or_eq;
                }
                break :blk .greater_than;
            },
            .keyword => |val| switch (val) {
                .and_token => .and_operator,
                .or_token => .or_operator,
                else => return self.newError(ParseErrorKind.IllegalIdentifier, "", .{}),
            },
            else => return self.newError(ParseErrorKind.OperatorExpected, "", .{}),
        };
    }

    fn parseInfixOperator(self: *Self) ParseErrorKind!ast.InfixOperator {
        const operator = try self.scanInfixOperator();
        switch (operator) {
            .less_than_or_eq, .greater_than_or_eq, .concat => self.nextToken(),
            else => {},
        }
        self.nextToken();
        return operator;
    }

    fn parsePrefixOperator(self: *Self) ParseErrorKind!ast.PrefixOperator {
        const operator = try self.scanPrefixOperator();
        self.nextToken();
        return operator;
    }

    pub fn parseStatement(self: *Self) ParseErrorKind!ast.Statement {
        const statement: ast.Statement = switch (self.curr_token) {
            .keyword => |kw| switch (kw) {
                .let => .{ .let = try self.parseLetStatement() },
                .return_token => .{ .return_ = try self.parseReturnStatement() },
                .function => .{ .function = try self.parseFunctionStatement() },
                .if_token => .{ .if_else = try self.parseIfElseStatement() },
                else => return self.newError(ParseErrorKind.InvalidToken, "", .{}),
            },
            else => return self.newError(ParseErrorKind.InvalidToken, "", .{}),
        };

        if (self.curr_token != lex.Token.semicolon) {
            if (self.curr_token == lex.Token.eof) {
                return statement;
            }
            return self.newError(ParseErrorKind.SemicolonExpected, "", .{});
        } else self.nextToken();

        return statement;
    }

    pub fn parseFunctionArgument(self: *Self) ParseErrorKind!ast.FunctionArgument {
        return ast.FunctionArgument{
            .identifier = try self.parseIdentifier(),
        };
    }

    fn parseOptionalElseStatement(self: *Self) ParseErrorKind!?ast.Block {
        try self.expectKeyword(.else_token);
        return try self.parseBlock();
    }

    pub fn parseIfElseStatement(self: *Self) ParseErrorKind!ast.IfStatement {
        self.nextToken();

        return ast.IfStatement{
            .condition = try self.parseExpression(),
            .consequence = try self.parseBlock(),
            .alternative = self.parseOptionalElseStatement() catch null, // TODO:
        };
    }

    pub fn parseFunctionCallExpression(self: *Self, ident: []const u8) ParseErrorKind!ast.FunctionCall {
        var arguments = Array(ast.Expression).init(self.alloc);

        self.nextToken();
        try self.expectToken(.lparen);

        while (!eql(self.curr_token, .rparen)) {
            const arg = try self.parseExpression();
            arguments.append(arg) catch @panic("unable to append");
        }

        try self.expectToken(.rparen);

        return ast.FunctionCall{
            .arguments = arguments.items,
            .identifier = ident,
        };
    }

    pub fn parseFunctionStatement(self: *Self) ParseErrorKind!ast.FunctionStatement {
        self.nextToken();

        const ident = try self.parseIdentifier();

        // arguments
        try self.expectToken(lex.Token.lparen);
        var arguments = Array(ast.FunctionArgument).init(self.alloc);
        while (!eql(self.curr_token, .rparen)) {
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

    pub fn parseReturnStatement(self: *Self) ParseErrorKind!ast.ReturnStatement {
        self.nextToken();

        return ast.ReturnStatement{
            .value = try self.parseExpression(),
        };
    }

    pub fn parseLetStatement(self: *Self) ParseErrorKind!ast.LetStatement {
        self.nextToken();

        const mutable = eql(self.curr_token, .{ .keyword = .mut });
        if (mutable) self.nextToken();

        const ident = try self.parseIdentifier();

        if (!self.expectInfixOperator(ast.InfixOperator.equal)) {
            return self.newError(self.newError(ParseErrorKind.AssignmentExpected, "", .{}), "", .{});
        } else {
            self.nextToken();
        }

        const res = ast.LetStatement{
            .identifier = ident,
            .value = try self.parseExpression(),
            .mutable = mutable,
        };

        return res;
    }

    pub fn parseExpression(self: *Self) ParseErrorKind!ast.Expression {
        if (self.scanPrefixOperator()) |operator| {
            return .{ .prefix_operator = try self.parsePrefixBinaryExpression(operator) };
        }

        const token: ast.Expression = switch (self.curr_token) {
            .integer => |val| .{
                .integer_literal = std.fmt.parseInt(i64, val, 10) catch {
                    return self.newError(ParseErrorKind.InvalidInteger, "", .{});
                },
            },

            .keyword => |val| switch (val) {
                .true_token => .{ .boolean = true },
                .false_token => .{ .boolean = false },
                else => return self.newError(ParseErrorKind.IllegalKeyword, "", .{}),
            },

            .ident => |ident| blk: {
                if (eql(self.peek_token, .lparen)) {
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

            else => return self.newError(ParseErrorKind.ExpressionExpected, "", .{}),
        };

        self.nextToken();
        if (self.curr_token == .semicolon) return token;
        _ = self.scanInfixOperator() catch return token;

        return .{
            .infix_operator = try self.parseInfixBinaryExpression(token),
        };
    }

    // TODO: operator precedence
    // A discussion with Casey Muratori about how easy precedence is...
    // https://www.youtube.com/watch?v=fIPO4G42wYE&t=6165s
    fn parseInfixBinaryExpression(self: *Self, left: ast.Expression) ParseErrorKind!*ast.InfixBinaryExpression {
        const op = try self.parseInfixOperator();

        const expr = self.alloc.create(ast.InfixBinaryExpression) catch @panic("unable to allocate");
        expr.* = ast.InfixBinaryExpression{
            .left = left,
            .operator = op,
            .right = try self.parseExpression(),
        };

        return expr;
    }

    fn parsePrefixBinaryExpression(self: *Self, left: ast.PrefixOperator) ParseErrorKind!*ast.PrefixBinaryExpression {
        const expr = self.alloc.create(ast.PrefixBinaryExpression) catch @panic("unable to allocate");
        self.nextToken();
        expr.* = ast.PrefixBinaryExpression{
            .left = left,
            .right = try self.parseExpression(),
        };
        return expr;
    }

    pub fn printDebug(self: *Self, message: []const u8, print_position: bool) void {
        if (print_position) {
            var iter = std.mem.splitSequence(u8, self.lexer.content, "\n");
            var number: u32 = 0;
            while (iter.next()) |line| {
                number += 1;
                if (self.lexer.line + 1 == number) {
                    const linenumber = std.fmt.allocPrint(self.alloc, "{d}: ", .{number}) catch unreachable;
                    std.debug.print("{s}{s}\n", .{ linenumber, line });
                    const padding = utils.repeatString(self.alloc, " ", line.len + linenumber.len - 1) catch unreachable;
                    std.debug.print("{s}^\n", .{padding});
                }
            }
        }
        std.debug.print("{s}: curr_token: {any} peek_token: {any}\n", .{
            message,
            self.curr_token,
            self.peek_token,
        });

        for (self.errors.items) |err| {
            std.debug.print("error: {any} at line {d}\n{s}\n", .{
                err.kind,
                err.line,
                err.message,
            });
        }
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

    const pretty = @import("pretty");
    try pretty.printWriter(alloc, result.writer(), nodes, .{
        .array_show_item_idx = false,
        .max_depth = std.math.maxInt(u8),
        .ptr_skip_dup_unfold = false,
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

test "Parse - let statements" {
    try expectEqualAst("let age=21;",
        \\.statement:
        \\  .let:
        \\    .identifier: "age"
        \\    .value:
        \\      .integer_literal: 21
        \\    .mutable: false
    );

    try expectEqualAst("let mut age = 21;",
        \\.statement:
        \\  .let:
        \\    .identifier: "age"
        \\    .value:
        \\      .integer_literal: 21
        \\    .mutable: true
    );

    try expectEqualAst("let name = \"bob\";",
        \\.statement:
        \\  .let:
        \\    .identifier: "name"
        \\    .value:
        \\      .string_literal: "bob"
        \\    .mutable: false
    );
}

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

    try expectEqualAst("return 2500;",
        \\.statement:
        \\  .return_:
        \\    .value:
        \\      .integer_literal: 2500
    );
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
        \\    .mutable: false
    );
}

test "Parse - nested operator expressions" {
    try expectEqualAst("let name = 25 * 10 - 50;",
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
        \\            .left:
        \\              .integer_literal: 10
        \\            .operator: .subtract
        \\            .right:
        \\              .integer_literal: 50
        \\    .mutable: false
    );
}

test "Parse - conditionals" {
    try expectEqualAst(
        \\if 100 > 50 {
        \\  let a = "bigger";
        \\};
    ,
        \\.statement:
        \\  .if_else:
        \\    .condition:
        \\      .operator:
        \\        .left:
        \\          .integer_literal: 100
        \\        .operator: .greater_than
        \\        .right:
        \\          .integer_literal: 50
        \\    .consequence:
        \\      .nodes:
        \\        .statement:
        \\          .let:
        \\            .identifier: "a"
        \\            .value:
        \\              .string_literal: "bigger"
        \\            .mutable: false
        \\    .alternative: null
    );
}
