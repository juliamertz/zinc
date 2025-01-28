const std = @import("std");
const ast = @import("ast.zig");
const utils = @import("utils.zig");
const lex = @import("lexer.zig");

const Array = std.ArrayList;
const eql = std.meta.eql;
const debug = std.debug.print; // TODO: remove thi

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
    column: usize,
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
            .line = self.lexer.line,
            .column = self.lexer.column,
        };
        self.errors.append(err) catch @panic("unable to allocate for ParseError");
        return kind;
    }

    pub fn nextToken(self: *Self) void {
        self.curr_token = self.peek_token;
        self.lexer.advance();
        self.peek_token = self.lexer.readToken();
    }

    fn expectInfixOperator(self: *Self, expected: ast.InfixOperator) ParseErrorKind!void {
        const operator = self.scanInfixOperator() catch |err| {
            return self.newError(
                err,
                "expected an infix operator got: {any}",
                .{self.curr_token},
            );
        };

        if (operator != expected) {
            return self.newError(
                ParseErrorKind.InvalidToken, // TODO: InvalidOperator
                "invalid operator expected: {any}, got: {any}",
                .{ expected, operator },
            );
        }
    }

    fn expectKeyword(self: *Self, expected: lex.Keyword) ParseErrorKind!void {
        if (!eql(self.curr_token, .{ .keyword = expected })) {
            return ParseErrorKind.InvalidToken;
        } else self.nextToken();
    }

    fn expectToken(self: *Self, expected: lex.Token) !void {
        if (eql(self.curr_token, expected)) {
            return;
        }

        return self.newError(
            error.InvalidToken,
            "expected next token to be {any}, got {any} instead",
            .{ expected, self.curr_token },
        );
    }

    fn consumeToken(self: *Self, expected: lex.Token) !void {
        try self.expectToken(expected);
        self.nextToken();
    }

    pub fn parseNode(self: *Self) ParseErrorKind!ast.Node {
        switch (self.curr_token) {
            .keyword => return .{ .statement = try self.parseStatement() },
            .ident => return {
                const ident = try self.parseIdentifier();
                try self.consumeToken(.equal);
                const expr = try self.parseExpression();
                try self.consumeToken(.semicolon);

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
        try self.consumeToken(.lsquirly);
        const nodes = try self.parseNodes();
        try self.consumeToken(.rsquirly);
        return .{ .nodes = nodes };
    }

    fn parseIdentifier(self: *Self) ParseErrorKind![]const u8 {
        const ident: []const u8 = switch (self.curr_token) {
            .ident => |val| blk: {
                self.nextToken();
                break :blk val;
            },

            else => return self.newError(
                error.IdentifierExpected,
                "expected next to be token to be an identifier, got {any}",
                .{self.curr_token},
            ),
        };

        return ident;
    }

    fn parseIntegerLiteral(self: *Self) ParseErrorKind!i64 {
        const int = switch (self.curr_token) {
            .integer => |val| std.fmt.parseInt(i64, val, 10) catch {
                return self.newError(
                    error.InvalidInteger,
                    "expected next token to be a valid integer, got {any}",
                    .{self.curr_token},
                );
            },
            else => self.newError(
                error.IntegerExpected,
                "expected next token to be an integer, got {any}",
                .{self.curr_token},
            ),
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
                    break :blk .range;
                }
                return self.newError(
                    ParseErrorKind.OperatorExpected,
                    "use .. for range operator",
                    .{},
                );
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
                else => return self.newError(
                    error.IllegalKeyword,
                    "The keyword {any} is not allowed here",
                    .{val},
                ),
            },
            else => return self.newError(
                ParseErrorKind.OperatorExpected,
                "Operator expected, found {any}",
                .{self.curr_token},
            ),
        };
    }

    fn parseInfixOperator(self: *Self) ParseErrorKind!ast.InfixOperator {
        const operator = try self.scanInfixOperator();
        switch (operator) {
            .less_than_or_eq, .greater_than_or_eq, .range => self.nextToken(),
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
            .alternative = self.parseOptionalElseStatement() catch null, // FIX: legitimate errors while parsing else statements are ignored
        };
    }

    pub fn parseMatchExpression(self: *Self) ParseErrorKind!ast.MatchExpression {
        self.nextToken();
        const value = try self.parseExpression();
        try self.consumeToken(.lsquirly);

        var arms = Array(ast.MatchArm).init(self.alloc);
        while (self.curr_token != .rsquirly) {
            const arm = try self.parseMatchArm();
            arms.append(arm) catch @panic("unable to append");
        }

        try self.consumeToken(.rsquirly);

        return ast.MatchExpression{
            .value = value, // TODO:
            .arms = arms.items,
        };
    }

    pub fn parseMatchArm(self: *Self) ParseErrorKind!ast.MatchArm {
        const patterns = try self.parseMatchPatterns();

        // '->' to indicate end of patterns
        try self.consumeToken(.minus);
        try self.consumeToken(.rangle);

        const expr = try self.parseExpression();
        try self.consumeToken(.comma);

        const arm = ast.MatchArm{
            .patterns = patterns,
            .consequence = expr,
        };
        return arm;
    }

    pub fn parseMatchPattern(self: *Self) ParseErrorKind!ast.Pattern {
        switch (self.curr_token) {
            .keyword => |kw| switch (kw) {
                .else_token => {
                    debug("catch all found", .{});
                    self.nextToken();
                    return .{ .catch_all = {} };
                },
                else => return self.newError(
                    ParseErrorKind.IllegalKeyword,
                    "Only the 'else' keyword is allowed as a match pattern, found: {}",
                    .{kw},
                ),
            },
            // .string_literal => |val| {
            // },
            .integer => |val| {
                self.nextToken();

                if (self.curr_token != .dot) {
                    return .{ .integer_literal = val };
                }

                try self.expectInfixOperator(.range);
                self.nextToken();
                self.nextToken();

                const to = switch (self.curr_token) {
                    .integer => |rval| rval,
                    else => return self.newError(
                        ParseErrorKind.InvalidInteger,
                        "invalid integer: {any}",
                        .{self.curr_token},
                    ),
                };
                self.nextToken();

                return .{
                    .integer_range = ast.IntegerRange{
                        .from = val,
                        .to = to,
                    },
                };
            },

            else => return self.newError(
                ParseErrorKind.InvalidToken,
                "Invalid match pattern",
                .{},
            ),
        }
    }

    pub fn parseMatchPatterns(self: *Self) ParseErrorKind![]ast.Pattern {
        var patterns = Array(ast.Pattern).init(self.alloc);

        const pattern = try self.parseMatchPattern();
        patterns.append(pattern) catch @panic("unable to append pattern");

        while (self.curr_token == .pipe) {
            self.nextToken();
            const p = try self.parseMatchPattern();
            patterns.append(p) catch @panic("unable to append pattern");
        }

        return patterns.items;
    }

    pub fn parseFunctionCallExpression(self: *Self, ident: []const u8) ParseErrorKind!ast.FunctionCall {
        var arguments = Array(ast.Expression).init(self.alloc);
        self.nextToken();
        try self.consumeToken(.lparen);

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
        try self.consumeToken(lex.Token.lparen);

        var arguments = Array(ast.FunctionArgument).init(self.alloc);
        while (!eql(self.curr_token, .rparen)) {
            const arg = try self.parseFunctionArgument();
            arguments.append(arg) catch @panic("unable to append");
        }

        try self.consumeToken(lex.Token.rparen);

        const body = try self.parseBlock();

        try self.consumeToken(.semicolon);

        return ast.FunctionStatement{
            .identifier = ident,
            .arguments = arguments.items,
            .body = body,
        };
    }

    pub fn parseReturnStatement(self: *Self) ParseErrorKind!ast.ReturnStatement {
        self.nextToken();
        const expr = try self.parseExpression();
        try self.consumeToken(.semicolon);

        return ast.ReturnStatement{
            .value = expr,
        };
    }

    pub fn parseLetStatement(self: *Self) ParseErrorKind!ast.LetStatement {
        self.nextToken();

        const mutable = eql(self.curr_token, .{ .keyword = .mut });
        if (mutable) self.nextToken();

        const ident = try self.parseIdentifier();

        try self.expectInfixOperator(ast.InfixOperator.equal);
        self.nextToken();

        const res = ast.LetStatement{
            .identifier = ident,
            .value = try self.parseExpression(),
            .mutable = mutable,
        };

        try self.consumeToken(.semicolon);

        return res;
    }

    pub fn parseExpression(self: *Self) ParseErrorKind!ast.Expression {
        if (self.scanPrefixOperator()) |operator| {
            return .{ .prefix_operator = try self.parsePrefixBinaryExpression(operator) };
        }

        const token: ast.Expression = switch (self.curr_token) {
            .integer => |val| .{
                .integer_literal = val,
            },

            .keyword => |val| switch (val) {
                .true_token => .{ .boolean = true },
                .false_token => .{ .boolean = false },
                .match => {
                    const expr = self.alloc.create(ast.MatchExpression) catch @panic("unable to allocate");
                    expr.* = try self.parseMatchExpression();
                    return .{ .match = expr };
                },
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

            .lparen => blk: {
                const expr = self.alloc.create(ast.GroupedExpression) catch @panic("unable to allocate");
                expr.* = try self.parseGroupedExpression();
                break :blk .{ .grouped_expression = expr };
            },

            .eof => unreachable,

            else => return self.newError(ParseErrorKind.ExpressionExpected, "", .{}),
        };

        self.nextToken();
        if (self.curr_token == .semicolon or self.curr_token == .lsquirly) return token;

        _ = self.scanInfixOperator() catch return token;
        return .{
            .infix_operator = try self.parseInfixBinaryExpression(token),
        };
    }

    pub fn parseGroupedExpression(self: *Self) ParseErrorKind!ast.GroupedExpression {
        try self.consumeToken(.lparen);
        const expr = try self.parseExpression();

        try self.expectToken(.rparen);
        return .{ .expression = expr };
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

    pub fn printPosition(self: *Self, line: usize, column: usize) void {
        var iter = std.mem.splitSequence(u8, self.lexer.content, "\n");
        var i: u32 = 0;
        while (iter.next()) |content| {
            if (line == i) {
                const linenumber = std.fmt.allocPrint(self.alloc, "{d}: ", .{i + 1}) catch unreachable;
                const extra_padding = linenumber.len - 1 + column;
                std.debug.print("{s}{s}\n", .{ linenumber, content });
                const padding = utils.repeatString(self.alloc, " ", extra_padding) catch unreachable;
                std.debug.print("{s}^\n", .{padding});
            }
            i += 1;
        }
    }

    pub fn printDebug(self: *Self, message: []const u8, print_errors: bool) void {
        std.debug.print("{s}: curr_token: {any} peek_token: {any}\n", .{
            message,
            self.curr_token,
            self.peek_token,
        });

        if (print_errors) {
            for (self.errors.items) |err| {
                std.debug.print("error: {any} at {d}:{d}\n{s}\n", .{
                    err.kind,
                    err.line + 1,
                    err.column + 1,
                    err.message,
                });
                self.printPosition(err.line, err.column);
            }
        }
    }
};

fn expectEqualAst(content: []const u8, expected: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const alloc = arena.allocator();

    var parser = Parser.new(content, alloc);
    defer arena.deinit();
    const nodes = try parser.parseNodes();

    const pretty = @import("pretty");
    const result = try pretty.dump(alloc, nodes, .{
        .array_show_item_idx = false,
        .max_depth = std.math.maxInt(u8),
        .ptr_skip_dup_unfold = false,
        .show_type_names = false,
    });

    const trimmed = std.mem.trim(u8, result, " ␃\n");
    std.testing.expectEqualStrings(expected, trimmed) catch |err| {
        const file = try std.fs.cwd().createFile("actual", .{});
        defer file.close();
        try file.writeAll(trimmed);
        return err;
    };
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
        \\      .infix_operator:
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
        \\      .infix_operator:
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
        \\      .infix_operator:
        \\        .left:
        \\          .integer_literal: 25
        \\        .operator: .multiply
        \\        .right:
        \\          .infix_operator:
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
        \\}
    ,
        \\.statement:
        \\  .if_else:
        \\    .condition:
        \\      .infix_operator:
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

test "Parse - grouped expressions" {
    try expectEqualAst("let mygroup = 10 - (2 * 5);",
        \\.statement:
        \\  .let:
        \\    .identifier: "mygroup"
        \\    .value:
        \\      .infix_operator:
        \\        .left:
        \\          .integer_literal: 10
        \\        .operator: .subtract
        \\        .right:
        \\          .grouped_expression:
        \\            .expression:
        \\              .infix_operator:
        \\                .left:
        \\                  .integer_literal: 2
        \\                .operator: .multiply
        \\                .right:
        \\                  .integer_literal: 5
        \\    .mutable: false
    );
}

test "Parse - function call" {
    try expectEqualAst("let greeting = greet(\"bob\");",
        \\.statement:
        \\  .let:
        \\    .identifier: "greeting"
        \\    .value:
        \\      .function_call:
        \\        .identifier: "greet"
        \\        .arguments:
        \\          .string_literal: "bob"
        \\    .mutable: false
    );
}

test "Parse - match expression" {
    try expectEqualAst(
        \\let age_group = match 5 {
        \\  0..10 -> "teens",
        \\  10..20 -> "twenties",
        \\};
    ,
        \\.statement:
        \\  .let:
        \\    .identifier: "age_group"
        \\    .value:
        \\      .match:
        \\        .value:
        \\          .integer_literal: 5
        \\        .arms:
        \\          .pattern:
        \\            .integer_range:
        \\              .from: 0
        \\              .to: 10
        \\          .consequence:
        \\            .string_literal: "teens"
        \\          .pattern:
        \\            .integer_range:
        \\              .from: 10
        \\              .to: 20
        \\          .consequence:
        \\            .string_literal: "twenties"
        \\    .mutable: false
    );
}
