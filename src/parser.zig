const std = @import("std");
const ast = @import("ast.zig");
const utils = @import("utils.zig");
const lex = @import("lexer.zig");

const Array = std.ArrayList;
const eql = std.meta.eql;

const Precedence = enum {
    lowest,
    equals,
    less_greater,
    sum,
    product,
    prefix,
    call,

    const Self = @This();

    fn fromInfixOperator(op: ast.InfixOperator) Self {
        return switch (op) {
            .equal, .assign => .equals,
            .subtract, .add => .sum,
            .divide, .multiply => .product,

            .less_than_or_eq,
            .less_than,
            .greater_than,
            .greater_than_or_eq,
            => .less_greater,

            else => .lowest,
        };
    }
};

pub const ErrorKind = error{
    IdentifierExpected,
    AssignmentExpected,
    SemicolonExpected,
    IntegerExpected,
    OperatorExpected,
    ExpressionExpected,

    IllegalKeyword,
    IllegalIdentifier,

    InvalidInteger,
    InvalidOperator,
    InvalidToken,

    UnexpectedEof,
    OutOfMemory,
};

pub const ParseError = struct {
    kind: ErrorKind,
    message: []const u8,
    location: std.zig.Loc,
};

pub const Parser = struct {
    lexer: lex.Lexer,
    alloc: std.mem.Allocator,
    errors: Array(ParseError),

    curr_token: lex.Token,
    peek_token: lex.Token,

    const Self = @This();

    pub fn init(content: []const u8, alloc: std.mem.Allocator) Self {
        var lexer = lex.Lexer.new(content);
        const curr = lexer.readToken();
        lexer.advance();
        const peek = lexer.readToken();

        return Parser{
            .lexer = lexer,
            .errors = Array(ParseError).init(alloc),
            .alloc = alloc,
            .curr_token = curr,
            .peek_token = peek,
        };
    }

    /// Append error message to self.errors
    fn errorMessage(self: *Self, kind: ErrorKind, comptime msg: []const u8, args: anytype) ErrorKind {
        try self.errors.append(ParseError{
            .kind = kind,
            .message = std.fmt.allocPrint(self.alloc, msg, args) catch "failed to print message??",
            .location = std.zig.findLineColumn(self.lexer.content, self.lexer.position),
        });
        return kind;
    }

    /// Advance to the next token in the lexer
    fn next(self: *Self) void {
        self.curr_token = self.peek_token;
        self.lexer.advance();
        self.peek_token = self.lexer.readToken();
    }

    /// Check if the current token is equal to passed in infix operator
    fn expectInfixOperator(self: *Self, expected: ast.InfixOperator) ErrorKind!void {
        const operator = try self.scanInfixOperator();

        if (operator != expected) {
            return self.errorMessage(
                ErrorKind.InvalidOperator,
                "invalid operator expected: {any}, got: {any}",
                .{ expected, operator },
            );
        }
    }

    /// Check if current token is equal to passed in keyword
    /// throws an error if it does not match
    fn expectKeyword(self: *Self, expected: lex.Keyword) ErrorKind!void {
        if (!eql(self.curr_token, .{ .keyword = expected })) {
            return ErrorKind.InvalidToken;
        }
    }

    /// Check if current token is equal to passed in token
    /// throws an error if it does not match
    fn expectToken(self: *Self, expected: lex.Token) !void {
        if (eql(self.curr_token, expected)) {
            return;
        }

        return self.errorMessage(
            error.InvalidToken,
            "expected next token to be {any}, got {any} instead",
            .{ expected, self.curr_token },
        );
    }

    /// Skip current token if it is of the expected kind, otherwise do nothing
    fn skipToken(self: *Self, expected: lex.Token) void {
        if (eql(self.curr_token, expected)) {
            self.next();
        }
    }

    /// Check if current token is equal to passed in token, then advance.
    /// throws an error if it does not match
    fn consumeToken(self: *Self, expected: lex.Token) !void {
        try self.expectToken(expected);
        self.next();
    }

    /// Check if current token is equal to passed in token, then advance.
    /// throws an error if it does not match
    fn consumeKeyword(self: *Self, expected: lex.Keyword) !void {
        try self.expectKeyword(expected);
        self.next();
    }

    pub fn parseModule(self: *Self) ErrorKind!ast.Module {
        return .{ .nodes = try self.parseNodes() };
    }

    /// Parse nodes and accumulate into an `ArrayList`
    /// This will attempt to parse until `eof` is reached.
    pub fn parseNodes(self: *Self) ErrorKind![]ast.Node {
        var nodes = Array(ast.Node).init(self.alloc);

        while (!eql(self.curr_token, .eof)) {
            const node = self.parseNode() catch |err| {
                if (err == ErrorKind.ExpressionExpected and
                    eql(self.curr_token, .rsquirly)) break;

                return err;
            };

            try nodes.append(node);
        }

        return nodes.items;
    }

    fn parseNode(self: *Self) ErrorKind!ast.Node {
        return switch (self.curr_token) {
            .keyword => |kw| switch (kw) {
                .match, .@"if" => .{ .expression = try self.parseExpression(.lowest) },
                else => .{ .statement = try self.parseStatement() },
            },
            .ident => blk: {
                // TODO: make sure this doesn't interfere with equality expressions and it only matches assignment statements
                if (self.peek_token == .equal) {
                    const ident = try self.parseIdentifier();
                    try self.consumeToken(.equal);
                    const expr = try self.parseExpression(.lowest);
                    self.skipToken(.semicolon);

                    break :blk .{
                        .statement = .{
                            .assign = .{
                                .identifier = ident,
                                .value = expr,
                            },
                        },
                    };
                } else {
                    break :blk .{ .expression = try self.parseExpression(.lowest) };
                }
            },
            else => blk: {
                const expr = try self.parseExpression(.lowest);
                self.skipToken(.semicolon);
                break :blk .{ .expression = expr };
            },
        };
    }

    /// Parse block expression including surrounding curly braces
    fn parseBlock(self: *Self) ErrorKind!ast.Block {
        try self.consumeToken(.lsquirly);
        const nodes = try self.parseNodes();
        try self.consumeToken(.rsquirly);
        return .{ .nodes = nodes };
    }

    fn parseIdentifier(self: *Self) ErrorKind![]const u8 {
        const ident: []const u8 = switch (self.curr_token) {
            .ident => |val| blk: {
                self.next();
                break :blk val;
            },

            else => return self.errorMessage(
                error.IdentifierExpected,
                "expected next token to be an identifier, got {any}",
                .{self.curr_token},
            ),
        };

        return ident;
    }

    // Check if current token is a prefix operator
    fn scanPrefixOperator(self: *Self) ?ast.PrefixOperator {
        return switch (self.curr_token) {
            .minus => .minus,
            .bang => .negate,
            else => null,
        };
    }

    // Check if current token is an infix operator
    fn scanInfixOperator(self: *Self) ErrorKind!?ast.InfixOperator {
        return switch (self.curr_token) {
            .equal => switch (self.peek_token) {
                .equal => .equal,
                else => .assign,
            },
            .plus => .add,
            .minus => .subtract,
            .asterisk => .multiply,
            .forward_slash => .divide,
            .dot => switch (self.peek_token) {
                .dot => .range,
                else => .chain,
            },
            .langle => switch (self.peek_token) {
                .equal => .less_than_or_eq,
                else => .less_than,
            },
            .rangle => switch (self.peek_token) {
                .equal => .greater_than_or_eq,
                else => .greater_than,
            },
            .keyword => |val| switch (val) {
                .@"and" => .and_operator,
                .@"or" => .or_operator,
                else => return self.errorMessage(
                    error.IllegalKeyword,
                    "The keyword {any} is not allowed here",
                    .{val},
                ),
            },
            else => null,
        };
    }

    /// Wrapper around `scanInfixOperator` that returns an error if no infix operator is found at current position
    fn parseInfixOperator(self: *Self) ErrorKind!ast.InfixOperator {
        const operator = try self.scanInfixOperator() orelse return self.errorMessage(
            ErrorKind.OperatorExpected,
            "Expected an infix operator got: {any}",
            .{self.curr_token},
        );

        switch (operator) {
            .less_than_or_eq, .greater_than_or_eq, .range, .equal => self.next(),
            else => {},
        }
        self.next();
        return operator;
    }

    fn parsePrefixOperator(self: *Self) ErrorKind!ast.PrefixOperator {
        const operator = self.scanPrefixOperator() orelse return self.errorMessage(
            ErrorKind.OperatorExpected,
            "Expected an operator got: {any}",
            .{self.curr_token},
        );
        self.next();
        return operator;
    }

    fn parseIndexExpression(self: *Self, value: ast.Expression) ErrorKind!ast.IndexExpression {
        try self.consumeToken(.lbracket);
        const index = try self.parseExpression(.lowest);
        try self.consumeToken(.rbracket);
        return ast.IndexExpression{
            .index = index,
            .value = value,
        };
    }

    fn parseStatement(self: *Self) ErrorKind!ast.Statement {
        const statement: ast.Statement = switch (self.curr_token) {
            .keyword => |keyword| switch (keyword) {
                .let => .{ .let = try self.parseLetStatement() },
                .function => .{ .function = try self.parseFunctionStatement() },
                .@"return" => .{ .@"return" = try self.parseReturnStatement() },
                .@"for" => .{ .for_loop = try self.parseForStatement() },
                .@"while" => .{ .while_loop = try self.parseWhileStatement() },
                else => return self.errorMessage(
                    ErrorKind.IllegalKeyword,
                    "Invalid keyword {any} while parsing statement",
                    .{keyword},
                ),
            },
            else => return self.errorMessage(
                ErrorKind.InvalidToken,
                "Expected a keyword while parsing statement, found token: {any}",
                .{self.curr_token},
            ),
        };

        return statement;
    }

    fn parseFunctionArgument(self: *Self) ErrorKind!ast.FunctionArgument {
        return .{ .identifier = try self.parseIdentifier() };
    }

    fn parseOptionalElseStatement(self: *Self) ErrorKind!?ast.Block {
        try self.consumeKeyword(.@"else");
        return try self.parseBlock();
    }

    fn parseIfElseExpression(self: *Self) ErrorKind!ast.IfExpression {
        try self.consumeKeyword(.@"if");
        const expr = try self.parseExpression(.lowest);
        const block = try self.parseBlock();
        // FIX: legitimate errors while parsing else statements are ignored
        const alternative = self.parseOptionalElseStatement() catch null;

        return .{
            .condition = expr,
            .consequence = block,
            .alternative = alternative,
        };
    }

    fn parseMatchExpression(self: *Self) ErrorKind!ast.MatchExpression {
        self.next();
        const value = try self.parseExpression(.lowest);
        try self.consumeToken(.lsquirly);

        var arms = Array(ast.MatchArm).init(self.alloc);
        while (self.curr_token != .rsquirly) {
            const arm = try self.parseMatchArm();
            try arms.append(arm);
        }

        try self.consumeToken(.rsquirly);

        return ast.MatchExpression{
            .value = value,
            .arms = arms.items,
        };
    }

    fn parseMatchArm(self: *Self) ErrorKind!ast.MatchArm {
        const patterns = try self.parseMatchPatterns();

        // '->' to indicate end of patterns
        try self.consumeToken(.minus);
        try self.consumeToken(.rangle);

        const expr = try self.parseExpression(.lowest);
        try self.consumeToken(.comma);

        const arm = ast.MatchArm{
            .patterns = patterns,
            .consequence = expr,
        };
        return arm;
    }

    fn parseMatchPattern(self: *Self) ErrorKind!ast.Pattern {
        switch (self.curr_token) {
            .string_literal => |val| {
                self.next();
                return .{
                    .literal = .{ .string_literal = val },
                };
            },

            .integer => |val| {
                self.next();

                if (self.curr_token != .dot) {
                    return .{ .literal = .{ .integer_literal = val } };
                }

                try self.expectInfixOperator(.range);
                self.next();
                self.next();

                const to = switch (self.curr_token) {
                    .integer => |rval| rval,
                    else => return self.errorMessage(
                        ErrorKind.InvalidInteger,
                        "invalid integer: {any}",
                        .{self.curr_token},
                    ),
                };
                self.next();

                return .{
                    .range = ast.RangePattern{
                        .left = val,
                        .right = to,
                    },
                };
            },

            .lbracket => {
                return .{ .literal = .{ .list = try self.parseListExpression() } };
            },

            .ident => |ident| {
                self.next();
                return .{ .irrefutable = ident };
            },

            else => return self.errorMessage(
                ErrorKind.InvalidToken,
                "Invalid match pattern",
                .{},
            ),
        }
    }

    fn parseRangeExpression(self: *Self, left: ast.Expression) ErrorKind!ast.RangeExpression {
        try self.consumeToken(.dot);
        try self.consumeToken(.dot);
        return ast.RangeExpression{
            .left = left,
            .right = try self.parseExpression(.lowest),
        };
    }

    fn parseMatchPatterns(self: *Self) ErrorKind![]ast.Pattern {
        var patterns = Array(ast.Pattern).init(self.alloc);

        const pattern = try self.parseMatchPattern();
        try patterns.append(pattern);

        while (self.curr_token == .pipe) {
            self.next();
            const p = try self.parseMatchPattern();
            try patterns.append(p);
        }

        return patterns.items;
    }

    fn parseFunctionCallExpression(self: *Self, ident: []const u8) ErrorKind!ast.FunctionCall {
        var arguments = Array(ast.Expression).init(self.alloc);
        try self.consumeToken(.lparen);

        while (!eql(self.curr_token, .rparen)) {
            const arg = try self.parseExpression(.lowest);
            try arguments.append(arg);

            if (self.curr_token == .comma) {
                self.next();
            } else break;
        }

        try self.consumeToken(.rparen);

        return ast.FunctionCall{
            .arguments = arguments.items,
            .identifier = ident,
        };
    }

    fn parseFunctionLiteral(self: *Self) ErrorKind!ast.FunctionLiteral {
        try self.consumeKeyword(lex.Keyword.function);
        try self.consumeToken(lex.Token.lparen);

        var arguments = Array(ast.FunctionArgument).init(self.alloc);
        while (!eql(self.curr_token, .rparen)) {
            const arg = try self.parseFunctionArgument();
            try arguments.append(arg);
        }
        try self.consumeToken(lex.Token.rparen);
        const body = try self.parseBlock();

        return ast.FunctionLiteral{
            .arguments = arguments.items,
            .body = body,
        };
    }

    fn parseFunctionStatement(self: *Self) ErrorKind!ast.FunctionStatement {
        self.next();
        const ident = try self.parseIdentifier();

        try self.consumeToken(lex.Token.lparen);
        var arguments = Array(ast.FunctionArgument).init(self.alloc);
        while (!eql(self.curr_token, .rparen)) {
            const arg = try self.parseFunctionArgument();
            try arguments.append(arg);
        }
        try self.consumeToken(lex.Token.rparen);

        const body = try self.parseBlock();
        self.skipToken(.semicolon);

        return ast.FunctionStatement{
            .identifier = ident,
            .arguments = arguments.items,
            .body = body,
        };
    }

    fn parseReturnStatement(self: *Self) ErrorKind!ast.ReturnStatement {
        self.next();
        const expr = try self.parseExpression(.lowest);
        self.skipToken(.semicolon);

        return ast.ReturnStatement{
            .value = expr,
        };
    }

    fn parseLetStatement(self: *Self) ErrorKind!ast.LetStatement {
        self.next();

        const mutable = eql(self.curr_token, .{ .keyword = .mut });
        if (mutable) self.next();

        const ident = try self.parseIdentifier();
        try self.consumeToken(.equal);

        const res = ast.LetStatement{
            .identifier = ident,
            .value = try self.parseExpression(.lowest),
            .mutable = mutable,
        };

        self.skipToken(.semicolon);

        return res;
    }

    // TODO: think of a better name, this is too similar to `parsePrefixExpression` which has a different use
    fn parsePrefix(self: *Self) ErrorKind!ast.Expression {
        return switch (self.curr_token) {
            .ident => |ident| blk: {
                self.next();
                break :blk .{ .identifier = ident };
            },

            .integer => |val| blk: {
                self.next();
                break :blk .{ .integer_literal = val };
            },

            .bang, .minus => blk: {
                const operator = try self.parsePrefixOperator();
                break :blk .{
                    .prefix_operator = try self.parsePrefixExpression(operator),
                };
            },

            .keyword => |val| switch (val) {
                .true => blk: {
                    self.next();
                    break :blk .{ .boolean = true };
                },
                .false => blk: {
                    self.next();
                    break :blk .{ .boolean = false };
                },
                .function => blk: {
                    const func = try self.alloc.create(ast.FunctionLiteral);
                    func.* = try self.parseFunctionLiteral();
                    break :blk .{
                        .function_literal = func,
                    };
                },
                .match => blk: {
                    const expr = try self.alloc.create(ast.MatchExpression);
                    expr.* = try self.parseMatchExpression();
                    break :blk .{ .match = expr };
                },
                .@"if" => blk: {
                    const expr = try self.alloc.create(ast.IfExpression);
                    expr.* = try self.parseIfElseExpression();
                    break :blk .{ .if_else = expr };
                },
                else => return self.errorMessage(ErrorKind.IllegalKeyword, "", .{}),
            },
            .string_literal => |value| blk: {
                self.next();
                break :blk .{ .string_literal = value };
            },
            .lparen => blk: {
                const expr = try self.alloc.create(ast.GroupedExpression);
                expr.* = try self.parseGroupedExpression();
                break :blk .{ .grouped_expression = expr };
            },
            .lsquirly => blk: {
                const expr = try self.alloc.create(ast.ModuleLiteral);
                expr.* = try self.parseModuleLiteral();
                break :blk .{ .module_literal = expr };
            },
            .lbracket => .{ .list = try self.parseListExpression() },

            else => return self.errorMessage(
                ErrorKind.ExpressionExpected,
                "Expected a prefix expression found: {any}",
                .{self.curr_token},
            ),
        };
    }

    // TODO: rename function
    fn parseInfix(self: *Self, left: ast.Expression) ErrorKind!?ast.Expression {
        return switch (self.curr_token) {
            // TODO: This doesn't cover all infix cases, maybe attempt to parse an operator and switch on that?
            .plus,
            .minus,
            .forward_slash,
            .asterisk,
            .equal,
            .rangle,
            .langle,
            .dot,
            => .{ .infix_operator = try self.parseInfixExpression(left) },

            .lparen => blk: {
                const identifier = switch (left) {
                    .identifier => |value| value,
                    else => return self.errorMessage(ErrorKind.IdentifierExpected, "Expected identifer for function call got: {any}", .{self.curr_token}),
                };

                const call_expr = try self.alloc.create(ast.FunctionCall);
                call_expr.* = try self.parseFunctionCallExpression(identifier);

                // FIX: seems a little hacky to optionally recurse here
                // it works though ¯\_(ツ)_/¯
                // TODO: does this messes with operator precendence once implemented?
                const expr = ast.Expression{ .function_call = call_expr };
                if (try self.scanInfixOperator()) |_| {
                    break :blk self.parseInfix(expr);
                }
                break :blk expr;
            },

            .lbracket => blk: {
                const index_expr = try self.alloc.create(ast.IndexExpression);
                index_expr.* = try self.parseIndexExpression(left);
                break :blk .{ .index = index_expr };
            },

            else => null,
        };
    }

    fn parseExpression(self: *Self, _: Precedence) ErrorKind!ast.Expression {
        const leftExpr = try self.parsePrefix();
        // std.debug.print("precedence: {any} of value {d}\n", .{ precedence, @intFromEnum(precedence) });

        // TODO: do we even need this check?
        // switch (self.curr_token) {
        //     .semicolon => return leftExpr,
        //     else => {},
        // }

        // TODO: operator precedence
        if (try self.parseInfix(leftExpr)) |infix| {
            std.debug.print("found an infix for {any}: {any}\n", .{ leftExpr, infix });
            return infix;
        }

        return leftExpr;
    }

    fn parseForStatement(self: *Self) ErrorKind!ast.ForStatement {
        try self.expectKeyword(.@"for");
        self.next();

        var identifiers = Array(ast.Identifier).init(self.alloc);
        while (true) {
            switch (self.curr_token) {
                .keyword => |kw| if (kw == .in) break,
                .comma => self.next(),
                else => {},
            }
            const ident = try self.parseIdentifier();
            try identifiers.append(ident);
        }

        try self.expectKeyword(.in);
        self.next();

        const value = try self.parseExpression(.lowest);
        const block = try self.parseBlock();

        return ast.ForStatement{
            .value = value,
            .identifiers = identifiers.items,
            .body = block,
        };
    }

    fn parseWhileStatement(self: *Self) ErrorKind!ast.WhileStatement {
        try self.expectKeyword(.@"while");
        self.next();

        const condition = try self.parseExpression(.lowest);
        const block = try self.parseBlock();

        return ast.WhileStatement{
            .condition = condition,
            .body = block,
        };
    }

    fn parseModuleField(self: *Self) ErrorKind!ast.ModuleField {
        const ident = try self.parseIdentifier();
        try self.consumeToken(.colon);
        const value = try self.parseExpression(.lowest);

        return ast.ModuleField{
            .identifier = ident,
            .value = value,
        };
    }

    fn parseModuleLiteral(self: *Self) ErrorKind!ast.ModuleLiteral {
        try self.consumeToken(.lsquirly);

        var fields = Array(ast.ModuleField).init(self.alloc);
        while (self.curr_token != .rsquirly) {
            const field = try self.parseModuleField();
            try fields.append(field);
            if (self.curr_token == .comma) self.next();
        }

        try self.consumeToken(.rsquirly);
        return ast.ModuleLiteral{
            .fields = fields.items,
        };
    }

    /// Parse list of expressions seperated by comma's and surrounded by brackets
    fn parseListExpression(self: *Self) ErrorKind![]ast.Expression {
        try self.consumeToken(.lbracket);

        var expressions = Array(ast.Expression).init(self.alloc);
        while (self.curr_token != .rbracket) {
            const e = try self.parseExpression(.lowest);
            try expressions.append(e);
            if (self.curr_token == .comma) self.next(); // TODO: nice error if comma is missing between items
        }

        try self.consumeToken(.rbracket);
        return expressions.items;
    }

    /// Parse expression surrounded by parentheses
    fn parseGroupedExpression(self: *Self) ErrorKind!ast.GroupedExpression {
        try self.consumeToken(.lparen);
        const expr = try self.parseExpression(.lowest);
        try self.consumeToken(.rparen);
        return .{ .expression = expr };
    }

    fn parseInfixExpression(self: *Self, left: ast.Expression) ErrorKind!*ast.InfixExpression {
        const op = try self.parseInfixOperator();
        const expr = try self.alloc.create(ast.InfixExpression);

        const precendence = Precedence.fromInfixOperator(op);
        expr.* = ast.InfixExpression{
            .left = left,
            .operator = op,
            .right = try self.parseExpression(precendence),
        };

        return expr;
    }

    fn parsePrefixExpression(self: *Self, left: ast.PrefixOperator) ErrorKind!*ast.PrefixExpression {
        const expr = try self.alloc.create(ast.PrefixExpression);
        expr.* = ast.PrefixExpression{
            .left = left,
            .right = try self.parseExpression(.prefix),
        };
        return expr;
    }

    /// Print out current line's content with cursor
    /// pointing at the lexers current position
    fn printPosition(self: *Self, line: usize, column: usize) void {
        var iter = std.mem.splitSequence(u8, self.lexer.content, "\n");
        var i: u32 = 0;

        while (iter.next()) |content| {
            if (line == i) {
                const linenumber = std.fmt.allocPrint(self.alloc, "{d}: ", .{i + 1}) catch unreachable;
                const extra_padding = linenumber.len - 1 + column;
                std.log.debug("{s}{s}\n", .{ linenumber, content });
                const padding = utils.repeatString(self.alloc, " ", extra_padding) catch unreachable;
                std.log.debug("{s}^\n", .{padding});
            }
            i += 1;
        }
    }

    /// Print lexers current and next token, optionally printing `self.errors`
    pub fn debug(self: *Self, message: []const u8, print_errors: bool) void {
        std.log.debug("{s}: curr_token: {any} peek_token: {any}\n", .{
            message,
            self.curr_token,
            self.peek_token,
        });

        if (print_errors) {
            for (self.errors.items) |err| {
                std.log.err("{any} at {d}:{d}\n{s}\n", .{
                    err.kind,
                    err.location.line + 1,
                    err.location.column + 1,
                    err.message,
                });
                self.printPosition(err.location.line, err.location.column);
            }
        }
    }
};

inline fn expectAst(content: []const u8, expected: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const alloc = arena.allocator();

    var parser = Parser.init(content, alloc);
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
    try expectAst("let age=21;",
        \\.statement:
        \\  .let:
        \\    .identifier: "age"
        \\    .value:
        \\      .integer_literal: 21
        \\    .mutable: false
    );

    try expectAst("let mut age = 21;",
        \\.statement:
        \\  .let:
        \\    .identifier: "age"
        \\    .value:
        \\      .integer_literal: 21
        \\    .mutable: true
    );

    try expectAst("let name = \"bob\";",
        \\.statement:
        \\  .let:
        \\    .identifier: "name"
        \\    .value:
        \\      .string_literal: "bob"
        \\    .mutable: false
    );
}

test "Parse - return" {
    try expectAst("return 2500 - 10;",
        \\.statement:
        \\  .return:
        \\    .value:
        \\      .infix_operator:
        \\        .left:
        \\          .integer_literal: 2500
        \\        .operator: .subtract
        \\        .right:
        \\          .integer_literal: 10
    );

    try expectAst("return 2500;",
        \\.statement:
        \\  .return:
        \\    .value:
        \\      .integer_literal: 2500
    );
}

test "Parse - operator expressions" {
    try expectAst("let name = 25*10;",
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
    try expectAst("let name = 25 * 10 - 50;",
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
    try expectAst("some() + value()",
        \\.expression:
        \\  .infix_operator:
        \\    .left:
        \\      .function_call:
        \\        .identifier: "some"
        \\        .arguments: (empty)
        \\    .operator: .add
        \\    .right:
        \\      .function_call:
        \\        .identifier: "value"
        \\        .arguments: (empty)
    );
}

test "Parse - prefix operators" {
    try expectAst("!true",
        \\.expression:
        \\  .prefix_operator:
        \\    .left: .negate
        \\    .right:
        \\      .boolean: true
    );
    try expectAst("-20",
        \\.expression:
        \\  .prefix_operator:
        \\    .left: .minus
        \\    .right:
        \\      .integer_literal: 20
    );
}

test "Parse - conditionals" {
    try expectAst(
        \\if 100 > 50 {
        \\  let a = "bigger";
        \\}
    ,
        \\.expression:
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
    try expectAst("let mygroup = 10 - (2 * 5);",
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
    try expectAst("let greeting = greet(\"bob\");",
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
    try expectAst("greet(\"bob\")",
        \\.expression:
        \\  .function_call:
        \\    .identifier: "greet"
        \\    .arguments:
        \\      .string_literal: "bob"
    );
    try expectAst("concat(\"left\", \"middle\", \"right\")",
        \\.expression:
        \\  .function_call:
        \\    .identifier: "concat"
        \\    .arguments:
        \\      .string_literal: "left"
        \\      .string_literal: "middle"
        \\      .string_literal: "right"
    );
}

test "Parse - match expression" {
    try expectAst(
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
        \\          .patterns:
        \\            .range:
        \\              .left: 0
        \\              .right: 10
        \\          .consequence:
        \\            .string_literal: "teens"
        \\          .patterns:
        \\            .range:
        \\              .left: 10
        \\              .right: 20
        \\          .consequence:
        \\            .string_literal: "twenties"
        \\    .mutable: false
    );
}

test "Parse - list expression" {
    try expectAst("let items = [10, 20, \"thirty\"];",
        \\.statement:
        \\  .let:
        \\    .identifier: "items"
        \\    .value:
        \\      .list:
        \\        .integer_literal: 10
        \\        .integer_literal: 20
        \\        .string_literal: "thirty"
        \\    .mutable: false
    );
}

test "Parse - index expression" {
    try expectAst("my_list[10]",
        \\.expression:
        \\  .index:
        \\    .value:
        \\      .identifier: "my_list"
        \\    .index:
        \\      .integer_literal: 10
    );
}

test "Parse - for loop" {
    try expectAst(
        \\ for i, item in items {
        \\   let a = 10;
        \\ }
    ,
        \\.statement:
        \\  .for_loop:
        \\    .identifiers: "i"
        \\      "item"
        \\    .value:
        \\      .identifier: "items"
        \\    .body:
        \\      .nodes:
        \\        .statement:
        \\          .let:
        \\            .identifier: "a"
        \\            .value:
        \\              .integer_literal: 10
        \\            .mutable: false
    );
}

test "Parse - while loop" {
    try expectAst(
        \\ while true {
        \\   let b = 20;
        \\ }
    ,
        \\.statement:
        \\  .while_loop:
        \\    .condition:
        \\      .boolean: true
        \\    .body:
        \\      .nodes:
        \\        .statement:
        \\          .let:
        \\            .identifier: "b"
        \\            .value:
        \\              .integer_literal: 20
        \\            .mutable: false
    );
}

test "Parse - module definition" {
    try expectAst(
        \\{
        \\  intToStr: "todo"
        \\}
    ,
        \\.expression:
        \\  .module_literal:
        \\    .fields:
        \\      .identifier: "intToStr"
        \\      .value:
        \\        .string_literal: "todo"
    );
}

test "Parse - module field access" {
    try expectAst(
        \\builtins.intToStr(10)
    ,
        \\.expression:
        \\  .infix_operator:
        \\    .left:
        \\      .identifier: "builtins"
        \\    .operator: .chain
        \\    .right:
        \\      .function_call:
        \\        .identifier: "intToStr"
        \\        .arguments:
        \\          .integer_literal: 10
    );
}
