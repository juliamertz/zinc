const std = @import("std");
const lex = @import("./lexer.zig");

pub const Node = union(enum) {
    module: Module,
    statement: Statement,
    expression: Expression,
};

pub const LetStatement = struct {
    identifier: []const u8,
    value: Expression,
};

pub const Statement = union(enum) {
    let: LetStatement,
};

pub const OperatorExpression = struct {
    // TODO: recursive expression instead of int
    left: u32,
    operator: lex.Token,
    right: u32,
};

pub const Expression = union(enum) {
    operator: OperatorExpression,
};

pub const Module = struct {
    statements: []Statement,
};
