const std = @import("std");
const lex = @import("lexer.zig");

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

pub const Expression = union(enum) {
    // need to use pointer to avoid allocating struct of infinite size
    operator: *const OperatorExpression,
    integer_literal: i64,
};

pub const Operator = enum {
    add,
    subtract,
    multiply,
    divide,

    assign,
    equal,
    not_equal,
    less_or_eq,
    less_than,
    greater_than,
    greater_or_eq,

    // string concatenation with: ..
    concat,
};

pub const OperatorExpression = struct {
    left: Expression,
    operator: Operator,
    right: Expression,
};

pub const Module = struct {
    statements: []Statement,
};
