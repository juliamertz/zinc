const std = @import("std");
const lex = @import("lexer.zig");

pub const Node = union(enum) {
    statement: Statement,
    expression: Expression,
};

pub const Block = struct {
    nodes: []const Node,
};

pub const LetStatement = struct {
    identifier: []const u8,
    value: Expression,
};

pub const FunctionArgument = struct {
    identifier: []const u8,
};

pub const FunctionStatement = struct {
    identifier: []const u8,
    arguments: []FunctionArgument,
    body: Block,
};

pub const FunctionCall = struct {
    identifier: []const u8,
    arguments: []Expression,
};

pub const ReturnStatement = struct {
    value: Expression,
};

pub const Statement = union(enum) {
    let: LetStatement,
    function: FunctionStatement,
    return_: ReturnStatement,
};

pub const Expression = union(enum) {
    operator: *OperatorExpression,
    integer_literal: i64,
    identifier: []const u8,
    function_call: *FunctionCall,
    string_literal: []const u8,
};

pub const OperatorExpression = struct {
    left: Expression,
    operator: Operator,
    right: Expression,
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
