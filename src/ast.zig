const std = @import("std");
const lex = @import("lexer.zig");

pub const BlockStatement = struct {
    nodes: []const Node,
};

pub const Node = union(enum) {
    statement: Statement,
    expression: Expression,
};

pub const Statement = union(enum) {
    let: LetStatement,
    function: FunctionStatement,
    return_: ReturnStatement,
    if_else: IfStatement,
    assign: AssingStatement,
    // block: BlockStatement,
};

pub const Expression = union(enum) {
    operator: *OperatorExpression,
    integer_literal: i64,
    identifier: []const u8,
    function_call: *FunctionCall,
    string_literal: []const u8,
    boolean: bool,
};

pub const AssingStatement = struct {
    identifier: []const u8,
    value: Expression,
};

pub const LetStatement = struct {
    identifier: []const u8,
    value: Expression,
    mutable: bool,
};

pub const IfStatement = struct {
    condition: Expression,
    consequence: BlockStatement,
    alternative: ?BlockStatement,
};

pub const FunctionArgument = struct {
    identifier: []const u8,
};

pub const FunctionStatement = struct {
    identifier: []const u8,
    arguments: []FunctionArgument,
    body: BlockStatement,
};

pub const FunctionCall = struct {
    identifier: []const u8,
    arguments: []Expression,
};

pub const ReturnStatement = struct {
    value: Expression,
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
    less_than_or_eq,
    less_than,
    greater_than,
    greater_than_or_eq,
    and_operator,
    or_operator,

    // string concatenation with: ..
    concat,
};
