const std = @import("std");
const lex = @import("lexer.zig");

pub const Block = struct {
    nodes: []const Node,
};

pub const Node = union(enum) {
    statement: Statement,
    expression: Expression,
    block: Block,
};

pub const Statement = union(enum) {
    let: LetStatement,
    function: FunctionStatement,
    return_: ReturnStatement,
    if_else: IfStatement,
    assign: AssingStatement,
};

pub const Expression = union(enum) {
    grouped_expression: *GroupedExpression,
    infix_operator: *InfixBinaryExpression,
    prefix_operator: *PrefixBinaryExpression,
    integer_literal: i64,
    identifier: []const u8,
    function_call: *FunctionCall,
    match: *MatchExpression,
    string_literal: []const u8,
    boolean: bool,
};

pub const GroupedExpression = struct {
    expression: Expression,
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
    consequence: Block,
    alternative: ?Block,
};

pub const MatchArm = struct {
    pattern: Pattern, // TODO: actual pattern parsing
    consequence: Expression,
};

pub const MatchExpression = struct {
    value: Expression,
    arms: []MatchArm,
};

pub const IntegerRange = struct {
    from: i64,
    to: i64,
};

pub const Pattern = union(enum) {
    integer_range: IntegerRange,
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

pub const InfixBinaryExpression = struct {
    left: Expression,
    operator: InfixOperator,
    right: Expression,
};

pub const PrefixBinaryExpression = struct {
    left: PrefixOperator,
    right: Expression,
};

pub const PrefixOperator = enum {
    negate,
    minus,
};

// TODO: do we need to define precedence explicitly?
pub const InfixOperator = enum(u8) {
    equal = 0,
    assign,

    range,

    add,
    subtract,
    multiply,
    divide,

    not_equal,
    less_than_or_eq,
    less_than,
    greater_than,
    greater_than_or_eq,
    and_operator,
    or_operator,
};
