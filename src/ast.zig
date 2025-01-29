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
    prefix_operator: *PrefixBinaryExpression,
    infix_operator: *InfixBinaryExpression,
    index: *IndexExpression,
    integer_literal: i64,
    identifier: []const u8,
    function_call: *FunctionCall,
    match: *MatchExpression,
    string_literal: []const u8,
    boolean: bool,
    list: []Expression,
};

pub const GroupedExpression = struct {
    expression: Expression,
};

/// range holding left/right expressions of unknown type
pub const RangeExpression = struct {
    left: Expression,
    right: Expression,
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

pub const MatchExpression = struct {
    value: Expression,
    arms: []MatchArm,
};

pub const MatchArm = struct {
    patterns: []Pattern,
    consequence: Expression,
};

/// range pattern with literal integer values
pub const RangePattern = struct {
    left: i64,
    right: i64,
};

/// enum representing all valid pattern kinds
pub const Pattern = union(enum) {
    range: RangePattern,
    literal: Expression,
    catch_all,
};

pub const Identifier = []const u8;

// TODO: remove this in favor if ident alias ^
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

// pub const ForStatement = struct {
//     identifiers: [][]const u8,
//     value: Expression,
// };

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

pub const IndexExpression = struct {
    value: Expression,
    index: Expression,
};

pub const InfixOperator = enum {
    equal,
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
