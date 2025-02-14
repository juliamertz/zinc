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
    for_loop: ForStatement,
    while_loop: WhileStatement,
};

pub const Expression = union(enum) {
    grouped_expression: *GroupedExpression,
    prefix_operator: *PrefixBinaryExpression,
    infix_operator: *InfixBinaryExpression,
    index: *IndexExpression,
    identifier: []const u8,
    function_call: *FunctionCall,
    boolean: bool,
    list: []Expression,
    match: *MatchExpression,
    string_literal: []const u8,
    integer_literal: i64,
    function_literal: *FunctionLiteral,
    object_literal: *ObjectLiteral,
};

pub const Identifier = []const u8;

pub const GroupedExpression = struct {
    expression: Expression,
};

pub const ObjectField = struct {
    identifier: Identifier,
    value: Expression,
};

pub const ObjectLiteral = struct {
    fields: []ObjectField,
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

pub const Pattern = union(enum) {
    range: RangePattern,
    literal: Expression,
    irrefutable: Identifier,
};

pub const FunctionArgument = struct {
    identifier: Identifier,
};

pub const FunctionStatement = struct {
    identifier: []const u8,
    arguments: []FunctionArgument,
    body: Block,
};

pub const FunctionLiteral = struct {
    arguments: []FunctionArgument,
    body: Block,
};

pub const FunctionCall = struct {
    identifier: []const u8,
    arguments: []Expression,
};

pub const ForStatement = struct {
    identifiers: [][]const u8,
    value: Expression,
    body: Block,
};

pub const WhileStatement = struct {
    condition: Expression,
    body: Block,
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

pub const IndexExpression = struct {
    value: Expression,
    index: Expression,
};

pub const InfixOperator = enum {
    equal,
    assign,

    chain,
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
