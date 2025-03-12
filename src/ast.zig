const std = @import("std");
const lex = @import("lexer.zig");

pub const Identifier = []const u8;

pub const Module = struct {
    nodes: []const Node,
};

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
    @"return": ReturnStatement,
    assign: AssingStatement,
    for_loop: ForStatement,
    while_loop: WhileStatement,
};

pub const Expression = union(enum) {
    grouped_expression: *GroupedExpression,
    prefix_operator: *PrefixExpression,
    infix_operator: *InfixExpression,
    index: *IndexExpression,
    identifier: []const u8,
    if_else: *IfExpression,
    function_call: *FunctionCall,
    boolean: bool,
    list: []Expression,
    match: *MatchExpression,
    string_literal: []const u8,
    integer_literal: i64,
    function_literal: *FunctionLiteral,
    module_literal: *ModuleLiteral,

    const Self = @This();

    /// return string content for identifier or string literal
    pub fn text(self: Self) ?[]const u8 {
        return switch (self) {
            .string_literal => |val| val,
            .identifier => |val| val,
            else => null,
        };
    }
};

pub const GroupedExpression = struct {
    expression: Expression,
};

pub const ModuleField = struct {
    identifier: Identifier,
    value: Expression,
};

pub const ModuleLiteral = struct {
    fields: []ModuleField,
};

/// range holding left/right expressions of unknown type
pub const RangeExpression = struct {
    left: Expression,
    right: Expression,
};

pub const AssingStatement = struct {
    identifier: Identifier,
    value: Expression,
};

pub const LetStatement = struct {
    identifier: Identifier,
    value: Expression,
    mutable: bool,
};

pub const IfExpression = struct {
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
    irrefutable: Expression,
};

pub const FunctionArgument = struct {
    identifier: Identifier,
};

pub const FunctionStatement = struct {
    identifier: Identifier,
    arguments: []FunctionArgument,
    body: Block,
};

pub const FunctionLiteral = struct {
    arguments: []FunctionArgument,
    body: Block,
};

pub const FunctionCall = struct {
    function: Expression,
    arguments: []Expression,
};

pub const ForStatement = struct {
    identifiers: []Identifier,
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

pub const InfixExpression = struct {
    left: Expression,
    operator: InfixOperator,
    right: Expression,
};

pub const PrefixExpression = struct {
    left: PrefixOperator,
    right: Expression,
};

pub const IndexExpression = struct {
    value: Expression,
    index: Expression,
};

pub const PrefixOperator = enum {
    negate,
    minus,
};

pub const InfixOperator = enum {
    assign,
    chain,
    range,
    plus,
    minus,
    asterisk,
    slash,
    equals,
    not_equal,
    less_than_or_eq,
    less_than,
    greater_than,
    greater_than_or_eq,
    @"and",
    @"or",
};
