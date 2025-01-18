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

    pub fn jsonStringify(self: @This(), jws: anytype) !void {
        try jws.beginObject();

        switch (self) {
            .operator => |expr| {
                try jws.objectField("operator");
                try jws.write(expr.*);
            },
            .integer_literal => |expr| {
                try jws.objectField("integer_literal");
                try jws.write(expr);
            },
        }
        // var it = self.map.iterator();
        // while (it.next()) |kv| {
        //     try jws.objectField(kv.key_ptr.*);
        //     try jws.write(kv.value_ptr.*);
        // }
        // try jws.endObject();
    }
};

pub const OperatorExpression = struct {
    left: Expression,
    operator: lex.Token,
    right: Expression,
};

pub const Module = struct {
    statements: []Statement,
};
