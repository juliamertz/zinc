const std = @import("std");
const ast = @import("../ast.zig");
const values = @import("values.zig");

const EvalError = error{
    MismatchedTypes,
    NoSuchVariable,
    NoSuchFunction,
    IllegalOperator,
    IllegalReturn,
    NotAFunction,
};

pub const Scope = struct {
    parent: ?*Scope,
    variables: std.StringHashMap(values.Value),
};

pub const Interpreter = struct {
    root: Scope,
    alloc: std.mem.Allocator,

    const Self = @This();

    pub fn new(alloc: std.mem.Allocator) Self {
        return Self{
            .alloc = alloc,
            .root = Scope{
                .parent = null,
                .variables = std.StringHashMap(values.Value).init(alloc),
            },
        };
    }

    pub fn evaluate(self: *Self, module: ast.BlockStatement) !void {
        for (module.nodes) |node|
            _ = try self.evaluateNode(node, &self.root);

        try self.printDebug();
    }

    fn evaluateBlock(self: *Self, module: ast.BlockStatement) EvalError!values.Value {
        var res: ?values.Value = null;
        for (module.nodes, 0..) |node, i| {
            const val = try self.evaluateNode(node, &self.root);
            if (i == module.nodes.len - 1) {
                res = val;
            }
        }
        return res;
    }

    fn evaluateNode(self: *Self, node: ast.Node, scope: *Scope) EvalError!values.Value {
        switch (node) {
            .statement => |s| {
                try self.evaluateStatement(s, scope);
                return values.Value.null;
            },
            .expression => |e| {
                return try self.evaluateExpression(e, scope);
            },
        }
    }

    fn evaluateStatement(self: *Self, statement: ast.Statement, scope: *Scope) EvalError!void {
        return switch (statement) {
            .function => |func| {
                scope.variables.put(
                    func.identifier,
                    .{ .function = func },
                ) catch @panic("unable to append");
            },
            .let => |let| {
                scope.variables.put(
                    let.identifier,
                    try self.evaluateExpression(let.value, scope),
                ) catch @panic("unable to append");
            },
            .if_else => |cond| {
                const value = try self.evaluateExpression(cond.condition, scope);
                switch (value) {
                    .boolean => |do| {
                        if (do) {}
                    },
                    else => @panic("boolean expected"),
                }
            },
            .return_ => EvalError.IllegalReturn,
        };
    }

    fn evaluateExpression(self: *Self, expr: ast.Expression, scope: *Scope) EvalError!values.Value {
        return switch (expr) {
            .integer_literal => |val| .{ .integer = val },
            .operator => |val| try self.evaluateOperatorExpression(val.*, scope),
            .identifier => |val| {
                return scope.variables.get(val) orelse EvalError.NoSuchVariable;
            },
            .boolean => |val| .{ .boolean = val },
            .string_literal => |val| .{ .string = val },
            .function_call => |func| {
                if (scope.variables.get(func.identifier)) |ptr| {
                    const func_ptr: ast.FunctionStatement = switch (ptr) {
                        .function => |v| v,
                        else => return EvalError.NotAFunction,
                    };

                    const function_scope = scope;
                    for (func.arguments, 0..) |argument, index| {
                        const identifier = func_ptr.arguments[index].identifier;
                        const value = try self.evaluateExpression(argument, function_scope);
                        function_scope.variables.put(identifier, value) catch @panic("unable to append function to scope");
                    }

                    const res = self.evaluateFunction(func_ptr, function_scope);
                    return res;
                }

                return EvalError.NoSuchFunction;
            },
        };
    }

    fn evaluateFunction(self: *Self, func: ast.FunctionStatement, scope: *Scope) !values.Value {
        for (func.body.nodes) |node| {
            switch (node) {
                .statement => |statement| {
                    switch (statement) {
                        .return_ => |val| return try self.evaluateExpression(val.value, scope),
                        else => {
                            try self.evaluateStatement(statement, &self.root);
                        },
                    }
                },
                .expression => |expr| {
                    return try self.evaluateExpression(expr, scope);
                },
            }
        }

        return .null;
    }

    pub fn printDebug(self: *Self) !void {
        const stdout = std.io.getStdOut().writer();

        var variables = self.root.variables.iterator();
        while (variables.next()) |variable| {
            const value: values.Value = variable.value_ptr.*;
            switch (value) {
                .string => |str| try stdout.print("identifier: {s}: type: string, value: {s}\n", .{ variable.key_ptr.*, str }),
                else => try stdout.print("{s}: {any}\n", .{ variable.key_ptr.*, value }),
            }
        }
    }

    fn evaluateIntegerOperatorExpression(op: ast.Operator, left: i64, right: i64) EvalError!i64 {
        return switch (op) {
            .add => left + right,
            .subtract => left - right,
            .multiply => left * right,
            .divide => @divTrunc(left, right),

            else => EvalError.IllegalOperator,
        };
    }

    fn evaluateBooleanOperatorExpression(op: ast.Operator, left: bool, right: bool) EvalError!bool {
        return switch (op) {
            .and_operator => left and right,
            .or_operator => left or right,

            else => EvalError.IllegalOperator,
        };
    }

    fn evaluateStringOperatorExpression(self: *Self, op: ast.Operator, left: []const u8, right: []const u8) EvalError![]const u8 {
        return switch (op) {
            .concat => {
                const buff = [2][]const u8{ left, right };
                const result = std.mem.join(self.alloc, "", &buff) catch @panic("can't join strings");
                return result;
            },

            else => EvalError.IllegalOperator,
        };
    }

    fn evaluateOperatorExpression(self: *Self, expr: ast.OperatorExpression, scope: *Scope) EvalError!values.Value {
        const left = try self.evaluateExpression(expr.left, scope);
        const right = try self.evaluateExpression(expr.right, scope);

        return switch (left) {
            .integer => |l_val| switch (right) {
                .integer => |r_val| .{
                    .integer = try evaluateIntegerOperatorExpression(expr.operator, l_val, r_val),
                },
                else => @panic("todo"),
            },
            .boolean => |l_val| switch (right) {
                .boolean => |r_val| .{
                    .boolean = try evaluateBooleanOperatorExpression(expr.operator, l_val, r_val),
                },
                else => @panic("todo"),
            },
            .string => |l_val| switch (right) {
                .string => |r_val| blk: {
                    break :blk .{
                        .string = try self.evaluateStringOperatorExpression(expr.operator, l_val, r_val),
                    };
                },
                else => @panic("todo"),
            },
            else => @panic("todo"),
        };
    }
};
