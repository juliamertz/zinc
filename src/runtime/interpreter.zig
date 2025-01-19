const std = @import("std");
const ast = @import("../ast.zig");
const values = @import("values.zig");

const EvalError = error{
    MismatchedTypes,
    NoSuchVariable,
    IllegalOperator,
    IllegalReturn,
    NotAFunction,
};

pub const Scope = struct {
    parent: ?*Scope,
    variables: std.StringHashMap(values.Value),
};

pub const Interpreter = struct {
    module: ast.Block,
    root: Scope,
    alloc: std.mem.Allocator,

    const Self = @This();

    pub fn new(alloc: std.mem.Allocator, module: ast.Block) Self {
        return Self{
            .module = module,
            .alloc = alloc,
            .root = Scope{
                .parent = null,
                .variables = std.StringHashMap(values.Value).init(alloc),
            },
        };
    }

    pub fn evaluate(self: *Self) !void {
        for (self.module.statements) |statement|
            try self.evaluateStatement(statement, &self.root);

        try self.printDebug();
    }

    fn evaluateStatement(self: *Self, statement: ast.Statement, scope: *Scope) EvalError!void {
        return switch (statement) {
            .function => |func| {
                std.debug.print("functioning {s}\n", .{func.identifier});
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

            .return_ => EvalError.IllegalReturn,
        };
    }

    fn evaluateFunction(self: *Self, func: ast.FunctionStatement, _: []ast.FunctionArgument) !values.Value {
        for (func.body.statements) |statement| switch (statement) {
            else => try self.evaluateStatement(statement, &self.root),
        };
    }

    fn evaluateExpression(self: *Self, expr: ast.Expression, scope: *Scope) EvalError!values.Value {
        return switch (expr) {
            .integer_literal => |val| .{ .integer = val },
            .operator => |val| try self.evaluateOperatorExpression(val.*, scope),
            .identifier => |val| {
                return scope.variables.get(val) orelse EvalError.NoSuchVariable;
            },
            else => unreachable,
            // .function_call => |func| {
            //     if (scope.variables.get(func.identifier)) |ptr| {
            //         _ = switch (ptr) {
            //             .function => |v| v,
            //             else => return EvalError.NotAFunction,
            //         };
            //         // return self.evaluateFunction(thing, [])
            //     }
            // },
        };
    }

    pub fn printDebug(self: *Self) !void {
        const stdout = std.io.getStdOut().writer();

        var variables = self.root.variables.iterator();
        while (variables.next()) |variable| {
            try stdout.print("{s}: {any}\n", .{ variable.key_ptr.*, variable.value_ptr.* });
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
            else => @panic("todo"),
        };
    }
};
