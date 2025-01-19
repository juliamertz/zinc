const std = @import("std");
const ast = @import("../ast.zig");
const values = @import("values.zig");

const EvalError = error{
    MismatchedTypes,
    NoSuchVariable,
    IllegalOperator,
};

pub const Scope = struct {
    parent: ?*Scope,
    variables: std.StringHashMap(values.Value),
};

pub const Interpreter = struct {
    module: ast.Module,
    root: Scope,
    alloc: std.mem.Allocator,

    const Self = @This();

    pub fn new(alloc: std.mem.Allocator, module: ast.Module) Self {
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
        for (self.module.statements) |s| switch (s) {
            .return_ => {
                std.debug.print("retuning", .{});
            },
            .let => |statement| {
                // std.debug.print("assigning {any}\n", .{statement});
                try self.root.variables.put(
                    statement.identifier,
                    try self.evaluateExpression(statement.value),
                );
            },
        };

        try self.printDebug();
    }

    pub fn printDebug(self: *Self) !void {
        const stdout = std.io.getStdOut().writer();

        var variables = self.root.variables.iterator();
        while (variables.next()) |variable| {
            try stdout.print("{s}: {any}\n", .{ variable.key_ptr.*, variable.value_ptr.* });
        }

        // try std.json.stringify(self.root.variables., .{}, stdout);
        // const pretty = @import("pretty");
        // try pretty.print(self.alloc, self.root.variables, .{});
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

    fn evaluateOperatorExpression(self: *Self, expr: ast.OperatorExpression) EvalError!values.Value {
        const left = try self.evaluateExpression(expr.left);
        const right = try self.evaluateExpression(expr.right);

        // if (@TypeOf(left) != @TypeOf(right)) {
        //     return EvalError.MismatchedTypes;
        // }

        return switch (left) {
            .integer => |l_val| switch (right) {
                .integer => |r_val| .{
                    .integer = try evaluateIntegerOperatorExpression(expr.operator, l_val, r_val),
                },
                else => @panic("todo"),
            },
            else => @panic("todo"),
        };

        // return switch (expr.left) {
        //     .integer_literal => |v| blk: {
        //         const vr = switch (expr.right) {
        //             .integer_literal => |val| val,
        //             else => unreachable,
        //         };
        //         break :blk .{ .integer = evaluateIntegerOperatorExpression(expr.operator, v, vr) };
        //     },
        //     .identifier => |v| blk: {
        //         const vr = switch (expr.right) {
        //             .identifier => |val| val,
        //             else => unreachable,
        //         };
        //         break :blk .{ .integer = evaluateIntegerOperatorExpression(expr.operator, v, vr) };
        //     },
        //     else => @panic("todo"),
        // };
    }

    fn evaluateExpression(self: *Self, expr: ast.Expression) EvalError!values.Value {
        return switch (expr) {
            .integer_literal => |val| .{ .integer = val },
            .operator => |val| try self.evaluateOperatorExpression(val.*),
            .identifier => |val| {
                return self.root.variables.get(val) orelse EvalError.NoSuchVariable;
            },
        };
    }
};
