const std = @import("std");
const ast = @import("../ast.zig");
const values = @import("values.zig");

const StringHashMap = std.StringHashMap;
const Value = values.Value;

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
    variables: StringHashMap(Value),

    fn init(alloc: std.mem.Allocator, parent: ?*Scope) Scope {
        return Scope{
            .parent = parent,
            .variables = StringHashMap(Value).init(alloc),
        };
    }
};

pub const Interpreter = struct {
    root: Scope,
    alloc: std.mem.Allocator,

    const Self = @This();

    pub fn new(alloc: std.mem.Allocator) Self {
        return Self{
            .alloc = alloc,
            .root = Scope.init(alloc, null),
        };
    }

    pub fn evaluate(self: *Self, nodes: []ast.Node) !void {
        for (nodes) |node|
            _ = try self.evaluateNode(node, &self.root);

        try self.printDebug();
    }

    fn evaluateBlock(self: *Self, module: ast.Block, scope: *Scope) EvalError!Value {
        var res: Value = .null;
        for (module.nodes, 0..) |node, i| {
            const val = try self.evaluateNode(node, scope) orelse Value.null;
            if (i == module.nodes.len - 1) {
                res = val;
            }
        }
        return res;
    }

    fn evaluateNode(self: *Self, node: ast.Node, scope: *Scope) EvalError!?Value {
        switch (node) {
            .statement => |s| {
                if (try self.evaluateStatement(s, scope)) |value| {
                    return value;
                }
            },
            .expression => |e| {
                return try self.evaluateExpression(e, scope);
            },
            .block => |b| {
                return try self.evaluateBlock(b, scope);
            },
        }

        return Value.null;
    }

    fn evaluateStatement(self: *Self, statement: ast.Statement, scope: *Scope) EvalError!?Value {
        switch (statement) {
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
            .if_else => |stmnt| {
                const value = try self.evaluateExpression(stmnt.condition, scope);
                switch (value) {
                    .boolean => |do| {
                        if (do) {
                            var if_scope = Scope.init(self.alloc, scope);
                            // TODO: return last value without return keyword
                            _ = try self.evaluateBlock(stmnt.consequence, &if_scope);
                        }
                    },
                    else => @panic("boolean expected"),
                }
            },
            .assign => |stmnt| {
                const exists = scope.variables.contains(stmnt.identifier);
                if (!exists) {
                    return EvalError.NoSuchVariable;
                }

                scope.variables.put(
                    stmnt.identifier,
                    try self.evaluateExpression(stmnt.value, scope),
                ) catch @panic("unable to append");
            },
            .return_ => return EvalError.IllegalReturn,
        }

        return null;
    }

    fn evaluateExpression(self: *Self, expr: ast.Expression, scope: *Scope) EvalError!Value {
        return switch (expr) {
            .integer_literal => |val| .{ .integer = val },
            .infix_operator => |val| try self.evaluateInfixBinaryExpression(val.*, scope),
            .prefix_operator => |val| try self.evaluatePrefixBinaryExpression(val.*, scope),
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
            .grouped_expression => |group| {
                return try self.evaluateExpression(group.expression, scope);
            },

            else => @panic("todo"),
        };
    }

    fn evaluateFunction(self: *Self, func: ast.FunctionStatement, scope: *Scope) !Value {
        for (func.body.nodes) |node| {
            switch (node) {
                .statement => |statement| {
                    switch (statement) {
                        .return_ => |val| return try self.evaluateExpression(val.value, scope),
                        else => {
                            return try self.evaluateStatement(statement, &self.root) orelse Value.null;
                        },
                    }
                },
                .expression => |expr| {
                    return try self.evaluateExpression(expr, scope);
                },
                else => @panic("todo!"),
            }
        }

        return .null;
    }

    pub fn printDebug(self: *Self) !void {
        const stdout = std.io.getStdOut().writer();

        var variables = self.root.variables.iterator();
        while (variables.next()) |variable| {
            const value: Value = variable.value_ptr.*;
            switch (value) {
                .string => |str| try stdout.print("identifier: {s}: type: string, value: {s}\n", .{ variable.key_ptr.*, str }),
                else => try stdout.print("{s}: {any}\n", .{ variable.key_ptr.*, value }),
            }
        }
    }

    fn evaluateIntegerBinaryExpression(op: ast.InfixOperator, left: i64, right: i64) EvalError!Value {
        return switch (op) {
            .add => .{ .integer = left + right },
            .subtract => .{ .integer = left - right },
            .multiply => .{ .integer = left * right },
            .divide => .{ .integer = @divTrunc(left, right) },
            .greater_than => .{ .boolean = left > right },
            .greater_than_or_eq => .{ .boolean = left >= right },
            .less_than => .{ .boolean = left > right },
            .less_than_or_eq => .{ .boolean = left <= right },

            else => EvalError.IllegalOperator,
        };
    }

    fn evaluateBooleanBinaryExpression(op: ast.InfixOperator, left: bool, right: bool) EvalError!bool {
        return switch (op) {
            .and_operator => left and right,
            .or_operator => left or right,

            else => EvalError.IllegalOperator,
        };
    }

    fn evaluateStringBinaryExpression(self: *Self, op: ast.InfixOperator, left: []const u8, right: []const u8) EvalError![]const u8 {
        return switch (op) {
            .add => {
                const buff = [2][]const u8{ left, right };
                const result = std.mem.join(self.alloc, "", &buff) catch @panic("can't join strings");
                return result;
            },

            else => EvalError.IllegalOperator,
        };
    }

    fn evaluateInfixBinaryExpression(self: *Self, expr: ast.InfixBinaryExpression, scope: *Scope) EvalError!Value {
        const left = try self.evaluateExpression(expr.left, scope);
        const right = try self.evaluateExpression(expr.right, scope);

        return switch (left) {
            .integer => |l_val| switch (right) {
                .integer => |r_val| try evaluateIntegerBinaryExpression(expr.operator, l_val, r_val),
                else => EvalError.MismatchedTypes,
            },
            .boolean => |l_val| switch (right) {
                .boolean => |r_val| .{
                    .boolean = try evaluateBooleanBinaryExpression(expr.operator, l_val, r_val),
                },
                else => EvalError.MismatchedTypes,
            },
            .string => |l_val| switch (right) {
                .string => |r_val| blk: {
                    break :blk .{
                        .string = try self.evaluateStringBinaryExpression(expr.operator, l_val, r_val),
                    };
                },
                else => EvalError.MismatchedTypes,
            },
            else => @panic("todo"),
        };
    }

    fn evaluatePrefixBinaryExpression(self: *Self, expr: ast.PrefixBinaryExpression, scope: *Scope) EvalError!Value {
        const right = try self.evaluateExpression(expr.right, scope);
        return switch (expr.left) {
            .negate => switch (right) {
                .boolean => |val| .{ .boolean = !val },
                else => return EvalError.IllegalOperator,
            },
            .minus => switch (right) {
                .integer => |val| .{ .integer = -val },
                else => return EvalError.IllegalOperator,
            },
        };
    }
};
