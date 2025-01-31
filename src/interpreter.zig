const std = @import("std");
const ast = @import("ast.zig");
const values = @import("values.zig");

const Array = std.ArrayList;
const StringHashMap = std.StringHashMap;
const Value = values.Value;

const ErrorKind = error{
    MismatchedTypes,
    NoSuchVariable,
    NoSuchFunction,
    IllegalOperator,
    IllegalReturn,
    NotAFunction,
};

const EvalError = struct {
    kind: ErrorKind,
    message: []const u8,
};

pub const Scope = struct {
    parent: ?*Scope,
    variables: StringHashMap(Value),

    const Self = @This();

    fn init(alloc: std.mem.Allocator, parent: ?*Self) Self {
        return Scope{
            .parent = parent,
            .variables = StringHashMap(Value).init(alloc),
        };
    }

    fn exists(self: *Self, key: []const u8) bool {
        if (self.variables.contains(key)) {
            return true;
        } else if (self.parent) |parent| {
            return parent.exists(key);
        }

        return false;
    }

    fn retrieve(self: *Self, key: []const u8) ?Value {
        if (self.variables.get(key)) |val| {
            return val;
        } else if (self.parent) |parent| {
            return parent.retrieve(key);
        }

        return null;
    }

    // binds key to value in local scope
    fn bind() void {}

    // attempts to assign to existing key in local or any parent scope
    fn assignGlobal(self: *Self, key: []const u8, value: Value) void {
        self.variables.put(key, value) catch @panic("unable to append");
    }
};

pub const Interpreter = struct {
    root: Scope,
    alloc: std.mem.Allocator,
    errors: Array(EvalError),

    const Self = @This();

    pub fn new(alloc: std.mem.Allocator) Self {
        return Self{
            .alloc = alloc,
            .errors = Array(EvalError).init(alloc),
            .root = Scope.init(alloc, null),
        };
    }

    /// Append error message to self.errors
    fn errorMessage(self: *Self, kind: ErrorKind, comptime msg: []const u8, args: anytype) ErrorKind {
        const err = EvalError{
            .kind = kind,
            .message = std.fmt.allocPrint(self.alloc, msg, args) catch "failed to print message??",
        };
        self.errors.append(err) catch @panic("unable to allocate for ParseError");
        return kind;
    }

    pub fn evaluate(self: *Self, nodes: []ast.Node) !void {
        for (nodes) |node|
            _ = try self.evaluateNode(node, &self.root);

        try self.printDebug();
    }

    fn evaluateBlock(self: *Self, module: ast.Block, scope: *Scope) ErrorKind!Value {
        var res: Value = .null;
        for (module.nodes, 0..) |node, i| {
            const val = try self.evaluateNode(node, scope) orelse Value.null;
            if (i == module.nodes.len - 1) {
                res = val;
            }
        }
        return res;
    }

    fn evaluateNode(self: *Self, node: ast.Node, scope: *Scope) ErrorKind!?Value {
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

    fn evaluateStatement(self: *Self, statement: ast.Statement, scope: *Scope) ErrorKind!?Value {
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
                if (!scope.exists(stmnt.identifier)) {
                    return ErrorKind.NoSuchVariable;
                }

                scope.variables.put(
                    stmnt.identifier,
                    try self.evaluateExpression(stmnt.value, scope),
                ) catch @panic("unable to append");
            },
            .return_ => return ErrorKind.IllegalReturn,

            else => @panic("todo"),
        }

        return null;
    }

    fn evaluateExpression(self: *Self, expr: ast.Expression, scope: *Scope) ErrorKind!Value {
        return switch (expr) {
            .integer_literal => |val| .{ .integer = val },
            .infix_operator => |val| try self.evaluateInfixBinaryExpression(val.*, scope),
            .prefix_operator => |val| try self.evaluatePrefixBinaryExpression(val.*, scope),
            .identifier => |val| {
                return scope.retrieve(val) orelse ErrorKind.NoSuchVariable;
                // return scope.variables.get(val) orelse EvalError.NoSuchVariable;
            },
            .boolean => |val| .{ .boolean = val },
            .string_literal => |val| .{ .string = val },
            .function_call => |func| {
                if (scope.variables.get(func.identifier)) |ptr| {
                    const func_ptr: ast.FunctionStatement = switch (ptr) {
                        .function => |v| v,
                        else => return ErrorKind.NotAFunction,
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

                return ErrorKind.NoSuchFunction;
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

    fn evaluateIntegerBinaryExpression(op: ast.InfixOperator, left: i64, right: i64) ErrorKind!Value {
        return switch (op) {
            .add => .{ .integer = left + right },
            .subtract => .{ .integer = left - right },
            .multiply => .{ .integer = left * right },
            .divide => .{ .integer = @divTrunc(left, right) },
            .greater_than => .{ .boolean = left > right },
            .greater_than_or_eq => .{ .boolean = left >= right },
            .less_than => .{ .boolean = left > right },
            .less_than_or_eq => .{ .boolean = left <= right },

            else => ErrorKind.IllegalOperator,
        };
    }

    fn evaluateBooleanBinaryExpression(op: ast.InfixOperator, left: bool, right: bool) ErrorKind!bool {
        return switch (op) {
            .and_operator => left and right,
            .or_operator => left or right,

            else => ErrorKind.IllegalOperator,
        };
    }

    fn evaluateStringBinaryExpression(self: *Self, op: ast.InfixOperator, left: []const u8, right: []const u8) ErrorKind![]const u8 {
        return switch (op) {
            .add => {
                const buff = [2][]const u8{ left, right };
                const result = std.mem.join(self.alloc, "", &buff) catch @panic("can't join strings");
                return result;
            },

            else => ErrorKind.IllegalOperator,
        };
    }

    fn evaluateInfixBinaryExpression(self: *Self, expr: ast.InfixBinaryExpression, scope: *Scope) ErrorKind!Value {
        const left = try self.evaluateExpression(expr.left, scope);
        const right = try self.evaluateExpression(expr.right, scope);

        return switch (left) {
            .integer => |l_val| switch (right) {
                .integer => |r_val| try evaluateIntegerBinaryExpression(expr.operator, l_val, r_val),
                else => ErrorKind.MismatchedTypes,
            },
            .boolean => |l_val| switch (right) {
                .boolean => |r_val| .{
                    .boolean = try evaluateBooleanBinaryExpression(expr.operator, l_val, r_val),
                },
                else => ErrorKind.MismatchedTypes,
            },
            .string => |l_val| switch (right) {
                .string => |r_val| blk: {
                    break :blk .{
                        .string = try self.evaluateStringBinaryExpression(expr.operator, l_val, r_val),
                    };
                },
                else => ErrorKind.MismatchedTypes,
            },
            else => @panic("todo"),
        };
    }

    fn evaluatePrefixBinaryExpression(self: *Self, expr: ast.PrefixBinaryExpression, scope: *Scope) ErrorKind!Value {
        const right = try self.evaluateExpression(expr.right, scope);
        return switch (expr.left) {
            .negate => switch (right) {
                .boolean => |val| .{ .boolean = !val },
                else => return ErrorKind.IllegalOperator,
            },
            .minus => switch (right) {
                .integer => |val| .{ .integer = -val },
                else => return ErrorKind.IllegalOperator,
            },
        };
    }
};
