const std = @import("std");
const pretty = @import("pretty");
const ast = @import("../ast.zig");
const utils = @import("../utils.zig");
const builtins = @import("builtins.zig");

const Array = std.ArrayList;
const StringHashMap = std.StringHashMap;

pub const ErrorKind = error{
    MismatchedTypes,
    NoSuchVariable,
    NoSuchFunction,
    IllegalOperator,
    IllegalReturn,
    NotAFunction,
    OutOfMemory,
};

const EvalError = struct {
    kind: ErrorKind,
    message: []const u8,
};

pub const BuiltinPtr = *const fn (args: []const Value) ErrorKind!Value;

pub const Value = union(enum) {
    string: []const u8,
    integer: i64,
    boolean: bool,
    function: Function,
    builtin: BuiltinPtr,
    module: Module,
    null,
};

pub const Module = struct {
    root: *Scope,
};

const Function = struct {
    arguments: []ast.FunctionArgument,
    body: ast.Block,
};

pub const ValueBinding = struct {
    value: Value,
    mutable: bool,
};

// PERF: would merging scopes be faster than recursive lookup?
pub const Scope = struct {
    parent: ?*Scope,
    variables: StringHashMap(ValueBinding),

    const Self = @This();

    pub fn init(alloc: std.mem.Allocator, parent: ?*Self) Self {
        return Scope{
            .parent = parent,
            .variables = StringHashMap(ValueBinding).init(alloc),
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
        if (self.variables.get(key)) |binding| {
            return binding.value;
        } else if (self.parent) |parent| {
            return parent.retrieve(key);
        }

        return null;
    }

    /// binds key to value in local scope
    /// will overwrite any existing variable
    pub fn bind(self: *Self, key: []const u8, value: ValueBinding) void {
        self.variables.put(key, value) catch @panic("unable to append");
    }

    /// attempts to assign to existing key in local or any parent scope
    /// panics if binding is immutable
    fn assignGlobal(self: *Self, key: []const u8, value: Value) void {
        if (self.variables.getPtr(key)) |binding| {
            if (binding.mutable) {
                binding.value = value;
            } else @panic("attempted to re-assign to immutable value");
        } else if (self.parent) |parent| {
            parent.assignGlobal(key, value);
        }
    }
};

pub const Interpreter = struct {
    root: Scope,
    alloc: std.mem.Allocator,
    errors: Array(EvalError),

    const Self = @This();

    pub fn init(alloc: std.mem.Allocator) Self {
        var scope = Scope.init(alloc, null);

        // TODO: find cleaner way to do this
        // set up std lib module
        const builtins_module = builtins.module(alloc);
        scope.bind("builtins", .{ .value = .{ .module = builtins_module }, .mutable = false });

        return Self{
            .alloc = alloc,
            .errors = Array(EvalError).init(alloc),
            .root = scope,
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

    pub fn evaluateModule(self: *Self, module: ast.Module) !Module {
        var scope = Scope.init(self.alloc, null);
        for (module.nodes) |node|
            _ = try self.evaluateNode(node, &scope);

        try self.printDebug();

        return .{ .root = &scope };
    }

    /// Evaluate block line by line and return last value
    fn evaluateBlock(self: *Self, module: ast.Block, scope: *Scope) ErrorKind!Value {
        var res: Value = .null;
        for (module.nodes, 0..) |node, i| {
            if (try self.evaluateNode(node, scope)) |value| {
                if (i == module.nodes.len - 1) res = value;
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
                scope.bind(func.identifier, .{
                    .mutable = false,
                    .value = .{
                        .function = Function{
                            .arguments = func.arguments,
                            .body = func.body,
                        },
                    },
                });
            },
            .let => |let| {
                scope.bind(let.identifier, .{
                    .mutable = let.mutable,
                    .value = try self.evaluateExpression(let.value, scope),
                });
            },
            .assign => |stmnt| {
                if (!scope.exists(stmnt.identifier)) {
                    std.debug.print("tried to assign to non existing variable for assign: {s}\n", .{stmnt.identifier});
                    return ErrorKind.NoSuchVariable;
                } else {}

                scope.assignGlobal(
                    stmnt.identifier,
                    try self.evaluateExpression(stmnt.value, scope),
                );
            },

            .@"return" => return ErrorKind.IllegalReturn,

            else => std.debug.panic("Unhandled statement: {any}", .{statement}),
        }

        return null;
    }

    fn evaluateExpression(self: *Self, expr: ast.Expression, scope: *Scope) ErrorKind!Value {
        return switch (expr) {
            .integer_literal => |val| .{ .integer = val },
            .infix_operator => |val| try self.evaluateInfixBinaryExpression(val.*, scope),
            .prefix_operator => |val| try self.evaluatePrefixBinaryExpression(val.*, scope),
            .grouped_expression => |group| try self.evaluateExpression(group.expression, scope),
            .identifier => |ident| blk: {
                if (builtins.fromStr(ident)) |builtin| {
                    return .{ .builtin = builtin };
                }

                if (scope.retrieve(ident)) |value| {
                    break :blk value;
                } else {
                    std.debug.print("tried to get to non existing variable for identifier: {s}\n", .{ident});
                    break :blk ErrorKind.NoSuchVariable;
                }
            },
            .boolean => |val| .{ .boolean = val },
            .string_literal => |val| .{ .string = try utils.preEscapeString(self.alloc, val) },
            .function_literal => |f| .{
                .function = .{ .body = f.body, .arguments = f.arguments },
            },

            .function_call => |func| {
                if (builtins.fromStr(func.identifier)) |builtin| {
                    var values = Array(Value).init(self.alloc);
                    for (func.arguments) |exp| {
                        const value = try self.evaluateExpression(exp, scope);
                        try values.append(value);
                    }

                    return builtin(values.items);
                }

                if (scope.retrieve(func.identifier)) |val| {
                    const func_ptr = switch (val) {
                        .function => |v| v,
                        else => return ErrorKind.NotAFunction,
                    };

                    var func_scope = Scope.init(self.alloc, scope);
                    for (func.arguments, 0..) |argument, index| {
                        const identifier = func_ptr.arguments[index].identifier;
                        const value = try self.evaluateExpression(argument, &func_scope);
                        // function bindings are always immutable
                        func_scope.bind(identifier, .{ .mutable = false, .value = value });
                    }

                    const res = self.evaluateFunction(func_ptr.body, func_ptr.arguments, &func_scope);

                    return res;
                }

                return ErrorKind.NoSuchFunction;
            },
            .if_else => |stmnt| {
                const value = try self.evaluateExpression(stmnt.condition, scope);
                switch (value) {
                    .boolean => |do| {
                        if (do) {
                            var if_scope = Scope.init(self.alloc, scope);
                            return try self.evaluateBlock(stmnt.consequence, &if_scope);
                        }
                        // TODO: add inline else if
                        else if (stmnt.alternative) |alternative| {
                            var else_scope = Scope.init(self.alloc, scope);
                            return try self.evaluateBlock(alternative, &else_scope);
                        }
                    },
                    else => @panic("boolean expected"),
                }
                return Value.null;
            },

            .match => |stmnt| {
                const value: Value = try self.evaluateExpression(stmnt.value, scope);
                // for (stmnt.arms) |arm| {
                // // switch (value) {
                // //    .string => |val| {}
                // // }
                // }
                std.debug.panic("todo implement match, value: {}", .{value});
            },

            else => std.debug.panic("Unhandled expression: {any}", .{expr}),
        };
    }

    fn evaluateFunction(
        self: *Self,
        body: ast.Block,
        _: []ast.FunctionArgument,
        scope: *Scope,
    ) !Value {
        for (body.nodes, 0..) |node, i| switch (node) {
            .statement => |statement| {
                switch (statement) {
                    .@"return" => |val| {
                        return try self.evaluateExpression(val.value, scope);
                    },
                    else => if (try self.evaluateStatement(statement, scope)) |value| {
                        if (i == body.nodes.len - 1) return value;
                    },
                }
            },
            .expression => |expr| {
                const value = try self.evaluateExpression(expr, scope);
                if (i == body.nodes.len - 1) return value;
            },

            else => std.debug.panic("Unhandled node in evaluate function: {any}", .{node}),
        };

        return .null;
    }

    pub fn printDebug(self: *Self) !void {
        const stdout = std.io.getStdOut().writer();

        var variables = self.root.variables.iterator();
        while (variables.next()) |variable| {
            const binding: ValueBinding = variable.value_ptr.*;
            switch (binding.value) {
                .string => |str| try stdout.print("identifier: {s}: type: string, value: {s}\n", .{ variable.key_ptr.*, str }),
                else => try stdout.print("{s}: {any}\n", .{ variable.key_ptr.*, binding }),
            }
        }
    }

    fn evaluateIntegerBinaryExpression(op: ast.InfixOperator, left: i64, right: i64) ErrorKind!Value {
        return switch (op) {
            .add => .{ .integer = left + right },
            .subtract => .{ .integer = left - right },
            .multiply => .{ .integer = left * right },
            .divide => .{ .integer = @divTrunc(left, right) },
            .equal => .{ .boolean = left == right },
            .greater_than => .{ .boolean = left > right },
            .greater_than_or_eq => .{ .boolean = left >= right },
            .less_than => .{ .boolean = left < right },
            .less_than_or_eq => .{ .boolean = left <= right },
            else => ErrorKind.IllegalOperator,
        };
    }

    fn evaluateBooleanBinaryExpression(op: ast.InfixOperator, left: bool, right: bool) ErrorKind!bool {
        return switch (op) {
            .and_operator => left and right,
            .or_operator => left or right,
            .equal => left == right,
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
            // TODO: return Value from this and eval bool bin expr
            // .equal => left == right,

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
