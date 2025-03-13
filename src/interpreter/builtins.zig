const std = @import("std");
const interpreter = @import("main.zig");

const Value = interpreter.Value;
const BuiltinPtr = interpreter.BuiltinPtr;

const function_map = std.StaticStringMap(BuiltinPtr).initComptime(.{
    .{ "dbg", dbg },
    .{ "print", print },
    .{ "len", len },
    .{ "int_to_str", int_to_str },
});

pub fn fromStr(str: []const u8) ?BuiltinPtr {
    return function_map.get(str);
}

pub fn module(alloc: std.mem.Allocator) interpreter.Module {
    var scope = interpreter.Scope.init(alloc, null);
    for (function_map.keys()) |key| {
        const value = function_map.get(key).?;
        const binding = interpreter.ValueBinding{
            .mutable = false,
            .value = .{ .builtin = value },
        };
        scope.bind(key, binding);
    }

    return .{ .scope = scope };
}

// TODO:
fn dbg(args: []const Value) !Value {
    _ = try print(args);
    const stdout = std.io.getStdOut().writer();
    _ = stdout.writeAll("\n") catch unreachable;
    return .null;
}

fn print(args: []const Value) !Value {
    const stdout = std.io.getStdOut().writer();
    for (args) |arg| {
        const content = switch (arg) {
            .builtin => "<builtin function>",
            .function => "<function>",
            .module => "<module>",
            .list => "<list>", // TODO:
            .null => "null",
            .string => |val| val,
            .integer => |val| std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{val}) catch unreachable,
            .boolean => |val| switch (val) {
                true => "true",
                false => "false",
            },
        };
        stdout.writeAll(content) catch @panic("unable to write in print");
    }

    return .null;
}

fn len(args: []const Value) !Value {
    switch (args[0]) {
        .string => |val| return .{ .integer = @intCast(val.len) },
        .list => |val| return .{ .integer = @intCast(val.len) },
        else => unreachable, // TODO: error
    }
}

// TODO: errors
fn int_to_str(args: []const Value) !Value {
    if (args.len != 1) return .{ .string = "error: invalid arguments" };
    return switch (args[0]) {
        .integer => |val| .{ .string = std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{val}) catch unreachable },
        else => .{ .string = "error: not an integer" },
    };
}
