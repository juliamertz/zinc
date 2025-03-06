const std = @import("std");
const interpreter = @import("main.zig");

const Value = interpreter.Value;
const BuiltinPtr = interpreter.BuiltinPtr;

pub fn fromStr(str: []const u8) ?BuiltinPtr {
    const function_map = std.StaticStringMap(BuiltinPtr).initComptime(.{
        .{ "dbg", dbg },
        .{ "print", print },
        .{ "intToStr", intToStr },
    });

    return function_map.get(str);
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

// TODO: errors
fn intToStr(args: []const Value) !Value {
    if (args.len != 1) return .{ .string = "error: invalid arguments" };
    return switch (args[0]) {
        .integer => |val| .{ .string = std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{val}) catch unreachable },
        else => .{ .string = "error: not an integer" },
    };
}
