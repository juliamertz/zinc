const std = @import("std");
const interpreter = @import("main.zig");

const Value = interpreter.Value;
const BuiltinPtr = interpreter.BuiltinPtr;

pub fn fromStr(str: []const u8) ?BuiltinPtr {
    const function_map = std.StaticStringMap(BuiltinPtr).initComptime(.{
        .{ "dbg", dbg },
        .{ "print", print },
    });

    return function_map.get(str);
}

fn dbg(args: []const Value) !Value {
    const alloc = std.heap.page_allocator;
    var result = std.ArrayList(u8).init(alloc);

    for (args, 0..) |value, i| {
        const format = try switch (value) {
            .string => |text| std.fmt.allocPrint(alloc, "{s}", .{text}),
            .integer => |val| std.fmt.allocPrint(alloc, "{d}", .{val}),
            else => std.fmt.allocPrint(alloc, "{any}", .{value}),
        };

        if (i != args.len - 1) {
            try result.appendSlice(", ");
        }

        try result.appendSlice(format);
    }

    try result.append('\n');

    const stdout = std.io.getStdOut().writer();
    stdout.writeAll(result.items) catch unreachable;

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
            .integer => |val| std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{val}) catch @panic("alloc failed in builtin function"),
            .boolean => |val| switch (val) {
                true => "true",
                false => "false",
            },
        };
        stdout.writeAll(content) catch @panic("unable to write in print");
    }

    return .null;
}
