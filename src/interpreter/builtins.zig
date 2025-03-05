const std = @import("std");

const interpreter = @import("main.zig");
const Value = interpreter.Value;
const Error = interpreter.ErrorKind;

pub fn fromStr(str: []const u8) ?*const fn (args: []const Value) Error!Value {
    const function_map = std.StaticStringMap(*const fn (args: []const Value) Error!Value).initComptime(.{
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

// Another example function
fn print(args: []const Value) !Value {
    std.debug.print("Print function called: {any}\n", .{args});
    return .null;
}
