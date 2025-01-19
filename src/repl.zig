const std = @import("std");
const pretty = @import("pretty");

const lex = @import("lexer.zig");
const parse = @import("parser.zig");
const ast = @import("ast.zig");
const interp = @import("runtime/interpreter.zig");

const alloc = std.heap.page_allocator;

fn read_input() !?[]const u8 {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    try stdout.print(">> ", .{});
    const input = try stdin.readUntilDelimiterOrEofAlloc(alloc, '\n', std.math.maxInt(usize));
    return input;
}

pub fn start() !void {
    while (true) {
        if (try read_input()) |stdin| {
            defer alloc.free(stdin);

            const stderr = std.io.getStdErr().writer();
            var parser = parse.Parser.new(stdin);
            const stmnt = parser.parseStatement() catch |err| {
                try stderr.print(
                    "error while parsing input at position {d}: {}\n\n",
                    .{ parser.lexer.read_position, err },
                );
                continue;
            };
            try pretty.print(alloc, stmnt, .{
                .print_extra_empty_line = true,
                .ptr_skip_dup_unfold = false,
            });

            // const val = [_]ast.Statement{stmnt};
            // const interpreter = interp.Interpreter.new(.{
            //     .statements = &val,
            // });
            // std.debug.print("{}", .{interpreter});
        }
    }
}
