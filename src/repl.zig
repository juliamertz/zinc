const std = @import("std");
const pretty = @import("pretty");

const lex = @import("lexer.zig");
const parse = @import("parser.zig");
const ast = @import("ast.zig");
const interp = @import("runtime/interpreter.zig");

fn read_input(alloc: std.mem.Allocator) !?[]const u8 {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    try stdout.print(">> ", .{});
    const input = try stdin.readUntilDelimiterOrEofAlloc(alloc, '\n', std.math.maxInt(usize));
    return input;
}

pub fn run(alloc: std.mem.Allocator, content: []const u8) !void {
    const stderr = std.io.getStdErr().writer();
    var parser = parse.Parser.new(content, alloc);
    const module = parser.parseModule() catch |err| {
        try stderr.print(
            "error while parsing input at position {d}\ncurrent token: {any}\npeek token: {any}\n\n",
            .{ parser.lexer.read_position, parser.curr_token, parser.peek_token },
        );
        return err;
    };

    try pretty.print(alloc, module, .{
        .print_extra_empty_line = true,
        .ptr_skip_dup_unfold = false,
    });

    // const module: ast.Module = .{ .statements = &[_]ast.Statement{statement} };

    var interpreter = interp.Interpreter.new(alloc, module);
    try interpreter.evaluate();
}

pub fn start() !void {
    while (true) {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        if (try read_input(arena.allocator())) |stdin| {
            try run(arena.allocator(), stdin);
        }
    }
}
