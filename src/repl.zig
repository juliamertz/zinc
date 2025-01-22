const std = @import("std");
const pretty = @import("pretty");
const utils = @import("utils.zig");

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

pub fn run(alloc: std.mem.Allocator, interpreter: *interp.Interpreter, content: []const u8) !void {
    var parser = parse.Parser.new(content, alloc);
    const nodes = parser.parseNodes() catch |err| {
        const msg = try std.fmt.allocPrint(alloc, "error while parsing input at position {d}:\n", .{parser.lexer.position});
        parser.printDebug(msg, true);
        return err;
    };

    // const tree_debug_print = try utils.printAstNode(alloc, 0, nodes[0]);
    // std.debug.print("{s}\n", .{tree_debug_print});

    try pretty.print(alloc, nodes, .{
        .print_extra_empty_line = true,
        .ptr_skip_dup_unfold = false,
    });

    try interpreter.evaluate(nodes);
}

pub fn start() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var interpreter = interp.Interpreter.new(arena.allocator());

    while (true) {
        defer arena.deinit();
        defer arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);

        if (try read_input(arena.allocator())) |stdin| {
            try run(arena.allocator(), &interpreter, stdin);
        }
    }
}
