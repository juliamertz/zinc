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

const print_trace = false;

pub fn run(alloc: std.mem.Allocator, interpreter: *interp.Interpreter, content: []const u8) !void {
    var parser = parse.Parser.new(content, alloc);
    const nodes = parser.parseNodes() catch |err| {
        parser.printDebug("Parsing errors", true);
        if (print_trace) return err;
        unreachable;
    };

    try pretty.printWriter(alloc, std.io.getStdErr().writer(), nodes, .{
        .print_extra_empty_line = true,
        .max_depth = std.math.maxInt(u8),
        .ptr_skip_dup_unfold = false,
        .show_type_names = false,
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
