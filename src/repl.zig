const std = @import("std");
const utils = @import("utils.zig");

const lex = @import("lexer.zig");
const ast = @import("ast.zig");
const interp = @import("interpreter/main.zig");

const Parser = @import("parser.zig").Parser;

fn read_input(alloc: std.mem.Allocator) !?[]const u8 {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    try stdout.print(">> ", .{});
    const input = try stdin.readUntilDelimiterOrEofAlloc(alloc, '\n', std.math.maxInt(usize));
    return input;
}

const print_trace = true;

pub fn run(alloc: std.mem.Allocator, interpreter: *interp.Interpreter, content: []const u8) !void {
    var parser = Parser.init(content, alloc);
    // const nodes = parser.parseNodes() catch |err| {
    //     parser.debug("Parsing errors", true);
    //     if (print_trace) return err;
    //     std.process.exit(1);
    // };

    const moduleAst = parser.parseModule() catch |err| {
        parser.debug("Parsing errors", true);
        if (print_trace) return err;
        std.process.exit(1);
    };

    // const env = std.process.getEnvMap(alloc) catch @panic("unable to get environment");
    // if (env.get("ZINC_LOG")) |value| {
    // if (std.mem.eql(u8, value, "debug")) {
    try utils.printAst(alloc, moduleAst);
    std.debug.print("\n\n", .{});
    // }
    // }

    _ = try interpreter.evaluateModule(moduleAst);
}

pub fn start() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var interpreter = interp.Interpreter.init(arena.allocator());

    while (true) {
        defer arena.deinit();
        defer arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);

        if (try read_input(arena.allocator())) |stdin| {
            try run(arena.allocator(), &interpreter, stdin);
        }
    }
}
