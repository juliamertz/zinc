const std = @import("std");
const lex = @import("lexer.zig");
const parse = @import("parser.zig");

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

            // const parser = parse.Parser.new(stdin);
            // parser.parse_let_statement()

            var tokens = std.ArrayList(lex.Token).init(alloc);
            defer tokens.deinit();
            try lex.tokenize(&tokens, stdin);

            for (tokens.items) |tok| {
                std.debug.print("{any}\n", .{tok});
            }
        }
    }
}
