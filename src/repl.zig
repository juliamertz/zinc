const std = @import("std");
const lexer = @import("./lexer.zig");

fn wait_for_input() ![]const u8 {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    try stdout.print(">> ", .{});
    const input = try stdin.readUntilDelimiterOrEofAlloc(std.heap.page_allocator, '\n', std.math.maxInt(usize));
    return input orelse unreachable;
}

pub fn start_repl() !void {
    const in = try wait_for_input();

    var tokens = std.ArrayList(lexer.Token).init(std.heap.page_allocator);
    defer tokens.deinit();

    try lexer.tokenize(&tokens, in);
    for (tokens.items) |tok| {
        std.debug.print("{any}\n", .{tok});
    }
}
