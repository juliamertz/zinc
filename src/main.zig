const std = @import("std");
const lexer = @import("./lexer.zig");

pub fn main() !void {
    const alloc = std.heap.page_allocator;

    const data = try std.fs.cwd().readFileAlloc(alloc, "spec/1.mlang", std.math.maxInt(usize));
    defer alloc.free(data);

    std.debug.print("{s}", .{data});

    _ = try lexer.tokenize(data);
    // std.debug.print("{}\n", .{tokens});

    // var lex = lexer.Lexer{ .content = data, .pos = 0 };
    // const curr = lex.current();
    // std.debug.print("current char: {c}\n", .{curr});
    // const next = lex.advance();
    // if (next) |ch| {
    //     std.debug.print("current char: {c}\n", .{ch});
    // }
}
