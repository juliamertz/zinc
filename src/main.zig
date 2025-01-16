const std = @import("std");
// const lexer = @import("./lexer.zig");
const repl = @import("./repl.zig");

pub fn main() !void {
    // const alloc = std.heap.page_allocator;

    // const filename = "spec/func";
    // const data = try std.fs.cwd().readFileAlloc(alloc, filename, std.math.maxInt(usize));
    // defer alloc.free(data);

    // std.debug.print("input data:\n{s}\ntokens:\n", .{data});

    // _ = try lexer.tokenize(data);

    _ = try repl.start();
}
