const std = @import("std");
const utils = @import("utils.zig");
const lex = @import("lexer.zig");
const repl = @import("./repl.zig");

pub fn main() !void {
    var args = std.process.args();
    _ = args.skip();

    const stdout = std.io.getStdOut().writer();

    if (args.next()) |subcommand| {
        if (utils.str_cmp(subcommand, "repl")) {
            try repl.start();
        }
        //
        else if (utils.str_cmp(subcommand, "run")) {
            const filepath = args.next() orelse @panic("no filepath given");

            const alloc = std.heap.page_allocator;
            const max = std.math.maxInt(usize);
            const content = try std.fs.cwd().readFileAlloc(alloc, filepath, max);
            defer alloc.free(content);
            try stdout.print("content:\n\n{s}\n", .{content});

            try repl.run(alloc, content);

            // const parse = @import("./parser.zig");
            // var parser = parse.Parser.new(content);

            // const stmnt = try parser.parseStatement();
            // try std.json.stringify(&stmnt, .{ .whitespace = .indent_2 }, std.io.getStdOut().writer());
        }
    } else {
        try stdout.print("No subcommand given.\n", .{});
        std.process.exit(1);
    }
}
