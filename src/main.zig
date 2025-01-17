const std = @import("std");
const utils = @import("utils.zig");
const lex = @import("lexer.zig");

pub fn main() !void {
    var args = std.process.args();
    _ = args.skip();

    // std.debug.print("{any}", .{@typeInfo(lex.Token)});

    // const parse = @import("parser.zig");
    // var parser = parse.Parser.new("aap = ");
    // _ = try parser.parse_let_statement();
    // // std.debug.print("statemtent: {any}", .{thing});
    std.process.exit(0);

    const stdout = std.io.getStdOut().writer();

    if (args.next()) |subcommand| {
        if (utils.str_cmp(subcommand, "repl")) {
            try @import("./repl.zig").start();
        }
        //
        else if (utils.str_cmp(subcommand, "process")) {
            const alloc = std.heap.page_allocator;
            const max = std.math.maxInt(usize);
            const content = try std.fs.cwd().readFileAlloc(alloc, "./spec/var", max);
            defer alloc.free(content);
            try stdout.print("content: {s}\n", .{content});

            const parse = @import("./parser.zig");
            var parser = parse.Parser.new(content);

            const stmnt = try parser.parseStatement();
            try std.json.stringify(&stmnt, .{ .whitespace = .indent_2 }, std.io.getStdOut().writer());
        }
    } else {
        try stdout.print("No subcommand given.\n", .{});
        std.process.exit(1);
    }
}
