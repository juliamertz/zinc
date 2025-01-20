const std = @import("std");
const utils = @import("utils.zig");
const lex = @import("lexer.zig");
const repl = @import("./repl.zig");
const interp = @import("./runtime/interpreter.zig");

pub fn main() !void {
    var args = std.process.args();
    _ = args.skip();

    const stdout = std.io.getStdOut().writer();

    if (args.next()) |subcommand| {
        if (utils.str_cmp(subcommand, "repl")) {
            try repl.start();
        }
        //
        else if (utils.str_cmp(subcommand, "eval")) {
            const filepath = args.next() orelse @panic("no filepath given");
            var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            defer arena.deinit();

            const max = std.math.maxInt(usize);
            const content = try std.fs.cwd().readFileAlloc(arena.allocator(), filepath, max);
            try stdout.print("content:\n\n{s}\n", .{content});

            var interpreter = interp.Interpreter.new(arena.allocator());
            try repl.run(arena.allocator(), &interpreter, content);
        } else {
            try stdout.print("Invalid subcommand: {s}\n", .{subcommand});
            std.process.exit(1);
        }
    } else {
        try stdout.print("No subcommand given.\n", .{});
        std.process.exit(1);
    }
}
