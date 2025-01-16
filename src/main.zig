const std = @import("std");
const utils = @import("./utils.zig");

pub fn main() !void {
    var args = std.process.args();
    _ = args.skip();

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
            const parser = parse.Parser.new(content);

            try stdout.print("{any}\n", .{parser});
        }
    } else {
        try stdout.print("No subcommand given.\n", .{});
        std.process.exit(1);
    }
}
