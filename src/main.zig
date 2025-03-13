const std = @import("std");
const utils = @import("utils.zig");
const lex = @import("lexer.zig");
const repl = @import("repl.zig");
const interp = @import("interpreter/main.zig");

const Parser = @import("parser.zig").Parser;

const stdout = std.io.getStdOut().writer();

pub fn main() !void {
    var args = std.process.args();
    _ = args.skip();

    if (args.next()) |subcommand| {
        if (eql(subcommand, "repl")) {
            try repl.start();
        }
        //
        else if (eql(subcommand, "eval")) {
            const filepath = args.next() orelse @panic("no filepath given");
            var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            defer arena.deinit();

            const max = std.math.maxInt(usize);
            const content = try std.fs.cwd().readFileAlloc(arena.allocator(), filepath, max);
            // try stdout.print("content:\n\n{s}\n", .{content});

            var interpreter = interp.Interpreter.init(arena.allocator());
            try repl.run(arena.allocator(), &interpreter, content);
        } else if (eql(subcommand, "parse")) {
            const filepath = args.next() orelse @panic("no filepath given");
            var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            defer arena.deinit();

            const max = std.math.maxInt(usize);
            const content = try std.fs.cwd().readFileAlloc(arena.allocator(), filepath, max);
            try stdout.print("content:\n\n{s}\n", .{content});

            var parser = Parser.init(content, arena.allocator());
            const nodes = parser.parseNodes() catch |err| {
                parser.debug("Parsing errors", true);
                return err;
            };
            try utils.printAst(arena.allocator(), nodes);

            // var interpreter = interp.Interpreter.new(arena.allocator());
            // try repl.run(arena.allocator(), &interpreter, content);
        } else if (eql(subcommand, "lex")) {
            const filepath = args.next() orelse @panic("no filepath given");
            var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            defer arena.deinit();

            const max = std.math.maxInt(usize);
            const content = try std.fs.cwd().readFileAlloc(arena.allocator(), filepath, max);

            var lexer = lex.Lexer.init(content);
            var token: lex.Token = lexer.readToken();
            while (lexer.read_position <= lexer.content.len - 2) {
                switch (token) {
                    .ident => |val| std.debug.print("ident {c}\n", .{val}),
                    else => std.debug.print("{any}\n", .{token}),
                }

                lexer.advance();
                token = lexer.readToken();
                try stdout.writeAll("hi");
            }
            try stdout.writeAll("bye");

            // try stdout.writeAll(content);
        }
        //
        else {
            try stdout.print("Invalid subcommand: {s}\n", .{subcommand});
            std.process.exit(1);
        }
    }
    //
    else {
        try stdout.print("No subcommand given.\n", .{});
        std.process.exit(1);
    }
}

pub fn eql(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}
