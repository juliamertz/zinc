const std = @import("std");
const utils = @import("utils.zig");
const lex = @import("lexer.zig");
const repl = @import("repl.zig");
const interp = @import("interpreter/main.zig");
const parser = @import("parser.zig");

const stdout = std.io.getStdOut().writer();

const Error = error{
    NoSubcommand,
    NoSuchCommand,
    InvalidPath,
};

const Subcommand = enum {
    repl,
    eval,
    parse,

    const lookup: std.StaticStringMap(Subcommand) = .initComptime(.{
        .{ "repl", .repl },
        .{ "eval", .eval },
        .{ "parse", .parse },
    });
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    var args = std.process.args();
    _ = args.skip();

    const next = args.next() orelse return Error.NoSuchCommand;

    if (Subcommand.lookup.get(next)) |subcommand| switch (subcommand) {
        .repl => try handleReplCommand(),
        .parse => try handleParseCommand(allocator, &args),
        .eval => try handleEvalCommand(allocator, &args),
    } else {
        try stdout.print("No subcommand given.\n", .{});
        std.process.exit(1);
    }
}

fn handleReplCommand() !void {
    try repl.start();
}

fn handleEvalCommand(allocator: std.mem.Allocator, args: *std.process.ArgIterator) !void {
    const filepath = args.next() orelse return Error.InvalidPath;
    const content = try readFile(allocator, filepath);
    var interpreter = interp.Interpreter.init(allocator);
    try repl.run(allocator, &interpreter, content);
}

fn handleParseCommand(allocator: std.mem.Allocator, args: *std.process.ArgIterator) !void {
    const filepath = args.next() orelse return Error.InvalidPath;
    const content = try readFile(allocator, filepath);
    try stdout.print("content:\n\n{s}\n", .{content});

    var p = parser.Parser.init(content, allocator);
    const nodes = p.parseNodes() catch |err| {
        p.debug("Parsing errors", true);
        return err;
    };

    try utils.printAst(allocator, nodes);
}

fn readFile(allocator: std.mem.Allocator, filepath: []const u8) ![]const u8 {
    return try std.fs.cwd().readFileAlloc(
        allocator,
        filepath,
        std.math.maxInt(usize),
    );
}
