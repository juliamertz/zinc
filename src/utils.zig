const std = @import("std");
const ast = @import("ast.zig");
const pretty = @import("pretty");

pub fn trim(str: []const u8) []const u8 {
    return std.mem.trim(u8, str, " ␃\n");
}

pub fn isLetter(ch: u8) bool {
    return std.ascii.isAlphabetic(ch) or ch == '_';
}

pub fn isWhitespace(ch: u8) bool {
    return ch == ' ' or ch == '\n';
}

pub fn repeatString(allocator: std.mem.Allocator, s: []const u8, n: usize) ![]u8 {
    var result = try allocator.alloc([]const u8, n);
    var i: u8 = 0;
    while (i < n) {
        result[i] = s;
        i += 1;
    }
    return std.mem.join(allocator, "", result);
}

pub fn printAst(alloc: std.mem.Allocator, nodes: []ast.Node) !void {
    try pretty.printWriter(alloc, std.io.getStdErr().writer(), nodes, .{
        .print_extra_empty_line = true,
        .max_depth = std.math.maxInt(u8),
        .ptr_skip_dup_unfold = false,
        .show_type_names = false,
    });
}
