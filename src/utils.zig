const std = @import("std");
const ast = @import("ast.zig");

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

pub fn printAstNode(alloc: std.mem.Allocator, level: usize, node: ast.Node) ![]u8 {
    var out = std.ArrayList(u8).init(alloc);
    try out.appendNTimes(' ', 2 * level);
    switch (node) {
        .expression => |expr| switch (expr) {
            else => out.append("todo!"),
        },
        .statement => |expr| switch (expr) {
            else => out.append("todo!"),
        },
    }
}
