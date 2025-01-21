const std = @import("std");

pub fn str_cmp(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

pub fn is_letter(ch: u8) bool {
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
