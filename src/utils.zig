const std = @import("std");

pub fn str_cmp(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

pub fn is_letter(ch: u8) bool {
    return std.ascii.isAlphabetic(ch) or ch == '_';
}

pub fn is_whitespace(ch: u8) bool {
    return ch == ' ' or ch == '\n';
}
