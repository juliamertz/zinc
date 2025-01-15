const std = @import("std");

pub fn is_letter(ch: u8) bool {
    return std.ascii.isAlphabetic(ch) or ch == '_';
}

pub fn is_whitespace(ch: u8) bool {
    return ch == ' ' or ch == '\n';
}
