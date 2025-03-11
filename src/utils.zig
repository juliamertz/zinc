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

/// return new string with pre-escaped escape sequences
pub fn preEscape(allocator: std.mem.Allocator, str: []const u8) ![]const u8 {
    var buff = try allocator.alloc(u8, str.len);

    var char_idx: u8 = 0;
    var buff_idx: u8 = 0;
    while (char_idx < str.len) {
        if (str[char_idx] == '\\') {
            const escaped: ?u8 = switch (str[char_idx + 1]) {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\\' => '\\',
                '"' => '"',
                '\'' => '\'',
                else => null,
            };
            if (escaped) |val| {
                buff[buff_idx] = val;
                char_idx += 1; // skip escaped charachter
            }
        } else {
            buff[buff_idx] = str[char_idx];
        }

        buff_idx += 1;
        char_idx += 1;
    }

    return buff[0..buff_idx];
}

test "string pre-escaping" {
    const expected = "\"\n\t\"";
    const actual = try preEscape(std.heap.page_allocator, "\"\\n\\t\"");
    try std.testing.expectEqualStrings(expected, actual);
}

pub fn printAst(alloc: std.mem.Allocator, value: anytype) !void {
    const stdout = std.io.getStdOut().writer();
    const printed = try pretty.dump(alloc, value, .{
        .print_extra_empty_line = true,
        .max_depth = std.math.maxInt(u8),
        .ptr_skip_dup_unfold = false,
        .show_type_names = false,
    });

    try stdout.writeAll(printed);
}

// fn errorMessage(self: *Self, kind: ErrorKind, comptime msg: []const u8, args: anytype) ErrorKind {

pub fn debug(msg: []const u8, value: anytype) !void {
    const stderr = std.io.getStdErr().writer();
    const printed = try pretty.dump(std.heap.page_allocator, value, .{
        .print_extra_empty_line = true,
        .max_depth = std.math.maxInt(u8),
        .ptr_skip_dup_unfold = false,
        .show_type_names = false,
    });

    try stderr.writeAll(msg);
    try stderr.writeAll(":\n");
    try stderr.writeAll(printed);
}
