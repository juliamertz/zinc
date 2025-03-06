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

/// return new string with interpolated escape sequences
/// for example: .{ '\\', 'n' } returns .{ '\n' }
pub fn preEscapeString(allocator: std.mem.Allocator, str: []const u8) ![]const u8 {
    var buff = try allocator.alloc(u8, str.len);

    var i: u8 = 0;
    var j: u8 = 0;
    while (i < str.len) {
        if (str[i] == '\\') {
            const escaped: ?u8 = switch (str[i + 1]) {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\\' => '\\',
                '"' => '"',
                '\'' => '\'',
                else => null,
            };
            if (escaped) |val| {
                buff[j] = val;
                i += 1; // skip escaped charachter
            }
        } else {
            buff[j] = str[i];
        }

        j += 1;
        i += 1;
    }

    return buff;
}

pub fn printAst(alloc: std.mem.Allocator, nodes: []ast.Node) !void {
    const stdout = std.io.getStdOut().writer();
    const printed = try pretty.dump(alloc, nodes, .{
        .print_extra_empty_line = true,
        .max_depth = std.math.maxInt(u8),
        .ptr_skip_dup_unfold = false,
        .show_type_names = false,
    });

    try stdout.writeAll(printed);
}
