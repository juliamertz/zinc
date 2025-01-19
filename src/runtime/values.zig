const std = @import("std");

pub const Value = union(enum) {
    string: []const u8,
    integer: i64,
    bool: bool,
};
