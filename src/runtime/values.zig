const std = @import("std");
const ast = @import("../ast.zig");

pub const Value = union(enum) {
    string: []const u8,
    integer: i64,
    bool: bool,
    function: ast.FunctionStatement,
};
