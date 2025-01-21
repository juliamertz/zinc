const std = @import("std");
const ast = @import("../ast.zig");

pub const Value = union(enum) {
    string: []const u8,
    integer: i64,
    boolean: bool,
    function: ast.FunctionStatement,
    null, // TODO: remove this sometime when there is a a type system (soontm)
};
