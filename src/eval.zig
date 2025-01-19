const std = @import("std");
const ast = @import("ast.zig");

pub const Interpreter = struct {
    module: ast.Module,

    const Self = @This();

    pub fn new(module: ast.Module) Self {
        return Self{ .module = module };
    }
};
