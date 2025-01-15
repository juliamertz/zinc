const std = @import("std");

pub fn main() !void {
    const file = try std.fs.cwd().openFile("spec/1.mlang", .{});
    defer file.close();
    const contents = try file.readAllAlloc(std.heap.GeneralPurposeAllocator.init(), .{});
    std.debug.print("hello world, {}\n", .{contents});
}
