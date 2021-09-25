const std = @import("std");

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var arg_iterator = std.process.args();

    var allocator = &arena.allocator;

    // skip the executable name
    _ = arg_iterator.skip();

    const first_arg = try (arg_iterator.next(allocator) orelse {
        std.debug.warn("Expected at least one argument\n", .{});
        return error.InvalidArgs;
    });

    std.log.info("This first argument was: {s}", .{first_arg});
}
