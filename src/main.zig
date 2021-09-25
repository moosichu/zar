const std = @import("std");
const fs = std.fs;

const Archive = @import("archive/Archive.zig");

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

    std.log.info("The first argument was: {s}", .{first_arg});

    const path = first_arg;

    const file = try fs.cwd().openFile(path, .{});

    var archive = Archive.create(file, path);

    try archive.parse(allocator);

    // TODO: Start by implementing llvm-ar t archive.a
    // (Prints all the file names)

}
