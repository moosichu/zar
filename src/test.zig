const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;
const testing = std.testing;

const Archive = @import("archive/Archive.zig");

const test1_dir = "test/data/test1";
const test1_gnu_archive = "output_llvm-ar_gnu.a";
const test1_bsd_archive = "output_llvm-ar_bsd.a";
const test1_names = [_][]const u8{ "input1.txt", "input2.txt" };

const test2_dir = "test/data/test2";
const test2_gnu_archive = "output_llvm-ar_gnu.a";
const test2_bsd_archive = "output_llvm-ar_bsd.a";
const test2_names = [_][]const u8{ "input1.txt", "input2.txt", "input3_that_is_also_a_much_longer_file_name.txt", "input4_that_is_also_a_much_longer_file_name.txt" };

test "List Files GNU test1" {
    try testDislayContents(test1_dir, test1_gnu_archive, test1_names);
}

test "List Files BSD test1" {
    try testDislayContents(test1_dir, test1_bsd_archive, test1_names);
}

test "List Files GNU test2" {
    try testDislayContents(test2_dir, test2_gnu_archive, test2_names);
}

test "List Files BSD test2" {
    try testDislayContents(test2_dir, test2_bsd_archive, test2_names);
}

fn testDislayContents(test_dir_path: []const u8, archive_name: []const u8, file_names: anytype) !void {
    const test_dir = try fs.cwd().openDir(test_dir_path, .{});

    const file = try test_dir.openFile(archive_name, .{});
    defer file.close();

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var allocator = &arena.allocator;
    // TODO: pass through custom writer so we can check outputs on error tests
    const stderr = io.getStdErr().writer();

    var archive = Archive.create(file, archive_name);
    try archive.parse(allocator, stderr);
    for (file_names) |file_name, index| {
        try testing.expect(mem.eql(u8, archive.files.items[index].name, file_name));
    }
}
