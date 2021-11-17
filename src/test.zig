const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;
const testing = std.testing;
const logger = std.log.scoped(.tests);

const Archive = @import("archive/Archive.zig");

const test1_dir = "test/data/test1";
const test1_gnu_archive = "output_llvm-ar_gnu.a";
const test1_bsd_archive = "output_llvm-ar_bsd.a";
const test1_names = [_][]const u8{ "input1.txt", "input2.txt" };

const test2_dir = "test/data/test2";
const test2_gnu_archive = "output_llvm-ar_gnu.a";
const test2_bsd_archive = "output_llvm-ar_bsd.a";
const test2_names = [_][]const u8{ "input1.txt", "input2.txt", "input3_that_is_also_a_much_longer_file_name.txt", "input4_that_is_also_a_much_longer_file_name.txt" };

const test4_dir = "test/data/test4";
const test4_gnu_archive = "output_llvm-ar_gnu.a";
const test4_bsd_archive = "output_llvm-ar_bsd.a";
const test4_names = [_][]const u8{"input1.o"};

test "List Files GNU test1" {
    const allocator = std.testing.allocator;
    var argv = std.ArrayList([]const u8).init(allocator);
    defer argv.deinit();

    try argv.append("zig");
    try argv.append("ar");
    try argv.append("--format=gnu");

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const cwd = try std.fs.path.join(allocator, &[_][]const u8{
        "zig-cache", "tmp", &tmp_dir.sub_path,
    });
    defer allocator.free(cwd);

    const file_names = test1_names;
    const test_src_dir = try fs.cwd().openDir(test1_dir, .{});

    for (file_names) |test_file| {
        try std.fs.Dir.copyFile(test_src_dir, test_file, tmp_dir.dir, test_file, .{});
    }

    try argv.append("r");
    try argv.append(test1_gnu_archive);
    try argv.appendSlice(&test1_names);

    const result = try std.ChildProcess.exec(.{
        .allocator = allocator,
        .argv = argv.items,
        .cwd = cwd,
    });

    defer {
        allocator.free(result.stdout);
        allocator.free(result.stderr);
    }

    try testFileContents(cwd, test1_gnu_archive, test1_names);
}

test "List Files BSD test1" {
    try testFileContents(test1_dir, test1_bsd_archive, test1_names);
}

test "List Files GNU test2" {
    try testFileContents(test2_dir, test2_gnu_archive, test2_names);
}

test "List Files BSD test2" {
    try testFileContents(test2_dir, test2_bsd_archive, test2_names);
}

test "List Files GNU test4" {
    try testFileContents(test4_dir, test4_gnu_archive, test4_names);
}

test "List Files BSD test4" {
    try testFileContents(test4_dir, test4_bsd_archive, test4_names);
}

fn testFileContents(test_dir_path: []const u8, archive_name: []const u8, file_names: anytype) !void {
    const test_dir = try fs.cwd().openDir(test_dir_path, .{});

    const archive_file = try test_dir.openFile(archive_name, .{});
    defer archive_file.close();

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var allocator = &arena.allocator;

    var archive = try Archive.create(archive_file, archive_name, Archive.ArchiveType.ambiguous, .{});
    try archive.parse(allocator);

    var memory_buffer = try allocator.alloc(u8, 1024 * 1024);
    for (file_names) |file_name, index| {
        try testing.expect(mem.eql(u8, archive.files.items[index].name, file_name));
        const file = try test_dir.openFile(file_name, .{});
        defer file.close();

        const reader = file.reader();

        var current_start_pos: u64 = 0;
        while (true) {
            const num_read = try reader.read(memory_buffer);
            if (num_read == 0) {
                break;
            }
            try testing.expect(mem.eql(u8, memory_buffer[0..num_read], archive.files.items[index].contents.bytes[current_start_pos .. current_start_pos + num_read]));
            current_start_pos = current_start_pos + num_read;
        }
    }
}
