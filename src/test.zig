const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;
const testing = std.testing;
const logger = std.log.scoped(.tests);

const Archive = @import("archive/Archive.zig");
const main = @import("main.zig");

const llvm_ar_archive_name = "llvm-ar-archive.a";
const zig_ar_archive_name = "zig-ar-archive.a";

const test1_dir = "test/data/test1";
const test1_names = [_][]const u8{ "input1.txt", "input2.txt" };

const test2_dir = "test/data/test2";
const test2_names = [_][]const u8{ "input1.txt", "input2.txt", "input3_that_is_also_a_much_longer_file_name.txt", "input4_that_is_also_a_much_longer_file_name.txt" };

const test4_dir = "test/data/test4";
const test4_names = [_][]const u8{"input1.o"};

test "List Files GNU test1" {
    try createAndTestParsingOfLlvmArchive(.gnu, test1_dir, &test1_names);
}

test "List Files BSD test1" {
    try createAndTestParsingOfLlvmArchive(.bsd, test1_dir, &test1_names);
}

test "List Files GNU test2" {
    try createAndTestParsingOfLlvmArchive(.gnu, test2_dir, &test2_names);
}

test "List Files BSD test2" {
    try createAndTestParsingOfLlvmArchive(.bsd, test2_dir, &test2_names);
}

test "List Files GNU test4" {
    try createAndTestParsingOfLlvmArchive(.gnu, test4_dir, &test4_names);
}

test "List Files BSD test4" {
    try createAndTestParsingOfLlvmArchive(.bsd, test4_dir, &test4_names);
}

test "End-To-End Create test1" {
    var test_dir_info = try TestDirInfo.getInfo();
    defer test_dir_info.cleanup();
    const test_dir_path = test1_dir;
    const file_names = &test1_names;
    const format = LlvmFormat.gnu;
    try createLlvmArchive(format, test_dir_path, file_names, test_dir_info);

    const allocator = std.testing.allocator;
    {
        var argv = std.ArrayList([]const u8).init(allocator);
        defer argv.deinit();

        try argv.append("zar");
        switch (format) {
            .gnu => try argv.append("--format=gnu"),
            .bsd => try argv.append("--format=bsd"),
        }

        // TODO: the end-to-end test will interpret one of the files as
        // mach-O for some reason! So explicitly disable symbols for now.
        // (this needs fixing!)
        try argv.append("rSc");
        try argv.append(zig_ar_archive_name);
        try argv.appendSlice(file_names);

        var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
        defer arena.deinit();

        try std.os.chdir(test_dir_info.cwd);

        try main.archiveMain(&arena.allocator, argv.items);
    }

    {
        const llvm_ar_file_handle = try test_dir_info.tmp_dir.dir.openFile(llvm_ar_archive_name, .{});
        const zig_ar_file_handle = try test_dir_info.tmp_dir.dir.openFile(zig_ar_archive_name, .{});

        const llvm_ar_stat = try llvm_ar_file_handle.stat();
        const zig_ar_stat = try zig_ar_file_handle.stat();

        try testing.expect(llvm_ar_stat.size == zig_ar_stat.size);

        const llvm_ar_buffer = try allocator.alloc(u8, llvm_ar_stat.size);
        const zig_ar_buffer = try allocator.alloc(u8, zig_ar_stat.size);
        defer allocator.free(llvm_ar_buffer);
        defer allocator.free(zig_ar_buffer);

        {
            const llvm_ar_read = try llvm_ar_file_handle.preadAll(llvm_ar_buffer, 0);
            try testing.expect(llvm_ar_read == llvm_ar_stat.size);
        }

        {
            const zig_ar_read = try zig_ar_file_handle.preadAll(zig_ar_buffer, 0);
            try testing.expect(zig_ar_read == zig_ar_stat.size);
        }

        for (llvm_ar_buffer) |llvm_ar_byte, index| {
            const zig_ar_byte = zig_ar_buffer[index];
            try testing.expect(llvm_ar_byte == zig_ar_byte);
        }
    }
}

fn createAndTestParsingOfLlvmArchive(comptime format: LlvmFormat, comptime test_dir_path: []const u8, comptime file_names: []const []const u8) !void {
    var test_dir_info = try TestDirInfo.getInfo();
    defer test_dir_info.cleanup();

    try createLlvmArchive(format, test_dir_path, file_names, test_dir_info);
    try testArchiveParsing(test_dir_info.cwd, file_names);
}

fn testArchiveParsing(test_dir_path: []const u8, file_names: []const []const u8) !void {
    const test_dir = try fs.cwd().openDir(test_dir_path, .{});

    const archive_file = try test_dir.openFile(llvm_ar_archive_name, .{});
    defer archive_file.close();

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var allocator = &arena.allocator;

    var archive = try Archive.create(archive_file, llvm_ar_archive_name, Archive.ArchiveType.ambiguous, .{});
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

const LlvmFormat = enum { gnu, bsd };

const TestDirInfo = struct {
    tmp_dir: std.testing.TmpDir,
    cwd: []const u8,

    pub fn getInfo() !TestDirInfo {
        var result: TestDirInfo = .{
            .tmp_dir = std.testing.tmpDir(.{}),
            .cwd = undefined,
        };
        result.cwd = try std.fs.path.join(std.testing.allocator, &[_][]const u8{
            "zig-cache", "tmp", &result.tmp_dir.sub_path,
        });
        return result;
    }

    pub fn cleanup(self: *TestDirInfo) void {
        self.tmp_dir.cleanup();
        std.testing.allocator.free(self.cwd);
    }
};

fn createLlvmArchive(comptime format: LlvmFormat, comptime test_src_dir_path: []const u8, comptime file_names: []const []const u8, test_dir_info: TestDirInfo) !void {
    const allocator = std.testing.allocator;
    var argv = std.ArrayList([]const u8).init(allocator);
    defer argv.deinit();

    try argv.append("zig");
    try argv.append("ar");
    switch (format) {
        .gnu => try argv.append("--format=gnu"),
        .bsd => try argv.append("--format=bsd"),
    }

    const test_src_dir = try fs.cwd().openDir(test_src_dir_path, .{});

    for (file_names) |test_file| {
        try std.fs.Dir.copyFile(test_src_dir, test_file, test_dir_info.tmp_dir.dir, test_file, .{});
    }

    try argv.append("r");
    try argv.append(llvm_ar_archive_name);
    try argv.appendSlice(file_names);

    const result = try std.ChildProcess.exec(.{
        .allocator = allocator,
        .argv = argv.items,
        .cwd = test_dir_info.cwd,
    });

    defer {
        allocator.free(result.stdout);
        allocator.free(result.stderr);
    }
}
