const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;
const testing = std.testing;
const logger = std.log.scoped(.tests);
const trace = @import("tracy.zig").trace;
const Allocator = std.mem.Allocator;

const Archive = @import("archive/Archive.zig");
const main = @import("main.zig");

const llvm_ar_archive_name = "llvm-ar-archive.a";
const zig_ar_archive_name = "zig-ar-archive.a";

const no_symbols = [_][][]const u8{};
const no_dir = "test/data/none";

// Testing TODOs:
// - Create symbol comparison tests (generate source files procedurally)
// - Add testing matrices for testing different combinations of arguments & modifiers
// - Create end-to-end tests that check stdout of parsing functionality (not just archive generation)
//   Including variatns of modifiers
// - Create end-to-end tests that check extracted files are the same
// - Create "stress" tests that go beyond the basic tests & auto-generate a massive amount of
//   archive input that can be tested against.
// - Test the failure cases (and see how we handle them)
// - Add parsing tests using archives created by native archivers on appropriate platforms
// - Fuzz test
// - Test weird combinations and try to match llvm-ar output
// - Test multiple os/format combinations (i.e. bsd style archives)
// - Test ranlib functionality
// - Test bad inputs/
// - Test performance
// - Don't redo work between tests (compiling same files, running llvm ar multiple times).

// Allows us to invoke zar as a program, just to really confirm it works
// end-to-end.
const invoke_zar_as_child_process = false;

test "Test Archive Text Basic" {
    const test1_dir = "test/data/test1";
    const test1_names = [_][]const u8{ "input1.txt", "input2.txt" };
    const test1_symbols = no_symbols;
    try doStandardTests(test1_dir, &test1_names, &test1_symbols);
}

test "Test Archive Text With Long Filenames" {
    // Due to the fixed-size limits for filenames in the standard ar format,
    // this tests that the different ar-type specific extensions for dealing
    // with that properly work.
    const test2_dir = "test/data/test2";
    const test2_names = [_][]const u8{ "input1.txt", "input2.txt", "input3_that_is_also_a_much_longer_file_name.txt", "input4_that_is_also_a_much_longer_file_name.txt" };
    const test2_symbols = no_symbols;
    try doStandardTests(test2_dir, &test2_names, &test2_symbols);
}

test "Test Archive With Symbols Basic" {
    const test4_names = [_][]const u8{"input1.o"};
    const test4_symbols = [_][]const []const u8{
        &[_][]const u8{ "input1_symbol1", "input1_symbol2" },
    };
    try doStandardTests(no_dir, &test4_names, &test4_symbols);
}

test "Test Archive With Long Names And Symbols" {
    const test5_names = [_][]const u8{ "input1.o", "input2.o", "input3_that_is_also_a_much_longer_file_name.o" };
    const test5_symbols = [_][]const []const u8{
        &[_][]const u8{ "input1_symbol1", "input1_symbol2" },
        &[_][]const u8{ "input2_symbol1", "input2_symbol2_that_is_also_longer_symbol", "input2_symbol3" },
        &[_][]const u8{ "input3_that_is_also_a_much_longer_file_name_symbol1", "input3_symbol2_that_is_also_longer_symbol", "input3_symbol3_that_is_also_longer_symbol" },
    };
    try doStandardTests(no_dir, &test5_names, &test5_symbols);
}

test "Test Archive Stress Test" {
    // Generate 55 different files with an arbitrary number of symbols
    const test6_filecount = 55;
    const test6_symcount = 15;
    var test6_names: [test6_filecount][]u8 = undefined;
    var test6_symbols: [test6_filecount][][]u8 = undefined;
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    try initialiseTestData(arena.allocator(), &test6_names, &test6_symbols, test6_symcount);
    try doStandardTests(no_dir, &test6_names, &test6_symbols);
}

test "Test Archive Sorted" {
    // Confirm that our archives default files & their symbols in the correct way
    // for each target.
    const test_sort_names = [_][]const u8{ "dddd.o", "eeee.o", "ccccc.o", "aaaaaaaa.o", "aa.o", "cccc.o", "aaaa.o", "bbbb.o", "cc.o", "bb.o", "zz.o" };
    const test_sort = [_][]const []const u8{
        &[_][]const u8{ "ddd", "aaa" },
        &[_][]const u8{ "cccc", "ddd", "aaaa" },
        &[_][]const u8{ "z", "aa", "a" },
        &[_][]const u8{ "agsg", "ssss", "aaaa" },
        &[_][]const u8{ "_1_2_3", "__1", "_00000" },
        &[_][]const u8{ "AA", "aa", "BB" },
        &[_][]const u8{ "aa", "AA", "BB" },
        &[_][]const u8{ "BB", "AA", "aa" },
        &[_][]const u8{ "_123", "_22", "_12" },
        &[_][]const u8{ "bB", "aB", "cB" },
        &[_][]const u8{ "_11", "_12", "_13" },
    };
    // TODO: remove redundancy maybe by excluding parsing component of this test?
    try doStandardTests(no_dir, &test_sort_names, &test_sort);
}

fn initialiseTestData(allocator: Allocator, file_names: [][]const u8, symbol_names: [][][]const u8, symbol_count: u32) !void {
    for (file_names) |_, index| {
        file_names[index] = try std.fmt.allocPrint(allocator, "index_{}.o", .{index});
    }
    for (symbol_names) |_, file_index| {
        symbol_names[file_index] = try allocator.alloc([]u8, symbol_count);
        for (symbol_names[file_index]) |_, symbol_index| {
            symbol_names[file_index][symbol_index] = try std.fmt.allocPrint(allocator, "symbol_{}_file_{}", .{ symbol_index, file_index });
        }
    }
    return;
}

const targets = result: {
    const os_fields = std.meta.fields(OperatingSystem);
    const arch_fields = std.meta.fields(Architecture);
    comptime var aggregator: [os_fields.len * arch_fields.len]Target = undefined;
    comptime var target_index = 0;
    inline for (os_fields) |os_field| {
        inline for (arch_fields) |arch_field| {
            aggregator[target_index] = .{
                .architecture = @intToEnum(Architecture, arch_field.value),
                .operating_system = @intToEnum(OperatingSystem, os_field.value),
            };
            target_index += 1;
        }
    }
    break :result aggregator;
};

const Target = struct {
    architecture: Architecture,
    operating_system: OperatingSystem,

    fn targetToArgument(comptime target: Target) []const u8 {
        return @tagName(target.architecture) ++ "-" ++ @tagName(target.operating_system);
    }
};

const OperatingSystem = enum {
    linux,
    macos,
    freebsd,
    // windows,

    fn toDefaultLlvmFormat(operating_system: OperatingSystem) LlvmFormat {
        return switch (operating_system) {
            .linux => .gnu,
            .macos => .darwin,
            .freebsd => .gnu,
        };
    }
};

const Architecture = enum {
    aarch64,
    x86_64,
};

const llvm_formats = result: {
    const fields = std.meta.fields(LlvmFormat);
    comptime var aggregator: [fields.len]LlvmFormat = undefined;
    inline for (fields) |field, field_index| {
        aggregator[field_index] = @intToEnum(LlvmFormat, field.value);
    }
    break :result aggregator;
};

const LlvmFormat = enum {
    gnu,
    bsd,
    darwin,
    implicit,

    fn llvmFormatToArgument(comptime format: LlvmFormat) []const u8 {
        switch (format) {
            .gnu => return "--format=gnu",
            .bsd => return "--format=bsd",
            .darwin => return "--format=darwin",
            .implicit => return "",
        }
    }
};

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

pub fn doStandardTests(comptime test_dir_path: []const u8, file_names: []const []const u8, symbol_names: []const []const []const u8) !void {
    const tracy = trace(@src());
    defer tracy.end();
    const operation = "rc";

    inline for (targets) |target| {
        var test_dir_info = try TestDirInfo.getInfo();
        // if a test is going to fail anyway, this is a useful way to debug it for now..
        var cancel_cleanup = false;
        defer if (!cancel_cleanup) test_dir_info.cleanup();
        errdefer {
            cancel_cleanup = true;
        }

        // Create an archive with llvm ar & zar and confirm that the outputs match
        // byte-for-byte.
        try copyAssetsToTestDirectory(test_dir_path, file_names, test_dir_info);
        const llvm_format = comptime target.operating_system.toDefaultLlvmFormat();
        try generateCompiledFilesWithSymbols(target, file_names, symbol_names, test_dir_info);

        {
            try doLlvmArchiveOperation(.implicit, operation, file_names, test_dir_info);
            try testParsingOfLlvmGeneratedArchive(target, .implicit, file_names, symbol_names, test_dir_info);
            try testArchiveCreation(target, .implicit, file_names, test_dir_info);
            try test_dir_info.tmp_dir.dir.deleteFile(llvm_ar_archive_name);
        }
        {
            try doLlvmArchiveOperation(llvm_format, operation, file_names, test_dir_info);
            try testParsingOfLlvmGeneratedArchive(target, llvm_format, file_names, symbol_names, test_dir_info);
            try testArchiveCreation(target, llvm_format, file_names, test_dir_info);
            try test_dir_info.tmp_dir.dir.deleteFile(llvm_ar_archive_name);
        }
    }
}

fn testArchiveCreation(comptime target: Target, comptime format: LlvmFormat, file_names: []const []const u8, test_dir_info: TestDirInfo) !void {
    const tracy = trace(@src());
    defer tracy.end();

    errdefer {
        logger.err("Failed creation {s} on format {}", .{ target.targetToArgument(), format });
    }
    const operation = "rc";
    try doZarArchiveOperation(format, operation, file_names, test_dir_info);
    var cancel_cleanup = false;
    defer if (!cancel_cleanup) {
        test_dir_info.tmp_dir.dir.deleteFile(zig_ar_archive_name) catch |err| {
            logger.warn("error {} deleting {s}", .{ err, zig_ar_archive_name });
        };
    };
    errdefer {
        cancel_cleanup = true;
    }

    try compareGeneratedArchives(test_dir_info);
}

fn testParsingOfLlvmGeneratedArchive(comptime target: Target, comptime format: LlvmFormat, file_names: []const []const u8, symbol_names: []const []const []const u8, test_dir_info: TestDirInfo) !void {
    errdefer {
        logger.err("Failed parsing {s} on format {}", .{ target.targetToArgument(), format });
    }

    try testArchiveParsing(target, test_dir_info, file_names, symbol_names);
}

fn compareGeneratedArchives(test_dir_info: TestDirInfo) !void {
    const tracy = trace(@src());
    defer tracy.end();
    const allocator = std.testing.allocator;
    const llvm_ar_file_handle = try test_dir_info.tmp_dir.dir.openFile(llvm_ar_archive_name, .{ .mode = .read_only });
    defer llvm_ar_file_handle.close();
    const zig_ar_file_handle = try test_dir_info.tmp_dir.dir.openFile(zig_ar_archive_name, .{ .mode = .read_only });
    defer zig_ar_file_handle.close();

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

fn testArchiveParsing(comptime target: Target, test_dir_info: TestDirInfo, file_names: []const []const u8, symbol_names: []const []const []const u8) !void {
    const tracy = trace(@src());
    defer tracy.end();
    const test_dir = test_dir_info.tmp_dir.dir;

    const archive_file = try test_dir.openFile(llvm_ar_archive_name, .{ .mode = .read_only });
    defer archive_file.close();

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var allocator = arena.allocator();

    var archive = try Archive.create(test_dir, archive_file, llvm_ar_archive_name, Archive.ArchiveType.ambiguous, .{}, false);
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

    if (target.operating_system == .macos) {
        // TODO: darwin files are sorted by default, we need to make sure our
        // test can account for this!
        return;
    }

    var current_index = @as(u32, 0);
    for (symbol_names) |symbol_names_in_file, file_index| {
        for (symbol_names_in_file) |symbol_name| {
            const parsed_symbol = archive.symbols.items[current_index];
            var parsed_symbol_name = parsed_symbol.name;
            // darwin targets will prepend symbol names with underscores
            if (target.operating_system == .macos) {
                try testing.expect(parsed_symbol_name[0] == '_');
                parsed_symbol_name = parsed_symbol_name[1..parsed_symbol_name.len];
            }
            try testing.expect(mem.eql(u8, parsed_symbol_name, symbol_name));
            try testing.expect(mem.eql(u8, archive.files.items[parsed_symbol.file_index].name, file_names[file_index]));
            current_index = current_index + 1;
        }
    }
}

fn copyAssetsToTestDirectory(comptime test_src_dir_path: []const u8, file_names: []const []const u8, test_dir_info: TestDirInfo) !void {
    const tracy = trace(@src());
    defer tracy.end();
    var test_src_dir = fs.cwd().openDir(test_src_dir_path, .{}) catch |err| switch (err) {
        error.FileNotFound => return,
        else => return err,
    };
    defer test_src_dir.close();

    for (file_names) |test_file| {
        std.fs.Dir.copyFile(test_src_dir, test_file, test_dir_info.tmp_dir.dir, test_file, .{}) catch |err| switch (err) {
            error.FileNotFound => continue,
            else => return err,
        };
    }
}

fn doZarArchiveOperation(comptime format: LlvmFormat, comptime operation: []const u8, file_names: []const []const u8, test_dir_info: TestDirInfo) !void {
    const tracy = trace(@src());
    defer tracy.end();
    const allocator = std.testing.allocator;

    var argv = std.ArrayList([]const u8).init(allocator);
    defer argv.deinit();

    try argv.append("../../../zig-out/bin/zar");
    try argv.append(format.llvmFormatToArgument());

    try argv.append(operation);
    try argv.append(zig_ar_archive_name);
    try argv.appendSlice(file_names);

    if (invoke_zar_as_child_process) {
        const result = try std.ChildProcess.exec(.{
            .allocator = allocator,
            .argv = argv.items,
            .cwd = test_dir_info.cwd,
        });

        defer {
            allocator.free(result.stdout);
            allocator.free(result.stderr);
        }
    } else {
        var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
        defer arena.deinit();

        try main.archiveMain(test_dir_info.tmp_dir.dir, arena.allocator(), argv.items);
    }
}

fn doLlvmArchiveOperation(comptime format: LlvmFormat, comptime operation: []const u8, file_names: []const []const u8, test_dir_info: TestDirInfo) !void {
    const tracy = trace(@src());
    defer tracy.end();
    const allocator = std.testing.allocator;
    var argv = std.ArrayList([]const u8).init(allocator);
    defer argv.deinit();

    try argv.append("zig");
    try argv.append("ar");
    try argv.append(format.llvmFormatToArgument());

    try argv.append(operation);
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

fn generateCompiledFilesWithSymbols(comptime target: Target, file_names: []const []const u8, symbol_names: []const []const []const u8, test_dir_info: TestDirInfo) !void {
    const tracy = trace(@src());
    defer tracy.end();
    const allocator = std.testing.allocator;

    for (symbol_names) |file_symbols, index| {
        const file_name = file_names[index];
        const source_file_name = try std.fmt.allocPrint(allocator, "{s}.c", .{file_name});
        defer allocator.free(source_file_name);
        {
            const source_file = try test_dir_info.tmp_dir.dir.createFile(source_file_name, .{});
            defer source_file.close();

            const writer = source_file.writer();
            for (file_symbols) |symbol| {
                try writer.print("extern int {s}(int a) {{ return a; }}\n", .{symbol});
            }
        }

        var argv = std.ArrayList([]const u8).init(allocator);
        defer argv.deinit();

        try argv.append("zig");
        try argv.append("cc");
        try argv.append("-c");
        try argv.append("-o");
        try argv.append(file_name);
        try argv.append(source_file_name);
        try argv.append("-target");
        // TODO: Test other target triples with appropriate corresponding archive format!
        try argv.append(target.targetToArgument());

        // TODO: Do this in non-blocking way and spawn process  per-thread!
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
}
