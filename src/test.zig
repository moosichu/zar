const std = @import("std");
const builtin = @import("builtin");
const fs = std.fs;
const io = std.io;
const mem = std.mem;
const testing = std.testing;
const logger = std.log.scoped(.tests);
const trace = @import("tracy.zig").trace;
const traceNamed = @import("tracy.zig").traceNamed;
const Allocator = std.mem.Allocator;

const Archive = @import("archive/Archive.zig");
const main = @import("main.zig");
const build_options = @import("build_options");

// TODO: pass this through from build system
const path_to_zar = "../../../../zig-out/bin/zar";

const no_files = [_][]const u8{};
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
const always_invoke_zar_as_child_process = false;

test "Test Archive Text Basic" {
    const test_path = "test/data/test1";
    const test_names = [_][]const u8{ "input1.txt", "input2.txt" };

    const allocator = std.testing.allocator;

    var test1_dir = try fs.cwd().openDir(test_path, .{});
    defer test1_dir.close();

    var test_sequence: TestSequence = .{};
    defer test_sequence.deinit(allocator);

    for (test_names) |test1_name| {
        try test_sequence.copyTestFile(allocator, test1_dir, test1_name);
    }

    const archive_name = "test_archive.a";
    {
        const arguments = [_][]const u8{ "rc", archive_name } ++ test_names;
        try test_sequence.testArchiveOperation(allocator, &arguments);
    }

    const execution_options = TestSequence.ExecutionOptions.standardExecutionOptions();
    try test_sequence.execute(allocator, execution_options);
}

test "Test Archive Text With Long Filenames" {
    // Due to the fixed-size limits for filenames in the standard ar format,
    // this tests that the different ar-type specific extensions for dealing
    // with that properly work.
    const test_path = "test/data/test2";
    const test_names = [_][]const u8{ "input1.txt", "input2.txt", "input3_that_is_also_a_much_longer_file_name.txt", "input4_that_is_also_a_much_longer_file_name.txt" };

    const allocator = std.testing.allocator;

    var test_dir = try fs.cwd().openDir(test_path, .{});
    defer test_dir.close();

    var test_sequence: TestSequence = .{};
    defer test_sequence.deinit(allocator);

    for (test_names) |test2_name| {
        try test_sequence.copyTestFile(allocator, test_dir, test2_name);
    }

    const archive_name = "test_archive.a";
    {
        const arguments = [_][]const u8{ "rc", archive_name } ++ test_names;
        try test_sequence.testArchiveOperation(allocator, &arguments);
    }

    const execution_options = TestSequence.ExecutionOptions.standardExecutionOptions();
    try test_sequence.execute(allocator, execution_options);
}

test "Test MacOS aarch64" {
    const test_path = "test/data/test_macos_aarch64";
    const test_names = [_][]const u8{"a.o"};
    const allocator = std.testing.allocator;

    var test_dir = try fs.cwd().openDir(test_path, .{});
    defer test_dir.close();

    var test_sequence: TestSequence = .{};
    defer test_sequence.deinit(allocator);

    for (test_names) |test2_name| {
        try test_sequence.copyTestFile(allocator, test_dir, test2_name);
    }

    const archive_name = "test_archive.a";
    {
        const arguments = [_][]const u8{ "rc", archive_name } ++ test_names;
        try test_sequence.testArchiveOperation(allocator, &arguments);
    }

    var execution_options = TestSequence.ExecutionOptions.standardExecutionOptions();
    execution_options.targets = &[_]Target{
        .{
            .architecture = .aarch64,
            .operating_system = .macos,
        },
    };
    try test_sequence.execute(allocator, execution_options);
}

test "Test Archive With Symbols Basic" {
    const object_names = [_][]const u8{"input1.o"};
    const object_symbols = [_][]const []const u8{
        &[_][]const u8{ "input1_symbol1", "input1_symbol2" },
    };

    const allocator = std.testing.allocator;

    var test_sequence: TestSequence = .{};
    defer test_sequence.deinit(allocator);
    for (object_names, object_symbols) |object_name, symbols| {
        try test_sequence.buildObjectFile(allocator, object_name, symbols);
    }

    const archive_name = "test_archive.a";
    {
        const arguments = [_][]const u8{ "rc", archive_name } ++ object_names;
        try test_sequence.testArchiveOperation(allocator, &arguments);
    }
    {
        const arguments = [_][]const u8{ "rS", archive_name };
        try test_sequence.testArchiveOperation(allocator, &arguments);
    }
    {
        const arguments = [_][]const u8{ "s", archive_name };
        try test_sequence.testArchiveOperation(allocator, &arguments);
    }
    {
        const arguments = [_][]const u8{ "p", archive_name };
        try test_sequence.testArchiveOperation(allocator, &arguments);
    }
    {
        const arguments = [_][]const u8{ "t", archive_name };
        try test_sequence.testArchiveOperation(allocator, &arguments);
    }

    const execution_options = TestSequence.ExecutionOptions.standardExecutionOptions();
    try test_sequence.execute(allocator, execution_options);
}

test "Test Archive With Long Names And Symbols" {
    const object_names = [_][]const u8{ "input1.o", "input2.o", "input3_that_is_also_a_much_longer_file_name.o" };
    const object_symbols = [_][]const []const u8{
        &[_][]const u8{ "input1_symbol1", "input1_symbol2" },
        &[_][]const u8{ "input2_symbol1", "input2_symbol2_that_is_also_longer_symbol", "input2_symbol3" },
        &[_][]const u8{ "input3_that_is_also_a_much_longer_file_name_symbol1", "input3_symbol2_that_is_also_longer_symbol", "input3_symbol3_that_is_also_longer_symbol" },
    };
    const allocator = std.testing.allocator;

    var test_sequence: TestSequence = .{};
    defer test_sequence.deinit(allocator);
    for (object_names, object_symbols) |object_name, symbols| {
        try test_sequence.buildObjectFile(allocator, object_name, symbols);
    }

    const archive_name = "test_archive.a";
    {
        const arguments = [_][]const u8{ "rc", archive_name } ++ object_names;
        try test_sequence.testArchiveOperation(allocator, &arguments);
    }
    {
        const arguments = [_][]const u8{ "rS", archive_name };
        try test_sequence.testArchiveOperation(allocator, &arguments);
    }
    {
        const arguments = [_][]const u8{ "s", archive_name };
        try test_sequence.testArchiveOperation(allocator, &arguments);
    }

    const execution_options = TestSequence.ExecutionOptions.standardExecutionOptions();
    try test_sequence.execute(allocator, execution_options);
}

test "Test Archive Stress Test" {
    // Generate 55 different files with an arbitrary number of symbols
    const test6_filecount = 55;
    const test6_symcount = 15;
    var object_names: [test6_filecount][]u8 = undefined;
    var object_symbols: [test6_filecount][][]u8 = undefined;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    try initialiseTestData(allocator, &object_names, &object_symbols, test6_symcount);

    var test_sequence: TestSequence = .{};
    defer test_sequence.deinit(allocator);
    for (object_names, object_symbols) |object_name, symbols| {
        try test_sequence.buildObjectFile(allocator, object_name, symbols);
    }

    const archive_name = "test_archive.a";
    {
        const arguments = [_][]const u8{ "rc", archive_name } ++ object_names;
        try test_sequence.testArchiveOperation(allocator, &arguments);
    }
    {
        const arguments = [_][]const u8{ "rS", archive_name };
        try test_sequence.testArchiveOperation(allocator, &arguments);
    }
    {
        const arguments = [_][]const u8{ "s", archive_name };
        try test_sequence.testArchiveOperation(allocator, &arguments);
    }

    const testing_allocator = std.testing.allocator;

    const execution_options = TestSequence.ExecutionOptions.standardExecutionOptions();
    try test_sequence.execute(testing_allocator, execution_options);
}

test "Test Archive Sorted" {
    const object_names = [_][]const u8{ "dddd.o", "eeee.o", "ccccc.o", "aaaaaaaa.o", "aa.o", "cccc.o", "aaaa.o", "bbbb.o", "cc.o", "bb.o", "zz.o" };
    const object_symbols = [_][]const []const u8{
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

    const allocator = std.testing.allocator;

    var test_sequence: TestSequence = .{};
    defer test_sequence.deinit(allocator);
    for (object_names, object_symbols) |object_name, symbols| {
        try test_sequence.buildObjectFile(allocator, object_name, symbols);
    }

    const archive_name = "test_archive.a";
    {
        const arguments = [_][]const u8{ "rc", archive_name } ++ object_names;
        try test_sequence.testArchiveOperation(allocator, &arguments);
    }
    {
        const arguments = [_][]const u8{ "rS", archive_name };
        try test_sequence.testArchiveOperation(allocator, &arguments);
    }
    {
        const arguments = [_][]const u8{ "s", archive_name };
        try test_sequence.testArchiveOperation(allocator, &arguments);
    }
    {
        const arguments = [_][]const u8{ "p", archive_name };
        try test_sequence.testArchiveOperation(allocator, &arguments);
    }
    {
        const arguments = [_][]const u8{ "t", archive_name };
        try test_sequence.testArchiveOperation(allocator, &arguments);
    }
    {
        const arguments = [_][]const u8{ "d", archive_name, object_names[3], object_names[7] };
        try test_sequence.testArchiveOperation(allocator, &arguments);
    }

    const execution_options = TestSequence.ExecutionOptions.standardExecutionOptions();
    try test_sequence.execute(allocator, execution_options);
}

const TestSequence = struct {
    const ExecutionOptions = struct {
        targets: []const Target,
        pub fn standardExecutionOptions() ExecutionOptions {
            return .{
                .targets = &targets,
            };
        }
    };

    const TestOperation = union(enum) {
        const CopyTestFile = struct {
            src_dir: fs.Dir,
            file_name: []u8,
            pub fn init(allocator: Allocator, src_dir: fs.Dir, file_name: []const u8) !CopyTestFile {
                const owned_file_name = try allocator.alloc(u8, file_name.len);
                @memcpy(owned_file_name, file_name);
                errdefer allocator.free(owned_file_name);

                const result: CopyTestFile = .{
                    .src_dir = src_dir,
                    .file_name = owned_file_name,
                };

                return result;
            }
            pub fn deinit(copy_test_file: *CopyTestFile, allocator: Allocator) void {
                allocator.free(copy_test_file.file_name);
            }
        };
        const BuildObjectFile = struct {
            string_allocation: []u8,
            symbols: [][]const u8,
            object_name: []const u8,
            pub fn init(allocator: Allocator, object_name: []const u8, symbols: []const []const u8) !BuildObjectFile {
                const owned_symbols = try allocator.alloc([]const u8, symbols.len);

                var string_allocation = string_allocation: {
                    var string_allocation_size = object_name.len;
                    for (symbols) |symbol| {
                        string_allocation_size += symbol.len;
                    }
                    break :string_allocation try allocator.alloc(u8, string_allocation_size);
                };
                errdefer allocator.free(string_allocation);

                var current_index: usize = 0;
                const owned_object_name = owned_object_name: {
                    const next_index = current_index + object_name.len;
                    defer current_index = next_index;
                    const object_name_slice = string_allocation[current_index..next_index];
                    @memcpy(object_name_slice, object_name);
                    break :owned_object_name object_name_slice;
                };

                for (symbols, owned_symbols) |symbol, *owned_symbol| {
                    const next_index = current_index + symbol.len;
                    defer current_index = next_index;
                    const symbol_slice = string_allocation[current_index..next_index];
                    @memcpy(symbol_slice, symbol);
                    owned_symbol.* = symbol_slice;
                }

                const result: BuildObjectFile = .{
                    .string_allocation = string_allocation,
                    .object_name = owned_object_name,
                    .symbols = owned_symbols,
                };

                return result;
            }
            pub fn deinit(build_object_file: *BuildObjectFile, allocator: Allocator) void {
                allocator.free(build_object_file.string_allocation);
                allocator.free(build_object_file.symbols);
            }
        };
        const TestArchiveOperation = struct {
            string_allocation: []u8,
            arguments: [][]const u8,
            pub fn getArchiveArguments(test_archive_operation: *TestArchiveOperation, llvm_format: LlvmFormat) []const []const u8 {
                switch (llvm_format) {
                    .implicit => return test_archive_operation.arguments[1..],
                    else => {
                        test_archive_operation.arguments[0] = llvm_format.llvmFormatToArgument();
                        return test_archive_operation.arguments;
                    },
                }
            }

            pub fn init(
                allocator: Allocator,
                arguments: []const []const u8,
            ) !TestArchiveOperation {
                var owned_arguments = try allocator.alloc([]const u8, arguments.len + 1);
                errdefer allocator.free(owned_arguments);

                var string_allocation = string_allocation: {
                    var string_size: usize = 0;
                    for (arguments) |argument| {
                        string_size += argument.len;
                    }
                    break :string_allocation try allocator.alloc(u8, string_size);
                };
                errdefer allocator.free(string_allocation);

                {
                    var current_index: usize = 0;
                    for (owned_arguments[1..], arguments) |*owned_argument, argument| {
                        const next_index = current_index + argument.len;
                        defer current_index = next_index;
                        const argument_slice = string_allocation[current_index..next_index];
                        @memcpy(argument_slice, argument);
                        owned_argument.* = argument_slice;
                    }
                }

                const result: TestOperation.TestArchiveOperation = .{
                    .arguments = owned_arguments,
                    .string_allocation = string_allocation,
                };
                return result;
            }

            pub fn deinit(test_archive_operation: *TestArchiveOperation, allocator: Allocator) void {
                allocator.free(test_archive_operation.string_allocation);
                allocator.free(test_archive_operation.arguments);
            }
        };
        pub fn deinit(test_operation: *TestOperation, allocator: Allocator) void {
            switch (test_operation.*) {
                .build_object_file => |*build_object_file| {
                    build_object_file.deinit(allocator);
                },
                .test_archive_operation => |*test_archive_operation| {
                    test_archive_operation.deinit(allocator);
                },
                .copy_test_file => |*copy_test_file| {
                    copy_test_file.deinit(allocator);
                },
            }
        }
        build_object_file: BuildObjectFile,
        test_archive_operation: TestArchiveOperation,
        copy_test_file: CopyTestFile,
    };

    test_operations: std.ArrayListUnmanaged(TestOperation) = .{},

    pub fn copyTestFile(
        test_sequence: *TestSequence,
        allocator: Allocator,
        src_dir: std.fs.Dir,
        file_name: []const u8,
    ) !void {
        const copy_test_file = try TestOperation.CopyTestFile.init(
            allocator,
            src_dir,
            file_name,
        );
        try test_sequence.test_operations.append(allocator, .{
            .copy_test_file = copy_test_file,
        });
    }

    pub fn buildObjectFile(
        test_sequence: *TestSequence,
        allocator: Allocator,
        object_name: []const u8,
        symbols: []const []const u8,
    ) !void {
        const build_object_file = try TestOperation.BuildObjectFile.init(
            allocator,
            object_name,
            symbols,
        );
        try test_sequence.test_operations.append(allocator, .{
            .build_object_file = build_object_file,
        });
    }

    pub fn testArchiveOperation(
        test_sequence: *TestSequence,
        allocator: Allocator,
        arguments: []const []const u8,
    ) !void {
        const test_archive_operation = try TestOperation.TestArchiveOperation.init(allocator, arguments);
        try test_sequence.test_operations.append(allocator, .{
            .test_archive_operation = test_archive_operation,
        });
    }

    pub fn deinit(
        test_sequence: *TestSequence,
        allocator: Allocator,
    ) void {
        for (test_sequence.test_operations.items) |*test_operation| {
            test_operation.deinit(allocator);
        }
        test_sequence.test_operations.deinit(allocator);
    }
    pub fn execute(
        test_sequence: TestSequence,
        allocator: Allocator,
        execution_options: ExecutionOptions,
    ) !void {
        // TODO: get this from the execution options
        for (execution_options.targets) |target| {
            const llvm_format_options = [_]LlvmFormat{ .implicit, target.operating_system.toDefaultLlvmFormat() };
            for (llvm_format_options) |llvm_format_option| {
                var test_dir_info = try TestDirInfo.getInfo();
                // if a test is going to fail anyway, this is a useful way to debug it for now..
                var cancel_cleanup = false;
                defer if (!cancel_cleanup) test_dir_info.cleanup();
                errdefer |err| {
                    cancel_cleanup = true;
                    logger.err("Failed archiving test for target ({s}): {}", .{ target.targetToArgument(), err });
                }

                for (test_sequence.test_operations.items) |*test_operation| {
                    switch (test_operation.*) {
                        .build_object_file => |build_object_file| {
                            try generateCompiledFilesWithSymbols(
                                allocator,
                                target,
                                &[_][]const u8{build_object_file.object_name},
                                &[_][]const []const u8{build_object_file.symbols},
                                test_dir_info,
                            );
                        },
                        .test_archive_operation => |*test_archive_operation| {
                            try compareArchivers(test_archive_operation.getArchiveArguments(llvm_format_option), test_dir_info);
                        },
                        .copy_test_file => |copy_test_file| {
                            try copy_test_file.src_dir.copyFile(copy_test_file.file_name, test_dir_info.llvm_ar_wd, copy_test_file.file_name, .{});
                            try copy_test_file.src_dir.copyFile(copy_test_file.file_name, test_dir_info.zar_wd, copy_test_file.file_name, .{});
                        },
                    }
                }
            }
        }
    }
};

test "Test Argument Errors" {
    if (builtin.target.os.tag == .windows) {
        return;
    }
    const allocator = std.testing.allocator;
    var test_dir_info = try TestDirInfo.getInfo();
    defer test_dir_info.cleanup();

    var argv = std.ArrayList([]const u8).init(allocator);
    defer argv.deinit();

    {
        try argv.resize(0);
        const expected_out: ExpectedOut = .{
            .stderr = "error(archive_main): An operation must be provided.\n",
        };

        try invokeZar(allocator, argv.items, test_dir_info, expected_out);
    }

    {
        try argv.resize(0);
        try argv.append("j");
        const expected_out: ExpectedOut = .{
            .stderr = "error(archive_main): 'j' is not a valid operation.\n",
        };

        try invokeZar(allocator, argv.items, test_dir_info, expected_out);
    }

    {
        try argv.resize(0);
        try argv.append("rj");
        const expected_out: ExpectedOut = .{
            .stderr = "error(archive_main): 'j' is not a valid modifier.\n",
        };

        try invokeZar(allocator, argv.items, test_dir_info, expected_out);
    }
}

fn initialiseTestData(allocator: Allocator, file_names: [][]u8, symbol_names: [][][]u8, symbol_count: u32) !void {
    for (file_names, 0..) |_, index| {
        file_names[index] = try std.fmt.allocPrint(allocator, "index_{}.o", .{index});
    }
    for (symbol_names, 0..) |_, file_index| {
        symbol_names[file_index] = try allocator.alloc([]u8, symbol_count);
        for (symbol_names[file_index], 0..) |_, symbol_index| {
            symbol_names[file_index][symbol_index] = try std.fmt.allocPrint(allocator, "symbol_{}_file_{}", .{ symbol_index, file_index });
        }
    }
    return;
}

const targets = result: {
    const os_fields = std.meta.fields(OperatingSystem);
    const arch_fields = std.meta.fields(Architecture);
    var aggregator: [os_fields.len * arch_fields.len]Target = undefined;
    var target_index = 0;
    // "for"s were inline
    for (os_fields) |os_field| {
        for (arch_fields) |arch_field| {
            aggregator[target_index] = .{
                .architecture = @as(Architecture, @enumFromInt(arch_field.value)),
                .operating_system = @as(OperatingSystem, @enumFromInt(os_field.value)),
            };
            target_index += 1;
        }
    }
    break :result aggregator;
};

const Target = struct {
    architecture: Architecture,
    operating_system: OperatingSystem,

    fn targetToArgument(target: Target) []const u8 {
        switch (target.architecture) {
            inline else => |architecture| {
                switch (target.operating_system) {
                    inline else => |operating_system| {
                        return @tagName(architecture) ++ "-" ++ @tagName(operating_system);
                    },
                }
            },
        }
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
    var aggregator: [fields.len]LlvmFormat = undefined;
    for (fields, 0..) |field, field_index| {
        aggregator[field_index] = @as(LlvmFormat, @enumFromInt(field.value));
    }
    break :result aggregator;
};

const LlvmFormat = enum {
    gnu,
    bsd,
    darwin,
    implicit,

    fn llvmFormatToArgument(format: LlvmFormat) []const u8 {
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
    zar_wd: std.fs.Dir,
    llvm_ar_wd: std.fs.Dir,
    cwd: []const u8,

    pub fn getInfo() !TestDirInfo {
        var result: TestDirInfo = .{
            .tmp_dir = std.testing.tmpDir(.{}),
            .cwd = undefined,
            .zar_wd = undefined,
            .llvm_ar_wd = undefined,
        };
        errdefer result.tmp_dir.cleanup();

        try result.tmp_dir.dir.makeDir("zar_wd");
        result.zar_wd = try result.tmp_dir.dir.openDir("zar_wd", .{ .iterate = true });
        errdefer result.zar_wd.close();

        try result.tmp_dir.dir.makeDir("llvm_ar_wd");
        result.llvm_ar_wd = try result.tmp_dir.dir.openDir("llvm_ar_wd", .{ .iterate = true });
        errdefer result.llvm_ar_wd.close();

        result.cwd = try std.fs.path.join(std.testing.allocator, &[_][]const u8{
            ".zig-cache", "tmp", &result.tmp_dir.sub_path,
        });
        return result;
    }

    pub fn cleanup(self: *TestDirInfo) void {
        self.zar_wd.close();
        self.llvm_ar_wd.close();
        self.tmp_dir.cleanup();
        std.testing.allocator.free(self.cwd);
    }
};

fn compareGeneratedArchives(test_dir_info: TestDirInfo) !void {
    const tracy = trace(@src());
    defer tracy.end();
    const allocator = std.testing.allocator;

    var walker = try test_dir_info.llvm_ar_wd.walk(allocator);
    defer walker.deinit();
    while (try walker.next()) |walk| {
        const llvm_ar_file_handle = try test_dir_info.llvm_ar_wd.openFile(walk.path, .{ .mode = .read_only });
        defer llvm_ar_file_handle.close();
        const zig_ar_file_handle = try test_dir_info.zar_wd.openFile(walk.path, .{ .mode = .read_only });
        defer zig_ar_file_handle.close();

        const llvm_ar_stat = try llvm_ar_file_handle.stat();
        const zig_ar_stat = try zig_ar_file_handle.stat();

        try testing.expectEqual(llvm_ar_stat.size, zig_ar_stat.size);

        const llvm_ar_buffer = try allocator.alloc(u8, llvm_ar_stat.size);
        const zig_ar_buffer = try allocator.alloc(u8, zig_ar_stat.size);
        defer allocator.free(llvm_ar_buffer);
        defer allocator.free(zig_ar_buffer);

        {
            const llvm_ar_read = try llvm_ar_file_handle.preadAll(llvm_ar_buffer, 0);
            try testing.expectEqual(llvm_ar_read, llvm_ar_stat.size);
        }

        {
            const zig_ar_read = try zig_ar_file_handle.preadAll(zig_ar_buffer, 0);
            try testing.expectEqual(zig_ar_read, zig_ar_stat.size);
        }

        for (llvm_ar_buffer, 0..) |llvm_ar_byte, index| {
            const zig_ar_byte = zig_ar_buffer[index];
            try testing.expectEqual(llvm_ar_byte, zig_ar_byte);
        }
    }
}

const ExpectedOut = struct {
    stdout: ?[]const u8 = null,
    stderr: ?[]const u8 = null,
};

fn invokeZar(allocator: mem.Allocator, arguments: []const []const u8, test_dir_info: TestDirInfo, expected_out: ExpectedOut) !void {
    errdefer |err| {
        logger.err("test failure: {}", .{err});
    }

    var argv: std.ArrayListUnmanaged([]const u8) = .{};
    defer argv.deinit(allocator);
    try argv.append(allocator, path_to_zar);
    try argv.appendSlice(allocator, arguments);

    // argments[0] must be path_to_zar
    var invoke_as_child_process = always_invoke_zar_as_child_process;
    // At the moment it's easiest to verify the output of stdout/stderr by launching
    // zar as a child process, so just doing it like this for now.
    invoke_as_child_process = invoke_as_child_process or expected_out.stderr != null;
    invoke_as_child_process = invoke_as_child_process or expected_out.stdout != null;
    if (invoke_as_child_process) {
        errdefer |err| {
            logger.err("{}: {s} {}", .{ err, argv.items, test_dir_info.zar_wd });
        }
        const result = try std.process.Child.run(.{
            .allocator = allocator,
            .argv = argv.items,
            .cwd_dir = test_dir_info.zar_wd,
        });

        defer {
            allocator.free(result.stdout);
            allocator.free(result.stderr);
        }

        if (expected_out.stdout) |expected_stdout| {
            try testing.expectEqualStrings(expected_stdout, result.stdout);
            errdefer |err| {
                logger.err("{}, also received stdout \"{s}\", expected \"{s}\"", .{ err, result.stdout, expected_stdout });
            }
        }
        if (expected_out.stderr) |expected_stderr| {
            try testing.expectEqualStrings(expected_stderr, result.stderr);
            errdefer |err| {
                logger.err("{}, also received stderr \"{s}\", expected \"{s}\"", .{ err, result.stdout, expected_stderr });
            }
        }
    } else {
        // TODO: don't deinit testing allocator here so that we can confirm
        // the archiver does everything by the books?
        var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
        defer arena.deinit();

        main.archiveMain(test_dir_info.zar_wd, allocator, argv.items) catch {};
    }
}

fn compareArchivers(arguments: []const []const u8, test_dir_info: TestDirInfo) !void {
    errdefer {
        logger.err("Failure occured when comparing archivers with arguments: ({s})", .{arguments});
    }
    const allocator = std.testing.allocator;

    const llvm_run_result = llvm_run_result: {
        const tracy = traceNamed(@src(), "llvm ar");
        defer tracy.end();
        var argv: std.ArrayListUnmanaged([]const u8) = .{};
        defer argv.deinit(allocator);

        try argv.append(allocator, build_options.zig_exe_path);
        try argv.append(allocator, "ar");
        try argv.appendSlice(allocator, arguments);

        const result = try std.process.Child.run(.{
            .allocator = allocator,
            .argv = argv.items,
            .cwd_dir = test_dir_info.llvm_ar_wd,
        });
        break :llvm_run_result result;
    };

    defer {
        allocator.free(llvm_run_result.stdout);
        allocator.free(llvm_run_result.stderr);
    }
    try invokeZar(allocator, arguments, test_dir_info, .{
        .stderr = llvm_run_result.stderr,
        .stdout = llvm_run_result.stdout,
    });
    try compareGeneratedArchives(test_dir_info);
}

fn generateCompiledFilesWithSymbols(framework_allocator: Allocator, target: Target, file_names: []const []const u8, symbol_names: []const []const []const u8, test_dir_info: TestDirInfo) !void {
    const tracy = trace(@src());
    defer tracy.end();

    const worker_count = @max(1, std.Thread.getCpuCount() catch 1);
    const child_processes = try framework_allocator.alloc(std.process.Child, worker_count);
    defer framework_allocator.free(child_processes);

    var argv = std.ArrayList([]const u8).init(framework_allocator);
    defer argv.deinit();
    try argv.append(build_options.zig_exe_path);
    try argv.append("cc");
    try argv.append("-c");
    try argv.append("-o");
    const file_name_arg = argv.items.len;
    try argv.append("");
    const source_name_arg = argv.items.len;
    try argv.append("");
    try argv.append("-target");
    // TODO: Test other target triples with appropriate corresponding archive format!
    try argv.append(target.targetToArgument());

    for (file_names, symbol_names, 0..) |file_name, file_symbols, index| {
        const process_index = @mod(index, child_processes.len);
        if (index >= child_processes.len) {
            // TODO: read results etc.
            _ = try child_processes[process_index].wait();
        }

        const source_file_name = try std.fmt.allocPrint(framework_allocator, "{s}.c", .{file_name});
        defer framework_allocator.free(source_file_name);
        {
            const source_file = try test_dir_info.tmp_dir.dir.createFile(source_file_name, .{});
            defer source_file.close();

            const writer = source_file.writer();
            for (file_symbols) |symbol| {
                try writer.print("extern int {s}(int a) {{ return a; }}\n", .{symbol});
            }
        }

        argv.items[file_name_arg] = file_name;
        argv.items[source_name_arg] = source_file_name;

        child_processes[process_index] = std.process.Child.init(argv.items, framework_allocator);
        child_processes[process_index].cwd = test_dir_info.cwd;
        try child_processes[process_index].spawn();
    }

    {
        var process_index: u32 = 0;
        while (process_index < symbol_names.len and process_index < child_processes.len) {
            // TODO: read results etc.
            _ = try child_processes[process_index].wait();
            process_index += 1;
        }
    }

    for (file_names) |file_name| {
        try test_dir_info.tmp_dir.dir.copyFile(file_name, test_dir_info.zar_wd, file_name, .{});
        try test_dir_info.tmp_dir.dir.copyFile(file_name, test_dir_info.llvm_ar_wd, file_name, .{});
    }
}
