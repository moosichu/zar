const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("zar", "src/main.zig");

    var tests = b.addTest("src/test.zig");

    const exe_options = b.addOptions();
    exe.addOptions("build_options", exe_options);
    tests.addOptions("build_options", exe_options);

    {
        const tracy = b.option([]const u8, "tracy", "Enable Tracy integration. Supply path to Tracy source");
        const tracy_callstack = b.option(bool, "tracy-callstack", "Include callstack information with Tracy data. Does nothing if -Dtracy is not provided") orelse false;
        const tracy_allocation = b.option(bool, "tracy-allocation", "Include allocation information with Tracy data. Does nothing if -Dtracy is not provided") orelse false;

        exe_options.addOption(bool, "enable_tracy", tracy != null);
        exe_options.addOption(bool, "enable_tracy_callstack", tracy_callstack);
        exe_options.addOption(bool, "enable_tracy_allocation", tracy_allocation);

        // "-DTRACY_NO_EXIT=1"

        if (tracy) |tracy_path| {
            const client_cpp = std.fs.path.join(
                b.allocator,
                &[_][]const u8{ tracy_path, "TracyClient.cpp" },
            ) catch unreachable;

            // On mingw, we need to opt into windows 7+ to get some features required by tracy.
            const tracy_c_flags: []const []const u8 = if (target.isWindows() and target.getAbi() == .gnu)
                &[_][]const u8{ "-DTRACY_ENABLE=1", "-fno-sanitize=undefined", "-D_WIN32_WINNT=0x601" }
            else
                &[_][]const u8{ "-DTRACY_ENABLE=1", "-fno-sanitize=undefined" };

            exe.addIncludePath(tracy_path);
            exe.addCSourceFile(client_cpp, tracy_c_flags);
            exe.linkLibC();
            exe.linkLibCpp();

            tests.addIncludePath(tracy_path);
            tests.addCSourceFile(client_cpp, tracy_c_flags);
            tests.linkLibC();
            tests.linkLibCpp();

            if (target.isWindows()) {
                exe.linkSystemLibrary("dbghelp");
                exe.linkSystemLibrary("ws2_32");

                tests.linkSystemLibrary("dbghelp");
                tests.linkSystemLibrary("ws2_32");
            }
        }
    }

    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&tests.step);
}
