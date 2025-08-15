const std = @import("std");
const mem = std.mem;

const zar_version = std.SemanticVersion{ .major = 0, .minor = 0, .patch = 1 };

pub fn build(b: *std.Build) !void {
    // Standard target options allows the person running `zig build`
    // to choose what target to build for.
    // Here we do not override the defaults, which means any target is
    // allowed, and the default is native.
    // Other options for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build`
    // to select between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const optimize = b.standardOptimizeOption(.{});

    const test_filter = b.option([]const u8, "test-filter", "Filter tests by name");

    const exe_mod = b.addModule("zar", .{
        .root_source_file = .{ .src_path = .{ .owner = b, .sub_path = "src/main.zig" } },
        .target = target,
        .optimize = optimize,
    });

    const test_mod = b.addModule("tests", .{
        .root_source_file = .{ .src_path = .{ .owner = b, .sub_path = "src/test.zig" } },
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{ .name = "zar", .root_module = exe_mod, .version = zar_version });
    var tests = b.addTest(.{
        .root_module = test_mod,
        .filters = if (test_filter != null) &.{test_filter.?} else &.{},
    });

    // TODO: Figure this out!
    // {
    //     const zld_options = b.addOptions();
    //     zld.addOptions("build_options", zld_options);
    //     zld_options.addOption(bool, "enable_logging", false);
    //     zld_options.addOption(bool, "enable_tracy", tracy != null);
    // }

    const build_test_executable_only = b.option(bool, "build-tests", "Build tests but don't run them.") orelse false;

    const exe_options = b.addOptions();
    exe_mod.addOptions("build_options", exe_options);
    test_mod.addOptions("build_options", exe_options);

    // const zld = b.createModule(.{
    //     .source_file = .{ .path = "zld/src/Zld.zig" },
    //     .dependencies = &[_]std.build.ModuleDependency{.{ .name = "build_options", .module = options_module }},
    // });

    // exe.addModule("Zld", zld);
    // tests.addModule("Zld", zld);

    {
        const test_errors_handled = b.option(bool, "test-errors-handled", "Compile with this to confirm zar sends all io errors through the io error handler") orelse false;
        exe_options.addOption(bool, "test_errors_handled", test_errors_handled);
    }
    {
        // see docs/llvm_ar_wrong_behaviour_cross_compiled.md for more info on this problem
        // helps workaround it for testing purposes for now...
        const mimmick_broken_cross_compiled_llvm_ar_behaviour = b.option(bool, "mimmick-broken-cross-compiled-llvm-ar-behaviour", "Workaround for the fact that cross-compiling zig at the moment result in llvm ar behaving on macOS as if it is running on any other OS") orelse false;
        exe_options.addOption(bool, "mimmick_broken_cross_compiled_llvm_ar_behaviour", mimmick_broken_cross_compiled_llvm_ar_behaviour);
    }

    exe_options.addOption([]const u8, "zig_exe_path", b.graph.zig_exe);

    // Taken from https://github.com/ziglang/zig/blob/master/build.zig, extract
    // the git commit hash to get an actual version.
    const version = v: {
        if (!std.process.can_spawn) {
            std.debug.print("error: version info cannot be retrieved from git. Zig version must be provided using -Dversion-string\n", .{});
            std.process.exit(1);
        }
        const version_string = b.fmt("{d}.{d}.{d}", .{ zar_version.major, zar_version.minor, zar_version.patch });
        var code: u8 = undefined;
        const git_describe_untrimmed = b.runAllowFail(&[_][]const u8{
            "git", "-C", b.build_root.path.?, "describe", "--match", "*.*.*", "--tags",
        }, &code, .Ignore) catch {
            break :v version_string;
        };
        const git_describe = mem.trim(u8, git_describe_untrimmed, " \n\r");

        switch (mem.count(u8, git_describe, "-")) {
            0 => {
                // Tagged release version (e.g. 0.9.0).
                if (!mem.eql(u8, git_describe, version_string)) {
                    std.debug.print("Zig version '{s}' does not match Git tag '{s}'\n", .{ version_string, git_describe });
                    std.process.exit(1);
                }
                break :v version_string;
            },
            2 => {
                // Untagged development build (e.g. 0.9.0-dev.2025+ecf0050a9).
                var it = mem.splitScalar(u8, git_describe, '-');
                const tagged_ancestor = it.first();
                const commit_height = it.next().?;
                const commit_id = it.next().?;

                const ancestor_ver = try std.SemanticVersion.parse(tagged_ancestor);
                if (zar_version.order(ancestor_ver) != .gt) {
                    std.debug.print("Zig version '{}' must be greater than tagged ancestor '{}'\n", .{ zar_version, ancestor_ver });
                    std.process.exit(1);
                }

                // Check that the commit hash is prefixed with a 'g' (a Git convention).
                if (commit_id.len < 1 or commit_id[0] != 'g') {
                    std.debug.print("Unexpected `git describe` output: {s}\n", .{git_describe});
                    break :v version_string;
                }

                // The version is reformatted in accordance with the https://semver.org specification.
                break :v b.fmt("{s}-dev.{s}+{s}", .{ version_string, commit_height, commit_id[1..] });
            },
            else => {
                std.debug.print("Unexpected `git describe` output: {s}\n", .{git_describe});
                break :v version_string;
            },
        }
    };

    exe_options.addOption([]const u8, "version", version);

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
            const tracy_c_flags: []const []const u8 =
                if (target.result.os.tag == .windows and target.result.abi.isGnu())
                    &[_][]const u8{ "-DTRACY_ENABLE=1", "-fno-sanitize=undefined", "-D_WIN32_WINNT=0x601" }
                else
                    &[_][]const u8{ "-DTRACY_ENABLE=1", "-fno-sanitize=undefined" };
            const tracy_lazy_path: std.Build.LazyPath = .{ .src_path = .{ .owner = b, .sub_path = tracy_path } };
            const client_cpp_lazy: std.Build.LazyPath = .{ .src_path = .{ .owner = b, .sub_path = client_cpp } };
            exe_mod.addIncludePath(tracy_lazy_path);
            exe_mod.addCSourceFile(.{ .file = client_cpp_lazy, .flags = tracy_c_flags });
            exe.linkLibC();
            exe.linkLibCpp();

            tests.addIncludePath(tracy_lazy_path);
            tests.addCSourceFile(.{ .file = client_cpp_lazy, .flags = tracy_c_flags });
            tests.linkLibC();
            tests.linkLibCpp();

            if (target.result.os.tag == .windows) {
                exe.linkSystemLibrary("dbghelp");
                exe.linkSystemLibrary("ws2_32");

                tests.linkSystemLibrary("dbghelp");
                tests.linkSystemLibrary("ws2_32");
            } else if (target.result.os.tag.isDarwin()) {
                exe.linkFramework("CoreFoundation");
                tests.linkFramework("CoreFoundation");
            }
        }
    }

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    if (build_test_executable_only) {
        const test_step = b.step("test", "Run tests");
        test_step.dependOn(&tests.step);
        // tests.emit_bin = .{ .emit_to = "zig-out/bin/test" };
    } else {
        const test_step = b.step("test", "Run tests");
        const run_tests = b.addRunArtifact(tests);
        test_step.dependOn(&run_tests.step);
    }
}
