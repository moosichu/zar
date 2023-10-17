//! Here we test our archiver for correctness and functionality.
//! Currently, we support archiving on Linux and MacOS, but in the future we
//! will progressively relax those to exercise more combinations.

// Based on https://github.com/ziglang/zig/blob/master/test/link/elf.zig

const ArchiveStep = @import("../src/archive/step/ArchiveStep.zig");

pub fn build(b: *Build) void {
    const archiver_step = b.step("test-archiver", "Run archiver tests");
    b.default_step = archiver_step;

    const musl_target = CrossTarget{
        .cpu_arch = .x86_64, // TODO relax this once archiver linker is able to handle other archs
        .os_tag = .linux,
        .abi = .musl,
    };
    const glibc_target = CrossTarget{
        .cpu_arch = .x86_64,
        .os_tag = .linux,
        .abi = .gnu,
    };
    _ = glibc_target;

    // Exercise archiver
    archiver_step.dependOn(testArchiveBasic(b, .{ .target = musl_target }));
}

fn testArchiveBasic(b: *Build, opts: Options) *Step {
    const test_step = addTestStep(b, "archive-basic", opts);

    const a_lib = addStaticLibrary(b, "a", opts);
    addCSourceBytes(a_lib,
        \\extern int a() { return 1; }
    , &.{});
    a_lib.linkLibC();

    const b_lib = addStaticLibrary(b, "b", opts);
    addCSourceBytes(b_lib,
        \\extern int b() { return 1; }
    , &.{});
    b_lib.linkLibC();

    const archive = addArchive(b, "lib", opts);
    archive.addLibrary(a_lib);
    archive.addLibrary(b_lib);

    const exe = addExecutable(b, "main", opts);
    addCSourceBytes(exe,
        \\int main() { a(); b(); }
    , &.{});
    exe.addObjectFile(archive.getFile());
    exe.linkLibC();

    test_step.dependOn(&exe.step);

    return test_step;
}

const Options = struct {
    target: CrossTarget = .{ .cpu_arch = .x86_64, .os_tag = .linux },
    optimize: std.builtin.OptimizeMode = .Debug,
    use_llvm: bool = true,
};

fn addTestStep(b: *Build, comptime prefix: []const u8, opts: Options) *Step {
    const target = opts.target.zigTriple(b.allocator) catch @panic("OOM");
    const optimize = @tagName(opts.optimize);
    const use_llvm = if (opts.use_llvm) "llvm" else "no-llvm";
    const name = std.fmt.allocPrint(b.allocator, "test-archiver-" ++ prefix ++ "-{s}-{s}-{s}", .{
        target,
        optimize,
        use_llvm,
    }) catch @panic("OOM");
    return b.step(name, "");
}

fn addArchive(b: *Build, name: []const u8, opts: Options) *ArchiveStep {
    _ = opts;
    return ArchiveStep.create(b, .{
        .name = name,
    });
}

fn addExecutable(b: *Build, name: []const u8, opts: Options) *Compile {
    return b.addExecutable(.{
        .name = name,
        .target = opts.target,
        .optimize = opts.optimize,
        .use_llvm = opts.use_llvm,
        .use_lld = false,
    });
}

fn addObject(b: *Build, name: []const u8, opts: Options) *Compile {
    return b.addObject(.{
        .name = name,
        .target = opts.target,
        .optimize = opts.optimize,
        .use_llvm = opts.use_llvm,
        .use_lld = false,
    });
}

fn addStaticLibrary(b: *Build, name: []const u8, opts: Options) *Compile {
    return b.addStaticLibrary(.{
        .name = name,
        .target = opts.target,
        .optimize = opts.optimize,
        .use_llvm = opts.use_llvm,
        .use_lld = true,
    });
}

fn addSharedLibrary(b: *Build, name: []const u8, opts: Options) *Compile {
    return b.addSharedLibrary(.{
        .name = name,
        .target = opts.target,
        .optimize = opts.optimize,
        .use_llvm = opts.use_llvm,
        .use_lld = false,
    });
}

fn addRunArtifact(comp: *Compile) *Run {
    const b = comp.step.owner;
    const run = b.addRunArtifact(comp);
    run.skip_foreign_checks = true;
    return run;
}

fn addZigSourceBytes(comp: *Compile, bytes: []const u8) void {
    const b = comp.step.owner;
    const file = WriteFile.create(b).add("a.zig", bytes);
    file.addStepDependencies(&comp.step);
    comp.root_src = file;
}

fn addCSourceBytes(comp: *Compile, bytes: []const u8, flags: []const []const u8) void {
    const b = comp.step.owner;
    const file = WriteFile.create(b).add("a.c", bytes);
    comp.addCSourceFile(.{ .file = file, .flags = flags });
}

fn addCppSourceBytes(comp: *Compile, bytes: []const u8, flags: []const []const u8) void {
    const b = comp.step.owner;
    const file = WriteFile.create(b).add("a.cpp", bytes);
    comp.addCSourceFile(.{ .file = file, .flags = flags });
}

fn addAsmSourceBytes(comp: *Compile, bytes: []const u8) void {
    const b = comp.step.owner;
    const actual_bytes = std.fmt.allocPrint(b.allocator, "{s}\n", .{bytes}) catch @panic("OOM");
    const file = WriteFile.create(b).add("a.s", actual_bytes);
    comp.addAssemblyFile(file);
}

const std = @import("std");

const Build = std.Build;
const Compile = Step.Compile;
const CrossTarget = std.zig.CrossTarget;
const LazyPath = Build.LazyPath;
const Run = Step.Run;
const Step = Build.Step;
const WriteFile = Step.WriteFile;
