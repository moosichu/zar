//! ArchiveStep is primarily used to create a directory in an appropriate
//! location inside the local cache which has a set of files that have either
//! been generated during the build, or are copied from the source package.
//!
//! However, this step has an additional capability of writing data to paths
//! relative to the package root, effectively mutating the package's source
//! files. Be careful with the latter functionality; it should not be used
//! during the normal build process, but as a utility run by a developer with
//! intention to update source files, which will then be committed to version
//! control.
const std = @import("std");
const Step = std.Build.Step;
const Compile = std.Build.Step.Compile;
const fs = std.fs;
const ArrayList = std.ArrayList;
const ArchiveStep = @This();
const Archive = @import("../Archive.zig");

const assert = std.debug.assert;

step: Step,

generated_file: std.Build.GeneratedFile,

pub const base_id = .custom;

pub const Options = struct {
    name: []const u8,
};

pub fn create(owner: *std.Build, options: Options) *ArchiveStep {
    _ = options;
    const archive = owner.allocator.create(ArchiveStep) catch @panic("OOM");
    archive.* = .{
        .step = Step.init(.{
            .id = base_id,
            .name = "Archive",
            .owner = owner,
            .makeFn = make,
        }),
        .generated_file = .{ .step = &archive.step },
    };
    return archive;
}

pub fn addLibrary(self: *ArchiveStep, lib: *Compile) void {
    _ = self;
    assert(lib.kind == .lib);
}

pub fn getFile(self: *ArchiveStep) std.Build.LazyPath {
    return .{ .generated = &self.generated_file };
}

fn make(step: *Step, prog_node: *std.Progress.Node) !void {
    _ = prog_node;
    const b = step.owner;
    const archive = @fieldParentPtr(ArchiveStep, "step", step);
    _ = archive;

    // The cache is used here not really as a way to speed things up - because writing
    // the data to a file would probably be very fast - but as a way to find a canonical
    // location to put build artifacts.

    // If, for example, a hard-coded path was used as the location to put ArchiveStep
    // files, then two ArchiveSteps executing in parallel might clobber each other.

    var man = b.cache.obtain();
    defer man.deinit();

    // Random bytes to make ArchiveStep unique. Refresh this with
    // new random bytes when ArchiveStep implementation is modified
    // in a non-backwards-compatible way.
    man.hash.add(@as(u32, 0xebab9753));

    // for (archive.files.items) |file| {
    //     man.hash.addBytes(file.sub_path);
    //     switch (file.contents) {
    //         .bytes => |bytes| {
    //             man.hash.addBytes(bytes);
    //         },
    //         .copy => |file_source| {
    //             _ = try man.addFile(file_source.getPath(b), null);
    //         },
    //     }
    // }

    // if (try step.cacheHit(&man)) {
    //     const digest = man.final();
    //     for (archive.files.items) |file| {
    //         file.generated_file.path = try b.cache_root.join(b.allocator, &.{
    //             "o", &digest, file.sub_path,
    //         });
    //     }
    //     archive.generated_file.path = try b.cache_root.join(b.allocator, &.{ "o", &digest });
    //     return;
    // }

    // const digest = man.final();
    // const cache_path = "o" ++ fs.path.sep_str ++ digest;

    // archive.generated_file.path = try b.cache_root.join(b.allocator, &.{ "o", &digest });

    // var cache_dir = b.cache_root.handle.makeOpenPath(cache_path, .{}) catch |err| {
    //     return step.fail("unable to make path '{}{s}': {s}", .{
    //         b.cache_root, cache_path, @errorName(err),
    //     });
    // };
    // defer cache_dir.close();

    // for (archive.files.items) |file| {
    //     if (fs.path.dirname(file.sub_path)) |dirname| {
    //         cache_dir.makePath(dirname) catch |err| {
    //             return step.fail("unable to make path '{}{s}{c}{s}': {s}", .{
    //                 b.cache_root, cache_path, fs.path.sep, dirname, @errorName(err),
    //             });
    //         };
    //     }
    //     switch (file.contents) {
    //         .bytes => |bytes| {
    //             cache_dir.writeFile(file.sub_path, bytes) catch |err| {
    //                 return step.fail("unable to write file '{}{s}{c}{s}': {s}", .{
    //                     b.cache_root, cache_path, fs.path.sep, file.sub_path, @errorName(err),
    //                 });
    //             };
    //         },
    //         .copy => |file_source| {
    //             const source_path = file_source.getPath(b);
    //             const prev_status = fs.Dir.updateFile(
    //                 fs.cwd(),
    //                 source_path,
    //                 cache_dir,
    //                 file.sub_path,
    //                 .{},
    //             ) catch |err| {
    //                 return step.fail("unable to update file from '{s}' to '{}{s}{c}{s}': {s}", .{
    //                     source_path,
    //                     b.cache_root,
    //                     cache_path,
    //                     fs.path.sep,
    //                     file.sub_path,
    //                     @errorName(err),
    //                 });
    //             };
    //             // At this point we already will mark the step as a cache miss.
    //             // But this is kind of a partial cache hit since individual
    //             // file copies may be avoided. Oh well, this information is
    //             // discarded.
    //             _ = prev_status;
    //         },
    //     }

    //     file.generated_file.path = try b.cache_root.join(b.allocator, &.{
    //         cache_path, file.sub_path,
    //     });
    // }

    try step.writeManifest(&man);
}
