const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;
const process = std.process;

const Archive = @import("archive/Archive.zig");

const overview =
    \\Zig Archiver
    \\
    \\Usage: zar [options] [-]<operation>[modifiers] [relpos] [count] <archive> [files]
    \\
    \\Options:
    \\ TODO!
    \\
    \\Operations:
    \\ r - replace/insert [files] in <archive> (NOTE: c modifier allows for archive creation)
    \\ d - delete [files] from <archive>
    \\ m - move [files] in <archive>
    \\ p - print [files] in <archive>
    \\ q - quick append [files] to <archive>
    \\ s - act as ranlib
    \\ t - display contents of <archive>
    \\ x - extract [files] from <archive>
    \\
    \\Modifiers:
    \\ TODO!
    \\
;

fn printError(stderr: anytype, comptime errorString: []const u8) !void {
    try stderr.print("error: " ++ errorString ++ "\n", .{});
    try stderr.print(overview, .{});
}

fn checkArgsBounds(stderr: anytype, args: anytype, index: u32) !bool {
    if (index >= args.len) {
        try printError(stderr, "an archive must be specified");
        return false;
    }
    return true;
}

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var allocator = &arena.allocator;
    const args = try process.argsAlloc(allocator);

    // skip the executable name
    const stdout = io.getStdOut().writer();
    const stderr = io.getStdErr().writer();

    var arg_index: u32 = 1;
    if (!try checkArgsBounds(stderr, args, arg_index)) {
        return;
    }

    // Process Options First
    var keep_processing_current_option = true;
    while (keep_processing_current_option) {
        keep_processing_current_option = false;
        var current_arg = args[arg_index];
        {
            const format_string = "--format=";
            if (mem.startsWith(u8, current_arg, format_string)) {
                // TODO: Handle format option!
                keep_processing_current_option = true;
                arg_index = arg_index + 1;
                continue;
            }
        }
    }

    if (!try checkArgsBounds(stderr, args, arg_index)) {
        return;
    }

    // Process Operation!
    const operation = operation: {
        // the operation may start with a hyphen - so slice it!
        var arg_slice = args[arg_index][0..args[arg_index].len];
        if (arg_slice[0] == '-') {
            arg_slice = arg_slice[1..arg_slice.len];
        }
        switch (arg_slice[0]) {
            'r' => break :operation Archive.Operation.insert,
            'd' => break :operation Archive.Operation.delete,
            'm' => break :operation Archive.Operation.move,
            'p' => break :operation Archive.Operation.print,
            'w' => break :operation Archive.Operation.quick_append,
            's' => break :operation Archive.Operation.ranlib,
            't' => break :operation Archive.Operation.display_contents,
            'x' => break :operation Archive.Operation.extract,
            else => {
                try printError(stderr, "a valid operation must be provided");
                return;
            },
        }

        // TODO: Process modifiers!
    };

    arg_index = arg_index + 1;

    if (!try checkArgsBounds(stderr, args, arg_index)) {
        return;
    }

    // TODO: Process [relpos]

    // TODO: Process [count]

    const archive_path = args[arg_index];

    switch (operation) {
        .insert => {
            const file = try fs.cwd().createFile(archive_path, .{});
            defer file.close();

            var archive = Archive.create(file, archive_path);
            try archive.addFiles(allocator, args[arg_index + 1 ..]);
            try archive.finalize();
        },
        .delete => {
            const file = try fs.cwd().openFile(archive_path, .{ .write = true });
            defer file.close();

            var archive = Archive.create(file, archive_path);
            if (archive.parse(allocator, stderr)) {
                try archive.deleteFiles(args[arg_index + 1 ..]);
                try archive.finalize();
            } else |err| return err;
        },
        .display_contents => {
            const file = try fs.cwd().openFile(archive_path, .{});
            defer file.close();

            var archive = Archive.create(file, archive_path);
            if (archive.parse(allocator, stderr)) {
                for (archive.files.items) |parsed_file| {
                    try stdout.print("{s}\n", .{parsed_file.name});
                }
            } else |err| switch (err) {
                // These are errors we know how to handle
                error.NotArchive => {
                    // archive.parse prints appropriate errors for these messages
                    return;
                },
                else => return err,
            }
        },
        .print => {
            const file = try fs.cwd().openFile(archive_path, .{});
            defer file.close();

            var archive = Archive.create(file, archive_path);
            if (archive.parse(allocator, stderr)) {
                for (archive.files.items) |parsed_file| {
                    try parsed_file.contents.write(stdout, stderr);
                }
            } else |err| switch (err) {
                // These are errors we know how to handle
                error.NotArchive => {
                    // archive.parse prints appropriate errors for these messages
                    return;
                },
                else => return err,
            }
        },
        else => {
            std.debug.warn("Operation {} still needs to be implemented!\n", .{operation});
            return error.TODO;
        },
    }
}
