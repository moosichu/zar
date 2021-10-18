const std = @import("std");
const builtin = @import("builtin");
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
    \\ --format=<type>
    \\      Can be default, gnu, darwin or bsd. This determines the format used to serialise an archive, this is ignored when parsing archives as type there is always inferred. When creating an archive the host machine is used to infer <type> if one is not specified.
    \\ --version
    \\      Print program version details and exit.
    \\ -h, --help
    \\      Print (this) help text and exit.
    \\
    \\Ignored for compatability:
    \\ --plugin=<string>
    \\
    \\Operations:
    \\ r - replace/insert [files] in <archive>, create archive if it does not exist.
    \\ d - delete [files] from <archive>.
    \\ m - move [files] in <archive>.
    \\ p - print contents of files in <archive>.
    \\ q - quick append [files] to <archive>.
    \\ s - act as ranlib.
    \\ t - display filenames in <archive>.
    \\ x - extract [files] from <archive>.
    \\ S - show symbols in the <archive>.
    \\
    \\Modifiers:
    \\ c - Disable archive creation warning if inserting files to new archive.
    \\ u - Only update archive contents if [files] have more recent timestamps than it.
    \\ D - Use zero for timestamps, GIDs and UIDs in archived files (enabled by default).
    \\ U - Use real timestamps, GIDS and UIDs for archived files.
    \\ v - Print verbose output, depending on opertion:
    \\     S: show file names that symbols belong to.
    \\
    \\Note, in the case of conflicting modifiers, the last one listed always takes precedence.
    \\
;

const version = "0.0.0";

const version_details =
    \\zar (https://github.com/moosichu/zar):
    \\  zar version {s}
    \\  {s} build
    \\  default archive type: {s}
    \\  host: {s}-{s}-{s}
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

fn openOrCreateFile(archive_path: []u8, stderr: fs.File.Writer, print_creation_warning: bool) !fs.File {
    const open_file_handle = fs.cwd().openFile(archive_path, .{ .write = true }) catch |err| switch (err) {
        error.FileNotFound => {
            if (print_creation_warning) {
                try stderr.print("Warning: creating new archive as none exists at path provided\n", .{});
            }
            const create_file_handle = try fs.cwd().createFile(archive_path, .{ .read = true });
            return create_file_handle;
        },
        else => return err,
    };
    return open_file_handle;
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

    var archive_type = Archive.ArchiveType.ambiguous;

    // Process Options First
    var keep_processing_current_option = true;
    while (keep_processing_current_option) {
        if (!try checkArgsBounds(stderr, args, arg_index)) {
            return;
        }

        keep_processing_current_option = false;
        var current_arg = args[arg_index];
        {
            // TODO: Make sure an arg doesn't show up twice!
            const format_string_prefix = "--format=";
            const plugin_string_prefix = "--plugin=";
            const help_string = "--help";
            const help_shortcut = "-h";
            const version_string = "--version";
            if (mem.startsWith(u8, current_arg, format_string_prefix)) {
                // TODO: Handle format option!
                keep_processing_current_option = true;

                const format_string = current_arg[format_string_prefix.len..];
                if (mem.eql(u8, format_string, "default")) {
                    // do nothing
                } else if (mem.eql(u8, format_string, "bsd")) {
                    archive_type = .bsd;
                } else if (mem.eql(u8, format_string, "darwin")) {
                    archive_type = .bsd;
                } else if (mem.eql(u8, format_string, "gnu")) {
                    archive_type = .gnu;
                } else {
                    // TODO: do an actual error here!
                    return error.TODO;
                }
                arg_index = arg_index + 1;
                continue;
            } else if (mem.startsWith(u8, current_arg, plugin_string_prefix)) {
                keep_processing_current_option = true;
                arg_index = arg_index + 1;
                continue;
            } else if (mem.eql(u8, current_arg, help_string) or mem.eql(u8, current_arg, help_shortcut)) {
                try stdout.print(overview, .{});
                return;
            } else if (mem.eql(u8, current_arg, version_string)) {
                // TODO: calculate build, archive type & host!
                const target = builtin.target;
                const default_archive_type = @tagName(Archive.getDefaultArchiveTypeFromHost());
                try stdout.print(version_details, .{ version, @tagName(builtin.mode), default_archive_type, @tagName(target.cpu.arch), @tagName(target.os.tag), @tagName(target.abi) });
                return;
            }
        }
    }

    if (!try checkArgsBounds(stderr, args, arg_index)) {
        return;
    }

    const operation_slice = slice: {
        // the operation may start with a hyphen - so slice it!
        var arg_slice = args[arg_index][0..args[arg_index].len];
        if (arg_slice[0] == '-') {
            if (arg_slice.len == 1) {
                try printError(stderr, "a valid operation must be provided - only hyphen found");
                return;
            }

            arg_slice = arg_slice[1..arg_slice.len];
        }

        break :slice arg_slice;
    };

    // Process Operation
    const operation = operation: {
        switch (operation_slice[0]) {
            'r' => break :operation Archive.Operation.insert,
            'd' => break :operation Archive.Operation.delete,
            'm' => break :operation Archive.Operation.move,
            'p' => break :operation Archive.Operation.print_contents,
            'w' => break :operation Archive.Operation.quick_append,
            's' => break :operation Archive.Operation.ranlib,
            't' => break :operation Archive.Operation.print_names,
            'x' => break :operation Archive.Operation.extract,
            'S' => break :operation Archive.Operation.print_symbols,
            else => {
                try printError(stderr, "a valid operation must be provided");
                return;
            },
        }
    };

    var modifiers: Archive.Modifiers = .{};
    if (operation_slice.len > 1) {
        const modifier_slice = operation_slice[1..];
        for (modifier_slice) |modifier_char| {
            switch (modifier_char) {
                'c' => modifiers.create = true,
                'u' => modifiers.update_only = true,
                'U' => modifiers.use_real_timestamps_and_ids = true,
                'D' => modifiers.use_real_timestamps_and_ids = false,
                'v' => modifiers.verbose = true,
                's' => modifiers.build_symbol_table = true,
                'S' => modifiers.build_symbol_table = false,
                // TODO: should we print warning with unknown modifier?
                else => {},
            }
        }
    }

    arg_index = arg_index + 1;

    if (!try checkArgsBounds(stderr, args, arg_index)) {
        return;
    }

    // TODO: Process [relpos]

    // TODO: Process [count]

    const archive_path = args[arg_index];

    arg_index = arg_index + 1;

    const files = file_result: {
        if (args.len > arg_index) {
            break :file_result args[arg_index..args.len];
        }
        const empty = [_][:0]u8{};
        break :file_result &empty;
    };

    switch (operation) {
        .insert => {
            const file = try openOrCreateFile(archive_path, stderr, !modifiers.create);
            defer file.close();

            var archive = try Archive.create(file, archive_path, archive_type, modifiers);
            if (archive.parse(allocator)) {
                try archive.insertFiles(allocator, files);
                try archive.finalize(allocator);
            } else |err| try printParseError(archive, err, stderr);
        },
        .delete => {
            const file = try openOrCreateFile(archive_path, stderr, !modifiers.create);
            defer file.close();

            var archive = try Archive.create(file, archive_path, archive_type, modifiers);
            if (archive.parse(allocator)) {
                try archive.deleteFiles(files);
                try archive.finalize(allocator);
            } else |err| try printParseError(archive, err, stderr);
        },
        .print_names => {
            const file = try fs.cwd().openFile(archive_path, .{});
            defer file.close();

            var archive = try Archive.create(file, archive_path, archive_type, modifiers);
            if (archive.parse(allocator)) {
                for (archive.files.items) |parsed_file| {
                    try stdout.print("{s}\n", .{parsed_file.name});
                }
            } else |err| try printParseError(archive, err, stderr);
        },
        .print_contents => {
            const file = try fs.cwd().openFile(archive_path, .{});
            defer file.close();

            var archive = try Archive.create(file, archive_path, archive_type, modifiers);
            if (archive.parse(allocator)) {
                for (archive.files.items) |parsed_file| {
                    try parsed_file.contents.write(stdout, stderr);
                }
            } else |err| try printParseError(archive, err, stderr);
        },
        .print_symbols => {
            const file = try fs.cwd().openFile(archive_path, .{});
            defer file.close();

            var archive = try Archive.create(file, archive_path, archive_type, modifiers);
            if (archive.parse(allocator)) {
                for (archive.symbols.items) |symbol| {
                    if (modifiers.verbose) {
                        if (symbol.file_index == Archive.invalid_file_index) {
                            try stdout.print("?: {s}\n", .{symbol.name});
                        } else {
                            try stdout.print("{s}: {s}\n", .{ archive.files.items[symbol.file_index].name, symbol.name });
                        }
                    } else {
                        try stdout.print("{s}\n", .{symbol.name});
                    }
                }
            } else |err| try printParseError(archive, err, stderr);
        },
        else => {
            std.debug.warn("Operation {} still needs to be implemented!\n", .{operation});
            return error.TODO;
        },
    }
}

fn printParseError(archive: Archive, err: anytype, stderr: anytype) !void {
    switch (err) {
        // These are errors we know how to handle
        Archive.ParseError.NotArchive, Archive.ParseError.MalformedArchive => {
            // archive.parse prints appropriate errors for these messages
            try stderr.print("{s}\n", .{archive.error_string});
            return;
        },
        else => return err,
    }
}
