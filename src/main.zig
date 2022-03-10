const std = @import("std");
const builtin = @import("builtin");
const fs = std.fs;
const io = std.io;
const mem = std.mem;
const logger = std.log.scoped(.archive_main);
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
    \\ s - Generate symbol table
    \\ S - Do not generate symbol table
    \\ r - Create sorted symbol table
    \\ R - Do not create sorted symbol table
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

pub const full_logging = builtin.mode == .Debug;
pub const debug_errors = builtin.mode == .Debug;
pub const log_level: std.log.Level = if (full_logging) .debug else .warn;

// For the release standalone program, we just want to display concise errors
// to the end-user, but during development we want them to show up as part of
// the regular logging flow.
pub fn log(
    comptime level: std.log.Level,
    comptime scope: @TypeOf(.EnumLiteral),
    comptime format: []const u8,
    args: anytype,
) void {
    const scope_prefix = "(" ++ @tagName(scope) ++ "): ";

    const prefix = level.asText() ++ scope_prefix;

    std.debug.getStderrMutex().lock();
    defer std.debug.getStderrMutex().unlock();
    const stderr = std.io.getStdErr().writer();
    if (full_logging) {
        nosuspend stderr.print(prefix ++ format ++ "\n", args) catch return;
    } else {
        nosuspend stderr.print(format ++ "\n", args) catch return;
    }
}

// We want to show program overview if invalid argument combination is passed
// through to the program be the user, we do this often enough that it's worth
// having a procedure for it.
fn printArgumentError(comptime errorString: []const u8, args: anytype) void {
    logger.err(overview ++ "\nShowing help text above as error occured:\n" ++ errorString, args);
}

fn checkArgsBounds(args: anytype, index: u32, comptime missing_argument: []const u8) bool {
    if (index >= args.len) {
        printArgumentError("An " ++ missing_argument ++ " must be provided.", .{});
        return false;
    }
    return true;
}

fn openOrCreateFile(cwd: fs.Dir, archive_path: []const u8, print_creation_warning: bool) !fs.File {
    const open_file_handle = cwd.openFile(archive_path, .{ .mode = .read_write }) catch |err| switch (err) {
        error.FileNotFound => {
            if (print_creation_warning) {
                logger.warn("Creating new archive as none exists at path provided\n", .{});
            }
            const create_file_handle = try Archive.handleFileIoError(.creating, archive_path, cwd.createFile(archive_path, .{ .read = true }));
            return create_file_handle;
        },
        else => {
            Archive.printFileIoError(.opening, archive_path, err);
            return err;
        },
    };
    return open_file_handle;
}

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var allocator = arena.allocator();
    const args = process.argsAlloc(allocator) catch |err| if (debug_errors) {
        return err;
    } else {
        logger.err("Unknown error occured.");
    };

    const cwd = fs.cwd();
    archiveMain(cwd, allocator, args) catch |err| {
        handleArchiveError(err) catch |e| if (debug_errors) {
            return e;
        } else {
            logger.err("Unknown error occured.", {});
        };
    };
}

pub fn archiveMain(cwd: fs.Dir, allocator: anytype, args: anytype) anyerror!void {

    // skip the executable name
    const stdout = io.getStdOut().writer();
    const stderr = io.getStdErr().writer();

    var arg_index: u32 = 1;

    var archive_type = Archive.ArchiveType.ambiguous;

    // Process Options First
    var keep_processing_current_option = true;
    while (keep_processing_current_option) {
        if (!checkArgsBounds(args, arg_index, "operation")) {
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
                    archive_type = .darwin;
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
            } else if (args[arg_index].len == 0) {
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

    if (!checkArgsBounds(args, arg_index, "operation")) {
        return;
    }

    const operation_slice = slice: {
        // the operation may start with a hyphen - so slice it!
        var arg_slice = args[arg_index][0..args[arg_index].len];
        if (arg_slice[0] == '-') {
            if (arg_slice.len == 1) {
                printArgumentError("A valid operation must be provided - only hyphen found.", .{});
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
                printArgumentError("'{c}' is not a valid operation.", .{operation_slice[0]});
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
                'r' => modifiers.sort_symbol_table = true,
                'R' => modifiers.sort_symbol_table = false,
                // TODO: should we print warning with unknown modifier?
                else => {},
            }
        }
    }

    arg_index = arg_index + 1;

    if (!checkArgsBounds(args, arg_index, "archive")) {
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
            const file = try openOrCreateFile(cwd, archive_path, !modifiers.create);
            defer file.close();

            var archive = try Archive.create(cwd, file, archive_path, archive_type, modifiers);
            try archive.parse(allocator);
            try archive.insertFiles(allocator, files);
            try archive.finalize(allocator);
        },
        .delete => {
            const file = try openOrCreateFile(cwd, archive_path, !modifiers.create);
            defer file.close();

            var archive = try Archive.create(cwd, file, archive_path, archive_type, modifiers);
            try archive.parse(allocator);
            try archive.deleteFiles(files);
            try archive.finalize(allocator);
        },
        .print_names => {
            const file = try Archive.handleFileIoError(.opening, archive_path, cwd.openFile(archive_path, .{}));
            defer file.close();

            var archive = try Archive.create(cwd, file, archive_path, archive_type, modifiers);
            try archive.parse(allocator);
            for (archive.files.items) |parsed_file| {
                try stdout.print("{s}\n", .{parsed_file.name});
            }
        },
        .print_contents => {
            const file = try Archive.handleFileIoError(.opening, archive_path, cwd.openFile(archive_path, .{}));
            defer file.close();

            var archive = try Archive.create(cwd, file, archive_path, archive_type, modifiers);
            try archive.parse(allocator);
            for (archive.files.items) |parsed_file| {
                try parsed_file.contents.write(stdout, stderr);
            }
        },
        .print_symbols => {
            const file = try Archive.handleFileIoError(.opening, archive_path, cwd.openFile(archive_path, .{}));
            defer file.close();

            var archive = try Archive.create(cwd, file, archive_path, archive_type, modifiers);
            try archive.parse(allocator);
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
        },
        else => {
            logger.err("Operation {} still needs to be implemented!\n", .{operation});
            return error.TODO;
        },
    }
}

// TODO: systemically work through all errors, put them in an Archive error
// set so that we know they print appropriate error messages, make this NOT
// return any error type, and know we have a robust main program alongside
// a usable API that returns a well-defined set of errors.
fn handleArchiveError(err: anyerror) !void {
    {
        // we can ignore these errors because we log context specific
        // information about them at the time that they are thrown.
        const fields = comptime std.meta.fields(Archive.HandledError);
        inline for (fields) |field| {
            if (@field(Archive.HandledError, field.name) == err) {
                return;
            }
        }
    }

    switch (err) {
        // These are errors which already have appropraite log messages printed
        error.NotArchive => logger.err("Provided file is not an archive.", .{}),
        error.MalformedArchive, error.Overflow, error.InvalidCharacter => logger.err("Malformed archive provided.", .{}),
        error.OutOfMemory => logger.err("Program ran out of memory.", .{}),

        // TODO: ignore runtime errors as they aren't needed.
        // we bubble-up other errors as they are currently unhandled
        // TODO: handle these (either at top-level or in parsing method).
        // or have a bug reporting system (or make them not possible by explicitly
        // covering the union of all errors.
        else => return err,
    }
}
