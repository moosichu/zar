const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const trace = @import("tracy.zig").trace;
const fs = std.fs;
const io = std.io;
const mem = std.mem;
const logger = std.log.scoped(.archive_main);
const process = std.process;

const Archive = @import("archive/Archive.zig");

pub const zar_overview =
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
    \\ V - Display the version and exit
    \\
    \\Note, in the case of conflicting modifiers, the last one listed always takes precedence.
    \\
;

pub const ranlib_overview =
    \\Zig Ranlib
    \\
    \\Usage: zar ranlib [options] -[modifiers] <archive>
    \\
    \\Options:
    \\ -v, --version
    \\      Print program version details and exit.
    \\ -h, --help
    \\      Print (this) help text and exit.
    \\
    \\Modifiers:
    \\ D - Use zero for timestamps, GIDs and UIDs in archived files (enabled by default).
    \\ U - Use real timestamps, GIDS and UIDs for archived files.
    \\
    \\Note, in the case of conflicting modifiers, the last one listed always takes precedence.
    \\
;

pub const zar_error_prefix = zar_overview ++ "\n\x1B[1;31merror\x1B[0m: ";
pub const ranlib_error_prefix = ranlib_overview ++ "\n\x1B[1;31merror\x1B[0m: ";

const version = build_options.version;

const version_details =
    \\zar {s} (https://github.com/moosichu/zar):
    \\  zar version {s}
    \\  {s} build
    \\  default archive type: {s}
    \\  host: {s}-{s}-{s}
    \\
;

pub const full_logging = builtin.mode == .Debug;
pub const debug_errors = builtin.mode == .Debug;
pub const log_level: std.log.Level = if (full_logging) .debug else .warn;

pub const Mode = enum { ar, ranlib };

fn printHelp(stdout: fs.File.Writer, mode: Mode) !void {
    switch (mode) {
        .ar => try stdout.print(zar_overview, .{}),
        .ranlib => try stdout.print(ranlib_overview, .{}),
    }
}

fn printVersion(stdout: fs.File.Writer, mode: Mode) !void {
    // TODO: calculate build, archive type & host!
    const target = builtin.target;
    const default_archive_type = @tagName(Archive.getDefaultArchiveTypeFromHost());
    try stdout.print(version_details, .{ @tagName(mode), version, @tagName(builtin.mode), default_archive_type, @tagName(target.cpu.arch), @tagName(target.os.tag), @tagName(target.abi) });
}

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

    const prefix = comptime level.asText() ++ scope_prefix;

    std.debug.getStderrMutex().lock();
    defer std.debug.getStderrMutex().unlock();
    const stderr = std.io.getStdErr().writer();
    if (full_logging) {
        nosuspend stderr.print(prefix ++ format ++ "\n", args) catch return;
    } else {
        nosuspend stderr.print(format ++ "\n", args) catch return;
    }
}

// We want to show program zar_overview if invalid argument combination is passed
// through to the program be the user, we do this often enough that it's worth
// having a procedure for it.
fn printArgumentError(comptime errorString: []const u8, args: anytype, mode: Mode) void {
    if (mode == .ranlib) {
        logger.err(ranlib_error_prefix ++ errorString, args);
    } else {
        logger.err(zar_error_prefix ++ errorString, args);
    }
}

fn checkArgsBounds(args: []const []const u8, index: u32, comptime missing_argument: []const u8, mode: Mode) bool {
    if (index >= args.len) {
        printArgumentError("An " ++ missing_argument ++ " must be provided.", .{}, mode);
        return false;
    }
    return true;
}

fn openOrCreateFile(cwd: fs.Dir, archive_path: []const u8, print_creation_warning: bool, created: *bool) !fs.File {
    created.* = false;
    const open_file_handle = cwd.openFile(archive_path, .{ .mode = .read_write }) catch |err| switch (err) {
        error.FileNotFound => {
            created.* = true;
            if (print_creation_warning) {
                logger.warn("Creating new archive as none exists at path provided\n", .{});
            }
            const create_file_handle = try Archive.handleFileIoError(.creating, archive_path, cwd.createFile(archive_path, .{ .read = true }));
            return create_file_handle;
        },
        else => {
            return Archive.printFileIoError(.opening, archive_path, err);
        },
    };
    return open_file_handle;
}

fn processModifier(modifier_char: u8, mode: Mode, modifiers: *Archive.Modifiers) bool {
    switch (mode) {
        .ar => switch (modifier_char) {
            'c' => modifiers.create = true,
            'u' => modifiers.update_only = true,
            'U' => modifiers.use_real_timestamps_and_ids = true,
            'D' => modifiers.use_real_timestamps_and_ids = false,
            'v' => modifiers.verbose = true,
            's' => modifiers.build_symbol_table = true,
            'S' => modifiers.build_symbol_table = false,
            'r' => modifiers.sort_symbol_table = .set_true,
            'R' => modifiers.sort_symbol_table = .set_false,
            'a' => modifiers.move_setting = .before,
            'b', 'i' => modifiers.move_setting = .after,
            'V' => modifiers.show_version = true,
            'h' => modifiers.help = true,
            // TODO: handle other modifiers!
            else => {
                printArgumentError("'{c}' is not a valid modifier.", .{modifier_char}, mode);
                return false;
            },
        },
        .ranlib => switch (modifier_char) {
            'U' => modifiers.use_real_timestamps_and_ids = true,
            'D' => modifiers.use_real_timestamps_and_ids = false,
            // ranlib will always priorities which one of these modifiers comes first
            // this is in contrast to ar which always shows help over version if it's present
            // matching this specific behaviour may be overkill, but would rather
            // aggressively match llvm on this front as much as possible by default
            // TODO: write tests for these cases of ordering modifiers
            'v' => if (!modifiers.help) {
                modifiers.show_version = true;
            },
            'h' => if (!modifiers.show_version) {
                modifiers.help = true;
            },
            else => {
                printArgumentError("'{c}' is not a valid option.", .{modifier_char}, mode);
                return false;
            },
        },
    }
    return true;
}

pub fn main() anyerror!void {
    const tracy = trace(@src());
    defer tracy.end();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var allocator = arena.allocator();
    const args = process.argsAlloc(allocator) catch |err| if (debug_errors) {
        return err;
    } else {
        logger.err("Unknown error occured.", .{});
        return;
    };

    const cwd = fs.cwd();
    archiveMain(cwd, allocator, args) catch |err| {
        handleArchiveError(err) catch |e| if (debug_errors) {
            return e;
        } else {
            logger.err("Unknown error occured.", .{});
        };
    };
}

pub fn archiveMain(cwd: fs.Dir, allocator: anytype, args: []const []const u8) anyerror!void {
    // const tracy_zone = ztracy.zoneNC(@src(), "ArchiveMain", 0x00_ff_00_00, 1);
    // defer tracy_zone.end();

    // skip the executable name
    const stdout = io.getStdOut().writer();
    const stderr = io.getStdErr().writer();

    var arg_index: u32 = 1;

    var archive_type = Archive.ArchiveType.ambiguous;

    // Check if we are in ranlib mode!
    const mode: Mode = mode: {
        if (arg_index < args.len) {
            if (mem.eql(u8, "ranlib", args[arg_index])) {
                arg_index = arg_index + 1;
                break :mode .ranlib;
            }
        }
        break :mode .ar;
    };

    // Process Options First
    // TODO: based on observed behaviour of ranlib and ar, this order isn't actually
    // fixed. Need to write some tests to fuzz these and then match llvm's behaviour
    // in these cases.
    var keep_processing_current_option = true;
    while (keep_processing_current_option) {
        if (!checkArgsBounds(args, arg_index, "operation", mode)) {
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
            if (mode == .ar and mem.startsWith(u8, current_arg, format_string_prefix)) {
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
            } else if (mode == .ar and mem.startsWith(u8, current_arg, plugin_string_prefix)) {
                keep_processing_current_option = true;
                arg_index = arg_index + 1;
                continue;
            } else if (args[arg_index].len == 0) {
                keep_processing_current_option = true;
                arg_index = arg_index + 1;
                continue;
            } else if (mem.eql(u8, current_arg, help_string) or mem.eql(u8, current_arg, help_shortcut)) {
                try printHelp(stdout, mode);
                return;
            } else if (mem.eql(u8, current_arg, version_string)) {
                try printVersion(stdout, mode);
                return;
            }
        }
    }

    var modifier_slice: []const u8 = "";
    const operation = operation: {
        if (mode == .ranlib) {
            if (arg_index < args.len) {
                var arg_slice = args[arg_index][0..];
                if (arg_slice[0] == '-') {
                    if (arg_slice.len == 1) {
                        printArgumentError("A valid modifier must be provided - only hyphen found.", .{}, mode);
                        return;
                    }

                    modifier_slice = arg_slice[1..];
                }
            }
            break :operation Archive.Operation.ranlib;
        } else {
            if (!checkArgsBounds(args, arg_index, "operation", mode)) {
                return;
            }
            const operation_slice = slice: {
                // the operation may start with a hyphen - so slice it!
                var arg_slice = args[arg_index][0..];
                if (arg_slice[0] == '-') {
                    if (arg_slice.len == 1) {
                        printArgumentError("A valid operation must be provided - only hyphen found.", .{}, mode);
                        return;
                    }

                    arg_slice = arg_slice[1..arg_slice.len];
                }

                break :slice arg_slice;
            };

            modifier_slice = operation_slice[1..];

            // Process Operation
            switch (operation_slice[0]) {
                'r' => break :operation Archive.Operation.insert,
                'd' => break :operation Archive.Operation.delete,
                'm' => break :operation Archive.Operation.move,
                'p' => break :operation Archive.Operation.print_contents,
                'q' => break :operation Archive.Operation.quick_append,
                's' => break :operation Archive.Operation.ranlib,
                't' => break :operation Archive.Operation.print_names,
                'x' => break :operation Archive.Operation.extract,
                'S' => break :operation Archive.Operation.print_symbols,
                else => {
                    printArgumentError("'{c}' is not a valid operation.", .{operation_slice[0]}, mode);
                    return;
                },
            }
        }
    };

    var modifiers: Archive.Modifiers = .{};
    while (true) {
        if (modifier_slice.len == 0) {
            // operation argument didn't have any modifiers,
            // so incremement arg index if we are in ar land
            if (mode == .ar) {
                arg_index = arg_index + 1;
            }
            break;
        }
        for (modifier_slice) |modifier_char| {
            if (!processModifier(modifier_char, mode, &modifiers)) {
                return;
            }
        }

        arg_index = arg_index + 1;

        // check if we still have some modifiers!
        if (arg_index >= args.len) break;
        var arg_slice = args[arg_index][0..];
        if (arg_slice[0] != '-') break;
        if (arg_slice.len == 1) {
            printArgumentError("A valid modifer must be provided - only hyphen found.", .{}, mode);
            return;
        }

        modifier_slice = arg_slice[1..arg_slice.len];
    }

    if (modifiers.help) {
        try printHelp(stdout, mode);
        return;
    }

    if (modifiers.show_version) {
        try printVersion(stdout, mode);
        return;
    }

    if (!checkArgsBounds(args, arg_index, "archive", mode)) {
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
            var created = false;
            const file = try openOrCreateFile(cwd, archive_path, !modifiers.create, &created);
            defer file.close();

            var archive = try Archive.create(cwd, file, archive_path, archive_type, modifiers, created);
            try archive.parse(allocator);
            try archive.insertFiles(allocator, files);
            try archive.finalize(allocator);
        },
        .delete => {
            var created = false;
            const file = try openOrCreateFile(cwd, archive_path, !modifiers.create, &created);
            defer file.close();

            var archive = try Archive.create(cwd, file, archive_path, archive_type, modifiers, created);
            try archive.parse(allocator);
            try archive.deleteFiles(files);
            try archive.finalize(allocator);
        },
        .print_names => {
            const file = try Archive.handleFileIoError(.opening, archive_path, cwd.openFile(archive_path, .{}));
            defer file.close();

            var archive = try Archive.create(cwd, file, archive_path, archive_type, modifiers, false);
            try archive.parse(allocator);
            for (archive.files.items) |parsed_file| {
                try stdout.print("{s}\n", .{parsed_file.name});
            }
        },
        .print_contents => {
            const file = try Archive.handleFileIoError(.opening, archive_path, cwd.openFile(archive_path, .{}));
            defer file.close();

            var archive = try Archive.create(cwd, file, archive_path, archive_type, modifiers, false);
            try archive.parse(allocator);
            for (archive.files.items) |parsed_file| {
                try parsed_file.contents.write(stdout, stderr);
            }
        },
        .print_symbols => {
            const file = try Archive.handleFileIoError(.opening, archive_path, cwd.openFile(archive_path, .{}));
            defer file.close();

            var archive = try Archive.create(cwd, file, archive_path, archive_type, modifiers, false);
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
        .move => {
            var created = false;
            const file = try openOrCreateFile(cwd, archive_path, !modifiers.create, &created);
            defer file.close();

            var archive = try Archive.create(cwd, file, archive_path, archive_type, modifiers, created);
            try archive.parse(allocator);
            try archive.moveFiles(files);
            try archive.finalize(allocator);
        },
        .quick_append => {
            logger.err("quick append still needs to be implemented!\n", .{});
            return error.TODO;
        },
        .ranlib => {
            const file = try Archive.handleFileIoError(.opening, archive_path, cwd.openFile(archive_path, .{ .mode = .read_write }));
            defer file.close();
            var archive = try Archive.create(cwd, file, archive_path, archive_type, modifiers, false);
            try archive.parse(allocator);
            try archive.finalize(allocator);
        },
        .extract => {
            logger.err("extract still needs to be implemented!\n", .{});
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
