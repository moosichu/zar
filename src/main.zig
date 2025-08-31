const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const trace = @import("tracy.zig").trace;
const fs = std.fs;
const io = std.io;
const mem = std.mem;
const logger = std.log.scoped(.archive_main);
const process = std.process;

pub const Archive = @import("archive/Archive.zig");

pub const zar_overview =
    \\Zig Archiver
    \\
    \\Usage:
    \\  zar [options] [-]<operation>[modifiers] [relpos] [count] <archive> [files]
    \\
    \\Description:
    \\  The Zig Archiver is the self-hosted implementation of the ar utility
    \\  function that originated from Unix, created as a drop-in replacement for
    \\  llvm's implementation of ar (llvm ar).
    \\
    \\  For more information on archivers and their usage, see:
    \\    - https://en.wikipedia.org/wiki/Ar_(Unix)
    \\    - https://www.freebsd.org/cgi/man.cgi?query=ar&sektion=1
    \\    - https://llvm.org/docs/CommandGuide/llvm-ar.html
    \\
    \\Options:
    \\ --format=<type>
    \\      Can be default, gnu, darwin or bsd. This determines the format used to
    \\      serialise an archive, this is ignored when parsing archives as type
    \\      there is always inferred. When creating an archive the host machine is
    \\      used to infer <type> if one is not specified.
    \\ --thin
    \\      Create and modify thin archives. By default archives aren't thin. Thin
    \\      archives are converted to regular achives when modified without this
    \\      option.
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
    \\ a - Put [files] after the archive member named by [relpos]. (r, m)
    \\ b - Put [files] before the archive member named by [relpos]. (r, m)
    \\ c - Disable creation warning if inserting files to new archive. (r, q)
    \\ D - Use zero for timestamps, GIDs and UIDs in archived files (enabled by
    \\     default). (r, q, s)
    \\ h - Display this help text and exit. (alias for --help)
    \\ i - Put [files] before the archive member named by [relpos]. (r, m)
    \\ l - Ignored for compatability.
    \\ L - When quick appending and archive to an archive, append members. (q)
    \\ N - Delete the [count]th instance of duplicate member with [name]. (d)
    \\ o - Preserve the archived modification times on extraction. (x)
    \\ O - Display member offsets inside the archive. (?)
    \\ P - Use full paths when matching member names. Default for thin archives.
    \\ r - Create sorted symbol table.
    \\ R - Do not create sorted symbol table.
    \\ s - Generate symbol table, enabled by default. (i.e. as if using ranlib)
    \\ S - Do not generate symbol table.
    \\ T - Create and modify thin archives. (alias for --thin)
    \\ u - Only update archive contents if [files] have more recent timestamps
    \\     than it.
    \\ U - Use real timestamps, GIDS and UIDs for archived files.
    \\ v - Print verbose output, depending on opertion:
    \\      S: show file names that symbols belong to.
    \\ V - Display the version and exit.
    \\
    \\Note, in the case of conflicting modifiers, the last one listed always takes
    \\precedence.
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

const version = if (@hasField(build_options, "version")) build_options.version else "0.0.0";

const version_details =
    \\zar {s} (https://github.com/moosichu/zar):
    \\  zar version {s}
    \\  {s} build
    \\  default archive type: {s}
    \\  host: {s}-{s}-{s}
    \\
;

pub const debug_errors = builtin.mode == .Debug;
pub const log_level: std.log.Level = if (builtin.mode == .Debug) .debug else .warn;

pub const Mode = enum { ar, ranlib };

pub var mode: Mode = .ar;

fn printError(stderr: *std.io.Writer, stderr_config: std.io.tty.Config, comptime format: []const u8, args: anytype) void {
    stderr.print("zar: ", .{}) catch {};
    stderr_config.setColor(stderr, .red) catch {};
    stderr_config.setColor(stderr, .bold) catch {};
    stderr.print("error: ", .{}) catch {};
    stderr_config.setColor(stderr, .reset) catch {};
    stderr.print(format, args) catch {};
    stderr.print("\n", .{}) catch {};
    printHelp(stderr);
}

fn printHelp(stdout: *std.io.Writer) void {
    _ = switch (mode) {
        .ar => stdout.print(zar_overview, .{}),
        .ranlib => stdout.print(ranlib_overview, .{}),
    } catch {};
    stdout.flush() catch {};
}

fn printVersion(stdout: *std.io.Writer) void {
    const target = builtin.target;
    const default_archive_type = @tagName(Archive.getDefaultArchiveTypeFromHost());
    stdout.print(version_details, .{ @tagName(mode), version, @tagName(builtin.mode), default_archive_type, @tagName(target.cpu.arch), @tagName(target.os.tag), @tagName(target.abi) }) catch {};
    stdout.flush() catch {};
}

fn checkOptionalArgsBounds(
    stderr: *std.io.Writer,
    stderr_config: std.io.tty.Config,
    args: []const []const u8,
    index: usize,
    comptime missing_argument: []const u8,
    comptime for_modifier: []const u8,
) bool {
    if (index >= args.len or args[index].len < 1 or args[index][0] == '-') {
        printError(stderr, stderr_config, missing_argument ++ " must be provided for " ++ for_modifier ++ " modifier.", .{});
        return false;
    }
    return true;
}

fn openOrCreateFile(stdout: *std.io.Writer, cwd: fs.Dir, archive_path: []const u8, print_creation_warning: bool, created: *bool) !fs.File {
    created.* = false;
    const open_file_handle = cwd.openFile(archive_path, .{ .mode = .read_write }) catch |err| switch (err) {
        error.FileNotFound => {
            created.* = true;
            if (print_creation_warning) {
                stdout.print("Creating new archive as none exists at path provided\n", .{}) catch {};
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

fn processModifier(stderr: *std.io.Writer, stderr_config: std.io.tty.Config, modifier_char: u8, modifiers: *Archive.Modifiers) bool {
    // TODO(#63): make sure modifers are only allowed for their supported mode of
    // operation!
    switch (mode) {
        .ar => switch (modifier_char) {
            'a' => modifiers.move_setting = .{ .before = null },
            'b', 'i' => modifiers.move_setting = .{ .after = null },
            'c' => modifiers.create = true,
            'D' => modifiers.use_real_timestamps_and_ids = false,
            'h' => modifiers.help = true,
            'l' => {}, // ignored for compatability
            'L' => modifiers.quick_append_members = true,
            'N' => modifiers.instance_to_delete = 0,
            'o' => modifiers.preserve_original_dates = true,
            'O' => unreachable, // TODO(#69): implement this!
            'P' => modifiers.use_full_paths_when_matching = true,
            'r' => modifiers.sort_symbol_table = .set_true,
            'R' => modifiers.sort_symbol_table = .set_false,
            's' => modifiers.build_symbol_table = true,
            'S' => modifiers.build_symbol_table = false,
            'T' => modifiers.thin_archives = true,
            'u' => modifiers.update_only = true,
            'U' => modifiers.use_real_timestamps_and_ids = true,
            'v' => modifiers.verbose = true,
            'V' => modifiers.show_version = true,
            // TODO(#64): Ensure all modifiers we need to handle are handled!
            else => {
                printError(stderr, stderr_config, "'{c}' is not a valid modifier.", .{modifier_char});
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
            // TODO(#65): write tests for these cases of ordering modifiers
            'v' => if (!modifiers.help) {
                modifiers.show_version = true;
            },
            'h' => if (!modifiers.show_version) {
                modifiers.help = true;
            },
            else => {
                printError(stderr, stderr_config, "'{c}' is not a valid option.", .{modifier_char});
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

    const allocator = arena.allocator();
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

pub fn linkAsArchive(stdout: *std.io.Writer, gpa: std.mem.Allocator, archive_path: []const u8, file_names_ptr: []const [*:0]const u8, archive_type: Archive.ArchiveType) !void {
    var modifiers: Archive.Modifiers = .{};
    modifiers.build_symbol_table = true;
    modifiers.create = true;

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    const allocator = arena.allocator();

    const cwd = fs.cwd();

    var created = false;
    const file = try openOrCreateFile(stdout, cwd, archive_path, !modifiers.create, &created);
    defer file.close();

    var files = std.ArrayList([]const u8).init(allocator);
    defer files.deinit();
    for (file_names_ptr) |file_name_z| {
        const file_name = file_name_z[0..std.mem.len(file_name_z)];
        try files.append(file_name);
    }

    var archive = try Archive.init(allocator, cwd, file, archive_path, archive_type, modifiers, created);
    defer archive.deinit();
    try archive.parse();
    try archive.insertFiles(files.items);
    try archive.flush();
}

pub fn archiveMain(cwd: fs.Dir, allocator: anytype, args: []const []const u8) (Archive.UnhandledError || Archive.HandledError)!void {
    // const tracy_zone = ztracy.zoneNC(@src(), "ArchiveMain", 0x00_ff_00_00, 1);
    // defer tracy_zone.end();
    var stdout_buf: [1024]u8 = undefined;
    var stderr_buf: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buf);
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buf);
    const stdout = &stdout_writer.interface;
    const stderr = &stderr_writer.interface;

    const stdout_config = std.io.tty.detectConfig(std.fs.File.stdout());
    const stderr_config = std.io.tty.detectConfig(std.fs.File.stderr());
    _ = stdout_config;

    var archive_type = Archive.ArchiveType.ambiguous;
    defer stdout.flush() catch {};
    defer stderr.flush() catch {};

    // Check if we are in ranlib mode!
    mode, const offset: usize = determine: {
        if (args.len > 1) {
            if (mem.eql(u8, "ranlib", args[1])) {
                // skip executable name + "ranlib"
                break :determine .{ .ranlib, 2 };
            }
        }
        // only skip the executable name
        break :determine .{ .ar, 1 };
    };

    var modifiers: Archive.Modifiers = .{};
    var operation: Archive.Operation = if (mode == .ranlib) .ranlib else .undefined;
    var found_archive_path: ?[]const u8 = null;
    var files = std.array_list.Managed([]const u8).init(allocator);
    defer files.deinit();

    const ParseState = enum { normal, relpos_before, relpos_after, count_gate, count };
    var parser_state: ParseState = .normal;
    nxt: for (args[offset..], offset..) |arg, arg_index| {
        cur: switch (parser_state) {
            .normal => {
                {
                    const format_string_prefix = "--format=";
                    const plugin_string_prefix = "--plugin=";
                    const help_string = "--help";
                    const version_string = "--version";
                    const thin_string = "--thin";
                    if (mode == .ar and mem.startsWith(u8, arg, format_string_prefix)) {
                        const format_string = arg[format_string_prefix.len..];
                        if (mem.eql(u8, format_string, "default")) {
                            archive_type = Archive.ArchiveType.ambiguous;
                        } else if (mem.eql(u8, format_string, "bsd")) {
                            archive_type = .bsd;
                        } else if (mem.eql(u8, format_string, "darwin")) {
                            archive_type = .darwin;
                        } else if (mem.eql(u8, format_string, "gnu")) {
                            archive_type = .gnu;
                        } else {
                            printError(stderr, stderr_config, "Invalid format {s}", .{format_string});
                            return Archive.HandledError.UnknownFormat;
                        }
                        continue;
                    } else if (mode == .ar and mem.startsWith(u8, arg, plugin_string_prefix)) {
                        // Ignored for compatability!
                        continue;
                    } else if (mode == .ar and mem.eql(u8, arg, thin_string)) {
                        modifiers.thin_archives = true;
                        continue;
                    } else if (arg.len == 0) {
                        continue;
                    } else if (mem.eql(u8, arg, help_string)) {
                        printHelp(stdout);
                        return;
                    } else if (mem.eql(u8, arg, version_string)) {
                        printVersion(stdout);
                        return;
                    }
                }

                var modifier_slice: []const u8 = "";
                if (operation == .undefined) {
                    operation = operation: {
                        const operation_slice = slice: {
                            // the operation may start with a hyphen - so slice it!
                            var arg_slice = arg[0..];
                            if (arg_slice[0] == '-') {
                                if (arg_slice.len == 1) {
                                    printError(stderr, stderr_config, "A valid operation must be provided - only hyphen found.", .{});
                                    return;
                                }

                                arg_slice = arg_slice[1..];
                            }

                            break :slice arg_slice;
                        };

                        modifier_slice = operation_slice[1..];

                        // Process Operation
                        switch (operation_slice[0]) {
                            'r' => break :operation .insert,
                            'd' => break :operation .delete,
                            'm' => break :operation .move,
                            'p' => break :operation .print_contents,
                            'q' => break :operation .quick_append,
                            's' => break :operation .ranlib,
                            't' => break :operation .print_names,
                            'x' => break :operation .extract,
                            'S' => break :operation .print_symbols,
                            else => {
                                printError(stderr, stderr_config, "'{c}' is not a valid operation.", .{operation_slice[0]});
                                return;
                            },
                        }
                    };
                } else if (arg[0] == '-') {
                    if (arg.len > 1) {
                        modifier_slice = arg[1..];
                    }
                } else if (found_archive_path == null) {
                    found_archive_path = arg;
                    continue;
                } else {
                    try files.append(arg);
                    continue;
                }

                for (modifier_slice) |modifier_char| {
                    if (!processModifier(stderr, stderr_config, modifier_char, &modifiers)) {
                        return;
                    }
                }

                // Process [relpos] if needed!
                switch (modifiers.move_setting) {
                    .end => {}, // do nothing!
                    .before => |before| if (before) |_| {} else {
                        parser_state = .relpos_before;
                        continue :nxt;
                    },
                    .after => |after| if (after) |_| {} else {
                        parser_state = .relpos_after;
                        continue :nxt;
                    },
                }

                continue :cur .count_gate;
            },
            .relpos_before => {
                if (!checkOptionalArgsBounds(stderr, stderr_config, args, arg_index, "A [relpos]", "a, b or i")) return;
                modifiers.move_setting.before = arg;
                continue :cur .count_gate;
            },
            .relpos_after => {
                if (!checkOptionalArgsBounds(stderr, stderr_config, args, arg_index, "A [relpos]", "a, b or i")) return;
                modifiers.move_setting.after = arg;
                continue :cur .count_gate;
            },
            .count_gate => {
                // Process [count] if needed!
                parser_state = if (modifiers.instance_to_delete == 0) .count else .normal;
            },
            .count => {
                if (!checkOptionalArgsBounds(stderr, stderr_config, args, arg_index, "An [count]", "N")) return;
                modifiers.instance_to_delete = std.fmt.parseUnsigned(u32, arg, 10) catch {
                    printError(stderr, stderr_config, "[count] must be a positive number, received '{s}'.", .{arg});
                    return;
                };
                parser_state = .normal;
            },
        }
    }

    if (modifiers.help) {
        printHelp(stdout);
        return;
    }

    if (modifiers.show_version) {
        printVersion(stdout);
        return;
    }

    if (modifiers.move_setting != .end) {
        // TODO(#66): Implement this!
        return error.TODO;
    }

    if (modifiers.instance_to_delete > 1) {
        // TODO(#67): Implement this!
        return error.TODO;
    }

    if (modifiers.use_full_paths_when_matching) {
        // TODO(#68): Implement this!
        return error.TODO;
    }

    if (modifiers.thin_archives) {
        // TODO(#70): support thin archives!
        return error.TODO;
    }

    if (operation == .undefined) {
        printError(stderr, stderr_config, "An operation must be provided.", .{});
        return;
    }

    const archive_path = archive_path: {
        if (found_archive_path) |archive_path| {
            break :archive_path archive_path;
        }

        printError(stderr, stderr_config, "An archive must be provided.", .{});
        return;
    };

    switch (operation) {
        .insert => {
            var created = false;
            const file = try openOrCreateFile(stdout, cwd, archive_path, !modifiers.create, &created);
            defer file.close();

            var archive = try Archive.init(allocator, cwd, file, archive_path, archive_type, modifiers, created);
            defer archive.deinit();
            try archive.parse();
            try archive.insertFiles(files.items);
            try archive.flush();
        },
        .delete => {
            var created = false;
            const file = try openOrCreateFile(stdout, cwd, archive_path, !modifiers.create, &created);
            defer file.close();

            var archive = try Archive.init(allocator, cwd, file, archive_path, archive_type, modifiers, created);
            defer archive.deinit();
            try archive.parse();
            try archive.deleteFiles(files.items);
            try archive.flush();
        },
        .print_names => {
            const file = try Archive.handleFileIoError(.opening, archive_path, cwd.openFile(archive_path, .{}));
            defer file.close();

            var archive = try Archive.init(allocator, cwd, file, archive_path, archive_type, modifiers, false);
            defer archive.deinit();
            try archive.parse();
            for (archive.files.items) |parsed_file| {
                stdout.print("{s}\n", .{parsed_file.name}) catch {};
            }
        },
        .print_contents => {
            const file = try Archive.handleFileIoError(.opening, archive_path, cwd.openFile(archive_path, .{}));
            defer file.close();

            var archive = try Archive.init(allocator, cwd, file, archive_path, archive_type, modifiers, false);
            defer archive.deinit();
            try archive.parse();
            for (archive.files.items) |parsed_file| {
                parsed_file.contents.write(stdout, stderr) catch {};
            }
        },
        .print_symbols => {
            const file = try Archive.handleFileIoError(.opening, archive_path, cwd.openFile(archive_path, .{}));
            defer file.close();

            var archive = try Archive.init(allocator, cwd, file, archive_path, archive_type, modifiers, false);
            defer archive.deinit();
            try archive.parse();
            for (archive.symbols.items) |symbol| {
                if (modifiers.verbose) {
                    if (symbol.file_index == Archive.invalid_file_index) {
                        stdout.print("?: {s}\n", .{symbol.name}) catch {};
                    } else {
                        stdout.print("{s}: {s}\n", .{ archive.files.items[symbol.file_index].name, symbol.name }) catch {};
                    }
                } else {
                    stdout.print("{s}\n", .{symbol.name}) catch {};
                }
            }
        },
        .move => {
            var created = false;
            const file = try openOrCreateFile(stdout, cwd, archive_path, !modifiers.create, &created);
            defer file.close();

            var archive = try Archive.init(allocator, cwd, file, archive_path, archive_type, modifiers, created);
            defer archive.deinit();
            try archive.parse();
            try archive.moveFiles(files.items);
            try archive.flush();
        },
        .quick_append => {
            printError(stderr, stderr_config, "quick append still needs to be implemented!\n", .{});
            return error.TODO; // #71
        },
        .ranlib => {
            const file = try Archive.handleFileIoError(.opening, archive_path, cwd.openFile(archive_path, .{ .mode = .read_write }));
            defer file.close();
            var archive = try Archive.init(allocator, cwd, file, archive_path, archive_type, modifiers, false);
            defer archive.deinit();
            try archive.parse();
            try archive.flush();
        },
        .extract => {
            printError(stderr, stderr_config, "extract still needs to be implemented!\n", .{});
            if (modifiers.preserve_original_dates) {
                return error.TODO; // #74
            }
            return error.TODO; // #73
        },
        .undefined => {
            // This case is already handled earlier!
            unreachable;
        },
    }
}

fn handleArchiveError(err: (Archive.HandledError || Archive.UnhandledError)) !void {
    var stderr_buf: [1024]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buf);
    const stderr = &stderr_writer.interface;
    const stderr_config = std.io.tty.detectConfig(std.fs.File.stderr());
    defer stderr.flush() catch {};

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

    const unhandled_err: Archive.UnhandledError = @errorCast(err);

    switch (unhandled_err) {
        // These are errors which already have appropraite log messages printed
        Archive.ParseError.NotArchive => printError(stderr, stderr_config, "Provided file is not an archive.", .{}),
        Archive.ParseError.MalformedArchive, Archive.ParseError.Overflow, Archive.ParseError.InvalidCharacter => printError(stderr, stderr_config, "Malformed archive provided.", .{}),
        error.OutOfMemory => printError(stderr, stderr_config, "Program ran out of memory.", .{}),
        error.TODO => printError(stderr, stderr_config, "Unimplemented feature encountered (TODO error)", .{}),
    }

    if (debug_errors) return err;
}
