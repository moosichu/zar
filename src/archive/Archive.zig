const Archive = @This();

const std = @import("std");
const fmt = std.fmt;
const fs = std.fs;
const mem = std.mem;
const log = std.log.scoped(.archive);

const Allocator = std.mem.Allocator;

file: fs.File,
name: []const u8,

// We need to differentiate between inferred and output archive type, as other ar
// programs "just handle" any valid archive for parsing, regarldess of what a
// user has specified - the user specification should only matter for writing
// archives.
inferred_archive_type: ArchiveType,
output_archive_type: ArchiveType,

files: std.ArrayListUnmanaged(ArchivedFile),

// Use it so we can easily lookup files indices when inserting!
// TODO: A trie is probably a lot better here
filename_to_index: std.StringArrayHashMapUnmanaged(u64),

pub const ArchiveType = enum {
    ambiguous,
    gnu,
    gnuthin,
    gnu64,
    bsd,
    darwin64, // darwin_32 *is* bsd
    coff, // (windows)
};

pub const Operation = enum {
    insert,
    delete,
    move,
    print_contents,
    quick_append,
    ranlib,
    print_names,
    extract,
};

// All archive files start with this magic string
pub const magic_string = "!<arch>\n";
pub const magic_thin = "!<thin>\n";

// GNU constants
pub const gnu_first_line_buffer_length = 60;
pub const gnu_string_table_seek_pos = magic_string.len + gnu_first_line_buffer_length;

// BSD constants
pub const bsd_name_length_signifier = "#1/";
pub const bsd_symdef_magic = "__.SYMDEF";

// The format (unparsed) of the archive per-file header
// NOTE: The reality is more complex than this as different mechanisms
// have been devised for storing the names of files which exceed 16 byte!
pub const Header = extern struct {
    ar_name: [16]u8,
    ar_date: [12]u8,
    ar_uid: [6]u8,
    ar_gid: [6]u8,
    ar_mode: [8]u8,
    ar_size: [10]u8,
    ar_fmag: [2]u8,
};

pub const Contents = struct {
    bytes: []u8,
    length: u64,
    // mode: u64,

    // TODO: dellocation

    pub fn write(self: *const Contents, out_stream: anytype, stderr: anytype) !void {
        try out_stream.writeAll(self.bytes);
        _ = stderr;
    }
};

// An internal represantion of files being archived
pub const ArchivedFile = struct {
    name: []const u8,
    contents: Contents,
};

pub fn create(
    file: fs.File,
    name: []const u8,
    output_archive_type: ArchiveType,
) Archive {
    return Archive{
        .file = file,
        .name = name,
        .inferred_archive_type = .ambiguous,
        .output_archive_type = output_archive_type,
        .files = .{},
        .filename_to_index = .{},
    };
}

// TODO: This needs to be integrated into the workflow
// used for parsing. (use same error handling workflow etc.)

/// Use same naming scheme for objects (as found elsewhere in the file).
pub fn finalize(self: *Archive, allocator: *Allocator) !void {
    // Overwrite all contents
    try self.file.seekTo(0);

    if (self.output_archive_type == .ambiguous) {
        // Set output archive type of one we might just have parsed...
        self.output_archive_type = self.inferred_archive_type;
    }

    if (self.output_archive_type == .ambiguous) {
        // if output archive type is still ambiguous (none was inferred, and
        // none was set) then we need to infer it from the host platform!s

        // TODO: Set this based on the current platform you are using the tool
        // on!
        self.output_archive_type = .gnu;
    }

    const writer = self.file.writer();
    try writer.writeAll(if (self.output_archive_type == .gnuthin) magic_thin else magic_string);

    const header_names = try allocator.alloc([16]u8, self.files.items.len);

    switch (self.output_archive_type) {
        .gnu, .gnuthin, .gnu64 => {
            // GNU format: Create string table
            var string_table = std.ArrayList(u8).init(allocator);
            defer string_table.deinit();

            // Generate the complete string table
            for (self.files.items) |file, index| {
                const is_the_name_allowed = (file.name.len < 16) and (self.output_archive_type != .gnuthin);

                // If the file is small enough to fit in header, then just write it there
                // Otherwise, add it to string table and add a reference to its location
                const name = if (is_the_name_allowed) try mem.concat(allocator, u8, &.{ file.name, "/" }) else try std.fmt.allocPrint(allocator, "/{}", .{blk: {
                    // Get the position of the file in string table
                    const pos = string_table.items.len;

                    // Now add the file name to string table
                    try string_table.appendSlice(file.name);
                    try string_table.appendSlice("/\n");

                    break :blk pos;
                }});
                defer allocator.free(name);

                // Edit the header
                _ = try std.fmt.bufPrint(&(header_names[index]), "{s: <16}", .{name});
            }

            // Write the string table itself
            {
                if (string_table.items.len != 0) {
                    try writer.print("//{s}{: <10}`\n{s}", .{ " " ** 46, string_table.items.len, string_table.items });
                }
            }
        },
        .bsd, .darwin64 => {
            // BSD format: Just write the length of the name in header
            for (self.files.items) |file, index| {
                _ = try std.fmt.bufPrint(&(header_names[index]), "#1/{: <13}", .{file.name.len});
            }
        },
        else => unreachable,
    }

    // Write the files
    for (self.files.items) |file, index| {
        // Write the header
        // For now, we just write a garbage value to header.name and resolve it later
        var headerBuffer: [@sizeOf(Header)]u8 = undefined;
        _ = try std.fmt.bufPrint(
            &headerBuffer,
            "{s: <16}{: <12}{: <6}{: <6}{o: <8}{: <10}`\n",
            .{ &header_names[index], 0, 0, 0, 0, file.contents.length },
        );

        // TODO: handle errors
        _ = try writer.write(&headerBuffer);

        // Write the name of the file in the data section
        if (self.output_archive_type == .bsd) {
            try writer.writeAll(file.name);
        }

        if (self.output_archive_type != .gnuthin)
            try file.contents.write(writer, null);
    }

    // Truncate the file size
    try self.file.setEndPos(try self.file.getPos());
}

pub fn deleteFiles(self: *Archive, file_names: [][]const u8) !void {
    // For the list of given file names, find the entry in self.files
    // and remove it from self.files.
    for (file_names) |file_name| {
        for (self.files.items) |file, index| {
            if (std.mem.eql(u8, file.name, file_name)) {
                _ = self.files.orderedRemove(index);
                break;
            }
        }
    }
}

pub fn extract(self: *Archive, file_names: [][]const u8) !void {
    if (self.inferred_archive_type == .gnuthin) {
        // TODO: better error
        return error.ExtractingFromThin;
    }

    for (self.files.items) |archived_file| {
        for (file_names) |file_name| {
            if (std.mem.eql(u8, archived_file.name, file_name)) {
                const file = try std.fs.cwd().createFile(archived_file.name, .{});
                defer file.close();

                try file.writeAll(archived_file.contents.bytes);
                break;
            }
        }
    }
}

pub fn insertFiles(self: *Archive, allocator: *Allocator, file_names: [][]const u8) !void {
    for (file_names) |file_name| {
        // Open the file and read all of its contents
        const file = try std.fs.cwd().openFile(file_name, .{ .read = true });
        const file_stats = try file.stat();
        const archived_file = ArchivedFile{
            .name = file_name, // TODO: sort out the file-name with respect to path
            .contents = Contents{
                .bytes = try file.readToEndAlloc(allocator, std.math.maxInt(usize)),
                .length = file_stats.size,
                // .mode = file_stats.mode,
            },
        };

        // A trie-based datastructure would be better for this!
        const getOrPutResult = try self.filename_to_index.getOrPut(allocator, archived_file.name);
        if (getOrPutResult.found_existing) {
            const existing_index = getOrPutResult.value_ptr.*;
            self.files.items[existing_index] = archived_file;
        } else {
            getOrPutResult.value_ptr.* = self.files.items.len;
            try self.files.append(allocator, archived_file);
        }
    }
}

pub fn parse(self: *Archive, allocator: *Allocator, stderr: anytype) !void {
    const reader = self.file.reader();
    {
        // Is the magic header found at the start of the archive?
        var magic: [magic_string.len]u8 = undefined;
        const bytes_read = try reader.read(&magic);

        if (bytes_read == 0) {
            // Archive is empty and that is ok!
            return;
        }

        if (bytes_read < magic_string.len) {
            try stderr.print("File too short to be an archive\n", .{});
            return error.NotArchive;
        }

        const is_thin_archive = mem.eql(u8, &magic, magic_thin);

        if (is_thin_archive)
            self.inferred_archive_type = .gnuthin;

        if (!(mem.eql(u8, &magic, magic_string) or is_thin_archive)) {
            try stderr.print("Invalid magic string: expected '{s}' or '{s}', found '{s}'\n", .{ magic_string, magic_thin, magic });
            return error.NotArchive;
        }
    }

    var gnu_symbol_table_contents: []u8 = undefined;
    var string_table_contents: []u8 = undefined;
    var has_gnu_symbol_table = false;
    {
        // https://www.freebsd.org/cgi/man.cgi?query=ar&sektion=5
        // Process string/symbol tables and/or try to infer archive type!
        var starting_seek_pos = magic_string.len;
        while (true) {
            var first_line_buffer: [gnu_first_line_buffer_length]u8 = undefined;

            const has_line_to_process = result: {
                const chars_read = reader.read(&first_line_buffer) catch |err| switch (err) {
                    else => |e| return e,
                };

                if (chars_read < first_line_buffer.len) {
                    break :result false;
                }

                break :result true;
            };

            if (has_line_to_process) {
                if (mem.eql(u8, first_line_buffer[0..1], "//"[0..1])) {
                    switch (self.inferred_archive_type) {
                        .ambiguous => self.inferred_archive_type = .gnu,
                        .gnu, .gnuthin, .gnu64 => {},
                        else => {
                            try stderr.print("Came across gnu-style string table in {} archive\n", .{self.inferred_archive_type});
                            return error.NotArchive;
                        },
                    }

                    const table_size_string = first_line_buffer[48..58];
                    const table_size = try fmt.parseInt(u32, mem.trim(u8, table_size_string, " "), 10);

                    string_table_contents = try allocator.alloc(u8, table_size);
                    // TODO: actually error handle not expected number of bytes being read!
                    _ = try reader.read(string_table_contents);
                    // starting_seek_pos = starting_seek_pos + first_line_buffer.len + table_size;
                    break;
                } else if (!has_gnu_symbol_table and first_line_buffer[0] == '/') {
                    has_gnu_symbol_table = true;
                    switch (self.inferred_archive_type) {
                        .ambiguous => self.inferred_archive_type = .gnu,
                        .gnu, .gnuthin, .gnu64 => {},
                        else => {
                            try stderr.print("Came across gnu-style symbol table in {} archive\n", .{self.inferred_archive_type});
                            return error.NotArchive;
                        },
                    }

                    const table_size_string = first_line_buffer[48..58];
                    const table_size = try fmt.parseInt(u32, mem.trim(u8, table_size_string, " "), 10);

                    gnu_symbol_table_contents = try allocator.alloc(u8, table_size);
                    _ = try reader.read(gnu_symbol_table_contents);
                    // TODO: actually error handle not expected number of bytes being read!

                    starting_seek_pos = starting_seek_pos + first_line_buffer.len + table_size;
                } else {
                    try reader.context.seekTo(starting_seek_pos);
                    break;
                }
            }
        }
    }

    var is_first = true;

    while (true) {
        const archive_header = reader.readStruct(Header) catch |err| switch (err) {
            error.EndOfStream => break,
            else => |e| return e,
        };

        // the lifetime of the archive headers will matched that of the parsed files (for now)
        // so we can take a reference to the strings stored there directly!
        var trimmed_archive_name = mem.trim(u8, &archive_header.ar_name, " ");

        // Check against gnu naming properties
        const ends_with_gnu_slash = (trimmed_archive_name[trimmed_archive_name.len - 1] == '/');
        var gnu_offset_value: u32 = 0;
        const starts_with_gnu_offset = trimmed_archive_name[0] == '/';
        if (starts_with_gnu_offset) {
            gnu_offset_value = try fmt.parseInt(u32, trimmed_archive_name[1..trimmed_archive_name.len], 10);
        }

        const must_be_gnu = ends_with_gnu_slash or starts_with_gnu_offset or has_gnu_symbol_table;

        // Check against bsd naming properties
        const starts_with_bsd_name_length = (trimmed_archive_name.len >= 2) and mem.eql(u8, trimmed_archive_name[0..2], bsd_name_length_signifier[0..2]);
        const could_be_bsd = starts_with_bsd_name_length;

        // TODO: Have a proper mechanism for erroring on the wrong types of archive.
        switch (self.inferred_archive_type) {
            .ambiguous => {
                if (must_be_gnu) {
                    self.inferred_archive_type = .gnu;
                } else if (could_be_bsd) {
                    self.inferred_archive_type = .bsd;
                } else {
                    return error.TODO;
                }
            },
            .gnu, .gnuthin, .gnu64 => {
                if (!must_be_gnu) {
                    try stderr.print("Error parsing archive header name - format of {s} wasn't gnu compatible\n", .{trimmed_archive_name});
                    return error.BadArchive;
                }
            },
            .bsd, .darwin64 => {
                if (must_be_gnu) {
                    try stderr.print("Error parsing archive header name - format of {s} wasn't bsd compatible\n", .{trimmed_archive_name});
                    return error.BadArchive;
                }
            },
            else => {
                if (must_be_gnu) {
                    return error.TODO;
                }

                return error.TODO;
            },
        }

        if (ends_with_gnu_slash) {
            // slice-off the slash
            trimmed_archive_name = trimmed_archive_name[0 .. trimmed_archive_name.len - 1];
        }

        if (starts_with_gnu_offset) {
            const name_offset_in_string_table = try fmt.parseInt(u32, mem.trim(u8, trimmed_archive_name[1..trimmed_archive_name.len], " "), 10);

            // Navigate to the start of the string in the string table
            const string_start = string_table_contents[name_offset_in_string_table..string_table_contents.len];

            // Find the end of the string (which is always a newline)
            const end_string_index = mem.indexOf(u8, string_start, "\n");
            if (end_string_index == null) {
                try stderr.print("Error parsing name in string table, couldn't find terminating character\n", .{});
                return error.NotArchive;
            }
            const string_full = string_start[0..end_string_index.?];

            // String must have a forward slash before the newline, so check that
            // is there and remove it as well!
            if (string_full[string_full.len - 1] != '/') {
                try stderr.print("Error parsing name in string table, didn't find '/' before terminating newline\n", .{});
                return error.NotArchive;
            }

            // Referencing the slice directly is fine as same bumb allocator is
            // used as for the rest of the datastructure!
            trimmed_archive_name = string_full[0 .. string_full.len - 1];
        }

        var seek_forward_amount = try fmt.parseInt(u32, mem.trim(u8, &archive_header.ar_size, " "), 10);

        // Make sure that these allocations get properly disposed of later!
        if (starts_with_bsd_name_length) {
            trimmed_archive_name = trimmed_archive_name[bsd_name_length_signifier.len..trimmed_archive_name.len];
            const archive_name_length = fmt.parseInt(u32, trimmed_archive_name, 10) catch {
                try stderr.print("Error parsing bsd-style string length\n", .{});
                return error.NotArchive;
            };

            if (is_first) {
                // TODO: make sure this does a check on self.inferred_archive_type!

                // This could be the symbol table! So parse that here!
                const current_seek_pos = try reader.context.getPos();
                var symbol_magic_check_buffer: [bsd_symdef_magic.len]u8 = undefined;

                // TODO: handle not reading enough characters!
                _ = try reader.read(&symbol_magic_check_buffer);
                if (mem.eql(u8, bsd_symdef_magic, &symbol_magic_check_buffer)) {
                    // We have a symbol table!
                    // TODO: parse symbol table, we just skip it for now...
                    seek_forward_amount = seek_forward_amount - @as(u32, symbol_magic_check_buffer.len);
                    try reader.context.seekBy(seek_forward_amount);
                    break;
                }

                try reader.context.seekTo(current_seek_pos);
            }

            is_first = false;

            const archive_name_buffer = try allocator.alloc(u8, archive_name_length);

            // TODO: proper error handling and length checking here!
            _ = try reader.read(archive_name_buffer);
            seek_forward_amount = seek_forward_amount - archive_name_length;

            // strip null characters from name - TODO find documentation on this
            // could not find documentation on this being needed, but some archivers
            // seems to insert these (for alignment reasons?)
            trimmed_archive_name = mem.trim(u8, archive_name_buffer, "\x00");
        } else {
            const archive_name_buffer = try allocator.alloc(u8, trimmed_archive_name.len);
            mem.copy(u8, archive_name_buffer, trimmed_archive_name);
            trimmed_archive_name = archive_name_buffer;
        }

        const parsed_file = ArchivedFile{
            .name = trimmed_archive_name,
            .contents = Contents{
                .bytes = try allocator.alloc(u8, seek_forward_amount),
                .length = seek_forward_amount,
            },
        };

        if (self.inferred_archive_type == .gnuthin) {
            var thin_file = try std.fs.cwd().openFile(trimmed_archive_name, .{});
            defer thin_file.close();

            try thin_file.reader().readNoEof(parsed_file.contents.bytes);
        } else {
            try reader.readNoEof(parsed_file.contents.bytes);
        }

        try self.filename_to_index.put(allocator, trimmed_archive_name, self.files.items.len);
        try self.files.append(allocator, parsed_file);
    }
}

pub const MRIParser = struct {
    script: []const u8,
    archive: ?Archive,
    file_name: ?[]const u8,

    const CommandType = enum {
        open,
        create,
        createthin,
        addmod,
        list,
        delete,
        extract,
        save,
        clear,
        end,
    };

    const Self = @This();

    pub fn init(allocator: *Allocator, file: fs.File) !Self {
        const self = Self{
            .script = try file.readToEndAlloc(allocator, std.math.maxInt(usize)),
            .archive = null,
            .file_name = null,
        };

        return self;
    }

    // Returns the next token
    fn getToken(iter: *mem.SplitIterator(u8)) ?[]const u8 {
        while (iter.next()) |tok| {
            if (mem.startsWith(u8, tok, "*")) break;
            if (mem.startsWith(u8, tok, ";")) break;
            return tok;
        }
        return null;
    }

    // Returns a slice of tokens
    fn getTokenLine(allocator: *Allocator, iter: *mem.SplitIterator(u8)) ![][]const u8 {
        var list = std.ArrayList([]const u8).init(allocator);
        while (getToken(iter)) |tok| {
            try list.append(tok);
        }
        return list.toOwnedSlice();
    }

    pub fn execute(self: *Self, allocator: *Allocator, stdout: fs.File.Writer, stderr: fs.File.Writer) !void {
        // Split the file into lines
        var parser = mem.split(u8, self.script, "\n");

        while (parser.next()) |line| {
            // Split the line by spaces
            var line_parser = mem.split(u8, line, " ");

            if (getToken(&line_parser)) |tok| {
                var command_name = try allocator.dupe(u8, tok);
                defer allocator.free(command_name);

                _ = std.ascii.lowerString(command_name, tok);

                if (std.meta.stringToEnum(CommandType, command_name)) |command| {
                    if (self.archive) |archive| {
                        switch (command) {
                            .addmod => {
                                const file_names = try getTokenLine(allocator, &line_parser);
                                defer allocator.free(file_names);

                                try self.archive.?.insertFiles(allocator, file_names);
                            },
                            .list => {
                                // TODO: verbose output
                                for (archive.files.items) |parsed_file| {
                                    try stdout.print("{s}\n", .{parsed_file.name});
                                }
                            },
                            .delete => {
                                const file_names = try getTokenLine(allocator, &line_parser);
                                try self.archive.?.deleteFiles(file_names);
                            },
                            .extract => {
                                const file_names = try getTokenLine(allocator, &line_parser);
                                try self.archive.?.extract(file_names);
                            },
                            .save => {
                                try self.archive.?.finalize(allocator);
                            },
                            .clear => {
                                // This is a bit of a hack but its reliable.
                                // Instead of clearing out unsaved changes, we re-open the current file, which overwrites the changes.
                                const file = try fs.cwd().openFile(self.file_name.?, .{ .write = true });
                                self.archive = Archive.create(file, self.file_name.?);

                                try self.archive.?.parse(allocator, stderr);
                            },
                            .end => return,
                            else => {
                                try stderr.print(
                                    "Archive `{s}` is currently open.\nThe command `{s}` can only be executed when no current archive is active.\n",
                                    .{ self.file_name.?, command_name },
                                );
                                return error.ArchiveAlreadyOpen;
                            },
                        }
                    } else {
                        switch (command) {
                            .open => {
                                const file_name = getToken(&line_parser).?;

                                const file = try fs.cwd().openFile(file_name, .{ .write = true });
                                self.archive = Archive.create(file, file_name);
                                self.file_name = file_name;

                                try self.archive.?.parse(allocator, stderr);
                            },
                            .create, .createthin => {
                                // TODO: Thin archives creation
                                const file_name = getToken(&line_parser).?;

                                const file = try fs.cwd().createFile(file_name, .{ .read = true });
                                self.archive = Archive.create(file, file_name);
                                self.file_name = file_name;

                                try self.archive.?.parse(allocator, stderr);
                            },
                            .end => return,
                            else => {
                                try stderr.print("No currently active archive found.\nThe command `{s}` can only be executed when there is an opened archive.\n", .{command_name});
                                return error.NoCurrentArchive;
                            },
                        }
                    }
                }
            }
        }
    }
};
