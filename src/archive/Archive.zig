const Archive = @This();

const std = @import("std");
const fmt = std.fmt;
const fs = std.fs;
const mem = std.mem;
const log = std.log.scoped(.archive);

const Allocator = std.mem.Allocator;

file: fs.File,
name: []const u8,
archive_type: ArchiveType,

files: std.ArrayListUnmanaged(ArchivedFile),

// Use it so we can easily lookup files indices when inserting!
// TODO: A trie is probably a lot better here
filename_to_index: std.StringArrayHashMapUnmanaged(u64),

pub const ArchiveType = enum {
    ambiguous,
    gnu,
    gnu64,
    bsd,
    darwin64, // darwin_32 *is* bsd
    coff, // (windows)
};

pub const Operation = enum {
    insert,
    delete,
    move,
    print,
    quick_append,
    ranlib,
    display_contents,
    extract,
};

// All archive files start with this magic string
pub const magic_string = "!<arch>\n";

// GNU constants
pub const gnu_first_line_buffer_length = 60;
pub const gnu_string_table_seek_pos = magic_string.len + gnu_first_line_buffer_length;

// BSD constants
pub const bsd_name_length_signifier = "#1/";

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
) Archive {
    return Archive{
        .file = file,
        .name = name,
        .archive_type = .ambiguous,
        .files = .{},
        .filename_to_index = .{},
    };
}

// TODO: This needs to be integrated into the workflow
// used for parsing. (use same error handling workflow etc.)

/// Use same naming scheme for objects (as found elsewhere in the file).
pub fn finalize(self: *Archive, allocator: *Allocator) !void {
    // TODO: Currently this is a bit of a mine-field - so maybe just reading all the file-contents
    // into memory is the best bet for now?

    // Overwrite all contents
    try self.file.seekTo(0);

    const writer = self.file.writer();
    try writer.writeAll(magic_string);

    if (self.archive_type == .ambiguous) {
        // TODO: Set this based on the current platform you are using the tool
        // on!
        self.archive_type = .gnu;
    }

    const header_names = try allocator.alloc([16]u8, self.files.items.len);

    // GNU format: Create string table
    if (self.archive_type == .gnu) {
        var string_table = std.ArrayList(u8).init(allocator);
        defer string_table.deinit();

        // Generate the complete string table
        for (self.files.items) |file, index| {
            const is_the_name_allowed = (file.name.len < 16);

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
    } else if (self.archive_type == .bsd) {
        // BSD format: Just write the length of the name in header
        for (self.files.items) |file, index| {
            _ = try std.fmt.bufPrint(&(header_names[index]), "#1/{: <13}", .{file.name.len});
        }
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
        if (self.archive_type == .bsd) {
            try writer.writeAll(file.name);
        }
        try file.contents.write(writer, null);
    }

    // Truncate the file size
    try self.file.setEndPos(try self.file.getPos());
}

pub fn deleteFiles(self: *Archive, file_names: ?[][]u8) !void {
    // For the list of given file names, find the entry in self.files
    // and remove it from self.files.
    if (file_names) |names| {
        for (names) |file_name| {
            for (self.files.items) |file, index| {
                if (std.mem.eql(u8, file.name, file_name)) {
                    _ = self.files.orderedRemove(index);
                    break;
                }
            }
        }
    }
}

// Convenience function for doing mass operations
const OperationErrorSet = Allocator.Error || std.fmt.ParseIntError;
fn massOperation(self: *Archive, file_names: ?[][]u8, data: anytype, cb: fn (item: ArchivedFile, index: usize, data: anytype) OperationErrorSet!void) !void {
    if (file_names) |names| {
        for (self.files.items) |file, index| {
            for (names) |name| {
                if (std.mem.eql(u8, file.name, name)) {
                    try cb(file, index, data);
                    break;
                }
            }
        }
    } else {
        for (self.objects.items) |item, index| {
            try cb(item, index, data);
        }
    }
}

fn printOperation(item: ArchivedFile, index: usize, data: anytype) !void {
    _ = index;

    const writer = data;
    try writer.print("{s}", .{item.contents});
}

pub fn print(self: *Archive, file_names: ?[][]u8, writer: std.fs.File.Writer) !void {
    try self.massOperation(file_names, writer, printOperation);
}

fn extractOperation(item: ArchivedFile, index: usize, data: anytype) !void {
    _ = index;
    _ = data;

    const file = try std.fs.cwd().createFile(item.name, .{});
    defer file.close();

    try file.writeAll(item.contents);
}

pub fn extract(self: *Archive, file_names: ?[][]u8) !void {
    try self.massOperation(file_names, null, extractOperation);
}

pub fn insertFiles(self: *Archive, allocator: *Allocator, file_names: ?[][]u8) !void {
    if (file_names) |names| {
        for (names) |file_name| {
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

        if (!mem.eql(u8, &magic, magic_string)) {
            try stderr.print("Invalid magic string: expected '{s}', found '{s}'\n", .{ magic_string, magic });
            return error.NotArchive;
        }
    }

    // https://www.freebsd.org/cgi/man.cgi?query=ar&sektion=5
    // Process string/symbol tables and/or try to infer archive type!
    var string_table_contents: []u8 = undefined;
    {
        var starting_seek_pos = magic_string.len;

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
                switch (self.archive_type) {
                    .ambiguous => self.archive_type = .gnu,
                    .gnu, .gnu64 => {},
                    else => {
                        try stderr.print("Came across gnu-style string table in {} archive\n", .{self.archive_type});
                        return error.NotArchive;
                    },
                }

                const table_size_string = first_line_buffer[48..58];
                const table_size = try fmt.parseInt(u32, mem.trim(u8, table_size_string, " "), 10);

                string_table_contents = try allocator.alloc(u8, table_size);
                // TODO: actually error handle not expected number of bytes being read!
                _ = try reader.read(string_table_contents);
                starting_seek_pos = starting_seek_pos + first_line_buffer.len + table_size;
            }
        }

        try reader.context.seekTo(starting_seek_pos);
    }

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

        const must_be_gnu = ends_with_gnu_slash or starts_with_gnu_offset;

        // Check against bsd naming properties
        const starts_with_bsd_name_length = (trimmed_archive_name.len >= 2) and mem.eql(u8, trimmed_archive_name[0..2], bsd_name_length_signifier[0..2]);
        const could_be_bsd = starts_with_bsd_name_length;

        // TODO: Have a proper mechanism for erroring on the wrong types of archive.
        switch (self.archive_type) {
            .ambiguous => {
                if (must_be_gnu) {
                    self.archive_type = .gnu;
                } else if (could_be_bsd) {
                    self.archive_type = .bsd;
                } else {
                    return error.TODO;
                }
            },
            .gnu, .gnu64 => {
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

            const archive_name_buffer = try allocator.alloc(u8, archive_name_length);

            // TODO: proper error handling and length checking here!
            _ = try reader.read(archive_name_buffer);
            seek_forward_amount = seek_forward_amount - archive_name_length;

            trimmed_archive_name = archive_name_buffer;
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

        try reader.readNoEof(parsed_file.contents.bytes);

        try self.filename_to_index.put(allocator, trimmed_archive_name, self.files.items.len);
        try self.files.append(allocator, parsed_file);
    }
}
