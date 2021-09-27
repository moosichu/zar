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

// These are currently seperate so we can append
// header values to array seperately and then have
// the parsed file point to slices into that memory
// that we know will remain persistent.
headers: std.ArrayListUnmanaged(Header),
files: std.ArrayListUnmanaged(ArchivedFile),

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

// An internal represantion of files being archived
pub const ArchivedFile = struct {
    name: []const u8,
    // TODO - represent contents (using tagged union?)
    // that can either be a file-handle, or a seek position in the
    // archive we are looking at.
    contents: []u8,
};

pub fn create(
    file: fs.File,
    name: []const u8,
) Archive {
    return Archive{
        .file = file,
        .name = name,
        .archive_type = .ambiguous,
        .headers = .{},
        .files = .{},
    };
}

pub fn finalize(self: *Archive) !void {    
    // Overwrite all contents
    try self.file.seekTo(0);

    const writer = self.file.writer();
    try writer.writeAll(magic_string);

    // Write the files
    for (self.files.items) |file, index| {
        try writer.writeStruct(self.headers.items[index]);
        try writer.writeAll(file.contents);
    }
    
    // Truncate the file size
    try self.file.setEndPos(try self.file.getPos());
}

pub fn addFiles(self: *Archive, allocator: *Allocator, file_names: ?[][]u8) !void {
    if (file_names) |names| {
        for (names) |file_name| {
            // Open the file and read all of its contents
            const obj_file = try std.fs.cwd().openFile(file_name, .{ .read = true });
            defer obj_file.close();

            const data = try obj_file.readToEndAlloc(allocator, std.math.maxInt(usize));
            const stat = try obj_file.stat();

            // TODO: check for if the file is larger than 16-1 bytes and add it to string table 
            const name = try mem.concat(allocator, u8, &.{ file_name, "/" });
            defer allocator.free(name);

            // Write the header
            var buf = [_]u8{0} ** @sizeOf(Header);
            _ = try std.fmt.bufPrint(
                &buf,
                "{s: <16}{: <12}{: <6}{: <6}{o: <8}{: <10}`\n",
                .{ name, 0, 0, 0, stat.mode, stat.size },
            );

            const object = ArchivedFile{
                .name = file_name,
                .contents = data,
            };
    
            // Append header and file contents
            try self.headers.append(allocator, @ptrCast(*Header, &buf).*);
            try self.files.append(allocator, object);
        }
    }
}


pub fn parse(self: *Archive, allocator: *Allocator, stderr: anytype) !void {
    const reader = self.file.reader();
    {
        // Is the magic header found at the start of the archive?
        const magic = reader.readBytesNoEof(magic_string.len) catch |err| switch (err) {
            error.EndOfStream => {
                try stderr.print("File too short to be an archive\n", .{});
                return error.NotArchive;
            },
            else => |e| return e,
        };
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

        try self.headers.append(allocator, archive_header);

        // the lifetime of the archive headers will matched that of the parsed files (for now)
        // so we can take a reference to the strings stored there directly!
        var trimmed_archive_name = mem.trim(u8, &(self.headers.items[self.headers.items.len - 1].ar_name), " ");

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
        }

        const parsed_file = ArchivedFile{
            .name = trimmed_archive_name,
            .contents = undefined,
        };

        try self.files.append(allocator, parsed_file);

        try reader.context.seekBy(seek_forward_amount);
    }
}
