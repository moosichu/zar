const Archive = @This();

const std = @import("std");
const fmt = std.fmt;
const fs = std.fs;
const mem = std.mem;
const log = std.log.scoped(.archive);
const format = @import("format.zig");

const Allocator = std.mem.Allocator;

file: fs.File,
name: []const u8,
archive_type: format.ArchiveType,
archive_headers: std.ArrayListUnmanaged(format.ar_hdr),
parsed_files: std.ArrayListUnmanaged(format.ArchivedFile),

pub fn create(
    file: fs.File,
    name: []const u8,
) Archive {
    return Archive{
        .file = file,
        .name = name,
        .archive_type = .ambiguous,
        .archive_headers = .{},
        .parsed_files = .{},
    };
}

pub fn parse(self: *Archive, allocator: *Allocator, stderr: anytype) !void {
    const reader = self.file.reader();
    {
        // Is the magic header found at the start of the archive?
        const magic = reader.readBytesNoEof(format.magic_string.len) catch |err| switch (err) {
            error.EndOfStream => {
                try stderr.print("File too short to be an archive\n", .{});
                return error.NotArchive;
            },
            else => |e| return e,
        };
        if (!mem.eql(u8, &magic, format.magic_string)) {
            try stderr.print("Invalid magic string: expected '{s}', found '{s}'\n", .{ format.magic_string, magic });
            return error.NotArchive;
        }
    }

    // https://www.freebsd.org/cgi/man.cgi?query=ar&sektion=5
    // Process string/symbol tables and/or try to infer archive type!
    var string_table_contents: []u8 = undefined;
    {
        var starting_seek_pos = format.magic_string.len;

        var first_line_buffer: [format.gnu_first_line_buffer_length]u8 = undefined;

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
        const archive_header = reader.readStruct(format.ar_hdr) catch |err| switch (err) {
            error.EndOfStream => break,
            else => |e| return e,
        };

        try self.archive_headers.append(allocator, archive_header);

        // the lifetime of the archive headers will matched that of the parsed files (for now)
        // so we can take a reference to the strings stored there directly!
        var trimmed_archive_name = mem.trim(u8, &(self.archive_headers.items[self.archive_headers.items.len - 1].ar_name), " ");

        // Check against gnu naming properties
        const ends_with_gnu_slash = (trimmed_archive_name[trimmed_archive_name.len - 1] == '/');
        var gnu_offset_value: u32 = 0;
        const starts_with_gnu_offset = trimmed_archive_name[0] == '/';
        if (starts_with_gnu_offset) {
            gnu_offset_value = try fmt.parseInt(u32, trimmed_archive_name[1..trimmed_archive_name.len], 10);
        }

        const must_be_gnu = ends_with_gnu_slash or starts_with_gnu_offset;

        // Check against bsd naming properties
        const starts_with_bsd_name_length = mem.eql(u8, trimmed_archive_name[0..2], format.bsd_name_length_signifier[0..2]);
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

        const parsed_file = format.ArchivedFile{
            .name = trimmed_archive_name,
        };

        try self.parsed_files.append(allocator, parsed_file);

        try reader.context.seekBy(try fmt.parseInt(u32, mem.trim(u8, &archive_header.ar_size, " "), 10));
    }
}
