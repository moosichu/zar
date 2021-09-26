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
parsed_files: std.ArrayListUnmanaged(format.ParsedFile),

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

pub fn parse(self: *Archive, allocator: *Allocator) !void {
    const reader = self.file.reader();
    {
        // Is the magic header found at the start of the archive?
        const magic = reader.readBytesNoEof(format.magic_string.len) catch |err| switch (err) {
            error.EndOfStream => {
                log.debug("File too short to be an archive", .{});
                return error.NotArchive;
            },
            else => |e| return e,
        };
        if (!mem.eql(u8, &magic, format.magic_string)) {
            log.debug("Invalid magic string: expected '{s}', found '{s}'", .{ format.magic_string, magic });
            return error.NotArchive;
        }
    }

    // TODO: Identify string table and other cross-platform shenanigans! here
    // https://www.freebsd.org/cgi/man.cgi?query=ar&sektion=5

    _ = allocator;

    while (true) {
        const archive_header = reader.readStruct(format.ar_hdr) catch |err| switch (err) {
            error.EndOfStream => break,
            else => |e| return e,
        };

        try self.archive_headers.append(allocator, archive_header);

        // the lifetime of the archive headers will matched that of the parsed files (for now)
        // so we can take a reference to the string stored there.
        const parsed_file = format.ParsedFile{
            .name = &(self.archive_headers.items[self.archive_headers.items.len - 1].ar_name),
        };

        try self.parsed_files.append(allocator, parsed_file);

        try reader.context.seekBy(try fmt.parseInt(u32, mem.trim(u8, &archive_header.ar_size, " "), 10));
    }

    // TODO: Read and parse the ar_hdr headers!
}
