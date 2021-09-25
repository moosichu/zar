const Archive = @This();

const std = @import("std");
const fs = std.fs;
const mem = std.mem;
const log = std.log.scoped(.archive);
const format = @import("format.zig");

const Allocator = std.mem.Allocator;

file: fs.File,
name: []const u8,
archive_type: format.ArchiveType,
ar_hdr: format.ar_hdr,

pub fn parse(self: *Archive, allocator: *Allocator) !format.ArchiveType {
    const reader = self.file.reader();
    {
        // Is the magic header found at the start of the archive?
        const magic = reader.readBytesNoEof(format.magic_string.len) catch |err| switch (err) {
            error.EndOfStream => {
                log.debug("File to short to be an archive");
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

    // TODO: Read and parse the ar_hdr headers!
}
