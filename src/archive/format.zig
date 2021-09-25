pub const ArchiveType = enum {
    ambiguous,
    gnu,
    gnu_64,
    bsd,
    darwin_64, // darwin_32 *is* bsd
    coff, // (windows)
};

// All archive files start with this magic string
pub const magic_string = "!<arch>\n";

// The format (unparsed) of the archive per-file header
// NOTE: The reality is more complex than this as different mechanisms
// have been devised for storing the names of files which exceed 16 byte!
pub const ar_hdr = extern struct {
    ar_name: [16]u8,
    ar_date: [12]u8,
    ar_uid: [6]u8,
    ar_gid: [6]u8,
    ar_mode: [8]u8,
    ar_size: [10]u8,
    ar_fmag: [2]u8,
};

pub const ar_processed = struct {
    name: []const u8,
};
