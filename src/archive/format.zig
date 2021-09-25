const ArchiveType = enum {
    gnu,
    gnu_64,
    bsd,
    darwin_64, // darwin_32 *is* bsd
    coff, // (windows)
};

// All archive files start with this magic string
const magic_string = "!<arch>";

// The format (unparsed) of the archive per-file header
// NOTE: The reality is more complex than this as different mechanisms
// have been devised for storing the names of files which exceed 16 byte!
const ar_header = struct {
    name: [16]u8,
    date: [12]u8,
    uid: [6]u8,
    gid: [6]u8,
    mode: [8]u8,
    size: [10]u8,
    fmag: [2]u8,
};
