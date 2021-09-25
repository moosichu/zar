const ArchiveType = enum {
    gnu,
    gnu_64,
    bsd,
    darwin_64, // darwin_32 *is* bsd
    coff, // (windows)
};
