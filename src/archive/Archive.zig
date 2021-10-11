const Archive = @This();

const builtin = @import("builtin");
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
symbols: std.ArrayListUnmanaged(Symbol),

// Use it so we can easily lookup files indices when inserting!
// TODO: A trie is probably a lot better here
file_name_to_index: std.StringArrayHashMapUnmanaged(u64),
file_offset_to_index: std.AutoArrayHashMapUnmanaged(u64, u64),

modifiers: Modifiers,

stat: fs.File.Stat,

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
    print_symbols,
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

pub const Modifiers = extern struct {
    // Supress warning for file creation
    create: bool = false,
    // Only insert files with more recent timestamps than archive
    update_only: bool = false,
    use_real_timestamps_and_ids: bool = false,
    verbose: bool = false,
};

pub const Contents = struct {
    bytes: []u8,
    length: u64,
    mode: u64,
    timestamp: u128, // file modified time
    uid: u32,
    gid: u32,

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

pub const Symbol = struct {
    name: []const u8,
    file_offset: u32,
};

pub fn getDefaultArchiveTypeFromHost() ArchiveType {
    // TODO: Set this based on the current platform you are using the tool
    // on!
    return .gnu;
}

pub fn create(
    file: fs.File,
    name: []const u8,
    output_archive_type: ArchiveType,
    modifiers: Modifiers,
) !Archive {
    return Archive{
        .file = file,
        .name = name,
        .inferred_archive_type = .ambiguous,
        .output_archive_type = output_archive_type,
        .files = .{},
        .symbols = .{},
        .file_name_to_index = .{},
        .file_offset_to_index = .{},
        .modifiers = modifiers,
        .stat = try file.stat(),
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
        // none was set) then we need to infer it from the host platform!
        self.output_archive_type = getDefaultArchiveTypeFromHost();
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
                    if (string_table.items.len % 2 != 0)
                        try string_table.append('\n');
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
            .{ &header_names[index], file.contents.timestamp, file.contents.uid, file.contents.gid, file.contents.mode, file.contents.length },
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
        defer file.close();

        // We only need to do this because file stats don't include
        // guid and uid - maybe the right solution is to integrate that into
        // the std so we can call file.stat() on all platforms.
        var gid: u32 = 0;
        var uid: u32 = 0;
        var mtime: i128 = 0;
        var size: u64 = undefined;
        var mode: u64 = undefined;

        // FIXME: Currently windows doesnt support the Stat struct
        if (builtin.os.tag == .windows) {
            const file_stats = try file.stat();
            // Convert timestamp from ns to s
            mtime = file_stats.mtime;
            size = file_stats.size;
            mode = file_stats.mode;
        } else {
            const file_stats = try std.os.fstat(file.handle);

            gid = file_stats.gid;
            uid = file_stats.uid;
            const mtime_full = file_stats.mtime();
            mtime = mtime_full.tv_sec * std.time.ns_per_s + mtime_full.tv_nsec;
            size = @intCast(u64, file_stats.size);
            mode = file_stats.mode;
        }

        if (!self.modifiers.use_real_timestamps_and_ids) {
            gid = 0;
            uid = 0;
            mtime = 0;
        }

        const timestamp = @intCast(u128, @divFloor(mtime, std.time.ns_per_s));

        if (self.modifiers.update_only) {
            // TODO: Write a test that checks for this functionality still working!
            if (self.stat.mtime >= mtime) {
                continue;
            }
        }

        const archived_file = ArchivedFile{
            .name = fs.path.basename(file_name),
            .contents = Contents{
                .bytes = try file.readToEndAlloc(allocator, std.math.maxInt(usize)),
                .length = size,
                .mode = if (builtin.os.tag != .windows) mode & ~@as(u64, std.os.S.IFREG) else 0,
                .timestamp = timestamp,
                .gid = gid,
                .uid = uid,
            },
        };

        // A trie-based datastructure would be better for this!
        const getOrPutResult = try self.file_name_to_index.getOrPut(allocator, archived_file.name);
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
                if (mem.eql(u8, first_line_buffer[0..2], "//"[0..2])) {
                    switch (self.inferred_archive_type) {
                        .ambiguous => self.inferred_archive_type = .gnu,
                        .gnu, .gnuthin, .gnu64 => {},
                        else => {
                            try stderr.print("Came across gnu-style string table in {} archive\n", .{self.inferred_archive_type});
                            return error.NotArchive;
                        },
                    }

                    const string_table_num_bytes_string = first_line_buffer[48..58];
                    const string_table_num_bytes = try fmt.parseInt(u32, mem.trim(u8, string_table_num_bytes_string, " "), 10);

                    string_table_contents = try allocator.alloc(u8, string_table_num_bytes);
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

                    const symbol_table_num_bytes_string = first_line_buffer[48..58];
                    const symbol_table_num_bytes = try fmt.parseInt(u32, mem.trim(u8, symbol_table_num_bytes_string, " "), 10);

                    const num_symbols = try reader.readInt(u32, .Big);

                    var num_bytes_remaining = symbol_table_num_bytes - @sizeOf(u32);

                    const number_array = try allocator.alloc(u32, num_symbols);
                    for (number_array) |_, number_index| {
                        number_array[number_index] = try reader.readInt(u32, .Big);
                    }
                    defer allocator.free(number_array);

                    num_bytes_remaining = num_bytes_remaining - (@sizeOf(u32) * num_symbols);

                    gnu_symbol_table_contents = try allocator.alloc(u8, num_bytes_remaining);

                    // TODO: actually error handle not expected number of bytes being read!
                    _ = try reader.read(gnu_symbol_table_contents);

                    var current_symbol_string = gnu_symbol_table_contents;
                    var current_byte: u32 = 0;
                    while (current_byte < gnu_symbol_table_contents.len) {
                        var symbol_length: u32 = 0;
                        var skip_length: u32 = 0;

                        var found_zero = false;
                        for (current_symbol_string) |byte| {
                            if (found_zero and byte != 0) {
                                break;
                            }

                            current_byte = current_byte + 1;

                            if (byte == 0) {
                                found_zero = true;
                            }

                            skip_length = skip_length + 1;

                            if (!found_zero) {
                                symbol_length = symbol_length + 1;
                            }
                        }

                        const symbol = Symbol{
                            .name = current_symbol_string[0..symbol_length],
                            .file_offset = number_array[self.symbols.items.len],
                        };

                        try self.symbols.append(allocator, symbol);

                        current_symbol_string = current_symbol_string[skip_length..];
                    }

                    starting_seek_pos = starting_seek_pos + first_line_buffer.len + symbol_table_num_bytes;
                } else {
                    try reader.context.seekTo(starting_seek_pos);
                    break;
                }
            }
        }
    }

    var is_first = true;

    while (true) {
        const file_offset = try reader.context.getPos();

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

        // TODO: if modifiers.use_real_timestamps_and_ids is disabled, do we ignore this from existing archive?
        // Check against llvm ar
        const timestamp = try fmt.parseInt(u128, mem.trim(u8, &archive_header.ar_date, " "), 10);
        const uid = try fmt.parseInt(u32, mem.trim(u8, &archive_header.ar_uid, " "), 10);
        const gid = try fmt.parseInt(u32, mem.trim(u8, &archive_header.ar_gid, " "), 10);

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
                    continue;
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
                .mode = try fmt.parseInt(u32, mem.trim(u8, &archive_header.ar_size, " "), 10),
                .timestamp = timestamp,
                .uid = uid,
                .gid = gid,
            },
        };

        if (self.inferred_archive_type == .gnuthin) {
            var thin_file = try std.fs.cwd().openFile(trimmed_archive_name, .{});
            defer thin_file.close();

            try thin_file.reader().readNoEof(parsed_file.contents.bytes);
        } else {
            try reader.readNoEof(parsed_file.contents.bytes);
        }

        try self.file_name_to_index.put(allocator, trimmed_archive_name, self.files.items.len);
        try self.file_offset_to_index.put(allocator, file_offset, self.files.items.len);
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
