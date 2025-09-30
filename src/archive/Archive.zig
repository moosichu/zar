const Archive = @This();

const builtin = @import("builtin");
const std = @import("std");
const build_options = @import("build_options");
const trace = @import("../tracy.zig").trace;
const traceNamed = @import("../tracy.zig").traceNamed;
const MachO = @import("zld/Zld.zig").MachO;
const macho = std.macho;
const Coff = @import("zld/Zld.zig").Coff;
const ZarIo = @import("ZarIo.zig");

// We don't have any kind of bitcode parsing support at the moment, but we need
// to report dealing with bitcode files as an error. So embed magic like this
// matching the format of the actual zld package for now.
const Bitcode = struct {
    const magic = "BC\xC0\xDE";
};
const coff = std.coff;

const Allocator = std.mem.Allocator;
const allocator_limit = 10000000;

arena: std.heap.ArenaAllocator,
gpa: Allocator,
zar_io: *const ZarIo,
file: std.fs.File,
name: []const u8,
created: bool,

// We need to differentiate between inferred and output archive type, as other ar
// programs "just handle" any valid archive for parsing, regarldess of what a
// user has specified - the user specification should only matter for writing
// archives.
inferred_archive_type: ArchiveType,
output_archive_type: ArchiveType,

files: std.ArrayListUnmanaged(ArchivedFile),
symbols: std.ArrayListUnmanaged(Symbol),

// Use it so we can easily lookup files indices when inserting!
file_name_to_index: std.StringArrayHashMapUnmanaged(u64),

modifiers: Modifiers,

stat: std.fs.File.Stat,

pub const ArchiveType = enum {
    ambiguous,
    gnu,
    gnuthin,
    gnu64,
    bsd,
    darwin, // *mostly* like BSD, with some differences in limited contexts when writing for determinism reasons
    darwin64,
    coff, // (windows)

    pub fn getAlignment(self: ArchiveType) u32 {
        // See: https://github.com/llvm-mirror/llvm/blob/2c4ca6832fa6b306ee6a7010bfb80a3f2596f824/lib/Object/ArchiveWriter.cpp#L311
        return switch (self) {
            .ambiguous => unreachable,
            else => if (self.isBsdLike()) @as(u32, 8) else @as(u32, 2),
        };
    }

    pub fn getFileAlignment(self: ArchiveType) u32 {
        // In this context, bsd like archives get 2 byte alignment but darwin
        // stick to 8 byte alignment
        return switch (self) {
            .ambiguous => unreachable,
            else => if (self.isDarwin()) @as(u32, 8) else @as(u32, 2),
        };
    }

    pub fn isBsdLike(self: ArchiveType) bool {
        return switch (self) {
            .bsd, .darwin, .darwin64 => true,
            else => false,
        };
    }

    pub fn isDarwin(self: ArchiveType) bool {
        return switch (self) {
            .darwin, .darwin64 => true,
            else => false,
        };
    }
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
    undefined,
};

// We seperate errors into two classes, "handled" and "unhandled".
// The reason for this is that "handled" errors log appropriate error
// messages at the point they are created, whereas unhandled errors do
// not so the caller will need to print appropriate error messages
// themselves (if needed at all).
pub const UnhandledError = CreateError || ParseError || InsertError || DeleteError || FlushError || CriticalError;
pub const HandledError = HandledIoError || error{
    UnknownFormat,
};

// We can set this to true just to make Handled errors are actually handled at
// comptime!
pub const test_errors_handled = if (@hasField(build_options, "test_errors_handled")) build_options.test_errors_handled else false;

pub const HandledIoError = if (test_errors_handled) error{Handled} else IoError;

pub const CreateError = error{};

pub const ParseError = error{
    NotArchive,
    MalformedArchive,
    Overflow,
    InvalidCharacter,
};

pub const InsertError = error{};
pub const DeleteError = error{};
pub const FlushError = error{};

pub const CriticalError = error{
    OutOfMemory,
    ReadFailed,
    TODO,
};

pub const IoError = std.fs.File.OpenError || std.fs.File.ReadError || std.fs.File.SeekError || std.fs.File.StatError || std.fs.File.WriteError || std.io.Writer.Error || std.fs.File.Writer.EndError;

// All archive files start with this magic string
pub const magic_string = "!<arch>\n";
pub const magic_thin = "!<thin>\n";

// GNU constants
pub const gnu_first_line_buffer_length = 60;
pub const gnu_string_table_seek_pos = magic_string.len + gnu_first_line_buffer_length;

// BSD constants
pub const bsd_name_length_signifier = "#1/";
pub const bsd_symdef_magic = "__.SYMDEF";
pub const bsd_symdef_64_magic = "__.SYMDEF_64";
pub const bsd_symdef_sorted_magic = "__.SYMDEF SORTED";

pub const bsd_symdef_longest_magic = @max(@max(bsd_symdef_magic.len, bsd_symdef_64_magic.len), bsd_symdef_sorted_magic.len);

pub const invalid_file_index = std.math.maxInt(u64);

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

    pub const format_string = "{s: <16}{: <12}{: <6}{: <6}{: <8}{: <10}`\n";
};

pub const ExplicitBooleanSetting = enum { ambiguous, set_true, set_false };

pub const MoveSetting = union(enum) {
    end,
    before: ?[]const u8,
    after: ?[]const u8,
};

pub const Modifiers = struct {
    // Supress warning for file creation
    create: bool = false,
    // Only insert files with more recent timestamps than archive
    update_only: bool = false,
    use_real_timestamps_and_ids: bool = false,
    build_symbol_table: bool = true,
    sort_symbol_table: ExplicitBooleanSetting = .ambiguous,
    verbose: bool = false,
    move_setting: MoveSetting = .end,
    show_version: bool = false,
    help: bool = false,
    quick_append_members: bool = false,
    instance_to_delete: u32 = 1,
    preserve_original_dates: bool = false,
    use_full_paths_when_matching: bool = false,
    thin_archives: bool = false,
};

pub const Contents = struct {
    bytes: []align(8) u8,
    length: u64,
    mode: u64,
    timestamp: u128, // file modified time
    uid: u32,
    gid: u32,

    // TODO(#75): deallocation

    pub fn write(self: *const Contents, out_stream: *std.Io.Writer, stderr: anytype) !void {
        try out_stream.writeAll(self.bytes);
        _ = stderr;
    }
};

// An internal representation of files being archived
pub const ArchivedFile = struct {
    name: []const u8,
    contents: Contents,

    const Self = @This();
};

pub const Symbol = struct {
    name: []const u8,
    file_index: u64,
};

// TODO: BSD symbol table interpretation is architecture dependent,
// is there a way we can interpret this? (will be needed for
// cross-compilation etc. could possibly take it as a spec?)
// Hardcoding this information here is a bit of a hacky
// workaround in the short term - even though it is part of
// the spec. Maybe look into how llvm ar deals with this?
const IntType = i32;

// This name is confusing because ranlib is also the name of the ranlib
// program - but also what this struct is traditionally called within archives.
// :/
// type of ranlib used depends on the archive storage format
fn Ranlib(comptime storage: type) type {
    return extern struct {
        ran_strx: storage, // offset of symbol name in symbol table
        ran_off: storage, // offset of file header in archive
    };
}

const ErrorContext = enum {
    accessing,
    creating,
    opening,
    reading,
    seeking,
    stat,
    writing,
};

fn calculateLogicPosition(file_writer: std.fs.File.Writer) usize {
    return file_writer.pos + file_writer.interface.end;
}

// This exists because seekBy is currently bugged. See: https://github.com/ziglang/zig/issues/25087
fn seekByTempFix(file_reader: *std.fs.File.Reader, offset: i64) !void {
    try file_reader.seekTo(@intCast(@as(i64, @intCast(file_reader.logicalPos())) + offset));
}

// deprecated
// this is a poor way of dealing with these errors... it's too generic and really we should be context-sensitively dealing with these
// so we can report them helpfully to the users
pub fn printFileIoError(zar_io: *const ZarIo, comptime context: ErrorContext, file_name: []const u8, err: IoError) HandledIoError {
    const context_str = @tagName(context);

    switch (err) {
        error.AccessDenied => zar_io.printError(context_str ++ " '{s}', access denied.", .{file_name}),
        error.FileNotFound => zar_io.printError(context_str ++ " '{s}', file not found.", .{file_name}),
        else => zar_io.printError(context_str ++ " '{s}'.", .{file_name}),
    }
    if (test_errors_handled) return error.Handled;
    return err;
}

// The weird return type is so that we can distinguish between handled and unhandled IO errors,
// i.e. if test_errors_handled is set to true, and raw calls to io operations will return in a compile failure
pub fn handleFileIoError(zar_io: *const ZarIo, comptime context: ErrorContext, file_name: []const u8, fallible: anytype) HandledIoError!@typeInfo(@TypeOf(fallible)).error_union.payload {
    const unwrapped_result = fallible catch |err| {
        return printFileIoError(zar_io, context, file_name, err);
    };
    return unwrapped_result;
}

// These are the defaults llvm ar uses (excepting windows)
// https://github.com/llvm-mirror/llvm/blob/master/tools/llvm-ar/llvm-ar.cpp
pub fn getDefaultArchiveTypeFromHost() ArchiveType {
    if (build_options.mimmick_broken_cross_compiled_llvm_ar_behaviour) {
        return .gnu;
    }
    if (builtin.os.tag.isDarwin()) return .darwin;
    return .gnu;
}

pub fn init(
    allocator: std.mem.Allocator,
    zar_io: *const ZarIo,
    file: std.fs.File,
    name: []const u8,
    output_archive_type: ArchiveType,
    modifiers: Modifiers,
    created: bool,
) (CreateError || HandledIoError)!Archive {
    return Archive{
        .gpa = allocator,
        .arena = std.heap.ArenaAllocator.init(allocator),
        .zar_io = zar_io,
        .file = file,
        .name = name,
        .inferred_archive_type = .ambiguous,
        .output_archive_type = output_archive_type,
        .files = .{},
        .symbols = .{},
        .file_name_to_index = .{},
        .modifiers = modifiers,
        .stat = try handleFileIoError(zar_io, .stat, name, file.stat()),
        .created = created,
    };
}

const SymbolStringTableAndOffsets = struct {
    unpadded_symbol_table_length: i32,
    symbol_table: []u8,
    symbol_offsets: []i32,

    pub fn deinit(self: *const SymbolStringTableAndOffsets, allocator: Allocator) void {
        allocator.free(self.symbol_offsets);
        allocator.free(self.symbol_table);
    }
};

pub fn buildSymbolTable(
    self: *Archive,
    allocator: Allocator,
) !SymbolStringTableAndOffsets {
    const tracy = trace(@src());
    defer tracy.end();
    var symbol_table_size: usize = 0;
    const symbol_offsets = try allocator.alloc(i32, self.symbols.items.len);
    errdefer allocator.free(symbol_offsets);

    for (self.symbols.items, 0..) |symbol, idx| {
        symbol_offsets[idx] = @intCast(symbol_table_size);
        symbol_table_size += symbol.name.len + 1;
    }

    const unpadded_symbol_table_length = symbol_table_size;

    while (symbol_table_size % self.output_archive_type.getAlignment() != 0) {
        symbol_table_size += 1;
    }

    const symbol_table = try allocator.alloc(u8, symbol_table_size);
    symbol_table_size = 0;

    for (self.symbols.items) |symbol| {
        @memcpy(symbol_table[symbol_table_size..(symbol.name.len + symbol_table_size)], symbol.name);
        symbol_table[symbol_table_size + symbol.name.len] = 0;
        symbol_table_size += symbol.name.len + 1;
    }

    while (symbol_table_size % self.output_archive_type.getAlignment() != 0) {
        symbol_table[symbol_table_size] = 0;
        symbol_table_size += 1;
    }

    const result: SymbolStringTableAndOffsets = .{
        .unpadded_symbol_table_length = @as(i32, @intCast(unpadded_symbol_table_length)),
        .symbol_table = symbol_table,
        .symbol_offsets = symbol_offsets,
    };
    return result;
}

fn calculatePadding(self: *Archive, file_pos: usize) usize {
    var padding = file_pos % self.output_archive_type.getAlignment();
    padding = (self.output_archive_type.getAlignment() - padding) % self.output_archive_type.getAlignment();
    return padding;
}

// TODO: This needs to be integrated into the workflow
// used for parsing. (use same error handling workflow etc.)
/// Use same naming scheme for objects (as found elsewhere in the file).
pub fn flush(self: *Archive) (FlushError || HandledIoError || CriticalError)!void {
    const allocator = self.arena.allocator();
    const tracy = trace(@src());
    defer tracy.end();
    if (self.output_archive_type == .ambiguous) {
        // if output archive type is still ambiguous (none was inferred, and
        // none was set) then we need to infer it from the host platform!
        self.output_archive_type = getDefaultArchiveTypeFromHost();
    }

    // Overwrite all contents
    try handleFileIoError(self.zar_io, .seeking, self.name, self.file.seekTo(0));

    // TrackingBufferedWriter no longer appears to be necessary,
    // as std.fs.File.Writer has a "pos" field and a buffer now.
    var writer_buf: [4096]u8 = undefined;
    var file_writer = self.file.writer(&writer_buf);
    const writer = &file_writer.interface;
    defer writer.flush() catch {};

    try handleFileIoError(self.zar_io, .writing, self.name, writer.writeAll(if (self.output_archive_type == .gnuthin) magic_thin else magic_string));

    const header_names = try allocator.alloc([16]u8, self.files.items.len);

    const SortContext = struct {
        files: std.ArrayListUnmanaged(ArchivedFile),
    };
    const SortFn = struct {
        fn sorter(context: *const SortContext, x: Symbol, y: Symbol) bool {
            const x_file_name = context.files.items[x.file_index].name;
            const y_file_name = context.files.items[y.file_index].name;
            // we only sort symbol names internally within file, but maintain
            // the order within which they are added.
            if (x.file_index < y.file_index) {
                return true;
            } else if (x.file_index > y.file_index) {
                return false;
            }
            const order = std.mem.order(u8, x_file_name, y_file_name);
            if (order == .eq) {
                return std.mem.lessThan(u8, x.name, y.name);
            }
            return order == .lt;
        }
    };

    // Sort the symbols
    const sort_symbol_table = switch (self.modifiers.sort_symbol_table) {
        .ambiguous => self.output_archive_type.isDarwin(),
        .set_true => true,
        .set_false => false,
    };

    const sort_context: SortContext = .{ .files = self.files };
    if (sort_symbol_table) {
        const tracy_scope = traceNamed(@src(), "Sort Symbol Table");
        defer tracy_scope.end();
        std.sort.block(Symbol, self.symbols.items, &sort_context, SortFn.sorter);
    }

    // Calculate the offset of file independent of string table and symbol table itself.
    // It is basically magic size + file size from position 0

    const relative_file_offsets = try allocator.alloc(i32, self.files.items.len);
    defer allocator.free(relative_file_offsets);

    {
        var offset: u32 = 0;
        for (self.files.items, 0..) |file, idx| {
            relative_file_offsets[idx] = @as(i32, @intCast(offset));
            offset += @as(u32, @intCast(@sizeOf(Header) + file.contents.bytes.len));

            // BSD also keeps the name in its data section
            if (self.output_archive_type.isBsdLike()) {
                offset += @as(u32, @intCast(file.name.len));

                // Add padding
                while (offset % self.output_archive_type.getAlignment() != 0) {
                    offset += 1;
                }
            }
        }
    }

    // Set the mtime of symbol table to now seconds in non-deterministic mode
    const symtab_time: u64 = (if (self.modifiers.use_real_timestamps_and_ids) @as(u64, @intCast(std.time.milliTimestamp())) else 0) / 1000;

    switch (self.output_archive_type) {
        .gnu, .gnuthin, .gnu64 => {
            // GNU format: Create string table
            var string_table = std.array_list.Managed(u8).init(allocator);
            defer string_table.deinit();

            // Generate the complete string table
            for (self.files.items, 0..) |file, index| {
                const is_the_name_allowed = (file.name.len < 16) and (self.output_archive_type != .gnuthin);

                // If the file is small enough to fit in header, then just write it there
                // Otherwise, add it to string table and add a reference to its location
                const name = if (is_the_name_allowed) try std.mem.concat(allocator, u8, &.{ file.name, "/" }) else try std.fmt.allocPrint(allocator, "/{}", .{blk: {
                    // Get the position of the file in string table
                    const pos = string_table.items.len;

                    // Now add the file name to string table
                    try string_table.appendSlice(file.name);
                    try string_table.appendSlice("/\n");

                    break :blk pos;
                }});
                defer allocator.free(name);

                // Edit the header
                _ = std.fmt.bufPrint(&(header_names[index]), "{s: <16}", .{name}) catch |e| switch (e) {
                    // Should be unreachable as the buffer should already definetely be large enough...
                    error.NoSpaceLeft => unreachable,
                };
            }

            // Write the symbol table itself
            if (self.modifiers.build_symbol_table and self.symbols.items.len != 0) {
                const tracy_scope = traceNamed(@src(), "Write Symbol Table");
                defer tracy_scope.end();
                const symbol_string_table_and_offsets = try self.buildSymbolTable(allocator);
                defer symbol_string_table_and_offsets.deinit(allocator);

                const symbol_table = symbol_string_table_and_offsets.symbol_table;

                const format = self.output_archive_type;
                const int_size: usize = if (format == .gnu64) @sizeOf(u64) else @sizeOf(u32);

                const symbol_table_size =
                    symbol_table.len + // The string of symbols
                    (self.symbols.items.len * int_size) + // Size of all symbol offsets
                    int_size; // Value denoting the length of symbol table

                const magic: []const u8 = if (format == .gnu64) "/SYM64/" else "/";

                try handleFileIoError(self.zar_io, .writing, self.name, writer.print(Header.format_string, .{ magic, symtab_time, 0, 0, 0, symbol_table_size }));

                {
                    const tracy_scope_inner = traceNamed(@src(), "Write Symbol Count");
                    defer tracy_scope_inner.end();
                    if (format == .gnu64) {
                        try handleFileIoError(self.zar_io, .writing, self.name, writer.writeInt(
                            u64,
                            @as(u64, @intCast(self.symbols.items.len)),
                            .big,
                        ));
                    } else {
                        try handleFileIoError(self.zar_io, .writing, self.name, writer.writeInt(
                            u32,
                            @as(u32, @intCast(self.symbols.items.len)),
                            .big,
                        ));
                    }
                }

                // magic_string.len == magic_thin.len, so its not a problem
                var offset_to_files = magic_string.len;
                // Size of symbol table itself
                offset_to_files += @sizeOf(Header) + symbol_table_size;

                // Add padding
                while (offset_to_files % self.output_archive_type.getAlignment() != 0) {
                    offset_to_files += 1;
                }

                // Size of string table
                if (string_table.items.len != 0) {
                    offset_to_files += @sizeOf(Header) + string_table.items.len;
                }

                // Add further padding
                while (offset_to_files % self.output_archive_type.getAlignment() != 0) {
                    offset_to_files += 1;
                }

                {
                    const tracy_scope_inner = traceNamed(@src(), "Write Symbol File Offsets");
                    defer tracy_scope_inner.end();

                    for (self.symbols.items) |symbol| {
                        if (format == .gnu64) {
                            try handleFileIoError(self.zar_io, .writing, self.name, writer.writeInt(
                                i64,
                                relative_file_offsets[symbol.file_index] + @as(i64, @intCast(offset_to_files)),
                                .big,
                            ));
                        } else {
                            try handleFileIoError(self.zar_io, .writing, self.name, writer.writeInt(
                                i32,
                                relative_file_offsets[symbol.file_index] + @as(i32, @intCast(offset_to_files)),
                                .big,
                            ));
                        }
                    }
                }

                try handleFileIoError(self.zar_io, .writing, self.name, writer.writeAll(symbol_table));
            }

            // Write the string table itself
            {
                const tracy_scope = traceNamed(@src(), "Write String Table");
                defer tracy_scope.end();
                if (string_table.items.len != 0) {
                    while (string_table.items.len % self.output_archive_type.getAlignment() != 0) {
                        try string_table.append('\n');
                    }
                    try handleFileIoError(self.zar_io, .writing, self.name, writer.print("//{s}{: <10}`\n{s}", .{ " " ** 46, string_table.items.len, string_table.items }));
                }
            }
        },
        .bsd, .darwin, .darwin64 => {
            // BSD format: Write the symbol table
            // In darwin if symbol table writing is enabled the expected behaviour
            // is that we write an empty symbol table!
            // However - there is one exception to this, which is that llvm ar
            // does not generate the symbol table if we haven't just created
            // the archive *and* we aren't running from a darwin host.
            // WHAT ?!
            const write_symbol_table = write_symbol_table: {
                var result = self.modifiers.build_symbol_table;
                if (getDefaultArchiveTypeFromHost() != .darwin and !self.created) {
                    result = result and self.symbols.items.len != 0;
                }
                break :write_symbol_table result;
            };
            if (write_symbol_table) {
                const tracy_scope = traceNamed(@src(), "Write Symbol Table");
                defer tracy_scope.end();
                const symbol_string_table_and_offsets = try self.buildSymbolTable(allocator);
                defer symbol_string_table_and_offsets.deinit(allocator);

                const symbol_table = symbol_string_table_and_offsets.symbol_table;

                const format = self.output_archive_type;
                const int_size: usize = if (format == .darwin64) @sizeOf(u64) else @sizeOf(u32);

                const num_ranlib_bytes = self.symbols.items.len * @sizeOf(Ranlib(IntType));

                const symbol_table_size =
                    bsd_symdef_64_magic.len + // Length of name
                    int_size + // Int describing the size of ranlib
                    num_ranlib_bytes + // Size of ranlib structs
                    int_size + // Int describing size of symbol table's strings
                    symbol_table.len; // The lengths of strings themselves

                try handleFileIoError(self.zar_io, .writing, self.name, writer.print(Header.format_string, .{ "#1/12", symtab_time, 0, 0, 0, symbol_table_size }));

                const endian = builtin.cpu.arch.endian();

                if (format == .darwin64) {
                    try handleFileIoError(self.zar_io, .writing, self.name, writer.writeAll(bsd_symdef_64_magic));
                    try handleFileIoError(self.zar_io, .writing, self.name, writer.writeInt(u64, @as(u64, @intCast(num_ranlib_bytes)), endian));
                } else {
                    try handleFileIoError(self.zar_io, .writing, self.name, writer.writeAll(bsd_symdef_magic ++ "\x00\x00\x00"));
                    try handleFileIoError(self.zar_io, .writing, self.name, writer.writeInt(u32, @as(u32, @intCast(num_ranlib_bytes)), endian));
                }

                const ranlibs = try allocator.alloc(Ranlib(IntType), self.symbols.items.len);
                defer allocator.free(ranlibs);

                var offset_to_files: usize = magic_string.len + @sizeOf(Header) + symbol_table_size;

                // Add padding
                while (offset_to_files % self.output_archive_type.getAlignment() != 0) {
                    offset_to_files += 1;
                }

                for (self.symbols.items, 0..) |symbol, idx| {
                    ranlibs[idx].ran_strx = symbol_string_table_and_offsets.symbol_offsets[idx];
                    ranlibs[idx].ran_off = relative_file_offsets[symbol.file_index] + @as(i32, @intCast(offset_to_files));
                }

                try handleFileIoError(self.zar_io, .writing, self.name, writer.writeAll(std.mem.sliceAsBytes(ranlibs)));

                if (format == .darwin64) {
                    try handleFileIoError(self.zar_io, .writing, self.name, writer.writeInt(u64, @as(u64, @intCast(symbol_string_table_and_offsets.unpadded_symbol_table_length)), endian));
                } else {
                    try handleFileIoError(self.zar_io, .writing, self.name, writer.writeInt(u32, @as(u32, @intCast(symbol_string_table_and_offsets.unpadded_symbol_table_length)), endian));
                }

                try handleFileIoError(self.zar_io, .writing, self.name, writer.writeAll(symbol_table));
            }
        },
        // This needs to be able to tell whatsupp.
        else => unreachable,
    }

    // Write the files

    const tracy_scope = traceNamed(@src(), "Write Files To Archive");
    defer tracy_scope.end();
    for (self.files.items, 0..) |file, index| {
        var header_buffer: [@sizeOf(Header)]u8 = undefined;

        const file_length = file_length_calculation: {
            if (!self.output_archive_type.isBsdLike()) {
                break :file_length_calculation file.contents.length;
            } else {
                const padding = self.calculatePadding(calculateLogicPosition(file_writer) + header_buffer.len + file.name.len);

                // BSD format: Just write the length of the name in header
                _ = std.fmt.bufPrint(&(header_names[index]), "#1/{: <13}", .{file.name.len + padding}) catch |e| switch (e) {
                    // Should be unreachable as the buffer should already definetely be large enough...
                    error.NoSpaceLeft => unreachable,
                };
                if (self.output_archive_type.isDarwin()) {
                    var file_padding = file.contents.length % self.output_archive_type.getFileAlignment();
                    file_padding = (self.output_archive_type.getFileAlignment() - file_padding) % self.output_archive_type.getFileAlignment();
                    break :file_length_calculation file.contents.length + file.name.len + padding + file_padding;
                } else {
                    break :file_length_calculation file.contents.length + file.name.len + padding;
                }
            }
        };

        _ = std.fmt.bufPrint(
            &header_buffer,
            Header.format_string,
            .{ &header_names[index], file.contents.timestamp, file.contents.uid, file.contents.gid, file.contents.mode, file_length },
        ) catch |e| switch (e) {
            // Should be unreachable as the buffer should already definetely be large enough...
            error.NoSpaceLeft => unreachable,
        };

        // TODO: handle errors
        _ = try handleFileIoError(self.zar_io, .writing, self.name, writer.write(&header_buffer));

        // Write the name of the file in the data section
        if (self.output_archive_type.isBsdLike()) {
            try handleFileIoError(self.zar_io, .writing, self.name, writer.writeAll(file.name));
            try handleFileIoError(self.zar_io, .writing, self.name, writer.splatByteAll(0, self.calculatePadding(calculateLogicPosition(file_writer))));
        }

        if (self.output_archive_type != .gnuthin) {
            try handleFileIoError(self.zar_io, .writing, self.name, file.contents.write(writer, null));
            try handleFileIoError(self.zar_io, .writing, self.name, writer.splatByteAll('\n', self.calculatePadding(calculateLogicPosition(file_writer))));
        }
    }

    try handleFileIoError(self.zar_io, .writing, self.name, file_writer.end());
}

pub fn deinit(self: *Archive) void {
    self.arena.deinit();
}

pub fn deleteFiles(self: *Archive, file_names: []const []const u8) (DeleteError || HandledIoError || CriticalError)!void {
    const tracy = trace(@src());
    defer tracy.end();
    // For the list of given file names, find the entry in self.files
    // and remove it from self.files.
    for (file_names) |file_name| {
        for (self.files.items, 0..) |file, index| {
            if (std.mem.eql(u8, file.name, file_name)) {
                // Remove all the symbols associated with the file
                // when file is deleted
                var idx: usize = 0;
                while (idx < self.symbols.items.len) {
                    const sym = self.symbols.items[idx];
                    if (sym.file_index == index) {
                        _ = self.symbols.orderedRemove(idx);
                        continue;
                    }
                    idx += 1;
                }

                // Reset the index for all future symbols
                for (self.symbols.items) |*sym| {
                    if (sym.file_index > index) {
                        sym.file_index -= 1;
                    }
                }

                _ = self.files.orderedRemove(index);
                break;
            }
        }
    }
}

pub fn moveFiles(self: *Archive, file_names: []const []const u8) !void {
    switch (self.modifiers.move_setting) {
        .end => {
            // TODO: find files, move them, deal with all boundary cases!
            _ = file_names;
        },
        .before, .after => {
            // TODO: bounds check!
            // const relpos = file_names[0];
            // const other_files = file_names[1..file_names.len];
        },
    }
    self.zar_io.printError("Move operation still needs to be implemented!\n", .{});
    return error.TODO;
}

pub fn extract(self: *Archive, file_names: []const []const u8) !void {
    if (self.inferred_archive_type == .gnuthin) {
        // TODO: better error
        return error.ExtractingFromThin;
    }

    for (self.files.items) |archived_file| {
        for (file_names) |file_name| {
            if (std.mem.eql(u8, archived_file.name, file_name)) {
                const file = try self.zar_io.cwd.createFile(archived_file.name, .{});
                defer file.close();

                try file.writeAll(archived_file.contents.bytes);
                break;
            }
        }
    }
}

pub fn addToSymbolTable(self: *Archive, allocator: Allocator, archived_file: *const ArchivedFile, file_index: usize) (CriticalError || HandledIoError)!void {
    // TODO: make this read directly from the file contents buffer!
    const magic = archived_file.contents.bytes[0..4];

    blk: {
        // TODO: Load object from memory (upstream zld)
        // TODO(TRC):Now this should assert that the magic number is what we expect it to be
        // based on the parsed archive type! Not inferring what we should do based on it.
        // switch(self.output_archive_type)
        // {

        // }
        if (std.mem.eql(u8, magic[0..std.elf.MAGIC.len], std.elf.MAGIC)) {
            if (self.output_archive_type == .ambiguous) {
                // TODO: double check that this is the correct inference
                self.output_archive_type = .gnu;
            }
            var reader = std.io.Reader.fixed(archived_file.contents.bytes);
            const header = std.elf.Header.read(&reader) catch {
                return error.TODO;
            };
            var section_headers_iterator = header.iterateSectionHeadersBuffer(archived_file.contents.bytes);
            {
                var shdr_opt = section_headers_iterator.next() catch return error.TODO;
                while (shdr_opt) |shdr| {
                    defer shdr_opt = section_headers_iterator.next() catch null;
                    const contents = contents: {
                        var section_headers_iterator_nested = header.iterateSectionHeadersBuffer(archived_file.contents.bytes);

                        var section_counter: usize = 0;
                        while (section_counter < shdr.sh_link) : (section_counter += 1) {
                            _ = section_headers_iterator_nested.next() catch {
                                return error.TODO;
                            };
                        }
                        const shdr_nested = (section_headers_iterator_nested.next() catch return error.TODO).?;
                        break :contents archived_file.contents.bytes[shdr_nested.sh_offset..][0..shdr_nested.sh_size];
                    };
                    switch (shdr.sh_type) {
                        std.elf.SHT_SYMTAB => {
                            const nsyms = @divExact(shdr.sh_size, @sizeOf(std.elf.Elf64_Sym));
                            const syms = @as(
                                [*]const std.elf.Elf64_Sym,
                                @ptrCast(@alignCast(&archived_file.contents.bytes[shdr.sh_offset])),
                            )[0..nsyms];
                            for (syms) |sym| {
                                switch (sym.st_info >> 4) {
                                    std.elf.STB_WEAK, std.elf.STB_GLOBAL => {
                                        if (!(std.elf.SHN_LORESERVE <= sym.st_shndx and sym.st_shndx < std.elf.SHN_HIRESERVE and sym.st_shndx == std.elf.SHN_UNDEF) or
                                            ((sym.st_shndx == std.elf.SHN_UNDEF) and (sym.st_value != 0)))
                                        {
                                            const string = std.mem.sliceTo(@as([*:0]const u8, @ptrCast(contents.ptr + sym.st_name)), 0);
                                            const symbol = Symbol{
                                                .name = try allocator.dupe(u8, string),
                                                .file_index = file_index,
                                            };
                                            try self.symbols.append(allocator, symbol);
                                        }
                                    },
                                    else => {},
                                }
                            }
                        },
                        else => {},
                    }
                }
            }
        } else if (std.mem.eql(u8, magic[0..Bitcode.magic.len], Bitcode.magic)) {
            self.zar_io.printError("Zig ar does not currently support bitcode files, so no symbol table will be constructed for {s}.", .{archived_file.name});
            break :blk;
            // std.bitc

            // var bitcode_file = Bitcode{ .file = file, .name = archived_file.name };
            // defer bitcode_file.deinit(allocator);

            // // TODO: Do not use builtin.target like this, be more flexible!
            // bitcode_file.parse(allocator, builtin.target) catch |err| switch (err) {
            //     error.NotObject => break :blk,
            //     else => |e| return e,
            //};
        } else {
            // TODO(TRC):Now this should assert that the magic number is what we expect it to be
            // based on the parsed archive type! Not inferring what we should do based on it.
            // TODO: Should be based on target cpu arch!
            const magic_num = std.mem.readInt(u32, magic[0..], builtin.cpu.arch.endian());

            if (magic_num == macho.MH_MAGIC or magic_num == macho.MH_MAGIC_64) {
                if (self.output_archive_type == .ambiguous) {
                    self.output_archive_type = .darwin;
                }
                // As we are just extracting the symbols... we do not care about the mtime here.
                const mtime: u64 = 0;
                var macho_file = MachO.Object{ .name = archived_file.name, .mtime = mtime, .contents = archived_file.contents.bytes };
                defer macho_file.deinit(self.gpa);

                // TODO: Should be based on target cpu arch!
                macho_file.parse(self.gpa, builtin.cpu.arch) catch |err| switch (err) {
                    error.NotObject => break :blk,
                    error.OutOfMemory => return error.OutOfMemory,
                    error.UnsupportedCpuArchitecture, error.EndOfStream => return error.TODO,
                };

                if (macho_file.in_symtab) |in_symtab| {
                    for (in_symtab, 0..) |_, sym_index| {
                        if (macho_file.getSourceSymbol(@as(u32, @intCast(sym_index)))) |sym| {
                            if (sym.ext() and (sym.sect() or sym.tentative())) {
                                const symbol = Symbol{
                                    .name = try allocator.dupe(u8, macho_file.getSymbolName(@as(u32, @intCast(sym_index)))),
                                    .file_index = file_index,
                                };

                                try self.symbols.append(allocator, symbol);
                            }
                        }
                    }
                }
            } else if (false) {
                // TODO: Figure out the condition under which a file is a coff
                // file. This was originally just an else clause - but a file
                // might not contain any symbols!
                unreachable;
                // var coff_file = Coff.Object{ .file = file, .name = archived_file.name };
                // defer coff_file.deinit(allocator);

                // coff_file.parse(allocator, builtin.target) catch |err| return err;

                // for (coff_file.symtab.items) |sym| {
                //     if (sym.storage_class == Coff.Object.IMAGE_SYM_CLASS_EXTERNAL) {
                //         const symbol = Symbol{
                //             .name = try allocator.dupe(u8, sym.getName(&coff_file)),
                //             .file_index = file_index,
                //         };
                //         try self.symbols.append(allocator, symbol);
                //     }
                // }
            }
        }
    }
}

pub fn insertFiles(self: *Archive, file_names: []const []const u8) (InsertError || HandledIoError || CriticalError)!void {
    const allocator = self.arena.allocator();
    const tracy = trace(@src());
    defer tracy.end();

    // TODO: distribute this across n jobs in different chunks?
    for (file_names) |file_name| {
        // Open the file and read all of its contents
        const file = try handleFileIoError(self.zar_io, .opening, file_name, self.zar_io.cwd.openFile(file_name, .{ .mode = .read_only }));
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
            const file_stats = try handleFileIoError(self.zar_io, .stat, file_name, file.stat());
            // Convert timestamp from ns to s
            mtime = file_stats.mtime;
            size = file_stats.size;
            mode = file_stats.mode;
        } else {
            const file_stats = try handleFileIoError(self.zar_io, .stat, file_name, std.posix.fstat(file.handle));

            gid = file_stats.gid;
            uid = file_stats.uid;
            const mtime_full = file_stats.mtime();
            mtime = mtime_full.sec * std.time.ns_per_s + mtime_full.nsec;
            size = @as(u64, @intCast(file_stats.size));
            mode = file_stats.mode;
        }

        if (self.modifiers.update_only) {
            // TODO: Write a test that checks for this functionality still working!
            // TODO: Is this even correct? Shouldn't it be comparing to mtime in archive already?
            if (self.stat.mtime >= mtime and !self.created) {
                continue;
            }
        }

        if (!self.modifiers.use_real_timestamps_and_ids) {
            gid = 0;
            uid = 0;
            mtime = 0;
            // Even though it's not documented - in deterministic mode permissions are always set to:
            // https://github.com/llvm-mirror/llvm/blob/2c4ca6832fa6b306ee6a7010bfb80a3f2596f824/include/llvm/Object/ArchiveWriter.h#L27
            // https://github.com/llvm-mirror/llvm/blob/2c4ca6832fa6b306ee6a7010bfb80a3f2596f824/lib/Object/ArchiveWriter.cpp#L105
            mode = 644;
        }

        const timestamp = @as(u128, @intCast(@divFloor(mtime, std.time.ns_per_s)));

        // Extract critical error from error set - so IO errors can be handled seperately
        const bytes_or_io_error = file.readToEndAllocOptions(allocator, std.math.maxInt(usize), size, std.mem.Alignment.of(u64), null) catch |e| switch (e) {
            error.OutOfMemory => return error.OutOfMemory,
            else => @as(IoError, @errorCast(e)),
        };
        var archived_file = ArchivedFile{ // was var
            .name = try allocator.dupe(u8, std.fs.path.basename(file_name)),
            .contents = Contents{
                .bytes = try handleFileIoError(self.zar_io, .reading, file_name, bytes_or_io_error),
                .length = size,
                .mode = mode,
                .timestamp = timestamp,
                .gid = gid,
                .uid = uid,
            },
        };

        const file_index = if (self.file_name_to_index.get(file_name)) |file_id| file_id else self.files.items.len;

        // Read symbols
        if (self.modifiers.build_symbol_table) {
            try self.addToSymbolTable(allocator, &archived_file, file_index);
        }

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

const RootError = error{ ReadFailed, Unseekable, Unexpected, AccessDenied };

fn printArchiveReadError(zar_io: *const ZarIo, file_name: []const u8, root_err: RootError, file_reader: *const std.fs.File.Reader) void {
    const err = if (file_reader.err) |err| err else root_err;
    switch (err) {
        error.InputOutput,
        error.SystemResources,
        error.IsDir,
        error.OperationAborted,
        error.BrokenPipe,
        error.ConnectionResetByPeer,
        error.ConnectionTimedOut,
        error.NotOpenForReading,
        error.SocketNotConnected,
        error.WouldBlock,
        error.Canceled,
        error.AccessDenied,
        error.ProcessNotFound,
        error.LockViolation,
        error.Unexpected,
        error.ReadFailed,
        error.Unseekable,
        => {}, // We just handle these errors generically
    }
    zar_io.printError("Failed to read archive {s}, an io error occured.", .{file_name});
}

const archive_reader_buffer_size = 4096;

pub fn parse(self: *Archive) (ParseError || CriticalError)!void {
    const allocator = self.arena.allocator();
    const tracy = trace(@src());
    defer tracy.end();

    const endianess = builtin.cpu.arch.endian();

    var reader_buffer: [archive_reader_buffer_size]u8 = undefined;
    var file_reader = self.file.reader(&reader_buffer);
    const reader = &file_reader.interface;
    {
        // Is the magic header found at the start of the archive?
        var magic: [magic_string.len]u8 = undefined;
        const bytes_read = reader.readSliceShort(&magic) catch |err| {
            printArchiveReadError(self.zar_io, self.name, err, &file_reader);
            return err;
        };

        if (bytes_read == 0) {
            // Archive is empty and that is ok!
            return;
        }

        if (bytes_read < magic_string.len) {
            return ParseError.NotArchive;
        }

        const is_thin_archive = std.mem.eql(u8, &magic, magic_thin);

        if (is_thin_archive)
            self.inferred_archive_type = .gnuthin;

        if (!(std.mem.eql(u8, &magic, magic_string) or is_thin_archive)) {
            return ParseError.NotArchive;
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
                const chars_read = reader.readSliceShort(&first_line_buffer) catch |err| {
                    printArchiveReadError(self.zar_io, self.name, err, &file_reader);
                    return err;
                };

                if (chars_read < first_line_buffer.len) {
                    break :result false;
                }

                break :result true;
            };

            if (!has_line_to_process) {
                file_reader.seekTo(starting_seek_pos) catch |err| {
                    switch (err) {
                        error.EndOfStream => return ParseError.NotArchive,
                        else => |root_err| {
                            printArchiveReadError(self.zar_io, self.name, root_err, &file_reader);
                            return error.ReadFailed;
                        },
                    }
                };
                break;
            }

            if (std.mem.eql(u8, first_line_buffer[0..2], "//"[0..2])) {
                switch (self.inferred_archive_type) {
                    .ambiguous => self.inferred_archive_type = .gnu,
                    .gnu, .gnuthin, .gnu64 => {},
                    else => {
                        return ParseError.MalformedArchive;
                    },
                }

                const string_table_num_bytes_string = first_line_buffer[48..58];
                const string_table_num_bytes = try std.fmt.parseInt(u32, std.mem.trim(u8, string_table_num_bytes_string, " "), 10);
                // if (string_table_num_bytes > allocator_limit) {
                //     @panic("OOM!");
                // }

                string_table_contents = try allocator.alloc(u8, string_table_num_bytes);
                errdefer {
                    allocator.free(string_table_contents);
                }

                reader.readSliceAll(string_table_contents) catch |err| {
                    switch (err) {
                        error.EndOfStream => return ParseError.NotArchive,
                        else => |root_err| {
                            printArchiveReadError(self.zar_io, self.name, root_err, &file_reader);
                            return error.ReadFailed;
                        },
                    }
                };
                break;
            } else if (!has_gnu_symbol_table and first_line_buffer[0] == '/') {
                has_gnu_symbol_table = true;
                switch (self.inferred_archive_type) {
                    .ambiguous => self.inferred_archive_type = .gnu,
                    .gnu, .gnuthin, .gnu64 => {},
                    else => {
                        return ParseError.MalformedArchive;
                    },
                }

                const symbol_table_num_bytes_string = first_line_buffer[48..58];
                const symbol_table_num_bytes = try std.fmt.parseInt(u32, std.mem.trim(u8, symbol_table_num_bytes_string, " "), 10);

                // TODO: why is this big endian?
                const num_symbols = reader.takeInt(u32, .big) catch |err| {
                    switch (err) {
                        error.EndOfStream => return ParseError.NotArchive,
                        else => |root_err| {
                            printArchiveReadError(self.zar_io, self.name, root_err, &file_reader);
                            return error.ReadFailed;
                        },
                    }
                };

                var num_bytes_remaining = symbol_table_num_bytes - @sizeOf(u32);

                const number_array = try allocator.alloc(u32, num_symbols);
                for (number_array, 0..) |_, number_index| {
                    number_array[number_index] = reader.takeInt(u32, .big) catch |err| {
                        switch (err) {
                            error.EndOfStream => return ParseError.NotArchive,
                            else => |root_err| {
                                printArchiveReadError(self.zar_io, self.name, root_err, &file_reader);
                                return error.ReadFailed;
                            },
                        }
                    };
                }
                defer allocator.free(number_array);

                num_bytes_remaining = num_bytes_remaining - (@sizeOf(u32) * num_symbols);

                gnu_symbol_table_contents = try allocator.alloc(u8, num_bytes_remaining);

                const contents_read = reader.readSliceShort(gnu_symbol_table_contents) catch |err| {
                    printArchiveReadError(self.zar_io, self.name, err, &file_reader);
                    return err;
                };
                if (contents_read < gnu_symbol_table_contents.len) {
                    return ParseError.MalformedArchive;
                }

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

                    if (!self.modifiers.build_symbol_table) {
                        const symbol = Symbol{
                            .name = current_symbol_string[0..symbol_length],
                            // Note - we don't set the final file-index here,
                            // we recalculate and override that later in parsing
                            // when we know what they are!
                            .file_index = number_array[self.symbols.items.len],
                        };

                        try self.symbols.append(allocator, symbol);
                    }
                    current_symbol_string = current_symbol_string[skip_length..];
                }

                starting_seek_pos = starting_seek_pos + first_line_buffer.len + symbol_table_num_bytes;
            } else {
                file_reader.seekTo(starting_seek_pos) catch |err| {
                    switch (err) {
                        error.EndOfStream => return ParseError.NotArchive,
                        else => |root_err| {
                            printArchiveReadError(self.zar_io, self.name, root_err, &file_reader);
                            return error.ReadFailed;
                        },
                    }
                };
                break;
            }
        }
    }

    var is_first = true;

    var file_offset_to_index: std.AutoArrayHashMapUnmanaged(u64, u64) = .{};
    defer file_offset_to_index.clearAndFree(allocator);

    while (true) {
        const file_offset = file_offset_result: {
            // Archived files must start on even byte boundaries!
            // https://www.unix.com/man-page/opensolaris/3head/ar.h/
            if (@mod(file_reader.logicalPos(), 2) == 1) {
                seekByTempFix(&file_reader, 1) catch |err| {
                    switch (err) {
                        error.EndOfStream => return ParseError.NotArchive,
                        else => |root_err| {
                            printArchiveReadError(self.zar_io, self.name, root_err, &file_reader);
                            return error.ReadFailed;
                        },
                    }
                };
            }
            break :file_offset_result file_reader.logicalPos();
        };

        const archive_header = reader.takeStruct(Header, endianess) catch |err| switch (err) {
            error.EndOfStream => break,
            else => |root_err| {
                printArchiveReadError(self.zar_io, self.name, root_err, &file_reader);
                return error.ReadFailed;
            },
        };

        // the lifetime of the archive headers will matched that of the parsed files (for now)
        // so we can take a reference to the strings stored there directly!
        var trimmed_archive_name = std.mem.trim(u8, &archive_header.ar_name, " ");

        // Check against gnu naming properties
        const ends_with_gnu_slash = (trimmed_archive_name[trimmed_archive_name.len - 1] == '/');
        var gnu_offset_value: u32 = 0;
        const starts_with_gnu_offset = trimmed_archive_name[0] == '/';
        if (starts_with_gnu_offset) {
            gnu_offset_value = try std.fmt.parseInt(u32, trimmed_archive_name[1..trimmed_archive_name.len], 10);
        }

        const must_be_gnu = ends_with_gnu_slash or starts_with_gnu_offset or has_gnu_symbol_table;

        // TODO: if modifiers.use_real_timestamps_and_ids is disabled, do we ignore this from existing archive?
        // Check against llvm ar
        const timestamp = try std.fmt.parseInt(u128, std.mem.trim(u8, &archive_header.ar_date, " "), 10);
        const uid = try std.fmt.parseInt(u32, std.mem.trim(u8, &archive_header.ar_uid, " "), 10);
        const gid = try std.fmt.parseInt(u32, std.mem.trim(u8, &archive_header.ar_gid, " "), 10);

        // Check against bsd naming properties
        const starts_with_bsd_name_length = (trimmed_archive_name.len >= 2) and std.mem.eql(u8, trimmed_archive_name[0..2], bsd_name_length_signifier[0..2]);
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
                    return ParseError.MalformedArchive;
                }
            },
            .bsd, .darwin, .darwin64 => {
                if (must_be_gnu) {
                    return ParseError.MalformedArchive;
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
            const name_offset_in_string_table = try std.fmt.parseInt(u32, std.mem.trim(u8, trimmed_archive_name[1..trimmed_archive_name.len], " "), 10);

            // Navigate to the start of the string in the string table
            const string_start = string_table_contents[name_offset_in_string_table..string_table_contents.len];

            // Find the end of the string (which is always a newline)
            const end_string_index = std.mem.indexOf(u8, string_start, "\n");
            if (end_string_index == null) {
                return ParseError.MalformedArchive;
            }
            const string_full = string_start[0..end_string_index.?];

            // String must have a forward slash before the newline, so check that
            // is there and remove it as well!
            if (string_full[string_full.len - 1] != '/') {
                return ParseError.MalformedArchive;
            }

            // Referencing the slice directly is fine as same bumb allocator is
            // used as for the rest of the datastructure!
            trimmed_archive_name = string_full[0 .. string_full.len - 1];
        }

        var seek_forward_amount = try std.fmt.parseInt(u32, std.mem.trim(u8, &archive_header.ar_size, " "), 10);

        // Make sure that these allocations get properly disposed of later!
        if (starts_with_bsd_name_length) {
            trimmed_archive_name = trimmed_archive_name[bsd_name_length_signifier.len..trimmed_archive_name.len];
            const archive_name_length = try std.fmt.parseInt(u32, trimmed_archive_name, 10);

            // TODO: go through int casts & don't assume that they will just work, add defensive error checking
            // for them. (an internal checked cast or similar).

            if (is_first) {
                // TODO: make sure this does a check on self.inferred_archive_type!

                // This could be the symbol table! So parse that here!
                const current_seek_pos = file_reader.logicalPos();
                var symbol_magic_check_buffer: [bsd_symdef_longest_magic]u8 = undefined;

                // TODO: handle not reading enough characters!
                const chars_read = reader.readSliceShort(&symbol_magic_check_buffer) catch |err| {
                    printArchiveReadError(self.zar_io, self.name, err, &file_reader);
                    return err;
                };

                var sorted = false;

                const magic_match = magic_match_result: {
                    if (chars_read >= bsd_symdef_magic.len and std.mem.eql(u8, bsd_symdef_magic, symbol_magic_check_buffer[0..bsd_symdef_magic.len])) {
                        var magic_len = bsd_symdef_magic.len;

                        if (chars_read >= bsd_symdef_64_magic.len and std.mem.eql(u8, bsd_symdef_64_magic[bsd_symdef_magic.len..], symbol_magic_check_buffer[bsd_symdef_magic.len..])) {
                            magic_len = bsd_symdef_64_magic.len;
                        } else if (chars_read >= bsd_symdef_sorted_magic.len and std.mem.eql(u8, bsd_symdef_sorted_magic[bsd_symdef_magic.len..], symbol_magic_check_buffer[bsd_symdef_magic.len..])) {
                            magic_len = bsd_symdef_sorted_magic.len;
                            sorted = true;
                        }

                        if (chars_read - magic_len > 0) {
                            const seek_amount = @as(i64, @intCast(magic_len)) - @as(i64, @intCast(chars_read));
                            seekByTempFix(&file_reader, seek_amount) catch |err| {
                                switch (err) {
                                    error.EndOfStream => return ParseError.NotArchive,
                                    else => |root_err| {
                                        printArchiveReadError(self.zar_io, self.name, root_err, &file_reader);
                                        return error.ReadFailed;
                                    },
                                }
                            };
                        }

                        seek_forward_amount = seek_forward_amount - @as(u32, @intCast(magic_len));

                        break :magic_match_result true;
                    }

                    break :magic_match_result false;
                };

                if (magic_match) {
                    // TODO: comment on alignment stuff... (really should have done this when this was written!)
                    {
                        const current_pos = file_reader.logicalPos();
                        const remainder = @as(u32, @intCast((self.inferred_archive_type.getAlignment() - current_pos % self.inferred_archive_type.getAlignment()) % self.inferred_archive_type.getAlignment()));
                        seek_forward_amount = seek_forward_amount - remainder;
                        seekByTempFix(&file_reader, remainder) catch |err| {
                            switch (err) {
                                error.EndOfStream => return ParseError.NotArchive,
                                else => |root_err| {
                                    printArchiveReadError(self.zar_io, self.name, root_err, &file_reader);
                                    return error.ReadFailed;
                                },
                            }
                        };
                    }

                    // TODO: error if negative (because spec defines this as a long, so should never be that large?)
                    const num_ranlib_bytes_unchecked = reader.takeInt(IntType, endianess) catch |err| {
                        switch (err) {
                            error.EndOfStream => return ParseError.NotArchive,
                            else => |root_err| {
                                printArchiveReadError(self.zar_io, self.name, root_err, &file_reader);
                                return error.ReadFailed;
                            },
                        }
                    };
                    const num_ranlib_bytes: u32 = num_ranlib_bytes: {
                        if (num_ranlib_bytes_unchecked < 0) {
                            // TODO: Malformed Archive, log error
                            return error.TODO;
                        }
                        break :num_ranlib_bytes @intCast(num_ranlib_bytes_unchecked);
                    };
                    seek_forward_amount = seek_forward_amount - @as(u32, @sizeOf(IntType));

                    // TODO: error if this doesn't divide properly?
                    // const num_symbols = @divExact(num_ranlib_bytes_unchecked, @sizeOf(Ranlib(IntType)));

                    const ranlib_bytes = try allocator.alloc(u8, num_ranlib_bytes);

                    // TODO: error handling
                    _ = reader.readSliceShort(ranlib_bytes) catch |err| {
                        printArchiveReadError(self.zar_io, self.name, err, &file_reader);
                        return err;
                    };
                    if (seek_forward_amount < num_ranlib_bytes) {
                        // TODO: Malformed archive, log error,
                        return error.TODO;
                    }
                    seek_forward_amount = seek_forward_amount - num_ranlib_bytes;

                    const ranlibs = std.mem.bytesAsSlice(Ranlib(IntType), ranlib_bytes);

                    const symbol_strings_length = reader.takeInt(u32, endianess) catch |err| {
                        switch (err) {
                            error.EndOfStream => return ParseError.NotArchive,
                            else => |root_err| {
                                printArchiveReadError(self.zar_io, self.name, root_err, &file_reader);
                                return error.ReadFailed;
                            },
                        }
                    };
                    // TODO: We don't really need this information, but maybe it could come in handy
                    // later?
                    _ = symbol_strings_length;

                    seek_forward_amount = seek_forward_amount - @as(u32, @sizeOf(IntType));

                    const symbol_string_bytes = try allocator.alloc(u8, seek_forward_amount);
                    seek_forward_amount = 0;
                    // TODO: we shouldn't discard this result right? or assert.. or something
                    _ = reader.readSliceShort(symbol_string_bytes) catch |err| {
                        printArchiveReadError(self.zar_io, self.name, err, &file_reader);
                        return err;
                    };

                    if (!self.modifiers.build_symbol_table) {
                        for (ranlibs) |ranlib| {
                            const symbol_string = std.mem.sliceTo(symbol_string_bytes[@as(u64, @intCast(ranlib.ran_strx))..], 0);

                            const symbol = Symbol{
                                .name = symbol_string,
                                // Note - we don't set the final file-index here,
                                // we recalculate and override that later in parsing
                                // when we know what they are!
                                .file_index = @as(u64, @intCast(ranlib.ran_off)),
                            };

                            try self.symbols.append(allocator, symbol);
                        }

                        // We have a symbol table!
                    }
                    seekByTempFix(&file_reader, seek_forward_amount) catch |err| {
                        switch (err) {
                            error.EndOfStream => return ParseError.NotArchive,
                            else => |root_err| {
                                printArchiveReadError(self.zar_io, self.name, root_err, &file_reader);
                                return error.ReadFailed;
                            },
                        }
                    };
                    continue;
                }

                file_reader.seekTo(current_seek_pos) catch |err| {
                    switch (err) {
                        error.EndOfStream => return ParseError.NotArchive,
                        else => |root_err| {
                            printArchiveReadError(self.zar_io, self.name, root_err, &file_reader);
                            return error.ReadFailed;
                        },
                    }
                };
            }

            const archive_name_buffer = try allocator.alloc(u8, archive_name_length);

            reader.readSliceAll(archive_name_buffer) catch |err| {
                switch (err) {
                    error.EndOfStream => return ParseError.NotArchive,
                    else => |root_err| {
                        printArchiveReadError(self.zar_io, self.name, root_err, &file_reader);
                        return error.ReadFailed;
                    },
                }
            };

            seek_forward_amount = seek_forward_amount - archive_name_length;

            // strip null characters from name - TODO find documentation on this
            // could not find documentation on this being needed, but some archivers
            // seems to insert these (for alignment reasons?)
            trimmed_archive_name = std.mem.trim(u8, archive_name_buffer, "\x00");
        } else {
            const archive_name_buffer = try allocator.alloc(u8, trimmed_archive_name.len);
            @memcpy(archive_name_buffer, trimmed_archive_name);
            trimmed_archive_name = archive_name_buffer;
        }

        const parsed_file = ArchivedFile{
            .name = trimmed_archive_name,
            .contents = Contents{
                .bytes = try allocator.alignedAlloc(u8, std.mem.Alignment.of(u64), seek_forward_amount),
                .length = seek_forward_amount,
                .mode = try std.fmt.parseInt(u32, std.mem.trim(u8, &archive_header.ar_mode, " "), 10),
                .timestamp = timestamp,
                .uid = uid,
                .gid = gid,
            },
        };

        if (self.inferred_archive_type == .gnuthin) {
            // var thin_file = try handleFileIoError(self.zar_io, .opening, trimmed_archive_name, self.zar_io.cwd.openFile(trimmed_archive_name, .{}));
            var thin_file = self.zar_io.cwd.openFile(trimmed_archive_name, .{}) catch {
                // printArchiveOpenError();
                // _ = err;
                // TODO: handle error!
                return error.TODO;
            };
            defer thin_file.close();

            var thin_file_reader_buffer: [archive_reader_buffer_size]u8 = undefined;
            var thin_file_reader = thin_file.reader(&thin_file_reader_buffer);
            const thin_reader = &thin_file_reader.interface;
            thin_reader.readSliceAll(parsed_file.contents.bytes) catch |err| {
                switch (err) {
                    error.EndOfStream => return ParseError.NotArchive,
                    else => |root_err| {
                        printArchiveReadError(self.zar_io, self.name, root_err, &file_reader);
                        return error.ReadFailed;
                    },
                }
            };
        } else {
            reader.readSliceAll(parsed_file.contents.bytes) catch |err| {
                switch (err) {
                    error.EndOfStream => return ParseError.NotArchive,
                    else => |root_err| {
                        printArchiveReadError(self.zar_io, self.name, root_err, &file_reader);
                        return error.ReadFailed;
                    },
                }
            };
        }

        if (self.modifiers.build_symbol_table) {
            self.addToSymbolTable(allocator, &parsed_file, self.files.items.len) catch {
                return error.TODO;
            };
        }

        try self.file_name_to_index.put(allocator, trimmed_archive_name, self.files.items.len);
        try file_offset_to_index.put(allocator, file_offset, self.files.items.len);
        try self.files.append(allocator, parsed_file);

        is_first = false;
    }

    if (is_first) {
        const current_position = file_reader.logicalPos();
        if (current_position > magic_string.len) {
            return ParseError.MalformedArchive;
        }
    }

    if (!self.modifiers.build_symbol_table) {
        for (self.symbols.items) |*symbol| {
            if (file_offset_to_index.get(symbol.file_index)) |file_index| {
                symbol.file_index = file_index;
            } else {
                symbol.file_index = invalid_file_index;
            }
        }
    }

    // Set output archive type based on inference or current os if necessary
    if (self.output_archive_type == .ambiguous) {
        // Set output archive type of one we might just have parsed...
        self.output_archive_type = self.inferred_archive_type;
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

    pub fn init(allocator: Allocator, file: std.fs.File) !Self {
        const self = Self{
            .script = try file.readToEndAlloc(allocator, std.math.maxInt(usize)),
            .archive = null,
            .file_name = null,
        };

        return self;
    }

    // Returns the next token
    fn getToken(iter: *std.mem.SplitIterator(u8)) ?[]const u8 {
        while (iter.next()) |tok| {
            if (std.mem.startsWith(u8, tok, "*")) break;
            if (std.mem.startsWith(u8, tok, ";")) break;
            return tok;
        }
        return null;
    }

    // Returns a slice of tokens
    fn getTokenLine(allocator: Allocator, iter: *std.mem.SplitIterator(u8)) ![]const []const u8 {
        var list = std.ArrayList([]const u8).init(allocator);
        errdefer list.deinit();

        while (getToken(iter)) |tok| {
            try list.append(tok);
        }
        return list.toOwnedSlice();
    }

    pub fn execute(self: *Self, allocator: Allocator, stdout: *std.io.Writer, stderr: *std.io.Writer) !void {
        // Split the file into lines
        var parser = std.mem.split(u8, self.script, "\n");

        while (parser.next()) |line| {
            // Split the line by spaces
            var line_parser = std.mem.split(u8, line, " ");

            if (getToken(&line_parser)) |tok| {
                const command_name = try allocator.dupe(u8, tok);
                defer allocator.free(command_name);

                _ = std.ascii.lowerString(command_name, tok);

                if (std.meta.stringToEnum(CommandType, command_name)) |command| {
                    if (self.archive) |archive| {
                        switch (command) {
                            .addmod => {
                                const file_names = try getTokenLine(allocator, &line_parser);
                                defer allocator.free(file_names);

                                try self.archive.?.insertFiles(file_names);
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
                                try self.archive.?.flush();
                            },
                            .clear => {
                                // This is a bit of a hack but its reliable.
                                // Instead of clearing out unsaved changes, we re-open the current file, which overwrites the changes.
                                const file = try handleFileIoError(self.zar_io, .opening, self.file_name, self.zar_io.cwd.openFile(self.file_name.?, .{ .mode = .read_write }));
                                self.archive = Archive.init(file, self.file_name.?);
                                defer self.archive.deinit();

                                try self.archive.?.parse(stderr);
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

                                const file = try handleFileIoError(self.zar_io, .opening, file_name, self.zar_io.cwd.openFile(file_name, .{ .mode = .read_write }));
                                self.archive = Archive.init(file, file_name);
                                self.file_name = file_name;

                                try self.archive.?.parse(stderr);
                            },
                            .create, .createthin => {
                                // TODO: Thin archives creation
                                const file_name = getToken(&line_parser).?;

                                const file = try self.zar_io.cwd.createFile(file_name, .{ .read = true });
                                self.archive = Archive.init(file, file_name);
                                self.file_name = file_name;

                                try self.archive.?.parse(stderr);
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
