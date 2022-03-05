const Object = @This();

const std = @import("std");
const assert = std.debug.assert;
const fs = std.fs;
const log = std.log.scoped(.elf);
const math = std.math;
const mem = std.mem;

const Allocator = mem.Allocator;
const Bitcode = @import("../Bitcode.zig");
pub const magic = "BC\xC0\xDE";

// TODO: find examples of this to parse, as some bitcode files contain this
// as a wrapper - but it's not consistent.
// See: https://llvm.org/docs/BitCodeFormat.html#bitcode-wrapper-format
const BitcodeWrapperHeader = struct {
    magic: u32,
    version: u32,
    offset: u32,
    size: u32,
    cpu_type: u32,

    pub const magic: u32 = 0x0b17C0DE;
};

file: fs.File,
name: []const u8,
file_offset: ?u32 = null,

// Reference for llvm bitcode format:
// https://llvm.org/docs/BitCodeFormat.html

pub fn deinit(self: *Object, allocator: Allocator) void {
    _ = self;
    _ = allocator;
    // ZAR MODIFICATION:
    // We manage memory of assigned names ourselves in zar - so
    // freeing this here for that does not make much sense.
    // allocator.free(self.name);
}

pub fn parse(self: *Object, allocator: Allocator, target: std.Target) !void {
    _ = self;
    _ = allocator;
    _ = target;

    const reader = self.file.reader();

    var magic_buffer: [magic.len]u8 = undefined;
    _ = try reader.read(&magic_buffer);

    if (!mem.eql(u8, &magic_buffer, magic)) {
        log.debug("Invalid BitCodes magic {s}, expected " ++ magic, .{magic_buffer});
        return error.NotObject;
    }

    return error.Unimplemented;
}
