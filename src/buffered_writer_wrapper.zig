const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;

// This is a small hack to get around the fact that the buffered writer
// uses a stack allocatored array, and can't be passed as an immutable-by-value
// as-is to the Writer api - which we need to use to provide the writer
// functionality that we use.
pub fn BufferedWriterWrapper(
    comptime BufferedWriter: type,
) type {
    return struct {
        pub const Error = BufferedWriter.Error;
        buffered_writer: *BufferedWriter,
        const Self = @This();

        pub fn write(self: Self, bytes: []const u8) BufferedWriter.Error!usize {
            return self.buffered_writer.write(bytes);
        }
    };
}
