const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;

// This exists to allow us to track the current file position through a buffered
// writer as we need to keep track of this information for padding reasons when
// archiving.
pub fn TrackingBufferedWriter(
    comptime BufferedWriter: type,
) type {
    return struct {
        pub const Error = BufferedWriter.Error;
        buffered_writer: BufferedWriter,
        file_pos: usize = 0,
        const Self = @This();
        const Writer = std.io.Writer(*Self, Error, write);

        pub fn write(self: *Self, bytes: []const u8) BufferedWriter.Error!usize {
            const file_pos_change = try self.buffered_writer.write(bytes);
            self.file_pos += file_pos_change;
            return file_pos_change;
        }

        pub fn writer(self: *Self) Writer {
            return .{ .context = self };
        }

        pub fn flush(self: *Self) !void {
            try self.buffered_writer.flush();
        }
    };
}
