const std = @import("std");

const ZarIo = @This();

cwd: std.fs.Dir,
stdout: *std.io.Writer,
stdout_config: std.io.tty.Config,
stderr: *std.io.Writer,
stderr_config: std.io.tty.Config,

pub fn printError(zar_io: *const ZarIo, comptime format: []const u8, args: anytype) void {
    zar_io.stderr.print("zar: ", .{}) catch {};
    zar_io.stderr_config.setColor(zar_io.stderr, .red) catch {};
    zar_io.stderr_config.setColor(zar_io.stderr, .bold) catch {};
    zar_io.stderr.print("error: ", .{}) catch {};
    zar_io.stderr_config.setColor(zar_io.stderr, .reset) catch {};
    zar_io.stderr.print(format, args) catch {};
    zar_io.stderr.print("\n", .{}) catch {};
}
