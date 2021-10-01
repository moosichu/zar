const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;
const testing = std.testing;

const Archive = @import("archive/Archive.zig");

const test1_dir = "test/data/test1";
const test1_gnu_archive = "output_llvm-ar_gnu.a";
const test1_bsd_archive = "output_llvm-ar_bsd.a";
const test1_names = [_][]const u8{ "input1.txt", "input2.txt" };

const test2_dir = "test/data/test2";
const test2_gnu_archive = "output_llvm-ar_gnu.a";
const test2_bsd_archive = "output_llvm-ar_bsd.a";
const test2_names = [_][]const u8{ "input1.txt", "input2.txt", "input3_that_is_also_a_much_longer_file_name.txt", "input4_that_is_also_a_much_longer_file_name.txt" };

test "List Files GNU test1" {
    try testDislayContents(test1_dir, test1_gnu_archive, test1_names);
}

// test "List Files BSD test1" {
//     try testDislayContents(test1_dir, test1_bsd_archive, test1_names);
// }

test "List Files GNU test2" {
    try testDislayContents(test2_dir, test2_gnu_archive, test2_names);
}

// test "List Files BSD test2" {
//     try testDislayContents(test2_dir, test2_bsd_archive, test2_names);
// }

fn testDislayContents(test_dir_path: []const u8, archive_name: []const u8, file_names: anytype) !void {
    const test_dir = try fs.cwd().openDir(test_dir_path, .{});

    const file = try test_dir.openFile(archive_name, .{});
    defer file.close();

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var allocator = &arena.allocator;
    // TODO: pass through custom writer so we can check outputs on error tests
    const stderr = io.getStdErr().writer();

    var archive = Archive.create(file, archive_name);
    try archive.parse(allocator, stderr);
    for (file_names) |file_name, index| {
        try testing.expect(mem.eql(u8, archive.files.items[index].name, file_name));
    }
}

test "test suite" {
    var ctx = TestContext.init(std.heap.page_allocator);
    //simple test: create a new gnu archive and compare the output //doesn't work for now 
    {
       
        
        var case = try ctx.addCase("create gnu archive from txt files", "test1");
        
        try case.args.append("zar");
        try case.args.append("-rc");
        try case.args.append("./test.a");
        try case.args.append("./input1.txt");
        try case.testOutput();
    }
   

    
}
pub const TestContext = struct {
    //this struct manages the tests
    const Self = @This();
    cases: std.ArrayList(Case),
    
    pub const File = struct {
            
        fileType: FileType,
        path: []const u8,
        control: bool, 
        
        
        pub const FileType = enum {
            ar_bsd,
            ar_gnu,
            object, 
            txt,
        };
    };
    pub const Output = struct{
        //struct 
        stdout: ?[]const u8,
        stderr: ?[]const u8,
        outFile: File,
        
    };
        
    pub const Case = struct {
        name: []const u8, 
        input_files: std.ArrayList(File) = undefined,
        expected_out: Output = undefined,
        control: []const u8 = undefined,
        args: std.ArrayList([]const u8) = std.ArrayList([]const u8).init(std.heap.page_allocator),
        
        pub fn init(name: []const u8) Case {
            return .{ 
                .name = name,
            };
        }
        
        pub fn deinit(self: Case) void {
            self.args.deinit();
        }
            
            
        
        pub fn testOutput(self: *Case) !void {
            const result = try std.ChildProcess.exec(
                .{
                    .allocator = std.heap.page_allocator, 
                    .argv = self.args.items,
                    .cwd = "/home/jo/zar/test/data/test1",
                }
            );
        }
            
                
            
        
        pub fn addInput(self: *Case, file: File) !void {
            try self.input_files.append(file);
            errdefer {
                std.debug.print("\n error while adding file {s} to case {s}", .{
                    file, 
                    self.name,
                });
            }
        }
    };
    
    pub fn init( allocator: *Allocator) TestContext {
        var cases = std.ArrayList(Case).init(allocator); 
        return Self{ .cases = cases};
    }
    
    pub fn deinit(self: TestContext) void {
        for (self.cases.items) |*case| {
            case.deinit();
        }
        self.cases.deinit();
    }
    
    pub fn run(self: TestContext) !void {
        for (self.cases.items) |case| {
            
        }
    }
    pub fn addCase(self: TestContext, name: []const u8, working_dir: []const u8) !Case {
        const index = self.cases.items.len; 
        return Case{
            .name = name,
        };
    }
};
