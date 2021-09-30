const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("zar", "src/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    
    const tests = b.addTest("src/test.zig");
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
    
    const test_step = b.step("test", "Test the archiver"); 
    test_step.dependOn(&exe.step); 
    test_step.dependOn(&tests.step);
    
    
}


///////test.zig
const std = @import("std"); 
const file = std.fs.file; 
const Allocator = std.mem.Allocator;
//const tests = @import("../test/tests.zig");




test "test suite" {
    var ctx = TestContext.init(std.heap.page_allocator);
    //simple test: create a new gnu archive and compare the output
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
        //struct to describe the file used in the test
        
        //capture the output of the program
        
        name: []const u8, 
        input_files: std.ArrayList(File) = undefined,
        expected_out: Output = undefined,
        //args: []const u8,
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
        
        
        
        
        


        




