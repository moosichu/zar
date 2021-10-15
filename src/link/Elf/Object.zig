const Object = @This();

const std = @import("std");
const assert = std.debug.assert;
const elf = std.elf;
const fs = std.fs;
const log = std.log.scoped(.elf);
const math = std.math;
const mem = std.mem;

const Allocator = mem.Allocator;

file: fs.File,
name: []const u8,
file_offset: ?u32 = null,

header: ?elf.Elf64_Ehdr = null,

shdrs: std.ArrayListUnmanaged(elf.Elf64_Shdr) = .{},

sections: std.ArrayListUnmanaged(u16) = .{},
relocs: std.AutoHashMapUnmanaged(u16, u16) = .{},

symtab: std.ArrayListUnmanaged(elf.Elf64_Sym) = .{},
strtab: std.ArrayListUnmanaged(u8) = .{},

symtab_index: ?u16 = null,

pub fn init(file: fs.File, name: []const u8) Object {
    return .{.file = file, .name = name};
}

pub fn deinit(self: *Object, allocator: *Allocator) void {
    self.shdrs.deinit(allocator);
    self.sections.deinit(allocator);
    self.relocs.deinit(allocator);
    self.symtab.deinit(allocator);
    self.strtab.deinit(allocator);
    allocator.free(self.name);
}

pub fn parse(self: *Object, allocator: *Allocator, target: std.Target) !void {
    const reader = self.file.reader();
    if (self.file_offset) |offset| {
        try reader.context.seekTo(offset);
    }
    const header = try reader.readStruct(elf.Elf64_Ehdr);

    if (!mem.eql(u8, header.e_ident[0..4], "\x7fELF")) {
        log.debug("Invalid ELF magic {s}, expected \x7fELF", .{header.e_ident[0..4]});
        return error.NotObject;
    }
    if (header.e_ident[elf.EI_VERSION] != 1) {
        log.debug("Unknown ELF version {d}, expected 1", .{header.e_ident[elf.EI_VERSION]});
        return error.NotObject;
    }
    if (header.e_ident[elf.EI_DATA] != elf.ELFDATA2LSB) {
        log.err("TODO big endian support", .{});
        return error.TODOBigEndianSupport;
    }
    if (header.e_ident[elf.EI_CLASS] != elf.ELFCLASS64) {
        log.err("TODO 32bit support", .{});
        return error.TODOElf32bitSupport;
    }
    if (header.e_type != elf.ET.REL) {
        log.debug("Invalid file type {any}, expected ET.REL", .{header.e_type});
        return error.NotObject;
    }
    if (header.e_machine != target.cpu.arch.toElfMachine()) {
        log.debug("Invalid architecture {any}, expected {any}", .{
            header.e_machine,
            target.cpu.arch.toElfMachine(),
        });
        return error.InvalidCpuArch;
    }
    if (header.e_version != 1) {
        log.debug("Invalid ELF version {d}, expected 1", .{header.e_version});
        return error.NotObject;
    }

    assert(header.e_entry == 0);
    assert(header.e_phoff == 0);
    assert(header.e_phnum == 0);

    self.header = header;

    try self.parseShdrs(allocator, reader);
    try self.parseSymtab(allocator);
}

fn parseShdrs(self: *Object, allocator: *Allocator, reader: anytype) !void {
    const shnum = self.header.?.e_shnum;
    if (shnum == 0) return;

    const offset = self.file_offset orelse 0;
    try reader.context.seekTo(offset + self.header.?.e_shoff);
    try self.shdrs.ensureTotalCapacity(allocator, shnum);

    var i: u16 = 0;
    while (i < shnum) : (i += 1) {
        const shdr = try reader.readStruct(elf.Elf64_Shdr);
        self.shdrs.appendAssumeCapacity(shdr);

        switch (shdr.sh_type) {
            elf.SHT_SYMTAB => {
                self.symtab_index = i;
            },
            elf.SHT_PROGBITS, elf.SHT_NOBITS => {
                try self.sections.append(allocator, i);
            },
            elf.SHT_REL, elf.SHT_RELA => {
                try self.relocs.putNoClobber(allocator, @intCast(u16, shdr.sh_info), i);
            },
            else => {},
        }
    }

    // Parse shstrtab
    var buffer = try self.readShdrContents(allocator, self.header.?.e_shstrndx);
    defer allocator.free(buffer);
    try self.strtab.appendSlice(allocator, buffer);
}

fn parseSymtab(self: *Object, allocator: *Allocator) !void {
    if (self.symtab_index == null) return;

    const symtab_shdr = self.shdrs.items[self.symtab_index.?];

    // We first read the contents of string table associated with this symbol table
    // noting the offset at which it is appended to the existing string table, which
    // we will then use to fixup st_name offset within each symbol.
    const strtab_offset = @intCast(u32, self.strtab.items.len);
    var str_buffer = try self.readShdrContents(allocator, @intCast(u16, symtab_shdr.sh_link));
    defer allocator.free(str_buffer);
    try self.strtab.appendSlice(allocator, str_buffer);

    var sym_buffer = try self.readShdrContents(allocator, self.symtab_index.?);
    defer allocator.free(sym_buffer);
    const syms = @alignCast(@alignOf(elf.Elf64_Sym), mem.bytesAsSlice(elf.Elf64_Sym, sym_buffer));
    try self.symtab.ensureTotalCapacity(allocator, syms.len);

    for (syms) |sym| {
        var out_sym = sym;
        if (sym.st_name > 0) {
            out_sym.st_name += strtab_offset;
        } else if (sym.st_info & 0xf == elf.STT_SECTION) {
            // If the symbol is pointing to a section header, copy the sh_name offset as the new
            // st_name offset.
            const shdr = self.shdrs.items[sym.st_shndx];
            out_sym.st_name = shdr.sh_name;
        }
        self.symtab.appendAssumeCapacity(out_sym);
    }
}

fn sortBySeniority(aliases: []u32, object: *Object) void {
    const Context = struct {
        object: *Object,
    };
    const SortFn = struct {
        fn lessThan(ctx: Context, lhs: u32, rhs: u32) bool {
            const lhs_sym = ctx.object.symtab.items[lhs];
            const lhs_sym_bind = lhs_sym.st_info >> 4;
            const rhs_sym = ctx.object.symtab.items[rhs];
            const rhs_sym_bind = rhs_sym.st_info >> 4;

            if (lhs_sym_bind == rhs_sym_bind) {
                return false;
            }
            if (lhs_sym_bind == elf.STB_GLOBAL) {
                return true;
            } else if (lhs_sym_bind == elf.STB_WEAK and rhs_sym_bind != elf.STB_GLOBAL) {
                return true;
            }
            return false;
        }
    };

    std.sort.sort(u32, aliases, Context{ .object = object }, SortFn.lessThan);
}

pub fn getString(self: Object, off: u32) []const u8 {
    assert(off < self.strtab.items.len);
    return mem.spanZ(@ptrCast([*:0]const u8, self.strtab.items.ptr + off));
}

/// Caller owns the memory.
fn readShdrContents(self: Object, allocator: *Allocator, shdr_index: u16) ![]const u8 {
    const shdr = self.shdrs.items[shdr_index];
    var buffer = try allocator.alloc(u8, shdr.sh_size);
    errdefer allocator.free(buffer);

    const offset = self.file_offset orelse 0;
    const amt = try self.file.preadAll(buffer, shdr.sh_offset + offset);
    if (amt != buffer.len) {
        return error.InputOutput;
    }

    return buffer;
}
