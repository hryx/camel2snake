const std = @import("std");
const fs = std.fs;
const os = std.os;
const log = std.log;
const mem = std.mem;
const Allocator = mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const StringSet = std.StringHashMapUnmanaged(void);
const assert = std.debug.assert;

fn usage() void {
    const text =
        \\Usage: camel2snake [options] PATH ...
        \\
        \\Overview:
        \\
        \\  Convert camelCase identifiers to snake_case in Zig code.
        \\  Overwrites files in place.
        \\
        \\  PATH can be a .zig file or directory to recursively search for Zig files.
        \\
        \\  By default, no identifiers are affected.
        \\  Use the --convert* options to choose identifiers to convert.
        \\  Exceptions always take higher precedence.
        \\
        \\Options:
        \\
        \\  -h, --help              Print this help and exit
        \\  --convert=IDENT         Convert instances of IDENT to snake case
        \\  --convert-list=FILE     Load newline-separated list of identifiers to convert from FILE
        \\  --convert-all           Convert all identifiers except those explicitly excluded
        \\  --except=IDENT          Do not convert instances of IDENT to snake case
        \\  --except-list=FILE      Load newline-separated list of idenfifiers to ignore from FILE
        \\  --ignore-path=PATH      Do not process file or directory at PATH
        \\  --adult-camels          Additionally convert AdultCamels into Adult_Snakes
        \\  --dry-run               Print files that would have changes (does not modify files)
        \\  --dry-run-highlight     Print files, colorizing affected tokens (does not modify files)
        \\
    ;
    std.io.getStdErr().writeAll(text) catch unreachable;
}

fn get_flag_value(arg: []const u8, flag: [:0]const u8) ?[]const u8 {
    var iter = mem.split(u8, arg, "=");
    if (mem.eql(u8, iter.first(), flag)) return iter.rest();
    return null;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len < 2) {
        usage();
        os.exit(1);
    }

    var paths = std.ArrayList([]const u8).init(allocator);
    defer paths.deinit();

    var converter = Converter.init(allocator);
    defer converter.deinit();

    var no_more_options = false;
    for (args[1..]) |arg| {
        if (no_more_options) {
            try paths.append(arg);
            continue;
        }
        if (mem.eql(u8, arg, "--")) {
            no_more_options = true;
        } else if (mem.eql(u8, arg, "-h") or (mem.eql(u8, arg, "--help"))) {
            usage();
            return;
        } else if (mem.eql(u8, arg, "--dry-run")) {
            converter.action = .dry_run;
        } else if (mem.eql(u8, arg, "--dry-run-highlight")) {
            converter.action = .highlight;
        } else if (mem.eql(u8, arg, "--adult-camels")) {
            converter.adult_camels = true;
        } else if (mem.eql(u8, arg, "--convert-all")) {
            converter.convert_by_default = true;
        } else if (get_flag_value(arg, "--convert=")) |name| {
            try converter.convert_ident_set.put(converter.arena.allocator(), name, {});
        } else if (get_flag_value(arg, "--convert-list=")) |path| {
            _ = path;
            @panic("TODO: load convert list from file");
        } else if (get_flag_value(arg, "--except=")) |name| {
            try converter.ignore_ident_set.put(converter.arena.allocator(), name, {});
        } else if (get_flag_value(arg, "--except-list=")) |path| {
            _ = path;
            @panic("TODO: load exclude list from file");
        } else if (get_flag_value(arg, "--ignore-path=")) |path| {
            const abs_path = try fs.cwd().realpathAlloc(allocator, path);
            try converter.ignore_file_set.put(converter.arena.allocator(), abs_path, {});
        } else if (mem.startsWith(u8, arg, "-")) {
            log.err("unrecognized option: {s}", .{arg});
            usage();
            os.exit(1);
        } else {
            try paths.append(arg);
        }
    }

    if (paths.items.len == 0) {
        log.err("no files or directories specified", .{});
        usage();
        os.exit(1);
    }

    try converter.convert(paths.items);
}

const Converter = struct {
    arena: ArenaAllocator,
    ignore_file_set: StringSet,
    to_convert_file_set: StringSet,
    convert_ident_set: StringSet,
    ignore_ident_set: StringSet,
    convert_by_default: bool,
    adult_camels: bool,
    action: enum { dry_run, highlight, convert },

    pub fn init(allocator: Allocator) Converter {
        return .{
            .arena = ArenaAllocator.init(allocator),
            .ignore_file_set = .{},
            .to_convert_file_set = .{},
            .convert_ident_set = .{},
            .ignore_ident_set = .{},
            .convert_by_default = false,
            .adult_camels = false,
            .action = .convert,
        };
    }

    pub fn deinit(self: *Converter) void {
        self.arena.deinit();
    }

    pub fn convert(self: *Converter, paths: []const []const u8) !void {
        for (paths) |path| {
            try self.scan_path(fs.cwd(), path);
        }
        var key_iter = self.to_convert_file_set.keyIterator();
        while (key_iter.next()) |path| {
            try self.convert_file(path.*);
        }
    }

    fn scan_path(self: *Converter, dir: fs.Dir, path: []const u8) !void {
        const abs_path = try dir.realpathAlloc(self.arena.allocator(), path);

        if (self.ignore_file_set.contains(abs_path)) {
            return;
        }

        const stat = try dir.statFile(abs_path);
        switch (stat.kind) {
            .File => {
                if (mem.eql(u8, fs.path.extension(abs_path), ".zig")) {
                    try self.to_convert_file_set.put(self.arena.allocator(), abs_path, {});
                }
            },
            .Directory => {
                var sub_dir = try dir.openIterableDir(abs_path, .{});
                defer sub_dir.close();

                var iter = sub_dir.iterate();
                while (try iter.next()) |entry| {
                    try self.scan_path(sub_dir.dir, entry.name);
                }
            },
            else => return,
        }
    }

    fn convert_file(self: *Converter, path: []const u8) !void {
        const allocator = self.arena.child_allocator;
        const src = try fs.cwd().readFileAlloc(allocator, path, 10_000_000);
        defer allocator.free(src);

        const std_out = std.io.getStdOut().writer();
        var tokenizer = Tokenizer.init(src);
        switch (self.action) {
            .dry_run => {
                while (tokenizer.next()) |tok| {
                    switch (tok.tag) {
                        .camel => {
                            try std_out.print("{s}\n", .{path});
                            return;
                        },
                        .adult_camel => {
                            if (self.adult_camels) {
                                try std_out.print("{s}\n", .{path});
                                return;
                            }
                        },
                        .ident_ish, .other_stuff => {},
                    }
                }
            },
            .highlight => {
                while (tokenizer.next()) |tok| {
                    switch (tok.tag) {
                        .camel => {
                            try std_out.writeByte(0o033);
                            try std_out.writeAll("[31;4m");
                            try convert_case(tok.bytes, std_out);
                            try std_out.writeByte(0o033);
                            try std_out.writeAll("[0m");
                        },
                        .adult_camel => {
                            if (self.adult_camels) {
                                try std_out.writeByte(0o033);
                                try std_out.writeAll("[33;4m");
                                try convert_case(tok.bytes, std_out);
                                try std_out.writeByte(0o033);
                                try std_out.writeAll("[0m");
                            } else {
                                try std_out.writeAll(tok.bytes);
                            }
                        },
                        .ident_ish, .other_stuff => {
                            try std_out.writeAll(tok.bytes);
                        },
                    }
                }
            },
            .convert => {
                var changed = false;
                while (tokenizer.next()) |tok| {
                    switch (tok.tag) {
                        .camel => {
                            log.info("TODO: convert ident {s}", .{tok.bytes});
                            changed = true;
                        },
                        .adult_camel => {
                            if (self.adult_camels) {
                                log.info("TODO: convert ident {s}", .{tok.bytes});
                                changed = true;
                            }
                        },
                        .ident_ish, .other_stuff => {},
                    }
                }
                if (changed) {
                    try std_out.print("{s}\n", .{path});
                }
            },
        }
    }
};

fn convert_case(token: []const u8, writer: anytype) !void {
    const isUpper = std.ascii.isUpper;
    const isLower = std.ascii.isLower;
    const toLower = std.ascii.toLower;

    assert(token.len > 0);
    try writer.writeByte(token[0]);
    if (token.len == 1) return;

    // First character defines whether this is lowerCamel or UpperCamel
    const adult = isUpper(token[0]);
    var prev_was_upper = adult;
    for (token[1..], 1..) |c, i| {
        var is_boundary = false;
        if (isUpper(c)) {
            if (!prev_was_upper) {
                // myFunc
                //   ^
                is_boundary = true;
            } else if (i + 1 < token.len and isLower(token[i + 1])) {
                // myAPIFunc
                //      ^
                is_boundary = true;
            }
            prev_was_upper = true;
        } else {
            prev_was_upper = false;
        }
        if (is_boundary and token[i - 1] != '_') {
            try writer.writeByte('_');
        }
        try writer.writeByte(if (adult) c else toLower(c));
    }
}

test "convert case" {
    try testConvert("x", "x");
    try testConvert("already_snake", "already_snake");
    try testConvert("myFunc", "my_func");
    try testConvert("myIFunc", "my_i_func");
    try testConvert("myAPIFunc", "my_api_func");
    try testConvert("libC", "lib_c");
    try testConvert("libCPP", "lib_cpp");
    try testConvert("libZ2", "lib_z2");
    try testConvert("libX2StuffY", "lib_x2_stuff_y");

    try testConvert("X", "X");
    try testConvert("Already_Snake", "Already_Snake");
    try testConvert("MyFunc", "My_Func");
    try testConvert("MyIFunc", "My_I_Func");
    try testConvert("MyAPIFunc", "My_API_Func");
    try testConvert("LibC", "Lib_C");
    try testConvert("LibCPP", "Lib_CPP");
    try testConvert("LibZ2", "Lib_Z2");
    try testConvert("LibX2StuffY", "Lib_X2_Stuff_Y");
    try testConvert("LLVMLibC", "LLVM_Lib_C");
}

fn testConvert(token: []const u8, expected: []const u8) !void {
    var buf: [100]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    try convert_case(token, fbs.writer());
    try testing.expectEqualStrings(expected, fbs.getWritten());
}

const Tokenizer = struct {
    src: []const u8,
    index: u32,

    const Token = struct {
        bytes: []const u8,
        tag: Tag,

        const Tag = enum {
            ident_ish,
            camel,
            adult_camel,
            other_stuff,
        };
    };

    fn init(src: []const u8) Tokenizer {
        return .{
            .src = src,
            .index = 0,
        };
    }

    const State = enum {
        start,
        ident_ish,
        other_stuff,
    };

    fn next(self: *Tokenizer) ?Token {
        if (self.index == self.src.len) {
            return null;
        }
        var state = State.start;
        const start = self.index;
        var capitalized = false;
        var upper_count: u16 = 0;
        var lower_count: u16 = 0;
        while (true) {
            if (self.index == self.src.len) {
                break;
            }
            const c = self.src[self.index];
            switch (state) {
                .start => {
                    switch (c) {
                        'A'...'Z' => {
                            state = .ident_ish;
                            capitalized = true;
                            upper_count = 1;
                        },
                        'a'...'z' => {
                            state = .ident_ish;
                            lower_count = 1;
                        },
                        '_' => state = .ident_ish,
                        else => state = .other_stuff,
                    }
                    self.index += 1;
                },
                .ident_ish => {
                    switch (c) {
                        'A'...'Z' => upper_count += 1,
                        'a'...'z' => lower_count += 1,
                        '0'...'9', '_' => {},
                        else => break,
                    }
                    self.index += 1;
                },
                .other_stuff => switch (c) {
                    'A'...'Z', 'a'...'z', '_' => break,
                    else => self.index += 1,
                },
            }
        }
        const bytes = self.src[start..self.index];
        switch (state) {
            .start => return null,
            .ident_ish => {
                if (capitalized and lower_count > 0 and upper_count > 1)
                    return Token{ .bytes = bytes, .tag = .adult_camel };
                if (!capitalized and lower_count > 0 and upper_count > 0)
                    return Token{ .bytes = bytes, .tag = .camel };
                return Token{ .bytes = bytes, .tag = .ident_ish };
            },
            .other_stuff => return Token{ .bytes = bytes, .tag = .other_stuff },
        }
    }
};

test "tokenize" {
    try testTokenize(
        \\ [] {} 1234 fn
        \\   babbyFunc.ComplexPappy,x_qwfpgjl
        \\%SixtyFour();
    ,
        &.{
            .{ .tag = .other_stuff, .bytes = " [] {} 1234 " },
            .{ .tag = .ident_ish, .bytes = "fn" },
            .{ .tag = .other_stuff, .bytes = "\n   " },
            .{ .tag = .camel, .bytes = "babbyFunc" },
            .{ .tag = .other_stuff, .bytes = "." },
            .{ .tag = .adult_camel, .bytes = "ComplexPappy" },
            .{ .tag = .other_stuff, .bytes = "," },
            .{ .tag = .ident_ish, .bytes = "x_qwfpgjl" },
            .{ .tag = .other_stuff, .bytes = "\n%" },
            .{ .tag = .adult_camel, .bytes = "SixtyFour" },
            .{ .tag = .other_stuff, .bytes = "();" },
        },
    );
}

fn testTokenize(src: []const u8, expected: []const Tokenizer.Token) !void {
    var list = std.ArrayList(Tokenizer.Token).init(testing.allocator);
    defer list.deinit();
    var tokenizer = Tokenizer.init(src);
    while (tokenizer.next()) |tok| {
        try list.append(tok);
    }
    try testing.expectEqual(expected.len, list.items.len);
    for (list.items, expected) |actual, expect| {
        try testing.expectEqual(expect.tag, actual.tag);
        try testing.expectEqualStrings(expect.bytes, actual.bytes);
    }
}

const testing = std.testing;
