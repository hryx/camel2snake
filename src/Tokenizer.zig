const std = @import("std");

const Tokenizer = @This();

src: []const u8,
index: u32,

pub const Token = struct {
    bytes: []const u8,
    tag: Tag,

    const Tag = enum {
        ident_ish,
        camel,
        adult_camel,
        builtin,
        other_stuff,
    };
};

pub fn init(src: []const u8) Tokenizer {
    return .{
        .src = src,
        .index = 0,
    };
}

const State = enum {
    start,
    number_ish,
    ident_ish,
    backslash,
    at,
    builtin,
    unicode_escape,
    other_stuff,
};

pub fn next(self: *Tokenizer) ?Token {
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
                    '0'...'9' => state = .number_ish,
                    '\\' => state = .backslash,
                    '@' => state = .at,
                    else => state = .other_stuff,
                }
                self.index += 1;
            },
            .number_ish => {
                switch (c) {
                    'A'...'Z', 'a'...'z', '0'...'9', '_', '.' => self.index += 1,
                    else => break,
                }
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
            .backslash => switch (c) {
                'x' => {
                    if (self.index + 2 < self.src.len) {
                        self.index += 3;
                    }
                    break;
                },
                'u' => {
                    state = .unicode_escape;
                    self.index += 1;
                },
                else => {
                    self.index += 1;
                    break;
                },
            },
            .at => switch (c) {
                'A'...'Z', 'a'...'z' => {
                    state = .builtin;
                    self.index += 1;
                },
                else => break,
            },
            .builtin => {
                switch (c) {
                    'A'...'Z', 'a'...'z', '0'...'9', '_' => {},
                    else => break,
                }
                self.index += 1;
            },
            .unicode_escape => switch (c) {
                '}' => {
                    self.index += 1;
                    break;
                },
                else => self.index += 1,
            },
            .other_stuff => switch (c) {
                'A'...'Z', 'a'...'z', '0'...'9', '_', '\\', '@' => break,
                else => self.index += 1,
            },
        }
    }
    const bytes = self.src[start..self.index];
    switch (state) {
        .start => return null,
        .ident_ish => {
            // Zig keywords are never camel case, and removing them from the
            // pool now simplifies things later on.
            if (std.zig.Token.getKeyword(bytes) != null) {
                return Token{ .bytes = bytes, .tag = .ident_ish };
            }
            if (capitalized and lower_count > 0 and upper_count > 1) {
                return Token{ .bytes = bytes, .tag = .adult_camel };
            }
            if (!capitalized and lower_count > 0 and upper_count > 0) {
                return Token{ .bytes = bytes, .tag = .camel };
            }
            return Token{ .bytes = bytes, .tag = .ident_ish };
        },
        .builtin => {
            return Token{ .bytes = bytes, .tag = .builtin };
        },
        .other_stuff,
        .number_ish,
        .backslash,
        .at,
        .unicode_escape,
        => return Token{ .bytes = bytes, .tag = .other_stuff },
    }
}

test "tokenize" {
    try testTokenize(
        \\ [] {} 0x888.f000 :0x12Ab34Cd fn
        \\   babbyFunc.ComplexPappy,x_qwfpgjl
        \\%SixtyFour5();
        \\ \u{Ab02}\xFF@@popCount?extern
    ,
        &.{
            .{ .tag = .other_stuff, .bytes = " [] {} " },
            .{ .tag = .other_stuff, .bytes = "0x888.f000" },
            .{ .tag = .other_stuff, .bytes = " :" },
            .{ .tag = .other_stuff, .bytes = "0x12Ab34Cd" },
            .{ .tag = .other_stuff, .bytes = " " },
            .{ .tag = .ident_ish, .bytes = "fn" },
            .{ .tag = .other_stuff, .bytes = "\n   " },
            .{ .tag = .camel, .bytes = "babbyFunc" },
            .{ .tag = .other_stuff, .bytes = "." },
            .{ .tag = .adult_camel, .bytes = "ComplexPappy" },
            .{ .tag = .other_stuff, .bytes = "," },
            .{ .tag = .ident_ish, .bytes = "x_qwfpgjl" },
            .{ .tag = .other_stuff, .bytes = "\n%" },
            .{ .tag = .adult_camel, .bytes = "SixtyFour5" },
            .{ .tag = .other_stuff, .bytes = "();\n " },
            .{ .tag = .other_stuff, .bytes = "\\u{Ab02}" },
            .{ .tag = .other_stuff, .bytes = "\\xFF" },
            .{ .tag = .other_stuff, .bytes = "@" },
            .{ .tag = .builtin, .bytes = "@popCount" },
            .{ .tag = .other_stuff, .bytes = "?" },
            .{ .tag = .ident_ish, .bytes = "extern" },
        },
    );
}

const testing = std.testing;

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
