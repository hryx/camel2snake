const std = @import("std");
const fs = std.fs;
const os = std.os;
const log = std.log;
const mem = std.mem;
const Allocator = mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const assert = std.debug.assert;
const testing = std.testing;

const Rule = @import("Rule.zig");

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
        \\  --help-rules            Print help for identifier matching and replacement rules and exit
        \\  --convert-all           Convert identifiers by default, except those explicitly excluded
        \\  --convert=IDENT[=SUB]   Convert instances of IDENT to snake case, replacing with SUB or using default strategy
        \\  --except=IDENT          Do not convert instances of IDENT to snake case
        \\  --load-rules=FILE       Load matching rules from FILE
        \\  --ignore-path=PATH      Do not process file or directory at PATH
        \\  --adult-camels          If combined with --convert-all, additionally convert AdultCamels into Adult_Snakes
        \\  --dry-run               Print files that would have changes (does not modify files)
        \\  --dry-run-highlight     Print files, colorizing affected tokens (does not modify files)
        \\
    ;
    std.io.getStdErr().writeAll(text) catch unreachable;
}

fn rule_help() void {
    const text =
        \\camel2snake identifier matching and replacement rules
        \\
        \\camel2snake identifies all camel case tokens in files it reads,
        \\but by default, it will leave them all unchanged. Beavhior can
        \\be changed either by specifying individual patterns as
        \\command-line options or by pointing to files containing rules.
        \\
        \\Any of the options mentioned below can be specified multiple times.
        \\In all cases, tokens must be valid C/Zig identifiers, i.e. they
        \\must match the regular expression:
        \\
        \\    [A-Za-z_][A-Za-z0-9_]*
        \\
        \\The simplest option is --convert-all, which changes the baseline
        \\behavior from ignoring camel case tokens to replacing them with
        \\snake case equivalents. In other words, this changes the baseline
        \\behavior from opt-in to opt-out. Further options can add exceptions
        \\(tokens to explicitly ignore) or explicit substitution tokens.
        \\
        \\The --except=IDENT option ensures that tokens matching IDENT exactly
        \\will not be altered, even if other options have enabled token
        \\conversion. Explicit exclusions have the highest precedence.
        \\
        \\The --convert=IDENT option specifies a token which should be replaced
        \\using the default camel case -> snake case conversion algorithm.
        \\The form --convert=IDENT=SUB will replace all instances of the token
        \\IDENT with exact value SUB. This can be used for individual overrides
        \\when the default conversion strategy produces undesirable output.
        \\
        \\For anything more complicated, use the --load-rules=FILE option.
        \\The file at path FILE is a plain text file with syntax and behavior
        \\described by the following example.
        \\
        \\    # Comments and blank line are ignored.
        \\    # Comments may appear on their own on the same line as a rule.
        \\
        \\    # Replace all instances of a token with the default strategy.
        \\    myThing
        \\
        \\    # Replace all instances of a token with an explicit value.
        \\    myAPIthing my_api_thing
        \\
        \\    # Ignore all instances of a token.
        \\    # Only the first character may be a '!'.
        \\    !MOVcc # name of opcode
        \\    !macOS # brand name
        \\
        \\    # Wildcards may be used to match all tokens with a certain prefix.
        \\    # Only the last character may be a '*'.
        \\    # Wildcards can only use the default replacement strategy,
        \\    # not explicit replacements.
        \\    HttpError*
        \\
        \\    # Wildcards can also be used in exclusion rules.
        \\    !GLFW*
        \\    !SDL_*
        \\
        \\Wildcards have the lowest precedence; tokens are compared for exact
        \\matches before being checked against wildcard matches. If both a
        \\replacement and exclusion rule for the same wildcard pattern is found,
        \\the program returns an error.
        \\
        \\By default, camel case tokens that start with a capital letter are
        \\ignored. Use --adult-camels *in addition to* --convert-all to match
        \\and convert them as well. This option is not required to match or replace
        \\such tokens if they match an explicit rule as decribed above.
        \\
        \\When in doubt, use the --dry-run-highlight flag to preview the changes
        \\that would take effect with any combination of rules and options.
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

    var action: Converter.Action = .convert;
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
        } else if (mem.eql(u8, arg, "--help-rules")) {
            rule_help();
            return;
        } else if (mem.eql(u8, arg, "--dry-run")) {
            action = .dry_run;
        } else if (mem.eql(u8, arg, "--dry-run-highlight")) {
            action = .highlight;
        } else if (mem.eql(u8, arg, "--adult-camels")) {
            converter.adult_camels = true;
        } else if (mem.eql(u8, arg, "--convert-all")) {
            converter.convert_by_default = true;
        } else if (get_flag_value(arg, "--convert")) |kv| {
            var iter = mem.split(u8, kv, "=");
            const name = iter.first();
            const rep = iter.next(); // Optional replacement token
            _ = converter.register_replacement(name, rep) catch |err| switch (err) {
                error.InvalidIdentifier => {
                    log.err("invalid identifier in --convert: {s} = {s}", .{
                        name,
                        if (rep) |r| r else "(default)",
                    });
                    os.exit(1);
                },
                else => return err,
            };
        } else if (get_flag_value(arg, "--except")) |name| {
            converter.register_exclusion(name) catch |err| switch (err) {
                error.InvalidIdentifier => {
                    log.err("invalid identifier in --except: {s}", .{name});
                    os.exit(1);
                },
                else => return err,
            };
        } else if (get_flag_value(arg, "--load-rules")) |path| {
            const res = try converter.load_rules_from_file(path);
            switch (res) {
                .ok => {},
                .fail => |fail| {
                    log.err("{s}:{}: {s}", .{ path, fail.line, Rule.error_string(fail.err) });
                    os.exit(1);
                },
            }
        } else if (get_flag_value(arg, "--ignore-path")) |path| {
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

    try converter.process_files(paths.items, action);
}

const Converter = struct {
    arena: ArenaAllocator,
    ignore_file_set: std.StringHashMapUnmanaged(void),
    to_convert_file_set: std.StringArrayHashMapUnmanaged(void),
    /// Stores both explicit conversions and cached previous conversions.
    convert_ident_map: std.StringHashMapUnmanaged([]const u8),
    /// Tokens explicitly disallowed from conversion.
    ignore_ident_set: std.StringHashMapUnmanaged(void),
    /// Wildcard pattern storage. Longest rule matches first,
    /// so this must be kept sorted by decsending length.
    /// Wildcard matching is naive and would benefit from a trie
    /// or other data structure when using many wildcards.
    wildcard_rules: std.ArrayListUnmanaged(Wildcard),
    convert_by_default: bool,
    adult_camels: bool,

    const Action = enum {
        dry_run,
        highlight,
        convert,
    };

    const Wildcard = struct {
        prefix: []const u8,
        action: Wildcard.Action,

        const Action = enum { ignore, replace };
    };

    pub fn init(allocator: Allocator) Converter {
        return .{
            .arena = ArenaAllocator.init(allocator),
            .ignore_file_set = .{},
            .to_convert_file_set = .{},
            .convert_ident_map = .{},
            .ignore_ident_set = .{},
            .wildcard_rules = .{},
            .convert_by_default = false,
            .adult_camels = false,
        };
    }

    pub fn deinit(self: *Converter) void {
        self.arena.deinit();
    }

    pub fn process_files(self: *Converter, paths: []const []const u8, action: Action) !void {
        for (paths) |path| {
            try self.scan_path(fs.cwd(), path);
        }
        for (self.to_convert_file_set.keys()) |path| {
            try self.process_file(path, action);
        }
    }

    pub const LoadRulesResult = union(enum) {
        ok,
        fail: Fail,

        pub const Fail = struct {
            line: usize,
            err: Rule.Error,
        };
    };

    pub fn load_rules_from_file(self: *Converter, path: []const u8) !LoadRulesResult {
        const f = try fs.cwd().openFile(path, .{});
        defer f.close();

        var line_buf: [200]u8 = undefined;
        var line_no: usize = 0;
        while (try f.reader().readUntilDelimiterOrEof(&line_buf, '\n')) |line| : (line_no += 1) {
            const rule = Rule.parse(line) catch |err| return LoadRulesResult{ .fail = .{
                .line = line_no,
                .err = err,
            } };
            try self.register_rule(rule orelse continue);
        }
        return LoadRulesResult.ok;
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

    fn process_file(self: *Converter, path: []const u8, action: Action) !void {
        const allocator = self.arena.child_allocator;
        const src = try fs.cwd().readFileAlloc(allocator, path, 10_000_000);
        defer allocator.free(src);

        const std_out = std.io.getStdOut().writer();
        switch (action) {
            .dry_run => {
                if (try self.file_will_change(src)) {
                    try std_out.print("{s}\n", .{path});
                }
            },
            .highlight => {
                try self.write_with_changes(src, std_out, true);
            },
            .convert => {
                if (try self.file_will_change(src)) {
                    const stat = try fs.cwd().statFile(path);
                    var af = try fs.cwd().atomicFile(path, .{ .mode = stat.mode });
                    defer af.deinit();

                    try self.write_with_changes(src, af.file.writer(), false);
                    try af.finish();
                    try std_out.print("{s}\n", .{path});
                }
            },
        }
    }

    fn file_will_change(self: *Converter, src: []const u8) !bool {
        var tokenizer = Tokenizer.init(src);
        while (tokenizer.next()) |tok| {
            switch (tok.tag) {
                .camel, .adult_camel => {
                    if (try self.get_replacement(tok.bytes) != null) {
                        return true;
                    }
                },
                .ident_ish, .other_stuff => {},
            }
        }
        return false;
    }

    fn write_with_changes(self: *Converter, src: []const u8, w: anytype, highlight: bool) !void {
        var bw = std.io.bufferedWriter(w);
        const writer = bw.writer();
        var tokenizer = Tokenizer.init(src);
        while (tokenizer.next()) |tok| {
            switch (tok.tag) {
                .camel, .adult_camel => {
                    if (try self.get_replacement(tok.bytes)) |rep| {
                        if (highlight) {
                            const color = if (tok.tag == .camel) "[32m" else "[33m";
                            try writer.writeByte(0o033);
                            try writer.writeAll(color);
                            try writer.writeAll(rep);
                            try writer.writeByte(0o033);
                            try writer.writeAll("[0m");
                        } else {
                            try writer.writeAll(rep);
                        }
                    } else {
                        try writer.writeAll(tok.bytes);
                    }
                },
                .ident_ish, .other_stuff => {
                    try writer.writeAll(tok.bytes);
                },
            }
        }
        try bw.flush();
    }

    /// Returns null if the token should not be replaced.
    /// Caches result if token has not been seen previously.
    fn get_replacement(self: *Converter, token: []const u8) !?[]const u8 {
        if (self.ignore_ident_set.contains(token)) {
            return null;
        }
        if (self.convert_ident_map.get(token)) |val| {
            return val;
        }
        if (self.get_wildcard_match(token)) |action| switch (action) {
            .replace => return try self.register_replacement(token, null),
            .ignore => {
                try self.register_exclusion(token);
                return null;
            },
        };
        if (!self.convert_by_default) {
            return null;
        }
        if (isUpper(token[0])) {
            if (self.adult_camels) {
                return try self.register_replacement(token, null);
            }
            return null;
        }
        return try self.register_replacement(token, null);
    }

    fn get_wildcard_match(self: *Converter, token: []const u8) ?Wildcard.Action {
        for (self.wildcard_rules.items) |wc| {
            if (mem.startsWith(u8, token, wc.prefix)) return wc.action;
        }
        return null;
    }

    pub fn register_rule(self: *Converter, rule: Rule) !void {
        switch (rule.action) {
            .ignore => {
                _ = try self.register_exclusion(rule.token);
            },
            .ignore_wildcard => {
                try self.register_wildcard(.{ .prefix = rule.token, .action = .ignore });
            },
            .replace => {
                _ = try self.register_replacement(rule.token, null);
            },
            .replace_wildcard => {
                try self.register_wildcard(.{ .prefix = rule.token, .action = .replace });
            },
            .replace_explicit => |sub| {
                _ = try self.register_replacement(rule.token, sub);
            },
        }
    }

    /// Register a new token replacement and return it.
    /// If the replacement is the same as the original token,
    /// registers an exclusion instead and returns null.
    /// If `replacement` is null, creates a new one using the default replacement strategy.
    /// Makes a copy of `token` and `replacement` (if present).
    /// Asserts the token has not already been registered.
    fn register_replacement(
        self: *Converter,
        token: []const u8,
        replacement: ?[]const u8,
    ) !?[]const u8 {
        assert(!self.convert_ident_map.contains(token));

        if (!std.zig.isValidId(token)) return error.InvalidIdentifier;

        if (replacement) |rep| {
            if (!std.zig.isValidId(rep)) return error.InvalidIdentifier;
            if (mem.eql(u8, token, rep)) {
                try self.register_exclusion(token);
                return null;
            }
            const token_copy = try self.arena.allocator().dupe(u8, token);
            try self.convert_ident_map.put(self.arena.allocator(), token_copy, rep);
            return rep;
        }

        var buf: [200]u8 = undefined;
        var fbs = std.io.fixedBufferStream(&buf);
        try convert_case(token, fbs.writer());
        const rep = fbs.getWritten();

        if (mem.eql(u8, token, rep)) {
            try self.register_exclusion(token);
            return null;
        }

        const token_copy = try self.arena.allocator().dupe(u8, token);
        const rep_copy = try self.arena.allocator().dupe(u8, rep);
        try self.convert_ident_map.put(self.arena.allocator(), token_copy, rep_copy);
        return rep_copy;
    }

    /// Makes a copy of `token`.
    fn register_exclusion(self: *Converter, token: []const u8) !void {
        if (!std.zig.isValidId(token)) return error.InvalidIdentifier;
        const token_copy = try self.arena.allocator().dupe(u8, token);
        try self.ignore_ident_set.put(self.arena.allocator(), token_copy, {});
    }

    /// Insert the rule and maintain lexicographical order.
    /// No-op if rule already exists.
    /// Error if new rule conflicts with an existing rule.
    /// Makes a copy of the token if the rule is registered.
    fn register_wildcard(self: *Converter, wc: Wildcard) !void {
        if (!std.zig.isValidId(wc.prefix)) return error.InvalidIdentifier;
        for (self.wildcard_rules.items, 0..) |existing, i| {
            switch (mem.order(u8, existing.prefix, wc.prefix)) {
                .eq => {
                    if (existing.action == wc.action) return; // no-op
                    return error.WildcardRuleConflict;
                },
                .lt => {
                    const copy = try self.arena.allocator().dupe(u8, wc.prefix);
                    try self.wildcard_rules.insert(self.arena.allocator(), i, .{
                        .prefix = copy,
                        .action = wc.action,
                    });
                    return;
                },
                .gt => {},
            }
        }
        const copy = try self.arena.allocator().dupe(u8, wc.prefix);
        try self.wildcard_rules.append(self.arena.allocator(), .{
            .prefix = copy,
            .action = wc.action,
        });
    }
};

const isUpper = std.ascii.isUpper;
const isLower = std.ascii.isLower;
const toLower = std.ascii.toLower;

fn convert_case(token: []const u8, writer: anytype) !void {
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

test "wildcard match" {
    var c = Converter.init(testing.allocator);
    defer c.deinit();
    try c.register_wildcard(.{ .prefix = "x", .action = .replace });
    try c.register_wildcard(.{ .prefix = "M", .action = .replace });
    try c.register_wildcard(.{ .prefix = "MOV", .action = .ignore });
    try c.register_wildcard(.{ .prefix = "ffffff", .action = .ignore });
    try c.register_wildcard(.{ .prefix = "chan", .action = .replace });
    try c.register_wildcard(.{ .prefix = "MOVIE", .action = .replace });
    try testWildcardMatch(&c, "hooba", null);
    try testWildcardMatch(&c, "changeMe", .replace);
    try testWildcardMatch(&c, "chonge", null);
    try testWildcardMatch(&c, "cha", null);
    try testWildcardMatch(&c, "MOVcc", .ignore);
    try testWildcardMatch(&c, "MOV", .ignore);
    try testWildcardMatch(&c, "MOVI", .ignore);
    try testWildcardMatch(&c, "MOVIE", .replace);
    try testWildcardMatch(&c, "MO", .replace);
    try testWildcardMatch(&c, "M", .replace);
}

fn testWildcardMatch(c: *Converter, token: []const u8, expected: ?Converter.Wildcard.Action) !void {
    try testing.expectEqual(expected, c.get_wildcard_match(token));
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
        number_ish,
        ident_ish,
        backslash,
        unicode_escape,
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
                        '0'...'9' => state = .number_ish,
                        '\\' => state = .backslash,
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
                .unicode_escape => switch (c) {
                    '}' => {
                        self.index += 1;
                        break;
                    },
                    else => self.index += 1,
                },
                .other_stuff => switch (c) {
                    'A'...'Z', 'a'...'z', '0'...'9', '_' => break,
                    '\\' => break,
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
            .other_stuff,
            .number_ish,
            .backslash,
            .unicode_escape,
            => return Token{ .bytes = bytes, .tag = .other_stuff },
        }
    }
};

test "tokenize" {
    try testTokenize(
        \\ [] {} 0x888.f000 :0x12Ab34Cd fn
        \\   babbyFunc.ComplexPappy,x_qwfpgjl
        \\%SixtyFour5();
        \\ \u{Ab02}\xFF
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

test {
    _ = Rule;
}
