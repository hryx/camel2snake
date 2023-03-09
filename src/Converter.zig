const std = @import("std");
const fs = std.fs;
const mem = std.mem;
const fmt = std.fmt;
const Allocator = mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const assert = std.debug.assert;
const testing = std.testing;

const Rule = @import("Rule.zig");
const Tokenizer = @import("Tokenizer.zig");

const Converter = @This();

arena: ArenaAllocator,
/// Patterns to ignore. Can be either a file or directory name.
/// Any path ending in this will be ignored, so e.g. "zig-cache" will match
/// "zig-cache", "test/something/zig-cache", "zig-cache", etc.
/// Names are prefixed with filesystem separator to make matching simpler.
ignore_paths: std.ArrayListUnmanaged([]const u8),
to_convert_file_set: std.StringArrayHashMapUnmanaged(enum { pending, skipped, converted, fixuped }),
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
process_builtins: bool,
process_non_zig_files: bool,
fixup_compile_error_tests: bool,
/// Record path of file being checked so caller can report errors.
last_file_in_progress: []const u8,
line_increases: LineIncreases.Map,

babbies_found: u32 = 0,
babbies_changed: u32 = 0,
pappies_found: u32 = 0,
pappies_changed: u32 = 0,
unique_exclusions: u32 = 0,
unique_replacements: u32 = 0,

const LineIncreases = struct {
    const Map = std.ArrayHashMapUnmanaged(K, std.ArrayListUnmanaged(V), LineIncreases, false);

    pub fn hash(_: LineIncreases, key: K) u32 {
        const str_hash = @truncate(u32, std.hash_map.hashString(key.path));
        return str_hash ^ key.line;
    }

    pub fn eql(_: LineIncreases, a: K, b: K, _: usize) bool {
        if (a.line != b.line) return false;
        return mem.eql(u8, a.path, b.path);
    }

    const K = struct {
        path: []const u8,
        line: u32,
    };
    const V = struct {
        column: u32,
        bytes_added: u16,
    };
};

pub const Action = enum {
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
        .ignore_paths = .{},
        .to_convert_file_set = .{},
        .convert_ident_map = .{},
        .ignore_ident_set = .{},
        .wildcard_rules = .{},
        .convert_by_default = false,
        .adult_camels = false,
        .fixup_compile_error_tests = false,
        .process_builtins = false,
        .process_non_zig_files = false,
        .last_file_in_progress = "",
        .line_increases = .{},
    };
}

pub fn deinit(self: *Converter) void {
    self.arena.deinit();
}

pub fn add_ignore_path(self: *Converter, file_or_dir_name: []const u8) !void {
    const pattern = try fs.path.resolve(self.arena.allocator(), &.{ "/", file_or_dir_name });
    try self.ignore_paths.append(self.arena.allocator(), pattern);
}

fn should_ignore(self: *Converter, abs_path: []const u8) bool {
    for (self.ignore_paths.items) |suffix| {
        if (mem.endsWith(u8, abs_path, suffix)) return true;
    }
    return false;
}

test should_ignore {
    var c = Converter.init(testing.allocator);
    defer c.deinit();
    try c.add_ignore_path("zig-cache");
    try c.add_ignore_path("./myfile.zig");
    try c.add_ignore_path("/////other");
    try c.add_ignore_path("sub/special");
    try testing.expect(!c.should_ignore("zig-cache"));
    try testing.expect(c.should_ignore("/zig-cache"));
    try testing.expect(!c.should_ignore("/zig-cache2"));
    try testing.expect(c.should_ignore("/home/you/zig-cache"));
    try testing.expect(c.should_ignore("/home/you/myfile.zig"));
    try testing.expect(!c.should_ignore("/home/you/amyfile.zig"));
    try testing.expect(c.should_ignore("/other"));
    try testing.expect(c.should_ignore("../other"));
    try testing.expect(!c.should_ignore("/another"));
    try testing.expect(!c.should_ignore("/other.zig"));
    try testing.expect(c.should_ignore("/tmp/sub/special"));
    try testing.expect(!c.should_ignore("/tmp/sub"));
    try testing.expect(!c.should_ignore("/tmp/special"));
}

pub fn process_files(
    self: *Converter,
    paths: []const []const u8,
    action: Action,
    max_file_size: u32,
) !void {
    for (paths) |path| {
        try self.scan_path(fs.cwd(), path);
    }
    for (self.to_convert_file_set.keys()) |path| {
        try self.process_file(path, action, max_file_size);
    }
    if (self.fixup_compile_error_tests) {
        try self.apply_all_compile_error_test_fixups(max_file_size);
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
    self.last_file_in_progress = path;

    const abs_path = try dir.realpathAlloc(self.arena.allocator(), path);

    if (self.should_ignore(abs_path)) return;

    const stat = try dir.statFile(abs_path);
    switch (stat.kind) {
        .File => {
            if (mem.eql(u8, fs.path.extension(abs_path), ".zig") or self.process_non_zig_files) {
                try self.to_convert_file_set.put(self.arena.allocator(), abs_path, .pending);
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

fn process_file(
    self: *Converter,
    path: []const u8,
    action: Action,
    max_file_size: u32,
) !void {
    self.last_file_in_progress = path;

    const allocator = self.arena.child_allocator;
    const src = try fs.cwd().readFileAlloc(allocator, path, max_file_size);
    defer allocator.free(src);

    const std_out = std.io.getStdOut().writer();

    if (!try self.file_will_change(src)) {
        self.to_convert_file_set.putAssumeCapacity(path, .skipped);
        return;
    }

    switch (action) {
        .dry_run => {
            try std_out.print("convert {s}\n", .{path});
        },
        .highlight => {
            try self.write_with_changes(src, std_out, true);
        },
        .convert => {
            const stat = try fs.cwd().statFile(path);
            var af = try fs.cwd().atomicFile(path, .{ .mode = stat.mode });
            defer af.deinit();

            try self.write_with_changes(src, af.file.writer(), false);
            try af.finish();
            try std_out.print("convert {s}\n", .{path});
        },
    }

    self.to_convert_file_set.putAssumeCapacity(path, .converted);
}

const ChangeCounts = struct {
    skipped: u32 = 0,
    converted: u32 = 0,
    fixuped: u32 = 0,
};
pub fn count_changed_files(self: *Converter) ChangeCounts {
    var iter = self.to_convert_file_set.iterator();
    var counts = ChangeCounts{};
    while (iter.next()) |item| {
        switch (item.value_ptr.*) {
            .pending => unreachable,
            .skipped => counts.skipped += 1,
            .converted => counts.converted += 1,
            .fixuped => {
                counts.converted += 1;
                counts.fixuped += 1;
            },
        }
    }
    return counts;
}

fn file_will_change(self: *Converter, src: []const u8) !bool {
    var tokenizer = Tokenizer.init(src);
    var changed = false;
    while (tokenizer.next()) |tok| {
        switch (tok.tag) {
            .camel => {
                self.babbies_found += 1;
                if (try self.get_replacement(tok.bytes) != null) {
                    self.babbies_changed += 1;
                    changed = true;
                }
            },
            .adult_camel => {
                self.pappies_found += 1;
                if (try self.get_replacement(tok.bytes) != null) {
                    self.pappies_changed += @boolToInt(self.adult_camels);
                    changed = true;
                }
            },
            .builtin => {
                if (!self.process_builtins) continue;
                if (isUpper(tok.bytes[1])) {
                    self.pappies_found += 1;
                    changed = changed or (self.adult_camels and builtin_will_change(tok.bytes));
                } else {
                    self.babbies_found += 1;
                    changed = changed or builtin_will_change(tok.bytes);
                }
            },
            .ident_ish, .other_stuff => {},
        }
    }
    return changed;
}

/// old_tok must be a slice of src, not a copy.
/// For builtins, only pass the name portion and set is_builtin to true.
///
/// TODO: Calling get_slice_location() for every camel token is O(N²).
fn track_change(
    self: *Converter,
    src: []const u8,
    old_tok: []const u8,
    new_tok: []const u8,
    is_builtin: bool,
) !void {
    const diff = new_tok.len - old_tok.len;
    if (diff == 0) return;

    var loc = get_slice_location(src, old_tok);
    if (is_builtin) {
        assert(loc.column != 0);
        assert((old_tok.ptr - 1)[0] == '@');
        loc.column -= 1;
    }
    const gop = try self.line_increases.getOrPut(self.arena.allocator(), .{
        .path = self.last_file_in_progress,
        .line = @intCast(u32, loc.line),
    });
    if (!gop.found_existing) {
        gop.value_ptr.* = .{};
    }
    try gop.value_ptr.append(self.arena.allocator(), .{
        .column = @intCast(u32, loc.column),
        .bytes_added = @intCast(u16, diff),
    });
}

fn write_with_changes(self: *Converter, src: []const u8, w: anytype, highlight: bool) !void {
    var bw = std.io.bufferedWriter(w);
    const writer = bw.writer();
    var tokenizer = Tokenizer.init(src);
    while (tokenizer.next()) |tok| {
        switch (tok.tag) {
            .camel, .adult_camel => {
                if (try self.get_replacement(tok.bytes)) |rep| {
                    try self.track_change(src, tok.bytes, rep, false);

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
            .builtin => {
                const adult = isUpper(tok.bytes[1]);
                if (self.process_builtins and (!adult or self.adult_camels) and builtin_will_change(tok.bytes)) {
                    var buf: [30]u8 = undefined;
                    var fbs = std.io.fixedBufferStream(&buf);
                    try convert_case(tok.bytes[1..], fbs.writer());
                    const rep = fbs.getWritten();
                    try self.track_change(src, tok.bytes[1..], rep, true);

                    if (highlight) {
                        const color = if (adult) "[35m" else "[36m";
                        try writer.writeByte(0o033);
                        try writer.writeAll(color);
                        try writer.writeByte('@');
                        try writer.writeAll(rep);
                        try writer.writeByte(0o033);
                        try writer.writeAll("[0m");
                    } else {
                        try writer.writeByte('@');
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
pub fn register_replacement(
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
        self.unique_replacements += 1;
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

    self.unique_replacements += 1;
    const token_copy = try self.arena.allocator().dupe(u8, token);
    const rep_copy = try self.arena.allocator().dupe(u8, rep);
    try self.convert_ident_map.put(self.arena.allocator(), token_copy, rep_copy);
    return rep_copy;
}

/// Makes a copy of `token`.
pub fn register_exclusion(self: *Converter, token: []const u8) !void {
    if (!std.zig.isValidId(token)) return error.InvalidIdentifier;
    self.unique_exclusions += 1;
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

/// Parse error message comments like those found in test/cases/compile_errors
/// and update the expected column numbers based on how the new source has shifted
/// due to changing camel case to snake case. Files not formatted in the expected
/// way are ignored, and error messages that refer to unaffected lines are ignored.
fn apply_all_compile_error_test_fixups(self: *Converter, max_file_size: u32) !void {
    var file_iter = self.to_convert_file_set.iterator();
    while (file_iter.next()) |item| {
        if (item.value_ptr.* != .converted) continue;
        const path = item.key_ptr.*;
        const src = try fs.cwd().readFileAlloc(self.arena.child_allocator, path, max_file_size);
        defer self.arena.child_allocator.free(src);

        var af = try fs.cwd().atomicFile(path, .{});
        defer af.deinit();

        self.last_file_in_progress = path;
        const changed = try self.apply_compile_error_test_fixups(src, af.file.writer());
        if (changed) {
            try std.io.getStdOut().writer().print("fixup {s}\n", .{path});
            self.to_convert_file_set.putAssumeCapacity(path, .fixuped);
            try af.finish();
        }
    }
}

fn apply_compile_error_test_fixups(self: *Converter, src: []const u8, w: anytype) !bool {
    var bw = std.io.bufferedWriter(w);
    const writer = bw.writer();

    const Change = struct {
        err_msg_line: u32, // 0-indexed
        reported_line: u32, // 0-indexed
        new_column: u32, // 0-indexed
        line_prefix: []const u8, // everything before ":line:col"
        line_remainder: []const u8, // everything after ":line:col"
    };
    var changes = std.ArrayList(Change).init(self.arena.child_allocator);
    defer changes.deinit();

    const path = self.last_file_in_progress;

    var lines_backwards = mem.splitBackwards(u8, src, "\n");
    while (lines_backwards.next()) |line| {
        if (!mem.startsWith(u8, mem.trimLeft(u8, line, " "), "//")) continue;
        const line_err_start = mem.trimLeft(u8, line, " /");
        if (line_err_start.len == 0) continue;
        const line_err_start_offset = @ptrToInt(line_err_start.ptr) - @ptrToInt(line.ptr);
        const line_prefix = line[0 .. line_err_start_offset + 1];

        var number_iter = mem.tokenize(u8, line_err_start, ":");

        const err_line_str = number_iter.next() orelse continue;
        const err_line = if (fmt.parseInt(u32, err_line_str, 10)) |line_no| b: {
            if (line_no == 0) continue; // Error messages are 1-indexed, should not occur
            break :b line_no - 1;
        } else |_| continue;

        // Don't fix up the error message comments themselves
        const err_msg_line = @intCast(u32, get_slice_location(src, line).line);
        if (err_line == err_msg_line) continue;

        const err_col_str = number_iter.next() orelse continue;
        const err_col = if (fmt.parseInt(u32, err_col_str, 10)) |col_no| b: {
            if (col_no == 0) continue; // Error messages are 1-indexed, should not occur
            break :b col_no - 1;
        } else |_| continue;

        var new_err_col = err_col;
        const incrs = self.line_increases.get(.{ .path = path, .line = err_line }) orelse continue;
        for (incrs.items) |incr| {
            if (incr.column < err_col) {
                new_err_col += incr.bytes_added;
            }
        }
        if (new_err_col == err_col) continue;

        try changes.append(.{
            .err_msg_line = err_msg_line,
            .reported_line = err_line,
            .new_column = new_err_col,
            .line_prefix = line_prefix,
            .line_remainder = number_iter.rest(),
        });
    }

    if (changes.items.len == 0) return false;

    var lines = mem.split(u8, src, "\n");
    var next_change = changes.popOrNull();
    var line_i: u32 = 0;
    while (lines.next()) |line| : (line_i += 1) {
        const change = next_change orelse {
            try writer.writeAll(line);
            try writer.writeByte('\n');
            try writer.writeAll(lines.rest());
            break;
        };
        if (line_i == change.err_msg_line) {
            // "// :line:col: ..."
            try writer.writeAll(change.line_prefix);
            try fmt.formatInt(change.reported_line + 1, 10, .lower, .{}, writer);
            try writer.writeByte(':');
            try fmt.formatInt(change.new_column + 1, 10, .lower, .{}, writer);
            try writer.writeByte(':');
            try writer.writeAll(change.line_remainder);
            try writer.writeByte('\n');
            next_change = changes.popOrNull();
        } else {
            try writer.writeAll(line);
            try writer.writeByte('\n');
        }
    } else try writer.writeAll(lines.rest()); // trailing newlines

    try bw.flush();
    return true;
}

test "convert and apply error test fixups" {
    var c = Converter.init(testing.allocator);
    defer c.deinit();

    c.last_file_in_progress = "/tmp/fake/food.zig";
    c.process_builtins = true;

    const src =
        \\
        \\
        \\const x = 100000;
        \\const y = @intCast(u10, @intCast(u9, @as(u8, x)));
        \\
        \\
        \\fn heck() void { return null; }
        \\fn clob() usize { return @popCount(z); }
        \\
        \\// :4:38: error: wacky coercion
        \\//  :4:38: note: it donut fit in a u8
        \\//:7:25: error: expected 'void', found 'null literal'
        \\// :8:36: error: undeclared identifier 'z'
        \\// :?:101: note: more info over here
        \\
    ;

    var buf1: [src.len + 10]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf1);
    try c.write_with_changes(src, fbs.writer(), false);
    const altered = fbs.getWritten();

    try testing.expectEqualStrings(
        \\
        \\
        \\const x = 100000;
        \\const y = @int_cast(u10, @int_cast(u9, @as(u8, x)));
        \\
        \\
        \\fn heck() void { return null; }
        \\fn clob() usize { return @pop_count(z); }
        \\
        \\// :4:38: error: wacky coercion
        \\//  :4:38: note: it donut fit in a u8
        \\//:7:25: error: expected 'void', found 'null literal'
        \\// :8:36: error: undeclared identifier 'z'
        \\// :?:101: note: more info over here
        \\
    , altered);

    var buf2: [src.len + 10]u8 = undefined;
    fbs = std.io.fixedBufferStream(&buf2);
    const changed = try c.apply_compile_error_test_fixups(altered, fbs.writer());
    try testing.expect(changed);
    const fixuped = fbs.getWritten();

    try testing.expectEqualStrings(
        \\
        \\
        \\const x = 100000;
        \\const y = @int_cast(u10, @int_cast(u9, @as(u8, x)));
        \\
        \\
        \\fn heck() void { return null; }
        \\fn clob() usize { return @pop_count(z); }
        \\
        \\// :4:40: error: wacky coercion
        \\//  :4:40: note: it donut fit in a u8
        \\//:7:25: error: expected 'void', found 'null literal'
        \\// :8:37: error: undeclared identifier 'z'
        \\// :?:101: note: more info over here
        \\
    , fixuped);
}

fn get_slice_location(in_src: []const u8, token: []const u8) std.zig.Loc {
    const byte_offset = @ptrToInt(token.ptr) - @ptrToInt(in_src.ptr);
    return std.zig.findLineColumn(in_src, byte_offset);
}

/// Assumes the builtin is either a valid camel case Zig builtin
/// or one that has already been converted to snake case.
fn builtin_will_change(token: []const u8) bool {
    assert(token[0] == '@');
    if (token.len < 2) return false;
    for (token[2..]) |c| {
        if (c == '_') return false;
        if (isUpper(c)) return true;
    }
    return false;
}

test "builtin camels" {
    for (&[_][]const u8{ "@as", "@extern", "@This" }) |word| {
        try testing.expectEqual(false, builtin_will_change(word));
    }
    for (&[_][]const u8{ "@popCount", "@intCast", "@cImport", "@TypeOf" }) |word| {
        try testing.expectEqual(true, builtin_will_change(word));
    }
}

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

test {
    _ = Tokenizer;
}
