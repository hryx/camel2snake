const std = @import("std");
const fs = std.fs;
const os = std.os;
const log = std.log;
const mem = std.mem;
const fmt = std.fmt;

const Converter = @import("Converter.zig");
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
        \\  -h, --help                   Print this help and exit
        \\  --help-rules                 Print help for identifier matching and replacement rules and exit
        \\  --convert-all                Convert identifiers by default, except those explicitly excluded
        \\  --convert=IDENT[=SUB]        Convert instances of IDENT to snake case, replacing with SUB or using default strategy
        \\  --except=IDENT               Do not convert instances of IDENT to snake case
        \\  --convert-builtins           Process Zig @builtins (ignored by default)
        \\  --load-rules=FILE            Load matching rules from FILE
        \\  --ignore-path=NAME           Do not process files or directories matching NAME
        \\  --adult-camels               If combined with --convert-all, additionally convert AdultCamels into Adult_Snakes
        \\  --fixup-compile-error-tests  Adjust expected errors in Zig compile error test suite
        \\  --allow-non-zig-files        Process files that don't have a .zig extension
        \\  --dry-run                    Print files that would have changes (does not modify files)
        \\  --dry-run-highlight          Print files, colorizing affected tokens (does not modify files)
        \\  --max-file-kb=SIZE           Maximum file size in kilobytes (default 10000)
        \\  --stats                      When finished, print some information about changes to stderr
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
        \\Zig builtins like @popCount() are ignored. To process them, use
        \\--convert-builtins. This flag can be combined with --adult-camels
        \\to change builtins like @TypeOf(). Builtins cannot have explicit
        \\conversions, only the default one.
        \\
        \\Zig keywords are never camel case so they are always ignored.
        \\Adding them to a rule list or --convert option will have no effect.
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
    var max_file_size: u32 = 10_000_000;
    var print_stats = false;
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
        } else if (mem.eql(u8, arg, "--fixup-compile-error-tests")) {
            converter.fixup_compile_error_tests = true;
        } else if (mem.eql(u8, arg, "--convert-all")) {
            converter.convert_by_default = true;
        } else if (mem.eql(u8, arg, "--allow-non-zig-files")) {
            converter.process_non_zig_files = true;
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
        } else if (mem.eql(u8, arg, "--convert-builtins")) {
            converter.process_builtins = true;
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
            try converter.add_ignore_path(path);
        } else if (get_flag_value(arg, "--max-file-kb")) |val| {
            max_file_size = try fmt.parseUnsigned(u32, val, 10) * 1000;
        } else if (mem.eql(u8, arg, "--stats")) {
            print_stats = true;
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

    var timer = try std.time.Timer.start();
    const start_time = timer.read();

    defer {
        const end_time = timer.lap();
        const time_diff = end_time - start_time;
        const seconds = @intToFloat(f64, time_diff) / std.time.ns_per_s;
        if (print_stats) {
            const counts = converter.count_changed_files();
            const std_err = std.io.getStdErr().writer();
            std_err.print("Summary of changes:\n", .{}) catch {};
            std_err.print("  Time taken: {d:.5} seconds\n", .{seconds}) catch {};
            std_err.print("  Files found: {}\n", .{converter.to_convert_file_set.count()}) catch {};
            std_err.print("  Files changed: {}\n", .{counts.converted}) catch {};
            std_err.print("  Camel case tokens found:\n", .{}) catch {};
            std_err.print("    Lower: {}\n", .{converter.babbies_found}) catch {};
            std_err.print("    Upper: {}\n", .{converter.pappies_found}) catch {};
            std_err.print("    Total: {}\n", .{converter.babbies_found + converter.pappies_found}) catch {};
            std_err.print("  Camel case tokens changed:\n", .{}) catch {};
            std_err.print("    Lower: {}\n", .{converter.babbies_changed}) catch {};
            std_err.print("    Upper: {}\n", .{converter.pappies_changed}) catch {};
            std_err.print("    Total: {}\n", .{converter.babbies_changed + converter.pappies_changed}) catch {};
            std_err.print("  Unique camel case tokens registered:\n", .{}) catch {};
            std_err.print("    Converted: {}\n", .{converter.unique_replacements}) catch {};
            std_err.print("    Excluded or canonical: {}\n", .{converter.unique_exclusions}) catch {};
            std_err.print("    Total: {}\n", .{converter.unique_replacements + converter.unique_exclusions}) catch {};
            std_err.print("  Error tests fixed up: {}\n", .{counts.fixuped}) catch {};
        }
    }

    converter.process_files(paths.items, action, max_file_size) catch |err| {
        log.err("last file in progess: {s}", .{converter.last_file_in_progress});
        switch (err) {
            error.FileTooBig => log.err("file was too big, consider using --max-file-kb", .{}),
            else => {},
        }
        return err;
    };
}

test {
    _ = Converter;
    _ = Rule;
}
