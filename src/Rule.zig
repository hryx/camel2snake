const std = @import("std");
const mem = std.mem;
const testing = std.testing;

const Rule = @This();

token: []const u8,
action: Action,

pub const Action = union(enum) {
    ignore,
    ignore_wildcard,
    replace,
    replace_explicit: []const u8,
    replace_wildcard,
};

pub const Error = error{
    BangOnlyRule,
    WildcardOnlyRule,
    WildcardWithExplicitReplace,
    IgnoreWithExplicitReplace,
    TooManyArguments,
    InvalidIdentifier,
};

/// Parse a single line of text from a rule file or command line option.
/// Returns null if there is no meaningful information (empty or comment-only line).
/// Returns an error for malformed rules.
/// Assumes 'line' contains no newlines.
pub fn parse(line: []const u8) Error!?Rule {
    var rule = Rule{
        .token = undefined,
        .action = .replace,
    };
    var tokenizer = mem.tokenize(u8, line, " ");
    rule.token = tokenizer.next() orelse return null;
    if (rule.token[0] == '#') return null;
    if (rule.token[0] == '!') {
        if (rule.token.len < 2) return error.BangOnlyRule;
        rule.action = .ignore;
        rule.token = rule.token[1..];
    }
    if (rule.token[rule.token.len - 1] == '*') {
        if (rule.token.len < 2) return error.WildcardOnlyRule;
        rule.action = switch (rule.action) {
            .replace => .replace_wildcard,
            .ignore => .ignore_wildcard,
            else => unreachable,
        };
        rule.token = rule.token[0 .. rule.token.len - 1];
    }
    if (!std.zig.isValidId(rule.token)) return error.InvalidIdentifier;
    if (tokenizer.next()) |rep| {
        if (rep[0] == '#') return rule;
        switch (rule.action) {
            .replace_wildcard => return error.WildcardWithExplicitReplace,
            .ignore, .ignore_wildcard => return error.IgnoreWithExplicitReplace,
            else => {},
        }
        if (!std.zig.isValidId(rep)) return error.InvalidIdentifier;
        rule.action = .{ .replace_explicit = rep };
    }
    if (tokenizer.next()) |word| {
        if (word[0] == '#') return rule;
        return error.TooManyArguments;
    }
    return rule;
}

// https://github.com/ziglang/zig/issues/2473
pub fn get_rule_error(err: anyerror) ?Error {
    return switch (err) {
        error.BangOnlyRule,
        error.WildcardOnlyRule,
        error.WildcardWithExplicitReplace,
        error.IgnoreWithExplicitReplace,
        error.TooManyArguments,
        error.InvalidIdentifier,
        => @errSetCast(Error, err),
        else => null,
    };
}

pub fn error_string(err: Error) []const u8 {
    return switch (err) {
        error.BangOnlyRule => "match rule only contains a negation but no identifier",
        error.WildcardOnlyRule => "match rules cannot only be a '*'; use --convert-all to convert identifiers by default",
        error.WildcardWithExplicitReplace => "match rule with wildcard may not contain an explicit replacement",
        error.IgnoreWithExplicitReplace => "ignore match rules may not contain an explicit replacement",
        error.TooManyArguments => "match rule contains too many arguments",
        error.InvalidIdentifier => "match rule contains invalid identifier",
    };
}

fn testEqualRule(line: []const u8, expected: Rule) !void {
    const actual = (try Rule.parse(line)).?;
    try testing.expectEqualDeep(expected, actual);
}

test "rules" {
    try testing.expectEqual(@as(?Rule, null), try Rule.parse(""));
    try testing.expectEqual(@as(?Rule, null), try Rule.parse("    # hi"));
    try testing.expectError(error.TooManyArguments, Rule.parse("a b c"));
    try testing.expectError(error.InvalidIdentifier, Rule.parse("$bad"));
    try testing.expectError(error.InvalidIdentifier, Rule.parse(".bad ok"));
    try testing.expectError(error.InvalidIdentifier, Rule.parse("ok 0bad"));
    try testing.expectError(error.BangOnlyRule, Rule.parse(" !"));
    try testing.expectError(error.WildcardOnlyRule, Rule.parse("*"));
    try testing.expectError(error.WildcardOnlyRule, Rule.parse("!*"));
    try testing.expectError(error.WildcardWithExplicitReplace, Rule.parse("Hi* bad"));
    try testing.expectError(error.IgnoreWithExplicitReplace, Rule.parse("!Mother other"));
    try testing.expectError(error.IgnoreWithExplicitReplace, Rule.parse("!wild* card"));
    try testEqualRule("aBc #look", .{
        .token = "aBc",
        .action = .replace,
    });
    try testEqualRule("  aBc    one_two_three  ", .{
        .token = "aBc",
        .action = .{ .replace_explicit = "one_two_three" },
    });
    try testEqualRule(" anyThingWith*", .{
        .token = "anyThingWith",
        .action = .replace_wildcard,
    });
    try testEqualRule("  !NoTouch ", .{
        .token = "NoTouch",
        .action = .ignore,
    });
    try testEqualRule("  !No* ", .{
        .token = "No",
        .action = .ignore_wildcard,
    });
}
