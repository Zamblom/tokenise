const std = @import("std");

const grapheme = @import("grapheme");
const PropsData = @import("PropsData");

var grapheme_data: grapheme.GraphemeData = undefined;
var props_data: PropsData = undefined;

fn init(gpa: std.mem.Allocator) !void {
    grapheme_data = try .init(gpa);
    props_data = try .init(gpa);
}

fn deinit() void {
    grapheme_data.deinit();
    props_data.deinit();
}

// TODO: add multi-character Parenthesis
pub const TokenState = enum {
    word,
    left_delimiter,
    right_delimiter,
    balanced_delimiter,
    symbol_string,
    new_line,
    whitespace,
    single_line_comment,
    multi_line_comment,
    none,
};

pub const Side = enum {
    right,
    left,
    balanced,
    none,
};

pub const Token = struct {
    state: TokenState,
    value: []const u8,

    pub fn deinit(self: @This(), gpa: std.mem.Allocator) void {
        gpa.free(self.value);
    }

    pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("Token .{s} : \"", .{@tagName(self.state)});
        for (self.value) |c| {
            if (c == '\n') {
                try writer.writeByte('\\');
                try writer.writeByte('n');
                continue;
            }
            try writer.writeByte(c);
        }
        try writer.print("\"", .{});
    }
};

/// A single grapheme
pub const Grphm = []const u8;

/// A slice of graphemes
pub const Str = []const Grphm;

/// Frees a Str
pub fn strFree(gpa: std.mem.Allocator, s: Str) void {
    for (s) |g| gpa.free(g);
    gpa.free(s);
}

/// Returns `true` if `a` and `b` are equal, else `false`.
pub fn strEql(a: Str, b: Str) bool {
    if (a.len != b.len) return false;
    for (a, b) |i, j| if (!std.mem.eql(u8, i, j)) return false;
    return true;
}

/// Returns `true` if `a` ends with `b`, else `false`.
pub fn strEndsWith(a: Str, b: Str) bool {
    if (a.len < b.len) return false;
    const a_relevant = a[a.len - b.len ..];
    return strEql(a_relevant, b);
}

/// Splits a byte slice into its individual graphemes.
/// Caller owns the returned slice of slices.
fn bytesToStr(gpa: std.mem.Allocator, bytes: []const u8) ![]const Grphm {
    var graphemes: std.ArrayListUnmanaged([]const u8) = .empty;
    defer graphemes.deinit(gpa);

    var iter = grapheme.Iterator.init(bytes, &grapheme_data);
    while (iter.next()) |c| {
        const c_bytes = try gpa.dupe(u8, c.bytes(bytes));
        errdefer gpa.free(c_bytes);

        try graphemes.append(gpa, c_bytes);
    }
    return try graphemes.toOwnedSlice(gpa);
}

/// Combines a Str into a single byte slice.
/// Caller owns the returned slice.
fn strToBytes(gpa: std.mem.Allocator, s: Str) ![]const u8 {
    var bytes: std.ArrayListUnmanaged(u8) = .empty;
    defer bytes.deinit(gpa);

    for (s) |g| try bytes.appendSlice(gpa, g);
    return try bytes.toOwnedSlice(gpa);
}

pub const Tokeniser = struct {
    special_characters: []const Grphm,
    delimiter_pairs: []const [2]Grphm,
    balanced_delimiters: []const Grphm,
    single_line_comment: ?Str,
    multi_line_comment: ?[2]Str,

    // TODO: Implement build()
    pub fn init(gpa: std.mem.Allocator, special_characters: []const u8, delimiter_pairs: []const []const u8, balanced_delimiters: []const u8, single_line_comment: ?[]const u8, multi_line_comment: ?[2][]const u8) !@This() {
        const special_characters_owned = try bytesToStr(gpa, special_characters);
        errdefer gpa.free(special_characters_owned);

        const delimiter_pairs_owned = try gpa.alloc([2]Grphm, delimiter_pairs.len);
        errdefer gpa.free(delimiter_pairs_owned);

        for (delimiter_pairs, 0..) |delimiter_pair, i| {
            const vector = try bytesToStr(gpa, delimiter_pair);
            defer strFree(gpa, vector);

            if (vector.len != 2) return error.ParenthesisPairNotLength2;

            const left_side = try gpa.dupe(u8, vector[0]);
            errdefer gpa.free(left_side);

            const right_side = try gpa.dupe(u8, vector[1]);
            errdefer gpa.free(right_side);

            delimiter_pairs_owned[i] = .{ left_side, right_side };
        }
        errdefer for (delimiter_pairs_owned) |pair| {
            gpa.free(pair[0]);
            gpa.free(pair[1]);
        };

        const balanced_delimiters_owned = try bytesToStr(gpa, balanced_delimiters);
        errdefer gpa.free(balanced_delimiters_owned);

        var single_line_comment_owned: ?Str = null;
        if (single_line_comment) |slc| {
            single_line_comment_owned = try bytesToStr(gpa, slc);
            errdefer gpa.free(single_line_comment_owned);
        }

        var multi_line_comment_owned: ?[2]Str = null;
        if (multi_line_comment) |mlc| {
            const left_side = try bytesToStr(gpa, mlc[0]);
            errdefer strFree(gpa, left_side);

            const right_side = try bytesToStr(gpa, mlc[1]);
            errdefer strFree(gpa, right_side);

            multi_line_comment_owned = .{ left_side, right_side };
        }

        return .{
            .special_characters = special_characters_owned,
            .delimiter_pairs = delimiter_pairs_owned,
            .balanced_delimiters = balanced_delimiters_owned,
            .single_line_comment = single_line_comment_owned,
            .multi_line_comment = multi_line_comment_owned,
        };
    }

    pub fn deinit(self: *const @This(), gpa: std.mem.Allocator) void {
        strFree(gpa, self.special_characters);
        for (self.delimiter_pairs) |pair| {
            gpa.free(pair[0]);
            gpa.free(pair[1]);
        }
        gpa.free(self.delimiter_pairs);
        strFree(gpa, self.balanced_delimiters);
        if (self.single_line_comment) |slc| strFree(gpa, slc);
        if (self.multi_line_comment) |mlc| {
            strFree(gpa, mlc[0]);
            strFree(gpa, mlc[1]);
        }
    }

    fn isWhitespace(self: *const @This(), g: Grphm) bool {
        _ = self;
        for (g) |code_point| if (!props_data.isWhitespace(code_point) or std.mem.eql(u8, "\n", g)) return false;
        return true;
    }

    fn isSpecial(self: *const @This(), g: Grphm) bool {
        for (self.special_characters) |s_g| if (std.mem.eql(u8, s_g, g)) return true;
        return false;
    }

    fn getDelimiterSide(self: *const @This(), g: Grphm) Side {
        for (self.delimiter_pairs) |pair| {
            if (std.mem.eql(u8, pair[0], g)) return .left;
            if (std.mem.eql(u8, pair[1], g)) return .right;
        }

        for (self.balanced_delimiters) |d_g| {
            if (std.mem.eql(u8, d_g, g)) return .balanced;
        }

        return .none;
    }

    fn isSingleLineCommentStart(self: *const @This(), s: Str) bool {
        if (self.single_line_comment == null) return false;
        return strEql(self.single_line_comment.?, s);
    }

    fn endsWithSingleLineCommentStart(self: *const @This(), s: Str) bool {
        if (self.single_line_comment == null) return false;
        return strEndsWith(s, self.single_line_comment.?);
    }

    fn isMultiLineCommentStart(self: *const @This(), s: Str) bool {
        if (self.multi_line_comment == null) return false;
        return strEql(self.multi_line_comment.?[0], s);
    }

    fn endsWithMultiLineCommentStart(self: *const @This(), s: Str) bool {
        if (self.multi_line_comment == null) return false;
        return strEndsWith(s, self.multi_line_comment.?[0]);
    }

    fn endsWithMultiLineCommentEnd(self: *const @This(), s: Str) bool {
        std.debug.assert(self.multi_line_comment != null);
        return strEndsWith(s, self.multi_line_comment.?[1]);
    }

    fn isWellFormed(self: *const @This()) bool {
        for (self.delimiter_pairs) |pair| {
            if (!self.isSpecial(pair[0]) or !self.isSpecial(pair[1])) return false;
        }

        if (self.single_line_comment) |slc| {
            for (slc) |g| if (!self.isSpecial(g)) return false;
        }

        if (self.multi_line_comment) |mlc| {
            for (mlc) |pair| {
                for (pair[0]) |g| if (!self.isSpecial(g)) return false;
                for (pair[1]) |g| if (!self.isSpecial(g)) return false;
            }
        }

        return true;
    }

    /// A helper function that converts the input `Str` to bytes, combines it with `state` into a `Token`,
    /// and appends the token to `out`.
    fn addToken(gpa: std.mem.Allocator, out: *std.ArrayListUnmanaged(Token), state: TokenState, value: Str) !void {
        const value_bytes = try strToBytes(gpa, value);
        errdefer gpa.free(value_bytes);
        try out.append(gpa, .{ .state = state, .value = value_bytes });
    }

    pub fn tokenise(self: *const @This(), gpa: std.mem.Allocator, raw_text: []const u8) ![]Token {
        const text: Str = try bytesToStr(gpa, raw_text);
        defer strFree(gpa, text);

        var out: std.ArrayListUnmanaged(Token) = .empty;
        errdefer out.deinit(gpa);
        errdefer for (out.items) |token| token.deinit(gpa);

        var curr_start: usize = 0;
        var state: TokenState = .none;

        for (text, 0..) |g, i| {
            state_switch: switch (state) {
                .none => {
                    std.debug.assert(curr_start == i);
                    if (self.isSpecial(g)) {
                        // Handle the case where `single_line_comment_start` is a single character.
                        if (self.isSingleLineCommentStart(&.{g})) {
                            state = .single_line_comment;
                            continue;
                        }

                        // Handle the case where `multi_line_comment_start` is a single character.
                        if (self.isMultiLineCommentStart(&.{g})) {
                            state = .multi_line_comment;
                            continue;
                        }

                        switch (self.getDelimiterSide(g)) {
                            .left => {
                                try @This().addToken(gpa, &out, .left_delimiter, &.{g});
                                curr_start += 1;
                                continue;
                            },
                            .right => {
                                try @This().addToken(gpa, &out, .right_delimiter, &.{g});
                                curr_start += 1;
                                continue;
                            },
                            .balanced => {
                                try @This().addToken(gpa, &out, .balanced_delimiter, &.{g});
                                curr_start += 1;
                                continue;
                            },
                            .none => {
                                state = .symbol_string;
                                continue;
                            },
                        }
                        unreachable;
                    }

                    if (std.mem.eql(u8, "\n", g)) {
                        try @This().addToken(gpa, &out, .new_line, &.{g});
                        curr_start += 1;
                        continue;
                    }

                    if (self.isWhitespace(g)) {
                        state = .whitespace;
                        continue;
                    }

                    state = .word;
                },
                .word => {
                    // If the `.word` is over, push the token and reset `state` to `.none`
                    // and jump to the `.none` branch to handle the lastest grapheme.
                    if (self.isSpecial(g) or self.isWhitespace(g) or std.mem.eql(u8, "\n", g)) {
                        try @This().addToken(gpa, &out, .word, text[curr_start..i]);
                        state = .none;
                        curr_start = i;
                        continue :state_switch state;
                    }
                    // Else, keep looping
                },
                .symbol_string => {
                    // If the `.symbol_string` is over, push the token and reset `state` to `.none`
                    // and jump to the `.none` branch to handle the lastest grapheme.
                    if (!self.isSpecial(g)) {
                        try @This().addToken(gpa, &out, .symbol_string, text[curr_start..i]);
                        state = .none;
                        curr_start = i;
                        continue :state_switch state;
                    }

                    // Else, check if it's the start of a single line comment
                    const curr_str = text[curr_start .. i + 1];
                    if (self.endsWithSingleLineCommentStart(curr_str)) {
                        // `curr_str` is entirely the opening tag
                        if (curr_str.len == self.single_line_comment.?.len) {
                            state = .single_line_comment;
                            continue;
                        }

                        // `curr_str` contains some symbols followed by the opening tag
                        const new_start: usize = i + 1 - self.single_line_comment.?.len;
                        try @This().addToken(gpa, &out, .symbol_string, text[curr_start..new_start]);
                        state = .single_line_comment;
                        curr_start = new_start;
                        continue;
                    }

                    // Else, check if it's the start of a multi line comment
                    if (self.endsWithMultiLineCommentStart(curr_str)) {
                        // `curr_str` is entirely the opening tag
                        if (curr_str.len == self.multi_line_comment.?[0].len) {
                            state = .multi_line_comment;
                            continue;
                        }

                        // `curr_str` contains some symbols followed by the opening tag
                        const new_start: usize = i + 1 - self.multi_line_comment.?[0].len;
                        try @This().addToken(gpa, &out, .symbol_string, text[curr_start..new_start]);
                        curr_start = new_start;
                        state = .multi_line_comment;
                        continue;
                    }

                    // Else, keep looping
                },
                .whitespace => {
                    // If the `.whitespace` is over, push the token and reset `state` to `.none`
                    // and jump to the `.none` branch to handle the lastest grapheme.
                    if (!self.isWhitespace(g)) {
                        try @This().addToken(gpa, &out, .whitespace, text[curr_start..i]);
                        curr_start = i;
                        state = .none;
                        continue :state_switch state;
                    }
                    // Else, keep looping
                },
                .single_line_comment => {
                    std.debug.assert(self.single_line_comment != null);
                    // If the `.single_line_comment` is over, push the token and reset `state` to `.none`
                    // and jump to the `.none` branch to handle the lastest grapheme.
                    if (std.mem.eql(u8, "\n", g)) {
                        try @This().addToken(gpa, &out, .single_line_comment, text[curr_start..i]);
                        curr_start = i;
                        state = .none;
                        continue :state_switch state;
                    }
                    // Else, keep looping
                },
                .multi_line_comment => {
                    std.debug.assert(self.multi_line_comment != null);
                    // If the `.multi_line_comment` is over, push the token and reset `state` to `.none`
                    // and jump to the `.none` branch to handle the lastest grapheme.
                    const curr_str = text[curr_start .. i + 1];
                    // Make sure to exclude the starting delimiter to avoid cases like `/*/` for `/*` and `*/`.
                    if (self.endsWithMultiLineCommentEnd(curr_str[self.multi_line_comment.?[0].len..])) {
                        try @This().addToken(gpa, &out, .multi_line_comment, curr_str);
                        curr_start = i + 1;
                        state = .none;
                        continue;
                    }
                    // Else, keep looping
                },
                inline else => {
                    std.debug.print("Entered state \".{s}\", which should not be possible!", .{@tagName(state)});
                    return error.InvalidState;
                },
            }
        }

        // for (out.items, 0..) |token, i| {
        //     std.debug.print("{d} {}\n", .{ i, token });
        // }

        // If we've not pushed the last token, check it's valid and then do so.
        if (curr_start != text.len) switch (state) {
            .symbol_string => try @This().addToken(gpa, &out, .symbol_string, text[curr_start..]),
            .whitespace => try @This().addToken(gpa, &out, .whitespace, text[curr_start..]),
            .word => try @This().addToken(gpa, &out, .word, text[curr_start..]),
            .single_line_comment => try @This().addToken(gpa, &out, .single_line_comment, text[curr_start..]),
            inline else => |s| {
                std.debug.print("Invalid Unpushed State: {s}\n\"{s}\"\n", .{ @tagName(s), text[curr_start..] });
                return error.MalformedText;
            },
        };

        return try out.toOwnedSlice(gpa);
    }
};

test "tokeniser init works" {
    try init(std.testing.allocator);
    defer deinit();

    const tokeniser: Tokeniser = try .init(
        std.testing.allocator,
        "!@%👨‍💻*",
        &.{ "<>", "()", "{}", "🇺🇸👋🏽" },
        "\"",
        "//",
        .{ "/*", "*/" },
    );
    defer tokeniser.deinit(std.testing.allocator);

    const special: []const []const u8 = &.{ "!", "@", "%", "👨‍💻", "*" };
    try std.testing.expectEqualDeep(special, tokeniser.special_characters);

    const delimiter_pairs: []const [2][]const u8 = &.{
        [2][]const u8{ "<", ">" },
        [2][]const u8{ "(", ")" },
        [2][]const u8{ "{", "}" },
        [2][]const u8{ "🇺🇸", "👋🏽" },
    };
    try std.testing.expectEqualDeep(delimiter_pairs, tokeniser.delimiter_pairs);

    const single_line_comment: []const []const u8 = &.{ "/", "/" };
    try std.testing.expectEqualDeep(single_line_comment, tokeniser.single_line_comment.?);

    const multi_line_comment = [2][]const []const u8{ &.{ "/", "*" }, &.{ "*", "/" } };
    try std.testing.expectEqualDeep(multi_line_comment, tokeniser.multi_line_comment.?);
}

test "tokeniser tokenise works" {
    try init(std.testing.allocator);
    defer deinit();

    const source: []const u8 = " hi, skdjfs;;    842\t 39fsl == 3\n what's going on? idk... \nfire___sldfksfl // what's going on? \nidk what I'm 🇺🇸doing \n\nnow👋🏽 hi £*$*@ \nhelp!\n\"hello\"hi";

    const tokeniser = try Tokeniser.init(
        std.testing.allocator,
        ",;=?.'*)(/£<>@🇺🇸👋🏽\"",
        &.{ "()", "[]" },
        "\"",
        "//",
        .{ "🇺🇸", "👋🏽" },
    );
    defer tokeniser.deinit(std.testing.allocator);

    const tokens = try tokeniser.tokenise(std.testing.allocator, source);
    defer std.testing.allocator.free(tokens);
    defer for (tokens) |token| token.deinit(std.testing.allocator);

    try std.testing.expectEqual(56, tokens.len);

    try std.testing.expectEqualDeep(Token{ .state = .whitespace, .value = " " }, tokens[0]);
    try std.testing.expectEqualDeep(Token{ .state = .word, .value = "hi" }, tokens[1]);
    try std.testing.expectEqualDeep(Token{ .state = .symbol_string, .value = "," }, tokens[2]);
    try std.testing.expectEqualDeep(Token{ .state = .whitespace, .value = " " }, tokens[3]);

    try std.testing.expectEqualDeep(Token{ .state = .word, .value = "skdjfs" }, tokens[4]);
    try std.testing.expectEqualDeep(Token{ .state = .symbol_string, .value = ";;" }, tokens[5]);
    try std.testing.expectEqualDeep(Token{ .state = .whitespace, .value = "    " }, tokens[6]);
    try std.testing.expectEqualDeep(Token{ .state = .word, .value = "842" }, tokens[7]);

    try std.testing.expectEqualDeep(Token{ .state = .whitespace, .value = "\t " }, tokens[8]);
    try std.testing.expectEqualDeep(Token{ .state = .word, .value = "39fsl" }, tokens[9]);
    try std.testing.expectEqualDeep(Token{ .state = .whitespace, .value = " " }, tokens[10]);
    try std.testing.expectEqualDeep(Token{ .state = .symbol_string, .value = "==" }, tokens[11]);

    try std.testing.expectEqualDeep(Token{ .state = .whitespace, .value = " " }, tokens[12]);
    try std.testing.expectEqualDeep(Token{ .state = .word, .value = "3" }, tokens[13]);
    try std.testing.expectEqualDeep(Token{ .state = .new_line, .value = "\n" }, tokens[14]);
    try std.testing.expectEqualDeep(Token{ .state = .whitespace, .value = " " }, tokens[15]);

    try std.testing.expectEqualDeep(Token{ .state = .word, .value = "what" }, tokens[16]);
    try std.testing.expectEqualDeep(Token{ .state = .symbol_string, .value = "'" }, tokens[17]);
    try std.testing.expectEqualDeep(Token{ .state = .word, .value = "s" }, tokens[18]);
    try std.testing.expectEqualDeep(Token{ .state = .whitespace, .value = " " }, tokens[19]);

    try std.testing.expectEqualDeep(Token{ .state = .word, .value = "going" }, tokens[20]);
    try std.testing.expectEqualDeep(Token{ .state = .whitespace, .value = " " }, tokens[21]);
    try std.testing.expectEqualDeep(Token{ .state = .word, .value = "on" }, tokens[22]);
    try std.testing.expectEqualDeep(Token{ .state = .symbol_string, .value = "?" }, tokens[23]);

    try std.testing.expectEqualDeep(Token{ .state = .whitespace, .value = " " }, tokens[24]);
    try std.testing.expectEqualDeep(Token{ .state = .word, .value = "idk" }, tokens[25]);
    try std.testing.expectEqualDeep(Token{ .state = .symbol_string, .value = "..." }, tokens[26]);
    try std.testing.expectEqualDeep(Token{ .state = .whitespace, .value = " " }, tokens[27]);

    try std.testing.expectEqualDeep(Token{ .state = .new_line, .value = "\n" }, tokens[28]);
    try std.testing.expectEqualDeep(Token{ .state = .word, .value = "fire___sldfksfl" }, tokens[29]);
    try std.testing.expectEqualDeep(Token{ .state = .whitespace, .value = " " }, tokens[30]);
    try std.testing.expectEqualDeep(Token{ .state = .single_line_comment, .value = "// what's going on? " }, tokens[31]);

    try std.testing.expectEqualDeep(Token{ .state = .new_line, .value = "\n" }, tokens[32]);
    try std.testing.expectEqualDeep(Token{ .state = .word, .value = "idk" }, tokens[33]);
    try std.testing.expectEqualDeep(Token{ .state = .whitespace, .value = " " }, tokens[34]);
    try std.testing.expectEqualDeep(Token{ .state = .word, .value = "what" }, tokens[35]);

    try std.testing.expectEqualDeep(Token{ .state = .whitespace, .value = " " }, tokens[36]);
    try std.testing.expectEqualDeep(Token{ .state = .word, .value = "I" }, tokens[37]);
    try std.testing.expectEqualDeep(Token{ .state = .symbol_string, .value = "'" }, tokens[38]);
    try std.testing.expectEqualDeep(Token{ .state = .word, .value = "m" }, tokens[39]);

    try std.testing.expectEqualDeep(Token{ .state = .whitespace, .value = " " }, tokens[40]);
    try std.testing.expectEqualDeep(Token{ .state = .multi_line_comment, .value = "🇺🇸doing \n\nnow👋🏽" }, tokens[41]);
    try std.testing.expectEqualDeep(Token{ .state = .whitespace, .value = " " }, tokens[42]);
    try std.testing.expectEqualDeep(Token{ .state = .word, .value = "hi" }, tokens[43]);

    try std.testing.expectEqualDeep(Token{ .state = .whitespace, .value = " " }, tokens[44]);
    try std.testing.expectEqualDeep(Token{ .state = .symbol_string, .value = "£*" }, tokens[45]);
    try std.testing.expectEqualDeep(Token{ .state = .word, .value = "$" }, tokens[46]);
    try std.testing.expectEqualDeep(Token{ .state = .symbol_string, .value = "*@" }, tokens[47]);

    try std.testing.expectEqualDeep(Token{ .state = .whitespace, .value = " " }, tokens[48]);
    try std.testing.expectEqualDeep(Token{ .state = .new_line, .value = "\n" }, tokens[49]);
    try std.testing.expectEqualDeep(Token{ .state = .word, .value = "help!" }, tokens[50]);
    try std.testing.expectEqualDeep(Token{ .state = .new_line, .value = "\n" }, tokens[51]);

    try std.testing.expectEqualDeep(Token{ .state = .balanced_delimiter, .value = "\"" }, tokens[52]);
    try std.testing.expectEqualDeep(Token{ .state = .word, .value = "hello" }, tokens[53]);
    try std.testing.expectEqualDeep(Token{ .state = .balanced_delimiter, .value = "\"" }, tokens[54]);
    try std.testing.expectEqualDeep(Token{ .state = .word, .value = "hi" }, tokens[55]);
}
