const std = @import("std");

const grapheme = @import("grapheme");
const PropsData = @import("PropsData");

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
            switch (c) {
                '\n' => {
                    try writer.writeByte('\\');
                    try writer.writeByte('n');
                },
                '\r' => {
                    try writer.writeByte('\\');
                    try writer.writeByte('r');
                },
                else => try writer.writeByte(c),
            }
        }
        try writer.print("\"", .{});
    }
};

/// A single grapheme
const Grphm = []const u8;

/// A slice of graphemes
const Str = []const Grphm;

/// Frees a Str
fn strFree(gpa: std.mem.Allocator, s: Str) void {
    for (s) |g| gpa.free(g);
    gpa.free(s);
}

/// Returns `true` if `a` and `b` are equal, else `false`.
fn strEql(a: Str, b: Str) bool {
    if (a.len != b.len) return false;
    for (a, b) |i, j| if (!std.mem.eql(u8, i, j)) return false;
    return true;
}

/// Returns `true` if `a` ends with `b`, else `false`.
fn strEndsWith(a: Str, b: Str) bool {
    if (a.len < b.len) return false;
    const a_relevant = a[a.len - b.len ..];
    return strEql(a_relevant, b);
}

/// Splits a byte slice into its individual graphemes.
/// Caller owns the returned slice of slices.
fn bytesToStr(gpa: std.mem.Allocator, grapheme_data: *const grapheme.GraphemeData, bytes: []const u8) std.mem.Allocator.Error![]const Grphm {
    var graphemes: std.ArrayListUnmanaged([]const u8) = .empty;
    defer graphemes.deinit(gpa);

    var iter = grapheme.Iterator.init(bytes, grapheme_data);
    while (iter.next()) |c| {
        const c_bytes = try gpa.dupe(u8, c.bytes(bytes));
        errdefer gpa.free(c_bytes);

        try graphemes.append(gpa, c_bytes);
    }
    return try graphemes.toOwnedSlice(gpa);
}

/// Combines a Str into a single byte slice.
/// Caller owns the returned slice.
fn strToBytes(gpa: std.mem.Allocator, s: Str) std.mem.Allocator.Error![]const u8 {
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
    grapheme_data: grapheme.GraphemeData,
    props_data: PropsData,

    pub const InitError = error{
        DelimiterPairsPairLengthMustBeTwo,
        SpecialCharactersMustNotBeNewLines,
        SpecialCharactersMustNotBeWhitespace,
        DelimitersPairStartsMustBeSpecial,
        DelimitersPairEndsMustBeSpecial,
        BalancedDelimitersMustBeSpecial,
        BalancedDelimitersMustNotBeDelimiterPairStarts,
        BalancedDelimitersMustNotBeDelimiterPairEnds,
        SingleLineCommentMustBeLongerThanZero,
        SingleLineCommentMustBeEntirelySpecial,
        SingleLineCommentMustNotIncludeDelimiterPairStarts,
        SingleLineCommentMustNotIncludeDelimiterPairEnds,
        SingleLineCommentMustNotIncludeBalancedDelimiters,
        SingleLineCommentMustNotContainMultiLineCommentStart,
        SingleLineCommentMustNotContainMultiLineCommentEnd,
        MultiLineCommentStartMustBeLongerThanZero,
        MultiLineCommentStartMustBeEntirelySpecial,
        MultiLineCommentStartMustNotIncludeDelimiterPairStarts,
        MultiLineCommentStartMustNotIncludeDelimiterPairEnds,
        MultiLineCommentStartMustNotIncludeBalancedDelimiters,
        MultiLineCommentEndMustBeLongerThanZero,
        MultiLineCommentEndMustBeEntirelySpecial,
        MultiLineCommentEndMustNotIncludeDelimiterPairStarts,
        MultiLineCommentEndMustNotIncludeDelimiterPairEnds,
        MultiLineCommentEndMustNotIncludeBalancedDelimiters,
        MultiLineCommentStartMustNotContainSingleLineComment,
        MultiLineCommentEndMustNotContainSingleLineComment,
        GraphemeDataFailedToInit,
        PropsDataFailedToInit,
    } || std.mem.Allocator.Error;

    pub const TokeniserError = error{
        MalformedText,
    } || std.mem.Allocator.Error;

    // TODO: Implement build()
    pub fn init(gpa: std.mem.Allocator, special_characters: []const u8, delimiter_pairs: []const []const u8, balanced_delimiters: []const u8, single_line_comment: ?[]const u8, multi_line_comment: ?[2][]const u8) InitError!@This() {
        const grapheme_data: grapheme.GraphemeData = grapheme.GraphemeData.init(gpa) catch return InitError.GraphemeDataFailedToInit;
        errdefer grapheme_data.deinit();

        const props_data: PropsData = PropsData.init(gpa) catch return InitError.PropsDataFailedToInit;
        errdefer props_data.deinit();

        const special_characters_owned = try bytesToStr(gpa, &grapheme_data, special_characters);
        errdefer strFree(gpa, special_characters_owned);

        const delimiter_pairs_owned = try gpa.alloc([2]Grphm, delimiter_pairs.len);
        errdefer gpa.free(delimiter_pairs_owned);

        for (delimiter_pairs, 0..) |delimiter_pair, i| {
            const vector = try bytesToStr(gpa, &grapheme_data, delimiter_pair);
            defer strFree(gpa, vector);

            if (vector.len != 2) return InitError.DelimiterPairsPairLengthMustBeTwo;

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

        const balanced_delimiters_owned = try bytesToStr(gpa, &grapheme_data, balanced_delimiters);
        errdefer strFree(gpa, balanced_delimiters_owned);

        var single_line_comment_owned: ?Str = null;
        if (single_line_comment) |slc| {
            single_line_comment_owned = try bytesToStr(gpa, &grapheme_data, slc);
            errdefer strFree(single_line_comment_owned);
        }
        errdefer if (single_line_comment_owned) |slc| strFree(gpa, slc);

        var multi_line_comment_owned: ?[2]Str = null;
        if (multi_line_comment) |mlc| {
            const left_side = try bytesToStr(gpa, &grapheme_data, mlc[0]);
            errdefer strFree(gpa, left_side);

            const right_side = try bytesToStr(gpa, &grapheme_data, mlc[1]);
            errdefer strFree(gpa, right_side);

            multi_line_comment_owned = .{ left_side, right_side };
        }
        errdefer if (multi_line_comment_owned) |mlc| {
            strFree(gpa, mlc[0]);
            strFree(gpa, mlc[1]);
        };

        const self: @This() = .{
            .special_characters = special_characters_owned,
            .delimiter_pairs = delimiter_pairs_owned,
            .balanced_delimiters = balanced_delimiters_owned,
            .single_line_comment = single_line_comment_owned,
            .multi_line_comment = multi_line_comment_owned,
            .grapheme_data = grapheme_data,
            .props_data = props_data,
        };

        try self.ensureWellFormed(gpa);

        return self;
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
        self.grapheme_data.deinit();
        self.props_data.deinit();
    }

    /// Checks that the given `Tokeniser` is well formed.
    /// Returns the relevant error
    fn ensureWellFormed(self: *const @This(), gpa: std.mem.Allocator) InitError!void {
        // Special Character must not be newlines or whitespace.
        for (self.special_characters) |special| {
            if (self.isNewLine(special)) return InitError.SpecialCharactersMustNotBeNewLines;
            if (self.isWhitespace(special)) return InitError.SpecialCharactersMustNotBeWhitespace;
        }

        // Delimiters Pairs must be entirely special.
        for (self.delimiter_pairs) |pair| {
            if (!self.isSpecial(pair[0])) return InitError.DelimitersPairStartsMustBeSpecial;
            if (!self.isSpecial(pair[1])) return InitError.DelimitersPairEndsMustBeSpecial;
        }

        // Balanced Delimiters must be special and not within Delimiter Pairs.
        for (self.balanced_delimiters) |delimiter| {
            if (!self.isSpecial(delimiter)) return InitError.BalancedDelimitersMustBeSpecial;
            for (self.delimiter_pairs) |pair| {
                if (std.mem.eql(u8, delimiter, pair[0])) return InitError.BalancedDelimitersMustNotBeDelimiterPairStarts;
                if (std.mem.eql(u8, delimiter, pair[1])) return InitError.BalancedDelimitersMustNotBeDelimiterPairEnds;
            }
        }

        // Single Line Comments must be entirely special, must not contain delimiters, and must not contain multi line comments.
        if (self.single_line_comment) |slc| {
            if (slc.len == 0) return InitError.SingleLineCommentMustBeLongerThanZero;
            for (slc) |g| {
                if (!self.isSpecial(g)) return InitError.SingleLineCommentMustBeEntirelySpecial;
                for (self.delimiter_pairs) |pair| {
                    if (std.mem.eql(u8, g, pair[0])) return InitError.SingleLineCommentMustNotIncludeDelimiterPairStarts;
                    if (std.mem.eql(u8, g, pair[1])) return InitError.SingleLineCommentMustNotIncludeDelimiterPairEnds;
                }
                for (self.balanced_delimiters) |delimiter| {
                    if (std.mem.eql(u8, g, delimiter)) return InitError.SingleLineCommentMustNotIncludeBalancedDelimiters;
                }
            }
            if (self.multi_line_comment) |mlc| {
                const slc_bytes = try strToBytes(gpa, slc);
                defer gpa.free(slc_bytes);

                const mlc_start_bytes = try strToBytes(gpa, mlc[0]);
                defer gpa.free(mlc_start_bytes);

                const mlc_end_bytes = try strToBytes(gpa, mlc[1]);
                defer gpa.free(mlc_end_bytes);

                if (std.mem.containsAtLeast(u8, slc_bytes, 1, mlc_start_bytes)) return InitError.SingleLineCommentMustNotContainMultiLineCommentStart;
                if (std.mem.containsAtLeast(u8, slc_bytes, 1, mlc_end_bytes)) return InitError.SingleLineCommentMustNotContainMultiLineCommentEnd;
            }
        }

        // Multi Line Comments must be entirely special, must not contain delimiters, and must not contain single line comments.
        if (self.multi_line_comment) |mlc| {
            if (mlc[0].len == 0) return InitError.MultiLineCommentStartMustBeLongerThanZero;
            if (mlc[1].len == 0) return InitError.MultiLineCommentEndMustBeLongerThanZero;
            for (mlc[0]) |g| {
                if (!self.isSpecial(g)) return InitError.MultiLineCommentStartMustBeEntirelySpecial;
                for (self.delimiter_pairs) |pair| {
                    if (std.mem.eql(u8, g, pair[0])) return InitError.MultiLineCommentStartMustNotIncludeDelimiterPairStarts;
                    if (std.mem.eql(u8, g, pair[1])) return InitError.MultiLineCommentStartMustNotIncludeDelimiterPairEnds;
                }
                for (self.balanced_delimiters) |delimiter| {
                    if (std.mem.eql(u8, g, delimiter)) return InitError.MultiLineCommentStartMustNotIncludeBalancedDelimiters;
                }
            }
            for (mlc[1]) |g| {
                if (!self.isSpecial(g)) return InitError.MultiLineCommentEndMustBeEntirelySpecial;
                for (self.delimiter_pairs) |pair| {
                    if (std.mem.eql(u8, g, pair[0])) return InitError.MultiLineCommentEndMustNotIncludeDelimiterPairStarts;
                    if (std.mem.eql(u8, g, pair[1])) return InitError.MultiLineCommentEndMustNotIncludeDelimiterPairEnds;
                }
                for (self.balanced_delimiters) |delimiter| {
                    if (std.mem.eql(u8, g, delimiter)) return InitError.MultiLineCommentEndMustNotIncludeBalancedDelimiters;
                }
            }
            if (self.single_line_comment) |slc| {
                const mlc_start_bytes = try strToBytes(gpa, mlc[0]);
                defer gpa.free(mlc_start_bytes);

                const mlc_end_bytes = try strToBytes(gpa, mlc[1]);
                defer gpa.free(mlc_end_bytes);

                const slc_bytes = try strToBytes(gpa, slc);
                defer gpa.free(slc_bytes);

                if (std.mem.containsAtLeast(u8, mlc_start_bytes, 1, slc_bytes)) return InitError.MultiLineCommentStartMustNotContainSingleLineComment;
                if (std.mem.containsAtLeast(u8, mlc_end_bytes, 1, slc_bytes)) return InitError.MultiLineCommentEndMustNotContainSingleLineComment;
            }
        }
    }

    /// Returns whether the given grapheme was marked as special during init.
    fn isSpecial(self: *const @This(), g: Grphm) bool {
        for (self.special_characters) |s_g| if (std.mem.eql(u8, s_g, g)) return true;
        return false;
    }

    /// Returns whether the given grapheme is a newline.
    fn isNewLine(self: *const @This(), g: Grphm) bool {
        _ = self;
        return std.mem.eql(u8, "\r", g) or std.mem.eql(u8, "\r\n", g) or std.mem.eql(u8, "\n", g);
    }

    /// Returns whether the given grapheme is whitespace.
    /// Newlines return false to aid tokenisation.
    fn isWhitespace(self: *const @This(), g: Grphm) bool {
        if (isNewLine(self, g)) return false;
        for (g) |code_point| if (!self.props_data.isWhitespace(code_point)) return false;
        return true;
    }

    /// Returns whether the given grapheme is part of a word.
    fn isWord(self: *const @This(), g: Grphm) bool {
        return !self.isSpecial(g) and !self.isWhitespace(g) and !self.isNewLine(g);
    }

    /// Returns what type of delimiter the given grapheme is.
    /// Returns `.none` if the grapheme isn't a delimiter.
    fn getDelimiterSide(self: *const @This(), g: Grphm) enum { right, left, balanced, none } {
        for (self.delimiter_pairs) |pair| {
            if (std.mem.eql(u8, pair[0], g)) return .left;
            if (std.mem.eql(u8, pair[1], g)) return .right;
        }

        for (self.balanced_delimiters) |d_g| {
            if (std.mem.eql(u8, d_g, g)) return .balanced;
        }

        return .none;
    }

    /// Returns true if s ends with a single line comment start
    fn endsWithSingleLineCommentStart(self: *const @This(), s: Str) bool {
        if (self.single_line_comment == null) return false;
        return strEndsWith(s, self.single_line_comment.?);
    }

    /// Returns true if s ends with a multi line comment start
    fn endsWithMultiLineCommentStart(self: *const @This(), s: Str) bool {
        if (self.multi_line_comment == null) return false;
        return strEndsWith(s, self.multi_line_comment.?[0]);
    }

    /// Returns true if s ends with a multi line comment end
    fn endsWithMultiLineCommentEnd(self: *const @This(), s: Str) bool {
        std.debug.assert(self.multi_line_comment != null);
        return strEndsWith(s, self.multi_line_comment.?[1]);
    }

    /// A helper function that converts the input `Str` to bytes, combines it with `state` into a `Token`,
    /// and appends the token to `out`.
    fn addToken(gpa: std.mem.Allocator, out: *std.ArrayListUnmanaged(Token), state: TokenState, value: Str) std.mem.Allocator.Error!void {
        const value_bytes = try strToBytes(gpa, value);
        errdefer gpa.free(value_bytes);
        try out.append(gpa, .{ .state = state, .value = value_bytes });
    }

    /// Take a input string `raw_text` and tokenise it into a slice of tokens.
    /// The caller owns the returned slice and should both free it and call `deinit` on each of the tokens within.
    pub fn tokenise(self: *const @This(), gpa: std.mem.Allocator, raw_text: []const u8) TokeniserError![]Token {
        const text: Str = try bytesToStr(gpa, &self.grapheme_data, raw_text);
        defer strFree(gpa, text);

        var out: std.ArrayListUnmanaged(Token) = .empty;
        errdefer out.deinit(gpa);
        errdefer for (out.items) |token| token.deinit(gpa);

        var current_start: usize = 0;
        var state: TokenState = .none;

        for (text, 0..) |g, i| {
            state_switch: switch (state) {
                .none => {
                    std.debug.assert(current_start == i);

                    if (self.isSpecial(g)) {
                        // Handle the case where `single_line_comment_start` is a single character.
                        if (self.endsWithSingleLineCommentStart(&.{g})) {
                            state = .single_line_comment;
                            continue;
                        }

                        // Handle the case where `multi_line_comment_start` is a single character.
                        if (self.endsWithMultiLineCommentStart(&.{g})) {
                            state = .multi_line_comment;
                            continue;
                        }

                        switch (self.getDelimiterSide(g)) {
                            .left => {
                                try @This().addToken(gpa, &out, .left_delimiter, &.{g});
                                current_start += 1;
                                continue;
                            },
                            .right => {
                                try @This().addToken(gpa, &out, .right_delimiter, &.{g});
                                current_start += 1;
                                continue;
                            },
                            .balanced => {
                                try @This().addToken(gpa, &out, .balanced_delimiter, &.{g});
                                current_start += 1;
                                continue;
                            },
                            .none => {
                                state = .symbol_string;
                                continue;
                            },
                        }
                        unreachable;
                    }

                    if (self.isWhitespace(g)) {
                        state = .whitespace;
                        continue;
                    }

                    if (self.isNewLine(g)) {
                        try @This().addToken(gpa, &out, .new_line, &.{g});
                        current_start += 1;
                        continue;
                    }

                    if (self.isWord(g)) {
                        state = .word;
                        continue;
                    }

                    unreachable;
                },
                .word => {
                    if (self.isWord(g)) {
                        // We're still in the `.word`.
                        continue;
                    }

                    // The `.word` is over, push the token and reset `state` to `.none`
                    // and jump to the `.none` branch to handle the lastest grapheme.
                    try @This().addToken(gpa, &out, .word, text[current_start..i]);
                    state = .none;
                    current_start = i;
                    continue :state_switch state;
                },
                .symbol_string => {
                    if (self.isSpecial(g)) {
                        const curr_str = text[current_start .. i + 1];

                        if (self.endsWithSingleLineCommentStart(curr_str)) {
                            // We've got the start of a `.single_line_comment`.
                            state = .single_line_comment;

                            if (curr_str.len > self.single_line_comment.?.len) {
                                // There's a token before the comment opening, push it.
                                const new_start: usize = i + 1 - self.single_line_comment.?.len;
                                try @This().addToken(gpa, &out, .symbol_string, text[current_start..new_start]);
                                current_start = new_start;
                            }

                            continue;
                        }

                        if (self.endsWithMultiLineCommentStart(curr_str)) {
                            // We've got the start of a `.multi_line_comment`.
                            state = .multi_line_comment;

                            if (curr_str.len > self.multi_line_comment.?[0].len) {
                                // There's a token before the comment opening, push it.
                                const new_start: usize = i + 1 - self.multi_line_comment.?[0].len;
                                try @This().addToken(gpa, &out, .symbol_string, text[current_start..new_start]);
                                current_start = new_start;
                            }

                            continue;
                        }

                        switch (self.getDelimiterSide(g)) {
                            .none => {
                                // We're still in the `.symbol_string`.
                                continue;
                            },
                            inline else => {
                                // The `.symbol_string` is over, push the token and reset `state` to `.none`
                                // and jump to the `.none` branch to handle the lastest grapheme.
                                try @This().addToken(gpa, &out, .symbol_string, text[current_start..i]);
                                state = .none;
                                current_start = i;
                                continue :state_switch state;
                            },
                        }
                        unreachable;
                    }

                    // The `.symbol_string` is over, push the token and reset `state` to `.none`
                    // and jump to the `.none` branch to handle the lastest grapheme.
                    try @This().addToken(gpa, &out, .symbol_string, text[current_start..i]);
                    state = .none;
                    current_start = i;
                    continue :state_switch state;
                },
                .whitespace => {
                    if (self.isWhitespace(g)) {
                        // We're still in the `.whitespace`.
                        continue;
                    }

                    // The `.whitespace` is over, push the token and reset `state` to `.none`
                    // and jump to the `.none` branch to handle the lastest grapheme.
                    try @This().addToken(gpa, &out, .whitespace, text[current_start..i]);
                    current_start = i;
                    state = .none;
                    continue :state_switch state;
                },
                .single_line_comment => {
                    std.debug.assert(self.single_line_comment != null);

                    if (!self.isNewLine(g)) {
                        // We're still in the `.single_line_comment`.
                        continue;
                    }

                    // The `.single_line_comment` is over, push the token and reset `state` to `.none`
                    // and jump to the `.none` branch to handle the lastest grapheme.
                    try @This().addToken(gpa, &out, .single_line_comment, text[current_start..i]);
                    current_start = i;
                    state = .none;
                    continue :state_switch state;
                },
                .multi_line_comment => {
                    std.debug.assert(self.multi_line_comment != null);

                    const current_str = text[current_start .. i + 1];

                    // Make sure to exclude the starting delimiter to avoid cases like `/*/` for `/*` and `*/`.
                    if (!self.endsWithMultiLineCommentEnd(current_str[self.multi_line_comment.?[0].len..])) {
                        // We're still in the `.multi_line_comment`.
                        continue;
                    }

                    // The `.multi_line_comment` is over, push the token and reset `state` to `.none`.
                    try @This().addToken(gpa, &out, .multi_line_comment, current_str);
                    current_start = i + 1;
                    state = .none;
                },
                inline else => std.debug.assert(false),
            }
        }

        if (current_start != text.len) switch (state) {
            // We've not pushed the last token, check it's valid and then do so.
            .symbol_string => try @This().addToken(gpa, &out, .symbol_string, text[current_start..]),
            .whitespace => try @This().addToken(gpa, &out, .whitespace, text[current_start..]),
            .word => try @This().addToken(gpa, &out, .word, text[current_start..]),
            .single_line_comment => try @This().addToken(gpa, &out, .single_line_comment, text[current_start..]),
            inline else => return TokeniserError.MalformedText,
        };

        return try out.toOwnedSlice(gpa);
    }
};

test "tokeniser init works" {
    const tokeniser: Tokeniser = try .init(
        std.testing.allocator,
        "!@%👨‍💻<>(){}🇺🇸👋🏽\"/*",
        &.{ "<>", "()", "{}", "🇺🇸👋🏽" },
        "\"",
        "//",
        .{ "/*", "*/" },
    );
    defer tokeniser.deinit(std.testing.allocator);

    const special: []const []const u8 = &.{ "!", "@", "%", "👨‍💻", "<", ">", "(", ")", "{", "}", "🇺🇸", "👋🏽", "\"", "/", "*" };
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

test "long tokeniser test" {
    const tokeniser = try Tokeniser.init(
        std.testing.allocator,
        ",;=?.'*()[]/£<>@🇺🇸👋🏽\"",
        &.{ "()", "[]" },
        "\"",
        "//",
        .{ "🇺🇸", "👋🏽" },
    );
    defer tokeniser.deinit(std.testing.allocator);

    const source: []const u8 = " hi, skdjfs;;    842\t 39fsl == 3\r\n what's going on? idk... \rfire___sldfksfl // what's going on? \nidk what I'm 🇺🇸doing \n\nnow👋🏽 hi £*$*@ \nhelp!\n\"hello\"hi";

    const tokens = try tokeniser.tokenise(std.testing.allocator, source);
    defer std.testing.allocator.free(tokens);
    defer for (tokens) |token| token.deinit(std.testing.allocator);

    errdefer for (tokens, 0..) |token, i| std.debug.print("{d} {}\n", .{ i, token });

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
    try std.testing.expectEqualDeep(Token{ .state = .new_line, .value = "\r\n" }, tokens[14]);
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

    try std.testing.expectEqualDeep(Token{ .state = .new_line, .value = "\r" }, tokens[28]);
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

/// Attempts to tokenise `token.value` and expects the result is `token`.
/// This function should be used for testing purposes only.
fn test_token_singular(tokeniser: Tokeniser, comptime token: Token) !void {
    const tokens = try tokeniser.tokenise(std.testing.allocator, token.value);
    defer std.testing.allocator.free(tokens);
    defer for (tokens) |t| t.deinit(std.testing.allocator);
    errdefer {
        std.debug.print("\nFailed Token Singular Test:\nExpected Token: {}\n", .{token});
        std.debug.print("Actual Tokens:\n", .{});
        for (tokens, 0..) |t, i| std.debug.print("  {d} {}\n", .{ i, t });
    }
    try std.testing.expectEqualDeep(&[_]Token{token}, tokens);
}

/// Attempts to tokenise `token_a.value ++ token_b.value` and expects the result to be `expected_tokens`.
/// This function should be used for testing purposes only.
fn test_token_combination(tokeniser: Tokeniser, comptime token_a: Token, comptime token_b: Token, comptime expected_tokens: []const Token) !void {
    const tokens = try tokeniser.tokenise(std.testing.allocator, token_a.value ++ token_b.value);
    defer std.testing.allocator.free(tokens);
    defer for (tokens) |token| token.deinit(std.testing.allocator);
    errdefer {
        std.debug.print("\nFailed Token Combination Test:\nToken A: {}\nToken B: {}\nExpected Tokens:\n", .{ token_a, token_b });
        for (expected_tokens, 0..) |token, i| std.debug.print("  {d} {}\n", .{ i, token });
        std.debug.print("Actual Tokens:\n", .{});
        for (tokens, 0..) |token, i| std.debug.print("  {d} {}\n", .{ i, token });
    }
    try std.testing.expectEqualDeep(expected_tokens, tokens);
}

test "tokeniser singular and combination test" {
    const word_tokens = [_]Token{
        Token{ .state = .word, .value = "a" },
        Token{ .state = .word, .value = "A" },
    };

    const left_delimiter_tokens = [_]Token{
        Token{ .state = .left_delimiter, .value = "(" },
        Token{ .state = .left_delimiter, .value = "[" },
    };

    const right_delimiter_tokens = [_]Token{
        Token{ .state = .right_delimiter, .value = ")" },
        Token{ .state = .right_delimiter, .value = "]" },
    };

    const balanced_delimiter_tokens = [_]Token{
        Token{ .state = .balanced_delimiter, .value = "\"" },
        Token{ .state = .balanced_delimiter, .value = "'" },
    };

    const symbol_string_tokens = [_]Token{
        Token{ .state = .symbol_string, .value = ";" },
        Token{ .state = .symbol_string, .value = "/" },
        Token{ .state = .symbol_string, .value = "*" },
        Token{ .state = .symbol_string, .value = ";;" },
        Token{ .state = .symbol_string, .value = ";/" },
        Token{ .state = .symbol_string, .value = "/;" },
        Token{ .state = .symbol_string, .value = "*;" },
        Token{ .state = .symbol_string, .value = ";*" },
        Token{ .state = .symbol_string, .value = "*/" },
        Token{ .state = .symbol_string, .value = "/;/" },
        Token{ .state = .symbol_string, .value = "/;*" },
    };

    const new_line_tokens = [_]Token{
        // The ordering of "\r\n" before "\r" or "\n" is important for the following tests.
        Token{ .state = .new_line, .value = "\r\n" },
        Token{ .state = .new_line, .value = "\r" },
        Token{ .state = .new_line, .value = "\n" },
    };

    const whitespace_tokens = [_]Token{
        Token{ .state = .whitespace, .value = " " },
        Token{ .state = .whitespace, .value = "\t" },
        Token{ .state = .whitespace, .value = " \t" },
        Token{ .state = .whitespace, .value = "\t " },
    };

    const single_line_comment_tokens = [_]Token{
        Token{ .state = .single_line_comment, .value = "//" },
        Token{ .state = .single_line_comment, .value = "//a" },
        Token{ .state = .single_line_comment, .value = "//(" },
        Token{ .state = .single_line_comment, .value = "//)" },
        Token{ .state = .single_line_comment, .value = "//\"" },
        Token{ .state = .single_line_comment, .value = "//;" },
        Token{ .state = .single_line_comment, .value = "// " },
        Token{ .state = .single_line_comment, .value = "////" },
        Token{ .state = .single_line_comment, .value = "///*" },
    };

    const multi_line_comment_tokens = [_]Token{
        Token{ .state = .multi_line_comment, .value = "/**/" },
        Token{ .state = .multi_line_comment, .value = "/*a*/" },
        Token{ .state = .multi_line_comment, .value = "/*(*/" },
        Token{ .state = .multi_line_comment, .value = "/*)*/" },
        Token{ .state = .multi_line_comment, .value = "/*\"*/" },
        Token{ .state = .multi_line_comment, .value = "/*;*/" },
        Token{ .state = .multi_line_comment, .value = "/*\r*/" },
        Token{ .state = .multi_line_comment, .value = "/*\r\n*/" },
        Token{ .state = .multi_line_comment, .value = "/*\n*/" },
        Token{ .state = .multi_line_comment, .value = "/* */" },
        Token{ .state = .multi_line_comment, .value = "/*//*/" },
        Token{ .state = .multi_line_comment, .value = "/*/*/" },
    };

    const tokeniser = try Tokeniser.init(
        std.testing.allocator,
        ";()[]\"'/*",
        &.{ "()", "[]" },
        "\"'",
        "//",
        .{ "/*", "*/" },
    );
    defer tokeniser.deinit(std.testing.allocator);

    @setEvalBranchQuota(50000);

    inline for (word_tokens) |a| {
        try test_token_singular(tokeniser, a);
        // Tokenising two `.word`s should combine them into a single token.
        inline for (word_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{Token{ .state = .word, .value = a.value ++ b.value }});
        inline for (left_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (right_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (balanced_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (symbol_string_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (new_line_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (whitespace_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (single_line_comment_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (multi_line_comment_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
    }

    inline for (left_delimiter_tokens) |a| {
        try test_token_singular(tokeniser, a);
        inline for (word_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (left_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (right_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (balanced_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (symbol_string_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (new_line_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (whitespace_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (single_line_comment_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (multi_line_comment_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
    }

    inline for (right_delimiter_tokens) |a| {
        try test_token_singular(tokeniser, a);
        inline for (word_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (left_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (right_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (balanced_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (symbol_string_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (new_line_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (whitespace_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (single_line_comment_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (multi_line_comment_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
    }

    inline for (balanced_delimiter_tokens) |a| {
        try test_token_singular(tokeniser, a);
        inline for (word_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (left_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (right_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (balanced_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (symbol_string_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (new_line_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (whitespace_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (single_line_comment_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (multi_line_comment_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
    }

    inline for (symbol_string_tokens) |a| {
        try test_token_singular(tokeniser, a);
        inline for (word_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (left_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (right_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (balanced_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (symbol_string_tokens) |b| {
            if (comptime std.mem.startsWith(u8, a.value ++ b.value, "//")) {
                // Tokenising a string starting with "//" should create a result starting with a `.single_line_comment`.
                try test_token_combination(tokeniser, a, b, &[_]Token{Token{ .state = .single_line_comment, .value = a.value ++ b.value }});
            } else if (comptime std.mem.indexOf(u8, (a.value ++ b.value), "//")) |index| {
                // Tokenising a string starting with a `.symbol_string` and then "//" should result in `.symbol_string` followed by a `.single_line_comment`.
                try test_token_combination(tokeniser, a, b, &[_]Token{ Token{ .state = .symbol_string, .value = a.value[0..index] }, Token{ .state = .single_line_comment, .value = (a.value ++ b.value)[index..] } });
            } else if (comptime std.mem.containsAtLeast(u8, a.value ++ b.value, 1, "/*")) {
                // Tokenising a string containing a "/*" should result in a `Tokeniser.TokeniserError.MalformedText` because these tests don't include a `.multi_line_comment` closing tag.
                try std.testing.expectError(Tokeniser.TokeniserError.MalformedText, test_token_combination(tokeniser, a, b, &[0]Token{}));
            } else {
                // Tokenising two `.symbol_string`s should combine them into a single token.
                try test_token_combination(tokeniser, a, b, &[_]Token{Token{ .state = .symbol_string, .value = a.value ++ b.value }});
            }
        }
        inline for (new_line_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (whitespace_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (single_line_comment_tokens) |b| {
            if (comptime std.mem.startsWith(u8, a.value ++ b.value, "//")) {
                // Tokenising a string starting with "//" should create a result starting with a `.single_line_comment`.
                try test_token_combination(tokeniser, a, b, &[_]Token{Token{ .state = .single_line_comment, .value = a.value ++ b.value }});
            } else if (comptime std.mem.indexOf(u8, (a.value ++ b.value), "//")) |index| {
                // Tokenising a string starting with a `.symbol_string` and then "//" should result in `.symbol_string` followed by a `.single_line_comment`.
                try test_token_combination(tokeniser, a, b, &[_]Token{ Token{ .state = .symbol_string, .value = a.value[0..index] }, Token{ .state = .single_line_comment, .value = (a.value ++ b.value)[index..] } });
            } else {
                try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
            }
        }
        inline for (multi_line_comment_tokens) |b| {
            if (comptime std.mem.startsWith(u8, a.value ++ b.value, "//")) {
                // Tokenising a string starting with "//" should create a result starting with a `.single_line_comment`.
                var had_newline = false;
                inline for (new_line_tokens) |c| {
                    // Occurances of a `.new_line` should end the `.single_line_comment`.
                    if (comptime std.mem.indexOf(u8, a.value ++ b.value, c.value)) |nl_i| {
                        if (comptime nl_i + 2 == a.value.len + b.value.len) {
                            try test_token_combination(tokeniser, a, b, &[_]Token{ Token{ .state = .single_line_comment, .value = (a.value ++ b.value)[0..nl_i] }, Token{ .state = .new_line, .value = (a.value ++ b.value)[nl_i .. nl_i + c.value.len] } });
                        } else {
                            try test_token_combination(tokeniser, a, b, &[_]Token{ Token{ .state = .single_line_comment, .value = (a.value ++ b.value)[0..nl_i] }, Token{ .state = .new_line, .value = (a.value ++ b.value)[nl_i .. nl_i + c.value.len] }, Token{ .state = .symbol_string, .value = (a.value ++ b.value)[nl_i + c.value.len ..] } });
                        }
                        had_newline = true;
                        break;
                    }
                }
                if (!had_newline) {
                    try test_token_combination(tokeniser, a, b, &[_]Token{Token{ .state = .single_line_comment, .value = a.value ++ b.value }});
                }
            } else if (comptime std.mem.containsAtLeast(u8, a.value ++ b.value, 1, "//") and (!std.mem.containsAtLeast(u8, a.value ++ b.value, 1, "/*") or (std.mem.indexOf(u8, a.value ++ b.value, "//").? < std.mem.indexOf(u8, a.value ++ b.value, "/*").?))) {
                // Tokenising a string starting with a `.symbol_string` and then "//" should result in `.symbol_string` followed by a `.single_line_comment`.
                const slc_i = comptime std.mem.indexOf(u8, (a.value ++ b.value), "//").?;
                inline for (new_line_tokens) |c| {
                    // Occurances of a `.new_line` should end the `.single_line_comment`.
                    if (comptime std.mem.indexOf(u8, (a.value ++ b.value)[slc_i..], c.value)) |nl_i_offset| {
                        const nl_i = nl_i_offset + slc_i;
                        if (nl_i + 2 == a.value.len + b.value.len) {
                            try test_token_combination(tokeniser, a, b, &[_]Token{ Token{ .state = .symbol_string, .value = (a.value ++ b.value)[0..slc_i] }, Token{ .state = .single_line_comment, .value = (a.value ++ b.value)[slc_i..nl_i] }, Token{ .state = .new_line, .value = (a.value ++ b.value)[nl_i .. nl_i + c.value.len] } });
                        } else {
                            try test_token_combination(tokeniser, a, b, &[_]Token{ Token{ .state = .symbol_string, .value = (a.value ++ b.value)[0..slc_i] }, Token{ .state = .single_line_comment, .value = (a.value ++ b.value)[slc_i..nl_i] }, Token{ .state = .new_line, .value = (a.value ++ b.value)[nl_i .. nl_i + c.value.len] }, Token{ .state = .symbol_string, .value = (a.value ++ b.value)[nl_i + c.value.len ..] } });
                        }
                        break;
                    }
                } else {
                    try test_token_combination(tokeniser, a, b, &[_]Token{ Token{ .state = .symbol_string, .value = (a.value ++ b.value)[0..slc_i] }, Token{ .state = .single_line_comment, .value = (a.value ++ b.value)[slc_i..] } });
                }
            } else {
                try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
            }
        }
    }

    inline for (new_line_tokens) |a| {
        try test_token_singular(tokeniser, a);
        inline for (word_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (left_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (right_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (balanced_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (symbol_string_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (new_line_tokens) |b| {
            if (comptime std.mem.eql(u8, a.value, "\r") and std.mem.eql(u8, b.value, "\n")) {
                // Tokenising "\r\n" should result in a single `.new_line` containing "\r\n".
                try test_token_combination(tokeniser, a, b, &[_]Token{Token{ .state = .new_line, .value = a.value ++ b.value }});
            } else {
                try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
            }
        }
        inline for (whitespace_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (single_line_comment_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (multi_line_comment_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
    }

    inline for (whitespace_tokens) |a| {
        try test_token_singular(tokeniser, a);
        inline for (word_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (left_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (right_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (balanced_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (symbol_string_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (new_line_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        // Tokenising two `.whitespace`s should combine them into a single token.
        inline for (whitespace_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{Token{ .state = .whitespace, .value = a.value ++ b.value }});
        inline for (single_line_comment_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (multi_line_comment_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
    }

    inline for (single_line_comment_tokens) |a| {
        try test_token_singular(tokeniser, a);
        // Until a `.new_line` is present, any tokens following a `.single_line_comment` opening should be part of the comment.
        inline for (word_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{Token{ .state = .single_line_comment, .value = a.value ++ b.value }});
        inline for (left_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{Token{ .state = .single_line_comment, .value = a.value ++ b.value }});
        inline for (right_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{Token{ .state = .single_line_comment, .value = a.value ++ b.value }});
        inline for (balanced_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{Token{ .state = .single_line_comment, .value = a.value ++ b.value }});
        inline for (symbol_string_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{Token{ .state = .single_line_comment, .value = a.value ++ b.value }});
        // A `.new_line` should both close the `.single_line_comment` and preserve the `.new_line`.
        inline for (new_line_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (whitespace_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{Token{ .state = .single_line_comment, .value = a.value ++ b.value }});
        inline for (single_line_comment_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{Token{ .state = .single_line_comment, .value = a.value ++ b.value }});
        inline for (multi_line_comment_tokens) |b| {
            inline for (new_line_tokens) |c| {
                // Occurances of a `.new_line` should end the `.single_line_comment`.
                if (comptime std.mem.indexOf(u8, a.value ++ b.value, c.value)) |nl_i| {
                    if (nl_i + 2 == a.value.len + b.value.len) {
                        try test_token_combination(tokeniser, a, b, &[_]Token{ Token{ .state = .single_line_comment, .value = (a.value ++ b.value)[0..nl_i] }, Token{ .state = .new_line, .value = (a.value ++ b.value)[nl_i .. nl_i + c.value.len] } });
                    } else {
                        try test_token_combination(tokeniser, a, b, &[_]Token{ Token{ .state = .single_line_comment, .value = (a.value ++ b.value)[0..nl_i] }, Token{ .state = .new_line, .value = (a.value ++ b.value)[nl_i .. nl_i + c.value.len] }, Token{ .state = .symbol_string, .value = (a.value ++ b.value)[nl_i + c.value.len ..] } });
                    }
                    break;
                }
            } else {
                try test_token_combination(tokeniser, a, b, &[_]Token{Token{ .state = .single_line_comment, .value = a.value ++ b.value }});
            }
        }
    }

    inline for (multi_line_comment_tokens) |a| {
        try test_token_singular(tokeniser, a);
        inline for (word_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (left_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (right_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (balanced_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (symbol_string_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (new_line_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (whitespace_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (single_line_comment_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (multi_line_comment_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
    }
}

test "tokeniser well formed test" {
    // DelimiterPairsPairLengthMustBeTwo,
    try std.testing.expectError(Tokeniser.InitError.DelimiterPairsPairLengthMustBeTwo, Tokeniser.init(std.testing.allocator, "()!", &.{""}, "", null, null));
    try std.testing.expectError(Tokeniser.InitError.DelimiterPairsPairLengthMustBeTwo, Tokeniser.init(std.testing.allocator, "()!", &.{"("}, "", null, null));
    try std.testing.expectError(Tokeniser.InitError.DelimiterPairsPairLengthMustBeTwo, Tokeniser.init(std.testing.allocator, "()!", &.{"()!"}, "", null, null));

    // SpecialCharactersMustNotBeNewLines,
    try std.testing.expectError(Tokeniser.InitError.SpecialCharactersMustNotBeNewLines, Tokeniser.init(std.testing.allocator, "\r\n", &.{}, "", null, null));
    try std.testing.expectError(Tokeniser.InitError.SpecialCharactersMustNotBeNewLines, Tokeniser.init(std.testing.allocator, "\r", &.{}, "", null, null));
    try std.testing.expectError(Tokeniser.InitError.SpecialCharactersMustNotBeNewLines, Tokeniser.init(std.testing.allocator, "\n", &.{}, "", null, null));

    // SpecialCharactersMustNotBeWhitespace,
    try std.testing.expectError(Tokeniser.InitError.SpecialCharactersMustNotBeWhitespace, Tokeniser.init(std.testing.allocator, "\t", &.{}, "", null, null));
    try std.testing.expectError(Tokeniser.InitError.SpecialCharactersMustNotBeWhitespace, Tokeniser.init(std.testing.allocator, " ", &.{}, "", null, null));

    // DelimitersPairStartsMustBeSpecial,
    try std.testing.expectError(Tokeniser.InitError.DelimitersPairStartsMustBeSpecial, Tokeniser.init(std.testing.allocator, ")", &.{"()"}, "", null, null));

    // DelimitersPairEndsMustBeSpecial,
    try std.testing.expectError(Tokeniser.InitError.DelimitersPairEndsMustBeSpecial, Tokeniser.init(std.testing.allocator, "(", &.{"()"}, "", null, null));

    // BalancedDelimitersMustBeSpecial,
    try std.testing.expectError(Tokeniser.InitError.BalancedDelimitersMustBeSpecial, Tokeniser.init(std.testing.allocator, "", &.{}, "\"", null, null));

    // BalancedDelimitersMustNotBeDelimiterPairStarts,
    try std.testing.expectError(Tokeniser.InitError.BalancedDelimitersMustNotBeDelimiterPairStarts, Tokeniser.init(std.testing.allocator, "()", &.{"()"}, "(", null, null));

    // BalancedDelimitersMustNotBeDelimiterPairEnds,
    try std.testing.expectError(Tokeniser.InitError.BalancedDelimitersMustNotBeDelimiterPairEnds, Tokeniser.init(std.testing.allocator, "()", &.{"()"}, ")", null, null));

    // SingleLineCommentMustBeLongerThanZero,
    try std.testing.expectError(Tokeniser.InitError.SingleLineCommentMustBeLongerThanZero, Tokeniser.init(std.testing.allocator, "/*", &.{}, "", "", null));

    // SingleLineCommentMustBeEntirelySpecial,
    try std.testing.expectError(Tokeniser.InitError.SingleLineCommentMustBeEntirelySpecial, Tokeniser.init(std.testing.allocator, "", &.{}, "", "<", null));
    try std.testing.expectError(Tokeniser.InitError.SingleLineCommentMustBeEntirelySpecial, Tokeniser.init(std.testing.allocator, "<", &.{}, "", "<!", null));

    // SingleLineCommentMustNotIncludeDelimiterPairStarts,
    try std.testing.expectError(Tokeniser.InitError.SingleLineCommentMustNotIncludeDelimiterPairStarts, Tokeniser.init(std.testing.allocator, "()/\"", &.{"()"}, "\"", "(", null));
    try std.testing.expectError(Tokeniser.InitError.SingleLineCommentMustNotIncludeDelimiterPairStarts, Tokeniser.init(std.testing.allocator, "()/\"", &.{"()"}, "\"", "/(", null));

    // SingleLineCommentMustNotIncludeDelimiterPairEnds,
    try std.testing.expectError(Tokeniser.InitError.SingleLineCommentMustNotIncludeDelimiterPairEnds, Tokeniser.init(std.testing.allocator, "()/\"", &.{"()"}, "\"", ")", null));
    try std.testing.expectError(Tokeniser.InitError.SingleLineCommentMustNotIncludeDelimiterPairEnds, Tokeniser.init(std.testing.allocator, "()/\"", &.{"()"}, "\"", "/)", null));

    // SingleLineCommentMustNotIncludeBalancedDelimiters,
    try std.testing.expectError(Tokeniser.InitError.SingleLineCommentMustNotIncludeBalancedDelimiters, Tokeniser.init(std.testing.allocator, "()/\"", &.{"()"}, "\"", "\"", null));
    try std.testing.expectError(Tokeniser.InitError.SingleLineCommentMustNotIncludeBalancedDelimiters, Tokeniser.init(std.testing.allocator, "()/\"", &.{"()"}, "\"", "/\"", null));

    // SingleLineCommentMustNotContainMultiLineCommentStart,
    try std.testing.expectError(Tokeniser.InitError.SingleLineCommentMustNotContainMultiLineCommentStart, Tokeniser.init(std.testing.allocator, "/*", &.{}, "", "/*", .{ "/*", "*/" }));
    try std.testing.expectError(Tokeniser.InitError.SingleLineCommentMustNotContainMultiLineCommentStart, Tokeniser.init(std.testing.allocator, "/*", &.{}, "", "//*", .{ "/*", "*/" }));

    // SingleLineCommentMustNotContainMultiLineCommentEnd,
    try std.testing.expectError(Tokeniser.InitError.SingleLineCommentMustNotContainMultiLineCommentEnd, Tokeniser.init(std.testing.allocator, "/*", &.{}, "", "*/", .{ "/*", "*/" }));
    try std.testing.expectError(Tokeniser.InitError.SingleLineCommentMustNotContainMultiLineCommentEnd, Tokeniser.init(std.testing.allocator, "/*", &.{}, "", "*//", .{ "/*", "*/" }));

    // MultiLineCommentStartMustBeLongerThanZero,
    try std.testing.expectError(Tokeniser.InitError.MultiLineCommentStartMustBeLongerThanZero, Tokeniser.init(std.testing.allocator, "/*", &.{}, "", null, .{ "", "*/" }));

    // MultiLineCommentStartMustBeEntirelySpecial,
    try std.testing.expectError(Tokeniser.InitError.MultiLineCommentStartMustBeEntirelySpecial, Tokeniser.init(std.testing.allocator, ">", &.{}, "", null, .{ "<", ">" }));
    try std.testing.expectError(Tokeniser.InitError.MultiLineCommentStartMustBeEntirelySpecial, Tokeniser.init(std.testing.allocator, "<>", &.{}, "", null, .{ "<!", ">" }));

    // MultiLineCommentStartMustNotIncludeDelimiterPairStarts,
    try std.testing.expectError(Tokeniser.InitError.MultiLineCommentStartMustNotIncludeDelimiterPairStarts, Tokeniser.init(std.testing.allocator, "()<>/\"", &.{"()"}, "\"", null, .{ "(", ">" }));
    try std.testing.expectError(Tokeniser.InitError.MultiLineCommentStartMustNotIncludeDelimiterPairStarts, Tokeniser.init(std.testing.allocator, "()<>/\"", &.{"()"}, "\"", null, .{ "/(", ">" }));

    // MultiLineCommentStartMustNotIncludeDelimiterPairEnds,
    try std.testing.expectError(Tokeniser.InitError.MultiLineCommentStartMustNotIncludeDelimiterPairEnds, Tokeniser.init(std.testing.allocator, "()<>/\"", &.{"()"}, "\"", null, .{ ")", ">" }));
    try std.testing.expectError(Tokeniser.InitError.MultiLineCommentStartMustNotIncludeDelimiterPairEnds, Tokeniser.init(std.testing.allocator, "()<>/\"", &.{"()"}, "\"", null, .{ "/)", ">" }));

    // MultiLineCommentStartMustNotIncludeBalancedDelimiters,
    try std.testing.expectError(Tokeniser.InitError.MultiLineCommentStartMustNotIncludeBalancedDelimiters, Tokeniser.init(std.testing.allocator, "()<>/\"", &.{"()"}, "\"", null, .{ "\"", ">" }));
    try std.testing.expectError(Tokeniser.InitError.MultiLineCommentStartMustNotIncludeBalancedDelimiters, Tokeniser.init(std.testing.allocator, "()<>/\"", &.{"()"}, "\"", null, .{ "/\"", ">" }));

    // MultiLineCommentEndMustBeLongerThanZero,
    try std.testing.expectError(Tokeniser.InitError.MultiLineCommentEndMustBeLongerThanZero, Tokeniser.init(std.testing.allocator, "/*", &.{}, "", null, .{ "/*", "" }));

    // MultiLineCommentEndMustBeEntirelySpecial,
    try std.testing.expectError(Tokeniser.InitError.MultiLineCommentEndMustBeEntirelySpecial, Tokeniser.init(std.testing.allocator, "<", &.{}, "", null, .{ "<", ">" }));
    try std.testing.expectError(Tokeniser.InitError.MultiLineCommentEndMustBeEntirelySpecial, Tokeniser.init(std.testing.allocator, "<>", &.{}, "", null, .{ "<", "!>" }));

    // MultiLineCommentEndMustNotIncludeDelimiterPairStarts,
    try std.testing.expectError(Tokeniser.InitError.MultiLineCommentEndMustNotIncludeDelimiterPairStarts, Tokeniser.init(std.testing.allocator, "()<>/\"", &.{"()"}, "\"", null, .{ "<", "(" }));
    try std.testing.expectError(Tokeniser.InitError.MultiLineCommentEndMustNotIncludeDelimiterPairStarts, Tokeniser.init(std.testing.allocator, "()<>/\"", &.{"()"}, "\"", null, .{ "<", "/(" }));

    // MultiLineCommentEndMustNotIncludeDelimiterPairEnds,
    try std.testing.expectError(Tokeniser.InitError.MultiLineCommentEndMustNotIncludeDelimiterPairEnds, Tokeniser.init(std.testing.allocator, "()<>/\"", &.{"()"}, "\"", null, .{ "<", ")" }));
    try std.testing.expectError(Tokeniser.InitError.MultiLineCommentEndMustNotIncludeDelimiterPairEnds, Tokeniser.init(std.testing.allocator, "()<>/\"", &.{"()"}, "\"", null, .{ "<", "/)" }));

    // MultiLineCommentEndMustNotIncludeBalancedDelimiters,
    try std.testing.expectError(Tokeniser.InitError.MultiLineCommentEndMustNotIncludeBalancedDelimiters, Tokeniser.init(std.testing.allocator, "()<>/\"", &.{"()"}, "\"", null, .{ "<", "\"" }));
    try std.testing.expectError(Tokeniser.InitError.MultiLineCommentEndMustNotIncludeBalancedDelimiters, Tokeniser.init(std.testing.allocator, "()<>/\"", &.{"()"}, "\"", null, .{ "<", "/\"" }));

    // MultiLineCommentStartMustNotContainSingleLineComment,
    try std.testing.expectError(Tokeniser.InitError.MultiLineCommentStartMustNotContainSingleLineComment, Tokeniser.init(std.testing.allocator, "/*", &.{}, "", "//", .{ "//*", "*/" }));

    // MultiLineCommentEndMustNotContainSingleLineComment,
    try std.testing.expectError(Tokeniser.InitError.MultiLineCommentEndMustNotContainSingleLineComment, Tokeniser.init(std.testing.allocator, "/*", &.{}, "", "//", .{ "/*", "*//" }));
}
