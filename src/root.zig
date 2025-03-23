//! # Tokenise
//!
//! A flexible lexical analyser (tokeniser) for parsing text into configurable token types.
//!
//! `tokenise` allows you to split text into tokens based on customisable rules for special characters,
//! delimiters, and comments. It's designed to be flexible enough to handle various syntax styles
//! while remaining simple to configure.
//!
//! ## Basic Usage
//!
//! The following example demonstrates how to configure a tokeniser with common syntax elements
//! and process a simple code snippet:
//!
//! ```
//! use tokenise::Tokeniser;
//!
//! fn main() {
//!     // Create a new tokeniser
//!     let mut tokeniser = Tokeniser::new();
//!
//!     // Configure tokeniser with rules
//!     tokeniser.add_specials(".,;:!?");
//!     tokeniser.add_delimiter_pairs(&vec!["()", "[]", "{}"]).unwrap();
//!     tokeniser.add_balanced_delimiter("\"").unwrap();
//!     tokeniser.set_sl_comment("//").unwrap();
//!     tokeniser.set_ml_comment("/*", "*/").unwrap();
//!
//!     // Tokenise some source text
//!     let source = "let x = 42; // The answer\nprint(\"Hello world!\");";
//!     let tokens = tokeniser.tokenise(source).unwrap();
//!
//!     // Work with the resulting tokens
//!     for token in tokens {
//!         println!("{:?}: '{}'", token.get_state(), token.value());
//!     }
//! }
//! ```
//!
//! ## Features
//!
//! - Unicode support (using grapheme clusters)
//! - Configurable special characters and delimiters
//! - Support for paired delimiters (e.g., parentheses, brackets)
//! - Support for balanced delimiters (e.g., quotation marks)
//! - Single-line and multi-line comment handling
//! - Whitespace and newline preservation
//!
//! ## Token Types
//!
//! The tokeniser recognises several token types represented by the `TokenState` enum:
//!
//! - `Word`: Non-special character sequences (anything not identified as a special character or whitespace)
//! - `LDelimiter`/`RDelimiter`: Left/right delimiters of a pair (e.g., '(', ')')
//! - `BDelimiter`: Balanced delimiters (e.g., quotation marks)
//! - `SymbolString`: Special characters
//! - `NewLine`: Line breaks
//! - `WhiteSpace`: Spaces, tabs, etc.
//! - `SLComment`: Single-line comments
//! - `MLComment`: Multi-line comments
//!
//! More precise definitions can be found in the documentation for each specific type.

const std = @import("std");

const grapheme = @import("grapheme");
const PropsData = @import("PropsData");

/// Represents the type of a token in the tokenisation process.
///
/// Each token in the parsed text is classified as one of these types,
/// which determines how it is interpreted and processed.
pub const TokenState = enum {
    /// A sequence of non-special characters (excluding whitespace).
    word,
    /// A start delimiter of a pair (e.g., opening bracket).
    start_delimiter,
    /// An end delimiter of a pair (e.g., closing bracket).
    end_delimiter,
    /// A balanced delimiter that can open or close (e.g., quotation mark).
    balanced_delimiter,
    /// A sequence of special characters not recognized as delimiters or comments.
    symbol_string,
    /// A newline character sequence (\n, \r, or \r\n).
    new_line,
    /// A sequence of whitespace characters (excluding newlines).
    whitespace,
    /// A single-line comment.
    single_line_comment,
    /// A multi-line comment.
    multi_line_comment,
    /// A null state. Never emitted.
    none,
};

/// Represents a token extracted from the source text during tokenisation.
///
/// Each token has a state (type), and a string value.
///
/// The value slice is owned by the token and must be freed.
pub const Token = struct {
    /// The type of the token.
    state: TokenState,
    /// The string content of the token.
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

/// A single grapheme cluster
///
/// This name is for readability only; its just a slice of `u8`.
const GraphemeCluster = []const u8;

/// Frees a slice of `GraphemeCluster`
fn freeGraphemeClusters(gpa: std.mem.Allocator, s: []const GraphemeCluster) void {
    for (s) |g| gpa.free(g);
    gpa.free(s);
}

/// Returns `true` if `a` and `b` are equal, else `false`.
fn graphemeClusterEql(a: []const GraphemeCluster, b: []const GraphemeCluster) bool {
    if (a.len != b.len) return false;
    for (a, b) |i, j| if (!std.mem.eql(u8, i, j)) return false;
    return true;
}

/// Returns `true` if `a` ends with `b`, else `false`.
fn graphemeClustersEndWith(a: []const GraphemeCluster, b: []const GraphemeCluster) bool {
    if (a.len < b.len) return false;
    const a_relevant = a[a.len - b.len ..];
    return graphemeClusterEql(a_relevant, b);
}

/// Splits a slice of `u8` into a slice of `GraphemeCluster`.
///
/// Caller owns the returned slice.
fn toGraphemeClusters(gpa: std.mem.Allocator, grapheme_data: *const grapheme.GraphemeData, bytes: []const u8) std.mem.Allocator.Error![]const GraphemeCluster {
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

/// Combines a slice of `GraphemeCluster` into a slice of `u8`.
///
/// Caller owns the returned slice.
fn fromGraphemeCluster(gpa: std.mem.Allocator, s: []const GraphemeCluster) std.mem.Allocator.Error![]const u8 {
    var bytes: std.ArrayListUnmanaged(u8) = .empty;
    defer bytes.deinit(gpa);

    for (s) |g| try bytes.appendSlice(gpa, g);
    return try bytes.toOwnedSlice(gpa);
}

/// A configurable tokeniser for parsing text into meaningful tokens.
///
/// The `Tokeniser` can be customised with special characters, delimiter pairs,
/// balanced delimiters, and comment markers to suit different syntax requirements.
/// Once configured, it can parse text into tokens according to those rules.
///
/// Note that delimiters and the characters in comment markers are automatically
/// treated as special characters, but with additional distinctions in how they're
/// processed during tokenisation.
///
/// ### Example Start
/// ```
/// // Create and configure a tokeniser for a C-like language
/// var tokeniser: Tokeniser = try .init(gpa);
/// defer tokeniser.deinit(gpa);
///
/// try tokeniser.addSpecials(gpa, "+-*/=<>!&|^~%");
/// try tokeniser.addDelimiterPairs(gpa, &.{"()", "[]", "{}"})
/// try tokeniser.setSingleLineComment(gpa, "//");
/// try tokeniser.setMultiLineComment(gpa, "/*", "*/");
///
/// // Tokenise some code
/// let code = "int main() { // Entry point\n    return 0;\n}";
/// let tokens = try tokeniser.tokenise(code);
/// ```
/// ### Example End
pub const Tokeniser = struct {
    specials: std.StringHashMapUnmanaged(void) = .empty,
    start_delimiters: std.StringHashMapUnmanaged(void) = .empty,
    end_delimiters: std.StringHashMapUnmanaged(void) = .empty,
    balanced_delimiters: std.StringHashMapUnmanaged(void) = .empty,
    single_line_comment: ?[]const GraphemeCluster = null,
    multi_line_comment: ?[2][]const GraphemeCluster = null,
    grapheme_data: grapheme.GraphemeData,
    props_data: PropsData,

    pub const InitError = error{
        GraphemeDataFailedToInit,
        PropsDataFailedToInit,
    };

    pub const SetupError = error{
        CommentsCannotBeZeroLength,
        CommentsCannotContainDelimiters,
        DelimiterAlreadyExists,
        DelimitersMustBeSingleGraphemeClusters,
        PairedDelimitersCannotMatch,
        PairedDelimitersMustBeDualGraphemeClusters,
        SpecialsMustBeSingleGraphemeClusters,
    } || std.mem.Allocator.Error;

    pub const TokeniserError = error{
        MalformedText,
    } || std.mem.Allocator.Error;

    /// Initialises a new, unconfigured `Tokeniser`.
    ///
    /// This constructor creates a tokeniser with no special characters, delimiters, or comment markers.
    ///
    /// You'll need to configure it with the appropriate methods before it's ready for use.
    pub fn init(gpa: std.mem.Allocator) InitError!@This() {
        return .{
            .grapheme_data = grapheme.GraphemeData.init(gpa) catch return InitError.GraphemeDataFailedToInit,
            .props_data = PropsData.init(gpa) catch return InitError.PropsDataFailedToInit,
        };
    }

    /// Adds a single special character to the tokeniser.
    ///
    /// Special characters are treated differently from regular text during tokenisation.
    /// They form `symbol_string` tokens unless they're also configured as delimiters or
    /// used in comment markers.
    ///
    /// ### Arguments
    ///
    /// * `special` - The special character to add, which must be a single grapheme
    ///
    /// ### Example Start
    /// ```
    /// var tokeniser: Tokeniser = try .init(gpa);
    /// defer tokeniser.deinit(gpa);
    ///
    /// try tokeniser.addSpecial(gpa, "@");
    /// try tokeniser.addSpecial(gpa, "+");
    ///
    /// // Unicode graphemes are supported
    /// try tokeniser.addSpecial("👨‍💻").unwrap();
    ///
    /// // This would return an error as it's not a single grapheme
    /// tokeniser.addSpecial(gpa, "abc");
    /// ```
    /// ### Example End
    pub fn addSpecial(self: *@This(), gpa: std.mem.Allocator, special: []const u8) SetupError!void {
        if (!self.isSingleGraphemeCluster(special)) return SetupError.SpecialsMustBeSingleGraphemeClusters;
        if (self.isSpecial(special)) return;

        try self.specials.ensureUnusedCapacity(gpa, 1);
        self.specials.putAssumeCapacity(try gpa.dupe(u8, special), {});
    }

    /// Adds multiple special characters to the tokeniser.
    ///
    /// This is a convenience method that adds each grapheme in the input string
    /// as a special character.
    ///
    /// ### Arguments
    ///
    /// * `specials` - A string containing the special characters to add
    ///
    /// ### Example Start
    /// ```
    /// var tokeniser: Tokeniser = .init(gpa);
    /// try tokeniser.addSpecials("+-*/=<>!&|^~%");
    /// ```
    /// ### Example End
    pub fn addSpecials(self: *@This(), gpa: std.mem.Allocator, specials: []const u8) SetupError!void {
        const count = self.graphemeClusterCount(specials);
        try self.specials.ensureUnusedCapacity(gpa, @intCast(count));

        var it = grapheme.Iterator.init(specials, &self.grapheme_data);
        while (it.next()) |gc| self.addSpecial(gpa, gc.bytes(specials)) catch unreachable;
    }

    /// Adds a pair of left and right delimiters to the tokeniser.
    ///
    /// Delimiter pairs are used to mark the beginning and end of sections in text,
    /// such as parentheses, brackets, and braces. During tokenisation, they are
    /// classified as `start_delimiter` and `end_delimiter` respectively.
    ///
    /// Both characters are automatically added as special characters if they aren't already.
    ///
    /// ### Arguments
    ///
    /// * `start` - The start (opening) delimiter, which must be a single grapheme
    /// * `end` - The end (closing) delimiter, which must be a single grapheme
    ///
    /// # Example Start
    /// ```
    /// var tokeniser: Tokeniser = try .init(gpa);
    /// defer tokeniser.deinit(gpa);
    ///
    /// try tokeniser.addDelimiterPair(gpa, "(", ")");
    /// try tokeniser.addDelimiterPair(gpa, "[", "]");
    /// try tokeniser.addDelimiterPair(gpa, "{", "}");
    ///
    /// // Unicode delimiters are supported
    /// try tokeniser.addDelimiterPair(gpa, "「", "」");
    /// ```
    /// ### Example End
    pub fn addDelimiterPair(self: *@This(), gpa: std.mem.Allocator, start: []const u8, end: []const u8) SetupError!void {
        if (!self.isSingleGraphemeCluster(start)) return SetupError.DelimitersMustBeSingleGraphemeClusters;
        if (!self.isSingleGraphemeCluster(end)) return SetupError.DelimitersMustBeSingleGraphemeClusters;
        if (std.mem.eql(u8, start, end)) return SetupError.PairedDelimitersCannotMatch;
        if (self.getDelimiterSide(start) != .none) return SetupError.DelimiterAlreadyExists;
        if (self.getDelimiterSide(end) != .none) return SetupError.DelimiterAlreadyExists;

        if (self.single_line_comment) |slc| {
            for (slc) |gc| {
                if (std.mem.eql(u8, gc, start)) return SetupError.CommentsCannotContainDelimiters;
                if (std.mem.eql(u8, gc, end)) return SetupError.CommentsCannotContainDelimiters;
            }
        }

        if (self.multi_line_comment) |mlc| {
            for (mlc[0]) |gc| {
                if (std.mem.eql(u8, gc, start)) return SetupError.CommentsCannotContainDelimiters;
                if (std.mem.eql(u8, gc, end)) return SetupError.CommentsCannotContainDelimiters;
            }
            for (mlc[1]) |gc| {
                if (std.mem.eql(u8, gc, start)) return SetupError.CommentsCannotContainDelimiters;
                if (std.mem.eql(u8, gc, end)) return SetupError.CommentsCannotContainDelimiters;
            }
        }

        const start_owned_delim = try gpa.dupe(u8, start);
        errdefer gpa.free(start_owned_delim);
        const end_owned_delim = try gpa.dupe(u8, end);
        errdefer gpa.free(end_owned_delim);
        const start_owned_special = try gpa.dupe(u8, start);
        errdefer gpa.free(start_owned_special);
        const end_owned_special = try gpa.dupe(u8, end);
        errdefer gpa.free(end_owned_special);

        try self.start_delimiters.ensureUnusedCapacity(gpa, 1);
        try self.end_delimiters.ensureUnusedCapacity(gpa, 1);
        try self.specials.ensureUnusedCapacity(gpa, 2);

        self.start_delimiters.putAssumeCapacity(start_owned_delim, {});
        self.end_delimiters.putAssumeCapacity(end_owned_delim, {});
        if (!self.specials.contains(start)) self.specials.putAssumeCapacity(start_owned_special, {}) else gpa.free(start_owned_special);
        if (!self.specials.contains(end)) self.specials.putAssumeCapacity(end_owned_special, {}) else gpa.free(end_owned_special);
    }

    /// Adds multiple delimiter pairs to the tokeniser.
    ///
    /// Each pair should be represented as a string containing exactly two graphemes,
    /// where the first is the start delimiter and the second is the end delimiter.
    ///
    /// Each character is automatically added as a special character if it isn't already.
    ///
    /// ### Arguments
    ///
    /// * `delimiter_pairs` - A slice of strings, each containing exactly two graphemes
    ///
    /// ### Example Start
    /// ```
    /// var tokeniser: Tokeniser = try .init(gpa);
    /// defer tokeniser.deinit(gpa);
    ///
    /// try tokeniser.addDelimiterPairs(&.{"()", "[]", "{}"});
    /// ```
    /// ### Example End
    pub fn addDelimiterPairs(self: *@This(), gpa: std.mem.Allocator, pairs: []const []const u8) SetupError!void {
        var additions_start: std.StringArrayHashMapUnmanaged(void) = .empty;
        defer additions_start.deinit(gpa);
        errdefer {
            var it = additions_start.iterator();
            while (it.next()) |gc| _ = self.start_delimiters.remove(gc.key_ptr.*);
        }

        var additions_end: std.StringArrayHashMapUnmanaged(void) = .empty;
        defer additions_end.deinit(gpa);
        errdefer {
            var it = additions_end.iterator();
            while (it.next()) |gc| _ = self.end_delimiters.remove(gc.key_ptr.*);
        }

        var additions_special: std.StringArrayHashMapUnmanaged(void) = .empty;
        defer additions_special.deinit(gpa);
        errdefer {
            var it = additions_special.iterator();
            while (it.next()) |gc| _ = self.specials.remove(gc.key_ptr.*);
        }

        try additions_start.ensureUnusedCapacity(gpa, pairs.len);
        try additions_end.ensureUnusedCapacity(gpa, pairs.len);
        try additions_special.ensureUnusedCapacity(gpa, pairs.len * 2);

        for (pairs) |pair| {
            var it = grapheme.Iterator.init(pair, &self.grapheme_data);
            const start = (it.next() orelse return SetupError.PairedDelimitersMustBeDualGraphemeClusters).bytes(pair);
            const end = (it.next() orelse return SetupError.PairedDelimitersMustBeDualGraphemeClusters).bytes(pair);
            if (it.next() != null) return SetupError.PairedDelimitersMustBeDualGraphemeClusters;

            if (!self.start_delimiters.contains(start)) additions_start.putAssumeCapacity(start, {});
            if (!self.end_delimiters.contains(end)) additions_end.putAssumeCapacity(end, {});
            if (!self.specials.contains(start)) additions_special.putAssumeCapacity(start, {});
            if (!self.specials.contains(end)) additions_special.putAssumeCapacity(end, {});
            try self.addDelimiterPair(gpa, start, end);
        }
    }

    /// Adds a balanced delimiter to the tokeniser.
    ///
    /// Balanced delimiters are characters that serve as both opening and closing markers,
    /// such as quotation marks. During tokenisation, they are classified as `balanced_delimiter`.
    ///
    /// The character is automatically added as a special character if it isn't already.
    ///
    /// ### Arguments
    ///
    /// * `balanced` - The balanced delimiter, which must be a single grapheme
    ///
    /// ### Example Start
    /// ```
    /// var tokeniser: Tokeniser = try .init(gpa);
    /// defer tokeniser.deinit(gpa);
    ///
    /// try tokeniser.addBalancedDelimiter(gpa, "\""); // Double quote
    /// try tokeniser.addBalancedDelimiter(gpa, "'");  // Single quote
    /// try tokeniser.addBalancedDelimiter(gpa, "`");  // Backtick
    /// ```
    /// ### Example End
    pub fn addBalancedDelimiter(self: *@This(), gpa: std.mem.Allocator, balanced: []const u8) SetupError!void {
        if (!self.isSingleGraphemeCluster(balanced)) return SetupError.DelimitersMustBeSingleGraphemeClusters;
        if (self.getDelimiterSide(balanced) != .none) return SetupError.DelimiterAlreadyExists;

        if (self.single_line_comment) |slc| {
            for (slc) |gc| if (std.mem.eql(u8, gc, balanced)) return SetupError.CommentsCannotContainDelimiters;
        }

        if (self.multi_line_comment) |mlc| {
            for (mlc[0]) |gc| if (std.mem.eql(u8, gc, balanced)) return SetupError.CommentsCannotContainDelimiters;
            for (mlc[1]) |gc| if (std.mem.eql(u8, gc, balanced)) return SetupError.CommentsCannotContainDelimiters;
        }

        const balanced_owned_delim = try gpa.dupe(u8, balanced);
        const balanced_owned_special = try gpa.dupe(u8, balanced);

        try self.balanced_delimiters.ensureUnusedCapacity(gpa, 1);
        try self.specials.ensureUnusedCapacity(gpa, 1);

        self.balanced_delimiters.putAssumeCapacity(balanced_owned_delim, {});
        if (!self.specials.contains(balanced)) self.specials.putAssumeCapacity(balanced_owned_special, {}) else gpa.free(balanced_owned_special);
    }

    /// Adds multiple balanced delimiters to the tokeniser.
    ///
    /// Each character in the input string is added as a balanced delimiter.
    /// The characters are automatically added as special characters if they aren't already.
    ///
    /// ### Arguments
    ///
    /// * `balanced_delimiters` - A string containing the balanced delimiters to add
    ///
    /// ### Example Start
    ///
    /// ```
    /// var tokeniser: Tokeniser = try .init(gpa);
    /// defer tokeniser.deinit(gpa);
    ///
    /// try tokeniser.addBalancedDelimiters(gpa, "\"'`"); // Adds ", ', and ` as balanced delimiters
    /// ```
    /// ### Example End
    pub fn addBalancedDelimiters(self: *@This(), gpa: std.mem.Allocator, balanced_delimiters: []const u8) SetupError!void {
        var additions_balanced: std.StringArrayHashMapUnmanaged(void) = .empty;
        defer additions_balanced.deinit(gpa);
        errdefer {
            var it = additions_balanced.iterator();
            while (it.next()) |gc| _ = self.balanced_delimiters.remove(gc.key_ptr.*);
        }

        var additions_special: std.StringArrayHashMapUnmanaged(void) = .empty;
        defer additions_special.deinit(gpa);
        errdefer {
            var it = additions_special.iterator();
            while (it.next()) |gc| _ = self.specials.remove(gc.key_ptr.*);
        }

        const count = self.graphemeClusterCount(balanced_delimiters);
        try additions_balanced.ensureUnusedCapacity(gpa, count);
        try additions_special.ensureUnusedCapacity(gpa, count);

        var it = grapheme.Iterator.init(balanced_delimiters, &self.grapheme_data);
        while (it.next()) |gc| {
            const balanced_owned = gc.bytes(balanced_delimiters);
            if (!self.balanced_delimiters.contains(balanced_owned)) additions_balanced.putAssumeCapacity(balanced_owned, {});
            if (!self.specials.contains(balanced_owned)) additions_special.putAssumeCapacity(balanced_owned, {});
            try self.addBalancedDelimiter(gpa, balanced_owned);
        }
    }

    /// Sets the marker for single-line comments.
    ///
    /// Single-line comments run from the marker to the end of the line.
    /// During tokenisation, they are classified as `single_line_comment`.
    ///
    /// All characters in the comment marker are automatically added as special characters.
    ///
    /// ### Arguments
    ///
    /// * `comm` - The single-line comment marker (e.g., "//")
    ///
    /// ### Example Start
    /// ```
    /// var tokeniser: Tokeniser = try .init(gpa)
    /// defer tokeniser.deinit(gpa);
    ///
    /// try tokeniser.setSingleLineComment(gpa, "//");  // C/C++/Rust/Zig style
    ///
    /// // Could also use other styles
    /// // try tokeniser.setSingleLineComment(gpa, "#").unwrap();   // Python/Ruby style
    /// // try tokeniser.setSingleLineComment(gpa, "--").unwrap();  // SQL/Lua style
    /// ```
    /// ### Example End
    pub fn setSingleLineComment(self: *@This(), gpa: std.mem.Allocator, start: []const u8) SetupError!void {
        if (start.len == 0) return SetupError.CommentsCannotBeZeroLength;

        const gcs_slc = try toGraphemeClusters(gpa, &self.grapheme_data, start);
        errdefer freeGraphemeClusters(gpa, gcs_slc);
        for (gcs_slc) |gc| if (self.getDelimiterSide(gc) != .none) return SetupError.CommentsCannotContainDelimiters;

        const gcs_specials = try toGraphemeClusters(gpa, &self.grapheme_data, start);
        defer gpa.free(gcs_specials);

        try self.specials.ensureUnusedCapacity(gpa, @intCast(gcs_specials.len));

        // Free existing single_line_comment if present.
        if (self.single_line_comment) |slc| freeGraphemeClusters(gpa, slc);

        // Set single_line_comment and add each grapheme cluster to specials.
        self.single_line_comment = gcs_slc;
        for (gcs_specials) |gc| {
            if (!self.specials.contains(gc)) self.specials.putAssumeCapacity(gc, {}) else gpa.free(gc);
        }
    }

    /// Sets the markers for multi-line comments.
    ///
    /// Multi-line comments run from the start marker to the end marker,
    /// potentially spanning multiple lines. During tokenisation, they
    /// are classified as `multi_line_comment`.
    ///
    /// All characters in both comment markers are automatically added as special characters.
    ///
    /// ### Arguments
    ///
    /// * `start` - The start marker for multi-line comments (e.g., "/*")
    /// * `end` - The end marker for multi-line comments (e.g., "*/")
    ///
    /// ### Example Start
    /// ```
    /// var tokeniser: Tokeniser = try .init(gpa);
    /// defer tokeniser.deinit(gpa);
    ///
    /// try tokeniser.setMultiLineComment(gpa, "/*", "*/");  // C/C++/Rust style
    ///
    /// // Could also use other styles
    /// // try tokeniser.setMultiLineComment(gpa, "<!--", "-->"); // HTML/XML style
    /// // try tokeniser.setMultiLineComment(gpa, "{-", "-}");    // Haskell style
    /// ```
    /// ### Example End
    ///
    /// ### Warning Start
    /// Be cautious with comment markers that contain alphanumeric characters (like words).
    /// Since all characters in comment markers are added as special characters, using
    /// word-based markers may cause unexpected tokenisation of normal text:
    ///
    /// ```
    /// var tokeniser: Tokeniser = try .init(gpa);
    /// defer tokeniser.deinit(gpa);
    ///
    /// // Not recommended - would treat the letters in "begin" and "end" as special characters
    /// try tokeniser.setMultiLineComment("=begin", "=end").unwrap(); // Ruby style
    /// ```
    /// ### Warning End
    pub fn setMultiLineComment(self: *@This(), gpa: std.mem.Allocator, start: []const u8, end: []const u8) SetupError!void {
        if (start.len == 0) return SetupError.CommentsCannotBeZeroLength;
        if (end.len == 0) return SetupError.CommentsCannotBeZeroLength;

        const gcs_mlc_start = try toGraphemeClusters(gpa, &self.grapheme_data, start);
        errdefer freeGraphemeClusters(gpa, gcs_mlc_start);
        for (gcs_mlc_start) |gc| if (self.getDelimiterSide(gc) != .none) return SetupError.CommentsCannotContainDelimiters;

        const gcs_mlc_end = try toGraphemeClusters(gpa, &self.grapheme_data, end);
        errdefer freeGraphemeClusters(gpa, gcs_mlc_end);
        for (gcs_mlc_start) |gc| if (self.getDelimiterSide(gc) != .none) return SetupError.CommentsCannotContainDelimiters;

        const gcs_specials_start = try toGraphemeClusters(gpa, &self.grapheme_data, start);
        defer gpa.free(gcs_specials_start);

        const gcs_specials_end = try toGraphemeClusters(gpa, &self.grapheme_data, end);
        defer gpa.free(gcs_specials_end);

        try self.specials.ensureUnusedCapacity(gpa, @intCast(gcs_specials_start.len + gcs_specials_end.len));

        // Free existing multi_line_comment if present.
        if (self.multi_line_comment) |mlc| {
            freeGraphemeClusters(gpa, mlc[0]);
            freeGraphemeClusters(gpa, mlc[1]);
        }

        // Set multi_line_comment and add each grapheme cluster to specials.
        self.multi_line_comment = .{ gcs_mlc_start, gcs_mlc_end };
        for (gcs_specials_start) |gc| {
            if (!self.specials.contains(gc)) self.specials.putAssumeCapacity(gc, {}) else gpa.free(gc);
        }
        for (gcs_specials_end) |gc| {
            if (!self.specials.contains(gc)) self.specials.putAssumeCapacity(gc, {}) else gpa.free(gc);
        }
    }

    /// Frees all memory allocated during setup.
    ///
    /// This function should be called after tokenisation is complete.
    pub fn deinit(self: *@This(), gpa: std.mem.Allocator) void {
        var special_it = self.specials.iterator();
        while (special_it.next()) |entry| gpa.free(entry.key_ptr.*);
        self.specials.deinit(gpa);

        var start_it = self.start_delimiters.iterator();
        while (start_it.next()) |entry| gpa.free(entry.key_ptr.*);
        self.start_delimiters.deinit(gpa);

        var end_it = self.end_delimiters.iterator();
        while (end_it.next()) |entry| gpa.free(entry.key_ptr.*);
        self.end_delimiters.deinit(gpa);

        var balanced_it = self.balanced_delimiters.iterator();
        while (balanced_it.next()) |entry| gpa.free(entry.key_ptr.*);
        self.balanced_delimiters.deinit(gpa);

        if (self.single_line_comment) |slc| freeGraphemeClusters(gpa, slc);

        if (self.multi_line_comment) |mlc| {
            freeGraphemeClusters(gpa, mlc[0]);
            freeGraphemeClusters(gpa, mlc[1]);
        }

        self.grapheme_data.deinit();
        self.props_data.deinit();
    }

    /// Returns the number of grapheme clusters in the given string.
    fn graphemeClusterCount(self: *const @This(), s: []const u8) usize {
        var count: usize = 0;
        var it = grapheme.Iterator.init(s, &self.grapheme_data);
        while (it.next() != null) count += 1;
        return count;
    }

    /// Returns whether the given string consists of exactly one grapheme cluster.
    fn isSingleGraphemeCluster(self: *const @This(), s: []const u8) bool {
        var it = grapheme.Iterator.init(s, &self.grapheme_data);
        return (it.next() orelse return false).len == s.len;
    }

    /// Returns whether the given grapheme was marked as special during init.
    fn isSpecial(self: *const @This(), g: GraphemeCluster) bool {
        return self.specials.contains(g);
    }

    /// Returns whether the given grapheme is a newline.
    fn isNewLine(self: *const @This(), g: GraphemeCluster) bool {
        _ = self;
        return std.mem.eql(u8, "\r", g) or std.mem.eql(u8, "\r\n", g) or std.mem.eql(u8, "\n", g);
    }

    /// Returns whether the given grapheme is whitespace.
    ///
    /// Newlines return false to aid tokenisation.
    fn isWhitespace(self: *const @This(), g: GraphemeCluster) bool {
        if (isNewLine(self, g)) return false;
        for (g) |code_point| if (!self.props_data.isWhitespace(code_point)) return false;
        return true;
    }

    /// Returns whether the given grapheme is part of a word.
    fn isWord(self: *const @This(), g: GraphemeCluster) bool {
        return !self.isSpecial(g) and !self.isWhitespace(g) and !self.isNewLine(g);
    }

    /// Returns what type of delimiter the given grapheme is.
    ///
    /// Returns `.none` if the grapheme isn't a delimiter.
    fn getDelimiterSide(self: *const @This(), g: GraphemeCluster) enum { start, end, balanced, none } {
        if (self.start_delimiters.contains(g)) return .start;
        if (self.end_delimiters.contains(g)) return .end;
        if (self.balanced_delimiters.contains(g)) return .balanced;
        return .none;
    }

    /// Returns true if `s` ends with a `single_line_comment` start
    fn endsWithSingleLineCommentStart(self: *const @This(), s: []const GraphemeCluster) bool {
        if (self.single_line_comment == null) return false;
        return graphemeClustersEndWith(s, self.single_line_comment.?);
    }

    /// Returns true if `s` ends with a `multi_line_comment` start
    fn endsWithMultiLineCommentStart(self: *const @This(), s: []const GraphemeCluster) bool {
        if (self.multi_line_comment == null) return false;
        return graphemeClustersEndWith(s, self.multi_line_comment.?[0]);
    }

    /// Returns true if `s` ends with a `multi_line_comment` end
    fn endsWithMultiLineCommentEnd(self: *const @This(), s: []const GraphemeCluster) bool {
        std.debug.assert(self.multi_line_comment != null);
        return graphemeClustersEndWith(s, self.multi_line_comment.?[1]);
    }

    /// A helper function that combines the slice of `GraphemeCluster` into a slice of `u8`,
    /// combines it with `state` into a `Token`,
    /// and appends the token to `out`.
    fn addToken(gpa: std.mem.Allocator, out: *std.ArrayListUnmanaged(Token), state: TokenState, value: []const GraphemeCluster) std.mem.Allocator.Error!void {
        const value_bytes = try fromGraphemeCluster(gpa, value);
        errdefer gpa.free(value_bytes);
        try out.append(gpa, .{ .state = state, .value = value_bytes });
    }

    /// Tokenises a string according to the configured rules.
    ///
    /// This is the main method of the library, converting a string into a sequence of tokens
    /// based on the special characters, delimiters, and comment markers that have been configured.
    ///
    /// ### Arguments
    ///
    /// * `text` - The string to tokenise
    ///
    /// ### Returns
    ///
    /// * `[]Token` - A slice of `Token`. Owned by the caller, call deinit on each token to free.
    ///
    /// ### Example Start
    /// ```
    /// var tokeniser: Tokeniser = try .init(gpa);
    /// defer tokeniser.deinit(gpa);
    ///
    /// try tokeniser.addSpecials(gpa, "+-*/=");
    /// try tokeniser.addDelimiterPairs(gpa, &.{"()", "[]"});
    /// try tokeniser.setSingleLineComment(gpa, "//");
    ///
    /// const source = "x = 42; // The answer";
    /// const tokens = try tokeniser.tokenise(gpa, source);
    ///
    /// // We can now work with the tokens
    /// for (tokens) |token| {
    ///     switch (token.state) {
    ///         .word => std.debug.print("Word: {s}\n", .{token.value}),
    ///         .symbol_string => std.debug.print("Symbol String: {s}\n", .{token.value});
    ///         .single_line_comment => std.debug.print("Single Line Comment: {s}\n", .{token.value}),
    ///         inline else => std.debug.print("Other: {s}\n", .{token.value}),
    ///     }
    /// }
    /// ```
    /// ### Example End
    pub fn tokenise(self: *const @This(), gpa: std.mem.Allocator, text: []const u8) TokeniserError![]Token {
        const gcs_text: []const GraphemeCluster = try toGraphemeClusters(gpa, &self.grapheme_data, text);
        defer freeGraphemeClusters(gpa, gcs_text);

        var out: std.ArrayListUnmanaged(Token) = .empty;
        errdefer out.deinit(gpa);
        errdefer for (out.items) |token| token.deinit(gpa);

        var current_start: usize = 0;
        var state: TokenState = .none;

        for (gcs_text, 0..) |g, i| {
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
                            .start => {
                                try @This().addToken(gpa, &out, .start_delimiter, &.{g});
                                current_start += 1;
                                continue;
                            },
                            .end => {
                                try @This().addToken(gpa, &out, .end_delimiter, &.{g});
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
                    try @This().addToken(gpa, &out, .word, gcs_text[current_start..i]);
                    state = .none;
                    current_start = i;
                    continue :state_switch state;
                },
                .symbol_string => {
                    if (self.isSpecial(g)) {
                        const curr_str = gcs_text[current_start .. i + 1];

                        if (self.endsWithSingleLineCommentStart(curr_str)) {
                            // We've got the start of a `.single_line_comment`.
                            state = .single_line_comment;

                            if (curr_str.len > self.single_line_comment.?.len) {
                                // There's a token before the comment opening, push it.
                                const new_start: usize = i + 1 - self.single_line_comment.?.len;
                                try @This().addToken(gpa, &out, .symbol_string, gcs_text[current_start..new_start]);
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
                                try @This().addToken(gpa, &out, .symbol_string, gcs_text[current_start..new_start]);
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
                                try @This().addToken(gpa, &out, .symbol_string, gcs_text[current_start..i]);
                                state = .none;
                                current_start = i;
                                continue :state_switch state;
                            },
                        }
                        unreachable;
                    }

                    // The `.symbol_string` is over, push the token and reset `state` to `.none`
                    // and jump to the `.none` branch to handle the lastest grapheme.
                    try @This().addToken(gpa, &out, .symbol_string, gcs_text[current_start..i]);
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
                    try @This().addToken(gpa, &out, .whitespace, gcs_text[current_start..i]);
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
                    try @This().addToken(gpa, &out, .single_line_comment, gcs_text[current_start..i]);
                    current_start = i;
                    state = .none;
                    continue :state_switch state;
                },
                .multi_line_comment => {
                    std.debug.assert(self.multi_line_comment != null);

                    const current_str = gcs_text[current_start .. i + 1];

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

        if (current_start != gcs_text.len) switch (state) {
            // We've not pushed the last token, check it's valid and then do so.
            .symbol_string => try @This().addToken(gpa, &out, .symbol_string, gcs_text[current_start..]),
            .whitespace => try @This().addToken(gpa, &out, .whitespace, gcs_text[current_start..]),
            .word => try @This().addToken(gpa, &out, .word, gcs_text[current_start..]),
            .single_line_comment => try @This().addToken(gpa, &out, .single_line_comment, gcs_text[current_start..]),
            inline else => return TokeniserError.MalformedText,
        };

        return try out.toOwnedSlice(gpa);
    }
};

test "tokeniser setup works" {
    var tokeniser = try Tokeniser.init(std.testing.allocator);
    defer tokeniser.deinit(std.testing.allocator);

    try tokeniser.addSpecials(std.testing.allocator, "!@%");
    try tokeniser.addDelimiterPairs(std.testing.allocator, &.{ "<>", "()", "{}", "🇺🇸👋🏽" });
    try tokeniser.addBalancedDelimiter(std.testing.allocator, "\"");
    try tokeniser.setSingleLineComment(std.testing.allocator, "//");
    try tokeniser.setMultiLineComment(std.testing.allocator, "/*", "*/");

    try std.testing.expect(tokeniser.specials.count() == 14);
    try std.testing.expect(tokeniser.specials.contains("!"));
    try std.testing.expect(tokeniser.specials.contains("@"));
    try std.testing.expect(tokeniser.specials.contains("%"));
    try std.testing.expect(tokeniser.specials.contains("<"));
    try std.testing.expect(tokeniser.specials.contains(">"));
    try std.testing.expect(tokeniser.specials.contains("("));
    try std.testing.expect(tokeniser.specials.contains(")"));
    try std.testing.expect(tokeniser.specials.contains("{"));
    try std.testing.expect(tokeniser.specials.contains("}"));
    try std.testing.expect(tokeniser.specials.contains("🇺🇸"));
    try std.testing.expect(tokeniser.specials.contains("👋🏽"));
    try std.testing.expect(tokeniser.specials.contains("\""));
    try std.testing.expect(tokeniser.specials.contains("/"));
    try std.testing.expect(tokeniser.specials.contains("*"));

    try std.testing.expect(tokeniser.start_delimiters.count() == 4);
    try std.testing.expect(tokeniser.start_delimiters.contains("<"));
    try std.testing.expect(tokeniser.start_delimiters.contains("("));
    try std.testing.expect(tokeniser.start_delimiters.contains("{"));
    try std.testing.expect(tokeniser.start_delimiters.contains("🇺🇸"));

    try std.testing.expect(tokeniser.end_delimiters.count() == 4);
    try std.testing.expect(tokeniser.end_delimiters.contains(">"));
    try std.testing.expect(tokeniser.end_delimiters.contains(")"));
    try std.testing.expect(tokeniser.end_delimiters.contains("}"));
    try std.testing.expect(tokeniser.end_delimiters.contains("👋🏽"));

    try std.testing.expect(tokeniser.balanced_delimiters.count() == 1);
    try std.testing.expect(tokeniser.balanced_delimiters.contains("\""));

    try std.testing.expectEqualDeep(@as([]const []const u8, &.{ "/", "/" }), tokeniser.single_line_comment.?);

    try std.testing.expectEqualDeep([2][]const []const u8{ &.{ "/", "*" }, &.{ "*", "/" } }, tokeniser.multi_line_comment.?);
}

test "long tokenise test" {
    var tokeniser = try Tokeniser.init(std.testing.allocator);
    defer tokeniser.deinit(std.testing.allocator);

    try tokeniser.addSpecials(std.testing.allocator, ",;=?.'*£<>@");
    try tokeniser.addDelimiterPairs(std.testing.allocator, &.{ "()", "[]" });
    try tokeniser.addBalancedDelimiter(std.testing.allocator, "\"");
    try tokeniser.setSingleLineComment(std.testing.allocator, "//");
    try tokeniser.setMultiLineComment(std.testing.allocator, "🇺🇸", "👋🏽");

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

test "singular and combination tokenise test" {
    const word_tokens = [_]Token{
        Token{ .state = .word, .value = "a" },
        Token{ .state = .word, .value = "A" },
    };

    const start_delimiter_tokens = [_]Token{
        Token{ .state = .start_delimiter, .value = "(" },
        Token{ .state = .start_delimiter, .value = "[" },
    };

    const end_delimiter_tokens = [_]Token{
        Token{ .state = .end_delimiter, .value = ")" },
        Token{ .state = .end_delimiter, .value = "]" },
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

    var tokeniser = try Tokeniser.init(std.testing.allocator);
    defer tokeniser.deinit(std.testing.allocator);

    try tokeniser.addSpecials(std.testing.allocator, ";");
    try tokeniser.addDelimiterPairs(std.testing.allocator, &.{ "()", "[]" });
    try tokeniser.addBalancedDelimiters(std.testing.allocator, "\"'");
    try tokeniser.setSingleLineComment(std.testing.allocator, "//");
    try tokeniser.setMultiLineComment(std.testing.allocator, "/*", "*/");

    @setEvalBranchQuota(50000);

    inline for (word_tokens) |a| {
        try test_token_singular(tokeniser, a);
        // Tokenising two `.word`s should combine them into a single token.
        inline for (word_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{Token{ .state = .word, .value = a.value ++ b.value }});
        inline for (start_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (end_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (balanced_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (symbol_string_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (new_line_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (whitespace_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (single_line_comment_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (multi_line_comment_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
    }

    inline for (start_delimiter_tokens) |a| {
        try test_token_singular(tokeniser, a);
        inline for (word_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (start_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (end_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (balanced_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (symbol_string_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (new_line_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (whitespace_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (single_line_comment_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (multi_line_comment_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
    }

    inline for (end_delimiter_tokens) |a| {
        try test_token_singular(tokeniser, a);
        inline for (word_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (start_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (end_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
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
        inline for (start_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (end_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
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
        inline for (start_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (end_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
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
        inline for (start_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (end_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
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
        inline for (start_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (end_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
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
        inline for (start_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{Token{ .state = .single_line_comment, .value = a.value ++ b.value }});
        inline for (end_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{Token{ .state = .single_line_comment, .value = a.value ++ b.value }});
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
        inline for (start_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (end_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (balanced_delimiter_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (symbol_string_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (new_line_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (whitespace_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (single_line_comment_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
        inline for (multi_line_comment_tokens) |b| try test_token_combination(tokeniser, a, b, &[_]Token{ a, b });
    }
}
