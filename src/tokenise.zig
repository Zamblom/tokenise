const std = @import("std");
const Grapheme = @import("grapheme");
const PropsData = @import("PropsData");

const Side = enum {
    balanced,
    left,
    right,
    none,
};

// pub fn is_whitespace(c: &str) -> bool {
//     c.trim().is_empty()
// }

pub const Token = struct {
    state: State,
    value: []const u8,
    byte_offset: usize,
    grapheme_offset: usize,

    pub const State = enum {
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
        end,
    };
};

pub const Tokeniser = struct {
    /// A set of graphemes that are treated as special graphemes
    special_graphemes: []const Grapheme.Grapheme,
    /// A pair of graphemes defining the left and right hand side of delimiter pair
    delimiter_pairs: []const ScalarDelimiterPair,
    balanced_delimiters: []const ScalarDelimiter,
    /// A pair of sets of utf8 encoded graphemes defining the left and right hand side of a single line comment
    single_line_comment: ?SequenceDelimiterPair,
    /// A pair of sets of utf8 encoded graphemes defining the left and right hand side of a multi line comment
    multi_line_comment: ?SequenceDelimiterPair,

    allocator: std.mem.Allocator,
    /// The current source (used for tokenisation)
    source: []const u8 = undefined,
    /// The current grapheme
    grapheme: Grapheme.Grapheme = undefined,
    /// The grapheme iterator
    grapheme_iterator: Grapheme.Iterator = undefined,
    /// The current offset into source in bytes (used for tokenisation)
    byte_offset: usize = 0,
    /// The current offset into source in graphemes (used for tokenisation)
    grapheme_offset: usize = 0,
    /// The current list of tokens (used for tokenisation)
    tokens: std.ArrayListUnmanaged(Token) = .empty,

    sequence: ?Sequence,

    const Sequence = struct {
        state: Token.State,
        start_byte_offset: usize,
        start_grapheme_offset: usize,
    };

    /// A scalar delimiter
    pub const ScalarDelimiter = Grapheme.Grapheme;

    /// A sequence delimiter
    pub const SequenceDelimiter = []const Grapheme.Grapheme;

    /// A pair of scalar delimiters
    pub const ScalarDelimiterPair = struct {
        left: ScalarDelimiter,
        right: ScalarDelimiter,
    };

    /// A pair of sequence delimiters
    pub const SequenceDelimiterPair = struct {
        left: SequenceDelimiter,
        right: SequenceDelimiter,
    };

    /// Adds a single grapheme token to the output
    /// There must not be a currently active sequence
    fn addSingleGraphemeToken(self: *@This(), state: Token.State, grapheme: Grapheme.Grapheme) !void {
        std.debug.assert(self.sequence == null);
        // TODO: Assert that state is valid

        try self.tokens.append(.{
            .state = state,
            .value = try self.allocator.dupe(u8, grapheme.bytes(self.source)),
            .byte_offset = self.byte_offset,
            .grapheme_offset = self.grapheme_offset,
        });
    }

    /// Begins a sequence
    fn startSequence(self: *@This(), state: Token.State) bool {
        std.debug.assert(self.sequence == null);
        // TODO: Assert that state is valid

        self.sequence = .{
            .state = state,
            .start_byte_offset = self.byte_offset,
            .start_grapheme_offset = self.grapheme_offset,
        };
    }

    /// Ends a sequence and adds a multi grapheme token to the output
    /// There must be a currently active sequence
    /// Returns state to `none`.
    fn endSequence(self: *@This(), grapheme: Grapheme.Grapheme) !void {
        std.debug.assert(self.sequence != null);

        try self.tokens.append(.{
            .state = self.sequence.state,
            .value = try self.allocator.dupe(u8, self.source[self.sequence.start_byte_offset .. self.byte_offset + grapheme.len]),
            .byte_offset = self.sequence.start_byte_offset,
            .grapheme_offset = self.sequence.start_grapheme_offset,
        });

        self.sequence = null;
    }

    /// Checks whether the given grapheme is either the left or right side of a delimiter pair, or a balanced delimiter, or returns none.
    fn getDelimiterSide(self: *@This(), grapheme: Grapheme.Grapheme) Side {
        for (self.delimiter_pairs) |delimiter_pair| {
            if (std.mem.eql(u8, delimiter_pair.left.bytes(), grapheme.bytes())) return .left;
            if (std.mem.eql(u8, delimiter_pair.right.bytes(), grapheme.bytes())) return .right;
        }

        for (self.balanced_delimiters) |balanced_delimiter| {
            if (std.mem.eql(u8, balanced_delimiter.bytes(), grapheme.bytes())) return .balanced;
        }

        return .none;
    }

    /// Checks whether the given `grapheme` is `special`.
    fn isSpecial(self: *const @This(), grapheme: Grapheme.Grapheme) bool {
        for (self.special_graphemes) |special_grapheme| {
            if (std.mem.eql(grapheme.bytes(), special_grapheme.bytes())) return true;
        } else return false;
    }

    /// Checks whether the given grapheme appears at the start of the `single_line_comment`
    fn isSingleLineCommentStart(self: *const @This(), grapheme: Grapheme.Grapheme) bool {
        if (self.single_line_comment == null) return false;
        return std.mem.eql(u8, self.single_line_comment[0].bytes(), grapheme.bytes(self.source));
    }

    /// Checks whether the given grapheme appears at the start of the left of the `multi_line_comment`
    fn isMultiLineCommentStart(self: *@This(), grapheme: Grapheme.Grapheme) bool {
        if (self.multi_line_comment == null) return false;
        return std.mem.eql(u8, self.multi_line_comment.left[0].bytes(), grapheme.bytes(self.source));
    }

    /// Checks whether the given grapheme is a new line
    fn isNewLine(self: *@This(), grapheme: Grapheme.Grapheme) bool {
        return std.mem.eql(u8, grapheme.bytes(self.source), "\n");
    }

    /// Checks whether the given grapheme is whitespace
    fn isWhitespace(self: @This(), grapheme: Grapheme.Grapheme) bool {
        for (grapheme.bytes(self.source)) |code_point| {
            if (!self.pd.isWhitespace(code_point)) return false;
        } else return true;
    }

    /// If so, the `grapheme` is handled accordingly and `true` is returned, else false.
    fn handleSpecial(self: *@This(), grapheme: Grapheme.Grapheme) !void {
        if (self.isSingleLineCommentStart(grapheme)) {
            self.startSequence(.single_line_comment);
            return;
        }

        if (self.isMultiLineCommentStartFromNone(grapheme)) {
            self.startSequence(.multi_line_comment);
            return;
        }

        const delimiter_side = self.getDelimiterSide(grapheme);
        if (delimiter_side != .none) {
            try self.handleDelimiterSide(delimiter_side, grapheme);
            return;
        }

        self.startSequence(.symbol_string);
    }

    fn handleDelimiterSide(self: *@This(), delimiter_side: Side, grapheme: Grapheme.Grapheme) !void {
        switch (delimiter_side) {
            .left => try self.addSingleGraphemeToken(.left_delimiter, grapheme),
            .right => try self.addSingleGraphemeToken(.right_delimiter, grapheme),
            .balanced => try self.addSingleGraphemeToken(.balanced_delimiter, grapheme),
            .none => unreachable,
        }
    }

    fn handleNewLine(self: *@This(), grapheme: Grapheme.Grapheme) bool {
        try self.addSingleGraphemeToken(.new_line, grapheme);
        return true;
    }

    /// The grapheme is handled accordingly and true is returned.
    fn handleWordFromNone(self: *@This(), grapheme: Grapheme.Grapheme) bool {
        _ = grapheme;
        self.startSequence(.word);
        return true;
    }

    /// Checks whether the given grapheme is either the left or right side of a delimiter pair, or a balanced delimiter, or returns none.
    /// TODO: This should probably use a hash map to avoid excessive looping string compares
    fn delimiterSide(self: *const @This(), grapheme: Grapheme.Grapheme) Side {
        for (self.delimiter_pairs) |delimiter_pair| {
            if (std.mem.eql(u8, delimiter_pair.left.bytes(), grapheme.bytes())) return .left;
            if (std.mem.eql(u8, delimiter_pair.right.bytes(), grapheme.bytes())) return .right;
        }

        for (self.balanced_delimiters) |balanced_delimiter| {
            if (std.mem.eql(u8, balanced_delimiter.bytes(), grapheme.bytes())) return .balanced;
        }

        return .none;
    }

    fn advanceState(self: *@This()) Token.State {
        self.byte_offset += self.grapheme.len;
        self.grapheme_offset += 1;
        const grapheme = self.grapheme_iterator.next();
        self.grapheme = grapheme orelse return .end;
        return if (self.sequence) |s| s.state else .none;
    }

    /// Tokenises a given unicode grapheme sequence and returns a slice of tokens.
    /// The tokens values are references to the source and, as such, should not outlive it.
    pub fn tokenise(self: *const @This(), allocator: std.mem.Allocator, source: []const u8) ![]Token {
        self.gd = try Grapheme.GraphemeData.init(allocator);
        defer self.gd.deinit();

        self.pd = try PropsData.init(allocator);
        defer self.pd.deinit();

        errdefer self.tokens.deinit(allocator);

        self.source = source;
        self.grapheme_iterator = Grapheme.Iterator.init(self.source, &self.gd);
        self.grapheme = self.grapheme_iterator.next();

        tokeniser: switch (self.sequence_state) {
            .none => {
                if (self.isSpecial()) {
                    if (self.isSingleLineCommentStart()) {
                        self.startSequence(.single_line_comment);
                        continue :tokeniser self.advanceState();
                    }

                    if (self.isMultiLineCommentStartFromNone()) {
                        self.startSequence(.multi_line_comment);
                        continue :tokeniser self.advanceState();
                    }

                    switch (self.getDelimiterSide()) {
                        .left => try self.addSingleGraphemeToken(.left_delimiter),
                        .right => try self.addSingleGraphemeToken(.right_delimiter),
                        .balanced => try self.addSingleGraphemeToken(.balanced_delimiter),
                        .none => self.startSequence(.symbol_string),
                    }
                    continue :tokeniser self.advanceState();
                }

                if (self.isNewLine()) {
                    try self.addSingleGraphemeToken(.new_line);
                    continue :tokeniser self.advanceState();
                }

                if (try self.isWhitespace()) {
                    try self.startSequence(.whitespace);
                    continue :tokeniser self.advanceState();
                }

                self.startSequence(.word);
                continue :tokeniser self.advanceState();
            },
            .word => {
                if (self.isSpecial()) {
                    try self.endSequence();

                    switch (self.getDelimiterSide()) {
                        .left => try self.addSingleGraphemeToken(.left_delimiter),
                        .right => try self.addSingleGraphemeToken(.right_delimiter),
                        .balanced => try self.addSingleGraphemeToken(.balanced_delimiter),
                        .none => self.startSequence(.symbol_string),
                    }
                    continue :tokeniser self.advanceState();
                }

                if (self.isNewLine()) {
                    try self.endSequence();
                    try self.addSingleGraphemeToken(.new_line);
                    continue :tokeniser self.advanceState();
                }

                if (self.isWhitespace()) {
                    try self.endSequence();
                    self.startSequence(.whitespace);
                    continue :tokeniser self.advanceState();
                }

                continue :tokeniser self.advanceState();
            },
            .symbol_string => {
                if (self.isSpecial()) {}
            },
            .end => {},
        }

        return &[0]Token{};
    }
};

//         for (curr_pos,c) in text.grapheme_indices(true) {
//             match curr_state {
//                 Some(SymbolString) => {
//                     if self.special(c) {
//                         let curr_str = &text[curr_start..curr_pos+c.len()];
//                         if self.ends_with_sl_comment_start(curr_str) {
//                             if self.is_sl_comment_start(curr_str) {
//                                 curr_state = Some(SLComment);
//                             } else {
//                                 let new_start = curr_pos + c.len() - self.sl_comment().unwrap().len();
//                                 out.push(Token {
//                                     state: SymbolString,
//                                     val: &text[curr_start..new_start],
//                                     start_pos: curr_start
//                                 });
//                                 curr_state = Some(SLComment);
//                                 curr_start = new_start;
//                             }
//                         } else if self.ends_with_ml_comment_start(curr_str) {
//                             if self.is_ml_comment_start(curr_str) {
//                                 curr_state = Some(MLComment);
//                             } else {
//                                 let new_start = curr_pos + c.len() - self.ml_comment().unwrap().0.len();
//                                 out.push(Token {
//                                     state: SymbolString,
//                                     val: &text[curr_start..new_start],
//                                     start_pos: curr_start
//                                 });
//                                 curr_state = Some(MLComment);
//                                 curr_start = new_start;
//                             }
//                         }
//                      } else {
//                         out.push(Token { state: SymbolString, val: &text[curr_start..curr_pos], start_pos: curr_start });
//                         curr_start = curr_pos;
//                         if is_whitespace(c) {
//                             if c == "\n" {
//                                 out.push(Token {
//                                     state: NewLine,
//                                     val: c,
//                                     start_pos: curr_pos
//                                 });
//                                 curr_state = None;
//                             } else {
//                                 curr_state = Some(WhiteSpace);
//                             }
//                         } else {
//                             curr_state = Some(Word);
//                         }
//                     }
//                 },
//                 Some(WhiteSpace) => {
//                     if self.special(c) {
//                         out.push(Token {
//                             state: WhiteSpace,
//                             val: &text[curr_start..curr_pos],
//                             start_pos: curr_start
//                         });
//                         match self.delimiter(c) {
//                             None => {
//                                 if self.is_sl_comment_start(c) {
//                                     curr_state = Some(SLComment);
//                                 } else if self.is_ml_comment_start(c) {
//                                     curr_state = Some(MLComment);
//                                 } else {
//                                     curr_state = Some(SymbolString);
//                                 }
//                                 curr_start = curr_pos;
//                             },
//                             Some(Side::Left) => {
//                                 out.push(Token { state: LDelimiter, val: c, start_pos: curr_pos });
//                                 curr_state = None;
//                             },
//                             Some(Side::Right) => {
//                                 out.push(Token { state: RDelimiter, val: c, start_pos: curr_pos });
//                                 curr_state = None;
//                             },
//                             Some(Side::Bal) => {
//                                 out.push(Token { state: BDelimiter, val: c, start_pos: curr_pos });
//                                 curr_state = None;
//                             }
//                         }
//                     } else {
//                         if c == "\n" {
//                             out.push(Token {
//                                 state: WhiteSpace,
//                                 val: &text[curr_start..curr_pos],
//                                 start_pos: curr_start
//                             });
//                             out.push(Token {
//                                 state: NewLine,
//                                 val: c,
//                                 start_pos: curr_pos
//                             });
//                             curr_state = None;
//                         } else if !is_whitespace(c) {
//                             out.push(Token {
//                                 state: WhiteSpace,
//                                 val: &text[curr_start..curr_pos],
//                                 start_pos: curr_start
//                             });
//                             curr_start = curr_pos;
//                             curr_state = Some(Word);
//                         }
//                     }
//                 },
//                 Some(SLComment) => {
//                     if c == "\n" {
//                         out.push(Token {
//                             state: SLComment,
//                             val: &text[curr_start..curr_pos],
//                             start_pos: curr_start
//                         });
//                         out.push(Token {
//                             state: NewLine,
//                             val: c,
//                             start_pos: curr_pos
//                         });
//                         curr_state = None;
//                     }
//                 },
//                 Some(MLComment) => {
//                     let curr_str = &text[curr_start..curr_pos+(c.len())];
//                     let end = match self.ml_comment() {
//                         Some((_, e)) => Ok(e),
//                         _ => Err("This should never happen".to_string())
//                     }.unwrap();
//                     if curr_str.ends_with(end) {
//                         out.push(Token {
//                             state: MLComment,
//                             val: &text[curr_start..curr_pos+(c.len())],
//                             start_pos: curr_start
//                         });
//                         curr_state = None;
//                     }
//                 },
//                 other => {return Err(format!("curr_state should never reach {:?}",other))}
//             }
//         }
//         if let Some(token) = out.last() {
//             if token.value().len() + token.start() != text.len() {
//                 out.push(Token {
//                     state: curr_state.unwrap(),
//                     val: &text[curr_start..],
//                     start_pos: curr_start
//                 });
//             }
//         }
//         Ok(out)
//     }
// }

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn token_gets_work() {
//         let token = Token{ state: Word, val:"hi", start_pos:4};
//         assert_eq!(token.start(), 4);
//         assert_eq!(token.value(), "hi");
//     }

//     #[test]
//     fn tokeniser_new_works() {
//         let tokeniser = Tokeniser::new("!@%👨‍💻*", &vec!["<>", "()", "{}", "🇺🇸👋🏽"], "\"", Some("//"), Some(("/*","*/"))).unwrap();
//         assert_eq!(tokeniser.specials(),&vec!["!", "@", "%", "👨‍💻", "*"]);
//         assert_eq!(tokeniser.lr_delimiters(),&vec![("<",">"),("(",")"),("{","}"),("🇺🇸","👋🏽")]);
//         assert_eq!(tokeniser.sl_comment(),Some("//"));
//         assert_eq!(tokeniser.ml_comment(),Some(("/*","*/")));
//     }

//     #[test]
//     fn tokeniser_tokenise_works() {
//         let source = " hi, skdjfs;;    842\t 39fsl == 3\n \
//         what's going on? idk... \n\
//         fire___sldfksfl // what's going on? \n\
//         idk what I'm 🇺🇸doing \n\
//         \n\
//          now👋🏽 hi £*$*@ \n\
//         help!\n\
//         \"hello\"hi";
//         let tokeniser = Tokeniser::new(r#",;=?.'*)(/£<>@🇺🇸👋🏽""#,&vec!["()","[]"], "\"", Some("//"), Some(("🇺🇸","👋🏽"))).unwrap();
//         assert_eq!(tokeniser.tokenise(source).unwrap(),vec![
//             Token { state: WhiteSpace, val: " ", start_pos: 0 },
//             Token { state: Word, val: "hi", start_pos: 1 },
//             Token { state: SymbolString, val: ",", start_pos: 3 },
//             Token { state: WhiteSpace, val: " ", start_pos: 4 },
//             Token { state: Word, val: "skdjfs", start_pos: 5 },
//             Token { state: SymbolString, val: ";;", start_pos: 11 },
//             Token { state: WhiteSpace, val: "    ", start_pos: 13 },
//             Token { state: Word, val: "842", start_pos: 17 },
//             Token { state: WhiteSpace, val: "\t ", start_pos: 20 },
//             Token { state: Word, val: "39fsl", start_pos: 22 },
//             Token { state: WhiteSpace, val: " ", start_pos: 27 },
//             Token { state: SymbolString, val: "==", start_pos: 28 },
//             Token { state: WhiteSpace, val: " ", start_pos: 30 },
//             Token { state: Word, val: "3", start_pos: 31 },
//             Token { state: NewLine, val: "\n", start_pos: 32 },
//             Token { state: WhiteSpace, val: " ", start_pos: 33 },
//             Token { state: Word, val: "what", start_pos: 34 },
//             Token { state: SymbolString, val: "'", start_pos: 38 },
//             Token { state: Word, val: "s", start_pos: 39 },
//             Token { state: WhiteSpace, val: " ", start_pos: 40 },
//             Token { state: Word, val: "going", start_pos: 41 },
//             Token { state: WhiteSpace, val: " ", start_pos: 46 },
//             Token { state: Word, val: "on", start_pos: 47 },
//             Token { state: SymbolString, val: "?", start_pos: 49 },
//             Token { state: WhiteSpace, val: " ", start_pos: 50 },
//             Token { state: Word, val: "idk", start_pos: 51 },
//             Token { state: SymbolString, val: "...", start_pos: 54 },
//             Token { state: WhiteSpace, val: " ", start_pos: 57 },
//             Token { state: NewLine, val: "\n", start_pos: 58 },
//             Token { state: Word, val: "fire___sldfksfl", start_pos: 59 },
//             Token { state: WhiteSpace, val: " ", start_pos: 74 },
//             Token { state: SLComment, val: "// what's going on? ", start_pos: 75 },
//             Token { state: NewLine, val: "\n", start_pos: 95 },
//             Token { state: Word, val: "idk", start_pos: 96 },
//             Token { state: WhiteSpace, val: " ", start_pos: 99 },
//             Token { state: Word, val: "what", start_pos: 100 },
//             Token { state: WhiteSpace, val: " ", start_pos: 104 },
//             Token { state: Word, val: "I", start_pos: 105 },
//             Token { state: SymbolString, val: "'", start_pos: 106 },
//             Token { state: Word, val: "m", start_pos: 107 },
//             Token { state: WhiteSpace, val: " ", start_pos: 108 },
//             Token { state: MLComment, val: "🇺🇸doing \n\nnow👋🏽", start_pos: 109 },
//             Token { state: WhiteSpace, val: " ", start_pos: 136 },
//             Token { state: Word, val: "hi", start_pos: 137 },
//             Token { state: WhiteSpace, val: " ", start_pos: 139 },
//             Token { state: SymbolString, val: "£*", start_pos: 140 },
//             Token { state: Word, val: "$", start_pos: 143 },
//             Token { state: SymbolString, val: "*@", start_pos: 144 },
//             Token { state: WhiteSpace, val: " ", start_pos: 146 },
//             Token { state: NewLine, val: "\n", start_pos: 147 },
//             Token { state: Word, val: "help!", start_pos: 148 },
//             Token { state: NewLine, val: "\n", start_pos: 153 },
//             Token { state: BDelimiter, val: "\"", start_pos: 154 },
//             Token { state: Word, val: "hello", start_pos: 155 },
//             Token { state: BDelimiter, val: "\"", start_pos: 160 },
//             Token { state: Word, val: "hi", start_pos: 161 }
//         ]);
//     }
// }
