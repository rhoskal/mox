const std = @import("std");

const ascii = std.ascii;
const assert = std.debug.assert;

pub const KeywordKind = enum {
    Alias,
    As,
    Else,
    Exposing,
    Fn,
    Foreign,
    Hiding,
    If,
    Include,
    InfixLeft,
    InfixNon,
    InfixRight,
    Let,
    Match,
    Module,
    On,
    Open,
    Then,
    Type,
    Using,
    When,
};

pub const OperatorKind = enum {
    // Arithmetic
    Exponent,
    FloatAdd,
    FloatDiv,
    FloatMul,
    FloatSub,
    IntAdd,
    IntDiv,
    IntMul,
    IntSub,

    // Comparison (Relational)
    Equality,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
    NotEqual,

    // Logical (Boolean)
    LogicalAnd,
    LogicalOr,

    // Functions
    ThinArrow,
    FatArrow,
    ComposeLeft,
    ComposeRight,
    PipeLeft,
    PipeRight,

    // Lists
    ListCons,
    ListConcat,

    // Strings
    StringConcat,

    // Assignment
    Equal,

    // Miscellaneous
    Expand,
    Pipe,
};

pub const DelimiterKind = enum {
    Colon,
    Comma,
    Dot,
    LeftBrace,
    LeftBracket,
    LeftParen,
    RightBrace,
    RightBracket,
    RightParen,
};

pub const LiteralKind = enum {
    Char,
    Float,
    Int,
    MultilineString,
    String,
};

pub const IdentifierKind = enum {
    Lower,
    Upper,
};

pub const SymbolKind = enum {
    TypeHole,
    Underscore,
};

pub const CommentKind = enum {
    Doc,
    Regular,
};

pub const SpecialKind = enum {
    Eof,
    Unrecognized,
};

/// A union enum representing the different kinds of tokens in the language.
/// Each category has its own enum defining the specific token types.
pub const TokenKind = union(enum) {
    comment: CommentKind,
    delimiter: DelimiterKind,
    identifier: IdentifierKind,
    keyword: KeywordKind,
    literal: LiteralKind,
    operator: OperatorKind,
    special: SpecialKind,
    symbol: SymbolKind,

    pub fn format(
        kind: TokenKind,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (kind) {
            .comment => |v| try writer.print(".comment = {s}", .{@tagName(v)}),
            .delimiter => |v| try writer.print(".delimiter = {s}", .{@tagName(v)}),
            .identifier => |v| try writer.print(".identifier = {s}", .{@tagName(v)}),
            .keyword => |v| try writer.print(".keyword = {s}", .{@tagName(v)}),
            .literal => |v| try writer.print(".literal = {s}", .{@tagName(v)}),
            .operator => |v| try writer.print(".operator = {s}", .{@tagName(v)}),
            .special => |v| try writer.print(".special = {s}", .{@tagName(v)}),
            .symbol => |v| try writer.print(".symbol = {s}", .{@tagName(v)}),
        }
    }
};

/// A human-readable line/column position.
pub const SourceLoc = struct {
    /// The line number in the source file (1-based).
    line: usize,

    /// The column number in the source file (1-based).
    col: usize,
};

/// A structure representing raw buffer locations.
pub const Span = struct {
    /// The start index of the lexeme in the source buffer (inclusive).
    start: usize,

    /// The end index of the lexeme in the source buffer (exclusive).
    end: usize,
};

/// Combines both raw and human-readable locations.
/// This dual representation enables both efficient string slicing (Span)
/// and meaningful error reporting (SourceLoc) without conversion overhead.
pub const TokenLoc = struct {
    /// The name of the source file where the token originated.
    filename: []const u8,

    /// The human-readable source location of the token.
    src: SourceLoc,

    /// The raw buffer location of the token.
    span: Span,
};

/// A structure representing a token with a specific kind, lexeme, and position in the source code.
pub const Token = struct {
    /// The kind of token (e.g., literal, keyword)
    kind: TokenKind,

    /// The string representation of the token
    lexeme: []const u8,

    /// The complete location information for the token
    loc: TokenLoc,

    pub fn init(kind: TokenKind, lexeme: []const u8, loc: TokenLoc) Token {
        // Assert location spans are valid
        assert(loc.span.start <= loc.span.end);
        // Assert line numbers are 1-based
        assert(loc.src.line > 0);
        // Assert column numbers are 1-based
        assert(loc.src.col > 0);
        // Assert lexeme length matches span
        assert(lexeme.len == loc.span.end - loc.span.start);

        return .{
            .kind = kind,
            .lexeme = lexeme,
            .loc = loc,
        };
    }

    pub fn format(
        token: Token,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("Token({s}) ({s}) ({d},{d}) ({d},{d})", .{
            token.lexeme,
            token.kind,
            token.loc.span.start,
            token.loc.span.end,
            token.loc.src.line,
            token.loc.src.col,
        });
    }
};

pub const LexerError = error{
    CodePointOutOfRange,
    EmptyCharLiteral,
    EmptyMultilineStringLiteral,
    InvalidFloatLiteral,
    InvalidIdentifier,
    InvalidIntLiteral,
    InvalidUnicodeEscapeSequence,
    MultipleCharsInLiteral,
    UnrecognizedCharEscapeSequence,
    UnrecognizedStrEscapeSequence,
    UnterminatedCharLiteral,
    UnterminatedStrLiteral,
};

/// A lexer that processes source code into a sequence of tokens.
///
/// The lexer is implemented as a deterministic finite automaton (DFA) with
/// single-character lookahead. This design choice means:
/// 1. Each state transition depends only on the current state and next character
/// 2. The lexer can make decisions with just one character of lookahead (peek)
/// 3. Token boundaries are determined without backtracking
/// 4. Error reporting is precise and immediate
///
/// This approach provides a good balance of simplicity, performance, and error
/// handling capability while keeping memory usage constant.
pub const Lexer = struct {
    /// The source code to be lexed.
    source: []const u8,

    /// The name of the source file where the token is located.
    loc: TokenLoc,

    pub fn init(source: []const u8, filename: []const u8) Lexer {
        // Filename must not be empty
        assert(filename.len > 0);

        return .{
            .source = source,
            .loc = .{
                .filename = filename,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 0 },
            },
        };
    }

    /// Retrieves and returns the next token from the source code, advancing the lexer position.
    pub fn nextToken(self: *Lexer) LexerError!Token {
        // Assert our position tracking is valid
        assert(self.loc.span.end >= self.loc.span.start);
        assert(self.loc.span.start <= self.source.len);

        self.skipWhitespace();

        // Verify skipWhitespace properly synchronized positions
        assert(self.loc.span.start == self.loc.span.end);

        const span_start = self.loc.span.start;
        const start_line = self.loc.src.line;
        const start_col = self.loc.src.col;

        const c = self.peek() orelse {
            return Token.init(.{ .special = .Eof }, "", .{
                .filename = self.loc.filename,
                .src = .{
                    .line = start_line,
                    .col = start_col,
                },
                .span = .{
                    .start = span_start,
                    .end = self.loc.span.start,
                },
            });
        };

        switch (c) {
            '?' => {
                self.advance();

                if (self.peek() == null) {
                    return Token.init(.{ .symbol = .TypeHole }, "?", .{
                        .filename = self.loc.filename,
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                    });
                } else {
                    return error.InvalidIdentifier;
                }
            },
            '#' => {
                const position_start = span_start;
                self.advance();

                var is_doc = false;
                if (self.peek()) |next| {
                    if (next == '#') {
                        self.advance();

                        is_doc = true;
                    }
                }

                while (self.peek()) |next| {
                    if (next == '\n') break;

                    const utf8_len = std.unicode.utf8ByteSequenceLength(next) catch {
                        // Invalid UTF-8 sequence, treat as single byte
                        self.advance();

                        continue;
                    };

                    var i: usize = 0;
                    while (i < utf8_len) : (i += 1) {
                        self.advance();
                    }
                }

                const kind = if (is_doc) TokenKind{ .comment = .Doc } else TokenKind{ .comment = .Regular };
                const lexeme = self.source[position_start..self.loc.span.end];

                return Token.init(kind, lexeme, .{
                    .filename = self.loc.filename,
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                });
            },
            '"' => {
                self.advance();

                assert(self.source[self.loc.span.end - 1] == '"');

                // Check for multiline string
                if (self.peek()) |next1| {
                    if (next1 == '"') {
                        self.advance();

                        if (self.peek()) |next2| {
                            if (next2 == '"') {
                                self.advance();

                                // We must have processed at least the opening triple quotes
                                assert(self.loc.span.end >= 3);
                                // Verify we entered this function after seeing exactly three quote characters
                                assert(std.mem.eql(u8, self.source[self.loc.span.end - 3 .. self.loc.span.end], "\"\"\""));

                                var found_end = false;

                                while (self.peek()) |next| {
                                    if (next == '"') {
                                        self.advance();

                                        if (self.peek()) |quote2| {
                                            if (quote2 == '"') {
                                                self.advance();

                                                // We must have processed at least the opening triple quotes
                                                assert(self.loc.span.end >= 3);

                                                if (self.peek()) |quote3| {
                                                    if (quote3 == '"') {
                                                        self.advance();

                                                        found_end = true;
                                                        break;
                                                    }
                                                }
                                            }
                                        }

                                        if (!found_end) {
                                            const utf8_len = std.unicode.utf8ByteSequenceLength(next) catch {
                                                // fail silently with invalid sequence
                                                continue;
                                            };

                                            var i: usize = 1;
                                            while (i < utf8_len) : (i += 1) {
                                                self.advance();
                                            }
                                        }
                                    } else {
                                        const utf8_len = std.unicode.utf8ByteSequenceLength(next) catch {
                                            // fail silently with invalid sequence
                                            self.advance();

                                            continue;
                                        };

                                        var i: usize = 0;
                                        while (i < utf8_len) : (i += 1) {
                                            self.advance();
                                        }
                                    }
                                }

                                if (found_end) {
                                    const lexeme = self.source[span_start..self.loc.span.end];

                                    assert(lexeme.len >= 6);
                                    assert(std.mem.startsWith(u8, lexeme, "\"\"\""));
                                    assert(std.mem.endsWith(u8, lexeme, "\"\"\""));

                                    if (lexeme.len - 6 == 0) {
                                        return error.EmptyMultilineStringLiteral;
                                    }

                                    return Token.init(.{ .literal = .MultilineString }, lexeme, .{
                                        .filename = self.loc.filename,
                                        .src = .{
                                            .line = start_line,
                                            .col = start_col,
                                        },
                                        .span = .{
                                            .start = span_start,
                                            .end = self.loc.span.end,
                                        },
                                    });
                                } else {
                                    return error.UnterminatedStrLiteral;
                                }
                            }
                        }

                        // Just two quotes - empty string
                        const lexeme = self.source[span_start..self.loc.span.end];

                        return Token.init(.{ .literal = .String }, lexeme, .{
                            .filename = self.loc.filename,
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                        });
                    }
                }

                var found_closing_quote = false;

                while (self.peek()) |next| {
                    if (next == '"') {
                        self.advance();

                        found_closing_quote = true;
                        break;
                    }

                    if (next == '\\') {
                        self.advance();

                        if (self.peek()) |escaped_char| {
                            switch (escaped_char) {
                                '\\', '"', 'n', 't', 'r' => self.advance(),
                                'u' => try self.handleUnicodeEscape(),
                                else => return error.UnrecognizedStrEscapeSequence,
                            }
                        } else {
                            return error.UnterminatedStrLiteral;
                        }
                    } else {
                        const utf8_len = std.unicode.utf8ByteSequenceLength(next) catch {
                            // fail silently with invalid sequence
                            self.advance();

                            continue;
                        };

                        var i: usize = 0;
                        while (i < utf8_len) : (i += 1) {
                            self.advance();
                        }
                    }
                }

                if (found_closing_quote) {
                    const lexeme = self.source[span_start..self.loc.span.end];

                    return Token.init(.{ .literal = .String }, lexeme, .{
                        .filename = self.loc.filename,
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                    });
                } else {
                    return error.UnterminatedStrLiteral;
                }
            },
            '\'' => {
                const position_start = span_start;
                self.advance();

                if (self.peek() == '\'') {
                    return error.EmptyCharLiteral;
                }

                var char_count: usize = 0;
                var found_closing_quote = false;

                while (self.peek()) |next| {
                    if (next == '\'') {
                        self.advance();

                        found_closing_quote = true;
                        break;
                    }

                    if (next == '\\') {
                        self.advance();

                        char_count += 1;

                        if (self.peek()) |escaped_char| {
                            switch (escaped_char) {
                                '\\', '\'', 'n', 't', 'r' => self.advance(),
                                'u' => try self.handleUnicodeEscape(),
                                else => return error.UnrecognizedCharEscapeSequence,
                            }
                        }
                    } else {
                        char_count += 1;

                        const utf8_len = std.unicode.utf8ByteSequenceLength(next) catch {
                            // fail silently with invalid sequence
                            self.advance();
                            continue;
                        };

                        var i: usize = 0;
                        while (i < utf8_len) : (i += 1) {
                            self.advance();
                        }
                    }
                }

                if (found_closing_quote) {
                    if (char_count == 1) {
                        const lexeme = self.source[position_start..self.loc.span.end];

                        return Token.init(.{ .literal = .Char }, lexeme, .{
                            .filename = self.loc.filename,
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                            .span = .{
                                .start = position_start,
                                .end = self.loc.span.end,
                            },
                        });
                    } else {
                        return error.MultipleCharsInLiteral;
                    }
                } else {
                    return error.UnterminatedCharLiteral;
                }
            },
            '+' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '.') {
                        self.advance();

                        return Token.init(.{ .operator = .FloatAdd }, "+.", .{
                            .filename = self.loc.filename,
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                        });
                    }
                }

                if (self.peek()) |next| {
                    if (next == '+') {
                        self.advance();

                        return Token.init(.{ .operator = .ListConcat }, "++", .{
                            .filename = self.loc.filename,
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                        });
                    }
                }

                return Token.init(.{ .operator = .IntAdd }, "+", .{
                    .filename = self.loc.filename,
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                });
            },
            '-' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '>') {
                        self.advance();

                        return Token.init(.{ .operator = .ThinArrow }, "->", .{
                            .filename = self.loc.filename,
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                        });
                    }

                    if (next == '.') {
                        self.advance();

                        return Token.init(.{ .operator = .FloatSub }, "-.", .{
                            .filename = self.loc.filename,
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                        });
                    }
                }

                return Token.init(.{ .operator = .IntSub }, "-", .{
                    .filename = self.loc.filename,
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                });
            },
            '*' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '*') {
                        self.advance();

                        return Token.init(.{ .operator = .Exponent }, "**", .{
                            .filename = self.loc.filename,
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                        });
                    }

                    if (next == '.') {
                        self.advance();

                        return Token.init(.{ .operator = .FloatMul }, "*.", .{
                            .filename = self.loc.filename,
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                        });
                    }
                }

                return Token.init(.{ .operator = .IntMul }, "*", .{
                    .filename = self.loc.filename,
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                });
            },
            '/' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '=') {
                        self.advance();

                        return Token.init(.{ .operator = .NotEqual }, "/=", .{
                            .filename = self.loc.filename,
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                        });
                    }

                    if (next == '.') {
                        self.advance();

                        return Token.init(.{ .operator = .FloatDiv }, "/.", .{
                            .filename = self.loc.filename,
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                        });
                    }
                }

                return Token.init(.{ .operator = .IntDiv }, "/", .{
                    .filename = self.loc.filename,
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                });
            },
            '<' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '=') {
                        self.advance();

                        return Token.init(.{ .operator = .LessThanEqual }, "<=", .{
                            .filename = self.loc.filename,
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                        });
                    }

                    if (next == '>') {
                        self.advance();

                        return Token.init(.{ .operator = .StringConcat }, "<>", .{
                            .filename = self.loc.filename,
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                        });
                    }

                    if (next == '|') {
                        self.advance();

                        return Token.init(.{ .operator = .PipeLeft }, "<|", TokenLoc{
                            .filename = self.loc.filename,
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                        });
                    }

                    if (next == '<') {
                        self.advance();

                        return Token.init(.{ .operator = .ComposeLeft }, "<<", TokenLoc{
                            .filename = self.loc.filename,
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                        });
                    }
                }

                return Token.init(.{ .operator = .LessThan }, "<", .{
                    .filename = self.loc.filename,
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                });
            },
            '>' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '=') {
                        self.advance();

                        return Token.init(.{ .operator = .GreaterThanEqual }, ">=", .{
                            .filename = self.loc.filename,
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                        });
                    }

                    if (next == '>') {
                        self.advance();

                        return Token.init(.{ .operator = .ComposeRight }, ">>", TokenLoc{
                            .filename = self.loc.filename,
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                        });
                    }
                }

                return Token.init(.{ .operator = .GreaterThan }, ">", .{
                    .filename = self.loc.filename,
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                });
            },
            '&' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '&') {
                        self.advance();

                        return Token.init(.{ .operator = .LogicalAnd }, "&&", .{
                            .filename = self.loc.filename,
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                        });
                    }
                }

                return Token.init(.{ .special = .Unrecognized }, "&", .{
                    .filename = self.loc.filename,
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                });
            },
            '|' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '|') {
                        self.advance();

                        return Token.init(.{ .operator = .LogicalOr }, "||", .{
                            .filename = self.loc.filename,
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                        });
                    }

                    if (next == '>') {
                        self.advance();

                        return Token.init(.{ .operator = .PipeRight }, "|>", .{
                            .filename = self.loc.filename,
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                        });
                    }
                }

                return Token.init(.{ .operator = .Pipe }, "|", .{
                    .filename = self.loc.filename,
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                });
            },
            ':' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == ':') {
                        self.advance();

                        return Token.init(.{ .operator = .ListCons }, "::", .{
                            .filename = self.loc.filename,
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                        });
                    }
                }

                return Token.init(.{ .delimiter = .Colon }, ":", .{
                    .filename = self.loc.filename,
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                });
            },
            ',' => {
                self.advance();

                return Token.init(.{ .delimiter = .Comma }, ",", .{
                    .filename = self.loc.filename,
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                });
            },
            '.' => {
                self.advance();

                if (self.peek()) |next| {
                    if (ascii.isDigit(next)) {
                        return error.InvalidFloatLiteral;
                    }

                    if (next == '.') {
                        self.advance();

                        return Token.init(.{ .operator = .Expand }, "..", .{
                            .filename = self.loc.filename,
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                        });
                    }
                }

                return Token.init(.{ .delimiter = .Dot }, ".", .{
                    .filename = self.loc.filename,
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                });
            },
            '{' => {
                self.advance();

                return Token.init(.{ .delimiter = .LeftBrace }, "{", .{
                    .filename = self.loc.filename,
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                });
            },
            '}' => {
                self.advance();

                return Token.init(.{ .delimiter = .RightBrace }, "}", .{
                    .filename = self.loc.filename,
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                });
            },
            '=' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '>') {
                        self.advance();

                        return Token.init(.{ .operator = .FatArrow }, "=>", .{
                            .filename = self.loc.filename,
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                        });
                    }

                    if (next == '=') {
                        self.advance();

                        return Token.init(.{ .operator = .Equality }, "==", .{
                            .filename = self.loc.filename,
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                        });
                    }
                }

                return Token.init(.{ .operator = .Equal }, "=", .{
                    .filename = self.loc.filename,
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                });
            },
            '(' => {
                self.advance();

                return Token.init(.{ .delimiter = .LeftParen }, "(", .{
                    .filename = self.loc.filename,
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                });
            },
            ')' => {
                self.advance();

                return Token.init(.{ .delimiter = .RightParen }, ")", .{
                    .filename = self.loc.filename,
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                });
            },
            '[' => {
                self.advance();

                return Token.init(.{ .delimiter = .LeftBracket }, "[", .{
                    .filename = self.loc.filename,
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                });
            },
            ']' => {
                self.advance();

                return Token.init(.{ .delimiter = .RightBracket }, "]", .{
                    .filename = self.loc.filename,
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                });
            },
            '0' => {
                self.advance();

                if (self.peek()) |next| {
                    switch (next) {
                        'b' => {
                            self.advance();

                            return self.handleNumber(.Binary);
                        },
                        'o' => {
                            self.advance();

                            return self.handleNumber(.Octal);
                        },
                        'x' => {
                            self.advance();

                            return self.handleNumber(.Hex);
                        },
                        else => return self.handleNumber(.Decimal),
                    }
                }

                const lexeme = self.source[span_start..self.loc.span.end];

                return Token.init(.{ .literal = .Int }, lexeme, .{
                    .filename = self.loc.filename,
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                });
            },
            '1'...'9' => {
                return self.handleNumber(.Decimal);
            },
            'a'...'z', 'A'...'Z', '_' => {
                if (c == '_') {
                    self.advance();

                    if (self.peek() == null) {
                        const lexeme = self.source[span_start..self.loc.span.end];

                        return Token.init(.{ .symbol = .Underscore }, lexeme, .{
                            .filename = self.loc.filename,
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                        });
                    }

                    if (self.peek()) |next| {
                        if (ascii.isUpper(next)) {
                            return error.InvalidIdentifier;
                        }

                        if (ascii.isDigit(next)) {
                            return error.InvalidIntLiteral;
                        }

                        switch (next) {
                            'a'...'z', '_' => {
                                while (self.peek()) |next_char| {
                                    switch (next_char) {
                                        'a'...'z', 'A'...'Z', '_', '0'...'9' => self.advance(),
                                        '?' => {
                                            self.advance();

                                            if (self.peek()) |x| {
                                                if (!ascii.isWhitespace(x)) {
                                                    return error.InvalidIdentifier;
                                                }
                                            }

                                            break;
                                        },
                                        else => break,
                                    }
                                }

                                const lexeme = self.source[span_start..self.loc.span.end];

                                return Token.init(.{ .identifier = .Lower }, lexeme, .{
                                    .filename = self.loc.filename,
                                    .src = .{
                                        .line = start_line,
                                        .col = start_col,
                                    },
                                    .span = .{
                                        .start = span_start,
                                        .end = self.loc.span.end,
                                    },
                                });
                            },
                            else => {
                                return Token.init(.{ .symbol = .Underscore }, "_", .{
                                    .filename = self.loc.filename,
                                    .src = .{
                                        .line = start_line,
                                        .col = start_col,
                                    },
                                    .span = .{
                                        .start = span_start,
                                        .end = self.loc.span.end,
                                    },
                                });
                            },
                        }
                    }

                    return Token.init(.{ .symbol = .Underscore }, "_", .{
                        .filename = self.loc.filename,
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                    });
                }

                while (self.peek()) |next| {
                    switch (next) {
                        'a'...'z', 'A'...'Z', '_', '0'...'9' => self.advance(),
                        '?' => {
                            self.advance();

                            if (self.peek()) |x| {
                                if (!ascii.isWhitespace(x)) {
                                    return error.InvalidIdentifier;
                                }
                            }

                            break;
                        },
                        else => break,
                    }
                }

                if (self.checkExactMatch(span_start, "alias", .{ .keyword = .Alias })) |token| return token;
                if (self.checkExactMatch(span_start, "as", .{ .keyword = .As })) |token| return token;
                if (self.checkExactMatch(span_start, "else", .{ .keyword = .Else })) |token| return token;
                if (self.checkExactMatch(span_start, "exposing", .{ .keyword = .Exposing })) |token| return token;
                if (self.checkExactMatch(span_start, "fn", .{ .keyword = .Fn })) |token| return token;
                if (self.checkExactMatch(span_start, "foreign", .{ .keyword = .Foreign })) |token| return token;
                if (self.checkExactMatch(span_start, "hiding", .{ .keyword = .Hiding })) |token| return token;
                if (self.checkExactMatch(span_start, "if", .{ .keyword = .If })) |token| return token;
                if (self.checkExactMatch(span_start, "include", .{ .keyword = .Include })) |token| return token;
                if (self.checkExactMatch(span_start, "infixl", .{ .keyword = .InfixLeft })) |token| return token;
                if (self.checkExactMatch(span_start, "infixn", .{ .keyword = .InfixNon })) |token| return token;
                if (self.checkExactMatch(span_start, "infixr", .{ .keyword = .InfixRight })) |token| return token;
                if (self.checkExactMatch(span_start, "let", .{ .keyword = .Let })) |token| return token;
                if (self.checkExactMatch(span_start, "match", .{ .keyword = .Match })) |token| return token;
                if (self.checkExactMatch(span_start, "module", .{ .keyword = .Module })) |token| return token;
                if (self.checkExactMatch(span_start, "on", .{ .keyword = .On })) |token| return token;
                if (self.checkExactMatch(span_start, "open", .{ .keyword = .Open })) |token| return token;
                if (self.checkExactMatch(span_start, "then", .{ .keyword = .Then })) |token| return token;
                if (self.checkExactMatch(span_start, "type", .{ .keyword = .Type })) |token| return token;
                if (self.checkExactMatch(span_start, "using", .{ .keyword = .Using })) |token| return token;
                if (self.checkExactMatch(span_start, "when", .{ .keyword = .When })) |token| return token;

                const lexeme = self.source[span_start..self.loc.span.end];
                const first_char = lexeme[0];

                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                };

                switch (first_char) {
                    'A'...'Z' => return Token.init(.{ .identifier = .Upper }, lexeme, end_loc),
                    'a'...'z', '_' => return Token.init(.{ .identifier = .Lower }, lexeme, end_loc),
                    else => return Token.init(.{ .special = .Unrecognized }, lexeme, end_loc),
                }
            },
            else => {
                self.advance();

                const lexeme = self.source[span_start..self.loc.span.end];

                return Token.init(.{ .special = .Unrecognized }, lexeme, .{
                    .filename = self.loc.filename,
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                });
            },
        }
    }

    /// Peeks at the next character in the source code without advancing the position.
    fn peek(self: *Lexer) ?u8 {
        // Assert our internal state is valid
        assert(self.loc.span.end >= self.loc.span.start);
        assert(self.loc.span.start <= self.source.len);

        if (self.loc.span.end < self.source.len) {
            return self.source[self.loc.span.end];
        } else {
            return null;
        }
    }

    /// Advances the lexer by one position in the source code.
    /// Updates the current line and column numbers accordingly.
    fn advance(self: *Lexer) void {
        // Assert we haven't reached the end of input
        assert(self.loc.span.end < self.source.len);
        // Assert our position tracking is valid
        assert(self.loc.src.line > 0);
        assert(self.loc.src.col > 0);
        // Spans remain ordered
        assert(self.loc.span.start <= self.loc.span.end);

        if (self.source[self.loc.span.end] == '\n') {
            self.loc.src.line += 1;
            self.loc.src.col = 1;
        } else {
            self.loc.src.col += 1;
        }

        self.loc.span.end += 1;

        assert(self.loc.span.end <= self.source.len);
        assert(self.loc.span.end > self.loc.span.start);
    }

    /// Advances past whitespace and synchronizes span positions.
    fn skipWhitespace(self: *Lexer) void {
        const initial_pos = self.loc.span.end;

        while (self.peek()) |c| {
            if (ascii.isWhitespace(c)) {
                self.advance();
            } else {
                break;
            }
        }

        self.loc.span.start = self.loc.span.end;

        // Assert we've either moved forward or stayed in place
        assert(self.loc.span.end >= initial_pos);
        // Assert our start/end positions are synchronized after skipping
        assert(self.loc.span.start == self.loc.span.end);
    }

    /// Checks if there is an exact match for a keyword starting at a given position.
    ///
    /// - `start`: The starting position of the match in the source code.
    /// - `keyword`: The keyword to check for.
    /// - `kind`: The token kind that corresponds to the keyword.
    fn checkExactMatch(
        self: *Lexer,
        start: usize,
        keyword: []const u8,
        kind: TokenKind,
    ) ?Token {
        // Assert the start position is valid
        assert(start <= self.loc.span.end);
        assert(start < self.source.len);

        const len = keyword.len;
        const lexeme = self.source[start..self.loc.span.end];

        const is_exact_match = self.loc.span.end - start == len and
            std.mem.eql(u8, lexeme, keyword);

        if (is_exact_match) {
            // Assert our column calculation is valid
            assert(self.loc.src.col >= len);

            return Token.init(kind, lexeme, .{
                .filename = self.loc.filename,
                .src = .{
                    .line = self.loc.src.line,
                    .col = self.loc.src.col - len,
                },
                .span = .{
                    .start = start,
                    .end = self.loc.span.end,
                },
            });
        } else {
            return null;
        }
    }

    fn handleNumber(self: *Lexer, base: enum { Decimal, Hex, Octal, Binary }) LexerError!Token {
        const offset = if (base == .Decimal) @as(usize, 0) else 2;
        const position_start = if (base == .Decimal)
            self.loc.span.start
        else
            self.loc.span.end - offset;

        // Assert our starting position is valid
        assert(position_start <= self.loc.span.end);
        assert(position_start < self.source.len);

        const start_col = if (base == .Decimal)
            self.loc.src.col - (self.loc.span.end - self.loc.span.start)
        else
            self.loc.src.col - offset;

        // Assert column calculation is valid
        assert(start_col > 0);

        if (base != .Decimal) {
            if (self.peek()) |next| {
                if (next == '_') {
                    return error.InvalidIntLiteral;
                }
            }
        }

        var found_valid_digit = true;
        var last_was_underscore = false;
        var is_float = false;

        // Handle initial digit sequence
        while (self.peek()) |c| {
            if (base == .Decimal and c == '.') {
                is_float = true;

                self.advance();
                break;
            }

            const is_valid_digit = switch (base) {
                .Decimal => ascii.isDigit(c),
                .Hex => ascii.isHex(c),
                .Binary => isBinDigit(c),
                .Octal => isOctDigit(c),
            };

            if (is_valid_digit) {
                found_valid_digit = true;
                last_was_underscore = false;

                self.advance();
            } else if (c == '_') {
                if (!found_valid_digit or last_was_underscore) {
                    return error.InvalidIntLiteral;
                }

                last_was_underscore = true;

                self.advance();
            } else {
                break;
            }
        }

        if (last_was_underscore) return error.InvalidIntLiteral;

        if (is_float) {
            // Must have at least one digit after decimal
            var found_fraction_digit = false;
            last_was_underscore = false;

            while (self.peek()) |c| {
                if (ascii.isDigit(c)) {
                    found_fraction_digit = true;
                    last_was_underscore = false;

                    self.advance();
                } else if (c == '_') {
                    if (!found_fraction_digit or last_was_underscore) {
                        return error.InvalidFloatLiteral;
                    }

                    last_was_underscore = true;

                    self.advance();
                } else {
                    break;
                }
            }

            if (!found_fraction_digit or last_was_underscore) {
                return error.InvalidFloatLiteral;
            }

            if (self.peek()) |c| {
                if (c == 'e') {
                    self.advance();

                    if (self.peek()) |next| {
                        if (next == '+' or next == '-') {
                            self.advance();
                        }
                    }

                    // Must have at least one digit in exponent
                    var found_exponent_digit = false;
                    last_was_underscore = false;

                    while (self.peek()) |digit| {
                        if (ascii.isDigit(digit)) {
                            found_exponent_digit = true;
                            last_was_underscore = false;

                            self.advance();
                        } else if (digit == '_') {
                            if (!found_exponent_digit or last_was_underscore) {
                                return error.InvalidFloatLiteral;
                            }

                            last_was_underscore = true;

                            self.advance();
                        } else {
                            break;
                        }
                    }

                    if (!found_exponent_digit or last_was_underscore) {
                        return error.InvalidFloatLiteral;
                    }
                }
            }

            const lexeme = self.source[position_start..self.loc.span.end];

            return Token.init(.{ .literal = .Float }, lexeme, .{
                .filename = self.loc.filename,
                .src = .{
                    .line = self.loc.src.line,
                    .col = start_col,
                },
                .span = .{
                    .start = position_start,
                    .end = self.loc.span.end,
                },
            });
        }

        const lexeme = self.source[position_start..self.loc.span.end];

        return Token.init(.{ .literal = .Int }, lexeme, .{
            .filename = self.loc.filename,
            .src = .{
                .line = self.loc.src.line,
                .col = start_col,
            },
            .span = .{
                .start = position_start,
                .end = self.loc.span.end,
            },
        });
    }

    fn handleUnicodeEscape(self: *Lexer) LexerError!void {
        // Assert we have input to process
        assert(self.loc.span.end < self.source.len);
        // Spans must remain ordered
        assert(self.loc.span.end >= self.loc.span.start);
        assert(self.loc.src.line > 0);
        assert(self.loc.src.col > 0);

        self.advance();

        if (self.peek()) |next| {
            if (next != '{') {
                return error.InvalidUnicodeEscapeSequence;
            }

            self.advance();
        } else {
            return error.InvalidUnicodeEscapeSequence;
        }

        // Save the starting position of the hex digits
        const escape_start = self.loc.span.end;
        const escape_col = self.loc.src.col;

        var unicode_value: u21 = 0;
        var digit_count: usize = 0;
        while (digit_count < 6) : (digit_count += 1) {
            if (self.peek()) |hex| {
                if (ascii.isHex(hex)) {
                    const digit_value = hexDigitToValue(hex);
                    unicode_value = (unicode_value << 4) | digit_value;

                    self.advance();
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        if (digit_count > 0) {
            if (isValidUnicodeCodepoint(unicode_value)) {
                if (self.peek()) |next| {
                    if (next == '}') {
                        self.advance();

                        return;
                    }
                }

                self.loc.span.start = escape_start;
                self.loc.src.col = escape_col;

                return error.InvalidUnicodeEscapeSequence;
            } else {
                self.loc.span.start = escape_start - 2;
                self.loc.src.col = escape_col - 2;

                return error.CodePointOutOfRange;
            }
        } else {
            self.loc.span.start = escape_start;
            self.loc.src.col = escape_col;

            return error.InvalidUnicodeEscapeSequence;
        }
    }

    fn hexDigitToValue(digit: u8) u4 {
        assert(ascii.isHex(digit));

        return switch (digit) {
            '0'...'9' => @intCast(digit - '0'),
            'a'...'f' => @intCast(digit - 'a' + 10),
            'A'...'F' => @intCast(digit - 'A' + 10),
            else => unreachable,
        };
    }

    fn isBinDigit(c: u8) bool {
        return c == '0' or c == '1';
    }

    fn isOctDigit(c: u8) bool {
        return c >= '0' and c <= '7';
    }

    fn isValidUnicodeCodepoint(value: u21) bool {
        // Value must be <= 0x10FFFF and not in the surrogate range (0xD800..0xDFFF)
        if (value > 0x10FFFF) return false;
        if (value >= 0xD800 and value <= 0xDFFF) return false;

        return true;
    }
};

const testing = std.testing;

const TEST_FILE = "test.mn";

const TestCase = struct {
    source: []const u8,
    token: Token,
};

test "[keyword]" {
    // Setup
    const cases = [_]TestCase{
        .{
            .source = "alias",
            .token = Token.init(.{ .keyword = .Alias }, "alias", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 5 },
            }),
        },
        .{
            .source = "as",
            .token = Token.init(.{ .keyword = .As }, "as", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        .{
            .source = "else",
            .token = Token.init(.{ .keyword = .Else }, "else", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 4 },
            }),
        },
        .{
            .source = "exposing",
            .token = Token.init(.{ .keyword = .Exposing }, "exposing", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 8 },
            }),
        },
        .{
            .source = "fn",
            .token = Token.init(.{ .keyword = .Fn }, "fn", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        .{
            .source = "foreign",
            .token = Token.init(.{ .keyword = .Foreign }, "foreign", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 7 },
            }),
        },
        .{
            .source = "hiding",
            .token = Token.init(.{ .keyword = .Hiding }, "hiding", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 6 },
            }),
        },
        .{
            .source = "if",
            .token = Token.init(.{ .keyword = .If }, "if", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        .{
            .source = "include",
            .token = Token.init(.{ .keyword = .Include }, "include", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 7 },
            }),
        },
        .{
            .source = "infixl",
            .token = Token.init(.{ .keyword = .InfixLeft }, "infixl", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 6 },
            }),
        },
        .{
            .source = "infixn",
            .token = Token.init(.{ .keyword = .InfixNon }, "infixn", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 6 },
            }),
        },
        .{
            .source = "infixr",
            .token = Token.init(.{ .keyword = .InfixRight }, "infixr", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 6 },
            }),
        },
        .{
            .source = "let",
            .token = Token.init(.{ .keyword = .Let }, "let", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 3 },
            }),
        },
        .{
            .source = "match",
            .token = Token.init(.{ .keyword = .Match }, "match", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 5 },
            }),
        },
        .{
            .source = "module",
            .token = Token.init(.{ .keyword = .Module }, "module", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 6 },
            }),
        },
        .{
            .source = "on",
            .token = Token.init(.{ .keyword = .On }, "on", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        .{
            .source = "open",
            .token = Token.init(.{ .keyword = .Open }, "open", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 4 },
            }),
        },
        .{
            .source = "then",
            .token = Token.init(.{ .keyword = .Then }, "then", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 4 },
            }),
        },
        .{
            .source = "type",
            .token = Token.init(.{ .keyword = .Type }, "type", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 4 },
            }),
        },
        .{
            .source = "using",
            .token = Token.init(.{ .keyword = .Using }, "using", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 5 },
            }),
        },
        .{
            .source = "when",
            .token = Token.init(.{ .keyword = .When }, "when", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 4 },
            }),
        },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.token.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.token.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(case.token.loc.span.start, token.loc.span.start);
        try testing.expectEqual(case.token.loc.span.end, token.loc.span.end);
        try testing.expectEqual(case.token.loc.src.line, token.loc.src.line);
        try testing.expectEqual(case.token.loc.src.col, token.loc.src.col);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[delimiter]" {
    // Setup
    const cases = [_]TestCase{
        .{
            .source = ":",
            .token = Token.init(.{ .delimiter = .Colon }, ":", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 1 },
            }),
        },
        .{
            .source = ",",
            .token = Token.init(.{ .delimiter = .Comma }, ",", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 1 },
            }),
        },
        .{
            .source = ".",
            .token = Token.init(.{ .delimiter = .Dot }, ".", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 1 },
            }),
        },
        .{
            .source = "[",
            .token = Token.init(.{ .delimiter = .LeftBracket }, "[", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 1 },
            }),
        },
        .{
            .source = "{",
            .token = Token.init(.{ .delimiter = .LeftBrace }, "{", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 1 },
            }),
        },
        .{
            .source = "(",
            .token = Token.init(.{ .delimiter = .LeftParen }, "(", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 1 },
            }),
        },
        .{
            .source = "]",
            .token = Token.init(.{ .delimiter = .RightBracket }, "]", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 1 },
            }),
        },
        .{
            .source = "}",
            .token = Token.init(.{ .delimiter = .RightBrace }, "}", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 1 },
            }),
        },
        .{
            .source = ")",
            .token = Token.init(.{ .delimiter = .RightParen }, ")", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 1 },
            }),
        },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.token.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.token.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(case.token.loc.span.start, token.loc.span.start);
        try testing.expectEqual(case.token.loc.span.end, token.loc.span.end);
        try testing.expectEqual(case.token.loc.src.line, token.loc.src.line);
        try testing.expectEqual(case.token.loc.src.col, token.loc.src.col);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[symbol]" {
    // Setup
    const cases = [_]TestCase{
        .{
            .source = "?",
            .token = Token.init(.{ .symbol = .TypeHole }, "?", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 1 },
            }),
        },
        .{
            .source = "_",
            .token = Token.init(.{ .symbol = .Underscore }, "_", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 1 },
            }),
        },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.token.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.token.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(case.token.loc.span.start, token.loc.span.start);
        try testing.expectEqual(case.token.loc.span.end, token.loc.span.end);
        try testing.expectEqual(case.token.loc.src.line, token.loc.src.line);
        try testing.expectEqual(case.token.loc.src.col, token.loc.src.col);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[operator]" {
    // Setup
    const cases = [_]TestCase{
        // Arithmetic
        .{
            .source = "**",
            .token = Token.init(.{ .operator = .Exponent }, "**", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        .{
            .source = "+.",
            .token = Token.init(.{ .operator = .FloatAdd }, "+.", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        .{
            .source = "/.",
            .token = Token.init(.{ .operator = .FloatDiv }, "/.", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        .{
            .source = "*.",
            .token = Token.init(.{ .operator = .FloatMul }, "*.", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        .{
            .source = "-.",
            .token = Token.init(.{ .operator = .FloatSub }, "-.", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        .{
            .source = "+",
            .token = Token.init(.{ .operator = .IntAdd }, "+", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 1 },
            }),
        },
        .{
            .source = "/",
            .token = Token.init(.{ .operator = .IntDiv }, "/", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 1 },
            }),
        },
        .{
            .source = "*",
            .token = Token.init(.{ .operator = .IntMul }, "*", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 1 },
            }),
        },
        .{
            .source = "-",
            .token = Token.init(.{ .operator = .IntSub }, "-", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 1 },
            }),
        },
        // Comparision
        .{
            .source = "==",
            .token = Token.init(.{ .operator = .Equality }, "==", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        .{
            .source = ">",
            .token = Token.init(.{ .operator = .GreaterThan }, ">", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 1 },
            }),
        },
        .{
            .source = ">=",
            .token = Token.init(.{ .operator = .GreaterThanEqual }, ">=", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        .{
            .source = "<",
            .token = Token.init(.{ .operator = .LessThan }, "<", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 1 },
            }),
        },
        .{
            .source = "<=",
            .token = Token.init(.{ .operator = .LessThanEqual }, "<=", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        .{
            .source = "/=",
            .token = Token.init(.{ .operator = .NotEqual }, "/=", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        // Logical
        .{
            .source = "&&",
            .token = Token.init(.{ .operator = .LogicalAnd }, "&&", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        .{
            .source = "||",
            .token = Token.init(.{ .operator = .LogicalOr }, "||", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        // Functions
        .{
            .source = "->",
            .token = Token.init(.{ .operator = .ThinArrow }, "->", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        .{
            .source = "=>",
            .token = Token.init(.{ .operator = .FatArrow }, "=>", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        .{
            .source = "<<",
            .token = Token.init(.{ .operator = .ComposeLeft }, "<<", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        .{
            .source = ">>",
            .token = Token.init(.{ .operator = .ComposeRight }, ">>", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        .{
            .source = "<|",
            .token = Token.init(.{ .operator = .PipeLeft }, "<|", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        .{
            .source = "|>",
            .token = Token.init(.{ .operator = .PipeRight }, "|>", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        // Lists
        .{
            .source = "::",
            .token = Token.init(.{ .operator = .ListCons }, "::", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        .{
            .source = "++",
            .token = Token.init(.{ .operator = .ListConcat }, "++", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        // Strings
        .{
            .source = "<>",
            .token = Token.init(.{ .operator = .StringConcat }, "<>", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        // Assignment
        .{
            .source = "=",
            .token = Token.init(.{ .operator = .Equal }, "=", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 1 },
            }),
        },

        // Other
        .{
            .source = "..",
            .token = Token.init(.{ .operator = .Expand }, "..", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        .{
            .source = "|",
            .token = Token.init(.{ .operator = .Pipe }, "|", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 1 },
            }),
        },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.token.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.token.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(case.token.loc.span.start, token.loc.span.start);
        try testing.expectEqual(case.token.loc.span.end, token.loc.span.end);
        try testing.expectEqual(case.token.loc.src.line, token.loc.src.line);
        try testing.expectEqual(case.token.loc.src.col, token.loc.src.col);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[special]" {
    // Setup
    const cases = [_]TestCase{
        .{
            .source = "%",
            .token = Token.init(.{ .special = .Unrecognized }, "%", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 1 },
            }),
        },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.token.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.token.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(case.token.loc.span.start, token.loc.span.start);
        try testing.expectEqual(case.token.loc.span.end, token.loc.span.end);
        try testing.expectEqual(case.token.loc.src.line, token.loc.src.line);
        try testing.expectEqual(case.token.loc.src.col, token.loc.src.col);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[comment]" {
    // Setup
    const cases = [_]TestCase{
        .{
            .source = "# this is a comment",
            .token = Token.init(.{ .comment = .Regular }, "# this is a comment", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 19 },
            }),
        },
        .{
            .source = "## this is a doc comment",
            .token = Token.init(.{ .comment = .Doc }, "## this is a doc comment", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 24 },
            }),
        },
        .{
            .source = "# 你好，世界",
            .token = Token.init(.{ .comment = .Regular }, "# 你好，世界", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 17 },
            }),
        },
        .{
            .source = "## こんにちは",
            .token = Token.init(.{ .comment = .Doc }, "## こんにちは", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 18 },
            }),
        },
        .{
            .source = "# 🚀 👽 💣",
            .token = Token.init(.{ .comment = .Regular }, "# 🚀 👽 💣", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 16 },
            }),
        },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.token.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.token.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(case.token.loc.span.start, token.loc.span.start);
        try testing.expectEqual(case.token.loc.span.end, token.loc.span.end);
        try testing.expectEqual(case.token.loc.src.line, token.loc.src.line);
        try testing.expectEqual(case.token.loc.src.col, token.loc.src.col);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[char literal]" {
    // Setup
    const cases = [_]TestCase{
        .{
            .source = "'a'",
            .token = Token.init(.{ .literal = .Char }, "'a'", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 3 },
            }),
        },
        .{
            .source = "'1'",
            .token = Token.init(.{ .literal = .Char }, "'1'", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 3 },
            }),
        },
        .{
            .source = "'$'",
            .token = Token.init(.{ .literal = .Char }, "'$'", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 3 },
            }),
        },
        .{
            .source = "'\\n'",
            .token = Token.init(.{ .literal = .Char }, "'\\n'", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 4 },
            }),
        },
        .{
            .source = "'\\t'",
            .token = Token.init(.{ .literal = .Char }, "'\\t'", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 4 },
            }),
        },
        .{
            .source = "'\\r'",
            .token = Token.init(.{ .literal = .Char }, "'\\r'", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 4 },
            }),
        },
        .{
            .source = "'\\''",
            .token = Token.init(.{ .literal = .Char }, "'\\''", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 4 },
            }),
        },
        .{
            .source = "'\\\\'",
            .token = Token.init(.{ .literal = .Char }, "'\\\\'", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 4 },
            }),
        },
        .{
            .source = "'\\u{1}'",
            .token = Token.init(.{ .literal = .Char }, "'\\u{1}'", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 7 },
            }),
        },
        .{
            .source = "'\\u{10}'",
            .token = Token.init(.{ .literal = .Char }, "'\\u{10}'", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 8 },
            }),
        },
        .{
            .source = "'\\u{100}'",
            .token = Token.init(.{ .literal = .Char }, "'\\u{100}'", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 9 },
            }),
        },
        .{
            .source = "'\\u{1000}'",
            .token = Token.init(.{ .literal = .Char }, "'\\u{1000}'", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 10 },
            }),
        },
        .{
            .source = "'\\u{10000}'",
            .token = Token.init(.{ .literal = .Char }, "'\\u{10000}'", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 11 },
            }),
        },
        .{
            .source = "'\\u{100000}'",
            .token = Token.init(.{ .literal = .Char }, "'\\u{100000}'", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 12 },
            }),
        },
        .{
            .source = "'\\u{10FFFF}'",
            .token = Token.init(.{ .literal = .Char }, "'\\u{10FFFF}'", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 12 },
            }),
        },
        .{
            .source = "'\\u{0000}'",
            .token = Token.init(.{ .literal = .Char }, "'\\u{0000}'", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 10 },
            }),
        },
        .{
            .source = "'\\u{0020}'",
            .token = Token.init(.{ .literal = .Char }, "'\\u{0020}'", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 10 },
            }),
        },
        .{
            .source = "'\\u{007F}'",
            .token = Token.init(.{ .literal = .Char }, "'\\u{007F}'", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 10 },
            }),
        },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.token.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.token.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(case.token.loc.span.start, token.loc.span.start);
        try testing.expectEqual(case.token.loc.span.end, token.loc.span.end);
        try testing.expectEqual(case.token.loc.src.line, token.loc.src.line);
        try testing.expectEqual(case.token.loc.src.col, token.loc.src.col);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[char literal] error.EmptyCharLiteral" {
    // Setup
    const source = "''";

    var lexer = Lexer.init(source, TEST_FILE);

    // Action
    const result = lexer.nextToken();

    // Assertions
    // Verify error
    try testing.expectError(error.EmptyCharLiteral, result);
}

test "[char literal] error.CodePointOutOfRange" {
    // Setup
    const invalid_cases = [_][]const u8{
        "'\\u{110000}'",
        "'\\u{D800}'", // high surrogate
        "'\\u{DFFF}'", // low surrogate
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
        try testing.expectError(error.CodePointOutOfRange, result);
    }
}

test "[char literal] error.UnrecognizedCharEscapeSequence" {
    // Setup
    const invalid_cases = [_][]const u8{
        "'\\q'",
        "'\\k'",
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
        try testing.expectError(error.UnrecognizedCharEscapeSequence, result);
    }
}

test "[char literal] error.MultipleCharsInLiteral" {
    // Setup
    const invalid_cases = [_][]const u8{
        "'ab'",
        "'foo'",
        "'\\n\n'",
        "'a\\n'",
        "'\\na'",
        "'\\u{123}k'",
        "'\\u{000000}a'",
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
        try testing.expectError(error.MultipleCharsInLiteral, result);
    }
}

test "[char literal] error.UnterminatedCharLiteral" {
    // Setup
    const invalid_cases = [_][]const u8{
        "'a",
        "'1",
        "'$",
        "'\\n",
        "'\\t",
        "'\\r",
        "'\\'",
        "'\\\\",
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
        try testing.expectError(error.UnterminatedCharLiteral, result);
    }
}

test "[char literal] error.InvalidUnicodeEscapeSequence" {
    // Setup
    const invalid_cases = [_][]const u8{
        "'\\u{}'",
        "'\\u{g}'",
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
        try testing.expectError(error.InvalidUnicodeEscapeSequence, result);
    }
}

test "[string literal]" {
    // Setup
    const cases = [_]TestCase{
        .{
            .source = "\"\"",
            .token = Token.init(.{ .literal = .String }, "\"\"", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        .{
            .source = "\"foo\"",
            .token = Token.init(.{ .literal = .String }, "\"foo\"", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 5 },
            }),
        },
        .{
            .source = "\"1\"",
            .token = Token.init(.{ .literal = .String }, "\"1\"", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 3 },
            }),
        },
        .{
            .source = "\"$\"",
            .token = Token.init(.{ .literal = .String }, "\"$\"", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 3 },
            }),
        },
        .{
            .source = "\"Backslash: \\\\\"",
            .token = Token.init(.{ .literal = .String }, "\"Backslash: \\\\\"", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 15 },
            }),
        },
        .{
            .source = "\"Double quote: \\\"Hello!\\\"\"",
            .token = Token.init(.{ .literal = .String }, "\"Double quote: \\\"Hello!\\\"\"", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 26 },
            }),
        },
        .{
            .source = "\"First line\\nSecond line\"",
            .token = Token.init(.{ .literal = .String }, "\"First line\\nSecond line\"", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 25 },
            }),
        },
        .{
            .source = "\"Column1\\tColumn2\\tColumn3\"",
            .token = Token.init(.{ .literal = .String }, "\"Column1\\tColumn2\\tColumn3\"", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 27 },
            }),
        },
        .{
            .source = "\"Carriage return\\rOverwritten text\"",
            .token = Token.init(.{ .literal = .String }, "\"Carriage return\\rOverwritten text\"", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 35 },
            }),
        },
        .{
            .source = "\"Unicode test: \\u{1}\"",
            .token = Token.init(.{ .literal = .String }, "\"Unicode test: \\u{1}\"", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 21 },
            }),
        },
        .{
            .source = "\"Unicode test: \\u{10}\"",
            .token = Token.init(.{ .literal = .String }, "\"Unicode test: \\u{10}\"", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 22 },
            }),
        },
        .{
            .source = "\"Unicode test: \\u{100}\"",
            .token = Token.init(.{ .literal = .String }, "\"Unicode test: \\u{100}\"", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 23 },
            }),
        },
        .{
            .source = "\"Unicode test: \\u{1000}\"",
            .token = Token.init(.{ .literal = .String }, "\"Unicode test: \\u{1000}\"", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 24 },
            }),
        },
        .{
            .source = "\"Unicode test: \\u{10000}\"",
            .token = Token.init(.{ .literal = .String }, "\"Unicode test: \\u{10000}\"", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 25 },
            }),
        },
        .{
            .source = "\"Unicode test: \\u{100000}\"",
            .token = Token.init(.{ .literal = .String }, "\"Unicode test: \\u{100000}\"", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 26 },
            }),
        },
        .{
            .source = "\"Unicode test: \\u{10FFFF}\"",
            .token = Token.init(.{ .literal = .String }, "\"Unicode test: \\u{10FFFF}\"", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 26 },
            }),
        },
        .{
            .source = "\"Unicode test: \\u{0000}\"",
            .token = Token.init(.{ .literal = .String }, "\"Unicode test: \\u{0000}\"", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 24 },
            }),
        },
        .{
            .source = "\"Unicode test: \\u{0020}\"",
            .token = Token.init(.{ .literal = .String }, "\"Unicode test: \\u{0020}\"", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 24 },
            }),
        },
        .{
            .source = "\"Unicode test: \\u{007F}\"",
            .token = Token.init(.{ .literal = .String }, "\"Unicode test: \\u{007F}\"", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 24 },
            }),
        },
        .{
            .source = "\"Unicode with extra: \\u{1234}Hello\"",
            .token = Token.init(.{ .literal = .String }, "\"Unicode with extra: \\u{1234}Hello\"", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 35 },
            }),
        },
        .{
            .source = "\"✅\"",
            .token = Token.init(.{ .literal = .String }, "\"✅\"", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 5 },
            }),
        },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.token.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.token.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(case.token.loc.span.start, token.loc.span.start);
        try testing.expectEqual(case.token.loc.span.end, token.loc.span.end);
        try testing.expectEqual(case.token.loc.src.line, token.loc.src.line);
        try testing.expectEqual(case.token.loc.src.col, token.loc.src.col);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[string literal] error.CodePointOutOfRange" {
    // Setup
    const invalid_cases = [_][]const u8{
        "\"\\u{110000}\"",
        "\"\\u{D800}\"", // high surrogate
        "\"\\u{DFFF}\"", // low surrogate
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
        try testing.expectError(error.CodePointOutOfRange, result);
    }
}

test "[string literal] error.UnrecognizedStrEscapeSequence" {
    const invalid_cases = [_][]const u8{
        "\"\\q\"",
        "\"\\k\"",
    };

    for (invalid_cases) |source| {
        // Setup
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
        try testing.expectError(error.UnrecognizedStrEscapeSequence, result);
    }
}

test "[string literal] error.InvalidUnicodeEscapeSequence" {
    // Setup
    const invalid_cases = [_][]const u8{
        "\"unicode missing digits: \\u{}\"", // Unicode escape needs at least 1 hex digit
        "\"invalid unicode: \\u{GHIJ}\"", // Unicode escape must only contain hex digits
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
        try testing.expectError(error.InvalidUnicodeEscapeSequence, result);
    }
}

test "[string literal] error.UnterminatedStrLiteral" {
    // Setup
    const invalid_cases = [_][]const u8{
        "\"no closing quote",
        "\"escape at end\\",
        "\"unicode escape at end\\u{123}",
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
        try testing.expectError(error.UnterminatedStrLiteral, result);
    }
}

test "[multiline string literal]" {
    // Setup
    const cases = [_]TestCase{
        .{
            .source =
            \\""" This is a
            \\multiline string with
            \\unicode: 你好, こんにちは
            \\"""
            ,
            .token = Token.init(.{ .literal = .MultilineString }, "\"\"\" This is a\nmultiline string with\nunicode: 你好, こんにちは\n\"\"\"", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 72 },
            }),
        },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.token.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.token.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(case.token.loc.span.start, token.loc.span.start);
        try testing.expectEqual(case.token.loc.span.end, token.loc.span.end);
        try testing.expectEqual(case.token.loc.src.line, token.loc.src.line);
        try testing.expectEqual(case.token.loc.src.col, token.loc.src.col);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[multiline string literal] error.UnterminatedStrLiteral" {
    // Setup
    const invalid_cases = [_][]const u8{
        \\""" This is an
        \\unterminated multiline string with
        \\unicode: 你好, こんにちは
        \\
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
        try testing.expectError(error.UnterminatedStrLiteral, result);
    }
}

test "[multiline string literal] error.EmptyMultilineStringLiteral" {
    // Setup
    var lexer = Lexer.init("\"\"\"\"\"\"", TEST_FILE);

    // Action
    const result = lexer.nextToken();

    // Assertions
    // Verify error
    try testing.expectError(error.EmptyMultilineStringLiteral, result);
}

test "[integer literal]" {
    // Setup
    const cases = [_]TestCase{
        .{
            .source = "42",
            .token = Token.init(.{ .literal = .Int }, "42", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 2 },
            }),
        },
        .{
            .source = "42_000_000",
            .token = Token.init(.{ .literal = .Int }, "42_000_000", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 10 },
            }),
        },
        .{
            .source = "0b101010",
            .token = Token.init(.{ .literal = .Int }, "0b101010", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 8 },
            }),
        },
        .{
            .source = "0b10_1010",
            .token = Token.init(.{ .literal = .Int }, "0b10_1010", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 9 },
            }),
        },
        .{
            .source = "0o52",
            .token = Token.init(.{ .literal = .Int }, "0o52", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 4 },
            }),
        },
        .{
            .source = "0o52_52",
            .token = Token.init(.{ .literal = .Int }, "0o52_52", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 7 },
            }),
        },
        .{
            .source = "0x2A",
            .token = Token.init(.{ .literal = .Int }, "0x2A", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 4 },
            }),
        },
        .{
            .source = "0x2A_2A",
            .token = Token.init(.{ .literal = .Int }, "0x2A_2A", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 7 },
            }),
        },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.token.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.token.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(case.token.loc.span.start, token.loc.span.start);
        try testing.expectEqual(case.token.loc.span.end, token.loc.span.end);
        try testing.expectEqual(case.token.loc.src.line, token.loc.src.line);
        try testing.expectEqual(case.token.loc.src.col, token.loc.src.col);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[integer literal] error.InvalidIntLiteral" {
    // Setup
    const invalid_cases = [_][]const u8{
        "10__000", // consecutive underscores
        "_1000", // leading underscore
        "1000_", // trailing underscore
        "0x_1F", // underscore after prefix
        "10__000.0", // fail before we even know it's a float
        "_1000.0", // fail before we even know it's a float
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
        try testing.expectError(error.InvalidIntLiteral, result);
    }
}

test "[float literal]" {
    // Setup
    const cases = [_]TestCase{
        .{
            .source = "42.0",
            .token = Token.init(.{ .literal = .Float }, "42.0", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 4 },
            }),
        },
        .{
            .source = "42_000_000.0",
            .token = Token.init(.{ .literal = .Float }, "42_000_000.0", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 12 },
            }),
        },
        .{
            .source = "0.5",
            .token = Token.init(.{ .literal = .Float }, "0.5", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 3 },
            }),
        },
        .{
            .source = "1.23e3",
            .token = Token.init(.{ .literal = .Float }, "1.23e3", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 6 },
            }),
        },
        .{
            .source = "4.56e-2",
            .token = Token.init(.{ .literal = .Float }, "4.56e-2", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 7 },
            }),
        },
        .{
            .source = "3.141_592",
            .token = Token.init(.{ .literal = .Float }, "3.141_592", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 9 },
            }),
        },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try std.testing.expectEqual(case.token.kind, token.kind);

        // Verify the token lexeme matches
        try std.testing.expectEqualStrings(case.token.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(case.token.loc.span.start, token.loc.span.start);
        try testing.expectEqual(case.token.loc.span.end, token.loc.span.end);
        try testing.expectEqual(case.token.loc.src.line, token.loc.src.line);
        try testing.expectEqual(case.token.loc.src.col, token.loc.src.col);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[float literal] error.InvalidFloat" {
    // Setup
    const invalid_cases = [_][]const u8{
        "1000._", // trailing underscore
        ".5e3", // must begin with a digit
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
        try std.testing.expectError(error.InvalidFloatLiteral, result);
    }
}

test "[identifier]" {
    // Setup
    const cases = [_]TestCase{
        .{
            .source = "Int",
            .token = Token.init(.{ .identifier = .Upper }, "Int", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 3 },
            }),
        },
        .{
            .source = "Float",
            .token = Token.init(.{ .identifier = .Upper }, "Float", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 5 },
            }),
        },
        .{
            .source = "Bool",
            .token = Token.init(.{ .identifier = .Upper }, "Bool", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 4 },
            }),
        },
        .{
            .source = "True",
            .token = Token.init(.{ .identifier = .Upper }, "True", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 4 },
            }),
        },
        .{
            .source = "False",
            .token = Token.init(.{ .identifier = .Upper }, "False", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 5 },
            }),
        },
        .{
            .source = "Unit",
            .token = Token.init(.{ .identifier = .Upper }, "Unit", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 4 },
            }),
        },
        .{
            .source = "foo",
            .token = Token.init(.{ .identifier = .Lower }, "foo", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 3 },
            }),
        },
        .{
            .source = "foo_bar",
            .token = Token.init(.{ .identifier = .Lower }, "foo_bar", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 7 },
            }),
        },
        .{
            .source = "_foo",
            .token = Token.init(.{ .identifier = .Lower }, "_foo", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 4 },
            }),
        },
        .{
            .source = "A",
            .token = Token.init(.{ .identifier = .Upper }, "A", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 1 },
            }),
        },
        .{
            .source = "a",
            .token = Token.init(.{ .identifier = .Lower }, "a", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 1 },
            }),
        },
        .{
            .source = "_",
            .token = Token.init(.{ .symbol = .Underscore }, "_", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 1 },
            }),
        },
        .{
            .source = "ABC123",
            .token = Token.init(.{ .identifier = .Upper }, "ABC123", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 6 },
            }),
        },
        .{
            .source = "abc123",
            .token = Token.init(.{ .identifier = .Lower }, "abc123", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 6 },
            }),
        },
        .{
            .source = "Foo_Bar",
            .token = Token.init(.{ .identifier = .Upper }, "Foo_Bar", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 7 },
            }),
        },
        .{
            .source = "_foo_BAR_123",
            .token = Token.init(.{ .identifier = .Lower }, "_foo_BAR_123", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 12 },
            }),
        },
        .{
            .source = "__foo",
            .token = Token.init(.{ .identifier = .Lower }, "__foo", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 5 },
            }),
        },
        .{
            .source = "foo?",
            .token = Token.init(.{ .identifier = .Lower }, "foo?", .{
                .filename = TEST_FILE,
                .src = .{ .line = 1, .col = 1 },
                .span = .{ .start = 0, .end = 4 },
            }),
        },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.token.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.token.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(case.token.loc.span.start, token.loc.span.start);
        try testing.expectEqual(case.token.loc.span.end, token.loc.span.end);
        try testing.expectEqual(case.token.loc.src.line, token.loc.src.line);
        try testing.expectEqual(case.token.loc.src.col, token.loc.src.col);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[identifier] error.InvalidIdentifier" {
    // Setup
    const invalid_cases = [_][]const u8{
        "_Foo",
        "_Bar",
        "?foo",
        "fo?o",
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
        try testing.expectError(error.InvalidIdentifier, result);
    }
}

test "[type variant]" {
    // Setup
    const source = "type FooBar = | Foo | Bar";

    const expected_tokens = [_]Token{
        Token.init(.{ .keyword = .Type }, "type", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 1 },
            .span = .{ .start = 0, .end = 4 },
        }),
        Token.init(.{ .identifier = .Upper }, "FooBar", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 6 },
            .span = .{ .start = 5, .end = 11 },
        }),
        Token.init(.{ .operator = .Equal }, "=", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 13 },
            .span = .{ .start = 12, .end = 13 },
        }),
        Token.init(.{ .operator = .Pipe }, "|", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 15 },
            .span = .{ .start = 14, .end = 15 },
        }),
        Token.init(.{ .identifier = .Upper }, "Foo", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 17 },
            .span = .{ .start = 16, .end = 19 },
        }),
        Token.init(.{ .operator = .Pipe }, "|", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 21 },
            .span = .{ .start = 20, .end = 21 },
        }),
        Token.init(.{ .identifier = .Upper }, "Bar", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 23 },
            .span = .{ .start = 22, .end = 25 },
        }),
        Token.init(.{ .special = .Eof }, "", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 26 },
            .span = .{ .start = 25, .end = 25 },
        }),
    };

    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(expected.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(expected.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(expected.loc.span.start, token.loc.span.start);
        try testing.expectEqual(expected.loc.span.end, token.loc.span.end);
        try testing.expectEqual(expected.loc.src.line, token.loc.src.line);
        try testing.expectEqual(expected.loc.src.col, token.loc.src.col);
    }
}

test "[type alias]" {
    // Setup
    const source = "type alias Seconds = Int";

    const expected_tokens = [_]Token{
        Token.init(.{ .keyword = .Type }, "type", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 1 },
            .span = .{ .start = 0, .end = 4 },
        }),
        Token.init(.{ .keyword = .Alias }, "alias", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 6 },
            .span = .{ .start = 5, .end = 10 },
        }),
        Token.init(.{ .identifier = .Upper }, "Seconds", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 12 },
            .span = .{ .start = 11, .end = 18 },
        }),
        Token.init(.{ .operator = .Equal }, "=", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 20 },
            .span = .{ .start = 19, .end = 20 },
        }),
        Token.init(.{ .identifier = .Upper }, "Int", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 22 },
            .span = .{ .start = 21, .end = 24 },
        }),
        Token.init(.{ .special = .Eof }, "", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 25 },
            .span = .{ .start = 24, .end = 24 },
        }),
    };

    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(expected.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(expected.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(expected.loc.span.start, token.loc.span.start);
        try testing.expectEqual(expected.loc.span.end, token.loc.span.end);
        try testing.expectEqual(expected.loc.src.line, token.loc.src.line);
        try testing.expectEqual(expected.loc.src.col, token.loc.src.col);
    }
}

test "[record type]" {
    // Setup
    const source = "type FooBar = { foo : Int, bar : String }";

    const expected_tokens = [_]Token{
        Token.init(.{ .keyword = .Type }, "type", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 1 },
            .span = .{ .start = 0, .end = 4 },
        }),
        Token.init(.{ .identifier = .Upper }, "FooBar", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 6 },
            .span = .{ .start = 5, .end = 11 },
        }),
        Token.init(.{ .operator = .Equal }, "=", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 13 },
            .span = .{ .start = 12, .end = 13 },
        }),
        Token.init(.{ .delimiter = .LeftBrace }, "{", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 15 },
            .span = .{ .start = 14, .end = 15 },
        }),
        Token.init(.{ .identifier = .Lower }, "foo", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 17 },
            .span = .{ .start = 16, .end = 19 },
        }),
        Token.init(.{ .delimiter = .Colon }, ":", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 21 },
            .span = .{ .start = 20, .end = 21 },
        }),
        Token.init(.{ .identifier = .Upper }, "Int", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 23 },
            .span = .{ .start = 22, .end = 25 },
        }),
        Token.init(.{ .delimiter = .Comma }, ",", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 26 },
            .span = .{ .start = 25, .end = 26 },
        }),
        Token.init(.{ .identifier = .Lower }, "bar", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 28 },
            .span = .{ .start = 27, .end = 30 },
        }),
        Token.init(.{ .delimiter = .Colon }, ":", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 32 },
            .span = .{ .start = 31, .end = 32 },
        }),
        Token.init(.{ .identifier = .Upper }, "String", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 34 },
            .span = .{ .start = 33, .end = 39 },
        }),
        Token.init(.{ .delimiter = .RightBrace }, "}", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 41 },
            .span = .{ .start = 40, .end = 41 },
        }),
        Token.init(.{ .special = .Eof }, "", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 42 },
            .span = .{ .start = 41, .end = 41 },
        }),
    };

    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(expected.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(expected.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(expected.loc.span.start, token.loc.span.start);
        try testing.expectEqual(expected.loc.span.end, token.loc.span.end);
        try testing.expectEqual(expected.loc.src.line, token.loc.src.line);
        try testing.expectEqual(expected.loc.src.col, token.loc.src.col);
    }
}

test "[module declaration]" {
    // Setup
    const source = "module Foo exposing (Foo(..), bar)";

    const expected_tokens = [_]Token{
        Token.init(.{ .keyword = .Module }, "module", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 1 },
            .span = .{ .start = 0, .end = 6 },
        }),
        Token.init(.{ .identifier = .Upper }, "Foo", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 8 },
            .span = .{ .start = 7, .end = 10 },
        }),
        Token.init(.{ .keyword = .Exposing }, "exposing", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 12 },
            .span = .{ .start = 11, .end = 19 },
        }),
        Token.init(.{ .delimiter = .LeftParen }, "(", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 21 },
            .span = .{ .start = 20, .end = 21 },
        }),
        Token.init(.{ .identifier = .Upper }, "Foo", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 22 },
            .span = .{ .start = 21, .end = 24 },
        }),
        Token.init(.{ .delimiter = .LeftParen }, "(", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 25 },
            .span = .{ .start = 24, .end = 25 },
        }),
        Token.init(.{ .operator = .Expand }, "..", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 26 },
            .span = .{ .start = 25, .end = 27 },
        }),
        Token.init(.{ .delimiter = .RightParen }, ")", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 28 },
            .span = .{ .start = 27, .end = 28 },
        }),
        Token.init(.{ .delimiter = .Comma }, ",", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 29 },
            .span = .{ .start = 28, .end = 29 },
        }),
        Token.init(.{ .identifier = .Lower }, "bar", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 31 },
            .span = .{ .start = 30, .end = 33 },
        }),
        Token.init(.{ .delimiter = .RightParen }, ")", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 34 },
            .span = .{ .start = 33, .end = 34 },
        }),
        Token.init(.{ .special = .Eof }, "", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 35 },
            .span = .{ .start = 34, .end = 34 },
        }),
    };

    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(expected.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(expected.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(expected.loc.span.start, token.loc.span.start);
        try testing.expectEqual(expected.loc.span.end, token.loc.span.end);
        try testing.expectEqual(expected.loc.src.line, token.loc.src.line);
        try testing.expectEqual(expected.loc.src.col, token.loc.src.col);
    }
}

test "[top level function declaration]" {
    // Setup
    const source = "let add (x : Int) (y : Int) : Int = x + y";

    const expected_tokens = [_]Token{
        Token.init(.{ .keyword = .Let }, "let", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 1 },
            .span = .{ .start = 0, .end = 3 },
        }),
        Token.init(.{ .identifier = .Lower }, "add", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 5 },
            .span = .{ .start = 4, .end = 7 },
        }),
        Token.init(.{ .delimiter = .LeftParen }, "(", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 9 },
            .span = .{ .start = 8, .end = 9 },
        }),
        Token.init(.{ .identifier = .Lower }, "x", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 10 },
            .span = .{ .start = 9, .end = 10 },
        }),
        Token.init(.{ .delimiter = .Colon }, ":", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 12 },
            .span = .{ .start = 11, .end = 12 },
        }),
        Token.init(.{ .identifier = .Upper }, "Int", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 14 },
            .span = .{ .start = 13, .end = 16 },
        }),
        Token.init(.{ .delimiter = .RightParen }, ")", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 17 },
            .span = .{ .start = 16, .end = 17 },
        }),
        Token.init(.{ .delimiter = .LeftParen }, "(", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 19 },
            .span = .{ .start = 18, .end = 19 },
        }),
        Token.init(.{ .identifier = .Lower }, "y", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 20 },
            .span = .{ .start = 19, .end = 20 },
        }),
        Token.init(.{ .delimiter = .Colon }, ":", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 22 },
            .span = .{ .start = 21, .end = 22 },
        }),
        Token.init(.{ .identifier = .Upper }, "Int", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 24 },
            .span = .{ .start = 23, .end = 26 },
        }),
        Token.init(.{ .delimiter = .RightParen }, ")", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 27 },
            .span = .{ .start = 26, .end = 27 },
        }),
        Token.init(.{ .delimiter = .Colon }, ":", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 29 },
            .span = .{ .start = 28, .end = 29 },
        }),
        Token.init(.{ .identifier = .Upper }, "Int", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 31 },
            .span = .{ .start = 30, .end = 33 },
        }),
        Token.init(.{ .operator = .Equal }, "=", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 35 },
            .span = .{ .start = 34, .end = 35 },
        }),
        Token.init(.{ .identifier = .Lower }, "x", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 37 },
            .span = .{ .start = 36, .end = 37 },
        }),
        Token.init(.{ .operator = .IntAdd }, "+", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 39 },
            .span = .{ .start = 38, .end = 39 },
        }),
        Token.init(.{ .identifier = .Lower }, "y", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 41 },
            .span = .{ .start = 40, .end = 41 },
        }),
        Token.init(.{ .special = .Eof }, "", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 42 },
            .span = .{ .start = 41, .end = 41 },
        }),
    };

    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(expected.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(expected.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(expected.loc.span.start, token.loc.span.start);
        try testing.expectEqual(expected.loc.span.end, token.loc.span.end);
        try testing.expectEqual(expected.loc.src.line, token.loc.src.line);
        try testing.expectEqual(expected.loc.src.col, token.loc.src.col);
    }
}

test "[pattern matching]" {
    // Setup
    const source = "match x on | Foo => 1 | Bar => 2 _ => 3";

    const expected_tokens = [_]Token{
        Token.init(.{ .keyword = .Match }, "match", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 1 },
            .span = .{ .start = 0, .end = 5 },
        }),
        Token.init(.{ .identifier = .Lower }, "x", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 7 },
            .span = .{ .start = 6, .end = 7 },
        }),
        Token.init(.{ .keyword = .On }, "on", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 9 },
            .span = .{ .start = 8, .end = 10 },
        }),
        Token.init(.{ .operator = .Pipe }, "|", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 12 },
            .span = .{ .start = 11, .end = 12 },
        }),
        Token.init(.{ .identifier = .Upper }, "Foo", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 14 },
            .span = .{ .start = 13, .end = 16 },
        }),
        Token.init(.{ .operator = .FatArrow }, "=>", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 18 },
            .span = .{ .start = 17, .end = 19 },
        }),
        Token.init(.{ .literal = .Int }, "1", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 21 },
            .span = .{ .start = 20, .end = 21 },
        }),
        Token.init(.{ .operator = .Pipe }, "|", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 23 },
            .span = .{ .start = 22, .end = 23 },
        }),
        Token.init(.{ .identifier = .Upper }, "Bar", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 25 },
            .span = .{ .start = 24, .end = 27 },
        }),
        Token.init(.{ .operator = .FatArrow }, "=>", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 29 },
            .span = .{ .start = 28, .end = 30 },
        }),
        Token.init(.{ .literal = .Int }, "2", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 32 },
            .span = .{ .start = 31, .end = 32 },
        }),
        Token.init(.{ .symbol = .Underscore }, "_", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 34 },
            .span = .{ .start = 33, .end = 34 },
        }),
        Token.init(.{ .operator = .FatArrow }, "=>", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 36 },
            .span = .{ .start = 35, .end = 37 },
        }),
        Token.init(.{ .literal = .Int }, "3", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 39 },
            .span = .{ .start = 38, .end = 39 },
        }),
        Token.init(.{ .special = .Eof }, "", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 40 },
            .span = .{ .start = 39, .end = 39 },
        }),
    };

    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(expected.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(expected.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(expected.loc.span.start, token.loc.span.start);
        try testing.expectEqual(expected.loc.span.end, token.loc.span.end);
        try testing.expectEqual(expected.loc.src.line, token.loc.src.line);
        try testing.expectEqual(expected.loc.src.col, token.loc.src.col);
    }
}

test "[let block]" {
    // Setup
    const source = "let x : Int = 42";

    const expected_tokens = [_]Token{
        Token.init(.{ .keyword = .Let }, "let", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 1 },
            .span = .{ .start = 0, .end = 3 },
        }),
        Token.init(.{ .identifier = .Lower }, "x", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 5 },
            .span = .{ .start = 4, .end = 5 },
        }),
        Token.init(.{ .delimiter = .Colon }, ":", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 7 },
            .span = .{ .start = 6, .end = 7 },
        }),
        Token.init(.{ .identifier = .Upper }, "Int", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 9 },
            .span = .{ .start = 8, .end = 11 },
        }),
        Token.init(.{ .operator = .Equal }, "=", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 13 },
            .span = .{ .start = 12, .end = 13 },
        }),
        Token.init(.{ .literal = .Int }, "42", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 15 },
            .span = .{ .start = 14, .end = 16 },
        }),
        Token.init(.{ .special = .Eof }, "", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 17 },
            .span = .{ .start = 16, .end = 16 },
        }),
    };

    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(expected.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(expected.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(expected.loc.span.start, token.loc.span.start);
        try testing.expectEqual(expected.loc.span.end, token.loc.span.end);
        try testing.expectEqual(expected.loc.src.line, token.loc.src.line);
        try testing.expectEqual(expected.loc.src.col, token.loc.src.col);
    }
}

test "[if_then_else statement]" {
    const source = "if x == 1 then True else False";

    const expected_tokens = [_]Token{
        Token.init(.{ .keyword = .If }, "if", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 1 },
            .span = .{ .start = 0, .end = 2 },
        }),
        Token.init(.{ .identifier = .Lower }, "x", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 4 },
            .span = .{ .start = 3, .end = 4 },
        }),
        Token.init(.{ .operator = .Equality }, "==", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 6 },
            .span = .{ .start = 5, .end = 7 },
        }),
        Token.init(.{ .literal = .Int }, "1", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 9 },
            .span = .{ .start = 8, .end = 9 },
        }),
        Token.init(.{ .keyword = .Then }, "then", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 11 },
            .span = .{ .start = 10, .end = 14 },
        }),
        Token.init(.{ .identifier = .Upper }, "True", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 16 },
            .span = .{ .start = 15, .end = 19 },
        }),
        Token.init(.{ .keyword = .Else }, "else", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 21 },
            .span = .{ .start = 20, .end = 24 },
        }),
        Token.init(.{ .identifier = .Upper }, "False", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 26 },
            .span = .{ .start = 25, .end = 30 },
        }),
        Token.init(.{ .special = .Eof }, "", .{
            .filename = TEST_FILE,
            .src = .{ .line = 1, .col = 31 },
            .span = .{ .start = 30, .end = 30 },
        }),
    };

    // Setup
    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(expected.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(expected.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(expected.loc.span.start, token.loc.span.start);
        try testing.expectEqual(expected.loc.span.end, token.loc.span.end);
        try testing.expectEqual(expected.loc.src.line, token.loc.src.line);
        try testing.expectEqual(expected.loc.src.col, token.loc.src.col);
    }
}
