const std = @import("std");

const ast = @import("ast.zig");
const lexer = @import("lexer.zig");

const assert = std.debug.assert;

pub const ParseError = error{
    EmptyFFIReference,
    EmptyImportList,
    InvalidCharLiteral,
    InvalidFloatLiteral,
    InvalidImportItem,
    InvalidIntLiteral,
    InvalidStrLiteral,
    UnexpectedToken,
} || std.mem.Allocator.Error;

pub const ParserError = ParseError || lexer.LexerError;

pub const Parser = struct {
    /// The lexer instance used to obtain tokens.
    lexer: *lexer.Lexer,

    /// The current token being processed by the parser.
    current_token: lexer.Token,

    /// Memory allocator used for parser operations.
    allocator: std.mem.Allocator,

    /// Represents the possible associativity rules for operators.
    /// Determines how operators of the same precedence are grouped.
    const Associativity = enum {
        /// Left-associative operators group from left to right
        ///
        /// Example: `a - b - c` is parsed as `(a - b) - c`
        Left,

        /// Non-associative operators cannot be chained
        ///
        /// Example: `a == b == c` is not allowed
        None,

        /// Right-associative operators group from right to left
        ///
        /// Example: `a :: b :: c` is parsed as `a :: (b :: c)`
        Right,
    };

    /// Operator metadata used by the precedence climbing parser to correctly structure
    /// expressions involving multiple operators with different precedence levels.
    const OperatorInfo = struct {
        /// Lower numbers indicate higher precedence.
        precedence: u8,
        associativity: Associativity,
    };

    /// Retrieves the operator information for a given token kind.
    /// Returns precedence and associativity rules used by the parser
    /// to correctly structure operator expressions.
    fn getOperatorInfo(kind: lexer.TokenKind) ?OperatorInfo {
        return switch (kind) {
            // Highest precedence (tightest binding)
            .operator => |op| switch (op) {
                .Exp => .{
                    .precedence = 12,
                    .associativity = .Left,
                },
                .FloatDiv,
                .FloatMul,
                .IntDiv,
                .IntMul,
                => .{
                    .precedence = 11,
                    .associativity = .Left,
                },
                .FloatAdd,
                .FloatSub,
                .IntAdd,
                .IntSub,
                => .{
                    .precedence = 10,
                    .associativity = .Left,
                },
                .Cons => .{
                    .precedence = 9,
                    .associativity = .Right,
                },
                .Expand => .{
                    .precedence = 8,
                    .associativity = .Right,
                },
                .StrConcat => .{
                    .precedence = 7,
                    .associativity = .Right,
                },
                .ListConcat => .{
                    .precedence = 6,
                    .associativity = .Right,
                },
                .Equality,
                .GreaterThan,
                .GreaterThanEqual,
                .LessThan,
                .LessThanEqual,
                .NotEqual,
                => .{
                    .precedence = 5,
                    .associativity = .None,
                },
                .LogicalAnd => .{
                    .precedence = 4,
                    .associativity = .Right,
                },
                .LogicalOr => .{
                    .precedence = 3,
                    .associativity = .Right,
                },
                .PipeRight => .{
                    .precedence = 2,
                    .associativity = .Left,
                },
                .Equal => .{
                    .precedence = 0,
                    .associativity = .None,
                },
            },
            .symbol => |sym| switch (sym) {
                .DoubleArrowRight => .{
                    .precedence = 1,
                    .associativity = .Right,
                },
                else => null,
            },
            else => null,
            // Lowest precedence (loosest binding)
        };
    }

    pub fn init(allocator: std.mem.Allocator, l: *lexer.Lexer) !*Parser {
        const first_token = try l.nextToken();

        const parser = try allocator.create(Parser);
        parser.* = .{
            .lexer = l,
            .current_token = first_token,
            .allocator = allocator,
        };

        return parser;
    }

    /// Frees resources associated with the parser instance.
    pub fn deinit(self: *Parser) void {
        self.allocator.destroy(self);
    }

    /// Advances to the next token in the input stream.
    fn advance(self: *Parser) ParserError!void {
        self.current_token = try self.lexer.nextToken();
    }

    /// Checks whether the current token matches a specific kind without consuming it.
    ///
    /// - `kind`: The kind of token to check.
    fn check(self: *Parser, kind: lexer.TokenKind) bool {
        const current = self.current_token.kind;

        return std.meta.activeTag(current) == std.meta.activeTag(kind) and
            std.meta.eql(current, kind);
    }

    /// Returns true and advances parser if current token matches kind.
    /// Returns false if no match, token remains unconsumed.
    ///
    /// - `kind`: The kind of token to check.
    fn match(self: *Parser, kind: lexer.TokenKind) ParserError!bool {
        if (self.check(kind)) {
            try self.advance();

            return true;
        }

        return false;
    }

    /// Ensures the current token matches a specific kind, consuming it if true.
    /// Errors if the token does not match.
    ///
    /// `kind`: The kind of token to check.
    fn expect(self: *Parser, kind: lexer.TokenKind) ParserError!lexer.Token {
        if (self.check(kind)) {
            const current_token = self.current_token;
            try self.advance();

            return current_token;
        }

        return error.UnexpectedToken;
    }

    //==========================================================================
    // Component Parsers
    //==========================================================================
    // These methods parse building blocks of larger language constructs.
    // They return specific node structs rather than allocated Nodes.
    //==========================================================================

    /// Parses a lower-case identifier into a structured node.
    fn parseLowerIdentifier(self: *Parser) ParserError!*ast.LowerIdentifierNode {
        const start_token = try self.expect(lexer.TokenKind{ .identifier = .Lower });

        const node = try self.allocator.create(ast.LowerIdentifierNode);
        errdefer node.release(self.allocator);

        node.* = .{
            .identifier = try self.allocator.dupe(u8, start_token.lexeme),
            .token = start_token,
        };

        return node;
    }

    /// Parses a upper-case identifier into a structured node.
    fn parseUpperIdentifier(self: *Parser) ParserError!*ast.UpperIdentifierNode {
        const start_token = try self.expect(lexer.TokenKind{ .identifier = .Upper });

        const node = try self.allocator.create(ast.UpperIdentifierNode);
        errdefer node.release(self.allocator);

        node.* = .{
            .identifier = try self.allocator.dupe(u8, start_token.lexeme),
            .token = start_token,
        };

        return node;
    }

    /// Parses an integer literal value into a structured node.
    /// The literal can be in decimal, hexadecimal (0x), binary (0b), or octal (0o) format.
    /// Underscores in numbers are allowed and ignored (e.g., 1_000_000).
    fn parseIntLiteral(self: *Parser) ParserError!ast.IntLiteralNode {
        const start_token = try self.expect(lexer.TokenKind{ .literal = .Int });

        const value = std.fmt.parseInt(i64, start_token.lexeme, 0) catch {
            return error.InvalidIntLiteral;
        };

        return .{
            .value = value,
            .token = start_token,
        };
    }

    /// Parses a floating-point literal value into a structured node.
    /// The literal can include decimal points and optional scientific notation (e.g., 1.23e-4).
    /// Underscores in numbers are allowed and ignored (e.g., 3.141_592).
    fn parseFloatLiteral(self: *Parser) ParserError!ast.FloatLiteralNode {
        const start_token = try self.expect(lexer.TokenKind{ .literal = .Float });

        const value = std.fmt.parseFloat(f64, start_token.lexeme) catch {
            return error.InvalidFloatLiteral;
        };

        return .{
            .value = value,
            .token = start_token,
        };
    }

    /// Parses a character literal into its Unicode codepoint value.
    /// Handles standard characters ('a'), escape sequences ('\n', '\t', etc.),
    /// and Unicode escape sequences ('\u{0061}').
    fn parseCharLiteral(self: *Parser) ParserError!ast.CharLiteralNode {
        const start_token = try self.expect(lexer.TokenKind{ .literal = .Char });

        // Lexeme includes the quotes, so it's at least 3 chars: 'x'
        std.debug.assert(start_token.lexeme.len >= 3);

        const unquoted = start_token.lexeme[1 .. start_token.lexeme.len - 1];

        var value: u21 = undefined;
        if (unquoted[0] == '\\') {
            value = try parseCharEscapeSequence(unquoted);
        } else {
            value = std.unicode.utf8Decode(unquoted) catch {
                return error.InvalidCharLiteral;
            };
        }

        return .{
            .value = value,
            .token = start_token,
        };
    }

    /// Parses a string literal into a structured node.
    fn parseStrLiteral(self: *Parser) ParserError!*ast.StrLiteralNode {
        const start_token = try self.expect(lexer.TokenKind{ .literal = .String });

        // Strip quotes
        const unquoted = start_token.lexeme[1 .. start_token.lexeme.len - 1];

        var result = std.ArrayList(u8).init(self.allocator);
        defer result.deinit();

        var i: usize = 0;
        while (i < unquoted.len) {
            if (unquoted[i] == '\\') {
                const sequence = try self.parseStrEscapeSequence(unquoted[i..]);
                try result.appendSlice(sequence.value);

                if (unquoted[i + 1] == 'u') {
                    self.allocator.free(sequence.value);
                }

                i += sequence.len;
            } else {
                try result.append(unquoted[i]);

                i += 1;
            }
        }

        const node = try self.allocator.create(ast.StrLiteralNode);
        errdefer node.release(self.allocator);

        node.* = .{
            .value = try self.allocator.dupe(u8, result.items),
            .token = start_token,
        };

        return node;
    }

    /// Parses a multiline string literal into a structured node.
    /// Handles triple-quoted strings `"""` that preserve line breaks and indentation,
    /// with support for escape sequences but not interpolation.
    ///
    /// Examples:
    /// - `"""Hello\nWorld"""`
    /// - `"""
    ///  This keeps
    ///  indentation
    ///"""`
    fn parseMultilineStringLiteral(self: *Parser) ParserError!*ast.MultilineStrLiteralNode {
        const start_token = try self.expect(lexer.TokenKind{ .literal = .MultilineString });

        const full_lexeme = start_token.lexeme;
        const content = full_lexeme[3 .. full_lexeme.len - 3];

        var raw_content = std.ArrayList(u8).init(self.allocator);
        defer raw_content.deinit();

        var i: usize = 0;
        while (i < content.len) {
            if (content[i] == '\\') {
                const sequence = try self.parseStrEscapeSequence(content[i..]);

                try raw_content.appendSlice(sequence.value);

                if (content[i + 1] == 'u') {
                    self.allocator.free(sequence.value);
                }

                i += sequence.len;
            } else {
                try raw_content.append(content[i]);

                i += 1;
            }
        }

        // Dedent lines
        var final_content = std.ArrayList(u8).init(self.allocator);
        errdefer final_content.deinit();

        var lines = std.mem.split(u8, raw_content.items, "\n");
        var first_line = true;
        var base_indent: ?usize = null;

        while (lines.next()) |line| {
            if (first_line) {
                if (line.len == 0) {
                    continue;
                }

                var indent: usize = 0;
                for (line) |c| {
                    if (!std.ascii.isWhitespace(c)) {
                        break;
                    }

                    indent += 1;
                }

                base_indent = indent;
                first_line = false;
            }

            if (line.len > 0) {
                var trimmed: []const u8 = line;
                if (base_indent) |indent| {
                    const indent_spaces = try self.allocator.alloc(u8, indent);
                    defer self.allocator.free(indent_spaces);
                    @memset(indent_spaces, ' ');

                    if (line.len > indent and std.mem.startsWith(u8, line, indent_spaces)) {
                        trimmed = line[indent..];
                    }
                }

                try final_content.appendSlice(trimmed);
            }

            if (lines.index != null) {
                try final_content.append('\n');
            }
        }

        const node = try self.allocator.create(ast.MultilineStrLiteralNode);
        errdefer node.release(self.allocator);

        node.* = .{
            .value = try self.allocator.dupe(u8, final_content.items),
            .token = start_token,
        };

        final_content.deinit();

        return node;
    }

    const EscapeSequence = struct {
        value: []const u8,
        len: usize,
    };

    fn parseStrEscapeSequence(self: *Parser, unquoted: []const u8) ParserError!EscapeSequence {
        return switch (unquoted[1]) {
            'n' => .{ .value = "\n", .len = 2 },
            'r' => .{ .value = "\r", .len = 2 },
            't' => .{ .value = "\t", .len = 2 },
            '\\' => .{ .value = "\\", .len = 2 },
            '\"' => .{ .value = "\"", .len = 2 },
            'u' => {
                // Handle Unicode escape \u{XXXXXX} where XXXXXX is 1-6 hex digits
                const brace_index = std.mem.indexOfScalar(u8, unquoted[3..], '}') orelse return error.InvalidStrLiteral;
                const hex = unquoted[3 .. 3 + brace_index];
                const code_point = std.fmt.parseInt(u21, hex, 16) catch {
                    return error.InvalidStrLiteral;
                };

                var utf8_buffer: [4]u8 = undefined;
                const utf8_len = std.unicode.utf8Encode(code_point, &utf8_buffer) catch {
                    return error.InvalidStrLiteral;
                };

                // Length is: \u{ + hex digits + }
                const sequence_len = 3 + hex.len + 1;

                return .{
                    .value = try self.allocator.dupe(u8, utf8_buffer[0..utf8_len]),
                    .len = sequence_len,
                };
            },
            else => return error.InvalidStrLiteral,
        };
    }

    fn parseCharEscapeSequence(unquoted: []const u8) ParserError!u21 {
        return switch (unquoted[1]) {
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '\\' => '\\',
            '\'' => '\'',
            'u' => {
                // Handle Unicode escape \u{XXXXXX} where XXXXXX is 1-6 hex digits
                // Skip the \u{ and }
                const hex = unquoted[3 .. unquoted.len - 1];

                return std.fmt.parseInt(u21, hex, 16) catch {
                    return error.InvalidCharLiteral;
                };
            },
            else => error.InvalidCharLiteral,
        };
    }

    //==========================================================================
    // Language Construct Parsers
    //==========================================================================
    // These methods parse complete language constructs.
    // They handle allocation and return *ast.Node.
    //==========================================================================

    /// Parses a complete program, which consists of a sequence of top-level declarations.
    pub fn parseProgram(self: *Parser) ParserError!*ast.Node {
        var statements = std.ArrayList(*ast.Node).init(self.allocator);
        errdefer {
            for (statements.items) |item| {
                item.release(self.allocator);
                self.allocator.destroy(item);
            }
            statements.deinit();
        }

        while (!self.check(lexer.TokenKind{ .special = .Eof })) {
            const stmt = try self.parseTopLevel();
            errdefer {
                stmt.release(self.allocator);
                self.allocator.destroy(stmt);
            }

            try statements.append(stmt);
        }

        const program_node = try self.allocator.create(ast.ProgramNode);
        errdefer program_node.release(self.allocator);

        program_node.* = .{
            .statements = statements,
            .token = self.current_token,
        };

        const node = try self.allocator.create(ast.Node);
        errdefer {
            node.release(self.allocator);
            self.allocator.destroy(node);
        }

        node.* = .{ .program = program_node };

        return node;
    }

    /// Parses a single top-level declaration based on the current token.
    fn parseTopLevel(self: *Parser) ParserError!*ast.Node {
        switch (self.current_token.kind) {
            .keyword => |kw| switch (kw) {
                .Module => return self.parseModuleDecl(),
                else => return error.UnexpectedToken,
            },
            .comment => |c| switch (c) {
                .Doc => return self.parseDocComment(),
                .Regular => return self.parseComment(),
            },
            else => return error.UnexpectedToken,
        }
    }

    /// Parses a module declaration with its name, exports, and contents.
    /// A module groups related declarations (functions, types, etc.) and controls
    /// their visibility through an export specification. This is the top-level
    /// construct for organizing code in a file.
    ///
    /// Examples:
    /// - `module A exposing (..) ... end`
    fn parseModuleDecl(self: *Parser) ParserError!*ast.Node {
        const start_token = try self.expect(lexer.TokenKind{ .keyword = .Module });

        const path_node = try self.parseModulePath();
        errdefer {
            path_node.release(self.allocator);
            self.allocator.destroy(path_node);
        }

        const module_path = path_node.module_path;
        self.allocator.destroy(path_node);

        const exports_node = try self.parseExportSpec();
        errdefer {
            exports_node.release(self.allocator);
            self.allocator.destroy(exports_node);
        }

        const exports = exports_node.export_spec;
        self.allocator.destroy(exports_node);

        var declarations = std.ArrayList(*ast.Node).init(self.allocator);
        errdefer {
            for (declarations.items) |decl| {
                decl.release(self.allocator);
                self.allocator.destroy(decl);
            }
            declarations.deinit();
        }

        while (!self.check(lexer.TokenKind{ .special = .Eof })) {
            const decl = try self.parseModuleStmt();
            errdefer {
                decl.release(self.allocator);
                self.allocator.destroy(decl);
            }

            try declarations.append(decl);
        }

        _ = try self.expect(lexer.TokenKind{ .special = .Eof });

        const module_node = try self.allocator.create(ast.ModuleDeclNode);
        errdefer module_node.release(self.allocator);

        module_node.* = .{
            .path = module_path,
            .exports = exports,
            .declarations = declarations,
            .token = start_token,
        };

        const node = try self.allocator.create(ast.Node);
        errdefer {
            node.release(self.allocator);
            self.allocator.destroy(node);
        }

        node.* = .{ .module_decl = module_node };

        return node;
    }

    /// Parses a single statement within a module declaration.
    /// Handles declarations such as functions, types, imports, and comments that
    /// appear inside a module's body.
    ///
    /// Examples:
    /// - `let double(x : Int) -> Int = x * 2`
    /// - `type Maybe(a) = None | Some(a)`
    /// - `open StdLib.List`
    fn parseModuleStmt(self: *Parser) ParserError!*ast.Node {
        switch (self.current_token.kind) {
            .keyword => |kw| switch (kw) {
                .Foreign => return self.parseForeignFunctionDecl(),
                .Include => return self.parseInclude(),
                .Let => return self.parseFunctionDecl(),
                .Open => return self.parseImportSpec(),
                .Type => return self.parseTypeDecl(),
                else => return error.UnexpectedToken,
            },
            .comment => |c| switch (c) {
                .Doc => return self.parseDocComment(),
                .Regular => return self.parseComment(),
            },
            else => return error.UnexpectedToken,
        }
    }

    /// Parses an export specification that defines which items a module exposes.
    /// Can specify all items with '..' or a list of specific items, optionally
    /// exposing constructors for types with '(..)'.
    ///
    /// Examples:
    /// - `exposing (double)`
    /// - `exposing (..)`
    /// - `exposing (Maybe(..), and_then)`
    fn parseExportSpec(self: *Parser) ParserError!*ast.Node {
        const start_token = try self.expect(lexer.TokenKind{ .keyword = .Exposing });

        _ = try self.expect(lexer.TokenKind{ .delimiter = .LeftParen });

        var exposing_all = false;

        var items: ?std.ArrayList(*ast.ExportItem) = null;
        errdefer {
            if (items) |*list| {
                for (list.items) |item| {
                    item.release(self.allocator);
                }
                list.deinit();
            }
        }

        if (try self.match(lexer.TokenKind{ .operator = .Expand })) {
            exposing_all = true;
        } else {
            items = std.ArrayList(*ast.ExportItem).init(self.allocator);

            while (!self.check(lexer.TokenKind{ .delimiter = .RightParen })) {
                var name: []const u8 = undefined;
                var token: lexer.Token = undefined;
                var expose_constructors = false;

                switch (self.current_token.kind) {
                    .identifier => |ident| switch (ident) {
                        .Lower => {
                            const ident_node = try self.parseLowerIdentifier();
                            defer ident_node.release(self.allocator);

                            name = try self.allocator.dupe(u8, ident_node.identifier);
                            token = ident_node.token;

                            // Lowercase identifiers cannot have (..)
                            if (self.check(lexer.TokenKind{ .delimiter = .LeftParen })) {
                                return error.UnexpectedToken;
                            }
                        },
                        .Upper => {
                            const ident_node = try self.parseUpperIdentifier();
                            defer ident_node.release(self.allocator);

                            name = try self.allocator.dupe(u8, ident_node.identifier);
                            token = ident_node.token;

                            if (try self.match(lexer.TokenKind{ .delimiter = .LeftParen })) {
                                _ = try self.expect(lexer.TokenKind{ .operator = .Expand });
                                _ = try self.expect(lexer.TokenKind{ .delimiter = .RightParen });

                                expose_constructors = true;
                            }
                        },
                    },
                    else => return error.UnexpectedToken,
                }

                const item = try self.allocator.create(ast.ExportItem);
                errdefer item.release(self.allocator);

                item.* = .{
                    .name = name,
                    .expose_constructors = expose_constructors,
                    .token = token,
                };

                try items.?.append(item);

                if (!self.check(lexer.TokenKind{ .delimiter = .RightParen })) {
                    _ = try self.expect(lexer.TokenKind{ .delimiter = .Comma });
                }
            }
        }

        _ = try self.expect(lexer.TokenKind{ .delimiter = .RightParen });

        const export_node = try self.allocator.create(ast.ExportSpecNode);
        errdefer export_node.release(self.allocator);

        export_node.* = .{
            .exposing_all = exposing_all,
            .items = items,
            .token = start_token,
        };

        const node = try self.allocator.create(ast.Node);
        errdefer {
            node.release(self.allocator);
            self.allocator.destroy(node);
        }

        node.* = .{ .export_spec = export_node };

        return node;
    }

    /// Parses an import specification that controls how a module is imported.
    ///
    /// Examples:
    /// - `open MyModule`
    /// - `open MyModule as M`
    /// - `open Std.List using (map, filter)`
    /// - `open Std.List using (map as list_map)`
    /// - `open Foo.Bar hiding (internal_func)`
    fn parseImportSpec(self: *Parser) ParserError!*ast.Node {
        const start_token = try self.expect(lexer.TokenKind{ .keyword = .Open });

        const path_node = try self.parseModulePath();
        errdefer self.allocator.destroy(path_node);

        const module_path = path_node.module_path;
        self.allocator.destroy(path_node);

        var kind: ast.ImportKind = .Simple;

        var alias: ?*ast.UpperIdentifierNode = null;
        errdefer {
            if (alias) |a| {
                a.release(self.allocator);
            }
        }

        var items: ?std.ArrayList(*ast.ImportItem) = null;
        errdefer {
            if (items) |*import_list| {
                for (import_list.items) |item| {
                    item.release(self.allocator);
                }
                import_list.deinit();
            }
        }

        switch (self.current_token.kind) {
            .keyword => |kw| switch (kw) {
                .As => {
                    try self.advance();

                    kind = .Alias;

                    alias = try self.parseUpperIdentifier();
                },
                .Using => {
                    try self.advance();

                    kind = .Using;

                    _ = try self.expect(lexer.TokenKind{ .delimiter = .LeftParen });

                    var list = std.ArrayList(*ast.ImportItem).init(self.allocator);
                    errdefer {
                        if (items) |*import_list| {
                            for (import_list.items) |item| {
                                item.release(self.allocator);
                            }
                            import_list.deinit();
                        }
                    }

                    while (!self.check(lexer.TokenKind{ .delimiter = .RightParen })) {
                        switch (self.current_token.kind) {
                            .identifier => |ident| switch (ident) {
                                .Lower => {
                                    const func = try self.parseLowerIdentifier();
                                    defer func.release(self.allocator);

                                    var func_alias: ?[]const u8 = null;
                                    errdefer {
                                        if (func_alias) |a| {
                                            self.allocator.free(a);
                                        }
                                    }

                                    if (try self.match(lexer.TokenKind{ .keyword = .As })) {
                                        const alias_ident = try self.parseLowerIdentifier();
                                        defer alias_ident.release(self.allocator);

                                        func_alias = try self.allocator.dupe(u8, alias_ident.identifier);
                                    }

                                    const import_item = try self.allocator.create(ast.ImportItem);
                                    errdefer import_item.release(self.allocator);

                                    import_item.* = .{
                                        .inner = .{
                                            .function = .{
                                                .name = try self.allocator.dupe(u8, func.identifier),
                                                .alias = func_alias,
                                            },
                                        },
                                    };

                                    try list.append(import_item);
                                },
                                .Upper => {
                                    const type_name = try self.parseUpperIdentifier();
                                    defer type_name.release(self.allocator);

                                    var expose_constructors = false;

                                    var type_alias: ?[]const u8 = null;
                                    errdefer {
                                        if (type_alias) |a| {
                                            self.allocator.free(a);
                                        }
                                    }

                                    if (try self.match(lexer.TokenKind{ .delimiter = .LeftParen })) {
                                        _ = try self.expect(lexer.TokenKind{ .operator = .Expand });
                                        _ = try self.expect(lexer.TokenKind{ .delimiter = .RightParen });

                                        expose_constructors = true;
                                    }

                                    if (try self.match(lexer.TokenKind{ .keyword = .As })) {
                                        const alias_ident = try self.parseUpperIdentifier();
                                        defer alias_ident.release(self.allocator);

                                        type_alias = try self.allocator.dupe(u8, alias_ident.identifier);
                                    }

                                    const import_item = try self.allocator.create(ast.ImportItem);
                                    errdefer import_item.release(self.allocator);

                                    import_item.* = .{
                                        .inner = .{
                                            .type = .{
                                                .name = try self.allocator.dupe(u8, type_name.identifier),
                                                .expose_constructors = expose_constructors,
                                                .alias = type_alias,
                                            },
                                        },
                                    };

                                    try list.append(import_item);
                                },
                            },
                            .delimiter => |delim| switch (delim) {
                                .LeftParen => {
                                    try self.advance();

                                    const op = self.current_token.lexeme;
                                    try self.advance();

                                    _ = try self.expect(lexer.TokenKind{ .delimiter = .RightParen });

                                    var op_alias: ?[]const u8 = null;
                                    errdefer {
                                        if (op_alias) |a| {
                                            self.allocator.free(a);
                                        }
                                    }

                                    if (try self.match(lexer.TokenKind{ .keyword = .As })) {
                                        const alias_ident = try self.parseLowerIdentifier();
                                        defer alias_ident.release(self.allocator);

                                        op_alias = try self.allocator.dupe(u8, alias_ident.identifier);
                                    }

                                    const import_item = try self.allocator.create(ast.ImportItem);
                                    errdefer import_item.release(self.allocator);

                                    import_item.* = .{
                                        .inner = .{
                                            .operator = .{
                                                .symbol = try self.allocator.dupe(u8, op),
                                                .alias = op_alias,
                                            },
                                        },
                                    };

                                    try list.append(import_item);
                                },
                                else => return error.InvalidImportItem,
                            },
                            else => return error.InvalidImportItem,
                        }

                        // If not at closing paren, expect a comma
                        if (!self.check(lexer.TokenKind{ .delimiter = .RightParen })) {
                            _ = try self.expect(lexer.TokenKind{ .delimiter = .Comma });
                        }
                    }

                    if (list.items.len == 0) {
                        return error.EmptyImportList;
                    }

                    _ = try self.expect(lexer.TokenKind{ .delimiter = .RightParen });

                    items = list;
                },
                .Hiding => {
                    try self.advance();

                    kind = .Hiding;

                    _ = try self.expect(lexer.TokenKind{ .delimiter = .LeftParen });

                    var list = std.ArrayList(*ast.ImportItem).init(self.allocator);
                    errdefer {
                        if (items) |*import_list| {
                            for (import_list.items) |item| {
                                item.release(self.allocator);
                            }
                            import_list.deinit();
                        }
                    }

                    while (!self.check(lexer.TokenKind{ .delimiter = .RightParen })) {
                        switch (self.current_token.kind) {
                            .identifier => |ident| switch (ident) {
                                .Lower => {
                                    const func = try self.parseLowerIdentifier();
                                    defer func.release(self.allocator);

                                    const import_item = try self.allocator.create(ast.ImportItem);
                                    errdefer import_item.release(self.allocator);

                                    import_item.* = .{
                                        .inner = .{
                                            .function = .{
                                                .name = try self.allocator.dupe(u8, func.identifier),
                                                .alias = null,
                                            },
                                        },
                                    };

                                    try list.append(import_item);
                                },
                                .Upper => {
                                    const type_name = try self.parseUpperIdentifier();
                                    defer type_name.release(self.allocator);

                                    var expose_constructors = false;

                                    if (try self.match(lexer.TokenKind{ .delimiter = .LeftParen })) {
                                        _ = try self.expect(lexer.TokenKind{ .operator = .Expand });
                                        _ = try self.expect(lexer.TokenKind{ .delimiter = .RightParen });

                                        expose_constructors = true;
                                    }

                                    const import_item = try self.allocator.create(ast.ImportItem);
                                    errdefer import_item.release(self.allocator);

                                    import_item.* = .{
                                        .inner = .{
                                            .type = .{
                                                .name = try self.allocator.dupe(u8, type_name.identifier),
                                                .expose_constructors = expose_constructors,
                                                .alias = null,
                                            },
                                        },
                                    };

                                    try list.append(import_item);
                                },
                            },
                            .delimiter => |delim| switch (delim) {
                                .LeftParen => {
                                    try self.advance();

                                    // Read the operator symbol
                                    const op = self.current_token.lexeme;
                                    try self.advance();

                                    _ = try self.expect(lexer.TokenKind{ .delimiter = .RightParen });

                                    const import_item = try self.allocator.create(ast.ImportItem);
                                    errdefer import_item.release(self.allocator);

                                    import_item.* = .{
                                        .inner = .{
                                            .operator = .{
                                                .symbol = try self.allocator.dupe(u8, op),
                                                .alias = null,
                                            },
                                        },
                                    };

                                    try list.append(import_item);
                                },
                                else => return error.InvalidImportItem,
                            },
                            else => return error.InvalidImportItem,
                        }

                        // If not at closing paren, expect a comma
                        if (!self.check(lexer.TokenKind{ .delimiter = .RightParen })) {
                            _ = try self.expect(lexer.TokenKind{ .delimiter = .Comma });
                        }
                    }

                    if (list.items.len == 0) {
                        return error.EmptyImportList;
                    }

                    _ = try self.expect(lexer.TokenKind{ .delimiter = .RightParen });

                    items = list;
                },
                else => {},
            },
            else => {},
        }

        const import_node = try self.allocator.create(ast.ImportSpecNode);
        errdefer import_node.release(self.allocator);

        import_node.* = .{
            .path = module_path,
            .kind = kind,
            .alias = alias,
            .items = items,
            .token = start_token,
        };

        const node = try self.allocator.create(ast.Node);
        errdefer {
            node.release(self.allocator);
            self.allocator.destroy(node);
        }

        node.* = .{ .import_spec = import_node };

        return node;
    }

    /// Parses a type declaration, which can be a type alias, record type, or variant type.
    /// Handles parsing of type parameters that may be used in the type definition.
    ///
    /// Examples:
    /// - `type alias Reader(r, a) = (r) -> a`
    /// - `type Dict(k, v) = { keys : List(k), values : List(v) }`
    /// - `type Maybe(a) = | None | Some(a)`
    fn parseTypeDecl(self: *Parser) ParserError!*ast.Node {
        const start_token = try self.expect(lexer.TokenKind{ .keyword = .Type });

        if (try self.match(lexer.TokenKind{ .keyword = .Alias })) {
            const type_ident = try self.parseUpperIdentifier();
            errdefer type_ident.release(self.allocator);

            var type_params = std.ArrayList([]const u8).init(self.allocator);
            errdefer {
                for (type_params.items) |param| {
                    self.allocator.free(param);
                }
                type_params.deinit();
            }

            if (try self.match(lexer.TokenKind{ .delimiter = .LeftParen })) {
                const param = try self.parseLowerIdentifier();
                errdefer param.release(self.allocator);

                try type_params.append(try self.allocator.dupe(u8, param.identifier));
                param.release(self.allocator);

                while (try self.match(lexer.TokenKind{ .delimiter = .Comma })) {
                    const next_param = try self.parseLowerIdentifier();
                    errdefer next_param.release(self.allocator);

                    try type_params.append(try self.allocator.dupe(u8, next_param.identifier));
                    next_param.release(self.allocator);
                }

                _ = try self.expect(lexer.TokenKind{ .delimiter = .RightParen });
            }

            _ = try self.expect(lexer.TokenKind{ .operator = .Equal });

            var parameter_types = std.ArrayList(*ast.Node).init(self.allocator);
            errdefer {
                for (parameter_types.items) |param_type| {
                    param_type.release(self.allocator);
                    self.allocator.destroy(param_type);
                }
                parameter_types.deinit();
            }

            if (try self.match(lexer.TokenKind{ .delimiter = .LeftParen })) {
                const first_type = try self.parseTypeExpr();
                errdefer {
                    first_type.release(self.allocator);
                    self.allocator.destroy(first_type);
                }

                try parameter_types.append(first_type);

                if (try self.match(lexer.TokenKind{ .delimiter = .Comma })) {
                    while (true) {
                        const next_type = try self.parseTypeExpr();
                        errdefer {
                            next_type.release(self.allocator);
                            self.allocator.destroy(next_type);
                        }

                        try parameter_types.append(next_type);

                        if (!try self.match(lexer.TokenKind{ .delimiter = .Comma })) {
                            break;
                        }
                    }
                }

                _ = try self.expect(lexer.TokenKind{ .delimiter = .RightParen });
            } else {
                const single_type = try self.parseTypeExpr();
                errdefer {
                    single_type.release(self.allocator);
                    self.allocator.destroy(single_type);
                }

                try parameter_types.append(single_type);
            }

            var value: *ast.Node = undefined;
            if (try self.match(lexer.TokenKind{ .symbol = .ArrowRight })) {
                const return_type = try self.parseTypeExpr();
                errdefer {
                    return_type.release(self.allocator);
                    self.allocator.destroy(return_type);
                }

                const function_sig_node = try self.allocator.create(ast.FunctionSignatureNode);
                errdefer function_sig_node.release(self.allocator);

                function_sig_node.* = .{
                    .parameter_types = parameter_types,
                    .return_type = return_type,
                    .token = start_token,
                };

                value = try self.allocator.create(ast.Node);
                errdefer {
                    value.release(self.allocator);
                    self.allocator.destroy(value);
                }

                value.* = .{ .function_signature = function_sig_node };
            } else {
                if (parameter_types.items.len == 1) {
                    value = parameter_types.items[0];
                    parameter_types.deinit();
                } else {
                    return error.UnexpectedToken;
                }
            }

            const alias_node = try self.allocator.create(ast.TypeAliasNode);
            errdefer alias_node.release(self.allocator);

            alias_node.* = .{
                .name = type_ident,
                .type_params = type_params,
                .value = value,
                .token = start_token,
            };

            const node = try self.allocator.create(ast.Node);
            errdefer {
                node.release(self.allocator);
                self.allocator.destroy(node);
            }

            node.* = .{ .type_alias = alias_node };

            return node;
        }

        const type_ident = try self.parseUpperIdentifier();
        errdefer type_ident.release(self.allocator);

        var type_params = std.ArrayList([]const u8).init(self.allocator);
        errdefer {
            for (type_params.items) |param| {
                self.allocator.free(param);
            }
            type_params.deinit();
        }

        if (try self.match(lexer.TokenKind{ .delimiter = .LeftParen })) {
            const param = try self.parseLowerIdentifier();
            errdefer param.release(self.allocator);

            try type_params.append(try self.allocator.dupe(u8, param.identifier));
            param.release(self.allocator);

            while (try self.match(lexer.TokenKind{ .delimiter = .Comma })) {
                const next_param = try self.parseLowerIdentifier();
                errdefer next_param.release(self.allocator);

                try type_params.append(try self.allocator.dupe(u8, next_param.identifier));
                next_param.release(self.allocator);
            }

            _ = try self.expect(lexer.TokenKind{ .delimiter = .RightParen });

            param.release(self.allocator);
        }

        _ = try self.expect(lexer.TokenKind{ .operator = .Equal });

        if (try self.match(lexer.TokenKind{ .delimiter = .LeftBrace })) {
            var fields = std.ArrayList(*ast.RecordFieldNode).init(self.allocator);
            errdefer {
                for (fields.items) |field| {
                    field.release(self.allocator);
                }
                fields.deinit();
            }

            const first_field_name = try self.parseLowerIdentifier();
            errdefer first_field_name.release(self.allocator);

            _ = try self.expect(lexer.TokenKind{ .delimiter = .Colon });

            const first_field_type = try self.parseTypeExpr();
            errdefer {
                first_field_type.release(self.allocator);
                self.allocator.destroy(first_field_type);
            }

            const first_field_node = try self.allocator.create(ast.RecordFieldNode);
            errdefer first_field_node.release(self.allocator);

            first_field_node.* = .{
                .name = first_field_name,
                .type = first_field_type,
                .token = first_field_name.token,
            };

            try fields.append(first_field_node);

            while (try self.match(lexer.TokenKind{ .delimiter = .Comma })) {
                const field_name = try self.parseLowerIdentifier();
                errdefer field_name.release(self.allocator);

                _ = try self.expect(lexer.TokenKind{ .delimiter = .Colon });

                const field_type = try self.parseTypeExpr();
                errdefer {
                    field_type.release(self.allocator);
                    self.allocator.destroy(field_type);
                }

                const field_node = try self.allocator.create(ast.RecordFieldNode);
                errdefer field_node.release(self.allocator);

                field_node.* = .{
                    .name = field_name,
                    .type = field_type,
                    .token = field_name.token,
                };

                try fields.append(field_node);
            }

            _ = try self.expect(lexer.TokenKind{ .delimiter = .RightBrace });

            const rtype_node = try self.allocator.create(ast.RecordTypeNode);
            errdefer rtype_node.release(self.allocator);

            rtype_node.* = .{
                .name = type_ident,
                .type_params = type_params,
                .fields = fields,
                .token = start_token,
            };

            const node = try self.allocator.create(ast.Node);
            errdefer {
                node.release(self.allocator);
                self.allocator.destroy(node);
            }

            node.* = .{ .record_type = rtype_node };

            return node;
        }

        var constructors = std.ArrayList(*ast.VariantConstructorNode).init(self.allocator);
        errdefer {
            for (constructors.items) |constructor| {
                constructor.release(self.allocator);
            }
            constructors.deinit();
        }

        _ = try self.match(lexer.TokenKind{ .symbol = .Pipe });

        while (true) {
            const constructor = try self.parseUpperIdentifier();
            errdefer constructor.release(self.allocator);

            var parameters = std.ArrayList(*ast.Node).init(self.allocator);
            errdefer {
                for (parameters.items) |param| {
                    param.release(self.allocator);
                    self.allocator.destroy(param);
                }
                parameters.deinit();
            }

            // Check for parameters in parentheses
            if (try self.match(lexer.TokenKind{ .delimiter = .LeftParen })) {
                const first_param = try self.parseTypeExpr();
                errdefer {
                    first_param.release(self.allocator);
                    self.allocator.destroy(first_param);
                }

                try parameters.append(first_param);

                // Parse additional parameters separated by commas
                while (try self.match(lexer.TokenKind{ .delimiter = .Comma })) {
                    const next_param = try self.parseTypeExpr();
                    errdefer {
                        next_param.release(self.allocator);
                        self.allocator.destroy(next_param);
                    }

                    try parameters.append(next_param);
                }

                _ = try self.expect(lexer.TokenKind{ .delimiter = .RightParen });
            }

            const constructor_node = try self.allocator.create(ast.VariantConstructorNode);
            errdefer constructor_node.release(self.allocator);

            constructor_node.* = .{
                .name = constructor,
                .parameters = parameters,
                .token = constructor.token,
            };

            try constructors.append(constructor_node);

            if (!try self.match(lexer.TokenKind{ .symbol = .Pipe })) {
                break;
            }
        }

        if (constructors.items.len == 0) {
            return error.UnexpectedToken;
        }

        const vtype_node = try self.allocator.create(ast.VariantTypeNode);
        errdefer vtype_node.release(self.allocator);

        vtype_node.* = .{
            .name = type_ident,
            .type_params = type_params,
            .constructors = constructors,
            .token = start_token,
        };

        const node = try self.allocator.create(ast.Node);
        errdefer {
            node.release(self.allocator);
            self.allocator.destroy(node);
        }

        node.* = .{ .variant_type = vtype_node };

        return node;
    }

    /// Parses a function declaration with an optional type annotations.
    ///
    /// Examples:
    /// - `let add(x, y) = x + y`
    /// - `let factorial(n : Int) -> Int = if n == 0 then 1 else n * factorial(n - 1)`
    /// - `let const(x : a, _ : b) -> a = x`
    fn parseFunctionDecl(self: *Parser) ParserError!*ast.Node {
        const start_token = try self.expect(lexer.TokenKind{ .keyword = .Let });

        const fn_name = try self.parseLowerIdentifier();
        errdefer fn_name.release(self.allocator);

        _ = try self.expect(lexer.TokenKind{ .delimiter = .LeftParen });

        var parameters = std.ArrayList(*ast.ParamDeclNode).init(self.allocator);
        errdefer {
            for (parameters.items) |param| {
                param.release(self.allocator);
            }
            parameters.deinit();
        }

        if (!self.check(lexer.TokenKind{ .delimiter = .RightParen })) {
            const first_param = try self.parseParameter();
            try parameters.append(first_param);

            while (try self.match(lexer.TokenKind{ .delimiter = .Comma })) {
                const next_param = try self.parseParameter();
                try parameters.append(next_param);
            }
        }

        _ = try self.expect(lexer.TokenKind{ .delimiter = .RightParen });

        var return_type: ?*ast.Node = null;
        errdefer {
            if (return_type) |rt| {
                rt.release(self.allocator);
                self.allocator.destroy(rt);
            }
        }

        if (try self.match(lexer.TokenKind{ .symbol = .ArrowRight })) {
            return_type = try self.parseTypeExpr();

            assert((return_type.?.* == .lower_identifier) or
                (return_type.?.* == .upper_identifier) or
                (return_type.?.* == .type_application));
        }

        _ = try self.expect(lexer.TokenKind{ .operator = .Equal });

        const value = try self.parseExpression();
        errdefer value.release(self.allocator);

        const func_decl_node = try self.allocator.create(ast.FunctionDeclNode);
        errdefer func_decl_node.release(self.allocator);

        func_decl_node.* = .{
            .name = fn_name,
            .parameters = parameters,
            .return_type = return_type,
            .value = value,
            .token = start_token,
        };

        const node = try self.allocator.create(ast.Node);
        errdefer {
            node.deinit(self.allocator);
            self.allocator.destroy(node);
        }

        node.* = .{ .function_decl = func_decl_node };

        return node;
    }

    /// Parses a foreign function declaration that links to external code.
    /// The declaration specifies an internal function name and type signature
    /// along with the external symbol name to link against.
    ///
    /// Examples:
    /// - `foreign sqrt(x : Float) -> Float = "c_sqrt"`
    /// - `foreign print(x : String) -> Unit = "c_print"`
    fn parseForeignFunctionDecl(self: *Parser) ParserError!*ast.Node {
        const start_token = try self.expect(lexer.TokenKind{ .keyword = .Foreign });

        const fn_name = try self.parseLowerIdentifier();
        errdefer fn_name.release(self.allocator);

        _ = try self.expect(lexer.TokenKind{ .delimiter = .LeftParen });

        var parameters = std.ArrayList(*ast.ParamDeclNode).init(self.allocator);
        errdefer {
            for (parameters.items) |param| {
                param.release(self.allocator);
            }
            parameters.deinit();
        }

        if (!self.check(lexer.TokenKind{ .delimiter = .RightParen })) {
            const first_param = try self.parseParameter();
            try parameters.append(first_param);

            while (try self.match(lexer.TokenKind{ .delimiter = .Comma })) {
                const second_param = try self.parseParameter();
                try parameters.append(second_param);
            }
        }

        _ = try self.expect(lexer.TokenKind{ .delimiter = .RightParen });

        _ = try self.match(lexer.TokenKind{ .symbol = .ArrowRight });

        const return_type = try self.parseTypeExpr();
        errdefer {
            return_type.release(self.allocator);
            self.allocator.destroy(return_type);
        }

        assert((return_type.* == .lower_identifier) or
            (return_type.* == .upper_identifier) or
            (return_type.* == .type_application));

        _ = try self.expect(lexer.TokenKind{ .operator = .Equal });

        const external_name = try self.parseStrLiteral();
        errdefer external_name.release(self.allocator);

        if (external_name.value.len == 0) {
            return error.EmptyFFIReference;
        }

        const foreign_node = try self.allocator.create(ast.ForeignFunctionDeclNode);
        errdefer foreign_node.release(self.allocator);

        foreign_node.* = .{
            .name = fn_name,
            .parameters = parameters,
            .return_type = return_type,
            .external_name = external_name,
            .token = start_token,
        };

        const node = try self.allocator.create(ast.Node);
        errdefer {
            node.release(self.allocator);
            self.allocator.destroy(node);
        }

        node.* = .{ .foreign_function_decl = foreign_node };

        return node;
    }

    /// Parses an include declaration which imports and re-exports all contents from a module.
    ///
    /// Examples:
    /// - `include MyModule`
    /// - `include Std.List`
    fn parseInclude(self: *Parser) ParserError!*ast.Node {
        const start_token = try self.expect(lexer.TokenKind{ .keyword = .Include });

        const path = try self.parseModulePath();
        errdefer {
            path.release(self.allocator);
            self.allocator.destroy(path);
        }

        const module_path = path.module_path;
        self.allocator.destroy(path);

        const include_node = try self.allocator.create(ast.IncludeNode);
        errdefer include_node.release(self.allocator);

        include_node.* = .{
            .path = module_path,
            .token = start_token,
        };

        const node = try self.allocator.create(ast.Node);
        errdefer {
            node.release(self.allocator);
            self.allocator.destroy(node);
        }

        node.* = .{ .include = include_node };

        return node;
    }

    /// Parses a complete expression by starting the precedence climbing algorithm
    /// at the lowest precedence level (0). This is the main entry point for
    /// parsing any expression.
    fn parseExpression(self: *Parser) ParserError!*ast.Node {
        // Start at highest precedence level
        return try self.parseBinaryExpr(0);
    }

    /// Implements precedence climbing to parse binary operator expressions.
    /// Uses operator precedence and associativity from getOperatorInfo() to build
    /// correctly structured expression trees.
    fn parseBinaryExpr(self: *Parser, min_precedence: u8) ParserError!*ast.Node {
        var left = try self.parseSimpleExpr();

        while (true) {
            const op_info = if (Parser.getOperatorInfo(self.current_token.kind)) |info| info else break;

            if (op_info.precedence < min_precedence) break;

            const operator = self.current_token;
            try self.advance();

            // Determine minimum precedence for right side
            const next_min = switch (op_info.associativity) {
                .Left => op_info.precedence + 1,
                .Right => op_info.precedence,
                .None => op_info.precedence + 1,
            };

            const right = try self.parseBinaryExpr(next_min);

            const node = try self.allocator.create(ast.Node);
            errdefer {
                node.release(self.allocator);
                self.allocator.destroy(node);
            }

            switch (operator.kind) {
                .operator => |op| {
                    if ((op == .Exp) or
                        (op == .FloatAdd) or
                        (op == .FloatDiv) or
                        (op == .FloatMul) or
                        (op == .FloatSub) or
                        (op == .IntAdd) or
                        (op == .IntDiv) or
                        (op == .IntMul) or
                        (op == .IntSub))
                    {
                        const arithmetic_node = try self.allocator.create(ast.ArithmeticExprNode);
                        errdefer arithmetic_node.release(self.allocator);

                        arithmetic_node.* = .{
                            .left = left,
                            .right = right,
                            .operator = operator,
                        };

                        node.* = .{ .arithmetic_expr = arithmetic_node };
                    } else if (op == .LogicalAnd or op == .LogicalOr) {
                        const logical_node = try self.allocator.create(ast.LogicalExprNode);
                        errdefer logical_node.release(self.allocator);

                        logical_node.* = .{
                            .left = left,
                            .right = right,
                            .operator = operator,
                        };

                        node.* = .{ .logical_expr = logical_node };
                    } else if ((op == .Equality) or
                        (op == .GreaterThan) or
                        (op == .GreaterThanEqual) or
                        (op == .LessThan) or
                        (op == .LessThanEqual) or
                        (op == .NotEqual))
                    {
                        const comparison_node = try self.allocator.create(ast.ComparisonExprNode);
                        errdefer comparison_node.release(self.allocator);

                        comparison_node.* = .{
                            .left = left,
                            .right = right,
                            .operator = operator,
                        };

                        node.* = .{ .comparison_expr = comparison_node };
                    } else if (op == .StrConcat) {
                        const str_concat_node = try self.allocator.create(ast.StrConcatExprNode);
                        errdefer str_concat_node.release(self.allocator);

                        str_concat_node.* = .{
                            .left = left,
                            .right = right,
                            .operator = operator,
                        };

                        node.* = .{ .str_concat_expr = str_concat_node };
                    } else if (op == .ListConcat) {
                        const list_concat_node = try self.allocator.create(ast.ListConcatExprNode);
                        errdefer list_concat_node.release(self.allocator);

                        list_concat_node.* = .{
                            .left = left,
                            .right = right,
                            .operator = operator,
                        };

                        node.* = .{ .list_concat_expr = list_concat_node };
                    } else if (op == .Cons) {
                        const cons_node = try self.allocator.create(ast.ConsExprNode);
                        errdefer cons_node.release(self.allocator);

                        cons_node.* = .{
                            .head = left,
                            .tail = right,
                            .operator = operator,
                        };

                        node.* = .{ .cons_expr = cons_node };
                    } else if (op == .PipeRight) {
                        const pipe_node = try self.allocator.create(ast.PipeExprNode);
                        errdefer pipe_node.release(self.allocator);

                        pipe_node.* = .{
                            .value = left,
                            .func = right,
                            .operator = operator,
                        };

                        node.* = .{ .pipe_expr = pipe_node };
                    } else {
                        unreachable;
                    }
                },
                else => unreachable,
            }

            left = node;
        }

        return left;
    }

    /// Identify if the current token _could_ be used as a unary operator.
    fn isUnaryOp(self: *Parser) bool {
        return self.check(lexer.TokenKind{ .operator = .IntSub });
    }

    /// Parses a simple expression, such as a unary operation or a primary expression.
    /// If the current token represents a unary operator, it parses the operator and its operand.
    /// Otherwise, delegates to `parsePrimaryExpr` for parsing primary expressions.
    fn parseSimpleExpr(self: *Parser) ParserError!*ast.Node {
        if (self.isUnaryOp()) {
            const operator = self.current_token;
            try self.advance();

            const operand = try self.parseSimpleExpr();

            const unary_node = try self.allocator.create(ast.UnaryExprNode);
            errdefer unary_node.release(self.allocator);

            unary_node.* = .{
                .operand = operand,
                .operator = operator,
            };

            const node = try self.allocator.create(ast.Node);
            errdefer {
                node.release(self.allocator);
                self.allocator.destroy(node);
            }

            node.* = .{ .unary_expr = unary_node };

            return node;
        }

        if (self.check(lexer.TokenKind{ .identifier = .Lower })) {
            const ident = try self.parseLowerIdentifier();
            errdefer ident.release(self.allocator);

            if (self.check(lexer.TokenKind{ .delimiter = .LeftParen })) {
                return self.parseFunctionCall(ident);
            }

            const node = try self.allocator.create(ast.Node);
            errdefer {
                node.release(self.allocator);
                self.allocator.destroy(node);
            }

            node.* = .{ .lower_identifier = ident };

            return node;
        }

        return self.parsePrimaryExpr();
    }

    /// Parses a function call expression.
    ///
    /// Examples:
    /// - `add(42, 17)`
    /// - `map(double, [1, 2, 3])`
    /// - `getFunction()(x, y)`
    fn parseFunctionCall(self: *Parser, fn_name: *ast.LowerIdentifierNode) ParserError!*ast.Node {
        _ = try self.expect(lexer.TokenKind{ .delimiter = .LeftParen });

        var arguments = std.ArrayList(*ast.Node).init(self.allocator);
        errdefer {
            for (arguments.items) |arg| {
                arg.release(self.allocator);
                self.allocator.destroy(arg);
            }
            arguments.deinit();
        }

        if (!self.check(lexer.TokenKind{ .delimiter = .RightParen })) {
            const first_arg = try self.parseExpression();
            try arguments.append(first_arg);

            while (try self.match(lexer.TokenKind{ .delimiter = .Comma })) {
                const next_arg = try self.parseExpression();
                try arguments.append(next_arg);
            }
        }

        _ = try self.expect(lexer.TokenKind{ .delimiter = .RightParen });

        const func_node = try self.allocator.create(ast.Node);
        errdefer {
            func_node.release(self.allocator);
            self.allocator.destroy(func_node);
        }

        func_node.* = .{ .lower_identifier = fn_name };

        const call_node = try self.allocator.create(ast.FunctionCallNode);
        errdefer call_node.release(self.allocator);

        call_node.* = .{
            .function = func_node,
            .arguments = arguments,
            .token = fn_name.token,
        };

        const node = try self.allocator.create(ast.Node);
        errdefer {
            node.release(self.allocator);
            self.allocator.destroy(node);
        }

        node.* = .{ .function_call = call_node };

        return node;
    }

    /// Parses a primary expression, such as literals, identifiers, or parenthesized expressions.
    /// Handles cases for basic literals (int, float, string, char), identifiers (lower and upper),
    /// and parenthesized expressions.
    fn parsePrimaryExpr(self: *Parser) ParserError!*ast.Node {
        switch (self.current_token.kind) {
            .delimiter => |delim| switch (delim) {
                .LeftBracket => {
                    return try self.parseList();
                },
                .LeftParen => {
                    try self.advance();

                    const expr = try self.parseExpression();

                    _ = try self.expect(lexer.TokenKind{ .delimiter = .RightParen });

                    return expr;
                },
                else => return error.UnexpectedToken,
            },
            .identifier => |ident| {
                const node = try self.allocator.create(ast.Node);
                errdefer {
                    node.release(self.allocator);
                    self.allocator.destroy(node);
                }

                switch (ident) {
                    .Lower => {
                        const identifier = try self.parseLowerIdentifier();

                        node.* = .{ .lower_identifier = identifier };
                    },
                    .Upper => {
                        const identifier = try self.parseUpperIdentifier();

                        node.* = .{ .upper_identifier = identifier };
                    },
                }

                return node;
            },
            .keyword => |kw| switch (kw) {
                .Fn => return try self.parseLambdaExpr(),
                .If => return try self.parseIfThenElse(),
                else => return error.UnexpectedToken,
            },
            .literal => |lit| {
                const node = try self.allocator.create(ast.Node);
                errdefer {
                    node.release(self.allocator);
                    self.allocator.destroy(node);
                }

                switch (lit) {
                    .Char => {
                        const char_literal = try self.parseCharLiteral();

                        node.* = .{ .char_literal = char_literal };
                    },
                    .Float => {
                        const float_literal = try self.parseFloatLiteral();

                        node.* = .{ .float_literal = float_literal };
                    },
                    .Int => {
                        const int_literal = try self.parseIntLiteral();

                        node.* = .{ .int_literal = int_literal };
                    },
                    .String => {
                        const str_literal = try self.parseStrLiteral();

                        node.* = .{ .str_literal = str_literal };
                    },
                    .MultilineString => {
                        const str_literal = try self.parseMultilineStringLiteral();

                        node.* = .{ .multiline_str_literal = str_literal };
                    },
                }

                return node;
            },
            else => return error.UnexpectedToken,
        }
    }

    /// Parses a lambda expression.
    ///
    /// Examples:
    /// - `fn(x) => x + 1`
    /// - `fn(x, y) => x * y`
    /// - `fn(a, b, c) => a + b + c`
    fn parseLambdaExpr(self: *Parser) ParserError!*ast.Node {
        const start_token = try self.expect(lexer.TokenKind{ .keyword = .Fn });
        _ = try self.expect(lexer.TokenKind{ .delimiter = .LeftParen });

        var parameters = std.ArrayList(*ast.ParamDeclNode).init(self.allocator);
        errdefer {
            for (parameters.items) |param| {
                param.release(self.allocator);
            }
            parameters.deinit();
        }

        if (!self.check(lexer.TokenKind{ .delimiter = .RightParen })) {
            const first_param = try self.parseParameter();
            try parameters.append(first_param);

            while (try self.match(lexer.TokenKind{ .delimiter = .Comma })) {
                const next_param = try self.parseParameter();
                try parameters.append(next_param);
            }
        }

        _ = try self.expect(lexer.TokenKind{ .delimiter = .RightParen });

        var return_type: ?*ast.Node = null;
        errdefer {
            if (return_type) |rt| {
                rt.release(self.allocator);
                self.allocator.destroy(rt);
            }
        }

        if (try self.match(lexer.TokenKind{ .symbol = .ArrowRight })) {
            return_type = try self.parseTypeExpr();
        }

        _ = try self.expect(lexer.TokenKind{ .symbol = .DoubleArrowRight });

        const body = try self.parseExpression();
        errdefer {
            body.release(self.allocator);
            self.allocator.destroy(body);
        }

        const lambda_node = try self.allocator.create(ast.LambdaExprNode);
        errdefer lambda_node.release(self.allocator);

        lambda_node.* = .{
            .parameters = parameters,
            .return_type = return_type,
            .body = body,
            .token = start_token,
        };

        const node = try self.allocator.create(ast.Node);
        errdefer {
            node.release(self.allocator);
            self.allocator.destroy(node);
        }

        node.* = .{ .lambda_expr = lambda_node };

        return node;
    }

    // Helper function to parse a single parameter with optional type annotation.
    fn parseParameter(self: *Parser) ParserError!*ast.ParamDeclNode {
        const param_token = self.current_token;

        const name = try self.parseParamName(param_token);
        errdefer name.release(self.allocator);

        var type_annotation: ?*ast.Node = null;
        errdefer {
            if (type_annotation) |t| {
                t.release(self.allocator);
                self.allocator.destroy(t);
            }
        }

        if (try self.match(lexer.TokenKind{ .delimiter = .Colon })) {
            type_annotation = try self.parseTypeExpr();
        }

        const node = try self.allocator.create(ast.ParamDeclNode);
        errdefer node.release(self.allocator);

        node.* = .{
            .name = name,
            .type_annotation = type_annotation,
            .token = param_token,
        };

        return node;
    }

    fn parseParamName(self: *Parser, param_token: lexer.Token) ParserError!*ast.LowerIdentifierNode {
        if (try self.match(lexer.TokenKind{ .symbol = .Underscore })) {
            const node = try self.allocator.create(ast.LowerIdentifierNode);
            errdefer node.release(self.allocator);

            node.* = .{
                .identifier = try self.allocator.dupe(u8, "_"),
                .token = param_token,
            };

            return node;
        } else {
            return try self.parseLowerIdentifier();
        }
    }

    /// Parses a list expression surrounded by square brackets with comma-separated elements.
    ///
    /// Examples:
    /// - `[]`
    /// - `[1, 2, 3]`
    /// - `[True, False]`
    /// - `["hello", "world"]`
    fn parseList(self: *Parser) ParserError!*ast.Node {
        const start_token = try self.expect(lexer.TokenKind{ .delimiter = .LeftBracket });

        var elements = std.ArrayList(*ast.Node).init(self.allocator);
        errdefer {
            for (elements.items) |elem| {
                elem.release(self.allocator);
                self.allocator.destroy(elem);
            }
            elements.deinit();
        }

        if (try self.match(lexer.TokenKind{ .delimiter = .RightBracket })) {
            const empty_list = try self.allocator.create(ast.ListNode);
            errdefer empty_list.release(self.allocator);

            empty_list.* = .{
                .elements = elements,
                .token = start_token,
            };

            const node = try self.allocator.create(ast.Node);
            errdefer {
                node.release(self.allocator);
                self.allocator.destroy(node);
            }

            node.* = .{ .list = empty_list };

            return node;
        }

        const first_element = try self.parseExpression();
        try elements.append(first_element);

        while (try self.match(lexer.TokenKind{ .delimiter = .Comma })) {
            const element = try self.parseExpression();
            try elements.append(element);
        }

        _ = try self.expect(lexer.TokenKind{ .delimiter = .RightBracket });

        const list_node = try self.allocator.create(ast.ListNode);
        errdefer list_node.release(self.allocator);

        list_node.* = .{
            .elements = elements,
            .token = start_token,
        };

        const node = try self.allocator.create(ast.Node);
        errdefer {
            node.release(self.allocator);
            self.allocator.destroy(node);
        }

        node.* = .{ .list = list_node };

        return node;
    }

    /// Parses a tuple expression surrounded by parentheses with 2 comma-separated elements.
    ///
    /// Examples:
    /// - `(1, "hello")`
    /// - `(x, True)`
    /// - `(42, some_func(x))`
    fn parseTuple(self: *Parser) ParserError!*ast.Node {
        const start_token = try self.expect(lexer.TokenKind{ .delimiter = .LeftParen });

        var elements = std.ArrayList(*ast.Node).init(self.allocator);
        errdefer {
            for (elements.items) |elem| {
                elem.release(self.allocator);
                self.allocator.destroy(elem);
            }
            elements.deinit();
        }

        const first = try self.parseSimpleExpr();
        errdefer {
            first.release(self.allocator);
            self.allocator.destroy(first);
        }

        try elements.append(first);

        _ = try self.match(lexer.TokenKind{ .delimiter = .Comma });

        const second = try self.parseSimpleExpr();
        errdefer {
            second.release(self.allocator);
            self.allocator.destroy(second);
        }

        try elements.append(second);

        _ = try self.match(lexer.TokenKind{ .delimiter = .RightParen });

        assert(elements.items.len == 2);

        const tuple_node = try self.allocator.create(ast.TupleNode);
        errdefer tuple_node.release(self.allocator);

        tuple_node.* = .{
            .elements = elements,
            .token = start_token,
        };

        const node = try self.allocator.create(ast.Node);
        errdefer {
            node.release(self.allocator);
            self.allocator.destroy(node);
        }

        node.* = .{ .tuple = tuple_node };

        return node;
    }

    /// Parses a conditional expression with a required 'then' and 'else' branch.
    /// The condition must evaluate to a boolean value. If true, the 'then' branch
    /// is evaluated; otherwise, the 'else' branch is evaluated.
    ///
    /// Examples:
    /// - `if x > 0 then
    ///    "positive"
    ///else
    ///    "non-positive"`
    /// - `if empty?(list) then
    ///    None
    ///else
    ///    head(list)`
    /// - `if x == 0 then
    ///    "zero"
    ///else if x < 0 then
    ///    "negative"
    ///else
    ///    "positive"`
    fn parseIfThenElse(self: *Parser) ParserError!*ast.Node {
        const start_token = try self.expect(lexer.TokenKind{ .keyword = .If });

        const condition = try self.parseExpression();
        errdefer {
            condition.release(self.allocator);
            self.allocator.destroy(condition);
        }

        _ = try self.expect(lexer.TokenKind{ .keyword = .Then });

        const then_branch = try self.parseExpression();
        errdefer {
            then_branch.release(self.allocator);
            self.allocator.destroy(then_branch);
        }

        _ = try self.expect(lexer.TokenKind{ .keyword = .Else });

        const else_branch = try self.parseExpression();
        errdefer {
            else_branch.release(self.allocator);
            self.allocator.destroy(else_branch);
        }

        const if_node = try self.allocator.create(ast.IfThenElseStmtNode);
        errdefer if_node.release(self.allocator);

        if_node.* = .{
            .condition = condition,
            .then_branch = then_branch,
            .else_branch = else_branch,
            .token = start_token,
        };

        const node = try self.allocator.create(ast.Node);
        errdefer {
            node.release(self.allocator);
            self.allocator.destroy(node);
        }

        node.* = .{ .if_then_else_stmt = if_node };

        return node;
    }

    /// Parses a type expression, which can be a simple type name or a more complex
    /// parameterized type.
    ///
    /// Examples:
    /// - `Int`
    /// - `Maybe(a)`
    /// - `List(String)`
    fn parseTypeExpr(self: *Parser) ParserError!*ast.Node {
        if (self.check(lexer.TokenKind{ .identifier = .Upper })) {
            const start_token = self.current_token;

            const type_name = try self.parseUpperIdentifier();
            errdefer type_name.release(self.allocator);

            var type_args = std.ArrayList(*ast.Node).init(self.allocator);
            errdefer {
                for (type_args.items) |t| {
                    t.release(self.allocator);
                    self.allocator.destroy(t);
                }
                type_args.deinit();
            }

            if (try self.match(lexer.TokenKind{ .delimiter = .LeftParen })) {
                const first_arg = try self.parseTypeExpr();
                errdefer {
                    first_arg.release(self.allocator);
                    self.allocator.destroy(first_arg);
                }

                try type_args.append(first_arg);

                while (try self.match(lexer.TokenKind{ .delimiter = .Comma })) {
                    const next_arg = try self.parseTypeExpr();
                    errdefer {
                        next_arg.release(self.allocator);
                        self.allocator.destroy(next_arg);
                    }

                    try type_args.append(next_arg);
                }

                _ = try self.expect(lexer.TokenKind{ .delimiter = .RightParen });

                const type_app_node = try self.allocator.create(ast.TypeApplicationNode);
                errdefer type_app_node.release(self.allocator);

                type_app_node.* = .{
                    .constructor = type_name,
                    .args = type_args,
                    .token = start_token,
                };

                const node = try self.allocator.create(ast.Node);
                errdefer {
                    node.release(self.allocator);
                    self.allocator.destroy(node);
                }

                node.* = .{ .type_application = type_app_node };

                return node;
            }

            const node = try self.allocator.create(ast.Node);
            errdefer {
                node.release(self.allocator);
                self.allocator.destroy(node);
            }

            node.* = .{ .upper_identifier = type_name };

            return node;
        }

        if (self.check(lexer.TokenKind{ .identifier = .Lower })) {
            const type_var = try self.parseLowerIdentifier();
            errdefer type_var.release(self.allocator);

            const node = try self.allocator.create(ast.Node);
            errdefer {
                node.release(self.allocator);
                self.allocator.destroy(node);
            }

            node.* = .{ .lower_identifier = type_var };

            return node;
        }

        if (try self.match(lexer.TokenKind{ .delimiter = .LeftParen })) {
            var types = std.ArrayList(*ast.Node).init(self.allocator);
            errdefer {
                for (types.items) |t| {
                    t.release(self.allocator);
                    self.allocator.destroy(t);
                }
                types.deinit();
            }

            const first_type = try self.parseTypeExpr();
            errdefer {
                first_type.release(self.allocator);
                self.allocator.destroy(first_type);
            }

            try types.append(first_type);

            if (try self.match(lexer.TokenKind{ .delimiter = .Comma })) {
                while (true) {
                    const next_type = try self.parseTypeExpr();
                    errdefer {
                        next_type.release(self.allocator);
                        self.allocator.destroy(next_type);
                    }

                    try types.append(next_type);

                    if (!try self.match(lexer.TokenKind{ .delimiter = .Comma })) {
                        break;
                    }
                }
            }

            _ = try self.expect(lexer.TokenKind{ .delimiter = .RightParen });

            if (types.items.len > 1) {
                const result = types.items[0]; // Temporary logic, need to properly handle function param lists

                for (types.items[1..]) |t| {
                    t.release(self.allocator);
                    self.allocator.destroy(t);
                }
                types.deinit();

                return result;
            }

            // For a single parenthesized type, just return the inner type
            const result = types.items[0];
            types.deinit();

            return result;
        }

        if (try self.match(lexer.TokenKind{ .special = .Hole })) {
            const node = try self.allocator.create(ast.Node);
            errdefer {
                node.release(self.allocator);
                self.allocator.destroy(node);
            }

            node.* = .{
                .typed_hole = .{
                    .token = self.current_token,
                },
            };

            return node;
        }

        return error.UnexpectedToken;
    }

    /// Parses a module path consisting of one or more uppercase identifiers separated by dots.
    /// Used by module declarations, includes, and imports.
    ///
    /// Examples:
    /// - `MyModule`
    /// - `Std.List`
    fn parseModulePath(self: *Parser) ParserError!*ast.Node {
        var segments = std.ArrayList(*ast.UpperIdentifierNode).init(self.allocator);
        errdefer {
            for (segments.items) |segment| {
                segment.release(self.allocator);
            }
            segments.deinit();
        }

        const first_segment = try self.parseUpperIdentifier();
        errdefer first_segment.release(self.allocator);

        try segments.append(first_segment);

        while (try self.match(lexer.TokenKind{ .delimiter = .Dot })) {
            const next_segment = try self.parseUpperIdentifier();
            errdefer next_segment.release(self.allocator);

            try segments.append(next_segment);
        }

        const path_node = try self.allocator.create(ast.ModulePathNode);
        errdefer path_node.release(self.allocator);

        path_node.* = .{
            .segments = segments,
            .token = first_segment.token,
        };

        const node = try self.allocator.create(ast.Node);
        errdefer {
            node.release(self.allocator);
            self.allocator.destroy(node);
        }

        node.* = .{ .module_path = path_node };

        return node;
    }

    /// Parses a regular comment node from the input.
    fn parseComment(self: *Parser) ParserError!*ast.Node {
        const start_token = try self.expect(lexer.TokenKind{ .comment = .Regular });

        // Content starts after '# ' prefix
        var i: usize = 1;
        while (i < start_token.lexeme.len and std.ascii.isWhitespace(start_token.lexeme[i])) {
            i += 1;
        }

        const text = std.mem.trimRight(u8, start_token.lexeme[i..], &std.ascii.whitespace);

        const comment_node = try self.allocator.create(ast.CommentNode);
        errdefer comment_node.release(self.allocator);

        comment_node.* = .{
            .text = try self.allocator.dupe(u8, text),
            .token = start_token,
        };

        const node = try self.allocator.create(ast.Node);
        errdefer {
            node.release(self.allocator);
            self.allocator.destroy(node);
        }

        node.* = .{ .comment = comment_node };

        return node;
    }

    /// Parses a documentation comment node from the input.
    fn parseDocComment(self: *Parser) ParserError!*ast.Node {
        const start_token = try self.expect(lexer.TokenKind{ .comment = .Doc });

        // Content starts after '## ' prefix
        var i: usize = 2;
        while (i < start_token.lexeme.len and std.ascii.isWhitespace(start_token.lexeme[i])) {
            i += 1;
        }

        const text = std.mem.trimRight(u8, start_token.lexeme[i..], &std.ascii.whitespace);

        const comment_node = try self.allocator.create(ast.DocCommentNode);
        errdefer comment_node.release(self.allocator);

        comment_node.* = .{
            .text = try self.allocator.dupe(u8, text),
            .token = start_token,
        };

        const node = try self.allocator.create(ast.Node);
        errdefer {
            node.release(self.allocator);
            self.allocator.destroy(node);
        }

        node.* = .{ .doc_comment = comment_node };

        return node;
    }
};

const testing = std.testing;

const TEST_FILE = "test.mn";

test "[comment]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        const source = "# This is a regular comment";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseComment();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node is a regular comment
        try testing.expect(node.* == .comment);

        const comment = node.comment;

        // Validate the comment token and its properties
        try testing.expectEqual(lexer.TokenKind{ .comment = .Regular }, comment.token.kind);

        // Ensure the content of the comment matches the source
        try testing.expectEqualStrings("This is a regular comment", comment.text);
    }
}

test "[doc_comment]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        const source = "## This is a doc comment";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseDocComment();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node is a doc comment
        try testing.expect(node.* == .doc_comment);

        const comment = node.doc_comment;

        // Validate the comment token and its properties
        try testing.expectEqual(lexer.TokenKind{ .comment = .Doc }, comment.token.kind);

        // Ensure the content of the comment matches the source
        try testing.expectEqualStrings("This is a doc comment", comment.text);
    }
}

test "[int_literal]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const TestCase = struct {
        source: []const u8,
        expected: i64,
    };

    const cases = [_]TestCase{
        // Base 16 (hex)
        .{
            .source = "0xFF",
            .expected = 255,
        },
        .{
            .source = "0xff",
            .expected = 255,
        },
        .{
            .source = "0xDEAD_BEEF",
            .expected = 0xDEADBEEF,
        },

        // Base 10 (decimal)
        .{
            .source = "42",
            .expected = 42,
        },
        .{
            .source = "1_234_567",
            .expected = 1234567,
        },

        // Base 8 (octal)
        .{
            .source = "0o52",
            .expected = 42,
        },
        .{
            .source = "0o755",
            .expected = 493,
        },

        // Base 2 (binary)
        .{
            .source = "0b1010",
            .expected = 10,
        },
        .{
            .source = "0b1010_1010",
            .expected = 170,
        },
    };

    for (cases) |case| {
        var l = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const literal = try parser.parseIntLiteral();

        // Assertions
        // Validate the literal token and its properties
        try testing.expectEqual(lexer.TokenKind{ .literal = .Int }, literal.token.kind);

        // Ensure the lexeme matches the source string
        try testing.expectEqualStrings(case.source, literal.token.lexeme);

        // Verify the parsed integer value matches the expected value
        try testing.expectEqual(case.expected, literal.value);
    }
}

test "[float_literal]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const TestCase = struct {
        source: []const u8,
        expected: f64,
    };

    const cases = [_]TestCase{
        .{
            .source = "3.14",
            .expected = 3.14,
        },
        .{
            .source = "42.0",
            .expected = 42.0,
        },
        .{
            .source = "1.23e4",
            .expected = 12300.0,
        },
        .{
            .source = "1.23e-4",
            .expected = 0.000123,
        },
        .{
            .source = "1_234.567_89",
            .expected = 1234.56789,
        },
    };

    for (cases) |case| {
        var l = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const literal = try parser.parseFloatLiteral();

        // Assertions
        // Validate the literal token and its properties
        try testing.expectEqual(lexer.TokenKind{ .literal = .Float }, literal.token.kind);

        // Ensure the lexeme matches the source string
        try testing.expectEqualStrings(case.source, literal.token.lexeme);

        // Verify the parsed float value matches the expected value
        try testing.expectEqual(case.expected, literal.value);
    }
}

test "[char_literal]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const TestCase = struct {
        source: []const u8,
        expected: u21,
    };

    const cases = [_]TestCase{
        // Regular characters
        .{
            .source = "'a'",
            .expected = 'a',
        },
        .{
            .source = "'Z'",
            .expected = 'Z',
        },
        .{
            .source = "'0'",
            .expected = '0',
        },
        .{
            .source = "'!'",
            .expected = '!',
        },

        // Escape sequences
        .{
            .source = "'\\n'",
            .expected = '\n',
        },
        .{
            .source = "'\\r'",
            .expected = '\r',
        },
        .{
            .source = "'\\t'",
            .expected = '\t',
        },
        .{
            .source = "'\\\\'",
            .expected = '\\',
        },
        .{
            .source = "'\\''",
            .expected = '\'',
        },

        // Unicode escape sequences
        .{
            .source = "'\\u{0061}'",
            .expected = 0x0061,
        }, // 'a'
        .{
            .source = "'\\u{1F600}'",
            .expected = 0x1F600,
        }, // 😀
        .{
            .source = "'😀'",
            .expected = 0x1F600,
        },
    };

    for (cases) |case| {
        var l = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const literal = try parser.parseCharLiteral();

        // Assertions
        // Validate the literal token and its properties
        try testing.expectEqual(lexer.TokenKind{ .literal = .Char }, literal.token.kind);

        // Ensure the lexeme matches the source string
        try testing.expectEqualStrings(case.source, literal.token.lexeme);

        // Verify the parsed char value matches the expected value
        try testing.expectEqual(case.expected, literal.value);
    }
}

test "[str_literal]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const TestCase = struct {
        source: []const u8,
        expected: []const u8,
    };

    const cases = [_]TestCase{
        // Empty string
        .{
            .source = "\"\"",
            .expected = "",
        },

        // Regular strings
        .{
            .source = "\"hello\"",
            .expected = "hello",
        },
        .{
            .source = "\"Hello, World!\"",
            .expected = "Hello, World!",
        },
        .{
            .source = "\"123\"",
            .expected = "123",
        },
        .{
            .source = "\"!@#$%^&*()\"",
            .expected = "!@#$%^&*()",
        },

        // Strings with spaces
        .{
            .source = "\"   \"",
            .expected = "   ",
        },
        .{
            .source = "\"a b c\"",
            .expected = "a b c",
        },
        .{
            .source = "\"  leading and trailing  \"",
            .expected = "  leading and trailing  ",
        },

        // Escape sequences
        .{
            .source = "\"\\n\"",
            .expected = "\n",
        },
        .{
            .source = "\"\\r\"",
            .expected = "\r",
        },
        .{
            .source = "\"\\t\"",
            .expected = "\t",
        },
        .{
            .source = "\"\\\\\"",
            .expected = "\\",
        },
        .{
            .source = "\"\\\"\"",
            .expected = "\"",
        },

        // Mixed escape sequences
        .{
            .source = "\"line1\\nline2\"",
            .expected = "line1\nline2",
        },
        .{
            .source = "\"tab\\there\"",
            .expected = "tab\there",
        },
        .{
            .source = "\"quotes: \\\"nested\\\"\"",
            .expected = "quotes: \"nested\"",
        },
        .{
            .source = "\"\\t\\r\\n\\\\\\\"\"",
            .expected = "\t\r\n\\\"",
        },
        .{
            .source = "\"line1\\r\\nline2\"",
            .expected = "line1\r\nline2",
        },
        .{
            .source = "\"line1\\n\\rline2\"",
            .expected = "line1\n\rline2",
        },

        // Unicode escape sequences
        .{
            .source = "\"\\u{0061}\"",
            .expected = "a",
        },
        .{
            .source = "\"\\u{1F600}\"",
            .expected = "😀",
        },
        .{
            .source = "\"hello \\u{1F600} world\"",
            .expected = "hello 😀 world",
        },

        // Direct Unicode characters
        .{
            .source = "\"Hello 世界\"",
            .expected = "Hello 世界",
        },
        .{
            .source = "\"café\"",
            .expected = "café",
        },
        .{
            .source = "\"🌟✨\"",
            .expected = "🌟✨",
        },

        // Mixed content
        .{
            .source = "\"Hello\\n\\tWorld\\u{1F600}!\"",
            .expected = "Hello\n\tWorld😀!",
        },
        .{
            .source = "\"Unicode \\u{2764} and emoji ❤\"",
            .expected = "Unicode ❤ and emoji ❤",
        },
        .{
            .source = "\"Escaped\\tand\\u{1F496}direct💖mixed\"",
            .expected = "Escaped\tand💖direct💖mixed",
        },
    };

    for (cases) |case| {
        var l = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const literal = try parser.parseStrLiteral();
        defer literal.release(allocator);

        // Assertions
        // Validate the literal token and its properties
        try testing.expectEqual(lexer.TokenKind{ .literal = .String }, literal.token.kind);

        // Ensure the lexeme matches the source string
        try testing.expectEqualStrings(case.source, literal.token.lexeme);

        // Verify the parsed string value matches the expected value
        try testing.expectEqualStrings(case.expected, literal.value);
    }
}

test "[multiline_str_literal]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const TestCase = struct {
        source: []const u8,
        expected: []const u8,
    };

    const cases = [_]TestCase{
        // Single line, no escapes
        .{
            .source = "\"\"\"Hello\"\"\"",
            .expected = "Hello",
        },

        // Single line with escape sequence
        .{
            .source = "\"\"\"Hello\\tWorld\"\"\"",
            .expected = "Hello\tWorld",
        },

        // Multiple lines, no indentation
        .{
            .source = "\"\"\"Hello\nWorld\"\"\"",
            .expected = "Hello\nWorld",
        },

        // Multiple lines with consistent indentation (dedented)
        .{
            .source = "\"\"\"    Hello\n    World\"\"\"",
            .expected = "Hello\nWorld",
        },

        // Multiple lines with varying indentation (dedented based on first line)
        .{
            .source = "\"\"\"    Hello\n        World\n    Goodbye\"\"\"",
            .expected = "Hello\n    World\nGoodbye",
        },

        // Leading newline (ignored for indentation)
        .{
            .source = "\"\"\"\n    Hello\n    World\"\"\"",
            .expected = "Hello\nWorld",
        },

        // Escape sequences across lines
        .{
            .source = "\"\"\"    Hello\\n\n    World\\t!\"\"\"",
            .expected = "Hello\n\nWorld\t!",
        },

        // Unicode escape
        .{
            .source = "\"\"\"    Hello\\u{2665}\n    World\"\"\"",
            .expected = "Hello♥\nWorld",
        },

        // Mixed content with escapes and indentation
        .{
            .source = "\"\"\"    Line1\\tTab\n        Line2\\n\n    Line3\"\"\"",
            .expected = "Line1\tTab\n    Line2\n\nLine3",
        },
    };

    for (cases) |case| {
        var l = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const literal = try parser.parseMultilineStringLiteral();
        defer literal.release(allocator);

        // Assertions
        // Validate the literal token and its properties
        try testing.expectEqual(lexer.TokenKind{ .literal = .MultilineString }, literal.token.kind);

        // Ensure the lexeme matches the source string
        try testing.expectEqualStrings(case.source, literal.token.lexeme);

        // Verify the parsed string value matches the expected value
        try testing.expectEqualStrings(case.expected, literal.value);
    }
}

test "[lower_identifier]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const TestCase = struct {
        source: []const u8,
        expected: []const u8,
    };

    const cases = [_]TestCase{
        .{
            .source = "x",
            .expected = "x",
        },
        .{
            .source = "foo",
            .expected = "foo",
        },
        .{
            .source = "valid?",
            .expected = "valid?",
        },
        .{
            .source = "foo_bar",
            .expected = "foo_bar",
        },
    };

    for (cases) |case| {
        var l = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const ident = try parser.parseLowerIdentifier();
        defer ident.release(allocator);

        // Assertions
        // Verify the token is recognized as a lower identifier
        try testing.expectEqual(lexer.TokenKind{ .identifier = .Lower }, ident.token.kind);

        // Ensure the lexeme matches the source string
        try testing.expectEqualStrings(case.source, ident.token.lexeme);

        // Verify the parsed identifier name matches the expected name
        try testing.expectEqualStrings(case.expected, ident.identifier);
    }
}

test "[upper_identifier]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const TestCase = struct {
        source: []const u8,
        expected: []const u8,
    };

    const cases = [_]TestCase{
        .{
            .source = "X",
            .expected = "X",
        },
        .{
            .source = "Foo",
            .expected = "Foo",
        },
        .{
            .source = "FooBar",
            .expected = "FooBar",
        },
        .{
            .source = "FooBar42",
            .expected = "FooBar42",
        },
        .{
            .source = "Foo_Bar",
            .expected = "Foo_Bar",
        },
    };

    for (cases) |case| {
        var l = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const ident = try parser.parseUpperIdentifier();
        defer ident.release(allocator);

        // Assertions
        // Verify the token is recognized as an upper identifier
        try testing.expectEqual(lexer.TokenKind{ .identifier = .Upper }, ident.token.kind);

        // Ensure the lexeme matches the source string
        try testing.expectEqualStrings(case.source, ident.token.lexeme);

        // Verify the parsed identifier name matches the expected name
        try testing.expectEqualStrings(case.expected, ident.identifier);
    }
}

test "[unary_expr]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        const source = "-42";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const expr = try parser.parseSimpleExpr();
        defer {
            expr.release(allocator);
            allocator.destroy(expr);
        }

        // Assertions
        // Ensure the expression is identified as a unary expression
        try testing.expect(expr.* == .unary_expr);

        // Validate the operator in the unary expression
        try testing.expectEqual(lexer.TokenKind{ .operator = .IntSub }, expr.unary_expr.operator.kind);
        try testing.expectEqualStrings("-", expr.unary_expr.operator.lexeme);

        const operand = expr.unary_expr.operand;

        // Ensure the operand is an integer literal
        try testing.expect(operand.* == .int_literal);

        // Verify the integer value of the operand is correct
        try testing.expect(operand.int_literal.value == 42);
    }
}

test "[arithmetic_expr]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        const source = "42 + 24";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const expr = try parser.parseExpression();
        defer {
            expr.release(allocator);
            allocator.destroy(expr);
        }

        // Assertions
        // Ensure the expression is identified as an arithmetic expression
        try testing.expect(expr.* == .arithmetic_expr);

        // Validate the operator in the arithmetic expression
        try testing.expectEqual(lexer.TokenKind{ .operator = .IntAdd }, expr.arithmetic_expr.operator.kind);

        const left = expr.arithmetic_expr.left;

        // Ensure the left operand is an integer literal
        try testing.expect(left.* == .int_literal);

        // Verify the integer value of the left operand is correct
        try testing.expect(left.int_literal.value == 42);

        const right = expr.arithmetic_expr.right;

        // Ensure the right operand is an integer literal
        try testing.expect(right.* == .int_literal);

        // Verify the integer value of the right operand is correct
        try testing.expect(right.int_literal.value == 24);
    }
}

test "[comparison_expr]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const TestCase = struct {
        source: []const u8,
        op: lexer.TokenKind,
        left_val: i64,
        right_val: i64,
    };

    const cases = [_]TestCase{
        .{
            .source = "42 == 24",
            .op = lexer.TokenKind{ .operator = .Equality },
            .left_val = 42,
            .right_val = 24,
        },
        .{
            .source = "42 /= 24",
            .op = lexer.TokenKind{ .operator = .NotEqual },
            .left_val = 42,
            .right_val = 24,
        },
        .{
            .source = "42 < 24",
            .op = lexer.TokenKind{ .operator = .LessThan },
            .left_val = 42,
            .right_val = 24,
        },
        .{
            .source = "42 > 24",
            .op = lexer.TokenKind{ .operator = .GreaterThan },
            .left_val = 42,
            .right_val = 24,
        },
        .{
            .source = "42 <= 24",
            .op = lexer.TokenKind{ .operator = .LessThanEqual },
            .left_val = 42,
            .right_val = 24,
        },
        .{
            .source = "42 >= 24",
            .op = lexer.TokenKind{ .operator = .GreaterThanEqual },
            .left_val = 42,
            .right_val = 24,
        },
    };

    for (cases) |case| {
        var l = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const expr = try parser.parseExpression();
        defer {
            expr.release(allocator);
            allocator.destroy(expr);
        }

        // Assertions
        // Ensure the expression is identified as a comparison expression
        try testing.expect(expr.* == .comparison_expr);

        // Validate that the operator in the comparison expression matches the expected operator
        try testing.expectEqual(case.op, expr.comparison_expr.operator.kind);

        const left = expr.comparison_expr.left;

        // Ensure the left operand is an integer literal
        try testing.expect(left.* == .int_literal);

        // Verify the value of the left operand matches the expected value
        try testing.expectEqual(case.left_val, left.int_literal.value);

        const right = expr.comparison_expr.right;

        // Ensure the right operand is an integer literal
        try testing.expect(right.* == .int_literal);

        // Verify the value of the right operand matches the expected value
        try testing.expectEqual(case.right_val, right.int_literal.value);
    }
}

test "[logical_expr]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const TestCase = struct {
        source: []const u8,
        op: lexer.TokenKind,
    };

    const cases = [_]TestCase{
        .{
            .source = "true && false",
            .op = lexer.TokenKind{ .operator = .LogicalAnd },
        },
        .{
            .source = "true || false",
            .op = lexer.TokenKind{ .operator = .LogicalOr },
        },
    };

    for (cases) |case| {
        var l = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const expr = try parser.parseExpression();
        defer {
            expr.release(allocator);
            allocator.destroy(expr);
        }

        // Assertions
        // Ensure the expression is identified as a logical expression
        try testing.expect(expr.* == .logical_expr);

        // Validate that the operator in the logical expression matches the expected operator
        try testing.expectEqual(case.op, expr.logical_expr.operator.kind);

        const left = expr.logical_expr.left;

        // Ensure the left operand is a lower identifier
        try testing.expect(left.* == .lower_identifier);

        // Verify the name of the left operand matches the expected value
        try testing.expectEqualStrings("true", left.lower_identifier.identifier);

        const right = expr.logical_expr.right;

        // Ensure the right operand is a lower identifier
        try testing.expect(right.* == .lower_identifier);

        // Verify the name of the right operand matches the expected value
        try testing.expectEqualStrings("false", right.lower_identifier.identifier);
    }
}

test "[cons_expr]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        const source = "x :: xs";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseExpression();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Ensure the expression is a cons expression
        try testing.expect(node.* == .cons_expr);

        const expr = node.cons_expr;

        // Verify head is 'x'
        try testing.expectEqualStrings("x", expr.head.lower_identifier.identifier);

        // Verify tail is 'xs'
        try testing.expectEqualStrings("xs", expr.tail.lower_identifier.identifier);
    }
}

test "[pipe_expr]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        // Simple pipe expression
        const source = "x |> f()";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseExpression();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node type is a pipe expression
        try testing.expect(node.* == .pipe_expr);

        const expr = node.pipe_expr;

        // Verify the pipe operator
        try testing.expectEqual(lexer.TokenKind{ .operator = .PipeRight }, expr.operator.kind);

        // Verify value side of pipe is 'x'
        try testing.expect(expr.value.* == .lower_identifier);
        try testing.expectEqualStrings("x", expr.value.lower_identifier.identifier);

        // Verify func side of pipe is 'f()'
        try testing.expect(expr.func.* == .function_call);
        try testing.expectEqualStrings("f", expr.func.function_call.function.lower_identifier.identifier);
        try testing.expectEqual(@as(usize, 0), expr.func.function_call.arguments.items.len);
    }

    {
        // Chained pipe expressions
        const source = "x |> f() |> g()";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseExpression();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node type is a pipe expression
        try testing.expect(node.* == .pipe_expr);

        const top_expr = node.pipe_expr;

        // Verify the pipe operator
        try testing.expectEqual(lexer.TokenKind{ .operator = .PipeRight }, top_expr.operator.kind);

        // Verify value side of top pipe is another pipe expression
        try testing.expect(top_expr.value.* == .pipe_expr);

        const nested_expr = top_expr.value.pipe_expr;

        // Verify the operator of the nested expression is also pipe
        try testing.expectEqual(lexer.TokenKind{ .operator = .PipeRight }, nested_expr.operator.kind);

        // Verify nested pipe value is 'x'
        try testing.expect(nested_expr.value.* == .lower_identifier);
        try testing.expectEqualStrings("x", nested_expr.value.lower_identifier.identifier);

        // Verify nested pipe func is 'f()'
        try testing.expect(nested_expr.func.* == .function_call);
        try testing.expectEqualStrings("f", nested_expr.func.function_call.function.lower_identifier.identifier);
        try testing.expectEqual(@as(usize, 0), nested_expr.func.function_call.arguments.items.len);

        // Verify top pipe func is 'g()'
        try testing.expect(top_expr.func.* == .function_call);
        try testing.expectEqualStrings("g", top_expr.func.function_call.function.lower_identifier.identifier);
        try testing.expectEqual(@as(usize, 0), top_expr.func.function_call.arguments.items.len);
    }

    {
        // Complex pipe expression with function call
        const source = "[1, 2, 3] |> map(double)";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseExpression();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node type is a pipe expression
        try testing.expect(node.* == .pipe_expr);

        const expr = node.pipe_expr;

        // Verify the pipe operator
        try testing.expectEqual(lexer.TokenKind{ .operator = .PipeRight }, expr.operator.kind);

        // Verify value side of pipe is a list
        try testing.expect(expr.value.* == .list);

        const list = expr.value.list;
        try testing.expectEqual(@as(usize, 3), list.elements.items.len);
        try testing.expect(list.elements.items[0].* == .int_literal);
        try testing.expectEqual(@as(i64, 1), list.elements.items[0].int_literal.value);
        try testing.expect(list.elements.items[1].* == .int_literal);
        try testing.expectEqual(@as(i64, 2), list.elements.items[1].int_literal.value);
        try testing.expect(list.elements.items[2].* == .int_literal);
        try testing.expectEqual(@as(i64, 3), list.elements.items[2].int_literal.value);

        // Verify func side of pipe is a function call
        try testing.expect(expr.func.* == .function_call);

        const func_call = expr.func.function_call;
        try testing.expectEqualStrings("map", func_call.function.lower_identifier.identifier);

        // Verify function call argument
        try testing.expectEqual(@as(usize, 1), func_call.arguments.items.len);
        try testing.expect(func_call.arguments.items[0].* == .lower_identifier);
        try testing.expectEqualStrings("double", func_call.arguments.items[0].lower_identifier.identifier);
    }

    {
        // Pipe with expression precedence
        const source = "1 + 2 |> toString()";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseExpression();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node type is a pipe expression
        try testing.expect(node.* == .pipe_expr);

        const expr = node.pipe_expr;

        // Verify the pipe operator
        try testing.expectEqual(lexer.TokenKind{ .operator = .PipeRight }, expr.operator.kind);

        // Verify value side of pipe is an addition expression
        try testing.expect(expr.value.* == .arithmetic_expr);

        const add_expr = expr.value.arithmetic_expr;
        try testing.expectEqual(lexer.TokenKind{ .operator = .IntAdd }, add_expr.operator.kind);

        // Verify the operands of the addition
        try testing.expect(add_expr.left.* == .int_literal);
        try testing.expectEqual(@as(i64, 1), add_expr.left.int_literal.value);
        try testing.expect(add_expr.right.* == .int_literal);
        try testing.expectEqual(@as(i64, 2), add_expr.right.int_literal.value);

        // Verify func side of pipe is 'toString()'
        try testing.expect(expr.func.* == .function_call);
        try testing.expectEqualStrings("toString", expr.func.function_call.function.lower_identifier.identifier);
        try testing.expectEqual(@as(usize, 0), expr.func.function_call.arguments.items.len);
    }
}

test "[operator precedence]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const TestCase = struct {
        source: []const u8,
        root_type: std.meta.Tag(ast.Node),
        root_op: lexer.TokenKind,
    };

    const cases = [_]TestCase{
        // Arithmetic precedence
        .{
            .source = "1 + 2 * 3", // Should be 1 + (2 * 3)
            .root_type = .arithmetic_expr,
            .root_op = lexer.TokenKind{ .operator = .IntAdd },
        },
        .{
            .source = "1 * 2 + 3", // Should be (1 * 2) + 3
            .root_type = .arithmetic_expr,
            .root_op = lexer.TokenKind{ .operator = .IntAdd },
        },
        .{
            .source = "2 ** 3 * 4", // Should be (2 ** 3) * 4
            .root_type = .arithmetic_expr,
            .root_op = lexer.TokenKind{ .operator = .IntMul },
        },

        // Comparison and arithmetic
        .{
            .source = "1 + 2 == 3 * 4", // Should be (1 + 2) == (3 * 4)
            .root_type = .comparison_expr,
            .root_op = lexer.TokenKind{ .operator = .Equality },
        },

        // Logical operators
        .{
            .source = "a && b || c", // Should be (a && b) || c
            .root_type = .logical_expr,
            .root_op = lexer.TokenKind{ .operator = .LogicalOr },
        },
        .{
            .source = "a || b && c", // Should be a || (b && c)
            .root_type = .logical_expr,
            .root_op = lexer.TokenKind{ .operator = .LogicalOr },
        },

        // Mixed operators
        .{
            .source = "1 + 2 * 3 == 4 && 5 * 6 == 7", // Should be ((1 + (2 * 3)) == 4) && ((5 * 6) == 7)
            .root_type = .logical_expr,
            .root_op = lexer.TokenKind{ .operator = .LogicalAnd },
        },
        .{
            .source = "2 ** 3 ** 4", // Should be 2 ** (3 ** 4) due to right associativity
            .root_type = .arithmetic_expr,
            .root_op = lexer.TokenKind{ .operator = .Exp },
        },
        .{
            .source = "a && b && c || d && e", // Should be ((a && b) && c) || (d && e)
            .root_type = .logical_expr,
            .root_op = lexer.TokenKind{ .operator = .LogicalOr },
        },
        .{
            .source = "1 * 2 + 3 * 4 + 5", // Should be ((1 * 2) + (3 * 4)) + 5
            .root_type = .arithmetic_expr,
            .root_op = lexer.TokenKind{ .operator = .IntAdd },
        },
        .{
            .source = "1 + 2 * 3 ** 4 * 5 + 6", // Should be 1 + ((2 * (3 ** 4)) * 5) + 6
            .root_type = .arithmetic_expr,
            .root_op = lexer.TokenKind{ .operator = .IntAdd },
        },
        .{
            .source = "a || b && c && d || e", // Should be (a || ((b && c) && d)) || e
            .root_type = .logical_expr,
            .root_op = lexer.TokenKind{ .operator = .LogicalOr },
        },
        .{
            .source = "1 * 2 + 3 == 4 + 5 * 6 && 7 + 8 == 9", // Should be (((1 * 2) + 3) == (4 + (5 * 6))) && ((7 + 8) == 9)
            .root_type = .logical_expr,
            .root_op = lexer.TokenKind{ .operator = .LogicalAnd },
        },
        .{
            .source = "a <= b + c * d && e > f ** g || h == i", // Should be ((a <= (b + (c * d))) && (e > (f ** g))) || (h == i)
            .root_type = .logical_expr,
            .root_op = lexer.TokenKind{ .operator = .LogicalOr },
        },
    };

    for (cases) |case| {
        var l = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const expr = try parser.parseExpression();
        defer {
            expr.release(allocator);
            allocator.destroy(expr);
        }

        // Assertions
        // Ensure the root type of the expression matches the expected type from the test case
        try testing.expectEqual(case.root_type, std.meta.activeTag(expr.*));

        switch (expr.*) {
            .arithmetic_expr => |aexpr| {
                // Validate the operator in an arithmetic expression
                try testing.expectEqual(case.root_op, aexpr.operator.kind);
            },
            .logical_expr => |lexpr| {
                // Validate the operator in a logical expression
                try testing.expectEqual(case.root_op, lexpr.operator.kind);
            },
            .comparison_expr => |cexpr| {
                // Validate the operator in a comparison expression
                try testing.expectEqual(case.root_op, cexpr.operator.kind);
            },
            else => unreachable,
        }
    }
}

test "[operator precedence] (structural)" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        const source = "1 + 2 * 3";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const expr = try parser.parseExpression();
        defer {
            expr.release(allocator);
            allocator.destroy(expr);
        }

        // Assertions
        // The expression should be structured as: (1 + (2 * 3))

        // Validate the root expression is an arithmetic expression with addition as the operator
        try testing.expect(expr.* == .arithmetic_expr);
        try testing.expectEqual(lexer.TokenKind{ .operator = .IntAdd }, expr.arithmetic_expr.operator.kind);

        const left = expr.arithmetic_expr.left;

        // Ensure the left operand is an integer literal with value 1
        try testing.expect(left.* == .int_literal);
        try testing.expectEqual(@as(i64, 1), left.int_literal.value);

        const right = expr.arithmetic_expr.right;

        // Ensure the right operand is an arithmetic expression with multiplication as the operator
        try testing.expect(right.* == .arithmetic_expr);
        try testing.expectEqual(lexer.TokenKind{ .operator = .IntMul }, right.arithmetic_expr.operator.kind);

        const mul_left = right.arithmetic_expr.left;

        // Ensure the left operand of the multiplication is an integer literal with value 2
        try testing.expect(mul_left.* == .int_literal);
        try testing.expectEqual(@as(i64, 2), mul_left.int_literal.value);

        const mul_right = right.arithmetic_expr.right;

        // Ensure the right operand of the multiplication is an integer literal with value 3
        try testing.expect(mul_right.* == .int_literal);
        try testing.expectEqual(@as(i64, 3), mul_right.int_literal.value);
    }

    {
        const source = "1 * 2 + 3 == 4";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const expr = try parser.parseExpression();
        defer {
            expr.release(allocator);
            allocator.destroy(expr);
        }

        // Assertions
        // The expression should be structured as: ((1 * 2 + 3) == 4)

        // Validate the root expression is a comparison expression with equality as the operator
        try testing.expect(expr.* == .comparison_expr);
        try testing.expectEqual(lexer.TokenKind{ .operator = .Equality }, expr.comparison_expr.operator.kind);

        const right = expr.comparison_expr.right;

        // Ensure the right operand is an integer literal with value 4
        try testing.expect(right.* == .int_literal);
        try testing.expectEqual(@as(i64, 4), right.int_literal.value);

        const left = expr.comparison_expr.left;

        // Ensure the left operand is an arithmetic expression with addition as the operator
        try testing.expect(left.* == .arithmetic_expr);
        try testing.expectEqual(lexer.TokenKind{ .operator = .IntAdd }, left.arithmetic_expr.operator.kind);

        const add_right = left.arithmetic_expr.right;

        // Ensure the right operand of the addition is an integer literal with value 3
        try testing.expect(add_right.* == .int_literal);
        try testing.expectEqual(@as(i64, 3), add_right.int_literal.value);

        const add_left = left.arithmetic_expr.left;

        // Ensure the left operand of the addition is an arithmetic expression with multiplication as the operator
        try testing.expect(add_left.* == .arithmetic_expr);
        try testing.expectEqual(lexer.TokenKind{ .operator = .IntMul }, add_left.arithmetic_expr.operator.kind);

        const mul_left = add_left.arithmetic_expr.left;

        // Ensure the left operand of the multiplication is an integer literal with value 1
        try testing.expect(mul_left.* == .int_literal);
        try testing.expectEqual(@as(i64, 1), mul_left.int_literal.value);

        const mul_right = add_left.arithmetic_expr.right;

        // Ensure the right operand of the multiplication is an integer literal with value 2
        try testing.expect(mul_right.* == .int_literal);
        try testing.expectEqual(@as(i64, 2), mul_right.int_literal.value);
    }
}

test "[lambda_expr]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        const source = "fn() => 1";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseExpression();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Ensure the expression is identified as a lambda expression
        try testing.expect(node.* == .lambda_expr);

        const expr = node.lambda_expr;

        // Verify the token kind matches
        try testing.expectEqual(lexer.TokenKind{ .keyword = .Fn }, expr.token.kind);

        // Validate that the lambda expression has no parameters
        try testing.expectEqual(@as(usize, 0), expr.parameters.items.len);

        // Verify return type
        try testing.expect(expr.return_type == null);

        // Validate that the body of the lambda expression is an arithmetic expression
        try testing.expect(expr.body.* == .int_literal);
    }

    {
        const source = "fn(x) => x + 1";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseExpression();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Ensure the expression is identified as a lambda expression
        try testing.expect(node.* == .lambda_expr);

        const expr = node.lambda_expr;

        // Verify the token kind matches
        try testing.expectEqual(lexer.TokenKind{ .keyword = .Fn }, expr.token.kind);

        const param1 = expr.parameters.items[0];

        // Validate that the lambda expression has exactly one parameter
        try testing.expectEqual(@as(usize, 1), expr.parameters.items.len);

        // Ensure the parameter name matches the expected value ("x")
        try testing.expectEqualStrings("x", param1.name.identifier);
        try testing.expect(param1.type_annotation == null);

        // Verify return type
        try testing.expect(expr.return_type == null);

        // Validate that the body of the lambda expression is an arithmetic expression
        try testing.expect(expr.body.* == .arithmetic_expr);
    }

    {
        const source = "fn(x : Int, y : Int) -> Int => x + y";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseExpression();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Ensure the expression is identified as a lambda expression
        try testing.expect(node.* == .lambda_expr);

        const expr = node.lambda_expr;

        // Verify the token kind matches
        try testing.expectEqual(lexer.TokenKind{ .keyword = .Fn }, expr.token.kind);

        // Validate that the lambda expression has exactly two parameters
        try testing.expectEqual(@as(usize, 2), expr.parameters.items.len);

        const param1 = expr.parameters.items[0];

        // Ensure the parameter name matches the expected value ("x")
        try testing.expectEqualStrings("x", param1.name.identifier);

        // Verify 'x : Int'
        try testing.expectEqualStrings("Int", param1.type_annotation.?.upper_identifier.identifier);

        const param2 = expr.parameters.items[1];

        // Ensure the parameter name matches the expected value ("y")
        try testing.expectEqualStrings("y", param2.name.identifier);

        // Verify 'y : Int'
        try testing.expectEqualStrings("Int", param2.type_annotation.?.upper_identifier.identifier);

        // Verify return type
        try testing.expectEqualStrings("Int", expr.return_type.?.upper_identifier.identifier);

        // Validate that the body of the lambda expression is an arithmetic expression
        try testing.expect(expr.body.* == .arithmetic_expr);
    }
}

test "[if_then_else_stmt]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        const source = "if x > 0 then 1 else -1";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseIfThenElse();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Ensure the node is identified as an if-then-else statement
        try testing.expect(node.* == .if_then_else_stmt);

        const stmt = node.if_then_else_stmt;

        // Validate the condition of the if-then-else statement
        try testing.expect(stmt.condition.* == .comparison_expr);

        const condition = stmt.condition.comparison_expr;

        // Ensure the condition's operator is a "greater than" comparison
        try testing.expectEqual(lexer.TokenKind{ .operator = .GreaterThan }, condition.operator.kind);

        // Check the left-hand side of the condition (should be the identifier "x")
        try testing.expect(condition.left.* == .lower_identifier);
        try testing.expectEqualStrings("x", condition.left.lower_identifier.identifier);

        // Check the right-hand side of the condition (should be the integer literal 0)
        try testing.expect(condition.right.* == .int_literal);
        try testing.expectEqual(@as(i64, 0), condition.right.int_literal.value);

        // Validate the "then" branch (should evaluate to integer literal 1)
        try testing.expect(stmt.then_branch.* == .int_literal);
        try testing.expectEqual(@as(i64, 1), stmt.then_branch.int_literal.value);

        // Validate the "else" branch (should evaluate to the unary expression -1)
        try testing.expect(stmt.else_branch.* == .unary_expr);

        const else_expr = stmt.else_branch.unary_expr;

        // Ensure the unary operator in the else branch is subtraction
        try testing.expectEqual(lexer.TokenKind{ .operator = .IntSub }, else_expr.operator.kind);

        // Ensure the operand of the unary expression is an integer literal 1
        try testing.expect(else_expr.operand.* == .int_literal);
        try testing.expectEqual(@as(i64, 1), else_expr.operand.int_literal.value);
    }
}

test "[function_decl]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        // Test a simple function declaration without type annotations
        const source = "let add(x, y) = x + y";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseFunctionDecl();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node is a function declaration
        try testing.expect(node.* == .function_decl);

        const decl = node.function_decl;

        // Check the function name matches
        try testing.expectEqualStrings("add", decl.name.identifier);

        // Verify the fn has exactly two parameters
        try testing.expectEqual(@as(usize, 2), decl.parameters.items.len);

        const param1 = decl.parameters.items[0];
        const param2 = decl.parameters.items[1];

        // Verify the parameter names are "x" and "y"
        try testing.expectEqualStrings("x", param1.name.identifier);
        try testing.expectEqualStrings("y", param2.name.identifier);

        // Verify paramater types are null
        try testing.expect(param1.type_annotation == null);
        try testing.expect(param2.type_annotation == null);

        // Verify the return type is null for this simple function declaration
        try testing.expect(decl.return_type == null);

        // Verify the function's value is an arithmetic expression
        try testing.expect(decl.value.* == .arithmetic_expr);

        const value = decl.value.arithmetic_expr;

        // Ensure the operator in the arithmetic expression is addition
        try testing.expectEqual(lexer.TokenKind{ .operator = .IntAdd }, value.operator.kind);

        // Verify the left operand of the addition is a lower identifier with the name "x"
        try testing.expect(value.left.* == .lower_identifier);
        try testing.expectEqualStrings("x", value.left.lower_identifier.identifier);

        // Verify the right operand of the addition is a lower identifier with the name "y"
        try testing.expect(value.right.* == .lower_identifier);
        try testing.expectEqualStrings("y", value.right.lower_identifier.identifier);
    }

    {
        // Test function declaration with type annotations
        const source = "let add(x : Int, y : Int) -> Int = x + y";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseFunctionDecl();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node is a function declaration
        try testing.expect(node.* == .function_decl);

        const decl = node.function_decl;

        // Check the function name matches
        try testing.expectEqualStrings("add", decl.name.identifier);

        // Verify the fn has exactly two parameters
        try testing.expectEqual(@as(usize, 2), decl.parameters.items.len);

        const param1 = decl.parameters.items[0];
        const param2 = decl.parameters.items[1];

        // Verify the parameter names are "x" and "y"
        try testing.expectEqualStrings("x", param1.name.identifier);
        try testing.expectEqualStrings("y", param2.name.identifier);

        // Verify paramater types are "Int"
        try testing.expectEqualStrings("Int", param1.type_annotation.?.upper_identifier.identifier);
        try testing.expectEqualStrings("Int", param2.type_annotation.?.upper_identifier.identifier);

        // Verify return type is "Int"
        try testing.expectEqualStrings("Int", decl.return_type.?.upper_identifier.identifier);

        // Verify the function's value is an arithmetic expression
        try testing.expect(decl.value.* == .arithmetic_expr);

        const value = decl.value.arithmetic_expr;

        // Ensure the operator in the arithmetic expression is addition
        try testing.expectEqual(lexer.TokenKind{ .operator = .IntAdd }, value.operator.kind);

        // Verify the left operand of the addition is a lower identifier with the name "x"
        try testing.expect(value.left.* == .lower_identifier);
        try testing.expectEqualStrings("x", value.left.lower_identifier.identifier);

        // Verify the right operand of the addition is a lower identifier with the name "y"
        try testing.expect(value.right.* == .lower_identifier);
        try testing.expectEqualStrings("y", value.right.lower_identifier.identifier);
    }

    {
        // Test function declaration with type annotations
        const source = "let identity(x : a) -> a = x";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseFunctionDecl();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node is a function declaration
        try testing.expect(node.* == .function_decl);

        const decl = node.function_decl;

        // Check the function name matches
        try testing.expectEqualStrings("identity", decl.name.identifier);

        // Verify the fn has exactly one parameter
        try testing.expectEqual(@as(usize, 1), decl.parameters.items.len);

        const param1 = decl.parameters.items[0];

        // Verify the parameter name is "x"
        try testing.expectEqualStrings("x", param1.name.identifier);

        // Verify paramater type is "a"
        try testing.expectEqualStrings("a", param1.type_annotation.?.lower_identifier.identifier);

        // Verify return type is "a"
        try testing.expectEqualStrings("a", decl.return_type.?.lower_identifier.identifier);

        // Verify the function's value is an identifier
        try testing.expect(decl.value.* == .lower_identifier);

        const value = decl.value.lower_identifier;

        try testing.expectEqualStrings("x", value.identifier);
    }

    {
        // Test function declaration with type annotations
        const source = "let ignore(_ : a) -> Unit = Unit";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseFunctionDecl();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node is a function declaration
        try testing.expect(node.* == .function_decl);

        const decl = node.function_decl;

        // Check the function name matches
        try testing.expectEqualStrings("ignore", decl.name.identifier);

        // Verify the fn has exactly one parameter
        try testing.expectEqual(@as(usize, 1), decl.parameters.items.len);

        const param1 = decl.parameters.items[0];

        // Verify the parameter name is "_"
        try testing.expectEqualStrings("_", param1.name.identifier);

        // Verify paramater type is "a"
        try testing.expectEqualStrings("a", param1.type_annotation.?.lower_identifier.identifier);

        // Verify return type is "Unit"
        try testing.expectEqualStrings("Unit", decl.return_type.?.upper_identifier.identifier);

        // Verify the function's value is an identifier
        try testing.expect(decl.value.* == .upper_identifier);

        const value = decl.value.upper_identifier;

        try testing.expectEqualStrings("Unit", value.identifier);
    }
}

test "[function_call]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        const source = "bitwise_and(x, y)";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseSimpleExpr();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node is a function call
        try testing.expect(node.* == .function_call);

        const call = node.function_call;

        // Verify the function name matches
        try testing.expectEqualStrings("bitwise_and", call.function.lower_identifier.identifier);

        // Verify the function call has exactly two parameters
        try testing.expectEqual(@as(usize, 2), call.arguments.items.len);

        // Verify the first arg is "x"
        try testing.expectEqualStrings("x", call.arguments.items[0].lower_identifier.identifier);

        // Verify the second arg is "y"
        try testing.expectEqualStrings("y", call.arguments.items[1].lower_identifier.identifier);
    }
}

test "[foreign_function_decl]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        const source = "foreign sqrt(x : Float) -> Float = \"c_sqrt\"";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseForeignFunctionDecl();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node is a foreign function declaration
        try testing.expect(node.* == .foreign_function_decl);

        const decl = node.foreign_function_decl;

        // Verify the function name matches
        try testing.expectEqualStrings("sqrt", decl.name.identifier);

        // Verify the function type has exactly one parameter
        try testing.expectEqual(@as(usize, 1), decl.parameters.items.len);

        const param1 = decl.parameters.items[0];

        // Verify the parameter name is "x"
        try testing.expectEqualStrings("x", param1.name.identifier);

        // Verify paramater type is "Flaot"
        try testing.expectEqualStrings("Float", param1.type_annotation.?.upper_identifier.identifier);

        // Verify return type is "Float"
        try testing.expectEqualStrings("Float", decl.return_type.upper_identifier.identifier);

        // Verify the external name matches
        try testing.expectEqualStrings("c_sqrt", decl.external_name.value);
    }
}

test "[module_path]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        const source = "Std.List";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseModulePath();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node is a path to a module
        try testing.expect(node.* == .module_path);

        const module_path = node.module_path;

        // Check the module path has exactly two segments
        try testing.expectEqual(@as(usize, 2), module_path.segments.items.len);

        // Verify the segments
        try testing.expectEqualStrings("Std", module_path.segments.items[0].identifier);
        try testing.expectEqualStrings("List", module_path.segments.items[1].identifier);
    }
}

test "[include]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        const source = "include Std.List";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseInclude();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node is an include statement
        try testing.expect(node.* == .include);

        const include = node.include;

        // Check the include path has exactly two segments
        try testing.expectEqual(@as(usize, 2), include.path.segments.items.len);

        // Verify the segments
        try testing.expectEqualStrings("Std", include.path.segments.items[0].identifier);
        try testing.expectEqualStrings("List", include.path.segments.items[1].identifier);
    }
}

test "[import_spec]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        // Simple import
        const source = "open MyModule";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseImportSpec();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node is an import specification
        try testing.expect(node.* == .import_spec);

        const import_spec = node.import_spec;

        // Verify it's a simple import
        try testing.expectEqual(ast.ImportKind.Simple, import_spec.kind);

        // Verify it has no alias
        try testing.expect(import_spec.alias == null);

        // Verify it has no items list
        try testing.expect(import_spec.items == null);

        // Verify the module path has exactly one segment
        try testing.expectEqual(@as(usize, 1), import_spec.path.segments.items.len);

        // Verify the module name is "MyModule"
        try testing.expectEqualStrings("MyModule", import_spec.path.segments.items[0].identifier);

        // Verify the token is the 'open' keyword
        try testing.expectEqual(lexer.TokenKind{ .keyword = .Open }, import_spec.token.kind);
        try testing.expectEqualStrings("open", import_spec.token.lexeme);
    }

    {
        // Alias import
        const source = "open MyModule as M";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseImportSpec();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node is an import specification
        try testing.expect(node.* == .import_spec);

        const import_spec = node.import_spec;

        // Verify it's an alias import
        try testing.expectEqual(ast.ImportKind.Alias, import_spec.kind);

        // Verify the module path has exactly one segment
        try testing.expectEqual(@as(usize, 1), import_spec.path.segments.items.len);

        // Verify the module name is "MyModule"
        try testing.expectEqualStrings("MyModule", import_spec.path.segments.items[0].identifier);

        // Verify it has an alias
        try testing.expect(import_spec.alias != null);

        // Verify the alias is "M"
        try testing.expectEqualStrings("M", import_spec.alias.?.identifier);

        // Verify it has no items list
        try testing.expect(import_spec.items == null);

        // Verify the token is the 'open' keyword
        try testing.expectEqual(lexer.TokenKind{ .keyword = .Open }, import_spec.token.kind);
        try testing.expectEqualStrings("open", import_spec.token.lexeme);
    }

    {
        // Using import
        const source = "open Std.List using (map, filter, Maybe, Either(..))";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseImportSpec();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node is an import specification
        try testing.expect(node.* == .import_spec);

        const import_spec = node.import_spec;

        // Verify it's a using import
        try testing.expectEqual(ast.ImportKind.Using, import_spec.kind);

        // Verify the module path has exactly two segments
        try testing.expectEqual(@as(usize, 2), import_spec.path.segments.items.len);

        // Verify the module name
        try testing.expectEqualStrings("Std", import_spec.path.segments.items[0].identifier);
        try testing.expectEqualStrings("List", import_spec.path.segments.items[1].identifier);

        // Verify it has no alias
        try testing.expect(import_spec.alias == null);

        // Verify it has exactly 4 imported items
        try testing.expect(import_spec.items.?.items.len == 4);

        // Check first function import (map)
        const item1 = import_spec.items.?.items[0].inner;
        try testing.expect(item1 == .function);
        try testing.expectEqualStrings("map", item1.function.name);
        try testing.expect(item1.function.alias == null);

        // Check second function import (filter)
        const item2 = import_spec.items.?.items[1].inner;
        try testing.expect(item2 == .function);
        try testing.expectEqualStrings("filter", item2.function.name);
        try testing.expect(item2.function.alias == null);

        // Check first type import (Maybe without constructors)
        const item3 = import_spec.items.?.items[2].inner;
        try testing.expect(item3 == .type);
        try testing.expectEqualStrings("Maybe", item3.type.name);
        try testing.expect(item3.type.alias == null);
        try testing.expect(item3.type.expose_constructors == false);

        // Check second type import (Either with constructors)
        const item4 = import_spec.items.?.items[3].inner;
        try testing.expect(item4 == .type);
        try testing.expectEqualStrings("Either", item4.type.name);
        try testing.expect(item4.type.alias == null);
        try testing.expect(item4.type.expose_constructors == true);
    }

    {
        // using import (with aliased and regular functions)
        const source = "open Std.List using (map as list_map, filter)";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseImportSpec();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node is an import specification
        try testing.expect(node.* == .import_spec);

        const import_spec = node.import_spec;

        // Verify it's a using import
        try testing.expectEqual(ast.ImportKind.Using, import_spec.kind);

        // Verify it has no alias
        try testing.expect(import_spec.alias == null);

        // Verify the module path has exactly two segments
        try testing.expectEqual(@as(usize, 2), import_spec.path.segments.items.len);

        // Verify the module segments
        try testing.expectEqualStrings("Std", import_spec.path.segments.items[0].identifier);
        try testing.expectEqualStrings("List", import_spec.path.segments.items[1].identifier);

        // Verify it has no alias
        try testing.expect(import_spec.items != null);

        // Check first item (map as list_map)
        const item1 = import_spec.items.?.items[0].inner;
        try testing.expect(item1 == .function);
        try testing.expectEqualStrings("map", item1.function.name);
        try testing.expect(item1.function.alias != null);
        try testing.expectEqualStrings("list_map", item1.function.alias.?);

        // Check second item (filter)
        const item2 = import_spec.items.?.items[1].inner;
        try testing.expect(item2 == .function);
        try testing.expectEqualStrings("filter", item2.function.name);
        try testing.expect(item2.function.alias == null);
    }

    {
        // Hiding import
        const source = "open Foo.Bar hiding (internal_func)";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseImportSpec();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node is an import specification
        try testing.expect(node.* == .import_spec);

        const import_spec = node.import_spec;

        // Verify it's a hiding import
        try testing.expectEqual(ast.ImportKind.Hiding, import_spec.kind);

        // Verify the module path has exactly two segments
        try testing.expectEqual(@as(usize, 2), import_spec.path.segments.items.len);

        // Verify the module segments
        try testing.expectEqualStrings("Foo", import_spec.path.segments.items[0].identifier);
        try testing.expectEqualStrings("Bar", import_spec.path.segments.items[1].identifier);

        // Verify it has no alias
        try testing.expect(import_spec.alias == null);

        // Verify it has exactly one hidden item
        try testing.expect(import_spec.items.?.items.len == 1);

        // Check the hidden function
        const item = import_spec.items.?.items[0].inner;
        try testing.expect(item == .function);
        try testing.expectEqualStrings("internal_func", item.function.name);
        try testing.expect(item.function.alias == null);
    }
}

test "[type_alias]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        // Basic type
        const source = "type alias UserId = String";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseTypeDecl();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node is a type alias declaration
        try testing.expect(node.* == .type_alias);

        const type_alias = node.type_alias;

        // Verify the name of the type alias is "UserId"
        try testing.expectEqualStrings("UserId", type_alias.name.identifier);

        // Verify the value of the type alias is an upper identifier
        try testing.expect(type_alias.value.* == .upper_identifier);

        // Verify the name of the upper identifier is "String"
        try testing.expectEqualStrings("String", type_alias.value.upper_identifier.identifier);
    }

    {
        // Type parameters
        const source = "type alias Dict(k, v) = Map(k, v)";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseTypeDecl();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node is a type alias declaration
        try testing.expect(node.* == .type_alias);

        const type_alias = node.type_alias;

        // Verify the name of the type alias is "Dict"
        try testing.expectEqualStrings("Dict", type_alias.name.identifier);

        // Verify the type alias has exactly two type parameters
        try testing.expectEqual(@as(usize, 2), type_alias.type_params.items.len);

        // Verify the type alias type parameters are: "k" and "v"
        try testing.expectEqualStrings("k", type_alias.type_params.items[0]);
        try testing.expectEqualStrings("v", type_alias.type_params.items[1]);

        // Check type application
        try testing.expect(type_alias.value.* == .type_application);

        const app = type_alias.value.type_application;

        // Verify the base type of the type application is "Map"
        try testing.expectEqualStrings("Map", app.constructor.identifier);

        // Verify the type application has exactly two arguments
        try testing.expectEqual(@as(usize, 2), app.args.items.len);

        // Verify the first argument is a lower identifier with the name "k"
        const arg1 = app.args.items[0];
        try testing.expect(arg1.* == .lower_identifier);
        try testing.expectEqualStrings("k", arg1.lower_identifier.identifier);

        // Verify the second argument is a lower identifier with the name "v"
        const arg2 = app.args.items[1];
        try testing.expect(arg2.* == .lower_identifier);
        try testing.expectEqualStrings("v", arg2.lower_identifier.identifier);
    }

    {
        // Function type
        const source = "type alias Reducer(a, b) = (a, b) -> b";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseTypeDecl();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify that the parsed node represents a type alias declaration
        try testing.expect(node.* == .type_alias);

        const type_alias = node.type_alias;

        // Verify the type alias name
        try testing.expectEqualStrings("Reducer", type_alias.name.identifier);

        // Verify type parameters
        try testing.expectEqual(@as(usize, 2), type_alias.type_params.items.len);
        try testing.expectEqualStrings("a", type_alias.type_params.items[0]);
        try testing.expectEqualStrings("b", type_alias.type_params.items[1]);

        // Ensure the type alias represents a function type
        try testing.expect(type_alias.value.* == .function_signature);

        const func_sig = type_alias.value.function_signature;

        // The function type should have two parameter types: 'a' and 'b'
        try testing.expectEqual(@as(usize, 2), func_sig.parameter_types.items.len);

        // Ensure the first parameter type is 'a'
        const sig_type1 = func_sig.parameter_types.items[0];
        try testing.expect(sig_type1.* == .lower_identifier);
        try testing.expectEqualStrings("a", sig_type1.lower_identifier.identifier);

        // Ensure the second parameter type is 'b'
        const sig_type2 = func_sig.parameter_types.items[1];
        try testing.expect(sig_type2.* == .lower_identifier);
        try testing.expectEqualStrings("b", sig_type2.lower_identifier.identifier);

        // Ensure the return type is 'b'
        const return_type = func_sig.return_type;
        try testing.expect(return_type.* == .lower_identifier);
        try testing.expectEqualStrings("b", return_type.lower_identifier.identifier);
    }

    {
        // Complex nested type
        const source = "type alias TreeMap(k, v) = Tree(Pair(k, v), Compare(k))";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseTypeDecl();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify that the parsed node represents a type alias declaration
        try testing.expect(node.* == .type_alias);

        const type_alias = node.type_alias;

        // Verify the name and type parameters of the alias
        try testing.expectEqualStrings("TreeMap", type_alias.name.identifier);
        try testing.expectEqual(@as(usize, 2), type_alias.type_params.items.len);
        try testing.expectEqualStrings("k", type_alias.type_params.items[0]);
        try testing.expectEqualStrings("v", type_alias.type_params.items[1]);

        // Ensure the alias represents a type application (Tree with arguments)
        try testing.expect(type_alias.value.* == .type_application);

        const tree_app = type_alias.value.type_application;

        // Validate that the base type is "Tree"
        try testing.expectEqual(lexer.TokenKind{ .identifier = .Upper }, tree_app.constructor.token.kind);
        try testing.expectEqualStrings("Tree", tree_app.constructor.identifier);

        // Ensure 'Tree' has exactly two type arguments: Pair(k, v) and Compare(k)
        try testing.expectEqual(@as(usize, 2), tree_app.args.items.len);

        // Check first argument "Pair(k, v)"
        {
            const pair_arg = tree_app.args.items[0];

            // Ensure it's a type application (Pair applied to k and v)
            try testing.expect(pair_arg.* == .type_application);

            const pair_app = pair_arg.type_application;

            // Verify 'Pair' as the base type
            try testing.expectEqual(lexer.TokenKind{ .identifier = .Upper }, pair_app.constructor.token.kind);
            try testing.expectEqualStrings("Pair", pair_app.constructor.identifier);

            // Ensure 'Pair' takes two arguments: 'k' and 'v'
            try testing.expectEqual(@as(usize, 2), pair_app.args.items.len);

            // Validate first argument of Pair: 'k'
            try testing.expect(pair_app.args.items[0].* == .lower_identifier);
            try testing.expectEqualStrings("k", pair_app.args.items[0].lower_identifier.identifier);

            // Validate second argument of Pair: 'v'
            try testing.expect(pair_app.args.items[1].* == .lower_identifier);
            try testing.expectEqualStrings("v", pair_app.args.items[1].lower_identifier.identifier);
        }

        // Check second argument "Compare(k)"
        {
            const compare_arg = tree_app.args.items[1];

            // Ensure it's a type application (Compare applied to k)
            try testing.expect(compare_arg.* == .type_application);

            const compare_app = compare_arg.type_application;

            // Verify "Compare" as the base type
            try testing.expectEqual(lexer.TokenKind{ .identifier = .Upper }, compare_app.constructor.token.kind);
            try testing.expectEqualStrings("Compare", compare_app.constructor.identifier);

            // Ensure "Compare" takes exactly one argument: 'k'
            try testing.expectEqual(@as(usize, 1), compare_app.args.items.len);

            // Validate argument of Compare: 'k'
            try testing.expect(compare_app.args.items[0].* == .lower_identifier);
            try testing.expectEqualStrings("k", compare_app.args.items[0].lower_identifier.identifier);
        }
    }
}

test "[record_type]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        const source = "type User = { name : String, age : Int }";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseTypeDecl();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Ensure the parsed node represents a record type
        try testing.expect(node.* == .record_type);

        const record = node.record_type;

        // Validate the name of the record type
        try testing.expectEqualStrings("User", record.name.identifier);

        // Ensure the record has no type parameters
        try testing.expectEqual(@as(usize, 0), record.type_params.items.len);

        // Verify the record has exactly two fields
        try testing.expectEqual(@as(usize, 2), record.fields.items.len);

        // Check first field (name)
        {
            const name_field = record.fields.items[0];

            // Ensure the field name is 'name'
            try testing.expectEqualStrings("name", name_field.name.identifier);

            // Ensure the field type is 'String'
            try testing.expect(name_field.type.* == .upper_identifier);
            try testing.expectEqualStrings("String", name_field.type.upper_identifier.identifier);
        }

        // Check second field (age)
        {
            const age_field = record.fields.items[1];

            // Ensure the field name is 'age'
            try testing.expectEqualStrings("age", age_field.name.identifier);

            // Ensure the field type is 'Int'
            try testing.expect(age_field.type.* == .upper_identifier);
            try testing.expectEqualStrings("Int", age_field.type.upper_identifier.identifier);
        }
    }

    {
        const source = "type Point(a) = { x : a, y : a }";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseTypeDecl();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Ensure the parsed node represents a record type
        try testing.expect(node.* == .record_type);

        const record = node.record_type;

        // Validate the name of the record type
        try testing.expectEqualStrings("Point", record.name.identifier);

        // Ensure the record has exactly one type parameter
        try testing.expectEqual(@as(usize, 1), record.type_params.items.len);
        try testing.expectEqualStrings("a", record.type_params.items[0]);

        // Verify the record has exactly two fields
        try testing.expectEqual(@as(usize, 2), record.fields.items.len);

        // Check first field (x)
        {
            const x_field = record.fields.items[0];

            // Ensure the field name is 'x'
            try testing.expectEqualStrings("x", x_field.name.identifier);

            // Ensure the field type is the generic type parameter 'a'
            try testing.expect(x_field.type.* == .lower_identifier);
            try testing.expectEqualStrings("a", x_field.type.lower_identifier.identifier);
        }

        // Check second field (y)
        {
            const y_field = record.fields.items[1];

            // Ensure the field name is 'y'
            try testing.expectEqualStrings("y", y_field.name.identifier);

            // Ensure the field type is the generic type parameter 'a'
            try testing.expect(y_field.type.* == .lower_identifier);
            try testing.expectEqualStrings("a", y_field.type.lower_identifier.identifier);
        }
    }

    {
        const source = "type Dict(k, v) = { keys : List(k), values : List(v) }";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseTypeDecl();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Ensure the parsed node represents a record type
        try testing.expect(node.* == .record_type);

        const record = node.record_type;

        // Validate the name of the record type
        try testing.expectEqualStrings("Dict", record.name.identifier);

        // Ensure the record has exactly two type parameters
        try testing.expectEqual(@as(usize, 2), record.type_params.items.len);
        try testing.expectEqualStrings("k", record.type_params.items[0]);
        try testing.expectEqualStrings("v", record.type_params.items[1]);

        // Verify the record has exactly two fields
        try testing.expectEqual(@as(usize, 2), record.fields.items.len);

        // Check first field (keys)
        {
            const keys_field = record.fields.items[0];

            // Ensure the field name is 'keys'
            try testing.expectEqualStrings("keys", keys_field.name.identifier);

            // Ensure the field type is a type application (List k)
            try testing.expect(keys_field.type.* == .type_application);

            const app = keys_field.type.type_application;

            // Ensure the base type is 'List'
            try testing.expectEqual(lexer.TokenKind{ .identifier = .Upper }, app.constructor.token.kind);
            try testing.expectEqualStrings("List", app.constructor.identifier);

            // Ensure 'List' has exactly one type argument: 'k'
            try testing.expectEqual(@as(usize, 1), app.args.items.len);
            try testing.expect(app.args.items[0].* == .lower_identifier);
            try testing.expectEqualStrings("k", app.args.items[0].lower_identifier.identifier);
        }

        // Check second field (values)
        {
            const values_field = record.fields.items[1];

            // Ensure the field name is 'values'
            try testing.expectEqualStrings("values", values_field.name.identifier);

            // Ensure the field type is a type application (List v)
            try testing.expect(values_field.type.* == .type_application);

            const app = values_field.type.type_application;

            // Ensure the base type is 'List'
            try testing.expectEqual(lexer.TokenKind{ .identifier = .Upper }, app.constructor.token.kind);
            try testing.expectEqualStrings("List", app.constructor.identifier);

            // Ensure 'List' has exactly one type argument: 'v'
            try testing.expectEqual(@as(usize, 1), app.args.items.len);
            try testing.expect(app.args.items[0].* == .lower_identifier);
            try testing.expectEqualStrings("v", app.args.items[0].lower_identifier.identifier);
        }
    }

    {
        const source = "type Storage(a) = { items : List(Maybe(a)), count : Maybe(Int), names : List(String) }";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseTypeDecl();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Ensure the parsed node is a record type
        try testing.expect(node.* == .record_type);

        const record = node.record_type;

        // Validate the name of the record type
        try testing.expectEqualStrings("Storage", record.name.identifier);

        // Ensure 'Storage' has one type parameter: 'a'
        try testing.expectEqual(@as(usize, 1), record.type_params.items.len);
        try testing.expectEqualStrings("a", record.type_params.items[0]);

        // Ensure the record has exactly three fields
        try testing.expectEqual(@as(usize, 3), record.fields.items.len);

        // Check first field (items: List(Maybe(a))
        {
            const items_field = record.fields.items[0];

            // Ensure the field name is 'items'
            try testing.expectEqualStrings("items", items_field.name.identifier);

            // Ensure the field type is a type application (List (Maybe a))
            try testing.expect(items_field.type.* == .type_application);

            const list_app = items_field.type.type_application;

            // Ensure the base type is 'List'
            try testing.expectEqualStrings("List", list_app.constructor.identifier);

            // Ensure List has exactly one type argument
            try testing.expectEqual(@as(usize, 1), list_app.args.items.len);

            // Ensure the argument is another type application (Maybe a)
            try testing.expect(list_app.args.items[0].* == .type_application);

            const maybe_app = list_app.args.items[0].type_application;

            // Ensure the base type is 'Maybe'
            try testing.expectEqualStrings("Maybe", maybe_app.constructor.identifier);

            // Ensure Maybe has one type argument: 'a'
            try testing.expectEqual(@as(usize, 1), maybe_app.args.items.len);
            try testing.expect(maybe_app.args.items[0].* == .lower_identifier);
            try testing.expectEqualStrings("a", maybe_app.args.items[0].lower_identifier.identifier);
        }

        // Check second field (count: Maybe(Int))
        {
            const count_field = record.fields.items[1];

            // Ensure the field name is 'count'
            try testing.expectEqualStrings("count", count_field.name.identifier);

            // Ensure the field type is a type application (Maybe Int)
            try testing.expect(count_field.type.* == .type_application);

            const maybe_app = count_field.type.type_application;

            // Ensure the base type is 'Maybe'
            try testing.expectEqual(lexer.TokenKind{ .identifier = .Upper }, maybe_app.constructor.token.kind);
            try testing.expectEqualStrings("Maybe", maybe_app.constructor.identifier);

            // Ensure Maybe has exactly one type argument: 'Int'
            try testing.expectEqual(@as(usize, 1), maybe_app.args.items.len);
            try testing.expect(maybe_app.args.items[0].* == .upper_identifier);
            try testing.expectEqualStrings("Int", maybe_app.args.items[0].upper_identifier.identifier);
        }

        // Check third field (names: List(String))
        {
            const names_field = record.fields.items[2];

            // Ensure the field name is 'names'
            try testing.expectEqualStrings("names", names_field.name.identifier);

            // Ensure the field type is a type application (List String)
            try testing.expect(names_field.type.* == .type_application);

            const list_app = names_field.type.type_application;

            // Ensure the base type is 'List'
            try testing.expectEqual(lexer.TokenKind{ .identifier = .Upper }, list_app.constructor.token.kind);
            try testing.expectEqualStrings("List", list_app.constructor.identifier);

            // Ensure List has exactly one type argument: 'String'
            try testing.expectEqual(@as(usize, 1), list_app.args.items.len);
            try testing.expect(list_app.args.items[0].* == .upper_identifier);
            try testing.expectEqualStrings("String", list_app.args.items[0].upper_identifier.identifier);
        }
    }
}

test "[variant_type]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        const source = "type Boolean = True | False";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseTypeDecl();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Ensure the parsed node is a variant type
        try testing.expect(node.* == .variant_type);

        const variant = node.variant_type;

        // Validate the name of the variant type
        try testing.expectEqualStrings("Boolean", variant.name.identifier);

        // Ensure 'Boolean' has no type parameters
        try testing.expectEqual(@as(usize, 0), variant.type_params.items.len);

        // Ensure the variant type has exactly two constructors (True, False)
        try testing.expectEqual(@as(usize, 2), variant.constructors.items.len);

        // Check first constructor (True)
        {
            const true_constructor = variant.constructors.items[0];

            // Ensure the constructor is named 'True'
            try testing.expectEqualStrings("True", true_constructor.name.identifier);

            // Ensure 'True' has no associated parameters
            try testing.expectEqual(@as(usize, 0), true_constructor.parameters.items.len);
        }

        // Check second constructor (False)
        {
            const false_constructor = variant.constructors.items[1];

            // Ensure the constructor is named 'False'
            try testing.expectEqualStrings("False", false_constructor.name.identifier);

            // Ensure 'False' has no associated parameters
            try testing.expectEqual(@as(usize, 0), false_constructor.parameters.items.len);
        }
    }

    {
        const source = "type Maybe(a) = | None | Some(a)";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseTypeDecl();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Ensure the parsed node is a variant type
        try testing.expect(node.* == .variant_type);

        const variant = node.variant_type;

        // Validate the name of the variant type
        try testing.expectEqualStrings("Maybe", variant.name.identifier);

        // Ensure 'Maybe' has a single type parameter 'a'
        try testing.expectEqual(@as(usize, 1), variant.type_params.items.len);
        try testing.expectEqualStrings("a", variant.type_params.items[0]);

        // Ensure the variant type has exactly two constructors (None, Some)
        try testing.expectEqual(@as(usize, 2), variant.constructors.items.len);

        // Check first constructor (None)
        {
            const none_constructor = variant.constructors.items[0];

            // Ensure the constructor is named 'None'
            try testing.expectEqualStrings("None", none_constructor.name.identifier);

            // Ensure 'None' has no associated parameters
            try testing.expectEqual(@as(usize, 0), none_constructor.parameters.items.len);
        }

        // Check second constructor (Some a)
        {
            const some_constructor = variant.constructors.items[1];

            // Ensure the constructor is named 'Some'
            try testing.expectEqualStrings("Some", some_constructor.name.identifier);

            // Ensure 'Some' has exactly one associated parameter
            try testing.expectEqual(@as(usize, 1), some_constructor.parameters.items.len);

            const param = some_constructor.parameters.items[0];

            // Validate that the parameter is the type variable 'a'
            try testing.expect(param.* == .lower_identifier);
            try testing.expectEqualStrings("a", param.lower_identifier.identifier);
        }
    }

    {
        const source = "type Tree(a) = | Leaf | Branch(Tree(a), a, Tree(a))";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseTypeDecl();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Ensure the parsed node is a variant type
        try testing.expect(node.* == .variant_type);

        const variant = node.variant_type;

        // Validate the name of the variant type
        try testing.expectEqualStrings("Tree", variant.name.identifier);

        // Ensure 'Tree' has a single type parameter 'a'
        try testing.expectEqual(@as(usize, 1), variant.type_params.items.len);
        try testing.expectEqualStrings("a", variant.type_params.items[0]);

        // Ensure the variant type has exactly two constructors (Leaf, Branch)
        try testing.expectEqual(@as(usize, 2), variant.constructors.items.len);

        // Check first constructor (Leaf)
        {
            const leaf_constructor = variant.constructors.items[0];

            // Ensure the constructor is named 'Leaf'
            try testing.expectEqualStrings("Leaf", leaf_constructor.name.identifier);

            // Ensure 'Leaf' has no associated parameters
            try testing.expectEqual(@as(usize, 0), leaf_constructor.parameters.items.len);
        }

        // Check second constructor (Branch (Tree a) a (Tree a))
        {
            const branch_constructor = variant.constructors.items[1];

            // Ensure the constructor is named 'Branch'
            try testing.expectEqualStrings("Branch", branch_constructor.name.identifier);

            // Ensure 'Branch' has exactly three parameters
            try testing.expectEqual(@as(usize, 3), branch_constructor.parameters.items.len);

            // First parameter (Tree a)
            {
                const param1 = branch_constructor.parameters.items[0];

                // Ensure 'param1' is a type application
                try testing.expect(param1.* == .type_application);

                const app = param1.type_application;

                // Ensure the base type is 'Tree'
                try testing.expectEqual(lexer.TokenKind{ .identifier = .Upper }, app.constructor.token.kind);
                try testing.expectEqualStrings("Tree", app.constructor.identifier);

                // Ensure 'Tree' is applied to one argument ('a')
                try testing.expectEqual(@as(usize, 1), app.args.items.len);
                try testing.expect(app.args.items[0].* == .lower_identifier);
                try testing.expectEqualStrings("a", app.args.items[0].lower_identifier.identifier);
            }

            // Second parameter (a)
            {
                const param2 = branch_constructor.parameters.items[1];

                // Ensure 'param2' is the type variable 'a'
                try testing.expect(param2.* == .lower_identifier);
                try testing.expectEqualStrings("a", param2.lower_identifier.identifier);
            }

            // Third parameter (Tree a)
            {
                const param3 = branch_constructor.parameters.items[2];

                // Ensure 'param3' is a type application
                try testing.expect(param3.* == .type_application);

                const app = param3.type_application;

                // Ensure the base type is 'Tree'
                try testing.expectEqual(lexer.TokenKind{ .identifier = .Upper }, app.constructor.token.kind);
                try testing.expectEqualStrings("Tree", app.constructor.identifier);

                // Ensure 'Tree' is applied to one argument ('a')
                try testing.expectEqual(@as(usize, 1), app.args.items.len);
                try testing.expect(app.args.items[0].* == .lower_identifier);
                try testing.expectEqualStrings("a", app.args.items[0].lower_identifier.identifier);
            }
        }
    }
}

test "[list]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        const source = "[]";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseList();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify that the node type is a list.
        try testing.expect(node.* == .list);

        const list = node.list;

        // Ensure the list has no elements.
        try testing.expectEqual(@as(usize, 0), list.elements.items.len);
    }

    {
        const source = "[1, 2, 3]";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseList();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify that the node type is a list.
        try testing.expect(node.* == .list);

        const list = node.list;

        // Ensure the list contains exactly 3 elements.
        try testing.expectEqual(@as(usize, 3), list.elements.items.len);

        // Test first element (1)
        try testing.expect(list.elements.items[0].* == .int_literal);
        try testing.expectEqual(@as(i64, 1), list.elements.items[0].int_literal.value);

        // Test second element (2)
        try testing.expect(list.elements.items[1].* == .int_literal);
        try testing.expectEqual(@as(i64, 2), list.elements.items[1].int_literal.value);

        // Test third element (3)
        try testing.expect(list.elements.items[2].* == .int_literal);
        try testing.expectEqual(@as(i64, 3), list.elements.items[2].int_literal.value);
    }
}

test "[tuple]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        const source = "(1, \"hello\")";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseTuple();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify that the node type is a tuple.
        try testing.expect(node.* == .tuple);

        const tuple = node.tuple;

        // Ensure the list has exactly two elements.
        try testing.expectEqual(@as(usize, 2), tuple.elements.items.len);
        try testing.expect(tuple.elements.items[0].int_literal.value == 1);
        try testing.expectEqualStrings("hello", tuple.elements.items[1].str_literal.value);
    }

    {
        const source = "(x, True)";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseTuple();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify that the node type is a tuple.
        try testing.expect(node.* == .tuple);

        const tuple = node.tuple;

        // Ensure the list has exactly two elements.
        try testing.expectEqual(@as(usize, 2), tuple.elements.items.len);
        try testing.expectEqualStrings("x", tuple.elements.items[0].lower_identifier.identifier);
        try testing.expectEqualStrings("True", tuple.elements.items[1].upper_identifier.identifier);
    }
}

test "[export_spec]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        const source = "exposing (double)";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseExportSpec();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify that the node type is an export spec
        try testing.expect(node.* == .export_spec);

        const export_spec = node.export_spec;

        // Verify that we are not exposing all items
        try testing.expect(!export_spec.exposing_all);

        // Verify that the export spec has exactly one item
        try testing.expectEqual(@as(usize, 1), export_spec.items.?.items.len);

        // Check the exported item (double)
        const item = export_spec.items.?.items[0];
        try testing.expectEqualStrings("double", item.name);
        try testing.expect(!item.expose_constructors);
    }

    {
        const source = "exposing (..)";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseExportSpec();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify that the node type is an export spec
        try testing.expect(node.* == .export_spec);

        const export_spec = node.export_spec;

        // Verify that we are exposing all items
        try testing.expect(export_spec.exposing_all);

        // Verify that the export spec has no specific items
        try testing.expect(export_spec.items == null);
    }

    {
        const source = "exposing (Maybe(..), and_then)";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseExportSpec();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify that the node type is an export spec
        try testing.expect(node.* == .export_spec);

        const export_spec = node.export_spec;

        // Verify that we are not exposing all items
        try testing.expect(!export_spec.exposing_all);

        // Verify that the export spec has exactly two items
        try testing.expectEqual(@as(usize, 2), export_spec.items.?.items.len);

        // Check the first exported item (Maybe with constructors)
        const type_item = export_spec.items.?.items[0];
        try testing.expectEqualStrings("Maybe", type_item.name);
        try testing.expect(type_item.expose_constructors);

        // Check the second exported item (and_then function)
        const func_item = export_spec.items.?.items[1];
        try testing.expectEqualStrings("and_then", func_item.name);
        try testing.expect(!func_item.expose_constructors);
    }

    {
        const source = "exposing (Result, map, flatMap, withDefault)";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseExportSpec();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify that the node type is an export spec
        try testing.expect(node.* == .export_spec);

        const export_spec = node.export_spec;

        // Verify that we are not exposing all items
        try testing.expect(!export_spec.exposing_all);

        // Verify that the export spec has exactly four items
        try testing.expectEqual(@as(usize, 4), export_spec.items.?.items.len);

        // Check the first exported item (Result type without constructors)
        try testing.expectEqualStrings("Result", export_spec.items.?.items[0].name);
        try testing.expect(!export_spec.items.?.items[0].expose_constructors);

        // Check the remaining function exports
        try testing.expectEqualStrings("map", export_spec.items.?.items[1].name);
        try testing.expectEqualStrings("flatMap", export_spec.items.?.items[2].name);
        try testing.expectEqualStrings("withDefault", export_spec.items.?.items[3].name);
    }
}

test "[module_decl]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        const source = "module Foo exposing (double)\n" ++ "let double(x : Int) -> Int = x * 2\n";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseModuleDecl();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify that the node type is a module decl
        try testing.expect(node.* == .module_decl);

        const module = node.module_decl;

        // Verify module path is 'Foo'
        try testing.expectEqualStrings("Foo", module.path.segments.items[0].identifier);

        // Verify exports is only 'double'
        try testing.expectEqual(@as(usize, 1), module.exports.items.?.items.len);
        try testing.expectEqualStrings("double", module.exports.items.?.items[0].name);

        // Verify declaration is only 'double'
        try testing.expectEqual(@as(usize, 1), module.declarations.items.len);
        try testing.expectEqualStrings("double", module.declarations.items[0].function_decl.name.identifier);
    }
}

test "[program]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        const source = "## some doc comment\n" ++ "module Foo exposing (double)\n" ++ "let double(x : Int) -> Int = x * 2\n";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseProgram();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify that the node type is a program.
        try testing.expect(node.* == .program);

        const program = node.program;

        // Verify programs has exactly two statements
        try testing.expectEqual(@as(usize, 2), program.statements.items.len);
        try testing.expect(program.statements.items[0].* == .doc_comment);
        try testing.expect(program.statements.items[1].* == .module_decl);
    }
}

test "[guard_node]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        // Simple guard condition
        const source = "when x > 0";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const guard = try parser.parseGuard();
        defer guard.release(allocator);

        // Assertions
        try testing.expect(guard.condition.* == .comparison_expr);
        try testing.expectEqual(lexer.TokenKind{ .operator = .GreaterThan }, guard.condition.comparison_expr.operator.kind);

        const left = guard.condition.comparison_expr.left;
        try testing.expect(left.* == .lower_identifier);
        try testing.expectEqualStrings("x", left.lower_identifier.identifier);

        const right = guard.condition.comparison_expr.right;
        try testing.expect(right.* == .int_literal);
        try testing.expectEqual(@as(i64, 0), right.int_literal.value);
    }

    {
        // Function call in guard
        const source = "when is_valid?(name)";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const guard = try parser.parseGuard();
        defer guard.release(allocator);

        // Assertions
        try testing.expect(guard.condition.* == .function_call);
        try testing.expectEqualStrings("is_valid?", guard.condition.function_call.function.lower_identifier.identifier);

        try testing.expectEqual(@as(usize, 1), guard.condition.function_call.arguments.items.len);
        try testing.expectEqualStrings("name", guard.condition.function_call.arguments.items[0].lower_identifier.identifier);
    }
}

test "[pattern_nodes]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        // Literal pattern test
        const source = "0";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const pattern = try parser.parsePattern();
        defer pattern.release(allocator);

        // Assertions
        try testing.expect(pattern.* == .literal_pattern);
        try testing.expect(pattern.literal_pattern.value.* == .int_literal);
        try testing.expectEqual(@as(i64, 0), pattern.literal_pattern.value.int_literal.value);
    }

    {
        // Variable pattern test
        const source = "x";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const pattern = try parser.parsePattern();
        defer pattern.release(allocator);

        // Assertions
        try testing.expect(pattern.* == .variable_pattern);
        try testing.expectEqualStrings("x", pattern.variable_pattern.name);
    }

    {
        // Wildcard pattern test
        const source = "_";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const pattern = try parser.parsePattern();
        defer pattern.release(allocator);

        // Assertions
        try testing.expect(pattern.* == .wildcard_pattern);
    }

    {
        // Cons pattern test
        const source = "x :: xs";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const pattern = try parser.parsePattern();
        defer pattern.release(allocator);

        // Assertions
        try testing.expect(pattern.* == .cons_pattern);

        // Check head is variable x
        try testing.expect(pattern.cons_pattern.head.* == .variable_pattern);
        try testing.expectEqualStrings("x", pattern.cons_pattern.head.variable_pattern.name);

        // Check tail is variable xs
        try testing.expect(pattern.cons_pattern.tail.* == .variable_pattern);
        try testing.expectEqualStrings("xs", pattern.cons_pattern.tail.variable_pattern.name);
    }

    {
        // List pattern test
        const source = "[1, x, _]";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const pattern = try parser.parsePattern();
        defer pattern.release(allocator);

        // Assertions
        try testing.expect(pattern.* == .list_pattern);
        try testing.expectEqual(@as(usize, 3), pattern.list_pattern.elements.items.len);

        // Check first element is literal 1
        try testing.expect(pattern.list_pattern.elements.items[0].* == .literal_pattern);
        try testing.expectEqual(@as(i64, 1), pattern.list_pattern.elements.items[0].literal_pattern.value.int_literal.value);

        // Check second element is variable x
        try testing.expect(pattern.list_pattern.elements.items[1].* == .variable_pattern);
        try testing.expectEqualStrings("x", pattern.list_pattern.elements.items[1].variable_pattern.name);

        // Check third element is wildcard
        try testing.expect(pattern.list_pattern.elements.items[2].* == .wildcard_pattern);
    }

    {
        // Constructor pattern test
        const source = "Some(x)";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const pattern = try parser.parsePattern();
        defer pattern.release(allocator);

        // Assertions
        try testing.expect(pattern.* == .constructor_pattern);
        try testing.expectEqualStrings("Some", pattern.constructor_pattern.name.identifier);
        try testing.expectEqual(@as(usize, 1), pattern.constructor_pattern.args.items.len);

        // Check argument is variable x
        try testing.expect(pattern.constructor_pattern.args.items[0].* == .variable_pattern);
        try testing.expectEqualStrings("x", pattern.constructor_pattern.args.items[0].variable_pattern.name);
    }
}

test "[match_expr]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        // Boolean variant type matching
        const source = "match x on\n| False => True\n| True => False";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseExpression();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node type is a match expression
        try testing.expect(node.* == .match_expr);

        const match_expr = node.match_expr;

        // Verify the subject is "x"
        try testing.expect(match_expr.subject.* == .lower_identifier);
        try testing.expectEqualStrings("x", match_expr.subject.lower_identifier.identifier);

        // Verify there are exactly two cases
        try testing.expectEqual(@as(usize, 2), match_expr.cases.items.len);

        // Verify first case (False => True)
        {
            const case = match_expr.cases.items[0];

            // Check pattern is a constructor pattern "False"
            try testing.expect(case.pattern.* == .constructor_pattern);
            try testing.expectEqualStrings("False", case.pattern.constructor_pattern.name.identifier);
            try testing.expectEqual(@as(usize, 0), case.pattern.constructor_pattern.args.items.len);

            // Check expression is constructor "True"
            try testing.expect(case.expression.* == .upper_identifier);
            try testing.expectEqualStrings("True", case.expression.upper_identifier.identifier);

            // Verify there's no guard
            try testing.expect(case.guard == null);
        }

        // Verify second case (True => False)
        {
            const case = match_expr.cases.items[1];

            // Check pattern is a constructor pattern "True"
            try testing.expect(case.pattern.* == .constructor_pattern);
            try testing.expectEqualStrings("True", case.pattern.constructor_pattern.name.identifier);
            try testing.expectEqual(@as(usize, 0), case.pattern.constructor_pattern.args.items.len);

            // Check expression is constructor "False"
            try testing.expect(case.expression.* == .upper_identifier);
            try testing.expectEqualStrings("False", case.expression.upper_identifier.identifier);

            // Verify there's no guard
            try testing.expect(case.guard == null);
        }
    }

    {
        // List pattern matching with cons and Nil
        const source = "match list on | Nil => 0 | x :: xs => 1 + length(xs)";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseExpression();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node type is a match expression
        try testing.expect(node.* == .match_expr);

        const match_expr = node.match_expr;

        // Verify the subject is "list"
        try testing.expect(match_expr.subject.* == .lower_identifier);
        try testing.expectEqualStrings("list", match_expr.subject.lower_identifier.identifier);

        // Verify there are exactly two cases
        try testing.expectEqual(@as(usize, 2), match_expr.cases.items.len);

        // Verify first case (Nil => 0)
        {
            const case = match_expr.cases.items[0];

            // Check pattern is a constructor pattern "Nil"
            try testing.expect(case.pattern.* == .constructor_pattern);
            try testing.expectEqualStrings("Nil", case.pattern.constructor_pattern.name.identifier);
            try testing.expectEqual(@as(usize, 0), case.pattern.constructor_pattern.args.items.len);

            // Check expression is an integer literal 0
            try testing.expect(case.expression.* == .int_literal);
            try testing.expectEqual(@as(i64, 0), case.expression.int_literal.value);

            // Verify there's no guard
            try testing.expect(case.guard == null);
        }

        // Verify second case (x :: xs => 1 + length(xs))
        {
            const case = match_expr.cases.items[1];

            // Check pattern is a cons pattern
            try testing.expect(case.pattern.* == .cons_pattern);

            // Check head is variable x
            try testing.expect(case.pattern.cons_pattern.head.* == .variable_pattern);
            try testing.expectEqualStrings("x", case.pattern.cons_pattern.head.variable_pattern.name);

            // Check tail is variable xs
            try testing.expect(case.pattern.cons_pattern.tail.* == .variable_pattern);
            try testing.expectEqualStrings("xs", case.pattern.cons_pattern.tail.variable_pattern.name);

            // Check expression is an arithmetic expression (1 + length(xs))
            try testing.expect(case.expression.* == .arithmetic_expr);

            // Verify there's no guard
            try testing.expect(case.guard == null);
        }
    }

    {
        // Match with guard conditions
        const source = "match n on | x when x > 0 => \"positive\" | x when x < 0 => \"negative\" | _ => \"zero\"";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseExpression();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node type is a match expression
        try testing.expect(node.* == .match_expr);

        const match_expr = node.match_expr;

        // Verify the subject is "n"
        try testing.expect(match_expr.subject.* == .lower_identifier);
        try testing.expectEqualStrings("n", match_expr.subject.lower_identifier.identifier);

        // Verify there are exactly three cases
        try testing.expectEqual(@as(usize, 3), match_expr.cases.items.len);

        // Verify first case (x when x > 0 => "positive")
        {
            const case = match_expr.cases.items[0];

            // Check pattern is a variable pattern "x"
            try testing.expect(case.pattern.* == .variable_pattern);
            try testing.expectEqualStrings("x", case.pattern.variable_pattern.name);

            // Check expression is a string literal "positive"
            try testing.expect(case.expression.* == .str_literal);
            try testing.expectEqualStrings("positive", case.expression.str_literal.value);

            // Verify there's a guard
            try testing.expect(case.guard != null);

            // Check guard condition is "x > 0"
            try testing.expect(case.guard.?.condition.* == .comparison_expr);
            try testing.expectEqual(lexer.TokenKind{ .operator = .GreaterThan }, case.guard.?.condition.comparison_expr.operator.kind);
        }

        // Verify second case (x when x < 0 => "negative")
        {
            const case = match_expr.cases.items[1];

            // Check pattern is a variable pattern "x"
            try testing.expect(case.pattern.* == .variable_pattern);
            try testing.expectEqualStrings("x", case.pattern.variable_pattern.name);

            // Check expression is a string literal "negative"
            try testing.expect(case.expression.* == .str_literal);
            try testing.expectEqualStrings("negative", case.expression.str_literal.value);

            // Verify there's a guard
            try testing.expect(case.guard != null);

            // Check guard condition is "x < 0"
            try testing.expect(case.guard.?.condition.* == .comparison_expr);
            try testing.expectEqual(lexer.TokenKind{ .operator = .LessThan }, case.guard.?.condition.comparison_expr.operator.kind);
        }

        // Verify third case (_ => "zero")
        {
            const case = match_expr.cases.items[2];

            // Check pattern is a wildcard
            try testing.expect(case.pattern.* == .wildcard_pattern);

            // Check expression is a string literal "zero"
            try testing.expect(case.expression.* == .str_literal);
            try testing.expectEqualStrings("zero", case.expression.str_literal.value);

            // Verify there's no guard
            try testing.expect(case.guard == null);
        }
    }

    {
        // Match with constructor patterns
        const source = "match result on | Ok(value) => value | Err(msg) => default_value";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseExpression();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node type is a match expression
        try testing.expect(node.* == .match_expr);

        const match_expr = node.match_expr;

        // Verify the subject is "result"
        try testing.expect(match_expr.subject.* == .lower_identifier);
        try testing.expectEqualStrings("result", match_expr.subject.lower_identifier.identifier);

        // Verify there are exactly two cases
        try testing.expectEqual(@as(usize, 2), match_expr.cases.items.len);

        // Verify first case (Ok(value) => value)
        {
            const case = match_expr.cases.items[0];

            // Check pattern is a constructor pattern
            try testing.expect(case.pattern.* == .constructor_pattern);
            try testing.expectEqualStrings("Ok", case.pattern.constructor_pattern.name.identifier);

            // Check it has one argument
            try testing.expectEqual(@as(usize, 1), case.pattern.constructor_pattern.args.items.len);

            // Check the argument is a variable pattern "value"
            try testing.expect(case.pattern.constructor_pattern.args.items[0].* == .variable_pattern);
            try testing.expectEqualStrings("value", case.pattern.constructor_pattern.args.items[0].variable_pattern.name);

            // Check expression is a variable "value"
            try testing.expect(case.expression.* == .lower_identifier);
            try testing.expectEqualStrings("value", case.expression.lower_identifier.identifier);

            // Verify there's no guard
            try testing.expect(case.guard == null);
        }

        // Verify second case (Err(msg) => default_value)
        {
            const case = match_expr.cases.items[1];

            // Check pattern is a constructor pattern
            try testing.expect(case.pattern.* == .constructor_pattern);
            try testing.expectEqualStrings("Err", case.pattern.constructor_pattern.name.identifier);

            // Check it has one argument
            try testing.expectEqual(@as(usize, 1), case.pattern.constructor_pattern.args.items.len);

            // Check the argument is a variable pattern "msg"
            try testing.expect(case.pattern.constructor_pattern.args.items[0].* == .variable_pattern);
            try testing.expectEqualStrings("msg", case.pattern.constructor_pattern.args.items[0].variable_pattern.name);

            // Check expression is a variable "default_value"
            try testing.expect(case.expression.* == .lower_identifier);
            try testing.expectEqualStrings("default_value", case.expression.lower_identifier.identifier);

            // Verify there's no guard
            try testing.expect(case.guard == null);
        }
    }

    {
        // Tuple pattern matching
        const source = "match (x, y) on\n| (True, True) => True\n| _ => False";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseExpression();
        defer {
            node.release(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node type is a match expression
        try testing.expect(node.* == .match_expr);

        const match_expr = node.match_expr;

        // Verify the subject is a tuple (x, y)
        try testing.expect(match_expr.subject.* == .tuple);
        try testing.expectEqual(@as(usize, 2), match_expr.subject.tuple.elements.items.len);
        try testing.expectEqualStrings("x", match_expr.subject.tuple.elements.items[0].lower_identifier.identifier);
        try testing.expectEqualStrings("y", match_expr.subject.tuple.elements.items[1].lower_identifier.identifier);

        // Verify there are exactly two cases
        try testing.expectEqual(@as(usize, 2), match_expr.cases.items.len);

        // Verify first case ((True, True) => True)
        {
            const case = match_expr.cases.items[0];

            // Check pattern is a tuple pattern
            try testing.expect(case.pattern.* == .tuple_pattern);
            try testing.expectEqual(@as(usize, 2), case.pattern.tuple_pattern.elements.items.len);

            // Check first element is constructor True
            try testing.expect(case.pattern.tuple_pattern.elements.items[0].* == .constructor_pattern);
            try testing.expectEqualStrings("True", case.pattern.tuple_pattern.elements.items[0].constructor_pattern.name.identifier);
            try testing.expectEqual(@as(usize, 0), case.pattern.tuple_pattern.elements.items[0].constructor_pattern.args.items.len);

            // Check second element is constructor True
            try testing.expect(case.pattern.tuple_pattern.elements.items[1].* == .constructor_pattern);
            try testing.expectEqualStrings("True", case.pattern.tuple_pattern.elements.items[1].constructor_pattern.name.identifier);
            try testing.expectEqual(@as(usize, 0), case.pattern.tuple_pattern.elements.items[1].constructor_pattern.args.items.len);

            // Check expression is constructor "True"
            try testing.expect(case.expression.* == .upper_identifier);
            try testing.expectEqualStrings("True", case.expression.upper_identifier.identifier);

            // Verify there's no guard
            try testing.expect(case.guard == null);
        }

        // Verify second case (_ => False)
        {
            const case = match_expr.cases.items[1];

            // Check pattern is a wildcard
            try testing.expect(case.pattern.* == .wildcard_pattern);

            // Check expression is constructor "False"
            try testing.expect(case.expression.* == .upper_identifier);
            try testing.expectEqualStrings("False", case.expression.upper_identifier.identifier);

            // Verify there's no guard
            try testing.expect(case.guard == null);
        }
    }
}
