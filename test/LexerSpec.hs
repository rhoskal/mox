module LexerSpec (lexerSpec) where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as B
import Data.Text.Encoding qualified as TE
import Syntax.Lexer
import Syntax.Token
import Test.Hspec

data TestToken = TestToken Tok ByteString
  deriving (Eq, Show)

toTestToken :: Token -> TestToken
toTestToken (Token _ tok lexeme) =
  TestToken tok lexeme

lexer :: ByteString -> Either String [TestToken]
lexer =
  fmap (map toTestToken) . runLexer "dummy_file.mox"

lexerSpec :: Spec
lexerSpec = do
  describe "comments" $ do
    it "should tokenize line comment correctly" $ do
      let input = "# some comment"
          expected =
            Right
              [ TestToken TokComment input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize doc comment correctly" $ do
      let input = "## some comment"
          expected =
            Right
              [ TestToken TokDocComment input
              ]
          actual = lexer input
       in actual `shouldBe` expected

  describe "symbols" $ do
    it "should tokenize '->' correctly" $ do
      let input = "->"
          expected =
            Right
              [ TestToken (TokSymbol SymArrow) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize '@' correctly" $ do
      let input = "@"
          expected =
            Right
              [ TestToken (TokSymbol SymAs) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize '=>' correctly" $ do
      let input = "=>"
          expected =
            Right
              [ TestToken (TokSymbol SymCase) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize '{}' correctly" $ do
      let input = "{}"
          expected =
            Right
              [ TestToken (TokSymbol SymOpenBrace) "{",
                TestToken (TokSymbol SymCloseBrace) "}"
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize '[]' correctly" $ do
      let input = "[]"
          expected =
            Right
              [ TestToken (TokSymbol SymOpenBracket) "[",
                TestToken (TokSymbol SymCloseBracket) "]"
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize '()' correctly" $ do
      let input = "()"
          expected =
            Right
              [ TestToken (TokSymbol SymOpenParen) "(",
                TestToken (TokSymbol SymCloseParen) ")"
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize ':' correctly" $ do
      let input = ":"
          expected =
            Right
              [ TestToken (TokSymbol SymColon) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize ',' correctly" $ do
      let input = ","
          expected =
            Right
              [ TestToken (TokSymbol SymComma) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize '.' correctly" $ do
      let input = "."
          expected =
            Right
              [ TestToken (TokSymbol SymDot) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize '..' correctly" $ do
      let input = ".."
          expected =
            Right
              [ TestToken (TokSymbol SymDotDot) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize '=' correctly" $ do
      let input = "="
          expected =
            Right
              [ TestToken (TokSymbol SymEqual) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize '?' correctly" $ do
      let input = "?"
          expected =
            Right
              [ TestToken (TokSymbol SymQuestionMark) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize ';' correctly" $ do
      let input = ";"
          expected =
            Right
              [ TestToken (TokSymbol SymSemi) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize '_' correctly" $ do
      let input = "_"
          expected =
            Right
              [ TestToken (TokSymbol SymUnderscore) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize '|' correctly" $ do
      let input = "|"
          expected =
            Right
              [ TestToken (TokSymbol SymVerticalBar) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

  describe "keywords" $ do
    it "should tokenize 'as' correctly" $ do
      let input = "as"
          expected =
            Right
              [ TestToken (TokKeyword KwAs) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize 'data' correctly" $ do
      let input = "data"
          expected =
            Right
              [ TestToken (TokKeyword KwData) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize 'else' correctly" $ do
      let input = "else"
          expected =
            Right
              [ TestToken (TokKeyword KwElse) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize 'fn' correctly" $ do
      let input = "fn"
          expected =
            Right
              [ TestToken (TokKeyword KwLambda) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize 'foreign' correctly" $ do
      let input = "foreign"
          expected =
            Right
              [ TestToken (TokKeyword KwForeign) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize 'hiding' correctly" $ do
      let input = "hiding"
          expected =
            Right
              [ TestToken (TokKeyword KwHiding) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize 'if' correctly" $ do
      let input = "if"
          expected =
            Right
              [ TestToken (TokKeyword KwIf) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize 'in' correctly" $ do
      let input = "in"
          expected =
            Right
              [ TestToken (TokKeyword KwIn) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize 'include' correctly" $ do
      let input = "include"
          expected =
            Right
              [ TestToken (TokKeyword KwInclude) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize 'infix' correctly" $ do
      let input = "infix"
          expected =
            Right
              [ TestToken (TokKeyword KwInfix) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize 'infixL' correctly" $ do
      let input = "infixl"
          expected =
            Right
              [ TestToken (TokKeyword KwInfixL) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize 'infixR' correctly" $ do
      let input = "infixr"
          expected =
            Right
              [ TestToken (TokKeyword KwInfixR) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize 'let' correctly" $ do
      let input = "let"
          expected =
            Right
              [ TestToken (TokKeyword KwLet) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize 'match' correctly" $ do
      let input = "match"
          expected =
            Right
              [ TestToken (TokKeyword KwMatch) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize 'module' correctly" $ do
      let input = "module"
          expected =
            Right
              [ TestToken (TokKeyword KwModule) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize 'opaque' correctly" $ do
      let input = "opaque"
          expected =
            Right
              [ TestToken (TokKeyword KwOpaque) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize 'open' correctly" $ do
      let input = "open"
          expected =
            Right
              [ TestToken (TokKeyword KwOpen) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize 'record' correctly" $ do
      let input = "record"
          expected =
            Right
              [ TestToken (TokKeyword KwRecord) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize 'renaming' correctly" $ do
      let input = "renaming"
          expected =
            Right
              [ TestToken (TokKeyword KwRenaming) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize 'then' correctly" $ do
      let input = "then"
          expected =
            Right
              [ TestToken (TokKeyword KwThen) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize 'using' correctly" $ do
      let input = "using"
          expected =
            Right
              [ TestToken (TokKeyword KwUsing) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize 'when' correctly" $ do
      let input = "when"
          expected =
            Right
              [ TestToken (TokKeyword KwWhen) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize 'where' correctly" $ do
      let input = "where"
          expected =
            Right
              [ TestToken (TokKeyword KwWhere) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize 'with' correctly" $ do
      let input = "with"
          expected =
            Right
              [ TestToken (TokKeyword KwWith) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

  describe "identifiers" $ do
    it "should tokenize lower id correctly" $ do
      let input = "foo"
          expected =
            Right
              [ TestToken (TokId $ TE.decodeUtf8Lenient $ B.toStrict input) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize upper id correctly" $ do
      let input = "Foo"
          expected =
            Right
              [ TestToken (TokId $ TE.decodeUtf8Lenient $ B.toStrict input) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize id with final correctly" $ do
      let input = "foo'"
          expected =
            Right
              [ TestToken (TokId $ TE.decodeUtf8Lenient $ B.toStrict input) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize module id correctly" $ do
      let input = "Foo.Bar"
          expected =
            Right
              [ TestToken (TokId $ TE.decodeUtf8Lenient $ B.toStrict input) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

  describe "literals" $ do
    it "should tokenize positive hex int correctly" $ do
      let input = "0x4f"
          expected =
            Right
              [ TestToken (TokLiteral (LitInt 79)) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize negative hex int correctly" $ do
      let input = "-0x4f"
          expected =
            Right
              [ TestToken (TokLiteral (LitInt (-79))) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize hex int with separator correctly" $ do
      let input = "0x4f_ae"
          expected =
            Right
              [ TestToken (TokLiteral (LitInt 20398)) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize positive octal int correctly" $ do
      let input = "0o14"
          expected =
            Right
              [ TestToken (TokLiteral (LitInt 12)) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize negative octal int correctly" $ do
      let input = "-0o14"
          expected =
            Right
              [ TestToken (TokLiteral (LitInt (-12))) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize octal int with separator correctly" $ do
      let input = "0o14_17"
          expected =
            Right
              [ TestToken (TokLiteral (LitInt 783)) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize positive binary int correctly" $ do
      let input = "0b1010"
          expected =
            Right
              [ TestToken (TokLiteral (LitInt 10)) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize negative binary int correctly" $ do
      let input = "-0b1010"
          expected =
            Right
              [ TestToken (TokLiteral (LitInt (-10))) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize binary int with separator correctly" $ do
      let input = "0b1010_1010"
          expected =
            Right
              [ TestToken (TokLiteral (LitInt 170)) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize positive float correctly" $ do
      let input = "42.0"
          expected =
            Right
              [ TestToken (TokLiteral (LitFloat 42.0)) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize negative float correctly" $ do
      let input = "-42.0"
          expected =
            Right
              [ TestToken (TokLiteral (LitFloat (-42.0))) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize positive float with separator correctly" $ do
      let input = "42_000.0"
          expected =
            Right
              [ TestToken (TokLiteral (LitFloat 42000.0)) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize positive int correctly" $ do
      let input = "42"
          expected =
            Right
              [ TestToken (TokLiteral (LitInt 42)) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize negative int correctly" $ do
      let input = "-42"
          expected =
            Right
              [ TestToken (TokLiteral (LitInt (-42))) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize positive int with separator correctly" $ do
      let input = "42_000"
          expected =
            Right
              [ TestToken (TokLiteral (LitInt 42000)) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize char correctly" $ do
      let input = "'x'"
          expected =
            Right
              [ TestToken (TokLiteral (LitChar 'x')) input
              ]
          actual = lexer input
       in actual `shouldBe` expected

    it "should tokenize string correctly" $ do
      let bsInput = "\"foo\""
          txtInput = TE.decodeUtf8Lenient $ B.toStrict bsInput
          expected =
            Right
              [ TestToken (TokLiteral (LitString txtInput)) bsInput
              ]
          actual = lexer bsInput
       in actual `shouldBe` expected

    it "should tokenize multi string correctly" $ do
      let bsInput = "\"\"\"foo\"\"\""
          txtInput = TE.decodeUtf8Lenient $ B.toStrict bsInput
          expected =
            Right
              [ TestToken (TokLiteral (LitString txtInput)) bsInput
              ]
          actual = lexer bsInput
       in actual `shouldBe` expected
