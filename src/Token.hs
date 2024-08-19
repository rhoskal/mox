module Token where

import Data.Char qualified as C
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

data Tok
  = TokEOF
  | TokComment
  | TokDocComment
  | TokId !Text
  | TokKeyword !Keyword
  | TokLiteral !Literal
  | TokSymbol !Symbol
  deriving (Eq, Show)

data Literal
  = LitChar !Char
  | LitFloat !Double
  | LitInt !Integer
  | LitString !Text
  deriving (Eq, Show)

data Keyword
  = -- | data
    KwData
  | -- | else
    KwElse
  | -- | fn
    KwFn
  | -- | if
    KwIf
  | -- | in
    KwIn
  | -- | include
    KwInclude
  | -- | infix
    KwInfix
  | -- | infixl
    KwInfixL
  | -- | infixr
    KwInfixR
  | -- | fn
    KwLambda
  | -- | let
    KwLet
  | -- | match
    KwMatch
  | -- | module
    KwModule
  | -- | opaque
    KwOpaque
  | -- | open
    KwOpen
  | -- | record
    KwRecord
  | -- | renaming
    KwRenaming
  | -- | then
    KwThen
  | -- | when
    KwWhen
  | -- | where
    KwWhere
  | -- | with
    KwWith
  deriving (Eq, Show)

data Symbol
  = -- | ->
    SymArrow
  | -- | @
    SymAs
  | -- | =>
    SymCase
  | -- | }
    SymCloseBrace
  | -- | ]
    SymCloseBracket
  | -- | )
    SymCloseParen
  | -- | :
    SymColon
  | -- | ,
    SymComma
  | -- | .
    SymDot
  | -- | ..
    SymDotDot
  | -- | =
    SymEqual
  | -- | {
    SymOpenBrace
  | -- | [
    SymOpenBracket
  | -- | (
    SymOpenParen
  | -- | ?
    SymQuestionMark
  | -- | ;
    SymSemi
  | -- | _
    SymUnderscore
  | -- | |
    SymVerticalBar
  deriving (Eq, Show)

parseInt :: Text -> Integer
parseInt = parseSign parsePosInt

parseSign :: (Text -> Integer) -> Text -> Integer
parseSign f str =
  case T.uncons str of
    Just (c, rest) ->
      if c == '-'
        then negate $ f rest
        else f str
    Nothing -> f str

parsePosInt :: Text -> Integer
parsePosInt str =
  case T.splitAt 2 str of
    ("0x", digits) -> digitsToNum 16 digits
    ("0o", digits) -> digitsToNum 8 digits
    ("0b", digits) -> digitsToNum 2 digits
    _ -> digitsToNum 10 str

digitsToNum :: Integer -> Text -> Integer
digitsToNum base =
  T.foldl' (\acc d -> acc * base + fromIntegral (C.digitToInt d)) 0

unsafeReadDouble :: Text -> Double
unsafeReadDouble str =
  fromMaybe (error $ "invalid float: " ++ show str) (readMaybe $ T.unpack str)
