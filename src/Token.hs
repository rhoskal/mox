module Token where

import Data.Char qualified as C
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

data Tok
  = TokEOF
  | TokId !Text
  | TokSpecial !Text
  | TokKeyword !Text
  | TokSymbol !Text
  | TokOperator !Text
  | TokAlias
  | TokAppend
  | TokAs
  | TokDef
  | TokDefPrivate
  | TokDo
  | TokElse
  | TokEnd
  | TokHiding
  | TokIf
  | TokIn
  | TokInclude
  | TokLambda
  | TokLet
  | TokMatch
  | TokModule
  | TokOpaque
  | TokOpen
  | TokRenaming
  | TokSig
  | TokThen
  | TokType
  | TokUnit
  | TokUsing
  | TokWhere
  | TokWith
  | TokAnd
  | TokAssign
  | TokAt
  | TokBitwiseAnd
  | TokBitwiseNot
  | TokBitwiseOr
  | TokBitwiseXor
  | TokColon
  | TokCons
  | TokDot
  | TokEquality
  | TokExponent
  | TokFloatAdd
  | TokFloatDiv
  | TokFloatMul
  | TokFloatSub
  | TokFatArrow
  | TokGreaterThan
  | TokGreaterThanEquals
  | TokIntAdd
  | TokIntDiv
  | TokIntMul
  | TokIntSub
  | TokLessThan
  | TokLessThanEquals
  | TokNotEquals
  | TokOr
  | TokRightArrow
  | TokRange
  | TokVerticalPipe
  | TokXor
  | TokChar
  | TokFloat !Double
  | TokInt !Integer
  | TokMultiString !Text
  | TokString !Text
  deriving (Eq, Show)

specials :: Set Text
specials =
  Set.fromList
    [ "{",
      "}",
      "(",
      ")",
      "[",
      "]",
      ";",
      ","
    ]

keywords :: Set Text
keywords =
  Set.fromList
    [ "data",
      "else",
      "fn",
      "if",
      "in",
      "include",
      "infix",
      "infixl",
      "infixr",
      "let",
      "match",
      "module",
      "opaque",
      "open",
      "record",
      "renaming",
      "then",
      "when",
      "where",
      "with"
    ]

symbols :: Set Text
symbols =
  Set.fromList
    [ "=",
      "..",
      ".",
      "<>",
      "@",
      "::",
      ":",
      "|",
      "->",
      "=>"
    ]

operators :: Set Text
operators =
  Set.fromList
    [ "==",
      "&&",
      "||",
      "^",
      ".&.",
      ".|.",
      ".^.",
      ".~.",
      "==",
      "/=",
      "<",
      "<=",
      ">",
      ">=",
      "**",
      "+.",
      "*.",
      "-.",
      "/.",
      "+",
      "*",
      "-",
      "/"
    ]

isSpecial :: Text -> Bool
isSpecial name =
  Set.member name specials

isKeyword :: Text -> Bool
isKeyword name =
  Set.member name keywords

isSymbol :: Text -> Bool
isSymbol name =
  Set.member name symbols

isOperator :: Text -> Bool
isOperator name =
  Set.member name operators

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
