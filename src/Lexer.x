{
module Lexer
  ( TokenType(..)
  , Lexeme
  , lex
  ) where

import Prelude hiding (lex)
import Data.Text (Text)
import Data.Text qualified as T
}

%wrapper "monadUserState-strict-text"

-----------------------------------------------------------
-- Character sets
-----------------------------------------------------------
$digit       = [0-9]
$hexdigit    = [0-9 A-F a-f]
$octaldigit  = [0-7]
$lower       = [a-z]
$upper       = [A-Z]
$letter      = [$lower $upper]
$space       = [\ ]
$tab         = [\t]
$return      = \r
$linefeed    = \n
$finalid     = [\']

$whitespace = [ \t\n\r\f\v]

-----------------------------------------------------------
-- Regex patterns
-----------------------------------------------------------
@decimal     = $digit+
@octal       = $octaldigit+
@hexadecimal = $hexdigit+
@exponent    = [eE] [\-\+] @decimal
@identifier  = $letter($letter | _ | $digit)*
@char        = $letter
@string      = $letter+

Mox :-
  <0> $white+                         { skip }
  <0> "##".*                          { skip } -- DocComment
  <0> "#".*                           { skip } -- LineComment

  -- Special
  <0> "("                             { mkL TokParenOpen }
  <0> ")"                             { mkL TokParenClose }
  <0> "{"                             { mkL TokBraceOpen }
  <0> "}"                             { mkL TokBraceClose }
  <0> "["                             { mkL TokBracketOpen }
  <0> "]"                             { mkL TokBracketClose }
  <0> ","                             { mkL TokComma }

  -- Reserved Operators
  <0> ".."                            { mkL TokRange }
  <0> "<>"                            { mkL TokAppend }
  <0> "="                             { mkL TokAssign }
  <0> "@"                             { mkL TokAt }
  <0> "::"                            { mkL TokCons }
  <0> ":"                             { mkL TokColon }
  <0> "|"                             { mkL TokVerticalPipe }
  <0> "->"                            { mkL TokRightArrow }
  <0> "=>"                            { mkL TokFatArrow }
  <0> "."                             { mkL TokDot }
  <0> "&&"                            { mkL TokAnd }
  <0> "||"                            { mkL TokOr }
  <0> "^"                             { mkL TokXor }
  <0> ".&."                           { mkL TokBitwiseAnd }
  <0> ".|."                           { mkL TokBitwiseOr }
  <0> ".^."                           { mkL TokBitwiseXor }
  <0> ".~."                           { mkL TokBitwiseNot }
  <0> "=="                            { mkL TokEquality }
  <0> "/="                            { mkL TokNotEquals }
  <0> "<"                             { mkL TokLessThan }
  <0> "<="                            { mkL TokLessThanEquals }
  <0> ">"                             { mkL TokGreaterThan }
  <0> ">="                            { mkL TokGreaterThanEquals }
  <0> "**"                            { mkL TokExponent }
  <0> "+."                            { mkL TokFloatAdd }
  <0> "-."                            { mkL TokFloatSub }
  <0> "*."                            { mkL TokFloatMul }
  <0> "/."                            { mkL TokFloatDiv }
  <0> "+"                             { mkL TokIntAdd }
  <0> "-"                             { mkL TokIntSub }
  <0> "*"                             { mkL TokIntMul }
  <0> "/"                             { mkL TokIntDiv }

  -- Reserved Keywords
  <0> "alias"                         { mkL TokAlias }
  <0> "as"                            { mkL TokAs }
  <0> "def"                           { mkL TokDef }
  <0> "defp"                          { mkL TokDefPrivate }
  <0> "do"                            { mkL TokDo }
  <0> "else"                          { mkL TokElse }
  <0> "end"                           { mkL TokEnd }
  <0> "fn"                            { mkL TokLambda }
  <0> "hiding"                        { mkL TokHiding }
  <0> "if"                            { mkL TokIf }
  <0> "in"                            { mkL TokIn }
  <0> "include"                       { mkL TokInclude }
  <0> "let"                           { mkL TokLet }
  <0> "match"                         { mkL TokMatch }
  <0> "module"                        { mkL TokModule }
  <0> "opaque"                        { mkL TokOpaque }
  <0> "open"                          { mkL TokOpen }
  <0> "renaming"                      { mkL TokRenaming }
  <0> "@sig"                          { mkL TokSig }
  <0> "then"                          { mkL TokThen }
  <0> "type"                          { mkL TokType }
  <0> "unit"                          { mkL TokUnit }
  <0> "using"                         { mkL TokUsing }
  <0> "where"                         { mkL TokWhere }
  <0> "with"                          { mkL TokWith }

  -- Literals
  <0> @decimal
    | 0o @octal
    | 0x @hexadecimal                 { mkL TokInteger }

  <0> @decimal \. @decimal @exponent?
    | @decimal @exponent              { mkL TokFloat }

  <0> \' @char \'                     { mkL TokChar }
  <0> \" [^\"]* \"                    { mkL TokString }
  <0> \"\"\" .* \"\"\"                { mkL TokMultilineString }
  <0> @identifier                     { getVariable }

{
data Lexeme
  = Lexeme AlexPosn TokenType Text
  deriving (Eq, Show)

data TokenType
  = TokEOF
  | TokId Text

  -- Special
  | TokBraceOpen
  | TokBraceClose
  | TokBracketOpen
  | TokBracketClose
  | TokParenOpen
  | TokParenClose
  | TokComma

  -- Reserved Keywords
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

  -- Reserved Operators
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

  -- Literals
  | TokChar
  | TokFloat
  | TokInteger
  | TokMultilineString
  | TokString
  deriving (Eq, Show)

type AlexUserState = Text

alexInitUserState :: AlexUserState
alexInitUserState =
  T.empty

mkL :: TokenType -> AlexInput -> Int -> Alex Lexeme
mkL tok (p, _, _, str) len =
  return $ Lexeme p tok (T.take len str)

getVariable :: AlexInput -> Int -> Alex Lexeme
getVariable (p, _, _, input) len =
  let var :: Text
      var = T.take len input
   in return $ Lexeme p (TokId var) var

lexError :: String -> Alex AlexInput
lexError s = do
  (p, _, _, input) <- alexGetInput
  alexError (showPosn p ++ ": " ++ s ++
    (if (not (T.null input))
      then " before " ++ show (T.head input)
      else " at end of file"))

showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) =
  show line ++ ':' : show col

alexEOF :: Alex Lexeme
alexEOF =
  return $ Lexeme undefined TokEOF ""

alexInitFilename :: Text -> Alex ()
alexInitFilename fileName =
  Alex $ \s -> Right (s { alex_ust = fileName }, ())

alexGetFilename :: Alex Text
alexGetFilename =
  Alex $ \s -> Right (s, alex_ust s)

alexLex :: Alex [Lexeme]
alexLex = do
  lexeme@(Lexeme _ tok _) <- alexMonadScan
  if tok == TokEOF
    then return [lexeme]
    else (lexeme:) <$> alexLex

lex :: Text -> Text -> Either String [Lexeme]
lex fileName input =
  runAlex input $ alexInitFilename fileName >> init <$> alexLex
}
