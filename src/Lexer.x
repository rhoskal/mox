{
module Lexer
  ( Token(..)
  , lexer
  ) where

import Prelude hiding (lex)
import Data.Text (Text)
import Data.Text qualified as T
import Token
}

%wrapper "monadUserState-strict-text"
%encoding "utf8"

-----------------------------------------------------------
-- Character sets
-----------------------------------------------------------
$digit         = [0-9]
$hexdigit      = [0-9 A-F a-f]
$octdigit      = [0-7]
$bindigit      = [0-1]
$lower         = [a-z]
$upper         = [A-Z]
$letter        = [a-z A-Z]
$space         = [\ ]
$tab           = \t
$return        = \r
$linefeed      = \n
$finalid       = \'
$graphic       = [\x21-\x7E]
$extended      = [\x80-\xBF]
$charesc       = [nrt\\\'\"]
$special       = [\(\)\[\]\{\}\;\,\?]
$symbol        = [\@\&\*\+\\\^\~\=\.\:\-\|\<\>]

$whitespace    = [ \t\n\r\f\v]

-----------------------------------------------------------
-- Regex patterns
-----------------------------------------------------------
@sign          = [\-]?
@digitsep      = \_ $digit+
@hexdigitsep   = \_ $hexdigit+
@octdigitsep   = \_ $octdigit+
@bindigitsep   = \_ $bindigit+
@digits        = $digit+ @digitsep*
@hexdigits     = $hexdigit+ @hexdigitsep*
@octdigits     = $octdigit+ @octdigitsep*
@bindigits     = $bindigit+ @bindigitsep*
@hexadecimal   = "0x" @hexdigits
@octal         = "0o" @octdigits
@binary        = "0b" @bindigits
@decimal       = 0 | [1-9] (\_? @digits)?
@integer       = @sign (@decimal | @hexadecimal | @octal | @binary)

@exponent      = [eE] [\-\+] @decimal
@float         = @sign @decimal \. @decimal @exponent?
               | @sign @decimal @exponent

@idchar        = $letter | $digit | \_
@idfinal       = \'
@idtail        = @idchar* @idfinal*
@lowerid       = $lower @idtail*
@upperid       = $upper @idtail*
@wildcard      = \_

@constrid      = @upperid
@varid         = @lowerid
@qid           = (@upperid \.)
@anyid         = @varid | @constrid

@char          = $letter
@string        = $letter+

-----------------------------------------------------------
-- Tokenizer
-----------------------------------------------------------
tokens :-
  <0> $white+           { skip }
  <0> "##".*            { skip } -- DocComment
  <0> "#".*             { skip } -- LineComment

  -- Specials
  <0> $special          { string $ TokSpecial }

  -- Reserved Operators
  <0> ".."              { mkToken TokRange }
  <0> "<>"              { mkToken TokAppend }
  <0> "="               { mkToken TokAssign }
  <0> "@"               { mkToken TokAt }
  <0> "::"              { mkToken TokCons }
  <0> ":"               { mkToken TokColon }
  <0> "|"               { mkToken TokVerticalPipe }
  <0> "->"              { mkToken TokRightArrow }
  <0> "=>"              { mkToken TokFatArrow }
  <0> "."               { mkToken TokDot }
  <0> "&&"              { mkToken TokAnd }
  <0> "||"              { mkToken TokOr }
  <0> "^"               { mkToken TokXor }
  <0> ".&."             { mkToken TokBitwiseAnd }
  <0> ".|."             { mkToken TokBitwiseOr }
  <0> ".^."             { mkToken TokBitwiseXor }
  <0> ".~."             { mkToken TokBitwiseNot }
  <0> "=="              { mkToken TokEquality }
  <0> "/="              { mkToken TokNotEquals }
  <0> "<"               { mkToken TokLessThan }
  <0> "<="              { mkToken TokLessThanEquals }
  <0> ">"               { mkToken TokGreaterThan }
  <0> ">="              { mkToken TokGreaterThanEquals }
  <0> "**"              { mkToken TokExponent }
  <0> "+."              { mkToken TokFloatAdd }
  <0> "-."              { mkToken TokFloatSub }
  <0> "*."              { mkToken TokFloatMul }
  <0> "/."              { mkToken TokFloatDiv }
  <0> "+"               { mkToken TokIntAdd }
  <0> "-"               { mkToken TokIntSub }
  <0> "*"               { mkToken TokIntMul }
  <0> "/"               { mkToken TokIntDiv }

  -- Reserved Keywords
  <0> "alias"           { mkToken TokAlias }
  <0> "as"              { mkToken TokAs }
  <0> "def"             { mkToken TokDef }
  <0> "defp"            { mkToken TokDefPrivate }
  <0> "do"              { mkToken TokDo }
  <0> "else"            { mkToken TokElse }
  <0> "end"             { mkToken TokEnd }
  <0> "fn"              { mkToken TokLambda }
  <0> "hiding"          { mkToken TokHiding }
  <0> "if"              { mkToken TokIf }
  <0> "in"              { mkToken TokIn }
  <0> "include"         { mkToken TokInclude }
  <0> "let"             { mkToken TokLet }
  <0> "match"           { mkToken TokMatch }
  <0> "module"          { mkToken TokModule }
  <0> "opaque"          { mkToken TokOpaque }
  <0> "open"            { mkToken TokOpen }
  <0> "renaming"        { mkToken TokRenaming }
  <0> "@sig"            { mkToken TokSig }
  <0> "then"            { mkToken TokThen }
  <0> "type"            { mkToken TokType }
  <0> "unit"            { mkToken TokUnit }
  <0> "using"           { mkToken TokUsing }
  <0> "where"           { mkToken TokWhere }
  <0> "with"            { mkToken TokWith }

  -- Literals
  <0> @integer          { string $ TokInt . parseInt . T.filter (/= '_') }
  <0> @float            { string $ TokFloat . unsafeReadDouble . T.filter (/= '_') }

  <0> \' @char \'       { mkToken TokChar }
  <0> \" [^\"]* \"      { string $ TokString }
  <0> \"\"\" .* \"\"\"  { string $ TokMultiString }
  <0> @varid            { getVariable }

{
------------------------------------------------------------------------------
-- Alex Setup
------------------------------------------------------------------------------
data Token = Token AlexPosn Tok Text
  deriving (Eq, Show)

type AlexUserState = Text

alexInitUserState :: AlexUserState
alexInitUserState =
  T.empty

mkToken :: Tok -> AlexInput -> Int -> Alex Token
mkToken tok (p, _, _, str) len =
  let lexeme :: Text
      lexeme = T.take len str
   in return $ Token p tok lexeme

string :: (Text -> Tok) -> AlexInput -> Int -> Alex Token
string tokenizer (p, _, _, str) len =
  let lexeme :: Text
      lexeme = T.take len str
   in return $ Token p (tokenizer lexeme) lexeme

keyword :: AlexInput -> Int -> Alex Token
keyword =
  string TokKeyword

getVariable :: AlexInput -> Int -> Alex Token
getVariable (p, _, _, input) len =
  let var :: Text
      var = T.take len input
   in return $ Token p (TokId var) var

lexError :: String -> Alex AlexInput
lexError s = do
  (p, _, _, input) <- alexGetInput
  alexError (showPosn p ++ ": " ++ s ++
    (if not (T.null input)
      then " before " ++ show (T.head input)
      else " at end of file"))

showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) =
  show line ++ ':' : show col

alexEOF :: Alex Token
alexEOF =
  return $ Token undefined TokEOF ""

alexInitFilename :: Text -> Alex ()
alexInitFilename fileName =
  Alex $ \s -> Right (s { alex_ust = fileName }, ())

alexGetFilename :: Alex Text
alexGetFilename =
  Alex $ \s -> Right (s, alex_ust s)

scanMany :: Alex [Token]
scanMany = do
  token'@(Token _ tok _) <- alexMonadScan
  if tok == TokEOF
    then return [token']
    else (token':) <$> scanMany

lexer :: Text -> Text -> Either String [Token]
lexer source input =
  runAlex input $ alexInitFilename source >> init <$> scanMany
}
