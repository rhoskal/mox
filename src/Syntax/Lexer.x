{
module Syntax.Lexer
  ( Token(..)
  , runLexer
  ) where

import Prelude hiding (lex)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Char qualified as C
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Syntax.Literal
import Syntax.Token
}

%wrapper "monadUserState-bytestring"
%encoding "utf8"

-----------------------------------------------------------
-- Character sets
-----------------------------------------------------------
$digit         = [0-9]
$hex_digit     = [0-9 A-F a-f]
$oct_digit     = [0-7]
$bin_digit     = [0-1]
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
$charesc       = [n r t \\ \' \"]
$special       = [\( \) \[ \] \{ \} \; \, \?]
$symbol        = [\@ \& \* \+ \\ \^ \~ \= \. \: \- \| \< \>]

$whitespace    = [\t \n \r \f \v]

-----------------------------------------------------------
-- Regex patterns
-----------------------------------------------------------
@sign           = [\-]?
@digits_sep     = \_ $digit+
@hex_digits_sep = \_ $hex_digit+
@oct_digits_sep = \_ $oct_digit+
@bin_digits_sep = \_ $bin_digit+
@digits         = $digit+ @digits_sep*
@hex_digits     = $hex_digit+ @hex_digits_sep*
@oct_digits     = $oct_digit+ @oct_digits_sep*
@bin_digits     = $bin_digit+ @bin_digits_sep*
@hexadecimal    = "0x" @hex_digits
@octal          = "0o" @oct_digits
@binary         = "0b" @bin_digits
@decimal        = 0 | [1-9] (\_? @digits)?
@integer        = @sign (@decimal | @hexadecimal | @octal | @binary)

@exponent       = [eE] [\-\+] @decimal
@float          = @sign @decimal \. @decimal @exponent?
                | @sign @decimal @exponent

@id_char        = $letter | $digit | \_
@id_final       = \'
@id_tail        = @id_char* @id_final*
@lower_id       = $lower @id_tail*
@upper_id       = $upper @id_tail*
@wildcard       = \_

@constr_id      = @upper_id
@var_id         = @lower_id
@q_id           = (@upper_id \.)
@any_id         = @var_id | @constr_id

@char           = $letter
@string         = \" [^\"]* \"
                | \"\"\" .* \"\"\"

-----------------------------------------------------------
-- Tokenizer
-----------------------------------------------------------
tokens :-
  <0> $white+           { skip }
  <0> "##".*            { docComment }
  <0> "#".*             { lineComment }

  -- Reserved Keywords
  <0> "as"              { keyword KwAs }
  <0> "data"            { keyword KwData }
  <0> "else"            { keyword KwElse }
  <0> "fn"              { keyword KwLambda }
  <0> "foreign"         { keyword KwForeign }
  <0> "hiding"          { keyword KwHiding }
  <0> "if"              { keyword KwIf }
  <0> "in"              { keyword KwIn }
  <0> "include"         { keyword KwInclude }
  <0> "infix"           { keyword KwInfix }
  <0> "infixl"          { keyword KwInfixL }
  <0> "infixr"          { keyword KwInfixR }
  <0> "let"             { keyword KwLet }
  <0> "match"           { keyword KwMatch }
  <0> "module"          { keyword KwModule }
  <0> "opaque"          { keyword KwOpaque }
  <0> "open"            { keyword KwOpen }
  <0> "record"          { keyword KwRecord }
  <0> "renaming"        { keyword KwRenaming }
  <0> "then"            { keyword KwThen }
  <0> "using"           { keyword KwUsing }
  <0> "when"            { keyword KwWhen }
  <0> "where"           { keyword KwWhere }
  <0> "with"            { keyword KwWith }

  -- Reserved Symbols
  <0> "->"              { symbol SymArrow }
  <0> "@"               { symbol SymAs }
  <0> "=>"              { symbol SymCase }
  <0> "}"               { symbol SymCloseBrace }
  <0> "]"               { symbol SymCloseBracket }
  <0> ")"               { symbol SymCloseParen }
  <0> ":"               { symbol SymColon }
  <0> ","               { symbol SymComma }
  <0> "."               { symbol SymDot }
  <0> ".."              { symbol SymDotDot }
  <0> "="               { symbol SymEqual }
  <0> "{"               { symbol SymOpenBrace }
  <0> "["               { symbol SymOpenBracket }
  <0> "("               { symbol SymOpenParen }
  <0> "?"               { symbol SymQuestionMark }
  <0> ";"               { symbol SymSemi }
  <0> "_"               { symbol SymUnderscore }
  <0> "|"               { symbol SymVerticalBar }

  -- Literals
  <0> \' @char \'       { litChar }
  <0> @float            { litFloat }
  <0> @integer          { litInt }
  <0> @string           { litString }

  -- Identifiers
  <0> @var_id           { getVariable }

{
------------------------------------------------------------------------------
-- Alex Setup
------------------------------------------------------------------------------
data Token = Token AlexPosn Tok ByteString
  deriving (Eq, Show)

type AlexUserState = ByteString

alexInitUserState :: AlexUserState
alexInitUserState =
  BL.empty

mkToken :: Tok -> AlexAction Token
mkToken tok = token tokenize
  where
    tokenize (posn, _, input, _) len =
      let lexeme = BL.take (fromIntegral len) input
       in Token posn tok lexeme

litInt :: AlexInput -> Int64 -> Alex Token
litInt (posn, _, input, _) len =
  let lexeme = BL.take len input
      tok = TokLiteral $ LitInt $ parseInt $ BL.filter (/= 95) input
   in return $ Token posn tok lexeme

litFloat :: AlexInput -> Int64 -> Alex Token
litFloat (posn, _, input, _) len =
  let lexeme = BL.take len input
      tok = TokLiteral $ LitFloat $ unsafeReadDouble $ BL.filter (/= 95) input
   in return $ Token posn tok lexeme

litChar :: AlexInput -> Int64 -> Alex Token
litChar (posn, _, input, _) len =
  let lexeme = BL.take len input
      tok = TokLiteral $ LitChar $ C.chr $ fromIntegral $ BL.head $ BL.tail $ BL.init input
   in return $ Token posn tok lexeme

litString :: AlexInput -> Int64 -> Alex Token
litString (posn, _, input, _) len =
  let lexeme = BL.take len input
      tok = TokLiteral $ LitString $ TE.decodeUtf8Lenient $ BL.toStrict lexeme
   in return $ Token posn tok lexeme

docComment :: AlexInput -> Int64 -> Alex Token
docComment =
  mkToken TokDocComment

lineComment :: AlexInput -> Int64 -> Alex Token
lineComment =
  mkToken TokComment

keyword :: Keyword -> AlexInput -> Int64 -> Alex Token
keyword =
  mkToken . TokKeyword

symbol :: Symbol -> AlexInput -> Int64 -> Alex Token
symbol =
  mkToken . TokSymbol

getVariable :: AlexInput -> Int64 -> Alex Token
getVariable (p, _, input, _) len =
  let lexeme = BL.take len input
   in return $ Token p (TokId $ TE.decodeUtf8Lenient $ BL.toStrict lexeme) lexeme

lexError :: String -> Alex AlexInput
lexError s = do
  (p, _, input, _) <- alexGetInput
  alexError (showPosn p ++ ": " ++ s ++
    (if not (BL.null input)
      then " before " ++ show (BL.head input)
      else " at end of file"))

showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) =
  show line ++ ':' : show col

alexEOF :: Alex Token
alexEOF =
  return $ Token undefined TokEOF ""

alexInitFilename :: ByteString -> Alex ()
alexInitFilename fileName =
  Alex $ \s -> Right (s { alex_ust = fileName }, ())

alexGetFilename :: Alex ByteString
alexGetFilename =
  Alex $ \s -> Right (s, alex_ust s)

scanMany :: Alex [Token]
scanMany = do
  token'@(Token _ tok _) <- alexMonadScan
  if tok == TokEOF
    then return [token']
    else (token':) <$> scanMany

runLexer :: ByteString -> ByteString -> Either String [Token]
runLexer source input =
  runAlex input $ alexInitFilename source >> init <$> scanMany
}
