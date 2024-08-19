module Syntax.Token where

import Data.Text (Text)

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
  = -- | as
    KwAs
  | -- | data
    KwData
  | -- | else
    KwElse
  | -- | fn
    KwFn
  | -- | foreign
    KwForeign
  | -- | hiding
    KwHiding
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
  | -- | using
    KwUsing
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
