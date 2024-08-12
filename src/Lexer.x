{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $white+				                  ;
  "##".*	  			                { \_ -> DocComment }
  "#".*	  			                  { \_ -> LineComment }

  -- Keywords
  module		  			              { \_ -> Module }
  where  		  			              { \_ -> Where }
  def		  			                  { \_ -> Def }
  do		  			                  { \_ -> Do }
  end		  			                  { \_ -> End }

  -- Let Block
  let		  			                  { \_ -> Let }
  in		  			                  { \_ -> In }

  -- Conditional
  if		  			                  { \_ -> If }
  then		  			                { \_ -> Then }
  else		  			                { \_ -> Else }

  -- Pattern Matching
  match		  			                { \_ -> Match }
  with		  			                { \_ -> With }
  "|"		  			                  { \_ -> VerticalPipe }
  "=>"		  			                { \_ -> FatArrow }

  -- Arithmetic operators
  "+."		  			                { \_ -> FAdd }
  "-."		  			                { \_ -> FSub }
  "*."		  			                { \_ -> FMul }
  "/."		  			                { \_ -> FDiv }
  "+"		  			                  { \_ -> IAdd }
  "-"		  			                  { \_ -> ISub }
  "*"		  			                  { \_ -> IMul }
  "/"		  			                  { \_ -> IDiv }

  -- Comparison operators
  "="		  			                  { \_ -> Assign }
  "=="		  			                { \_ -> Eq }
  "/="		  			                { \_ -> Neq }
  "<"		  			                  { \_ -> Lt }
  "<="		  			                { \_ -> Lte }
  ">"		  			                  { \_ -> Gt }
  ">="		  			                { \_ -> Gte }

  -- Logical operators
  "&&"		  			                { \_ -> And }
  "||"		  			                { \_ -> Or }
  "^" 		  			                { \_ -> Xor }
  ".&."		  			                { \_ -> BitwiseAnd }
  ".|."		  			                { \_ -> BitwiseOr }
  ".^." 		  			              { \_ -> BitwiseXor }

  -- Types
  ":"                             { \_ -> Colon }
  "->"                            { \_ -> RArrow }
  "@sig"		  			              { \_ -> FuncSig }
  opaque                          { \_ -> Opaque }
  type                            { \_ -> Type }
  alias                           { \_ -> Alias }

  "("                             { \_ -> LParen }
  ")"                             { \_ -> RParen }
  "["                             { \_ -> LBracket }
  "]"                             { \_ -> RBracket }
  "{"                             { \_ -> LCurly }
  "}"                             { \_ -> RCurly }
  ","                             { \_ -> Comma }

  $digit+				                  { \s -> Int (read s) }
  $alpha [$alpha $digit \_ \']*		{ \s -> Var s }

{
data Token
  = DocComment
  | LineComment
  | Module
  | Where
  | Def
  | Do
  | End
  | Let
  | In
  | If
  | Then
  | Else
  | Match
  | With
  | VerticalPipe
  | FatArrow
  | FAdd
  | FSub
  | FMul
  | FDiv
  | IAdd
  | ISub
  | IMul
  | IDiv
  | Assign
  | Eq
  | Neq
  | Lt
  | Lte
  | Gt
  | Gte
  | And
  | Or
  | Xor
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | Colon
  | RArrow
  | FuncSig
  | Opaque
  | Type
  | Alias
  | LParen
  | RParen
  | LBracket
  | RBracket
  | LCurly
  | RCurly
  | Comma
  | Var String
  | Int Int
  | Err
  deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
