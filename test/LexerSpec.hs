module LexerSpec (lexerSpec) where

import Data.Either (isRight)
import Data.Text qualified as T
import Lexer (Token)
import Lexer qualified as L
import Test.Hspec

lexer :: T.Text -> Either String [Token]
lexer = L.lexer "dummy.mox"

lexerSpec :: Spec
lexerSpec = do
  describe "special" $ do
    it "parens" $ do
      let lparen = lexer "("
          rparen = lexer ")"
       in isRight lparen && isRight rparen

    it "braces" $ do
      pending

    it "brackets" $ do
      pending

    it "comma" $ do
      pending
