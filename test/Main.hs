import LexerSpec (lexerSpec)
import Test.Hspec

main :: IO ()
main = hspec $ parallel $ do
  describe "Lexer" lexerSpec
