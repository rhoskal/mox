module Syntax.Literal where

import Data.Char qualified as C
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

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
