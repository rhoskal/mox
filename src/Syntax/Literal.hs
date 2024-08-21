module Syntax.Literal where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

parseInt :: ByteString -> Integer
parseInt = parseSign parsePosInt

parseSign :: (ByteString -> Integer) -> ByteString -> Integer
parseSign f str =
  case BL.uncons str of
    Just (c, rest) ->
      if c == 45
        then negate $ f rest
        else f str
    Nothing -> f str

parsePosInt :: ByteString -> Integer
parsePosInt str =
  case BL.splitAt 2 str of
    ("0x", digits) -> digitsToNum 16 digits
    ("0o", digits) -> digitsToNum 8 digits
    ("0b", digits) -> digitsToNum 2 digits
    _ -> digitsToNum 10 str

digitsToNum :: Integer -> ByteString -> Integer
digitsToNum base =
  BL.foldl' (\acc d -> acc * base + fromIntegral (d - 48)) 0

unsafeReadDouble :: ByteString -> Double
unsafeReadDouble str =
  fromMaybe (error $ "invalid float: " ++ show str) (readMaybe $ show str)
