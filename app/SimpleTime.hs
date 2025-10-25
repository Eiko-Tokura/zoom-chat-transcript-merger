{-# LANGUAGE OrPatterns #-}
module SimpleTime where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Data.Fixed

data SimpleTime = SimpleTime
  { hours     :: Int
  , minutes   :: Int
  , seconds   :: Int
  , fractions :: Maybe (Fixed E3)
  } deriving (Show, Eq, Ord)

simpleTimeParser :: Parsec () Text SimpleTime
simpleTimeParser = do
  h <- read <$> count 2 digitChar
  _ <- char ':'
  m <- read <$> count 2 digitChar
  _ <- char ':'
  s <- read <$> count 2 digitChar
  frac <- optional $ do
    _ <- char '.'
    fStr <- some digitChar
    return $ read ("0." ++ fStr)
  return $ SimpleTime h m s frac

addSeconds :: Int -> SimpleTime -> SimpleTime
addSeconds sec (SimpleTime h m s frac) =
  let totalSeconds = h * 3600 + m * 60 + s + sec
      newH = totalSeconds `div` 3600
      newM = (totalSeconds `mod` 3600) `div` 60
      newS = totalSeconds `mod` 60
  in SimpleTime newH newM newS frac

formatSimpleTime :: Bool -> SimpleTime -> String
formatSimpleTime alwaysFixed (SimpleTime h m s frac) =
  let fracStr = case frac of
        (Just 0; Nothing) -> if alwaysFixed then ".000" else ""
        Just f            -> '.' : drop 2 (show f)
  in pad2 h ++ ":" ++ pad2 m ++ ":" ++ pad2 s ++ fracStr
  where
    pad2 n = if n < 10 then '0' : show n else show n
