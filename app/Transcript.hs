{-# LANGUAGE OverloadedStrings #-}
module Transcript where

import Data.Text (Text, pack)
import Text.Megaparsec
import Text.Megaparsec.Char
import SimpleTime
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

{-
Format to parse:
WEBVTT

1
00:00:02.810 --> 00:00:06.129
Asuka: 对可以把它录下来，这样其他的巡游可以看到

2
00:00:06.650 --> 00:00:12.270
Asuka: 然后，但是依照你的聊天，他是录不下来的，就是你这个聊天文字他们都看不到。

3
00:00:12.420 --> 00:00:19.270
Asuka: 哎呀，因为你也看不到你直播的时候，他会把这个全屏蔽的。

4
00:00:19.740 --> 00:00:26.849
Asuka: 好的看一下我的诶，那是什么？

5
00:00:28.560 --> 00:00:32.410
Asuka: 对这三个就是我们来确定这三件事。
-}

data TranscriptEntry = TranscriptEntry
  { entryIndex   :: Int
  , startTime    :: SimpleTime
  , endTime      :: SimpleTime
  , entryContent :: Text
  } deriving (Show, Eq)

renderTranscriptEntry :: TranscriptEntry -> TL.Text
renderTranscriptEntry te
  =  TL.pack (show (entryIndex te))
  <> "\n"
  <> TL.pack (formatSimpleTime True (startTime te))
  <> " --> "
  <> TL.pack (formatSimpleTime True (endTime te))
  <> "\n"
  <> TL.fromStrict (removeNewLines $ entryContent te)
  <> "\n"
  where removeNewLines = T.replace "\n" ";" . T.replace "\r" ""

transcriptEntryParser :: Parsec () Text TranscriptEntry
transcriptEntryParser = do
  index <- read <$> some digitChar
  _ <- eol
  start <- simpleTimeParser
  _ <- string " --> "
  end <- simpleTimeParser
  _ <- eol
  content <- manyTill anySingle newline
  return $ TranscriptEntry index start end (pack content)

parseTranscript :: Text -> Either (ParseErrorBundle Text ()) [TranscriptEntry]
parseTranscript = parse
  (do
      _ <- string "WEBVTT" >> eol >> eol
      many (transcriptEntryParser <* optional eol)
  )
  "<Transcript>"
