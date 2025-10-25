module NewChat where

import Data.Text (Text)
import SimpleTime
import Text.Megaparsec
import Text.Megaparsec.Char

{- format to parse

00:18:23	irony:	说起来这个是vim嘛？
00:18:25	irony:	好高级（
00:18:29	irony:	诶oxo
00:19:00	irony:	Probabilistic Machine Learning: An Introduction
Book by Kevin P. Murphy

是这本嘛oxo
00:19:47	irony:	这个思路可以诶，不过一周会不会有点太紧了（
00:20:17	irony:	哦哦，一次oxo
00:20:20	irony:	那没问题oxo
00:20:53	irony:	（偷瞄

-}
-- this separator '^M' is actually '\r' character?

data NewChatMessage = NewChatMessage
  { timestamp :: SimpleTime
  , username  :: Text
  , content   :: Text
  } deriving (Show, Eq)

newChatMessageParser :: Parsec () Text NewChatMessage
newChatMessageParser = do
  time <- simpleTimeParser
  _ <- char '\t'
  user <- takeWhile1P (Just "username") (/= ':')
  _ <- char ':'
  _ <- char '\t'
  text <- takeWhile1P (Just "message content") (/= '\r')
  return $ NewChatMessage time user text

parseNewChatMessage :: Text -> Either (ParseErrorBundle Text ()) [NewChatMessage]
parseNewChatMessage = parse (many (newChatMessageParser <* optional eol)) "<NewChatMessage>"
