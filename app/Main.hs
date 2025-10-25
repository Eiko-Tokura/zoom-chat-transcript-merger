{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}
module Main where

import Data.Text (pack)
import SimpleTime
import Data.Function
import Data.List (sortOn)
import Transcript
import NewChat
import Options.Generic
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

data Args = Args
  { transcript :: FilePath
  , newChat    :: FilePath
  } deriving (Show, Eq, Generic, ParseRecord)

main :: IO ()
main = do
  args <- getRecord "Transcript and NewChat Parser"
  transcriptContent <- readFile (transcript args)
  newChatContent <- readFile (newChat args)

  case ( parseTranscript (pack transcriptContent)
       , parseNewChatMessage (pack newChatContent)
       ) of
    (Left err, _) -> putStrLn $ "Error parsing transcript: " ++ show err
    (_, Left err) -> putStrLn $ "Error parsing new chat messages: " ++ show err
    (Right transcriptData, Right newChatData) -> do
      let combined = map Left transcriptData ++ map Right newChatData
                   & sortOn (either startTime timestamp)
      case outputCombinedVtt combined of
        Nothing -> putStrLn "No data to output."
        Just vttText -> TLIO.putStrLn vttText

outputCombinedVtt :: [Either TranscriptEntry NewChatMessage] -> Maybe TL.Text
outputCombinedVtt [] = Nothing
outputCombinedVtt input@(_:xs)
  = Just
  $ ("WEBVTT\n\n" <>)
  $ mconcat
  $ map ((<> "\n") . renderTranscriptEntry)
  $ zipWith3 renderLogic
      [1..]
      input
      (map Just xs ++ [Nothing])

type Row = Either TranscriptEntry NewChatMessage
renderLogic
  :: Int       -- ^ entry index
  -> Row       -- ^ current row
  -> Maybe Row -- ^ next row
  -> TranscriptEntry
renderLogic ind (Left te) (Just (Left _))   = te { entryIndex = ind }
renderLogic ind (Left te) Nothing           = te { entryIndex = ind }
renderLogic ind (Left te) (Just (Right cm)) = te { entryIndex = ind, endTime = timestamp cm }
renderLogic ind (Right cm) anyEntry = TranscriptEntry
  { entryIndex = ind
  , startTime = timestamp cm
  , endTime = case anyEntry of
      Just (Left te) -> startTime te
      Just (Right cm2) -> timestamp cm2 `min` addSeconds 5 (timestamp cm)
      Nothing -> addSeconds 5 (timestamp cm)
  , entryContent = "[Chat]" <> username cm <> ": " <> content cm
  }
