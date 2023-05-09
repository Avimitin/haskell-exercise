{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)

parseMessage :: String -> LogMessage
parseMessage = handleMsgSegment . words

handleMsgSegment :: [String] -> LogMessage
handleMsgSegment ("I" : rest) = parseRestOfTheLog Info rest
handleMsgSegment ("W" : rest) = parseRestOfTheLog Warning rest
handleMsgSegment ("E" : rest) = parseErrorLog rest
handleMsgSegment orig = Unknown (unwords orig)

parseRestOfTheLog :: MessageType -> [String] -> LogMessage
parseRestOfTheLog _ [] = Unknown "Unreachable"
parseRestOfTheLog logTyp (digit : rest) = LogMessage logTyp (toInt digit) (unwords rest)

parseErrorLog :: [String] -> LogMessage
parseErrorLog (errLevel : digit : rest) = LogMessage (Error (toInt errLevel)) (toInt digit) (unwords rest)
parseErrorLog [_] = Unknown "Unreachable"
parseErrorLog [] = Unknown "Unreachable"

toInt :: String -> Int
toInt = read
