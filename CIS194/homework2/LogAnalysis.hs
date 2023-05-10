{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)

parseMessage :: String -> LogMessage
parseMessage = parseLogType . words

parseLogType :: [String] -> LogMessage
parseLogType ("I" : rest) = parseCommonLog Info rest
parseLogType ("W" : rest) = parseCommonLog Warning rest
parseLogType ("E" : rest) = parseErrorLog rest
parseLogType orig = Unknown (unwords orig)

parseCommonLog :: MessageType -> [String] -> LogMessage
parseCommonLog _ [] = Unknown "Unreachable"
parseCommonLog logTyp (timestamp : rest) = LogMessage logTyp (toInt timestamp) (unwords rest)

parseErrorLog :: [String] -> LogMessage
parseErrorLog (errLevel : timestamp : rest) = LogMessage (Error (toInt errLevel)) (toInt timestamp) (unwords rest)
parseErrorLog [_] = Unknown "Unreachable"
parseErrorLog [] = Unknown "Unreachable"

toInt :: String -> Int
toInt = read
