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

newMsgTreeNode :: LogMessage -> MessageTree
newMsgTreeNode msg = Node Leaf msg Leaf

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) _ = Leaf
insert msg Leaf = newMsgTreeNode msg
insert _ (Node _ (Unknown _) _) = Leaf
insert msg@(LogMessage _ t _) (Node lchild node@(LogMessage _ cur_t _) Leaf) =
  if t > cur_t
    then Node lchild node (newMsgTreeNode msg)
    else Node (insert msg lchild) node Leaf
insert msg@(LogMessage _ t _) (Node Leaf node@(LogMessage _ cur_t _) rchild) =
  if t > cur_t
    then Node Leaf node (insert msg rchild)
    else Node (newMsgTreeNode msg) node rchild
insert msg@(LogMessage _ t _) (Node lchild node@(LogMessage _ cur_t _) rchild) =
  if t > cur_t
    then Node lchild node (insert msg rchild)
    else Node (insert msg lchild) node rchild

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lchild msg rchild) = inOrder lchild ++ [msg] ++ inOrder rchild

isError :: LogMessage -> Bool
isError msg = case msg of
  LogMessage (Error level) _ _ | level > 50 -> True
  _ -> False

unwrapError :: LogMessage -> String
unwrapError msg = case msg of
  LogMessage (Error _) _ text -> text
  _ -> "Unreachable"

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map unwrapError . inOrder . build . filter isError
