{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage = parseWords . words

parseWords :: [String] -> LogMessage
parseWords ("I":timestamp:rest)           = LogMessage Info (read timestamp) (unwords rest)
parseWords ("W":timestamp:rest)           = LogMessage Warning (read timestamp) (unwords rest)
parseWords ("E":severity:timestamp:rest)  = LogMessage (Error (read severity)) (read timestamp) (unwords rest)
parseWords xs                             = Unknown (unwords xs)

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) a = a
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ ts _) n@(Node t1 m1@(LogMessage _ ts1 _) t2) =
  if ts > ts1 then Node n m Leaf else Node t1 m1 (insert m t2)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inRevOrder :: MessageTree -> [LogMessage]
inRevOrder Leaf = []
inRevOrder (Node l m r) = (inOrder l) ++ (m:(inOrder r))

inOrder :: MessageTree -> [LogMessage]
inOrder = reverse . inRevOrder



sortLog :: [LogMessage] -> [LogMessage]
sortLog = inOrder . build

showMessage :: LogMessage -> String
showMessage (Unknown x) = x
showMessage (LogMessage _ _ x) = x

isRelevant :: LogMessage -> Bool
isRelevant (LogMessage (Error x) _ _)
  | x >= 50 = True
isRelevant _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = map showMessage (sortLog (filter isRelevant xs))
