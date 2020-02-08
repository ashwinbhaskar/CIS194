{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- parseMessageTypeAndRest :: String -> (MessageType, String)
-- parseMessageTypeAndRest (first:' ':rest) = case first of
--     'I' -> (Info, rest)
--     'W' -> (Warning, rest)
--     'E' ->  case reads rest :: [(Int, String)] of
--         [(severity, r)] -> ((Error severity), r)

-- parseMessage :: String -> LogMessage
-- parseMessage m@(first:' ':rest) = case (parseMessageTypeAndRest m) of
--     (Info, r) -> case reads r :: [(TimeStamp, String)] of
--         [(timeStamp, message)] -> LogMessage Info timeStamp message
--     (Warning, r) -> case (reads r :: [(TimeStamp, String)]) of
--         [(timeStamp, message)] -> LogMessage Warning timeStamp message
--     ((Error s), r) -> case (reads r :: [(TimeStamp, String)]) of
--         [(timeStamp, message)] -> LogMessage (Error s) timeStamp message


parseInt :: String -> Int
parseInt str = read str :: Int

parseMessage :: String -> LogMessage
parseMessage m@(first:' ':rest) = case first of
    'I' -> LogMessage Info (parseInt(unwords (take 1 (words rest)))) (unwords (drop 1 (words rest)))
    'W' -> LogMessage Warning (parseInt(unwords (take 1 (words rest)))) (unwords (drop 1 (words rest)))
    'E' -> case (words rest) of
        (severity:r) -> LogMessage (Error (parseInt severity)) (parseInt(unwords (take 1 r))) (unwords (drop 1 r))
    _ -> Unknown m
parseMessage m = Unknown m

parseArray :: [String] -> [LogMessage]
parseArray [] = []
parseArray (first:rest) = (parseMessage first):(parseArray rest)

parse :: String -> [LogMessage]
parse input = parseArray(lines (input))

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown message) tree = tree
insert incomingMessage@(LogMessage messageType incomingTimeStamp message) tree = case tree of
    Leaf -> Node Leaf incomingMessage Leaf
    (Node leftTree m@(LogMessage _ ts _) rightTree) | incomingTimeStamp > ts -> (Node leftTree m (insert incomingMessage rightTree))
    (Node leftTree m rightTree) -> (Node (insert incomingMessage leftTree) m rightTree)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (logMessage:[]) = insert logMessage Leaf
build (logMessage:rest) = insert logMessage (build rest)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree m rightTree) = (inOrder leftTree) ++ [m] ++ (inOrder rightTree)

filterErrors :: [LogMessage] -> Int -> [String]
filterErrors [] _ = []
filterErrors ((LogMessage (Error severity) _ message):rest) severityThreshold | severity >= severityThreshold = message:(filterErrors rest severityThreshold)
filterErrors (first:rest) severityThreshold = filterErrors rest severityThreshold

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong logMessages = filterErrors (inOrder (build logMessages)) 50



