module Golf where

skip :: [a] -> Int -> [a]
skip [] _ = []
skip l 0 = l
skip  l skipLevel | (skipLevel > (length l)) = []
skip  l skipLevel= (l !! (skipLevel - 1)):(skip (drop skipLevel l) skipLevel)

skips :: [a] -> [[a]]
skips l = map (skip l) (enumFromTo 1 (length l))

sliding :: [a] -> Int -> [[a]]
sliding arr slidingWindow
    | (length arr) < slidingWindow = [[]]
    | otherwise = (take slidingWindow arr):(sliding (drop 1 arr) slidingWindow)

removeElement :: [a] -> Int -> [a]
removeElement [] _ = []
removeElement arr index | index >= (length arr) = arr
removeElement arr index = (take index arr) ++ (drop (index + 1) arr)

isLocalMaxima :: Int -> [Integer] -> Bool
isLocalMaxima _ [] = False
isLocalMaxima possibleMaximaPosition arr = (all (< (arr !! (possibleMaximaPosition - 1))) (removeElement arr (possibleMaximaPosition - 1))) 

localMaxima :: [Integer] -> [Integer]
localMaxima arr = map (!! 1) (filter (isLocalMaxima 2) (sliding arr 3))

times :: [Integer] -> Integer -> Int
times arr e =  length (filter (==e) arr)

histogram :: [Integer] -> String
histogram
