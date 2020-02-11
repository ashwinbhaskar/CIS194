module Golf where

skip :: [a] -> Int -> [a]
skip [] _ = []
skip l 0 = l
skip  l skipLevel | (skipLevel > (length l)) = []
skip  l skipLevel= (l !! (skipLevel - 1)):(skip (drop skipLevel l) skipLevel)

skips :: [a] -> [[a]]
skips l = map (skip l) (enumFromTo 1 (length l))

localMaxima :: [Integer] -> [Integer]
localMaxima