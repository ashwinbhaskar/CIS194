module Golf where

--Hopscotch
skip :: [a] -> Int -> [a]
skip [] _ = []
skip l 0 = l
skip  l skipLevel | (skipLevel > (length l)) = []
skip  l skipLevel= (l !! (skipLevel - 1)):(skip (drop skipLevel l) skipLevel)

skips :: [a] -> [[a]]
skips l = map (skip l) (enumFromTo 1 (length l))

--Local maxima
sliding :: [a] -> Int -> [[a]]
sliding arr slidingWindow
    | (length arr) < slidingWindow = [[]]
    | otherwise = (take slidingWindow arr):(sliding (drop 1 arr) slidingWindow)

removeElementAtIndex :: [a] -> Int -> [a]
removeElementAtIndex [] _ = []
removeElementAtIndex arr index | index >= (length arr) = arr
removeElementAtIndex arr index = (take index arr) ++ (drop (index + 1) arr)

isLocalMaxima :: Int -> [Integer] -> Bool
isLocalMaxima _ [] = False
isLocalMaxima possibleMaximaPosition arr = (all (< (arr !! (possibleMaximaPosition - 1))) (removeElementAtIndex arr (possibleMaximaPosition - 1))) 

localMaxima :: [Integer] -> [Integer]
localMaxima arr = map (!! 1) (filter (isLocalMaxima 2) (sliding arr 3))

--Histogram

--Replace the nth index element. Starts from a specified index
replaceNthWith :: [a] -> a -> Int -> Int -> [a]
replaceNthWith [] _ _ _ = []
replaceNthWith org@(head:tail) replaceBy targetIndex currentIndex
    | targetIndex == currentIndex = replaceBy:tail
    | otherwise = head:(replaceNthWith tail replaceBy targetIndex (currentIndex + 1))  

--Replace all given indexes. Starts from 0th index
replaceAllWith :: [Int] -> String -> Char -> String
replaceAllWith positions initialString replaceBy = foldl (\acc a -> replaceNthWith acc replaceBy a 0) initialString positions

--Insert an elemnt in to the list if not present. If present return the list unchanged
insertIfNotPresent :: Eq a => [a] -> a -> [a]
insertIfNotPresent arr e
    | (elem e arr) = arr
    | otherwise = e:arr

--List of all distinct elements in a list
distinct :: Eq a => [a] -> [a]
distinct arr = foldl insertIfNotPresent [] arr

--Remove an element from a list. Removes only the 1st occurence
removeElement :: Eq a => [a] -> a -> [a]
removeElement [] _ = []
removeElement (head:rest) elem
    | head == elem = rest
    | otherwise = head:(removeElement rest elem)

--Remove 1st occurences of the given elements from the list
removeAll :: Eq a => [a] -> [a] -> [a]
removeAll [] _ = []
removeAll arr [] = arr
removeAll arr (head:rest) = removeAll (removeElement arr head) rest

--Applies distinct on a list until the list is empty
allDistincts :: Eq a => [a] -> [[a]]
allDistincts [] = [[]]
allDistincts arr = (distinct arr):(allDistincts (removeAll arr (distinct arr)))

--Generate histogram
histogram :: [Int] -> String
histogram arr = foldl (\acc a -> (replaceAllWith a "          \n" '*') ++ acc) "==========\n0123456789\n" (init (allDistincts arr))
