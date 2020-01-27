-- Credit card validation
myLength :: Integer -> Int
myLength n = length (show n)

myReverse :: [Integer] -> [Integer]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

toDigits :: Integer -> [Integer]
toDigits k
    | k <= 0 = []
toDigits n 
    | myLength(n) == 1 = [n] 
    | otherwise = n `div` (10 ^ ((myLength n) - 1)) : toDigits (n `mod` (10 ^ ((myLength n) - 1)))

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = (reverse (toDigits n))

doubleEveryOtherFromFirst :: [Integer] -> [Integer]
doubleEveryOtherFromFirst [] = []
doubleEveryOtherFromFirst (x:[]) = [x]
doubleEveryOtherFromFirst (x:y:ys)  = [x, (2 * y)] ++ (doubleEveryOtherFromFirst ys)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther xs = (myReverse (doubleEveryOtherFromFirst (myReverse xs)))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
    | (myLength x) == 1 = x + sumDigits xs
    | otherwise = (sumDigits (toDigits x)) + sumDigits xs
