main :: IO()
main = do
    print $ countOccurrences 121 1 == 2
    print $ countOccurrences 222 1 == 0
    print $ countOccurrences 100 0 == 2
    print $ countOccurrences 0 0 == 1
    print $ countOccurrences 122312 2 == 3 -- my test

countOccurrences :: Int -> Int -> Int
countOccurrences number digit
 | number == 0 && digit == 0 = 1
 | otherwise = helper number 0
 where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper number result
     | mod number 10 == digit = helper (div number 10) (result + 1)
     | otherwise = helper (div number 10) result
