main :: IO()
main = do
    print $ countDigitsIter 12345 == 5
    print $ countDigitsIter 123 == 3
    print $ countDigitsIter 8 == 1 -- my test

    print $ countDigitsRec 12345 == 5
    print $ countDigitsRec 123 == 3
    print $ countDigitsRec 1234567890 == 10 -- my test

countDigitsRec :: Int -> Int
countDigitsRec number
 | number < 0 = error "Negative number is entered"
 | number < 10 = 1
 | otherwise = 1 + countDigitsRec (div number 10)

countDigitsIter :: Int -> Int
countDigitsIter number = helper number 1
 where
    helper :: Int -> Int -> Int
    helper number result
     | number < 0 = error "Negative number is entered"
     | number < 10 = result
     | otherwise = helper (div number 10) (result + 1)