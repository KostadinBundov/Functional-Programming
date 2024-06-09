main :: IO()
main = do
    print $ sumDigitsIter 12345 == 15
    print $ sumDigitsIter 123 == 6
    print $ sumDigitsIter 284655 == 30 -- my test
    print $ sumDigitsIter (-284655) == 30 -- my test

sumDigitsIter :: Int -> Int
sumDigitsIter number = helper number 0
 where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper number result
     | number < 0 = error "Number entered is negative"
     | otherwise = helper (div number 10) (result + mod number 10)