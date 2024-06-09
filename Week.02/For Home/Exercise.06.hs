main :: IO()
main = do
    print $ isInteresting 410 == True
    print $ isInteresting 212 == False
    print $ isInteresting 567 == False
    print $ isInteresting 70 == True
    print $ isInteresting 5 == True
    print $ isInteresting 4 == True
    print $ isInteresting 63 == True -- my test

isInteresting :: Int -> Bool
isInteresting number = mod number (sumDigits number 0) == 0
 where
    sumDigits :: Int -> Int -> Int
    sumDigits 0 rsult = rsult
    sumDigits number rsult
     | number < 0 = error "Number entered is negative"
     | otherwise = sumDigits (div number 10) (rsult + mod number 10)
