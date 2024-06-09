main :: IO()
main = do
    print $ areAmicable 200 300 == False
    print $ areAmicable 220 284 == True
    print $ areAmicable 284 220 == True
    print $ areAmicable 1184 1210 == True
    print $ areAmicable 2620 2924 == True
    print $ areAmicable 6232 6368 == True
    print $ areAmicable 5020 5564 == True -- my test

areAmicable :: Int -> Int -> Bool
areAmicable x y = sumDivisors x == y && sumDivisors y == x
 where
    sumDivisors :: Int -> Int
    sumDivisors number = helper 1 0
     where
        helper :: Int -> Int -> Int
        helper curr result
         | curr >= number = result
         | mod number curr == 0 = helper (curr + 1) (result + curr)
         | otherwise = helper (curr + 1) result