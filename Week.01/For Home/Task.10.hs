main :: IO()
main = do
    print $ snail 3 2 1 == 2
    print $ snail 10 3 1 == 5
    print $ snail 10 3 2 == 8
    print $ snail 100 20 5 == 7
    print $ snail 5 10 3 == 1
    print $ snail 10 4 3 == 7 -- my test


snail :: Int -> Int -> Int -> Int
snail columnHeight dayDist nightDist = helper dayDist 1
 where
    helper :: Int -> Int -> Int
    helper currHeight totalDays
     | currHeight >= columnHeight = totalDays
     | otherwise = helper (currHeight + dayDist - nightDist) (totalDays + 1)