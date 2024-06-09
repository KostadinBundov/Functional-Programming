main :: IO()
main = do
    print $ mySumRecNonPM [] == 0
    print $ mySumRecNonPM [1, 2, 3] == 6
    print $ mySumRecNonPM [2, 4, 6, 8, 10] == 30 -- my test

    print $ mySumRecPM [] == 0
    print $ mySumRecPM [1, 2, 3] == 6
    print $ mySumRecPM [1, 2, 3, 4, 5, 6, 7, 8] == 36 -- my test

    print $ mySumFunc [] == 0
    print $ mySumFunc [1, 2, 3] == 6
    print $ mySumFunc [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] == 55 -- my test

mySumFunc :: [Int] -> Int
mySumFunc = sum

mySumRecPM :: [Int] -> Int
mySumRecPM [] = 0
mySumRecPM (x:xs) = x + mySumRecPM xs

mySumRecNonPM :: [Int] -> Int
mySumRecNonPM xs 
 | null xs = 0
 | otherwise = head xs + mySumRecNonPM (tail xs)