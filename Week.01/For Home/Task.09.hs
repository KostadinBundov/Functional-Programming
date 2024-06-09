main :: IO()
main = do
    print $ growingPlant 5 2 5 == 1
    print $ growingPlant 5 2 6 == 2
    print $ growingPlant 10 9 4 == 1
    print $ growingPlant 100 10 910 == 10
    print $ growingPlant 10 5 20 == 3 -- my test
    
growingPlant :: Int -> Int -> Int -> Int
growingPlant upSpeed downSpeed desiredHeight = helper upSpeed 1
 where
    helper :: Int -> Int -> Int
    helper currHeight totalDays
     | currHeight >= desiredHeight = totalDays
     | otherwise = helper (currHeight + upSpeed - downSpeed) (totalDays + 1)