main :: IO()
main = do
    print $ removeFirstOccurrence 16366 5 == 16366
    print $ removeFirstOccurrence 110 1 == 10
    print $ removeFirstOccurrence 15365 5 == 1536
    print $ removeFirstOccurrence 15360 0 == 1536
    print $ removeFirstOccurrence 15300 0 == 1530
    print $ removeFirstOccurrence 15365 1 == 5365
    print $ removeFirstOccurrence 35365 3 == 3565
    print $ removeFirstOccurrence 1212 1 == 122
    print $ removeFirstOccurrence 1212 2 == 121
    print $ removeFirstOccurrence (removeFirstOccurrence 1212 1) 1 == 22

removeFirstOccurrence :: Int -> Int -> Int
removeFirstOccurrence number digit = helper number 0 0
 where
    helper :: Int -> Int -> Int -> Int
    helper 0 _ result = result
    helper number power result
     | mod number 10 == digit = (10^power) * div number 10 + result
     | otherwise = helper (div number 10) (power + 1) (result + (10^power) * mod number 10)