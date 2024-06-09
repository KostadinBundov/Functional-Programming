main :: IO()
main = do
    print $ maxMultiple 2 7 == 6
    print $ maxMultiple 3 10 == 9
    print $ maxMultiple 7 17 == 14
    print $ maxMultiple 10 50 == 50
    print $ maxMultiple 37 200 == 185
    print $ maxMultiple 7 100 == 98  
    print $ maxMultiple 7 10 == 7
    print $ maxMultiple 4 4 == 4
    print $ maxMultiple 7 62 == 56 -- my test

maxMultiple :: Int -> Int -> Int
maxMultiple x y = helper 1 0
 where
    helper :: Int -> Int -> Int
    helper curr result
     | curr > y = result
     | mod curr x == 0 && result < curr = helper (curr + 1) curr
     | otherwise = helper (curr + 1) result
