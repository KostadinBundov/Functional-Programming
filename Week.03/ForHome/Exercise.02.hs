main :: IO()
main = do
    print $ sortN 1714 == 7411
    print $ sortN 123450 == 543210
    print $ sortN 123405 == 543210
    print $ sortN 123045 == 543210
    print $ sortN 120345 == 543210
    print $ sortN 102345 == 543210
    print $ sortN 8910 == 9810
    print $ sortN 321 == 321
    print $ sortN 29210 == 92210
    print $ sortN 1230 == 3210
    print $ sortN 55345 == 55543
    print $ sortN 14752 == 75421
    print $ sortN 329450 == 954320
    print $ sortN 9125 == 9521

sortN :: Int -> Int
sortN number = helper 9 0
 where
    helper :: Int -> Int -> Int
    helper (-1) result = result
    helper digit result = helper (digit - 1) (addToResult result number)
      where 
        addToResult :: Int -> Int -> Int
        addToResult result 0 = result
        addToResult result number
         | mod number 10 == digit = addToResult (result * 10 + mod number 10) (div number 10)
         | otherwise = addToResult result (div number 10)
        
     
