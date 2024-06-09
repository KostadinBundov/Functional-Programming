main :: IO()
main = do
    print $ removeD 1 656 == 656
    print $ removeD 5 656 == 66
    print $ removeD 6 656 == 5
    print $ removeD 0 606 == 66
    print $ removeD 0 600 == 6
    print $ removeD 6 600 == 0
    print $ removeD 2 1423 == 143
    print $ removeD 3 1333 == 1 -- my test

removeD :: Int -> Int -> Int
removeD digit number = helper number 0 1
 where 
    helper :: Int -> Int -> Int -> Int
    helper 0 newNumber _ = newNumber
    helper curr newNumber multiplier
     | mod curr 10 == digit = helper (div curr 10) newNumber multiplier
     | otherwise = helper (div curr 10) ((mod curr 10) * multiplier + newNumber) (multiplier * 10)
