main :: IO()
main = do
    print $ isArithmetic [3] == True
    print $ isArithmetic [3, 5] == True
    print $ isArithmetic [1, 2, 3, 4, 5] == True
    print $ isArithmetic [3, 5, 7, 9, 11] == True
    print $ isArithmetic [3, 5, 8, 9, 11] == False
    print $ isArithmetic [3, 5, 9, 9, 11] == False
    print $ isArithmetic [1, -1, -3, -5, -7] == True -- my test

isArithmetic :: [Int] -> Bool
isArithmetic xs = helper xs 0
  where
    helper xs idx
      | idx + 2 >= length xs = True
      | otherwise = (xs !! (idx + 1) - xs !! idx) == (xs !! (idx + 2) - xs !! (idx + 1)) && helper xs (idx + 1)