main :: IO()
main = do
    print $ onlyArithmetic [[3], [1, 2, 3, 4, 5], [3, 5, 8, 9, 11]]  == [[3], [1, 2, 3, 4, 5]]

isArithmetic :: [Int] -> Bool
isArithmetic xs = helper xs 0
  where
    helper xs idx
      | idx + 2 >= length xs = True
      | otherwise = (xs !! (idx + 1) - xs !! idx) == (xs !! (idx + 2) - xs !! (idx + 1)) && helper xs (idx + 1)

onlyArithmetic :: [[Int]] -> [[Int]]
onlyArithmetic = filter isArithmetic