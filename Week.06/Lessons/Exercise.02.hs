main :: IO()
main = do
    print $ dominates (\x -> x + 1) (\x -> x + 2) [1, 2, 3, 4, 5] == False
    print $ dominates (\x -> x * 3) (\x -> x + 2) [1, 2, 3, 4, 5] == True

    print $ dominatesFold (\x -> x + 1) (\x -> x + 2) [1, 2, 3, 4, 5] == False
    print $ dominatesFold (\x -> x * 3) (\x -> x + 2) [1, 2, 3, 4, 5] == True

dominates :: (Int -> Int) -> (Int -> Int) -> [Int] -> Bool
dominates f g xs = all (\ x -> abs(f x) >= abs(g x)) xs

dominatesFold :: (Int -> Int) -> (Int -> Int) -> [Int] -> Bool
dominatesFold f g xs = foldl (&&) True $ map (\ x -> abs (f x) >= abs (g x)) xs