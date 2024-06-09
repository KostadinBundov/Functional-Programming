main::IO()

main = do
    print $ prodsOdd [1, 2, 3, 4, 5, 6] == 48


prodsOdd :: Num a => [a] -> a
prodsOdd arr = foldr (\(x, y) acc -> if(odd y) then acc * x else acc) 1 (zip arr [0..])