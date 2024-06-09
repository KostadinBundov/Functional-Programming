main :: IO()
main = do
    print $ (applyN (\x -> 2 * x) 5) 2 == 64
    print $ (applyN (\x -> div x 10) 2) 100 == 1
    print $ (applyN (\x -> 1 * x) 5) 10 == 10 -- my test

applyN :: (a -> a) -> Int -> (a -> a)
applyN f n
 | n <=0 = id
 | otherwise = f . applyN f (n - 1)