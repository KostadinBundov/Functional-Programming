main :: IO()
main = do
    print $ (sumExpr (2+) [0, 1, 2, 3]) 2 == 80
    print $ (sumExpr (*0.8) [0, 1, 2, 3, 4, 5]) 10 == 4345680.0
    print $ (sumExpr (3+) [1, 2, 3]) 3 == 120 -- my test

sumExpr :: (Enum a, Floating a) => (a -> a) -> [a] -> (a -> a)
sumExpr f ys = (\ x -> sum [y * f (x ** p) | (y, p) <- zip ys [1 .. ]])