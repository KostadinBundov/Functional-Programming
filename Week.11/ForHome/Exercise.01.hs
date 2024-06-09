main :: IO()

main = do
    print $ myPoly [2.7, 3.0 ..] 2.2 3 == -0.4399999999999998
    print $ myPoly [1, 2, 3, 4] 3 4 == 0 -- my test


myPoly :: [Double] -> (Double -> Int -> Double)
myPoly xs = (\x y -> foldl (\acc curr -> acc * (x - curr)) 1 (take y xs))