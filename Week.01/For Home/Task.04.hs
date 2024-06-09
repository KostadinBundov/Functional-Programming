main :: IO()
main = do

    print $ sumCubesPow 5 1 == 126
    print $ sumCubesPow 10 50 == 126000
    print $ sumCubesPow 10 5 == 1125 -- my test

    print $ sumCubesNoPow 5 1 == 126
    print $ sumCubesNoPow 10 50 == 126000
    print $ sumCubesNoPow 1 2 == 9

sumCubesPow :: Int -> Int -> Int
sumCubesPow x y = x ^ 3 + y ^ 3

sumCubesNoPow :: Int -> Int -> Int
sumCubesNoPow x y = x * x * x + y * y * y