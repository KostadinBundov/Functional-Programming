main :: IO()
main = do
    print $ divideNonPM (10, 5) == (2, 0)
    print $ divideNonPM (5, 10) == (0, 5)

    print $ dividePM (10, 5) == (2, 0)
    print $ dividePM (5, 10) == (0, 5)

    print $ (\ (k, l) -> (div k l, mod k l)) (10, 5) == (2, 0)
    print $ (\ (k, l) -> (div k l, mod k l)) (5, 10) == (0, 5)

dividePM :: (Int, Int) -> (Int, Int)
dividePM (k, l) = (div k l, mod k l)

divideNonPM :: (Int, Int) -> (Int, Int)
divideNonPM pair = (div (fst pair) (snd pair), mod (fst pair) (snd pair))