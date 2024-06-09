main :: IO()
main = do
    print $ sumTupleNonPM (4, 5) == 9
    print $ sumTupleNonPM (-5, 5) == 0

    print $ sumTuplePM (4, 5) == 9
    print $ sumTuplePM (-5, 5) == 0

sumTuplePM :: (Int, Int) -> Int
sumTuplePM (x, y) = x + y

sumTupleNonPM :: (Int, Int) -> Int
sumTupleNonPM pair = fst pair + snd pair;