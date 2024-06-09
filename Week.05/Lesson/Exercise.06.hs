main ::IO()
main = do
    print $ normalize (4, 2) == (2, 1)
    print $ normalize (8, 4) == (2, 1)
    print $ normalize (2, 4) == (1, 2)

    print $ normalizeUsingLet (4, 2) == (2, 1)
    print $ normalizeUsingLet (8, 4) == (2, 1)
    print $ normalizeUsingLet (2, 4) == (1, 2)


type Rat a = (a, a)

normalize :: (Integral a) => Rat a -> Rat a
normalize (num, denom) = (div num gcdXY, div denom gcdXY)
 where
        gcdXY = gcd num denom

normalizeUsingLet :: (Integral a) => Rat a -> Rat a
normalizeUsingLet (x, y) = let gcdXY = gcd x y
    in (div x gcdXY, div y gcdXY)