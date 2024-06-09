main :: IO()
main = do
    print $ sumRats (2, 5) (5, 2) == (29, 10)
    print $ sumRats (52, 123) (96, 14) == (6268, 861)
    print $ sumRats (2, 5) (3, 5) == (1, 1)

    print $ multiplyRats (2, 5) (5, 2) == (1, 1)
    print $ multiplyRats (52, 123) (96, 14) == (832, 287)
    print $ multiplyRats (2, 5) (3, 5) == (6, 25)

    print $ divideRats (2, 5) (5, 2) == (4, 25)
    print $ divideRats (52, 123) (96, 14) == (91, 1476)
    print $ divideRats (2, 5) (3, 5) == (2, 3)

    print $ areEqual (2, 5) (5, 2) == False
    print $ areEqual (52, 123) (52 * 3, 123 * 3) == True
    print $ areEqual (2, 0) (5, 15) == True

type Rat a = (a, a)

normalize :: (Integral a) => Rat a -> Rat a
normalize (_, 0) = error "denom cannot be zero!"
normalize (num, denom) = (div num gcdXY, div denom gcdXY)
 where
        gcdXY = gcd num denom

sumRats :: (Integral a) => Rat a -> Rat a -> Rat a
sumRats (nom1, denom1) (nom2, denom2) = normalize (nom1 * denom2 + nom2 * denom1, denom1 * denom2)

multiplyRats :: (Integral a) => Rat a -> Rat a -> Rat a
multiplyRats (nom1, denom1) (nom2, denom2) = normalize (nom1 * nom2, denom1 * denom2)

divideRats :: (Integral a) => Rat a -> Rat a -> Rat a
divideRats (nom1, denom1) (nom2, denom2) = normalize (nom1 * denom2, nom2 * denom1)

areEqual :: (Integral a) => Rat a -> Rat a -> Bool
areEqual leftRat rightRat = normalize leftRat == normalize rightRat