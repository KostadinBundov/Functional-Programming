main :: IO()
main = do
    print $ (myPolynomial (\x -> x - 2) []) 5 == 0
    print $ (myPolynomial (\x -> x + 10) [3.62, 6.12, 9.45, 8.02, 5, 2]) (-5) == -356.45
    print $ (myPolynomial (\x -> x - 2) [1, 4, 7, 8, 5, 2]) 5 == 453

myPolynomial :: (Enum a, Num a) => (a -> a) -> [a] -> (a -> a)
myPolynomial f ys = (\ x -> sum $ map (\ (y, pos) -> pos * (f $ y * x)) $ zip ys [1 .. ])