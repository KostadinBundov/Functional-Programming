main :: IO()
main = do
    print $ onDiag (5.5, 5.5) == True
    print $ onDiag (0.5, 0) == False

diagonal = line (0, 0) (1, 1)
onDiag = liesOn diagonal

type Point = (Double, Double)

line :: Point -> Point -> (Double -> Double)
line (x1, y1) (x2, y2) = (\x -> multiplier * x + resultSum)
 where
    resultSum = y1 - x1 * (y2 - y1) / (x2 - x1)
    multiplier = (y2 - y1) / (x2 - x1)

liesOn :: (Double -> Double) -> (Point -> Bool)
liesOn f = (\ (x,y) -> f x == y)