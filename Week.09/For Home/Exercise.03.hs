import Data.List

main :: IO()
main = do
    print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10)] == (4.47213595499958,ThreeD 4.0 5.0 6.0,ThreeD 2.0 5.0 10.0)
    print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10), (ThreeD 5 2 (-10)), (ThreeD (-2) 1 45), (ThreeD 12 0 2)] == (4.47213595499958,ThreeD 4.0 5.0 6.0,ThreeD 2.0 5.0 10.0)
    print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10), (ThreeD 5 2 (-10)), (ThreeD (-2) 1 45), (ThreeD 12 0 2), (ThreeD 6 5 4)] == (2.8284271247461903,ThreeD 4.0 5.0 6.0,ThreeD 6.0 5.0 4.0)
    print $ getClosestDistance [(TwoD 4 6), (TwoD 5 10), (TwoD 5 29), (TwoD 1 45), (TwoD 0 2), (TwoD 69 42)] == (4.123105625617661,TwoD 4.0 6.0,TwoD 5.0 10.0)
    print $ getClosestDistance [(ThreeD 4 8 10), (ThreeD 2 5 10)] == (3.605551275463989,ThreeD 4.0 8.0 10.0,ThreeD 2.0 5.0 10.0) --  my test

data Point a = TwoD a a | ThreeD a a a
 deriving(Eq, Ord, Show, Read)

findDistance :: (Floating a) => Point a -> Point a -> a
findDistance (ThreeD x1 x2 x3) (ThreeD y1 y2 y3) = sqrt $ (y1 - x1)**2 + (y2 - x2)**2 + (y3 - x3)**2 
findDistance (TwoD x1 x2) (TwoD y1 y2) = sqrt $ (y1 - x1)**2 + (y2 - x2)**2

getAllPairsOfPoints :: (Eq a, Ord a) => [Point a] -> [(Point a, Point a)]
getAllPairsOfPoints points = [(x, y) | x <- points, y <- points, x /= y]

createDistancePairs :: (Floating a) => [(Point a, Point a)] -> [(a, Point a, Point a)]
createDistancePairs pairs = map (\(p1, p2) -> (findDistance p1 p2, p1, p2)) pairs

getClosestDistance :: (Floating a, Ord a) => [Point a] -> (a, Point a, Point a)
getClosestDistance points = let
    pairs = getAllPairsOfPoints points
    distancePairs = createDistancePairs pairs
    in foldr1 (\(d1, p1, p2) (d2, p3, p4) -> if d1 <= d2 then (d1, p1, p2) else (d2, p3, p4)) distancePairs