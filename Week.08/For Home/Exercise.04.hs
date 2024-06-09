import Data.List

main :: IO()
main = do
    print $ getAreas [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == [78.53981633974483,11.25,113.30000000000001,9.127927385194024,6283.185307179587]
    print $ maxArea [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == (Cylinder 20.0 30.0)
    print $ maxArea [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 1 1] == (Rectangle 5.5 20.6) -- my test

data Shape a = Circle a | Rectangle a a | Triangle a a a | Cylinder a a
 deriving (Show, Eq, Read, Ord)

getAreas :: Floating a => [Shape a] -> [a]
getAreas shapes = map (\currShape -> area currShape) shapes

maxArea :: (Floating a, Ord a) => [Shape a] -> Shape a
maxArea (head:tail) = foldl (\current shape -> if area current >= area shape then current else shape) head tail

area :: Floating a => Shape a -> a
area (Circle r) = pi * r * r
area (Rectangle current y) = current * y
area (Triangle current y z) = let s = (current + y + z) / 2 in sqrt $ s * (s - current) * (s - y) * (s - z)
area (Cylinder r h) = 2 * pi * r * h + 2 * pi * r * r