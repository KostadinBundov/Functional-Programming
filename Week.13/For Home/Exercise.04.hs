import Data.Char
import Data.List

main :: IO()
main = do
    print $ isBoring t1 == False
    print $ isBoring t2 == True

isBoring :: (Eq a) => NTree a -> Bool
isBoring NullT = True
isBoring (Node _ []) = True
isBoring (Node value children) = all (\(Node v _) -> v == value) children && all isBoring children 

t1 :: NTree Int
t1 = Node 10 [Node 10 [Node 10 [], Node 8 [Node 10 []], Node 2 []], Node 10 [Node 11 [], Node 10 [], Node 6 []]]

t2 :: NTree Char
t2 = Node 's' [Node 's' [], Node 's' [], Node 's' []]

data NTree a = NullT | Node a [(NTree a)]