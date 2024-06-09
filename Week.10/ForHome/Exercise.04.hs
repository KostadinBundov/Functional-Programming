main :: IO()
main = do
    print $ isGraceful t1 == True
    print $ isGraceful t2 == False
    print $ isGraceful t3 == False -- my test



isGraceful :: NAryTree -> Bool
isGraceful Nil = True
isGraceful (Node _ children) = all (\ x -> isGraceful x) children && checkEvenDifferences (map(\(Node x _) -> x) children)

checkEvenDifferences :: [Int] -> Bool
checkEvenDifferences [] = True
checkEvenDifferences [_] = True
checkEvenDifferences (x:y:rest) = even (mod (abs (x - y)) 2) && checkEvenDifferences (y:rest)

data NAryTree = Nil | Node Int [NAryTree]
  deriving(Show)

t1 :: NAryTree
t1 = Node 1 [Node 3 [Nil], Node 5 [Nil], Node 7 [Nil], Node 9 [Nil]]

t2 :: NAryTree
t2 = Node 7 [Node 9 [Node 5 [Nil], Node 2 [Nil]]]

t3 :: NAryTree
t3 = Node 10 [Node 12 [Node 13 [Nil], Node 14 [Nil]], Node 15 [Node 11 [Nil]], Node 18 [Node 12 [Nil]]]