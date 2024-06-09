import Data.List
import Data.Char

main::IO()
main = do
    print $ maxSumSubT t1 == 5
    print $ maxSumSubT t2 == 2


maxSumSubT :: (Ord a, Num a) => BTree a -> a
maxSumSubT NullT = 0
maxSumSubT tree@(Node _ left right) = max currentSum (max leftMaxSum rightMaxSum)
 where
    currentSum = sumSubtree tree
    leftMaxSum = maxSumSubT left
    rightMaxSum = maxSumSubT right

sumSubtree :: (Num a) => BTree a -> a
sumSubtree NullT = 0
sumSubtree (Node value left right) = value + sumSubtree left + sumSubtree right

t1 :: BTree Int
t1 = (Node 3 (Node 0 NullT NullT) (Node 2 (Node 0 NullT NullT) NullT))

t2 :: BTree Int
t2 = (Node (-3) (Node 0 NullT NullT) (Node 2 (Node 0 NullT NullT) NullT))

data BTree a = NullT | Node a (BTree a) (BTree a)