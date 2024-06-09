main :: IO()
main = do
    print $ findUncles t 5 == [3,4]
    print $ findUncles t 7 == [2,4]
    print $ findUncles t 10 == [5]
    print $ findUncles t 8 == [2, 3] -- my test

type Tree = [(Int, [Int])]

t :: Tree
t = [(1,[2,3,4]),(2,[5,6]),(3,[7]),(4,[8,9]),(5,[]),(6,[10]),(7,[]),(8,[]),(9,[]),(10,[])]

findUncles :: Tree -> Int -> [Int]
findUncles tree node =
  let
   parent = fst $ head $ filter (\ (_, children) -> elem node children) tree
   siblings father = filter (/= father) $ concatMap snd $ filter (\ (parent, children) -> parent /= father && elem father children) tree
  in
   siblings parent