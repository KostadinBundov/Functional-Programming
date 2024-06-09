main :: IO()
main = do
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [1, 2, 4] == True
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [1, 3, 4] == False
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [2, 3] == True
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [3, 1] == False

type Node = Int
type Path = [Node]
type Children = [Node]
type Graph = [(Node, Children)]

isPath :: Graph -> Path -> Bool
isPath graph path = all (\ (parent, node) -> isChild node parent graph) $ zip path (tail path)

isChild :: Node -> Node -> Graph -> Bool
isChild node parent graph = elem node $ concat [child | (cParent, child) <- graph, cParent == parent]