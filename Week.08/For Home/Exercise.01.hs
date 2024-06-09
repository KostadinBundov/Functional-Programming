main :: IO()
main = do
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 0 1 == [[1]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 1 == [[1, 2], [1, 3]]
    print $ simplePaths [(1, [2, 3, 4]), (2, [3, 4]), (3, []), (4, [])] 1 1 == [[1,2],[1,3],[1,4]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 2 1 == [[1, 2, 3], [1, 2, 4]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 2 == [[2,3],[2,4]]
    print $ simplePaths [(1, [2, 3]), (2, [3]), (3, []), (4, [])] 1 2 == [[2,3]]
    print $ simplePaths [(1, [2, 3]), (2, [3]), (3, []), (4, [])] 2 1 == [[1, 2,3]] -- my test

type Node = Int
type Graph = [(Node, [Node])]
type Path = [Node]

simplePaths :: Graph -> Int -> Node -> [Path]
simplePaths graph k startNode = filter (\path -> length path == k + 1 && isPath graph path) $ findAllPaths graph [startNode] startNode k

findAllPaths :: Graph -> Path -> Node -> Int -> [Path]
findAllPaths graph currentPath currentNode k
    | k == 0 = [currentPath]
    | otherwise = concat [findAllPaths graph (currentPath ++ [child]) child (k - 1) | child <- getChildren currentNode graph]

getChildren :: Node -> Graph -> [Node]
getChildren node graph = concat $ map snd $ filter(\(parent, _) -> parent == node) graph

isPath :: Graph -> Path -> Bool
isPath graph path = all (\ (parent, node) -> isChild node parent graph) $ zip path (tail path)

isChild :: Node -> Node -> Graph -> Bool
isChild node parent graph = elem node $ concat [child | (cParent, child) <- graph, cParent == parent]