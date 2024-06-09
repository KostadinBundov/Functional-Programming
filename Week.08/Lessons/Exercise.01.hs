import Data.List

main::IO()

main = do
    print $ nodes [(1, 2), (1, 3), (2, 3), (2, 4)] == [1, 2, 3, 4]

    print $ neighbors 2 [(1, 2), (1, 3), (2, 3), (2, 4)] == [3, 4]
    print $ neighbors 4 [(1, 2), (1, 3), (2, 3), (2, 4)] == []

    print $ adjacencyList [(1, 2), (1, 3), (2, 3), (2, 4)] == [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])]

type Node = Int
type Nodes = [Int]
type Edge = (Node, Node)
type Graph = [Edge]

nodes :: Graph -> Nodes
nodes graph = nub $ concat $ map(\(x, y) -> [x, y]) graph

neighbors :: Node -> Graph -> Nodes
neighbors node graph = map(\(parent, child) -> child) $ filter (\(parent, child) -> parent == node) graph

adjacencyList :: Graph -> [(Node, Nodes)]
adjacencyList graph = [(node, neighbors node graph) | node <- nodes graph]

