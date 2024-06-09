import Data.List

main :: IO()
main = do
    print $ onlyUnique [1, 2, 3, 2] == [1, 3]
    print $ onlyUnique [1, -1] == [-1, 1]

    print $ sumUnique [[1,2,3,2],[1,-4],[1]] == 2
    print $ sumUnique [[1,2,3,2],[-4,-4],[5]] == 9 -- (= 1 + 3 + 5)
    print $ sumUnique [[2,2,2],[3,3,3],[4,4,4]] == 0
    print $ sumUnique [[1,2,3],[4,5,6],[7,8,9]] == 45


sumUnique :: [[Int]] -> Int
sumUnique arr = sum $ map (sum . onlyUnique) arr
-- sumUnique = sum . concatMap onlyUnique

onlyUnique :: [Int] -> [Int]
onlyUnique arr = [head subList | subList <- group $ sort $ arr, length subList == 1] 