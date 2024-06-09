import Data.List
import Data.Char

main::IO()

main = do
    print $ solve ["abode","ABc","xyzD"] == [4,3,1]
    print $ solve ["abide","ABc","xyz"] == [4,3,0]
    print $ solve ["IAMDEFANDJKL","thedefgh","xyzDEFghijabc"] == [6,5,7]
    print $ solve ["encode","abc","xyzD","ABmD"] == [1, 3, 1, 3]

solve::[String] -> [Int]
solve = map getCount


getCount :: [Char] -> Int
getCount word = length $ filter (\(x,y) -> x == y) (zip (map (\w -> toLower w) word) ['a'..])