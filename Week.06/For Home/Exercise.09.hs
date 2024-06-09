import Data.Char
import Data.List

main :: IO()
main = do
    print $ reduceStr "Aa" == ""
    print $ reduceStr "dabAcCaCBAcCcaDD" == "dabCBAcaDD" -- dabAcCaCBAcCcaDD -> dabAaCBAcCcaDD -> dabCBAcCcaDD -> dabCBAcaDD

-- reduceStr :: String -> String
-- reduceStr [] = []
-- reduceStr [x] = [x]
-- reduceStr (x:y:xs)
--   | (toUpper x == y || x == toUpper y) && (toLower x == toLower y) = reduceStr xs
--   | otherwise = x : reduceStr (y:xs)

areDuplicates :: Char -> Char -> Bool
areDuplicates lhs rhs = rhs /= lhs && toLower lhs == toLower rhs

reduceStr :: String -> String
reduceStr str = reverse $ helper str []
 where
  helper :: String -> [Char] -> String
  helper [] stack = stack
  helper (x:xs) [] = helper xs [x]
  helper (x:xs) (s:ss)
   | areDuplicates x s = helper xs ss
   | otherwise = helper xs (x:s:ss) 